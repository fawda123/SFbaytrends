###### 
# data process

library(tidyverse)
library(lubridate)
library(sf)
library(mgcv)

######
# wq data

dat <- read.csv('raw/df_18_36.csv', stringsAsFactors = F)

# add date columns, convert parameter names, filter by years, remove d_chl, old do ests (calculated below)
datprc <- dat %>% 
  mutate(
    date = ymd(date),
    doy = yday(date), 
    dec_time = decimal_date(date),
    yr = year(date),
    mo = month(date, label = T),
    param = tolower(param),
    param = gsub('^c_chl$', 'chl', param), 
    param = gsub('^s$', 'sal', param)
  ) %>% 
  filter(yr >= 1990 & yr <= 2017) %>% 
  filter(!param %in% c('dosat', 'docalc', 'd_chl'))
         
# get do bottom val
dodat <- read.csv('raw/DO_subset_stations.csv', stringsAsFactors = F) %>% 
 mutate(
   date = ymd(date),
   doy = yday(date), 
   dec_time = decimal_date(date),
   yr = year(date),
   mo = month(date, label = T)
 ) %>% 
 rename(
   dosat = DO_sat,
   docalc = DO_mgL
 ) %>% 
 group_by(date, station) %>% 
 filter(depth == max(depth, na.rm = T)) %>% # bottom do
 ungroup %>% 
 gather('param', 'value', dosat, docalc) %>% 
 select(date, station, param, value, doy, dec_time, yr, mo) %>% 
 filter(yr >= 1992 & yr <= 2017) %>% 
 filter(!is.na(value))

# get gpp estimates
gppdat <- read.csv('raw/GPP_dataset.csv', stringsAsFactors = F) %>% 
 select(dec_date, station, GPP) %>% 
 mutate(
   date = date_decimal(dec_date),
   date = as.Date(date),
   doy = yday(date),
   yr = year(date),
   mo = month(date, label = T)
 ) %>% 
 rename(
   gpp = GPP,
   dec_time = dec_date
 ) %>% 
 gather('param', 'value', gpp) %>% 
 select(date, station, param, value, doy, dec_time, yr, mo) %>% 
 filter(yr >= 1992 & yr <= 2017) %>% 
 filter(!is.na(value))

# combine new do ests, gpp with datprc
datprc <- datprc %>% 
 bind_rows(dodat, gppdat) %>% 
 arrange(date, station, param)

save(datprc, file = 'data/datprc.RData', compress = 'xz')

######
# station lat/lon

prj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

data(datprc)

locs <- read.csv('ignore/usgs_station_lat_lon.csv') %>% 
  filter(Station %in% datprc$station)

save(locs, file = 'data/locs.RData', compress = 'xz')

######
# get GAMs for each station, chl, docalc, dosat, gam0, gam1, gam2, gam6 from baytrends

data(datprc)

# smooths to evaluate
frms <- c(
  "dec_time + s(doy, bs = 'cc')",  
  "dec_time + s(dec_time) + s(doy, bs = 'cc')",
  "dec_time + s(dec_time) + s(doy, bs = 'cc') + ti(dec_time, doy, bs = c('tp', 'cc'))",
  "dec_time + s(dec_time) + s(doy, bs = 'cc')"
) %>% 
  as.list

# enframe frms for combine with tomod
frms <- frms %>% 
  enframe('modi', 'frm') %>% 
  unnest(frm)

# data to model, same as datprc, params in wide format, nested by station
# crossed with frms
tomod <- datprc %>% 
  filter(param %in% c('chl', 'docalc', 'dosat', 'gpp')) %>% # add parameters here
  group_by(station, param) %>% 
  nest %>% 
  crossing(frms) %>% 
  mutate(
    modi = as.list(modi), 
    param = ifelse(param %in% 'chl', 'log10(chl)', param),
    param = ifelse(param %in% 'gpp', 'log10(gpp)', param),
    frm = ifelse(param %in% c('log10(gpp)', 'log10(chl)'), paste0('log10(value) ~ ', frm), paste0('value ~ ', frm)),
    frm = as.list(frm)
  )

# create models for every station, gam model eval
modssta <- tomod %>%
  mutate(
    kyear = purrr::pmap(list(param, modi, data), function(param, modi, data){
      
      out <- NA
      
      # insert upper gamk1 rule for gam1
      if(modi %in% c(2, 3)){
  
        # get upper bounds of knots
        out <- data$yr %>%
          unique %>%
          length 
        out <- round(out * (2/3), 0)
        out <- pmax(10, out)
        
      }
      
      # insert upper gamk1* rule for gamk1*
      if(modi %in% 4){
        
        # get upper bounds of knots
        out <- 12 * length(unique(data$yr))
        
        # gpp have missing data in some months, so decrease upper k boundary
        if(param == 'log10(gpp)')
          out <- round(0.9 * nrow(data))
        
      }
      
      return(out)
      
    }),
    modv = purrr::pmap(list(station, param, data, modi, frm, kyear), function(station, param, data, modi, frm, kyear){
      
      cat(station, param, modi, '\t')
      
      # insert upper gamk1 rule for gam1
      if(modi %in% c(2, 3)){
        
        p1 <- gsub('(^.*)s\\(dec\\_time\\).*$', '\\1', frm)
        p3 <-  gsub('^.*s\\(dec\\_time\\)(.*)$', '\\1', frm)
        p2 <- paste0('s(dec_time, k = ', kyear, ')')
        frm <- paste0(p1, p2, p3)
        
      }
      
      # insert upper gamk1* rule for gamk1*
      if(modi %in% 4){
        
        p1 <- gsub('(^.*)s\\(dec\\_time\\).*$', '\\1', frm)
        p3 <-  gsub('^.*s\\(dec\\_time\\)(.*)$', '\\1', frm)
        p2 <- paste0('s(dec_time, k = ', kyear, ')')
        frm <- paste0(p1, p2, p3)
        
      }
      
      out <- gam(as.formula(frm),
                 knots = list(doy = c(1, 366)),
                 data = data,
                 na.action = na.exclude,
                 select = T
      )
      
      return(out)
      
    })
  )

# rename gam mod types
modssta <- modssta %>%
  mutate(modi = factor(modi, levels = c(1, 2, 3, 4), labels = c('gam0', 'gam1', 'gam2', 'gam6')))

# separate parameters into diff files (single is too large for git)
modssta_chl <- modssta %>% 
  filter(param %in% 'log10(chl)')
modssta_docalc <- modssta %>% 
  filter(param %in% 'docalc')
modssta_dosat <- modssta %>% 
  filter(param %in% 'dosat')
modssta_gpp <- modssta %>% 
  filter(param %in% 'log10(gpp)')

save(modssta_chl, file = 'data/modssta_chl.RData', compress = 'xz')
save(modssta_docalc, file = 'data/modssta_docalc.RData', compress = 'xz')
save(modssta_dosat, file = 'data/modssta_dosat.RData', compress = 'xz')
save(modssta_gpp, file = 'data/modssta_gpp.RData', compress = 'xz')

######
# check knots and fit

data(datprc)

# smooths to evaluate
frms <- c(
  gam1 = "log10(chl) ~ dec_time + s(dec_time) + s(doy, bs = 'cc')",
  gam2 = "log10(chl) ~ dec_time + s(dec_time) + s(doy, bs = 'cc') + ti(dec_time, doy, bs = c('tp', 'cc'))"
) %>% 
  as.list

# enframe frms for combine with tomod
frms <- frms %>% 
  enframe('modi', 'frm')

# knots to eval
ks <- seq(1, 100)

# station to evaluate
# sta <- 32

# data to model, same as datprc, params in wide format, nested by station
# crossed with frms
modkcmp <- datprc %>% 
  spread(param, value) %>% 
  group_by(station) %>% 
  # filter(station %in% sta) %>% 
  nest %>% 
  crossing(frms, ks) %>% 
  mutate(
    frm = pmap(list(ks, frm), function(ks, frm){
      
      p1 <- gsub('(^.*)s\\(dec\\_time\\).*$', '\\1', frm)
      p3 <-  gsub('^.*s\\(dec\\_time\\)(.*)$', '\\1', frm)
      p2 <- paste0('s(dec_time, k = ', ks, ')')
      frm <- paste0(p1, p2, p3)
      return(frm)
      
    }), 
    modv = pmap(list(data, frm), function(data, frm){
      
      cat(frm, '\n')
      
      gam(as.formula(frm),
          knots = list(doy = c(1, 366)),
          data = data,
          na.action = na.exclude,
          select = T
      )
      
    }), 
    ests = map(modv, function(x){
      
      data.frame(
        AIC = AIC(x), 
        GCV = x$gcv.ubre,
        R2 = summary(x)$r.sq
      )
      
    })
  )

# keep summaries only, otherwise huge
modkcmp <- modkcmp %>% 
  select(-data, -frm, -modv) %>% 
  unnest

save(modkcmp, file = 'data/modkcmp.RData', compress = 'xz')