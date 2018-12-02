###### 
# data process

library(tidyverse)
library(lubridate)
library(sf)

######
# wq data

dat <- read.csv('raw/df_18_36.csv', stringsAsFactors = F)

# add date columns, convert parameter names, filter by years, remove d_chl
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
  filter(!param %in% 'd_chl')
  
save(datprc, file = 'data/datprc.RData', compress = 'xz')

######
# station lat/lon

prj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

data(datprc)

locs <- read.csv('ignore/usgs_station_lat_lon.csv') %>% 
  filter(Station %in% datprc$station)

save(locs, file = 'data/locs.RData', compress = 'xz')

######
# get GAMs for each station, gam0, gam1, gam2, gam1* from baytrends

data(datprc)

# smooths to evaluate
frms <- c(
  "log10(chl) ~ dec_time + s(doy, bs = 'cc')",  
  "log10(chl) ~ dec_time + s(dec_time) + s(doy, bs = 'cc')",
  "log10(chl) ~ dec_time + s(dec_time) + s(doy, bs = 'cc') + ti(dec_time, doy, bs = c('tp', 'cc'))",
  "log10(chl) ~ dec_time + s(dec_time) + s(doy, bs = 'cc')"
) %>% 
  as.list

# enframe frms for combine with tomod
frms <- frms %>% 
  enframe('modi', 'frm') %>% 
  unnest

# data to model, same as datprc, params in wide format, nested by station
# crossed with frms
tomod <- datprc %>% 
  spread(param, value) %>% 
  group_by(station) %>% 
  nest %>% 
  crossing(frms) %>% 
  mutate(
    modi = as.list(modi), 
    frm = as.list(frm)
  )

# create models for every station, gam model eval
modssta <- tomod %>%
  mutate(
    modv = purrr::pmap(list(station, data, modi, frm), function(station, data, modi, frm){
      
      cat(station, modi, '\t')
      
      # insert upper gamk1 rule for gam1
      if(modi %in% c(2, 3)){
        
        # get upper bounds of knots
        gamk1 <- data$yr %>%
          unique %>%
          length 
        gamk1 <- gamk1 * (2/3) %>% 
          round(., 0)
        gamk1 <- gamk1 %>% 
          pmax(10, .)
        
        p1 <- gsub('(^.*)s\\(dec\\_time\\).*$', '\\1', frm)
        p3 <-  gsub('^.*s\\(dec\\_time\\)(.*)$', '\\1', frm)
        p2 <- paste0('s(dec_time, k = ', gamk1, ')')
        frm <- paste0(p1, p2, p3)
        
      }
      
      # insert upper gamk1* rule for gamk1*
      if(modi %in% 4){
        
        # get upper bounds of knots
        gamk1 <- 12 * length(unique(data$yr))
        
        p1 <- gsub('(^.*)s\\(dec\\_time\\).*$', '\\1', frm)
        p3 <-  gsub('^.*s\\(dec\\_time\\)(.*)$', '\\1', frm)
        p2 <- paste0('s(dec_time, k = ', gamk1, ')')
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
modssta <- modssta %>%
  mutate(modi = factor(modi, levels = c(1, 2, 3, 4), labels = c('gam0', 'gam1', 'gam2', 'gam1*')))
save(modssta, file = 'data/modssta.RData', compress = 'xz')

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
