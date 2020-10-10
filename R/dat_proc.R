library(tidyverse)
library(lubridate)
library(sf)
library(wqtrends)

# format raw wq data for use with wqtrends --------------------------------

dat <- read.csv('raw/sfb_surf_CB_SB_LSB.csv', stringsAsFactors = F)

# add date columns, convert parameter names, filter by years, remove d_chl, old do ests (calculated below)
datprc <- dat %>% 
  select(date, station, chl = chl_merge, docalc = do_merge) %>% 
  gather('param', 'value', -date, -station) %>% 
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
  filter(yr >= 1990 & yr <= 2019) %>% 
  filter(!is.na(value))

# get gpp estimates
gppdat <- read.csv('raw/sfb_GPP_monthly.csv', stringsAsFactors = F) %>% 
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
 filter(yr >= 1990 & yr <= 2019) %>% 
 filter(!is.na(value))

# combine new do ests, gpp with datprc
datprc <- datprc %>% 
 bind_rows(gppdat) %>% 
 arrange(station, param, date)

# rawdat <- datprc
# save(rawdat, file = '../wqtrends/data/rawdat.RData', compress = 'xz')
save(datprc, file = 'data/datprc.RData', compress = 'xz')

# station lat/lon as separate file ----------------------------------------

prj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

data(datprc)

locs <- read.csv('ignore/usgs_station_lat_lon.csv') %>% 
  filter(Station %in% datprc$station)

save(locs, file = '../wqtrends-manu/data/locs.RData', compress = 'xz')
save(locs, file = 'data/locs.RData', compress = 'xz')

# save separate rdata model files for each station, parameter -------------

data(datprc)

# data to model, same as datprc, params in wide format, nested by station
# crossed with frms
tomod <- datprc %>% 
  filter(param %in% c('chl', 'docalc', 'gpp')) %>% # add parameters here
  group_by(station, param) %>% 
  nest %>% 
  crossing(model = c('gam0', 'gam1', 'gam2', 'gam6')) %>% 
  mutate(
    trans = case_when(
      param %in% c('chl', 'gpp') ~ 'boxcox', 
      T ~ 'ident'
    )
  )

# create models for every station, gam model eval
modssta <- tomod %>%
  mutate(
    modi = purrr::pmap(list(station, param, model, trans, data), function(station, param, model, trans, data){
      
      cat(station, param, model, '\n')
      out <- anlz_gam(data, mod = model, trans = trans)
      return(out)
      
    })
  )

# separate models into diff files by parameter and stations (single is too large for git)
tosv <- modssta %>% 
  select(station, param) %>% 
  unique

for(i in 1:nrow(tosv)){
  
  cat(i, 'of', nrow(tosv), '\n')
  
  sta <- tosv[[i, 'station']]
  param <- tosv[[i, 'param']]
  
  fl <- modssta %>% 
    filter(station %in% !!sta) %>% 
    filter(param %in% !!param)
  
  flnm <- paste0('mods_', param, sta)
  
  assign(flnm, fl)
  
  save(list = flnm, file = paste0('data/', flnm, '.RData'), compress = 'xz')
  
}

# copy chlorophyll mods to manu repo
fls <- list.files('data', pattern = '^mods\\_chl', full.names = T)

file.copy(fls, '../wqtrends-manu/data/')

# same as above but log chl -----------------------------------------------

data(datprc)

# data to model, same as datprc, params in wide format, nested by station
# crossed with frms
tomod <- datprc %>% 
  filter(param %in% c('chl')) %>% # add parameters here
  group_by(station, param) %>% 
  nest %>% 
  crossing(model = c('gam0', 'gam1', 'gam2', 'gam6')) %>% 
  mutate(
    trans = 'log10'
  )

# create models for every station, gam model eval
modssta <- tomod %>%
  mutate(
    modi = purrr::pmap(list(station, param, model, trans, data), function(station, param, model, trans, data){
      
      cat(station, param, model, '\n')
      out <- anlz_gam(data, mod = model, trans = trans)
      return(out)
      
    })
  )

# separate models into diff files by parameter and stations (single is too large for git)
tosv <- modssta %>% 
  select(station, param) %>% 
  unique

for(i in 1:nrow(tosv)){
  
  cat(i, 'of', nrow(tosv), '\n')
  
  sta <- tosv[[i, 'station']]
  param <- tosv[[i, 'param']]
  
  fl <- modssta %>% 
    filter(station %in% !!sta) %>% 
    filter(param %in% !!param)
  
  flnm <- paste0('modslog_', param, sta)
  
  assign(flnm, fl)
  
  save(list = flnm, file = paste0('data/', flnm, '.RData'), compress = 'xz')
  
}

# copy chlorophyll mods to manu repo
fls <- list.files('data', pattern = '^modslog\\_chl', full.names = T)

file.copy(fls, '../wqtrends-manu/data/')

# checking fit with changing knots ----------------------------------------

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