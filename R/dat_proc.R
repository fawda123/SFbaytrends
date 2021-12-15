library(tidyverse)
library(lubridate)
library(sf)
library(wqtrends)

# station lat/lon as separate file ----------------------------------------

data(datprc)

locs <- read.csv('raw/usgs_stations.csv') %>%
  filter(Station %in% datprc$station)

save(locs, file = 'data/locs.RData', compress = 'xz')

# format raw wq data for use with wqtrends --------------------------------

# all raw files from DS
chlraw <- read.csv('data/raw/sfb_surf_CB_SB_LSB.csv', stringsAsFactors = F)
gppraw <- read.csv('data/raw/sfb_GPP_monthly.csv', stringsAsFactors = F) 
doraw <- read.csv('data/raw/CB_SB_LSB_depthavg_O2.csv', stringsAsFactors = F)

# get chlorophyll as ug l-1
chldat <- chlraw %>% 
  select(date, station, chl = chl_merge) %>% 
  gather('param', 'value', -date, -station) %>% 
  mutate(
    date = ymd(date),
    doy = yday(date), 
    cont_year = decimal_date(date),
    yr = year(date),
    mo = month(date, label = T),
    param = tolower(param)
  ) %>% 
  filter(!is.na(value))

# get gpp as mg C m-2 d-1
gppdat <- gppraw %>% 
  select(cont_year = dec_date, station, GPP) %>% 
  mutate(
    date = date_decimal(cont_year),
    date = as.Date(date),
    doy = yday(date),
    yr = year(date),
    mo = month(date, label = T)
  ) %>% 
  rename(
    gpp = GPP
  ) %>% 
  gather('param', 'value', gpp) %>% 
  select(date, station, param, value, doy, cont_year, yr, mo) %>% 
  filter(!is.na(value))

# light attenuation as m-1
kddat <- gppraw %>% 
  select(cont_year = dec_date, station, ext_merge) %>% 
  mutate(
    date = date_decimal(cont_year),
    date = as.Date(date),
    doy = yday(date),
    yr = year(date),
    mo = month(date, label = T)
  ) %>% 
  rename(
    kd = ext_merge
  ) %>% 
  gather('param', 'value', kd) %>% 
  select(date, station, param, value, doy, cont_year, yr, mo) %>% 
  filter(!is.na(value))

# get depth-averaged do (mg/l and % sat)
dodat <- doraw %>% 
  select(date, station, do, dosat = do_sat) %>% 
  gather('param', 'value', -date, -station) %>% 
  mutate(
    date = ymd(date),
    doy = yday(date), 
    cont_year = decimal_date(date),
    yr = year(date),
    mo = month(date, label = T),
    param = tolower(param)
  ) %>% 
  filter(!is.na(value))

# combine new do ests, gpp with datprc
datprc <- bind_rows(chldat, gppdat, dodat, kddat) %>% 
  arrange(station, param, date) %>% 
  filter(yr <= 2019)

# find the most recent year with less than five observations
# defaults to last year if all have at least five
yrflt <- datprc %>% 
  group_by(station, param, yr) %>% 
  summarise(cnt = n(), .groups= 'drop') %>% 
  group_by(station, param) %>% 
  arrange(station, param, -yr) %>% 
  mutate(
    thrsh = cnt < 5, 
    thrsh = cumsum(thrsh),
    toflt = case_when(
      thrsh == 1 & !duplicated(thrsh) ~ T,
      sum(thrsh) == 0 ~ c(rep(NA, length(thrsh) -1), T)
    )
  ) %>% 
  na.omit() %>% 
  ungroup() %>% 
  select(station, param, exclyr = yr)

# join with min year for filter
datprc <- datprc %>%
  left_join(yrflt, by = c('station', 'param')) %>%
  filter(yr > exclyr) %>%
  select(-exclyr)

# # combine new do ests, gpp with datprc
# datprc <- bind_rows(chldat, gppdat, dodat) %>% 
#   filter(yr >= 1990 & yr <= 2019) %>% 
#   arrange(station, param, date)
#
# rawdat <- datprc
# save(rawdat, file = '../wqtrends/data/rawdat.RData', compress = 'xz')

save(datprc, file = 'data/datprc.RData', compress = 'xz')

# save separate rdata model files for each station, parameter -------------

data(datprc)

# data to model, same as datprc, params in wide format, nested by station
# crossed with frms
tomod <- datprc %>% 
  filter(param %in% c('chl', 'do', 'dosat', 'gpp', 'kd')) %>% # add parameters here
  group_by(station, param) %>% 
  nest %>% 
  mutate(
    trans = case_when(
      param %in% c('chl', 'gpp', 'kd') ~ 'log10', 
      T ~ 'ident'
    )
  )

# create models for every station, gam model eval
modssta <- tomod %>%
  mutate(
    modi = purrr::pmap(list(station, param, trans, data), function(station, param, trans, data){
      
      cat(station, param, '\n')
      out <- anlz_gam(data, trans = trans)
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
