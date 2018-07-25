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
