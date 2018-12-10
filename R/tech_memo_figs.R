library(tidyverse)
library(lubridate)
library(mgcv)
library(ggmap)
library(gridExtra)
library(viridis)

source('R/funcs.R')

data(datprc)
data(modssta)
data(locs)
data(map)
data(modkcmp)

# ext <- make_bbox(locs$lon, locs$lat, f = 0.2)
# map <- get_stamenmap(ext,  zoom = 11, maptype = "toner-lite", where = getwd())
pbase <- ggmap(map) +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

# create plots for whole time series, by year, by month
# all variables, stations
rawplo <- datprc %>% 
  filter(param %in% 'chl') %>% 
  group_by(station) %>% 
  nest %>% 
  mutate(
    tot = map(data, function(x){
      
      p <- ggplot(x, aes(x = dec_time, y = value)) + 
        geom_line() +
        theme_bw(base_family = 'serif') +
        theme(strip.background = element_blank())
      
      return(p)
      
    }),
    yrs = map(data, function(x){
      
      p <- ggplot(x, aes(x = factor(yr), y = value)) + 
        geom_boxplot() +
        theme_bw(base_family = 'serif') +
        scale_x_discrete("Year") +
        theme(strip.background = element_blank())
      
      return(p)
      
    }), 
    mos = map(data, function(x){
      
      p <- ggplot(x, aes(x = mo, y = value)) + 
        geom_boxplot() +
        theme_bw() +
        theme(strip.background = element_blank())
      
      return(p)
      
    })
  )

# map ---------------------------------------------------------------------

p1 <- pbase + 
  geom_text(data = locs, aes(x = lon, y = lat, label = Station), size = 6)

pdf('figs/map.pdf', height = 6, width = 5, family = 'serif')
p1
dev.off()

# map and time series -----------------------------------------------------

# inputs
station <- 32

toplo <- rawplo %>% 
  filter(station %in% !!station) %>% 
  select(matches('tot')) %>% 
  .[[1]] %>% 
  .[[1]] + scale_y_continuous('chl') + 
  theme(axis.title.x = element_blank())

locpt <- locs %>% 
  filter(Station %in% !!station)

p1 <- pbase + 
  geom_point(data = locpt, aes(x = lon, y = lat), colour = 'tomato1', size = 8) +
  geom_text(data = locs, aes(x = lon, y = lat, label = Station))

pdf('figs/map_and_ts.pdf', height = 4, width = 11.5, family = 'serif')
grid.arrange(p1, toplo, widths = c(0.3, 0.7), ncol = 2)
dev.off()

# station 32 four mods ----------------------------------------------------

# inputs
station <- 32

mods <- modssta %>% filter(station %in% !!station)

prdplo <- mods %>%
  mutate(prddat = pmap(list(data, modv), function(data, modv){
    
    
    prddat <- data.frame(
      dec_time = seq(min(data$dec_time), max(data$dec_time), length = 1000)
    ) %>%
      mutate(
        date = date_decimal(dec_time),
        date = as.Date(date),
        mo = month(date, label = TRUE),
        doy = yday(date),
        yr = year(date)
      )
    
    prd <- predict(modv, newdata = prddat)
    
    prddat <- prddat %>% mutate(chl = prd)
    
    return(prddat)
    
  })) %>% 
  select(modi, prddat) %>% 
  unnest

rawchl <- mods$data[[1]] %>% 
  mutate(
    day = day(date)
  )

# plot
p <- ggplot(prdplo, aes(x = date)) + 
  geom_point(data = rawchl, aes(y = log10(chl)), size = 0.5) +
  geom_line(aes(y = chl, colour = factor(modi)), size = 0.75, alpha = 0.8) + 
  stat_smooth(aes(y = chl, group = modi), se = F, method = "loess", color = 'black', alpha = 0.7) +
  facet_wrap(~modi, ncol = 1) +
  scale_color_viridis_d() + 
  theme_bw(base_family = 'serif', base_size = 16) + 
  theme(
    legend.position = 'top', 
    legend.title = element_blank(),
    strip.text = element_blank(), 
    strip.background = element_blank()
  )

pdf('figs/station32_raw_pred.pdf', height = 9, width = 8, family = 'serif')
p
dev.off()

# station32 mod by doy ----------------------------------------------------

p <- ggplot(prdplo, aes(x = doy, group = factor(yr), colour = yr)) + 
  geom_line(aes(y = chl)) + 
  theme_bw(base_family = 'serif', base_size = 16) + 
  theme(
    legend.position = 'top', 
    legend.title = element_blank()
  ) + 
  scale_color_viridis_c() + 
  scale_y_continuous('log10(chl)') +
  facet_wrap(~ modi, ncol = 4) +
  guides(colour = guide_colourbar(barheight = 1, barwidth = 20))

pdf('figs/station32_doy_pred.pdf', height = 5, width = 11, family = 'serif')
p
dev.off()

# station18 mod by doy ----------------------------------------------------

# inputs
station <- 18

mods <- modssta %>% filter(station %in% !!station)

prdplo <- mods %>%
  mutate(prddat = pmap(list(data, modv), function(data, modv){
    
    
    prddat <- data.frame(
      dec_time = seq(min(data$dec_time), max(data$dec_time), length = 1000)
    ) %>%
      mutate(
        date = date_decimal(dec_time),
        date = as.Date(date),
        mo = month(date, label = TRUE),
        doy = yday(date),
        yr = year(date)
      )
    
    prd <- predict(modv, newdata = prddat)
    
    prddat <- prddat %>% mutate(chl = prd)
    
    return(prddat)
    
  })) %>% 
  select(modi, prddat) %>% 
  unnest

p <- ggplot(prdplo, aes(x = doy, group = factor(yr), colour = yr)) + 
  geom_line(aes(y = chl)) + 
  theme_bw(base_family = 'serif', base_size = 16) + 
  theme(
    legend.position = 'top', 
    legend.title = element_blank()
  ) + 
  scale_color_viridis_c() + 
  scale_y_continuous('log10(chl)') +
  facet_wrap(~ modi, ncol = 4) +
  guides(colour = guide_colourbar(barheight = 1, barwidth = 20))

pdf('figs/station18_doy_pred.pdf', height = 5, width = 11, family = 'serif')
p
dev.off()

# station18 gam1* by month --------------------------------------------------

# input
station <- 18
modi <- 'gam1*'

mods <- modssta %>% filter(station %in% !!station)

rawchl <- mods$data[[1]] %>% 
  mutate(
    day = day(date)
  )

prdplost <- mods %>% 
  filter(modi %in% !!modi) %>%
  mutate(prddat = pmap(list(data, modv), function(data, modv){
    
    data <- data$date %>% 
      range(na.rm  = T)
    data <- seq.Date(floor_date(data[1], 'year'), ceiling_date(data[2], 'year'), by = 'days') %>% 
      tibble(date = .) %>% 
      mutate(
        doy = yday(date), 
        dec_time = decimal_date(date),
        yr = year(date)
      )
    prd <- predict(modv, newdata = data)
    out <- data.frame(data, chl = prd)
    
    return(out)
    
  })
  
  ) %>% 
  select(modi, prddat) %>% 
  unnest %>% 
  select(-modi)

toplo <- prdplost %>% 
  mutate(day = day(date)) %>% 
  filter(day %in% c(1)) %>%
  mutate(month = month(date, label = T))

# plot
p <- ggplot(toplo, aes(x = date)) + 
  geom_point(data = rawchl, aes(y = log10(chl)), size = 0.5) +
  geom_line(aes(y = chl, group = month, colour = month), size = 1, alpha = 0.7) + 
  theme_bw(base_family = 'serif', base_size = 16) + 
  theme(
    legend.position = 'top'
  ) 
p

pdf('figs/station18_gam1st_by_month.pdf', height = 5, width = 11, family = 'serif')
p
dev.off()

# gam2 preds multiple stations --------------------------------------------

# input
station <- c(18, 21, 24, 27, 32, 36)
modi <- 'gam2'

mods <- modssta %>% filter(station %in% !!station)

rawchl <- mods %>% 
  filter(modi %in% !!modi) %>% 
  mutate(
    station = paste0('s', station),
    data = purrr::map(data, function(x){
      x %>% 
        mutate(
          day = day(date)
        )
    })
  ) %>% 
  select(station, data) %>% 
  unnest

prdplo <- mods %>%
  filter(modi %in% !!modi) %>% 
  mutate(prddat = pmap(list(data, modv), function(data, modv){
    
    prddat <- data.frame(
      dec_time = seq(min(data$dec_time), max(data$dec_time), length = 1000)
    ) %>%
      mutate(
        date = date_decimal(dec_time),
        date = as.Date(date),
        mo = month(date, label = TRUE),
        doy = yday(date),
        yr = year(date)
      )
    
    prd <- predict(modv, newdata = prddat)
    
    prddat <- prddat %>% mutate(chl = prd)
    
    return(prddat)
    
  })) %>% 
  select(station, prddat) %>% 
  unnest %>% 
  mutate(station = paste0('s', station))

# plot
p <- ggplot(prdplo, aes(x = date)) + 
  geom_point(data = rawchl, aes(y = log10(chl)), size = 0.5) +
  geom_line(aes(y = chl), colour = 'skyblue', size = 0.75, alpha = 0.8) + 
  stat_smooth(aes(y = chl, group = modi), se = F, method = "loess", color = 'black', alpha = 0.7) +
  facet_wrap(~station, ncol = 1, scales = 'free_y') +
  theme_bw(base_family = 'serif', base_size = 16) + 
  theme(
    legend.position = 'top', 
    legend.title = element_blank(),
    # strip.text = element_blank(), 
    strip.background = element_blank(), 
    axis.title.x = element_blank()
  )

pdf('figs/manystations_raw_pred.pdf', height = 13, width = 9, family = 'serif')
p
dev.off()

# gam2 preds doy multiple stations --------------------------------------------

p <- ggplot(prdplo, aes(x = doy, group = factor(yr), colour = yr)) + 
  geom_line(aes(y = chl)) + 
  theme_bw(base_family = 'serif', base_size = 16) + 
  theme(
    legend.position = 'top', 
    legend.title = element_blank(), 
    axis.title.x= element_blank(), 
    strip.background = element_blank()
  ) + 
  scale_color_viridis_c() + 
  scale_y_continuous('log10(chl)') +
  facet_wrap(~ station, ncol = 3, scales = 'free_y') +
  guides(colour = guide_colourbar(barheight = 1, barwidth = 20))

pdf('figs/manystations_doy_pred.pdf', height = 7, width = 10, family = 'serif')
p
dev.off()

# station 32, 18 gam2 by month --------------------------------------------

# input
station <- c(18, 32)
modi <- 'gam2'

mods <- modssta %>% filter(station %in% !!station)

rawchl <- mods %>% 
  filter(modi %in% !!modi) %>% 
  mutate(
    station = paste0('s', station),
    data = purrr::map(data, function(x){
      x %>% 
        mutate(
          day = day(date)
        )
    })
  ) %>% 
  select(station, data) %>% 
  unnest

prdplost <- mods %>% 
  filter(modi %in% !!modi) %>%
  mutate(prddat = pmap(list(data, modv), function(data, modv){
    
    data <- data$date %>% 
      range(na.rm  = T)
    data <- seq.Date(floor_date(data[1], 'year'), ceiling_date(data[2], 'year'), by = 'days') %>% 
      tibble(date = .) %>% 
      mutate(
        doy = yday(date), 
        dec_time = decimal_date(date),
        yr = year(date)
      )
    prd <- predict(modv, newdata = data)
    out <- data.frame(data, chl = prd)
    
    return(out)
    
  })
  
  ) %>% 
  select(station, prddat) %>% 
  unnest %>% 
  mutate(station = paste0('s', station))

toplo <- prdplost %>% 
  mutate(day = day(date)) %>% 
  filter(day %in% c(1)) %>%
  mutate(month = month(date, label = T))

# plot
p <- ggplot(toplo, aes(x = date)) + 
  geom_point(data = rawchl, aes(y = log10(chl)), size = 0.5) +
  geom_line(aes(y = chl, group = month, colour = month), size = 1, alpha = 0.7) + 
  theme_bw(base_family = 'serif', base_size = 16) + 
  theme(
    legend.position = 'top', 
    axis.title.x= element_blank(),
    strip.background = element_blank()
  ) +
  facet_wrap(~station, ncol = 1, scales = 'free_y')
p

pdf('figs/station1832_gam2_by_month.pdf', height = 8, width = 11, family = 'serif')
p
dev.off()
