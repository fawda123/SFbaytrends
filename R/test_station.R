library(tidyverse)
library(mgcv)
library(lubridate)

data(datprc)

# station 18 chlorophyll
tomod <- datprc %>% 
  filter(param %in% 'chl') %>% 
  filter(station %in% '18')

# k for gam1, gam2 is 2 /3 * nyears, k for gam 6 is 12 * nyears
gam0 <- gam(log10(value) ~ dec_time + s(doy, bs = 'cc'), knots = list(doy = c(1, 366)), data = tomod, na.action = na.exclude, select = T) 
gam1 <- gam(log10(value) ~ dec_time + s(dec_time, k = 28) + s(doy, bs = 'cc'), knots = list(doy = c(1, 366)), data = tomod, na.action = na.exclude, select = T)
gam2 <- gam(log10(value) ~ dec_time + s(dec_time, k = 28) + s(doy, bs = 'cc') + ti(dec_time, doy, bs = c('tp', 'cc')), knots = list(doy = c(1, 366)), data = tomod, na.action = na.exclude, select = T)
gam6 <- gam(log10(value) ~ dec_time + s(dec_time, k = 336) + s(doy, bs = 'cc'), knots = list(doy = c(1, 366)), data = tomod, na.action = na.exclude, select = T)
# gamgam <- gam(log10(value) ~ dec_time + s(dec_time, k = 336) + s(doy, bs = 'cc'), knots = list(doy = c(1, 366)), gamma = 2, data = tomod, na.action = na.exclude, select = T)

# model predictions to plot
prdplo <- list(
  'gam0' = gam0, 
  'gam1' = gam1, 
  'gam2' = gam2, 
  'gam6' = gam6#,
  # 'gamgam' = gamgam
  ) %>% 
  enframe('modi', 'modv') %>% 
  mutate(
    prddat = map(modv, function(modv){
    
      prddat <- data.frame(
        dec_time = seq(min(tomod$dec_time), max(tomod$dec_time), length = 1000)
        ) %>%
          mutate(
            date = date_decimal(dec_time),
            date = as.Date(date),
            mo = month(date, label = TRUE),
            doy = yday(date),
            yr = year(date)
          )
      
      prd <- predict(modv, newdata = prddat)
      
      prddat <- prddat %>% mutate(value = prd)
      
      return(prddat)
    
    })
  ) %>% 
  select(modi, prddat) %>% 
  unnest(prddat)

# make a plot
ggplot(prdplo, aes(x = date)) + 
  geom_point(data = tomod, aes(y = log10(value)), size = 0.5) +
  geom_line(aes(y = value, colour = factor(modi)), size = 0.75, alpha = 0.8) + 
  stat_smooth(aes(y = value, group = modi), se = F, method = "loess", color = 'black', alpha = 0.7) +
  facet_wrap(~modi, ncol = 1) +
  scale_color_viridis_d() + 
  theme_bw(base_family = 'serif', base_size = 16) + 
  theme(
    legend.position = 'top', 
    legend.title = element_blank(),
    strip.text = element_blank(), 
    strip.background = element_blank()
  ) + 
  ylab('log10(chl)')
