library(tidyverse)
library(patchwork)

tmp <- datprc %>% 
  filter(station == 21) %>% 
  filter(param == 'sal') %>% 
  mutate(
    mvavg = stats::filter(value, filter = rep(1 , 24)/24, sides = 1) %>% as.numeric,  
    diff = value - mvavg
  ) %>% 
  dplyr::select(date, value, mvavg, diff)

p1 <- ggplot(tmp, aes(x = date, y = value)) + 
  geom_line() + 
  # geom_line(aes(y = mvavg), col = 'blue') + 
  theme_bw() + 
  labs(y = 'Observed salinity') + 
  theme(axis.title.x = element_blank())

p2 <- ggplot(tmp, aes(x = date, y = mvavg)) + 
  geom_line() +
  geom_segment(aes(xend = date, yend = value)) +
  theme_bw() + 
  theme(axis.title.x = element_blank()) +
  labs(y = '2 year moving average\nwith difference')

p1 + p2 + plot_layout(ncol = 1)
