###### 
# data process

library(tidyverse)
library(lubridate)

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