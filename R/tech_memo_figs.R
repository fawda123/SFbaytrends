library(tidyverse)
library(lubridate)
library(mgcv)
library(shiny)
library(plotly)
library(ggmap)
library(gridExtra)
library(shinyWidgets)
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


# map ---------------------------------------------------------------------

p1 <- pbase + 
  geom_text(data = locs, aes(x = lon, y = lat, label = Station), size = 6)

pdf('figs/map.pdf', height = 6, width = 5, family = 'serif')
p1
dev.off()


# map and time series -----------------------------------------------------


