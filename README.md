# README

![Docker Pulls](https://img.shields.io/docker/pulls/fawda123/sfbaytrends)

Materials for Shiny application to evaluate SF bay trends with [wqtrends](https://tbep-tech.github.io/wqtrends) R package

## Data

Files in `/data` were created in`R/dat_proc.R`

* `datprc.RData` Bay data for all stations, date columns added (`doy`, `dec_time`, etc.), parameter names to lower case, `s` to `sal`, filter by years (1990 - 2017), removed `d_chl`

* `locs.RData` sf object of station lat/lon, filtered by those in `datrpc`

* `map.RData` ggmap object for basemap 

* `mods_.*.RData` models files for each station, parameter combination
