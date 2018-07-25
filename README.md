# README

Materials for analysis of SF bay trends, see results [here](http://162.243.131.102:3838/SFbaytrends/mods.Rmd)

## Data

Files in `/data` were created in`R/dat_proc.R`

* `datprc.RData` Bay data for all stations, date columns added (`doy`, `dec_time`, etc.), parameter names to lower case, `s` to `sal`, filter by years (1990 - 2017), removed `d_chl`

* `locs.RData` sf object of station lat/lon, filtered by those in `datrpc`

* `map.RData` ggmap object for basemap 

* `modkcmp.RData` Summary data of comparison for many k values, gam1 and gam2, all stations

* `modssta.RData` Nested model data for all stations evaluating log-chl by different combinations of smoothers for season, time

