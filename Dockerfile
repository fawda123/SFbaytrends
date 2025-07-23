# get rocker geospatial image
FROM rocker/geospatial:latest

# install shiny server
RUN /rocker_scripts/install_shiny_server.sh

# system libraries of general use
RUN apt-get update && apt-get install -y \
  sudo \
  pandoc
  
# install R packages required 
RUN install2.r --error --repos 'http://cran.rstudio.com/' \
  data.table DT ggmap gridExtra here leaflet mgcv patchwork plotly readr \
  remotes rmarkdown scales sf sfheaders shinycssloaders shinyWidgets tidyquant


# install specific version of package
RUN R -e "remotes::install_version('flexdashboard', '0.5.2')"
RUN R -e "remotes::install_version('ggplot2', '3.4.4')"

# install github packages
RUN R -e "remotes::install_github('fawda123/CTDplot', upgrade = 'never')"
RUN R -e "remotes::install_github('tbep-tech/wqtrends', upgrade = 'always')"

# select port
EXPOSE 3838

# allow permission
RUN chown shiny:shiny /var/lib/shiny-server/

COPY shiny-server.sh /usr/bin/shiny-server.sh
RUN ["chmod", "+x", "/usr/bin/shiny-server.sh"]

CMD ["/usr/bin/shiny-server.sh"]