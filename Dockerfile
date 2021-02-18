# get shiny serves plus tidyverse packages image
FROM rocker/shiny-verse:latest

# system libraries of general use
RUN apt-get update && apt-get install -y \
  sudo \
  pandoc \
  pandoc-citeproc \
  libcurl4-gnutls-dev \
  libcairo2-dev \
  libxt-dev \
  libssl-dev \
  libssh2-1-dev 


# install R packages required 
RUN R -e "install.packages('shiny', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('tidyverse', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('lubridate', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('remotes', repos='http://cran.rstudio.com/')"
RUN R -e "remotes::install_github('tbep-tech/wqtrends')"
RUN R -e "install.packages('ggmap', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinyWidgets', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('mgcv', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('plotly', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('gridExtra', repos='http://cran.rstudio.com/')"


# copy the app to the image
COPY SFbaytrends.Rproj /srv/shiny-server/
COPY kable.css /srv/shiny-server/
COPY app.R /srv/shiny-server/
COPY R /srv/shiny-server/R
COPY data /srv/shiny-server/data

# select port
EXPOSE 3838

# allow permission
RUN sudo chown -R shiny:shiny /srv/shiny-server

# run app
CMD ["/usr/bin/shiny-server.sh"]