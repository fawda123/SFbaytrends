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
RUN R -e "install.packages('rmarkdown', repos='http://cran.rstudio.com/')"
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
RUN sudo mkdir /srv/shiny-server/SFbaytrends
COPY google-analytics.js /srv/shiny-server/SFbaytrends
COPY kable.css /srv/shiny-server/SFbaytrends
COPY index.Rmd /srv/shiny-server/SFbaytrends
COPY /R /srv/shiny-server/SFbaytrends/R
COPY /data /srv/shiny-server/SFbaytrends/data

# select port
EXPOSE 3838

# allow permission
RUN chown shiny:shiny /var/lib/shiny-server/

COPY shiny-server.sh /usr/bin/shiny-server.sh
RUN ["chmod", "+x", "/usr/bin/shiny-server.sh"]

CMD ["/usr/bin/shiny-server.sh"]