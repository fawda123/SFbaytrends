library(shiny)
library(wqtrends)
library(ggplot2)
library(ggmap)
library(lubridate)
library(dplyr)
library(here)
library(shinycssloaders)

load(file = here('data/datprc.RData'))
load(file = here('data/locs.RData'))
load(file = here('data/mapnew.RData'))
locs<-read.csv(here('data/petersonlocations.csv'))%>%
  filter(Station %in% unique(datprc$station))%>%
  rename(lat=Lat,
         lon=Long)%>%
  select(-Location)

omit<-read.csv(here('data/omit_lookup.csv'))
  

params <- list(
  'Chlorophyll-a (ug/L)' = 'chl',
  'GPP (mg C m-2 d-1)' = 'gpp',
  'DO (mg/L)' = 'do',
  'DO sat. (%)' = 'dosat',
  'Kd (m-1)' = 'kd',
  'DIN (uM)' = 'din'
)

scaledig <- function(x) sprintf("%.1f", x)
bssz <- 17

pbase <- ggmap(map) +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.background = element_rect(fill = 'transparent', color = NA)
  )



server <- function(input, output, session){
  
  # selected station location and raw time series
  mapselplo <- eventReactive(input$submit, {
  
    # inputs
    station <- input$station
  
    locpt <- locs %>%
      filter(Station %in% !!station)
  
    # map
    p1 <- pbase +
      geom_point(data = locpt, aes(x = lon, y = lat), colour = 'tomato1', size = 8) +
      geom_text(data = locs, aes(x = lon, y = lat, label = Station)) +
      theme(
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10)
        )
  
    out <- p1
  
    return(out)
  
  }, ignoreNULL = FALSE)
  
  # models for selected station
  mod <- reactive({
  
    # inputs
    parameter <- input$parameter
    station <- input$station
  
    fl <- paste0('mods_', parameter, station)
    pth <- here(paste0('data/', fl, '.RData'))
    
    validate(
      need(file.exists(pth), "No model available")
    )
    
    load(file = here(pth))
  
    out <- get(fl) %>%
      .$modi %>% 
      .[[1]]
    
    return(out)
  
  })
  
  # year range from model
  yrs <- reactive({
    
    mod <- mod()
    
    validate(
      need(!is.null(mod), "No model available")
    )
    
    yrs <- mod$model %>% pull(cont_year) %>% date_decimal(.) %>% year %>% range()
    
    return(yrs)
    
  })
  
  # y axis label
  ylb <- reactive({
  
    # input
    parameter <- input$parameter
  
    out <- params %in% parameter %>% which %>% params[.] %>% names
  
    return(out)
  
  })
  
  # series plot
  prdseries <- eventReactive(input$submit, {
    
    # inputs
    yrs <- yrs()
    mod <- mod()
    ylb <- ylb()
    yrrng <- input$yrrng
    station <- input$station
    parameter <- input$parameter
    
    # special handling of omit years depending on station parameter
    omyr<-omit%>%
      filter(station==input$station)
    ompar<-factor(strsplit(omyr$parameters,",")[[1]])
    
    if(omyr$station[1]==station & parameter %in% ompar){
      parts <- unlist(strsplit(omyr$years[1], ","))
      
      # Parse each part
      yromit <- unlist(lapply(parts, function(x) {
        if (grepl(":", x)) {
          # If part contains a colon, treat it as a range
          range_vals <- as.numeric(unlist(strsplit(x, ":")))
          return(seq(range_vals[1], range_vals[2]))
        } else {
          # Otherwise, treat as a single number
          return(as.numeric(x))
        }
      }))
    } else {
      yromit <- NULL
    }
    
    validate(
      need(!is.null(mod), "No model available")
    )
    
    if(is.null(yrrng))
      yrrng <- yrs
    
    xlim <- as.Date(c(paste0(yrrng[1], '-01', '-01'), paste0(yrrng[2], '-12', '-31')))
 
    out <- show_prdseries(mod, ylab = ylb, base_size = bssz, xlim = xlim,yromit = yromit) + 
      labs(
        title = "GAM fit",
        subtitle = "Points are observed values"
      ) 
    
    return(out)
    
  }, ignoreNULL = FALSE)
  
  # seasonal averages with trend
  mettrndseason <- eventReactive(input$submit, {
  
    # inputs
    station <- input$station
    parameter <- input$parameter
    yrrng <- input$yrrng
    yrs <- yrs()
    dytr <- input$dytr
    mod <- mod()
    ylb <- ylb()
    metsel <- input$metsel
    wntr <- input$wntr
    wnty <- input$wnty
    
    validate(
      need(!is.null(mod), "No model available")
    )
    
    # use ave if metsel is mean
    useave <- F
    if(metsel == 'mean')
      useave <- T
  
    if(is.null(yrrng))
      yrrng <- yrs
    
    # special handling of omit years depending on station parameter
    omyr<-omit%>%
      filter(station==input$station)
    ompar<-factor(strsplit(omyr$parameters,",")[[1]])
    
    if(omyr$station[1]==input$station & input$parameter %in% ompar){
      yromit <- paste0('c(', omyr$years, ')')
    } else {
      yromit <- NULL
    }
    
    
    # plot
    toprs <- paste0('show_mettrndseason(mod, metfun = ', metsel, ', doystr = ', dytr[1], ', doyend = ', dytr[2], 
                    ', ylab = "', ylb, '", nsim = 1e3, na.rm = TRUE, width = 0.6, size = 4, useave = ', useave, ', 
                    base_size = ', bssz, ', win = ', wntr, ', justify = "', wnty, '",
                    xlim = c(', yrrng[1], ', ', yrrng[2], '), yromit = ', yromit , ', 
                    nms = c("Increasing (p < 0.05)", "Decreasing (p < 0.05)", "No trend", "No estimate"))')

    out <- eval(parse(text = toprs))
  
    out <- out + 
      scale_y_continuous(labels = scaledig)
    
    return(out)
  
  }, ignoreNULL = FALSE)
  
  # seasonal averages
  trndseason <- eventReactive(input$submit, {
  
    # inputs
    station <- input$station
    parameter <- input$parameter
    yrs <- yrs()
    yrrng <- input$yrrng
    dytr <- input$dytr
    mod <- mod()
    ylb <- ylb()
    metsel <- input$metsel
    wntr <- input$wntr
    wnty <- input$wnty
  
    validate(
      need(!is.null(mod), "No model available")
    )
    
    if(is.null(yrrng))
      yrrng <- yrs
  
    # use ave if metsel is mean
    useave <- F
    if(metsel == 'mean')
      useave <- T

    # special handling of omit years depending on station parameter
    yromit <- NULL
    omyr<-omit%>%
      filter(station==input$station)
    ompar<-factor(strsplit(omyr$parameters,",")[[1]])
    
    if(omyr$station[1]==input$station & input$parameter %in% ompar){
      yromit <- paste0('c(', omyr$years, ')')
    } else {
      yromit <- NULL
    }
    
    out <- try({

      toprs <- paste0('show_trndseason(mod, metfun = ', metsel, ', doystr = ', dytr[1], ', doyend = ', dytr[2], 
                      ', ylab = "', ylb, '", nsim = 1e3, win = ', wntr, ', justify = "', wnty, '",
                      useave = ', useave, ', base_size = ', bssz, ', xlim = c(', yrrng[1], ', ', yrrng[2], '),
                      yromit = ', yromit, ', nms = c("Increasing (p < 0.05)", "Decreasing (p < 0.05)", "No trend"))')
      
      eval(parse(text = toprs))
      
      })
  
    validate(
      need(inherits(out, 'ggplot'), 'Pick different year range')
    )
  
    out <- out + 
      scale_y_continuous(labels = scaledig)
    
    return(out)
  
  }, ignoreNULL = FALSE)
  
  output$mapselplo <- renderPlot(mapselplo())
  output$prdseries <- renderPlot(prdseries())
  output$mettrndseason <- renderPlot(mettrndseason())
  output$trndseason <- renderPlot(trndseason())
  output$yrrng <- renderUI({

    yrs <- yrs()      
    sliderInput('yrrng', 'Plot year range (all):', min = yrs[1], max = yrs[2], value = yrs, sep = "", step = 1, width = '100%')
    
  })
  
  # make default seasonal range full year if input is GPP, otherwise make it aug to oct
  observeEvent(input$parameter, {
    
    # input
    parameter <- input$parameter
    
    # make default seasonal range full year if input is GPP
    if(parameter == 'gpp')
      updateSliderInput(session, 'dytr', value = c(1, 365))
    if(parameter == 'din')
      updateSliderInput(session, 'dytr', value = c(213, 273))
    if(!parameter %in% c('gpp', 'din'))
      updateSliderInput(session, 'dytr', value = c(213, 304))
    
  })
  
  # subembayment map click
  observeEvent(input$submmap1_marker_click, {
    
    # input
    potwsel2 <- input$submmap1_marker_click$id
    req(!is.null(potwsel2))
    submsel1 <- locs %>% filter(POTW == potwsel2) %>% select(sub_name) %>% unique() %>% pull()
    
    updateSelectInput(session, 'submsel1', selected = submsel1)
    
  })  

}

ui <- function(request) {
  fluidPage(
  
  # style file
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  titlePanel("Water quality trends in south San Francisco Bay"),
  
  p(HTML('This application uses data from long-term USGS water quality monitoring in San Francisco Bay (<a href="https://doi.org/10.5066/F7TQ5ZPR" target="_blank">Cloern and Schraga 2016</a>; <a href="https://doi.org/10.1038/sdata.2017.98" target="_blank">Schraga and Cloern 2017</a>; <a href="https://doi.org/10.5066/F7D21WGF" target="_blank">Schraga et al. 2020</a>). Additional background details on the GAM, mixed-effects meta-analysis approach are presented in <a href="https://doi.org/10.1016/j.scitotenv.2021.149927" target="_blank">Beck et al (2022)</a>.')),
  
  column(12, bookmarkButton()),

  column(12, 
    column(4,
      withSpinner(plotOutput('mapselplo')),
      actionButton('submit', 'Submit', width = '100%', style = 'color: white; background-color: rgb(66, 139, 202);'),
      selectInput("station", "Station (all):",  selected = "18", sort(unique(datprc$station)), width = '100%'),
      selectInput("parameter", "Parameter (all):", choices = params, width = '100%'),
      selectInput('wnty', 'Window type (middle, bottom):', choices = c('right', 'left', 'center'), width = '100%'),
      selectInput('metsel', 'Summary metric (middle, bottom):', choices = c('mean', 'min', 'max', 'var'), width = '100%'),
      uiOutput('yrrng'),
      sliderInput('wntr', 'Window size (middle, bottom):', min = 2, max = 25, value = 10, sep = "", step = 1, width = '100%'),
      sliderInput('dytr', 'Season (middle, bottom):', min = 1, max = 365, value = c(213, 304), width = '100%')
    ),
    column(8,
      withSpinner(plotOutput('prdseries', height = 320)),
      withSpinner(plotOutput('mettrndseason', height = 390)),
      withSpinner(plotOutput('trndseason', height = 320))
    )
  )
)
}
enableBookmarking(store = "url")
shinyApp(ui = ui, server = server)