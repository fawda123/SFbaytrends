library(shiny)
library(wqtrends)
library(ggplot2)
library(ggmap)
library(lubridate)
library(dplyr)
library(here)
library(shinyWidgets)

load(file = here('data/datprc.RData'))
load(file = here('data/locs.RData'))
load(file = here('data/map.RData'))

params <- list(
  'Chlorophyll-a (ug/L)' = 'chl',
  'GPP (mg C m-2 d-1)' = 'gpp',
  'DO (mg/L)' = 'do',
  'DO sat. (%)' = 'dosat',
  'Kd (m-1)' = 'kd'
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
    load(file = here(paste0('data/', fl, '.RData')))
  
    out <- get(fl) %>%
      .$modi %>% 
      .[[1]]
    
    return(out)
  
  })
  
  # year range from model
  yrs <- reactive({
    
    mod <- mod()
    
    req(mod)
    
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
    
    if(is.null(yrrng))
      yrrng <- yrs
    
    xlim <- as.Date(c(paste0(yrrng[1], '-01', '-01'), paste0(yrrng[2], '-12', '-31')))
 
    out <- show_prdseries(mod, ylab = ylb, base_size = bssz, xlim = xlim) + 
      labs(
        title = "GAM fit",
        subtitle = "Points are observed values"
      ) 
    
    return(out)
    
  }, ignoreNULL = FALSE)
  
  # seasonal averages with trend
  mettrndseason <- eventReactive(input$submit, {
  
    # inputs
    yrrng <- input$yrrng
    yrs <- yrs()
    dytr <- input$dytr
    mod <- mod()
    ylb <- ylb()
    metsel <- input$metsel
    wntr <- input$wntr
    wnty <- input$wnty
    
    req(mod)
    # use ave if metsel is mean
    useave <- F
    if(metsel == 'mean')
      useave <- T
  
    if(is.null(yrrng))
      yrrng <- yrs
    
    # plot
    toprs <- paste0('show_mettrndseason(mod, metfun = ', metsel, ', doystr = ', dytr[1], ', doyend = ', dytr[2], 
                    ', ylab = "', ylb, '", nsim = 1e3, na.rm = TRUE, width = 0.6, size = 4, useave = ', useave, ', 
                    base_size = ', bssz, ', win = ', wntr, ', justify = "', wnty, '",
                    xlim = c(', yrrng[1], ', ', yrrng[2], '))')
    
    out <- eval(parse(text = toprs))
  
    out <- out + 
      scale_y_continuous(labels = scaledig)
    
    return(out)
  
  }, ignoreNULL = FALSE)
  
  # seasonal averages
  trndseason <- eventReactive(input$submit, {
  
    # inputs
    yrs <- yrs()
    yrrng <- input$yrrng
    dytr <- input$dytr
    mod <- mod()
    ylb <- ylb()
    metsel <- input$metsel
    wntr <- input$wntr
    wnty <- input$wnty
  
    if(is.null(yrrng))
      yrrng <- yrs
  
    # use ave if metsel is mean
    useave <- F
    if(metsel == 'mean')
      useave <- T

    out <- try({

      toprs <- paste0('show_trndseason(mod, metfun = ', metsel, ', doystr = ', dytr[1], ', doyend = ', dytr[2], 
                      ', ylab = "', ylb, '", nsim = 1e3, win = ', wntr, ', justify = "', wnty, '",
                      useave = ', useave, ', base_size = ', bssz, ', xlim = c(', yrrng[1], ', ', yrrng[2], '))')
      
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

}

ui <- fluidPage(
  
  # changes overall page width
  tags$head(tags$style(type="text/css", ".container-fluid {  max-width: 1200px;};")),
  
  titlePanel("Water quality trends in south San Francisco Bay"),
  
  p(HTML('This application uses data from long-term USGS water quality monitoring in San Francisco Bay (<a href="https://doi.org/10.5066/F7TQ5ZPR" target="_blank">Cloern and Schraga 2016</a>; <a href="https://doi.org/10.1038/sdata.2017.98" target="_blank">Schraga and Cloern 2017</a>; <a href="https://doi.org/10.5066/F7D21WGF" target="_blank">Schraga et al. 2020</a>). Additional background details on the GAM, mixed-effects meta-analysis approach are presented in <a href="https://doi.org/10.1016/j.scitotenv.2021.149927" target="_blank">Beck et al (2022)</a>.')),

  column(12, 
    column(4,
      addSpinner(plotOutput('mapselplo')),
      actionButton('submit', 'Submit', width = '100%', style = 'color: white; background-color: rgb(66, 139, 202);'),
      selectInput("station", "Station (all):", sort(unique(datprc$station)), width = '100%'),
      selectInput("parameter", "Parameter (all):", choices = params, width = '100%'),
      selectInput('wnty', 'Window type (middle, bottom):', choices = c('right', 'left', 'center'), width = '100%'),
      selectInput('metsel', 'Summary metric (middle, bottom):', choices = c('mean', 'min', 'max', 'var'), width = '100%'),
      uiOutput('yrrng'),
      sliderInput('wntr', 'Window size (middle, bottom):', min = 2, max = 25, value = 10, sep = "", step = 1, width = '100%'),
      sliderInput('dytr', 'Season (middle, bottom):', min = 1, max = 365, value = c(213, 304), width = '100%')
    ),
    column(8,
      addSpinner(plotOutput('prdseries', height = 320)),
      addSpinner(plotOutput('mettrndseason', height = 390)),
      addSpinner(plotOutput('trndseason', height = 320))
    )
  )
)

shinyApp(ui = ui, server = server)