######
# get legend from an existing ggplot object
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

######
# gam equivalent of dynaplot function
# looks at changes in in the chlorophyll-flow relationships by month over different years
# grd is the number of salinity values to use in predictions
dynagam <- function(mod_in, dat_in, cvr, grd = 30, years = NULL, alpha = 1,
                    size = 1, col_vec = NULL, allflo = FALSE, month = c(1:12), scales = NULL, ncol = NULL, 
                    pretty = TRUE, grids = TRUE, use_bw = TRUE, fac_nms = NULL,
                    cols = RColorBrewer::brewer.pal(11, 'Spectral')){

  # add year, month columns to dat_in
  dat_in <- mutate(dat_in, 
                   month = as.numeric(strftime(date, '%m')), 
                   year = as.numeric(strftime(date, '%Y'))
  )
  to_plo <- dat_in
  
  # assign name of covariate in input data with generic flo
  names(to_plo)[names(to_plo) %in% cvr] <- 'flo'
  names(dat_in)[names(dat_in) %in% cvr] <- 'flo'
  
  # flo values to predict
  flo_vals <- range(to_plo[, 'flo'], na.rm = TRUE)
  flo_vals <- seq(flo_vals[1], flo_vals[2], length = grd)
  
  # get model predictions across range of flow values
  dynadat <- rep(flo_vals, each = nrow(to_plo)) %>% 
    matrix(., nrow = nrow(to_plo), ncol = grd) %>% 
    cbind(to_plo[, c('dec_time', 'doy')], .) %>%
    gather('split', 'flo', -dec_time, -doy) %>% 
    select(-split)
  names(dynadat)[names(dynadat) %in% 'flo'] <- cvr
  dynadat <- data.frame(dynadat, res = predict(mod_in, newdata = dynadat)) 
  names(dynadat)[names(dynadat) %in% cvr] <- 'flo'
  dynadat <- dynadat %>% 
    spread(flo, res) %>% 
    select(-doy)
  dynadat <- dynadat %>% 
    mutate(
      date = date_decimal(dec_time),
      year = year(date),
      month = month(date)
    ) %>%  
    select(-date, -dec_time) %>% 
    gather('flo', 'res', -year, -month) %>% 
    mutate(flo = as.numeric(as.character(flo)))
  
  to_plo <- dynadat
  
  # subset years to plot
  if(!is.null(years)){
    
    to_plo <- to_plo[to_plo$year %in% years, ]
    to_plo <- to_plo[to_plo$month %in% month, ]
    
    if(nrow(to_plo) == 0) stop('No data to plot for the date range')
    
  }
  
  # constrain plots to salinity limits for the selected month
  if(!allflo){
    
    #min, max salinity values to plot
    lim_vals <- group_by(data.frame(dat_in), month) %>% 
      summarise(
        Low = quantile(flo, 0.05, na.rm = TRUE),
        High = quantile(flo, 0.95, na.rm = TRUE)
      )
    
    # month flo ranges for plot
    lim_vals <- lim_vals[lim_vals$month %in% month, ]
    
    # merge limts with months
    to_plo <- left_join(to_plo, lim_vals, by = 'month')
    to_plo <- to_plo[to_plo$month %in% month, ]
    
    # reduce data
    sel_vec <- with(to_plo, 
                    flo >= Low &
                      flo <= High
    )
    to_plo <- to_plo[sel_vec, !names(to_plo) %in% c('Low', 'High')]
    to_plo <- arrange(to_plo, year, month)
    
  } else {
    
    to_plo <- to_plo[to_plo$month %in% month, ]
    
  }
  
  # reshape data frame, average by year, month for symmetry
  to_plo <- group_by(to_plo, year, month, flo) %>% 
    summarise(
      res = mean(res, na.rm = TRUE)
    )
  
  # months labels as text
  mo_lab <- data.frame(
    num = seq(1:12), 
    txt = c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')
  )
  mo_lab <- mo_lab[mo_lab$num %in% month, ]
  to_plo$month <- factor(to_plo$month, levels =  mo_lab$num, labels = mo_lab$txt)
  
  # reassign facet names if fac_nms is provided
  if(!is.null(fac_nms)){
    
    if(length(fac_nms) != length(unique(to_plo$month))) stop('fac_nms must have same lengths as months')
    
    to_plo$month <- factor(to_plo$month, labels = fac_nms)
    
  }
  
  
  # make plot
  p <- ggplot(to_plo, aes(x = flo, y = res, group = year)) + 
    facet_wrap(~month, ncol = ncol, scales = scales)
  
  # return bare bones if FALSE
  if(!pretty) return(p + geom_line())
  
  # chllab function from WRTDStidal
  ylabel <- 'log10(chl)'
  
  # use bw theme
  if(use_bw) p <- p + theme_bw(base_family = 'serif', base_size = 16)
  
  p <- p + 
    geom_line(size = size, aes(colour = year), alpha = alpha) +
    scale_y_continuous(ylabel, expand = c(0, 0)) +
    scale_x_continuous(cvr, expand = c(0, 0)) +
    theme(
      legend.position = 'top'
    ) +
    scale_colour_gradientn('Year', colours = cols) +
    guides(colour = guide_colourbar(barwidth = 10)) 
  
  # remove grid lines
  if(!grids) 
    p <- p + 
    theme(      
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  return(p)
  
}