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
                    cols = RColorBrewer::brewer.pal(11, 'Spectral'), prdout = FALSE){

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
    ) 
  
  if(prdout) return(dynadat)
  
  to_plo <- dynadat %>%  
    select(-date, -dec_time) %>% 
    gather('flo', 'res', -year, -month) %>% 
    mutate(flo = as.numeric(as.character(flo)))
  
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

######
# estimate percent change trends for selected time periods
# taken from baytrends gamDiff function
# 
# @param modv gam model
# @param base.yr.set vector of starting years, defaults to first year plus one
# @param test.yr.set vector of ending years, default to last year minus one
# @param logspace logical if response variable from model is in logspace (base 10)
gamdiff <- function(modv, base.yr.set = NA, test.yr.set = NA, logspace = TRUE){
  
  # input data used to create model
  gamdat <- modv$model
  
  # get date range, center year
  por.rng    <- gamdat$dec_time %>% 
    date_decimal(tz = 'Pacific/Pitcairn') %>% 
    range
  por.rng <- lubridate::year(por.rng)
  
  centerYear <- gamdat$dec_time %>% 
    range %>% 
    mean
  
  # estimated values are on the fifteenth of every month for each year of comparisons
  doy.set <- c(15, 46, 75, 106, 136, 167, 197, 228, 259, 289, 320, 350)
  
  # set up base and test years to first two and last two records if base.yr.set
  # and/or test.yr.set not specified; otherwise concatenate the values
  if(is.na(base.yr.set[1])) {base.yr.set <-  c(por.rng[1],por.rng[1]+1)}
  if(is.na(test.yr.set[1])) {test.yr.set <-  c(por.rng[2]-1,por.rng[2])}
  Nbase.yr <- length(base.yr.set)       # count years in each period
  Ntest.yr <- length(test.yr.set)
  yr.set <- c(base.yr.set,test.yr.set)  # combine base years and test years
  
  Ndoy  <- length(doy.set) # count predictions per year
  Nbase <- Ndoy*Nbase.yr   # calculate total predictions in base period
  Ntest <- Ndoy*Ntest.yr   # calculate total predictions in test period
  
  # create a data frame with Nrow rows where Nrow= (Nbase*Ndoy) + (Ntest*Ndoy)...
  # Nbase yrs of Ndoy baseline dates and Ntest-yrs
  # of Ndoy current dates. Include: doy, year, logical field (bl) indicating baseline
  # and current, and centered decimal year (dec_time) using same centering value
  # computed from data set (centerYear)
  pdat        <- expand.grid(doy.set,yr.set)       # make df with all comb. of doy.set and yr.set
  names(pdat) <- c('doy','year')                   # rename variables
  pdat$bl     <- pdat$year <= base.yr.set[Nbase.yr] # create logical field indicating baseline
  pdat$dec_time  <- (pdat$year + (pdat$doy-1)/366) # compute dec_time
  
  # JBH(24Nov2017): extension of above xa and avg.per.mat
  #   keeping weight the same--just extending number of values by "*nrow(pdatWgt)"
  xa <- c(rep(1/Nbase,Nbase),
          rep(0,Ntest),
          rep(0,Nbase),
          rep(1/Ntest,Ntest)) # construct a matrix to average baseline and current periods
  avg.per.mat <- matrix(xa,nrow=2,ncol=(Nbase+Ntest), byrow=TRUE)
  
  # construct matrix to get difference of current minus baseline
  diff.mat <- c(-1,1)
  
  # Extract coefficients (number of terms depends on complexity of GAM formula)
  beta    <- modv$coefficients        # coefficients vector
  VCmat   <- modv$Vp                  # variance-covariance matrix of coefficents
  
  # Begin calculations to compute differences ####
  # extract matrix of linear predicters
  Xpc     <- predict(modv,newdata=pdat,type="lpmatrix")
  
  # Compute predictions based on linear predictors (Nrow x 1 matrix)
  pdep    <- Xpc%*%beta               # equivalent to "predict(gamRslt,newdata=pdatLong)"
  
  # Calc. average baseline and average current; stores as a 2x1 matrix
  period.avg <- avg.per.mat %*% pdep
  
  # Calc. average current - average baseline; stores as a 1x1 matrix
  diff.avg  <- diff.mat %*% period.avg # pre-multiply by differencing matrix to check results
  
  # Calc standard errors and confidence intervals on difference predictions
  xpd       <- diff.mat%*%avg.per.mat%*%Xpc # premultiply linear predictors by averaging and differencing matrices.
  diff.est  <- xpd%*%beta                   # compute estimate of difference
  diff.se   <- sqrt(xpd%*%VCmat%*%t(xpd))   # compute Std. Err. by usual rules
  diff.t    <- diff.est / diff.se
  diff.pval <- 2*pt(abs(diff.t), modv$df.null, 0, lower.tail = FALSE)

  #compute CI for differnce
  alpha <- 0.05
  halpha    <- alpha/2
  diff.ci   <- c(diff.est - qnorm(1-halpha) * diff.se,diff.est + qnorm(1-halpha) *diff.se)
  
  # observed units, backtransform if needed
  per.mn.obs <- period.avg
  if(logspace)
    per.mn.obs <- 10^(per.mn.obs) 
  
  # calculate percent change (03Nov)
  pct.chg <- 100*((per.mn.obs[2] - per.mn.obs[1])/per.mn.obs[1])
  
  # difference in obs units (03Nov)
  diff.est.obs <- per.mn.obs[2] - per.mn.obs[1]
  
  # pack up and return results ####
  gamDiff.tmp <- list(
                      pdat = pdat, # data used for prediction to get estimates
                      base.yr    = base.yr.set, # years in first baseline set
                      test.yr    = test.yr.set, # years in second baselin set
                      per.mn     = as.vector(period.avg), # means by period, analyzed units
                      per.mn.obs = as.vector(per.mn.obs), # means by period, observed units
                      pct.chg    = pct.chg, # percent change, back-transformed
                      diff.est   = diff.est, # difference between periods, analyzed units
                      diff.est.obs = diff.est.obs, # difference betwwn periods, observed units
                      diff.se    = diff.se, # standard errors
                      diff.ci    = diff.ci, # confidence intervals
                      diff.t     = diff.t, # t stat
                      diff.pval  = diff.pval # p value
  )
  
  return(gamDiff.tmp)
  
}

# function for formatting p-values in tables
p_ast <- function(x){
  
  sig_cats <- c('p < 0.001', 'p < 0.01', 'p < 0.05', 'ns')
  sig_vals <- c(-Inf, 0.001, 0.01, 0.05, Inf)
  
  out <- cut(x, breaks = sig_vals, labels = sig_cats, right = FALSE)
  out <- as.character(out)
  
  return(out)
  
}

######
# extract period averages from Perry's function
extractPeriodAverages <- function(fit, data, doy.start = 1, doy.end = 365) {
  
  # prep prediciton data
  numDays <- doy.end - doy.start + 1
  data$julian <- julian(data$date)
  fillData <- data.frame(julian = min(data$julian):max(data$julian))
  fillData$date <- as.Date(fillData$julian, origin = as.Date("1970-01-01"))
  fillData$yr <- year(fillData$date)
  fillData$fyr <- factor(fillData$yr)
  fillData$doy <- fillData$julian - julian(update(fillData$date, month = 1, mday = 1))
  fillData$dec_time <- fillData$yr + (fillData$doy - 1)/366
  centerYear <- mean(range(fillData$dec_time, na.rm=FALSE))
  fillData$cyr <- fillData$dec_time - centerYear
  
  ## Exclude days not in the desired range
  fillData <- subset(fillData, doy >= doy.start & doy <= doy.end)
  
  ## Exclude years that do not include all relevant days (i.e. start or end year)
  dayCounts <- table(fillData$yr)
  incompleteYear <- as.integer(names(dayCounts)[dayCounts != numDays])
  numYears <- length(dayCounts)-length(incompleteYear)
  fillData <- subset(fillData, !(yr %in% incompleteYear))
  yr <- as.integer(names(dayCounts)[dayCounts == numDays])
  
  ## See Examples section of help(predict.gam)
  Xp <- predict(fit, newdata = fillData, type = "lpmatrix")
  coefs <- coef(fit)
  A <- kronecker(diag(numYears), matrix(rep(1/numDays, numDays), nrow = 1))
  Xs <- A %*% Xp
  means <- as.numeric(Xs %*% coefs)
  ses <- sqrt(diag(Xs %*% fit$Vp %*% t(Xs)))
  out <- data.frame(predicted = means, se = ses, yr = yr )
  
  return(out)
  
}