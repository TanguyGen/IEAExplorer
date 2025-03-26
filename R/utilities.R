# utilities
#' ## The 2 main functions: ATAC (modelling) and ggATAC (plotting)
#' Modelling is done with the [BRMS package](https://paul-buerkner.github.io/brms/)
#' Plotting is done using the [ggplot2 package](https://ggplot2.tidyverse.org)
#' data = dataset with two columns: year and value
#' year_start = first year that is be used when fitting the models
#' year_end_trend = last year that is used when fitting the overall trend model (using all observations). By default this is the last year of the series
#' year_end_OL = last year that is used when fitting the model that is used for forecasting. By default this will be the last year of data minus the n years of forecasts
#' n_forecasts = number of forecasts to the produced. By default 3.

ggATAC <- function(results,backtransform=FALSE,trend=FALSE,width){
  if (backtransform==TRUE) results$value<-results$value^4
  fit.wide <- results %>% 
    distinct(year, param, .keep_all = TRUE) %>% 
    pivot_wider(names_from = "param")
  point_size <- max(ceiling(width / 800), 1.5)   # Scale points
  line_width <- max(ceiling(width / 1200), 0.6)   # Scale lines
  ribbon_alpha <- min(width / 2000, 0.25)        # Adjust ribbon transparency
  
  gg <- ggplot(data = fit.wide, aes(x = year))
  
  if (dim(fit.wide)[2] > 3) { # Check if model was fitted
    gg <- gg +
      geom_path(aes(y = trendOL), col = '#FFB302', lwd = line_width + 0.4, alpha = 0.75, lty = 1)
    
    if (sum(!is.na(fit.wide$prediction)) >= 2) {
      gg <- gg +
        geom_ribbon(aes(ymin = predQ2.5, ymax = predQ97.5), fill = "#104E8B", alpha = ribbon_alpha) +
        geom_path(aes(y = prediction), col = '#104E8B', lwd = line_width, lty =  2)
    }
    
    if (sum(!is.na(fit.wide$forecast)) >= 2) {
      gg <- gg +
        geom_ribbon(aes(ymin = forecastQ2.5, ymax = forecastQ97.5), fill = '#8B1A1A', alpha = ribbon_alpha) +
        geom_ribbon(aes(ymin = forecastQ12.5, ymax = forecastQ87.5), fill = '#8B1A1A', alpha = ribbon_alpha) +
        geom_ribbon(aes(ymin = forecastQ25, ymax = forecastQ75), fill = '#8B1A1A', alpha = ribbon_alpha) +
        geom_path(aes(y = forecast), col = '#8B1A1A', lwd = line_width, lty = 2)
    }
  }
  
  if (trend == TRUE) {
    gg <- gg + geom_path(aes(y = trend), col = 'black', alpha = 0.25, lwd = line_width + 0.5)
  }
  
  gg <- gg +
    geom_point(aes(y = observation), col = "#104E8B", pch = 19, size = point_size) +
    theme_bw(base_size = max(ceiling(width / 50), 12)) # Adjust text size
  
  return(gg)
}

