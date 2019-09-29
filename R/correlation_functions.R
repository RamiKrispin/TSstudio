#'  Time Series Lag Visualization
#' @export
#' @param ts.obj A univariate time series object of a class "ts", "zoo" or "xts" 
#' @param lags An integer, set the lags range, by default will plot the first 12 lags
#' @param Xshare Plotly parameter, should the x-axis be shared amongst the subplots?
#' @param Yshare Plotly parameter, should the y-axis be shared amongst the subplots?
#' @param margin Plotly parameter, either a single value or four values (all between 0 and 1).  
#' If four values provided, the first will be used as the left margin, 
#' the second will be used as the right margin, 
#' the third will be used as the top margin, 
#' and the fourth will be used as the bottom margin. 
#' If a single value provided, it will be used as all four margins.
#' @param n_plots An integer, define the number of plots per row
#' @description Visualization of series with its lags, 
#' can be used to identify a correlation between the series and it lags
#' @examples
#' data(USgas)
#' 
#' # Plot the first 12 lags (default)
#' ts_lags(USgas) 
#' 
#' # Plot the seasonal lags for the first 4 years (hence, lag 12, 24, 36, 48)
#' ts_lags(USgas, lags = c(12, 24, 36, 48))
#' 
#' # Setting the margin between the plot
#' ts_lags(USgas, lags = c(12, 24, 36, 48), margin = 0.01)

ts_lags <- function(ts.obj, lags = 1:12, margin = 0.02, 
                    Xshare = TRUE, Yshare = TRUE, n_plots = 3){
  `%>%` <- magrittr::`%>%`
  df <- df_wide <- p <- obj.name <- lag <- lag_plots <- time <- NULL
  
  obj.name <- base::deparse(base::substitute(ts.obj))
  # --------------Error handling --------------
  
  
  if(!is.numeric(lags)){
    warning("The 'lags' parameter is not valid, using the defualt setting (lags = 1:12)")
    lags <- 1:12
  } else if(base::any(lags <= 0) ){
    warning("The 'lags' parameter is not valid, using the defualt setting (lags = 1:12)")
    lags <- 1:12
  } else if(!all(base::round(lags) == lags)){
    stop("Some of the inputs of the 'lags' argument are not integer type")
  }
  
  
  if(!is.numeric(margin)){
    warning("The 'margin' parameter is not valid, using the defualt setting (margin = 0.2)")
    margin <- 0.2
  }
  
  
  if(!is.logical(Xshare)){
    warning("The 'Xshare' parameter is not valid, please use only boolean operators.",
            " Using the defualt setting setting (Xshare = TRUE")
    Xshare <- TRUE
  }
  
  if(!is.logical(Yshare)){
    warning("The 'Yshare' parameter is not valid, please use only boolean operators.",
            " Using the defualt setting setting (Yshare = TRUE")
    Yshare <- TRUE
  }
  
  # -------------- Error handling and creating the data frame --------------
  if (stats::is.ts(ts.obj)) {
    if (stats::is.mts(ts.obj)) {
      warning("The 'ts.obj' has multiple columns, only the first column will be plot")
      ts.obj <- ts.obj[, 1]
    }
    
    df <- base::data.frame(time = stats::time(ts.obj), y = base::as.numeric(ts.obj)) %>%
      dplyr::arrange(time)
    
  } else if (xts::is.xts(ts.obj) | zoo::is.zoo(ts.obj)) {
    if (!is.null(base::dim(ts.obj))) {
      if (base::dim(ts.obj)[2] > 1) {
        warning("The 'ts.obj' has multiple columns, only the first column will be plot")
        ts.obj <- ts.obj[, 1]
      }
    }
    df <- base::data.frame(time = zoo::index(ts.obj), y = base::as.numeric(ts.obj)) %>%
      dplyr::arrange(time)
  } else {
    stop("The input object is not valid (must be 'ts', 'xts', or 'zoo')")
  }
  
  
  p_list <- lapply(base::seq_along(lags), function(i){
    plotly::plot_ly(x = df$y %>% dplyr::lag(lags[i]), 
                    y = df$y,
                    type = "scatter",
                    mode = "markers") %>%
      plotly::layout(xaxis = list(title = "",
                                  range = c( base::min(stats::na.omit(df$y)),  
                                             base::max(stats::na.omit(df$y)))),
                     yaxis = list(range = c( base::min(stats::na.omit(df$y)),  
                                             base::max(stats::na.omit(df$y)))),
                     
                     annotations = list(text = paste("Lag", lags[i], sep = " "), 
                                        xref = "paper", yref = "paper", yanchor = "bottom", 
                                        xanchor = "center", align = "center", 
                                        x = 0.5, y = 0.9, showarrow = FALSE)
      )
  })
  
  p <- base::suppressWarnings(plotly::subplot(p_list, nrows = base::ceiling(base::length(p_list) / n_plots), 
                       margin = margin, 
                       shareX = Xshare, shareY = Yshare) %>%
    plotly::layout(title = paste(obj.name, "- Series (Y axis) vs. Lags (X axis)", sep = " ")) %>%
    plotly::hide_legend())
  
  
  
  # -------------- End --------------
  return(p)
  
}

#'  A Visualization Function of the ACF Estimation
#' @export ts_acf
#' @param ts.obj a univariate or multivariate time series object of class "ts", "mts", "zoo" or "xts"
#' @param lag.max maximum lag at which to calculate the acf. Default is 10*log10(N/m) 
#' where N is the number of observations and m the number of series. 
#' Will be automatically limited to one less than the number of observations in the series.
#' @param color The color of the plot, support both name and expression
#' @param ci the significant level of the estimation - a numeric value between 0 and 1, default is set for 0.95 
#' @examples
#' data(USgas)
#' 
#' ts_acf(USgas, lag.max = 60)


ts_acf <- function(ts.obj, lag.max = NULL, ci = 0.95, color = NULL) {
  base::.Deprecated(new = "ts_cor", msg = "The 'ts_acf' function is deprecated, please use 'ts_cor' instead")
  `%>%` <- magrittr::`%>%`
  # Error handling
  if (is.null(ts.obj)) {
    stop("The time series object is NULL")
  } else if (!stats::is.ts(ts.obj) & !xts::is.xts(ts.obj) & 
             !zoo::is.zoo(ts.obj)) {
    stop("Invalid class - Please make sure the object class is either \"ts\", \"mts\", \"xts\" or \"zoo\"")
  }
  
  if (ci > 1 | ci < 0) {
    warning("The 'ci' value is out of bound (0-1), the default option of 0.95 will be used")
    ci <- 0.95
  }
  if(!is.null(color)){
    if(!is.character(color)){
      warning("The value of the 'color' parameter is not valid")
      color = "#00526d"
    }
  } else{
    color = "#00526d"
  }
  
  x <- df <- obj.name <- NULL
  
  obj.name <- base::deparse(base::substitute(ts.obj))
  x <- stats::acf(ts.obj, lag.max = lag.max, plot = FALSE)
  
  # Error handling
  if (!is.null(lag.max)) {
    if (lag.max > x[[3]]) {
      warning(paste("The value of 'lag.max' exceed the number of available lags in the series, 
                    by default will use the max available lags", 
                    sep = " "))
    }
  }
  # Calculate the upper/lower CI values
  ci_value <- stats::qnorm((1 + ci)/2)/sqrt(x[[3]])
  
  # Convert the values into data frame format
  df <- data.frame(lag = base::round(x[[4]], 3), acf = base::round(x[[1]], 
                                                                   3), width = 0.02, ci_u = base::round(ci_value, 2), 
                   ci_d = base::round(-ci_value, 2))
  # Multivariate time series
  series_list <- series <- NULL
  if (ncol(df) > 5) {
    if (!is.null(x[[6]])) {
      series <- x[[6]]
    } else {
      for (i in 1:(base::dim(ts.obj)[2])) {
        series <- c(series, paste("Series", i, sep = " "))
      }
    }
    counter <- 0
    for (c in series) {
      for (r in series) {
        counter <- counter + 1
        if (r == c) {
          series_list[counter] <- plotly::plot_ly() %>% 
            plotly::add_bars(x = df[, counter], 
                             y = df[, (length(series))^2 + counter], 
                             width = df$width, name = "ACF") %>% 
            plotly::add_trace(x = df[, counter], 
                              y = df$ci_u, type = "scatter", mode = "lines", 
                              name = "CI Upper Bound", line = list(width = 1, 
                                                                   dash = "dash", color = "green")) %>% 
            plotly::add_trace(x = df[, counter], 
                              y = df$ci_d, type = "scatter", mode = "lines", 
                              name = "CI Upper Bound", line = list(width = 1, 
                                                                   dash = "dash", color = "green")) %>% 
            plotly::layout(xaxis = list(title = "Lag", 
                                        showgrid = FALSE), yaxis = list(title = "ACF", 
                                                                        showgrid = FALSE, range = c(base::min(x[[1]]), 
                                                                                                    base::max(x[[1]]))), annotations = list(text = c, 
                                                                                                                                      xref = "paper", yref = "paper", yanchor = "bottom", 
                                                                                                                                      xanchor = "center", align = "center", 
                                                                                                                                      x = 0.5, y = 0.9, showarrow = FALSE))
        } else {
          series_list[counter] <- plotly::plot_ly() %>% 
            plotly::add_bars(x = df[, counter], 
                             y = df[, (length(series))^2 + counter], 
                             width = df$width, name = "ACF") %>% 
            plotly::add_trace(x = df[, counter], 
                              y = df$ci_u, type = "scatter", mode = "lines", 
                              name = "CI Upper Bound", line = list(width = 1, 
                                                                   dash = "dash", color = "green")) %>% 
            plotly::add_trace(x = df[, counter], 
                              y = df$ci_d, type = "scatter", mode = "lines", 
                              name = "CI Lower Bound", line = list(width = 1, 
                                                                   dash = "dash", color = "green")) %>% 
            plotly::layout(xaxis = list(title = "Lag", 
                                        showgrid = FALSE), yaxis = list(title = "ACF", 
                                                                        showgrid = FALSE, range = c(base::min(x[[1]]), 
                                                                                                    base::max(x[[1]]))), annotations = list(text = paste(r, 
                                                                                                                                                   c, sep = " & "), xref = "paper", yref = "paper", 
                                                                                                                                      yanchor = "bottom", xanchor = "center", 
                                                                                                                                      align = "center", x = 0.5, y = 0.9, 
                                                                                                                                      showarrow = FALSE))
        }
      }
    }
    p <- plotly::subplot(series_list, nrows = length(series), 
                         titleX = FALSE, titleY = TRUE, margin = 0.03, 
                         shareX = FALSE, shareY = TRUE) %>% plotly::hide_legend() %>% 
      plotly::layout(title = "ACF Plot", margin = 0.06)
  } else if (ncol(df) == 5) {
    p <- plotly::plot_ly(data = df) %>% plotly::add_bars(x = ~lag, 
                                                         y = ~acf, 
                                                         width = ~width, 
                                                         marker = list(color = color),
                                                         name = "ACF") %>% 
      plotly::add_trace(x = ~lag, y = ~ci_u, type = "scatter", 
                        mode = "lines", name = "CI Upper Bound", 
                        line = list(width = 1, dash = "dash", color = "green")) %>% 
      plotly::add_trace(x = ~lag, y = ~ci_d, type = "scatter", 
                        mode = "lines", name = "CI Lower Bound", 
                        line = list(width = 1, dash = "dash", color = "green")) %>% 
      plotly::layout(title = paste("Series", obj.name, 
                                   sep = " "), xaxis = list(title = "Lag", 
                                                            showgrid = FALSE), yaxis = list(title = "ACF", 
                                                                                            showgrid = FALSE)) %>% plotly::hide_legend()
  }
  
  return(p)
  }



#'  A Visualization Function of the PACF Estimation
#' @export ts_pacf
#' @param ts.obj a univariate or multivariate time series object of class "ts", "mts", "zoo" or "xts"
#' @param lag.max maximum lag at which to calculate the acf. Default is 10*log10(N/m) 
#' where N is the number of observations and m the number of series. 
#' Will be automatically limited to one less than the number of observations in the series.
#' @param ci the significant level of the estimation - a numeric value between 0 and 1, 
#' default is set for 0.95 
#' @param color The color of the plot, support both name and expression
#' @examples
#' data(USgas)
#' 
#' ts_pacf(USgas, lag.max = 60)

ts_pacf <- function(ts.obj, lag.max = NULL, ci = 0.95, color = NULL) {
  base::.Deprecated(new = "ts_cor", msg = "The 'ts_pacf' function is deprecated, please use 'ts_cor' instead")
  `%>%` <- magrittr::`%>%`
  # Error handling
  if (is.null(ts.obj)) {
    stop("The time series object is NULL")
  } else if (!stats::is.ts(ts.obj) & !xts::is.xts(ts.obj) & 
             !zoo::is.zoo(ts.obj)) {
    stop("Invalid class - Please make sure the object class is either 'ts', 'mts', 'xts' or 'zoo'")
  }
  
  if (ci > 1 | ci < 0) {
    warning("The 'ci' value is out of bound (0-1), the default option of 0.95 will be used")
    ci <- 0.95
  }
  if(!is.null(color)){
    if(!is.character(color)){
      warning("The value of the 'color' parameter is not valid")
      color = "#00526d"
    }
  } else{
    color = "#00526d"
  }
  x <- df <- obj.name <- NULL
  
  obj.name <- base::deparse(base::substitute(ts.obj))
  x <- stats::pacf(ts.obj, lag.max = lag.max, plot = FALSE)
  
  # Error handling
  if (!is.null(lag.max)) {
    if (lag.max > x[[3]]) {
      warning(paste("The value of 'lag.max' exceed the number of available lags in the series, 
                    by default will use the max available lags", 
                    sep = " "))
    }
  }
  # Calculate the upper/lower CI values
  ci_value <- stats::qnorm((1 + ci)/2)/sqrt(x[[3]])
  
  # Convert the values into data frame format
  df <- data.frame(lag = base::round(x[[4]], 3), acf = base::round(x[[1]], 
                                                                   3), width = 0.02, ci_u = base::round(ci_value, 2), 
                   ci_d = base::round(-ci_value, 2))
  # Multivariate time series
  series_list <- series <- NULL
  if (ncol(df) > 5) {
    if (!is.null(x[[6]])) {
      series <- x[[6]]
    } else {
      for (i in 1:(base::dim(ts.obj)[2])) {
        series <- c(series, paste("Series", i, sep = " "))
      }
    }
    counter <- 0
    for (c in series) {
      for (r in series) {
        counter <- counter + 1
        if (r == c) {
          series_list[counter] <- plotly::plot_ly() %>% 
            plotly::add_bars(x = df[, counter], 
                             y = df[, (length(series))^2 + counter], 
                             width = df$width, name = "PACF") %>% 
            plotly::add_trace(x = df[, counter], 
                              y = df$ci_u, type = "scatter", mode = "lines", 
                              name = "CI Upper Bound", line = list(width = 1, 
                                                                   dash = "dash", color = "green")) %>% 
            plotly::add_trace(x = df[, counter], 
                              y = df$ci_d, type = "scatter", mode = "lines", 
                              name = "CI Upper Bound", line = list(width = 1, 
                                                                   dash = "dash", color = "green")) %>% 
            plotly::layout(xaxis = list(title = "Lag", 
                                        showgrid = FALSE), yaxis = list(title = "PACF", 
                                                                        showgrid = FALSE, range = c(base::min(x[[1]]), 
                                                                                                    base::max(x[[1]]))), annotations = list(text = c, 
                                                                                                                                      xref = "paper", yref = "paper", yanchor = "bottom", 
                                                                                                                                      xanchor = "center", align = "center", 
                                                                                                                                      x = 0.5, y = 0.9, showarrow = FALSE))
        } else {
          series_list[counter] <- plotly::plot_ly() %>% 
            plotly::add_bars(x = df[, counter], 
                             y = df[, (length(series))^2 + counter], 
                             width = df$width, name = "PACF") %>% 
            plotly::add_trace(x = df[, counter], 
                              y = df$ci_u, type = "scatter", mode = "lines", 
                              name = "CI Upper Bound", line = list(width = 1, 
                                                                   dash = "dash", color = "green")) %>% 
            plotly::add_trace(x = df[, counter], 
                              y = df$ci_d, type = "scatter", mode = "lines", 
                              name = "CI Lower Bound", line = list(width = 1, 
                                                                   dash = "dash", color = "green")) %>% 
            plotly::layout(xaxis = list(title = "Lag", 
                                        showgrid = FALSE), yaxis = list(title = "PACF", 
                                                                        showgrid = FALSE, range = c(base::min(x[[1]]), 
                                                                                                    base::max(x[[1]]))), annotations = list(text = paste(r, 
                                                                                                                                                   c, sep = " & "), xref = "paper", yref = "paper", 
                                                                                                                                      yanchor = "bottom", xanchor = "center", 
                                                                                                                                      align = "center", x = 0.5, y = 0.9, 
                                                                                                                                      showarrow = FALSE))
        }
      }
    }
    p <- plotly::subplot(series_list, nrows = length(series), 
                         titleX = FALSE, titleY = TRUE, margin = 0.03, 
                         shareX = FALSE, shareY = TRUE) %>% plotly::hide_legend() %>% 
      plotly::layout(title = "PACF Plot", margin = 0.06)
  } else if (ncol(df) == 5) {
    p <- plotly::plot_ly(data = df) %>% plotly::add_bars(x = ~lag, 
                                                         y = ~acf, 
                                                         width = ~width, 
                                                         marker = list(color = color),
                                                         name = "ACF") %>% 
      plotly::add_trace(x = ~lag, y = ~ci_u, type = "scatter", 
                        mode = "lines", name = "CI Upper Bound", 
                        line = list(width = 1, dash = "dash", color = "green")) %>% 
      plotly::add_trace(x = ~lag, y = ~ci_d, type = "scatter", 
                        mode = "lines", name = "CI Lower Bound", 
                        line = list(width = 1, dash = "dash", color = "green")) %>% 
      plotly::layout(title = paste("Series", obj.name, 
                                   sep = " "), xaxis = list(title = "Lag", 
                                                            showgrid = FALSE), yaxis = list(title = "PACF", 
                                                                                            showgrid = FALSE)) %>% plotly::hide_legend()
  }
  
  return(p)
}

#'  Visualization of the Decompose of a Time Series Object
#' @export
#' @param ts.obj a univariate time series object of a class "ts", "zoo" or "xts"
#' @param type Set the type of the seasonal component, can be set to either "additive",  "multiplicative" or "both" to compare between the first two options (default set to “additive”)
#' @param showline Logic, add a separation line between each of the plot components (default set to TRUE)
#' @description Interactive visualization the trend, seasonal and random components of a time series based on the decompose function from the stats package.
#' @examples
#' # Defualt decompose plot
#' ts_decompose(AirPassengers)
#' 
#' # Remove the sepration lines between the plot components
#' ts_decompose(AirPassengers, showline = FALSE)
#' 
#' # Plot side by side a decompose of additive and multiplicative series
#' ts_decompose(AirPassengers, type = "both")
#' 
ts_decompose <- function(ts.obj, type = "additive", showline = TRUE){
  
  # Error handling
  # Test if the object is either ts, zoo or xts
  if(!stats::is.ts(ts.obj) & !zoo::is.zoo(ts.obj) & !xts::is.xts(ts.obj)){
    stop("The 'ts.obj' is not a valid time series format (i.e. 'ts', 'xts', 'zoo')")
  }
  
  # If the object has multiple series select the first one
  if (stats::is.ts(ts.obj)) {
    if (stats::is.mts(ts.obj)) {
      warning("The \"ts.obj\" has multiple columns, only the first column will be plot")
      ts.obj <- ts.obj[, 1]
    }
  } else if (xts::is.xts(ts.obj) | zoo::is.zoo(ts.obj)) {
    if (!is.null(base::dim(ts.obj))) {
      if (base::dim(ts.obj)[2] > 1) {
        warning("The \"ts.obj\" has multiple columns, only the first column will be plot")
        ts.obj <- ts.obj[, 1]
      }
    }
  }
  
  # Test the function inputs are currect
  if(type != "additive" & 
     type != "multiplicative" & 
     type != "both"){
    warning("The value of 'type' is not valide, using the default option - 'additive'")
    type <- "additive"
  } 
  
  if(!is.logical(showline)){
    warning("The value of 'showline' is not valide, using the default option - TRUE")
    showline <- TRUE
  } 
  
  
  
  `%>%` <- magrittr::`%>%`  
  obj.name <- p <- p1 <- p2 <- NULL
  obj.name <- base::deparse(base::substitute(ts.obj))
  
  # Create a sub function for the decompose process
  decompose_sub <- function(ts.obj, type, showline, obj.name, shareY = FALSE){
    dec <- min <- max <- p_sub <- NULL
    
    if(stats::is.ts(ts.obj)){
      dec <- stats::decompose(ts.obj, type = type)
      
    } else if(xts::is.xts(ts.obj) | zoo::is.zoo(ts.obj)){
      ts.obj <- stats::as.ts(ts.obj, 
                             start = utils::head(zoo::index(ts.obj), 1), 
                             end = utils::tail(zoo::index(ts.obj), 1))
      
      dec <- stats::decompose(ts.obj, type = type)
    }
    
    min <- min(stats::time(ts.obj))
    max <- max(stats::time(ts.obj))
    
    obs <- TSstudio::ts_plot(dec$x) %>% 
      plotly::layout(yaxis = list(title = "Observed"),
                     xaxis = list(range = c(min,max),
                                  showline = showline,
                                  showticklabels = FALSE)
      )
    
    seasonal <- TSstudio::ts_plot(dec$seasonal) %>% 
      plotly::layout(yaxis = list(title = "Seasonal"),
                     xaxis = list(range = c(min,max),
                                  showline = showline,
                                  showticklabels = FALSE)
      )
    random <- TSstudio::ts_plot(dec$random) %>% 
      plotly::layout(yaxis = list(title = "Random"),
                     xaxis = list(range = c(min,max),
                                  showline = showline)
      )
    
    trend <- TSstudio::ts_plot(dec$trend) %>% 
      plotly::layout(yaxis = list(title = "Trend"),
                     xaxis = list(range = c(min,max),
                                  showline = showline,
                                  showticklabels = FALSE)
      )
    
    p_sub <- plotly::subplot(obs, trend, seasonal, random, nrows = 4, shareY = shareY) %>% 
      plotly::hide_legend() %>%
      plotly::layout(
        title = base::paste("Decomposition of", type, "time series -", obj.name)
      )
    
    return(p_sub)
  }
  
  if(type == "additive" | type == "multiplicative" ){
    p <- decompose_sub(ts.obj = ts.obj, type = type, showline = showline, obj.name = obj.name, shareY = TRUE)
  } else if(type == "both"){
    p1 <- decompose_sub(ts.obj = ts.obj, type = "additive", showline = showline, obj.name = obj.name, shareY = TRUE)
    p2 <- decompose_sub(ts.obj = ts.obj, type = "multiplicative", showline = showline, obj.name = obj.name, shareY = FALSE) %>%
      plotly::layout(yaxis = list(showlegend = FALSE))
    p <- plotly::subplot(p1, p2, titleY = T) %>% plotly::layout(
      title = base::paste("Decomposition of additive and multiplicative time series -", obj.name)
    )
  }
  
  return(p)
}


#'  Time Series Cross Correlation Lags Visualization
#' @export
#' @param x A univariate time series object of a class "ts"
#' @param y A univariate time series object of a class "ts"
#' @param lags An integer, set the lags range, 
#' by default will plot the two series along with the first 12 lags  
#' @param Xshare Plotly parameter, should the x-axis be shared amongst the subplots?
#' @param Yshare Plotly parameter, should the y-axis be shared amongst the subplots?
#' @param margin Plotly parameter, either a single value or four values (all between 0 and 1).  
#' If four values provided, the first will be used as the left margin, 
#' the second will be used as the right margin, 
#' the third will be used as the top margin, 
#' and the fourth will be used as the bottom margin. 
#' If a single value provided, it will be used as all four margins.
#' @param n_plots An integer, define the number of plots per row
#' @param title A character, optional, set the plot title 
#' @description Visualize the series y against the series x lags (according to the setting of the lags argument) 
#' and return the corresponding cross-correlation value for each lag
#' @return Plot
#' @examples
#' 
#' data("USUnRate")
#' data("USVSales")
#' 
#' ccf_plot(x = USVSales, y = USUnRate)
#' 
#' #Plotting the first 6 lead and lags of the USVSales with the USUnRate
#' ccf_plot(x = USVSales, y = USUnRate, lags = -6:6)
#' 
#' # Setting the plot margin and number of plots in each raw
#' ccf_plot(x = USVSales, y = USUnRate, lags = c(0, 6, 12, 24), 
#' margin = 0.01,  n_plots = 2)


ccf_plot <- function(x, y, 
                     lags = 0:12, 
                     margin = 0.02,
                     n_plots = 3,
                     Xshare = TRUE, 
                     Yshare = TRUE,
                     title = NULL){
  x.name <- y.name <- x_sub <- y_sub <- c <- ccf_df <- z <- ts_inter <- lags_plot <- NULL
  
  `%>%` <- magrittr::`%>%`
  x.name <- base::deparse(base::substitute(x))
  y.name <- base::deparse(base::substitute(y))
  ### Error handling 
  if(!base::is.null(title)){
    if(!base::is.character(title)){
      warning("The value of the 'title' is not valid, using default")
      title <- base::paste(y.name, 
                           "(Y axis) vs. the Lags of", 
                           x.name,
                           sep = " ")
    } 
  } else {
    title <- base::paste(y.name, 
                         "(Y axis) vs. the Lags of", 
                         x.name,
                         sep = " ")
  }
  
  if(!is.numeric(margin)){
    warning("The 'margin' parameter is not valid, using the defualt setting (margin = 0.2)")
    margin <- 0.2
  }
  
  if(!is.logical(Xshare)){
    warning("The 'Xshare' parameter is not valid, please use only boolean operators.",
            " Using the defualt setting setting (Xshare = TRUE")
    Xshare <- TRUE
  }
  if(!is.logical(Yshare)){
    warning("The 'Yshare' parameter is not valid, please use only boolean operators.",
            " Using the defualt setting setting (Yshare = TRUE")
    Yshare <- TRUE
  }
  
  if(!base::is.numeric(lags)){
    stop("The value of the 'lags' argument is not valid")
  } else if(base::any(lags %% 1 != 0)){
    stop("The value of the 'lags' argument is not integer")
  }
  if(!stats::is.ts(x)){
    stop("The 'x' argument is not a ts object")
  } else if(!stats::is.ts(y)){
    stop("The 'y' argument is not a ts object")
  } else if(stats::is.mts(x) || stats::is.mts(y)){
    stop("Cannot handel mts objects, please use only ts objects as an input")
  } else if(stats::frequency(x) != stats::frequency(y)){
    stop("Cannon handle series with different frequencies")
  }
  
  z <- stats::ts.intersect(x,y)
  if(base::is.null(z)){
    stop("There is no overlapping between the two inputs")
  }
  x_sub <- stats::window(x, start = stats::start(z), end = stats::end(z))
  y_sub <- stats::window(y, start = stats::start(z), end = stats::end(z))
  
  c <- stats::ccf(x = x_sub, y = y_sub,lag.max = max(lags), plot = FALSE)
  
  ccf_df <- base::data.frame(lag = (max(lags)):(-max(lags)) , acf = c$acf)
  
  
  output <- lapply(lags, function(i){
    ts_inter <- NULL
    if(i == 0){
      
      p <- plotly::plot_ly(x = x_sub, 
                           y = y_sub,
                           type = "scatter",
                           mode = "markers")
    } else {
      ts_inter <- stats::ts.intersect(y_sub, stats::lag(x_sub, -i)) %>% as.data.frame()
      base::colnames(ts_inter) <- c("y_sub", "x_sub_lag")
      
      p <- plotly::plot_ly(x = ts_inter$x_sub_lag, 
                           y = ts_inter$y_sub, 
                           type = "scatter",
                           mode = "markers")
    }
    p <-  p %>% plotly::layout(xaxis = list(title = "",
                                            range = c( base::min(stats::na.omit(x)) * 0.95,  
                                                       base::max(stats::na.omit(x))) * 1.05),
                               yaxis = list(range = c( base::min(stats::na.omit(y) * 0.95),  
                                                       base::max(stats::na.omit(y))) * 1.05),
                               
                               annotations = list(text = base::paste("Lag", i,
                                                                     base::paste("(",
                                                                                 base::round(ccf_df$acf[which(ccf_df$lag == i)], 3),
                                                                                 ")", sep = ""),
                                                                     sep = " "), 
                                                  xref = "paper", yref = "paper", yanchor = "bottom", 
                                                  xanchor = "center", align = "center", 
                                                  x = 0.5, y = 0.9, showarrow = FALSE)
    )
    return(p)
  })
  
  lags_plot <- plotly::subplot(output, 
                               nrows = base::length(output) %/% n_plots, 
                               margin = margin, 
                               shareX = Xshare, 
                               shareY = Yshare) %>% 
    plotly::layout(title = title) %>%
    plotly::hide_legend()
  
  return(lags_plot)
}


#'  An Interactive Visualization of the ACF and PACF Functions
#' @export
#' @param ts.obj A univariate time series object class 'ts'
#' @param type A character, defines the plot type - 'acf' for ACF plot, 'pacf' for PACF plot, and 'both' (default) for both ACF and PACF plots
#' @param seasonal A boolean, when set to TRUE (default) will color the seasonal lags
#' @param ci The significant level of the estimation - a numeric value between 0 and 1, default is set for 0.95 
#' @param lag.max maximum lag at which to calculate the acf. Default is 10*log10(N/m) 
#' where N is the number of observations and m the number of series. 
#' Will be automatically limited to one less than the number of observations in the series
#' @param seasonal_lags A vector of integers, highlight specific cyclic lags (besides the main seasonal lags of the series).  
#' This is useful when working with multiseasonal time series data. For example, for a monthly series 
#' (e.g., frequency 12) setting the argument to 3 will highlight the quarterly lags
#' @examples 
#' 
#' data(USgas)
#' 
#' ts_cor(ts.obj = USgas)
#' 
#' # Setting the maximum number of lags to 72
#' ts_cor(ts.obj = USgas, lag.max = 72)
#' 
#' # Plotting only ACF 
#' ts_cor(ts.obj = USgas, lag.max = 72, type = "acf")



ts_cor <- function(ts.obj, 
                   type = "both", 
                   seasonal = TRUE, 
                   ci = 0.95, 
                   lag.max = NULL,
                   seasonal_lags = NULL){
  `%>%` <- magrittr::`%>%`
  df <- f <- p1 <- p2 <- obj.name <- NULL
  
  obj.name <- base::deparse(base::substitute(ts.obj))
  
  
  storeWarn <- base::getOption("warn")
  base::options(warn = -1) 
  # Error handling 
  # Checking the input object
  if(!stats::is.ts(ts.obj)){
    stop("The 'ts.obj' argument is not a valid 'ts' object")
  } else if(stats::is.mts(ts.obj)){
    stop("Cannot use multiple time series object as an input")
  } 
  
  f <- stats::frequency(ts.obj)
  
  # Check the seasonal_lags argument
  if(!base::is.null(seasonal_lags)){
    if(!base::all(seasonal_lags %% 1 == 0)){
      stop("Error on the 'seasonal_lags' argument: one of the input is not integer")
    } else if(base::any(seasonal_lags <1)){
      stop("Error on the 'seasonal_lags' argument: all inputs must be greater than 1")
    } else if(f %in% seasonal_lags && seasonal){
      warning(base::paste("The 'seasonal_lags' argument includes the seasonal lag of the seires - ", f," and therefore, will be plot as the seasonal lag" ))
      seasonal_lags <- seasonal_lags[base::which(seasonal_lags != f)]
    }
    seasonal_lags <- base::sort(seasonal_lags)
    seasonal_colors <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(base::length(seasonal_lags))
    
  }
  
  
  
  
  if(type == "both" || type == "acf"){
    x <- stats::acf(ts.obj, lag.max = lag.max, plot = FALSE)
    
    upper <- stats::qnorm((1 + ci)/2)/sqrt(x[[3]])
    lower <- - upper
    
    df <- data.frame(y = as.numeric(x$acf),
                     lag = 0:(base::nrow(x$acf) -1),
                     stringsAsFactors = FALSE)
    
  
   
    if(seasonal){
      df$seasonal_lag <- ifelse(df$lag %% f  == 0 & df$lag != 0, df$y, NA)
      df$non_seasonal_lag <- ifelse(df$lag %% f  != 0, df$y, NA)
      df$zero_lag <-  ifelse(df$lag == 0, df$y, NA)
     
       p1 <- plotly::plot_ly()
      
       if(!base::is.null(seasonal_lags)){
        # for(l in base::seq_along(seasonal_lags)){
        # df[[base::paste("slag_", seasonal_lags[l], sep = "")]] <- ifelse(df$lag %% seasonal_lags[l] == 0 & 
        #                                                                    df$lag != 0 & 
        #                                                                    base::is.na(df$seasonal_lag), 
        #                                                                  df$y , NA)
        # df$non_seasonal_lag <- ifelse(df$lag %% seasonal_lags[l]  != 0, 
        #                               df$non_seasonal_lag, NA)
        # p1 <- p1 %>%
        #   plotly::add_trace(x = df$lag,
        #                     y =  df[[base::paste("slag_",  seasonal_lags[l], sep = "")]],
        #                     type = "bar",
        #                     marker = list(color = seasonal_colors[l]),
        #                     width = 0.1,
        #                     name = base::paste("Seasonal Lag", seasonal_lags[l], sep = " "),
        #                     legendgroup = base::paste("slag_", seasonal_lags[l], sep = ""),
        #                     showlegend = TRUE)
        # }
         
         
         c <- NULL
         seasonal_lags <- sort(seasonal_lags, decreasing = TRUE)
         for(i in base::seq_along(seasonal_lags)){
           if(i == 1){
             df[[paste("lag_", seasonal_lags[i])]] <- ifelse(df$lag %% seasonal_lags[i] == 0 & df$lag %% f != 0, df$y, NA)
             c <- c(c, seasonal_lags[i])
           } else {
             df[[paste("lag_",seasonal_lags[i])]] <- ifelse(df$lag %% seasonal_lags[i] == 0  & 
                                                              df$lag %% f != 0, df$y, NA)
             for(n in c){
               df[[paste("lag_",seasonal_lags[i])]] <- ifelse(!base::is.na(df[[paste("lag_",n)]]), 
                                                              NA, df[[paste("lag_",seasonal_lags[i])]])
             }
             c <- c(c, seasonal_lags[i])
             
           }
           
           df$non_seasonal_lag <- ifelse(!base::is.na( df[[paste("lag_",seasonal_lags[i])]]), NA, df$non_seasonal_lag)
           
           p1 <- p1 %>%
             plotly::add_trace(x = df$lag,
                               y =  df[[paste("lag_", seasonal_lags[i])]],
                               type = "bar",
                               marker = list(color = seasonal_colors[i]),
                               width = 0.1,
                               name = base::paste("Seasonal Lag", seasonal_lags[i], sep = " "),
                               legendgroup = base::paste("slag_", seasonal_lags[i], sep = ""),
                               showlegend = TRUE)
         }
         
         
         
       }

    p1 <- p1 %>%
      plotly::add_trace(x = df$lag, 
                        y = df$zero_lag, 
                        type = "bar",
                        marker = list(color = "black"), 
                        width = 0.1, 
                        name = "Lag-Zero", 
                        legendgroup = "lagzero",
                        showlegend = FALSE) %>%
      plotly::add_trace(x = df$lag, 
                        y = df$seasonal_lag, 
                        type = "bar", 
                        marker = list(color = "red"), 
                        width = 0.1, 
                        legendgroup = "seasonal",
                        name = base::paste("Seasonal Lag", f, sep = " ")) %>%
      plotly::add_trace(x = df$lag, 
                        y = df$non_seasonal_lag, 
                        type = "bar", 
                        marker = list(color = "#00526d"), 
                        width = 0.1, 
                        legendgroup = "nonseasonal",
                        name = "Non-Seasonal") %>%
      plotly::add_segments(x = min(df$lag), 
                           xend = max(df$lag), 
                           y = upper, 
                           yend = upper, 
                           line = list(color = "green", dash = "dash"), 
                           legendgroup = "ci", 
                           showlegend = FALSE, 
                           name = "CI Upper Bound") %>%
      plotly::add_segments(x = min(df$lag), 
                           xend = max(df$lag), 
                           y = lower, 
                           yend = lower, 
                           line = list(color = "green", dash = "dash"), 
                           legendgroup = "ci", 
                           showlegend = FALSE, 
                           name = "CI Lower Bound") %>%
      plotly::layout(xaxis = list(dtick = f, title = "Lag"),
                     yaxis = list(title = "ACF"))
    } else {
      df$zero_lag <-  ifelse(df$lag == 0, df$y, NA)
      df$non_seasonal_lag <-  ifelse(df$lag == 0, NA, df$y)
      
      p1 <- plotly::plot_ly()
      
      if(!base::is.null(seasonal_lags)){
        for(l in base::seq_along(seasonal_lags)){
          df[[base::paste("slag_", seasonal_lags[l], sep = "")]] <- ifelse(df$lag %% seasonal_lags[l] ==0 & 
                                                                             df$lag != 0, 
                                                                           df$y, NA)
        df$non_seasonal_lag <- ifelse(df$lag %% seasonal_lags[l]  != 0, df$non_seasonal_lag, NA)
        p1 <- p1 %>%
          plotly::add_trace(x = df$lag,
                            y =  df[[base::paste("slag_",  seasonal_lags[l], sep = "")]],
                            type = "bar",
                            marker = list(color = seasonal_colors[l]), 
                            width = 0.1, 
                            name = base::paste("Seasonal Lag", seasonal_lags[l], sep = " "), 
                            legendgroup = base::paste("slag_", seasonal_lags[l], sep = ""),
                            showlegend = TRUE)
        }
      }
      
      
      p1 <- p1 %>%
        plotly::add_trace(x = df$lag, 
                          y = df$zero_lag, 
                          type = "bar",
                          marker = list(color = "black"), 
                          width = 0.1, 
                          name = "Lag-Zero", 
                          legendgroup = "lagzero",
                          showlegend = FALSE) %>%
        plotly::add_trace(x = df$lag, 
                          y = df$non_seasonal_lag, 
                          type = "bar", 
                          marker = list(color = "#00526d"), 
                          width = 0.1, 
                          showlegend = FALSE,
                          legendgroup = "lag",
                          name = "Non-Seasonal") %>%
        plotly::add_segments(x = min(df$lag), 
                             xend = max(df$lag), 
                             y = upper, 
                             yend = upper, 
                             line = list(color = "green", dash = "dash"), 
                             legendgroup = "ci", 
                             showlegend = FALSE, 
                             name = "CI Upper Bound") %>%
        plotly::add_segments(x = min(df$lag), 
                             xend = max(df$lag), 
                             y = lower, 
                             yend = lower, 
                             line = list(color = "green", dash = "dash"), 
                             legendgroup = "ci", 
                             showlegend = FALSE, 
                             name = "CI Lower Bound") %>%
        plotly::layout(xaxis = list(dtick = f, title = "Lag"),
                       yaxis = list(title = "ACF"))
    }
    
  }
  
  if(type == "both" || type == "pacf"){
    x <- stats::pacf(ts.obj, lag.max = lag.max, plot = FALSE)
    
    upper <- stats::qnorm((1 + ci)/2)/sqrt(x[[3]])
    lower <- - upper
    
    df <- data.frame(y = as.numeric(x$acf),
                     lag = 1:(base::nrow(x$acf)),
                     stringsAsFactors = FALSE)
    if(seasonal){
    df$seasonal_lag <- ifelse(df$lag %% f  == 0, df$y, NA)
    df$non_seasonal_lag <- ifelse(df$lag %% f  != 0, df$y, NA)
    
    
    p2 <- plotly::plot_ly()
    
    showlegend <- ifelse(type == "both", FALSE, TRUE)
    
    if(!base::is.null(seasonal_lags)){
      for(l in base::seq_along(seasonal_lags)){
        df[[base::paste("slag_", seasonal_lags[l], sep = "")]] <- ifelse(df$lag %% seasonal_lags[l] ==0 & 
                                                                           df$lag != 0 & 
                                                                           base::is.na(df$seasonal_lag), 
                                                                         df$y, NA)
      df$non_seasonal_lag <- ifelse(df$lag %% seasonal_lags[l]  != 0, df$non_seasonal_lag, NA)
      p2 <- p2 %>%
        plotly::add_trace(x = df$lag,
                          y =  df[[base::paste("slag_", seasonal_lags[l], sep = "")]],
                          type = "bar",
                          marker = list(color = seasonal_colors[l]), 
                          width = 0.1, 
                          name = base::paste("Seasonal Lag", seasonal_lags[l], sep = " "), 
                          legendgroup = base::paste("slag_", seasonal_lags[l], sep = ""),
                          showlegend = showlegend)
      }
    }
    
    
    p2 <- p2 %>%
      plotly::add_trace(x = df$lag, 
                        y = df$seasonal_lag, 
                        type = "bar", 
                        marker = list(color = "red"), 
                        width = 0.1, 
                        legendgroup = "seasonal",
                        showlegend = showlegend,
                        name = base::paste("Seasonal Lag", f, sep = " ")) %>%
      plotly::add_trace(x = df$lag, 
                        y = df$non_seasonal_lag, 
                        type = "bar", 
                        marker = list(color = "#00526d"), 
                        width = 0.1, 
                        legendgroup = "nonseasonal",
                        showlegend = showlegend,
                        name = "Non-Seasonal") %>%
      plotly::add_segments(x = min(df$lag), 
                           xend = max(df$lag), 
                           y = upper, 
                           yend = upper, 
                           line = list(color = "green", dash = "dash"), 
                           legendgroup = "ci", 
                           showlegend = FALSE, 
                           name = "CI Upper Bound") %>%
      plotly::add_segments(x = min(df$lag), 
                           xend = max(df$lag), 
                           y = lower, 
                           yend = lower, 
                           line = list(color = "green", dash = "dash"), 
                           legendgroup = "ci", 
                           showlegend = FALSE, 
                           name = "CI Lower Bound") %>%
      plotly::layout(xaxis = list(dtick = f, title = "Lag"),
                      yaxis = list(title = "PACF"))
    } else {
      df$non_seasonal_lag <- df$y
      p2 <- plotly::plot_ly()
      
      if(!base::is.null(seasonal_lags)){
        for(l in base::seq_along(seasonal_lags)){
          df[[base::paste("slag_", seasonal_lags[l], sep = "")]] <- ifelse(df$lag %% seasonal_lags[l] ==0 & 
                                                                             df$lag != 0, 
                                                                           df$y, NA)
        df$non_seasonal_lag <- ifelse(df$lag %% seasonal_lags[l]  != 0, 
                                      df$non_seasonal_lag, NA)
        p2 <- p2 %>%
          plotly::add_trace(x = df$lag,
                            y =  df[[base::paste("slag_", seasonal_lags[l], sep = "")]],
                            type = "bar",
                            marker = list(color = seasonal_colors[l]), 
                            width = 0.1, 
                            name = base::paste("Seasonal Lag", seasonal_lags[l], sep = " "), 
                            legendgroup = base::paste("slag_", seasonal_lags[l], sep = ""),
                            showlegend = showlegend)
        }
      }
      
      
      
      p2 <- p2 %>%
        plotly::add_trace(x = df$lag, 
                          y = df$non_seasonal_lag, 
                          type = "bar", 
                          marker = list(color = "#00526d"), 
                          width = 0.1, 
                          showlegend = FALSE,
                          name = "Lags") %>%
        plotly::add_segments(x = min(df$lag), 
                             xend = max(df$lag), 
                             y = upper, 
                             yend = upper, 
                             line = list(color = "green", dash = "dash"), 
                             legendgroup = "ci", 
                             showlegend = FALSE, 
                             name = "CI Upper Bound") %>%
        plotly::add_segments(x = min(df$lag), 
                             xend = max(df$lag), 
                             y = lower, 
                             yend = lower, 
                             line = list(color = "green", dash = "dash"), 
                             legendgroup = "ci", 
                             showlegend = FALSE, 
                             name = "CI Lower Bound") %>%
        plotly::layout(xaxis = list(dtick = f, title = "Lag"),
                       yaxis = list(title = "PACF"))
    }
  }
  
  if(type == "both"){
    output <- plotly::subplot(p1, p2, nrows = 2, shareX = TRUE, titleY = TRUE) %>% 
      plotly::layout(title = base::paste(obj.name, "ACF and PACF Plots", sep = " "))
  } else if(type == "acf"){
    output <- p1 %>%
      plotly::layout(title = base::paste(obj.name, "ACF Plot", sep = " "))
  } else if(type == "pacf"){
    output <- p2 %>%
      plotly::layout(title = base::paste(obj.name, "PACF Plot", sep = " "))
  }
  base::options(warn = storeWarn) 
  return(base::suppressWarnings(output))
  
}


