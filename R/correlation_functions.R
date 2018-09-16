#'  Time Series Lag Visualization
#' @export
#' @param ts.obj A univariate time series object of a class "ts", "zoo" or "xts" 
#' @param lags An integer, set the lags range, by default will plot the first 12 lags
#' @param Xshare Plotly parameter, should the x-axis be shared amongst the subplots?
#' @param Yshare Plotly parameter, should the y-axis be shared amongst the subplots?
#' @param margin Plotly parameter, either a single value or four values (all between 0 and 1). 
#' If four values are provided, the first is used as the left margin, 
#' the second is used as the right margin, the third is used as the top margin, 
#' and the fourth is used as the bottom margin. 
#' If a single value is provided, it will be used as all four margins.
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
  df <- df_wide <- p <- obj.name <- lag <- lag_plots <- NULL
  
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
  
  if(!is.logical(Xtitle)){
    warning("The 'Xtitle' parameter is not valid, please use only boolean operators.",
            " Using the defualt setting setting (Xtitle = FALSE")
    Xtitle <- FALSE
  }
  
  if(!is.logical(Ytitle)){
    warning("The 'Ytitle' parameter is not valid, please use only boolean operators.",
            " Using the defualt setting setting (Ytitle = TRUE")
    Ytitle <- TRUE
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
  
  p <- plotly::subplot(p_list, nrows = ceiling(length(p_list) / n_plots), 
                       margin = margin, 
                       shareX = Xshare, shareY = Yshare) %>%
    plotly::layout(title = paste(obj.name, "- Series (Y axis) vs. Lags (X axis)", sep = " ")) %>%
    plotly::hide_legend()
  
  
  
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

