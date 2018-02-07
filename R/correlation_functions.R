#'  Time Series Lag Visualization
#' @export
#' @param ts.obj A univariate time series object of a class "ts", "zoo" or "xts" (support only series with either monthly or quarterly frequency)
#' @param lag.max An integer, number of lags to plot
#' @param Xshare Plotly parameter, should the x-axis be shared amongst the subplots?
#' @param Yshare Plotly parameter, should the y-axis be shared amongst the subplots?
#' @param Xtitle Plotly parameter, should x-axis titles be retained?
#' @param Ytitle Plotly parameter, should y-axis titles be retained?
#' @param margin Plotly parameter, either a single value or four values (all between 0 and 1). 
#' If four values are provided, the first is used as the left margin, 
#' the second is used as the right margin, the third is used as the top margin, 
#' and the fourth is used as the bottom margin. 
#' If a single value is provided, it will be used as all four margins.
#' @param n_row An integer, define the number of plots per row
#' @description Visualization of series with its lags, 
#' can be used to identify a correlation between the series and it lags
#' @examples
#' data(USgas)
#' 
#' ts_lags(USgas) 

ts_lags <- function(ts.obj, lag.max = 12, Xtitle = FALSE, Ytitle = TRUE, margin = 0.02, 
                    Xshare = TRUE, Yshare = TRUE, n_row = 3){
`%>%` <- magrittr::`%>%`
df <- df_wide <- p <- obj.name <- lag <- lag_plots <- NULL

obj.name <- base::deparse(base::substitute(ts.obj))
# --------------Error handling --------------
if(!is.numeric(lag.max)){
  warning("The 'lag.max' parameter is not valid, using the defualt setting (lag.max = 12)")
  lag.max <- 12
} else if(lag.max == 0){
  warning("The 'lag.max' parameter is not valid, using the defualt setting (lag.max = 12)")
  lag.max <- 12
} else if(round(lag.max) != lag.max){
  warning("The 'lag.max' parameter is not valid, using the defualt setting (lag.max = 12)")
  lag.max <- 12
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
  df <- base::data.frame(dec_left = floor(stats::time(ts.obj)), 
                         dec_right = stats::cycle(ts.obj), value = base::as.numeric(ts.obj))
  if(!stats::frequency(ts.obj) %in% c(4, 12)){
    stop("The frequency of the series is invalid, ",
         "the function support only 'monthly' or 'quarterly' frequencies")
  }
} else if (xts::is.xts(ts.obj) | zoo::is.zoo(ts.obj)) {
  if (!is.null(base::dim(ts.obj))) {
    if (base::dim(ts.obj)[2] > 1) {
      warning("The 'ts.obj' has multiple columns, only the first column will be plot")
      ts.obj <- ts.obj[, 1]
    }
  }
  freq <- xts::periodicity(ts.obj)[[6]]
  if (freq == "quarterly") {
    df <- base::data.frame(dec_left = lubridate::year(ts.obj), 
                           dec_right = lubridate::quarter(ts.obj), 
                           value = as.numeric(ts.obj))
  } else if (freq == "monthly") {
    df <- base::data.frame(dec_left = lubridate::year(ts.obj), 
                           dec_right = lubridate::month(ts.obj), 
                           value = as.numeric(ts.obj))
    # } else if (freq == "weekly") {
    #   df <- data.frame(dec_left = lubridate::year(ts.obj), 
    #                    dec_right = lubridate::week(ts.obj), value = as.numeric(ts.obj))
    # } else if (freq == "daily") {
    #   df <- data.frame(dec_left = lubridate::month(ts.obj), 
    #                    dec_right = lubridate::day(ts.obj), value = as.numeric(ts.obj))
  } else if (!freq %in% c("monthly", "quarterly")) {
    stop("The frequency of the series is invalid,",
         "the function support only 'monthly' or 'quarterly' frequencies")
  }
  
}

df <- df[base::order(df$dec_left, df$dec_right),]

# -------------- Creating the plot --------------
for(g in 1:lag.max){
  if(g == 1){
    lag <- c(NA, df$value[- nrow(df)]) 
  } else {
    lag <- c(NA,lag[-nrow(df)])
  }
  lag_plots[[g]] <- plotly::plot_ly(x = lag, 
                            y = df$value, 
                            type = "scatter",
                            mode = "markers",
                            name = paste("Lag", g, sep = " ")) %>%
    plotly::layout(xaxis = list(title = paste("Lag", g, sep = " "),
                        range = c( base::min(stats::na.omit(as.numeric(lag))),  
                                   base::max(stats::na.omit(as.numeric(lag))))),
           yaxis = list(title = paste("Series", sep = ""),
                        range = c( base::min(stats::na.omit(as.numeric(df$value))),  
                                   base::max(stats::na.omit(as.numeric(df$value))))),
           title = paste("Series vs Lags", sep = " "),
           annotations = list(text = paste("Lag", g, sep = " "), 
                              xref = "paper", yref = "paper", yanchor = "bottom", 
                              xanchor = "center", align = "center", 
                              x = 0.5, y = 0.9, showarrow = FALSE)
    )
}

p <- plotly::subplot(lag_plots, 
              titleX = Xtitle, titleY = Ytitle, margin = margin, 
              shareX = Xshare, shareY = Yshare,
              nrows = ceiling(length(lag_plots) / n_row))%>% 
  plotly::hide_legend()

# -------------- End --------------
return(p)

}

#'  A Visualization Function of the ACF Estimation
#' @export acf_ly ts_acf
#' @aliases acf_ly
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


acf_ly <- function(ts.obj, lag.max = NULL, ci = 0.95, color = NULL) {
 # .Deprecated("ts_acf")
  print("The acf_ly function will be deprecated on the next release, please use ts_acf instead")
  ts_acf(ts.obj, lag.max = lag.max, ci = ci, color = color)
}

#'  A Visualization Function of the PACF Estimation
#' @export pacf_ly ts_pacf
#' @aliases pacf_ly
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

pacf_ly <- function(ts.obj, lag.max = NULL, ci = 0.95, color = NULL) {
  # .Deprecated("ts_pacf")
  print("The pacf_ly function will be deprecated on the next release, please use ts_pacf instead")
  ts_acf(ts.obj, lag.max = lag.max, ci = ci, color = color)
}