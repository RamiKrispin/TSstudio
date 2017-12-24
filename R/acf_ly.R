#'  A Visualization Function of the ACF Estimation
#' @export
#' @param ts.obj a univariate or multivariate time series object of class "ts", "mts", "zoo" or "xts"
#' @param lag.max maximum lag at which to calculate the acf. Default is 10*log10(N/m) where N is the number of observations and m the number of series. Will be automatically limited to one less than the number of observations in the series.
#' @param ci the significant level of the estimation - a numeric value between 0 and 1, default is set for 0.95 
#' @examples
#' acf_ly(AirPassengers, lag.max = 60)


acf_ly <- function(ts.obj, lag.max = NULL, ci = 0.95) {
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
                      showgrid = FALSE, range = c(min(x[[1]]), 
                        max(x[[1]]))), annotations = list(text = c, 
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
                      showgrid = FALSE, range = c(min(x[[1]]), 
                        max(x[[1]]))), annotations = list(text = paste(r, 
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
            y = ~acf, width = ~width, name = "ACF") %>% 
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

