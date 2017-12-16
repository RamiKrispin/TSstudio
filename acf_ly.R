acf_ly <- function(ts.obj, lag.max = NULL, ci = 0.95){
  `%>%` <- magrittr::`%>%`
  
  if(is.null(ts.obj)){
    stop("The time series object is NULL")
  } else if(!is.ts(ts.obj) & !xts::is.xts(ts.obj) & !zoo::is.zoo(ts.obj)){
    stop('Invalid class - Please make sure the object class is either "ts", "mts", "xts" or "zoo"')
  }
  
  if(ci > 1 | ci < 0){
    warning("The 'ci' value is out of bound (0-1), the default option of 0.95 will be used")
    ci <- 0.95
  }
  
  x <- df <- obj.name <- NULL
  
  obj.name <- deparse(substitute(ts.obj))
  x <- acf(ts.obj, lag.max = lag.max, plot = FALSE)
  
  
  if(!is.null(lag.max)){
    if(lag.max > x[[3]]){
      warning(paste("The value of 'lag.max' exceed the number of available lags in the series, 
                    by default will use the max available lags", sep = " "))
    }
  }
  
  ci_value <- qnorm((1 + ci)/2)/sqrt(x[[3]])
  
  df <- data.frame(lag = round(x[[4]], 3), 
                   acf = round(x[[1]], 3), 
                   width = 0.02, 
                   ci_u = round(ci_value,2), 
                   ci_d = round(-ci_value,2)
                   )
  series_list <- series <- NULL
    if(ncol(df) > 5){
      if(!is.null(x[[6]])){
        series <- x[[6]]
      } else{
        for(i in 1:(dim(ts.obj)[2])){
          series <- c(series, paste("Series", i, sep = " "))
        }
      }
      counter <- 0
      for(r in series){
        for(c in series){
          counter <- counter + 1
          if(r == c){
            series_list[counter] <- plotly::plot_ly() %>% 
              plotly::add_bars(x = df[, counter], y = df[,(length(series))^2 + counter ], 
                               width = df$width, name = "ACF") %>%
              plotly::add_trace(x = df[, counter], y = df$ci_u, type = "scatter", mode = "lines", 
                                line = list(width = 1, dash = "dash", color = "green")) %>%
              plotly::add_trace(x = df[, counter], y = df$ci_d, type = "scatter", mode = "lines", 
                                line = list(width = 1, dash = "dash", color = "green")) %>%
              plotly::layout(
                xaxis = list(title = "Lag", showgrid = FALSE),
                yaxis = list(title = "ACF", showgrid = FALSE, range = c(min(x[[1]]), max(x[[1]]))),
                annotations = list(
                  text = c,
                  xref = "paper",
                  yref = "paper",
                  yanchor = "bottom",
                  xanchor = "center",
                  align = "center",
                  x = 0.5,
                  y = 1,
                  showarrow = FALSE
                )
              )
          } else {
            series_list[counter] <- plotly::plot_ly() %>% 
              plotly::add_bars(x = df[, counter], y = df[,(length(series))^2 + counter ], 
                               width = df$width, name = "ACF") %>%
              plotly::add_trace(x = df[, counter], y = df$ci_u, type = "scatter", mode = "lines", 
                                line = list(width = 1, dash = "dash", color = "green")) %>%
              plotly::add_trace(x = df[, counter], y = df$ci_d, type = "scatter", mode = "lines", 
                                line = list(width = 1, dash = "dash", color = "green")) %>%
              plotly::layout(
                xaxis = list(title = "Lag", showgrid = FALSE),
                yaxis = list(title = "ACF", showgrid = FALSE, range = c(min(x[[1]]), max(x[[1]]))),
                annotations = list(
                  text = paste(r, c, sep = " & "),
                  xref = "paper",
                  yref = "paper",
                  yanchor = "bottom",
                  xanchor = "center",
                  align = "center",
                  x = 0.5,
                  y = 0.9,
                  showarrow = FALSE
                )
              )
          }
        }
      }
      p <- plotly::subplot(series_list, nrows = length(series), 
                           titleX = TRUE, titleY = TRUE, margin = 0.03,
                           shareX = FALSE, shareY = TRUE) %>% 
        plotly::hide_legend() %>% 
        plotly::layout(title = "ACF", margin = 0.06)
      }
    
  p
  
  
  ci_value <- qnorm((1 + ci)/2)/sqrt(x[[3]])
  df <- data.frame(lag = round(x[[4]], 3), 
                   acf = round(x[[1]], 3), 
                   width = 0.02, 
                   ci_u = round(ci_value,2), 
                   ci_d = round(-ci_value,2)
                   )

  p <- plotly::plot_ly(data = df) %>% 
    plotly::add_bars(x = ~lag, y = ~acf, width = ~ width, name = "ACF") %>%
    plotly::add_trace(x = ~ lag, y = ~ci_u, type = "scatter", mode = "lines", 
                      line = list(width = 1, dash = "dash", color = "green")) %>%
    plotly::add_trace(x = ~ lag, y = ~ci_d, type = "scatter", mode = "lines", 
                      line = list(width = 1, dash = "dash", color = "green")) %>%
    plotly::layout(
      title = paste("Series", obj.name, sep = " "),
      xaxis = list(title = "Lag", showgrid = FALSE),
      yaxis = list(title = "ACF", showgrid = FALSE)
    ) %>%
    plotly::hide_legend()
  
  
  return(p)
}

pacf_ly <- function(ts.obj, lag.max = NULL, ci = 0.95){
  `%>%` <- magrittr::`%>%`
  x <- df <- obj.name <- NULL
  obj.name <- deparse(substitute(ts.obj))
  x <- pacf(ts.obj, lag.max = lag.max, plot = FALSE)
  ci_value <- qnorm((1 + ci)/2)/sqrt(x[[3]])
  df <- data.frame(lag = x[[4]], acf = x[[1]], width = 0.02, ci_u = ci_value, ci_d = -ci_value)
  
  p <- plotly::plot_ly(data = df) %>% 
    plotly::add_bars(x = ~lag, y = ~acf, width = ~ width) %>%
    plotly::add_trace(x = ~ lag, y = ~ci_u, type = "scatter", mode = "lines", 
                      line = list(width = 1, dash = "dash", color = "green")) %>%
    plotly::add_trace(x = ~ lag, y = ~ci_d, type = "scatter", mode = "lines", 
                      line = list(width = 1, dash = "dash", color = "green")) %>%
    plotly::layout(
      title = paste("Series", obj.name, sep = " "),
      xaxis = list(title = "Lag", showgrid = FALSE),
      yaxis = list(title = "ACF", showgrid = FALSE)
    ) %>%
    plotly::hide_legend()
  
  
  return(p)
}
