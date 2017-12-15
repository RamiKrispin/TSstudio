acf_ly <- function(ts.obj, lag.max = NULL, ci = 0.95){
  `%>%` <- magrittr::`%>%`
  x <- df <- NULL
  x <- acf(ts.obj, lag.max = lag.max, plot = FALSE)
  ci_value <- qnorm((1 + ci)/2)/sqrt(x[[3]])
  df <- data.frame(lag = x[[4]], acf = x[[1]], width = 0.01, ci_u = ci_value, ci_d = -ci_value)

  p <- plotly::plot_ly(data = df) %>% 
    plotly::add_bars(x = ~lag, y = ~acf, width = ~ width) %>%
    plotly::add_trace(x = ~ lag, y = ~ci_u, type = "scatter", mode = "lines", 
                      line = list(width = 1, dash = "dash", color = "green")) %>%
    plotly::add_trace(x = ~ lag, y = ~ci_d, type = "scatter", mode = "lines", 
                      line = list(width = 1, dash = "dash", color = "green")) %>%
    plotly::layout(
      xaxis = list(title = "Lag"),
      yaxis = list(title = "ACF")
    ) %>%
    plotly::hide_legend()
  
  
  return(p)
}
