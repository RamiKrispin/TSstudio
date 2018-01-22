#'  Time Series Lag Visualization
#' @export
#' @param ts.obj A univariate time series object of a class "ts", "zoo" or "xts" (support only series with either monthly or quarterly frequency)
#' @param max.lags An integer, number of lags to plot
#' @param Xshare Plotly parameter, should the x-axis be shared amongst the subplots?
#' @param Yshare Plotly parameter, should the y-axis be shared amongst the subplots?
#' @param Xtitle Plotly parameter, should x-axis titles be retained?
#' @param Ytitle Plotly parameter, should y-axis titles be retained?
#' @param margin Plotly parameter, either a single value or four values (all between 0 and 1). 
#' If four values are provided, the first is used as the left margin, 
#' the second is used as the right margin, the third is used as the top margin, 
#' and the fourth is used as the bottom margin. 
#' If a single value is provided, it will be used as all four margins.
#' @description Visualization of series with its lags, 
#' can be used to identify a correlation between the series and it lags
#' @examples
#' # Seasonal box plot
#' ts_lags(AirPassengers) 

ts_lags <- function(ts.obj, max.lags = 12, Xtitle = FALSE, Ytitle = TRUE, margin = 0.02, 
                    Xshare = TRUE, Yshare = TRUE){
`%>%` <- magrittr::`%>%`
df <- df_wide <- p <- obj.name <- lag <- lag_plots <- NULL

obj.name <- base::deparse(base::substitute(ts.obj))
# --------------Error handling --------------
if(!is.numeric(max.lags)){
  warning("The 'max.lags' parameter is not valid, using the defualt setting (max.lags = 12)")
  max.lags <- 12
} else if(max.lags == 0){
  warning("The 'max.lags' parameter is not valid, using the defualt setting (max.lags = 12)")
  max.lags <- 12
} else if(round(max.lags) != max.lags){
  warning("The 'max.lags' parameter is not valid, using the defualt setting (max.lags = 12)")
  max.lags <- 12
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
for(g in 1:max.lags){
  if(g == 1){
    lag <- c(NA, ts.df$value[- nrow(df)]) 
  } else {
    lag <- c(NA,lag[-nrow(df)])
  }
  lag_plots[[g]] <- plotly::plot_ly(x = lag, 
                            y = df$value, 
                            type = "scatter",
                            mode = "markers",
                            name = paste("Lag", g, sep = " ")) %>%
    plotly::layout(xaxis = list(title = paste("Lag", g, sep = " "),
                        range = c( min(na.omit(as.numeric(lag))),  
                                   max(na.omit(as.numeric(lag))))),
           yaxis = list(title = paste("Series", sep = ""),
                        range = c( min(na.omit(as.numeric(df$value))),  
                                   max(na.omit(as.numeric(df$value))))),
           title = paste("Series vs Lags", sep = " "),
           annotations = list(text = paste("Lag", g, sep = " "), 
                              xref = "paper", yref = "paper", yanchor = "bottom", 
                              xanchor = "center", align = "center", 
                              x = 0.5, y = 0.9, showarrow = FALSE)
    )
}

p <- plotly::subplot(lag_plots, 
              titleX = FALSE, titleY = TRUE, margin = 0.02, 
              shareX = TRUE, shareY = TRUE,
              nrows = ceiling(length(lag_plots) / 3))%>% 
  plotly::hide_legend()

# -------------- End --------------
return(p)

}
