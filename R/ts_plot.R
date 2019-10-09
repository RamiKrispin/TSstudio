#'  Plotting Time Series Objects
#' @export ts_plot
#' @description Visualization functions for time series object
#' @param ts.obj A univariate or multivariate time series object of class "ts", "mts", "zoo", "xts", or any data frame object with 
#' a minimum of one numeric column and either a Date or POSIXt class column
#' @param line.mode A plotly argument, define the plot type, c("lines", "lines+markers", "markers")
#' @param width An Integer, define the plot width, default is set to 2
#' @param dash A plotly argument, define the line style, c(NULL, "dot", "dash")
#' @param color The color of the plot, support both name and expression
#' @param slider Logic, add slider to modify the time axis (default set to FALSE)
#' @param type A character, optional, if having multiple tims series object,
#' will plot all series in one plot when set to "single" (default), 
#' or plot each series on a separate plot when set to "multiple"
#' @param Xtitle A character, set the X axis title, default set to NULL
#' @param Ytitle A character, set the Y axis title, default set to NULL
#' @param title A character, set the plot title, default set to NULL
#' @param Ygrid Logic,show the Y axis grid if set to TRUE
#' @param Xgrid Logic,show the X axis grid if set to TRUE
#' @examples
#' data(USVSales)
#' ts_plot(USVSales)
#' 
#' # adding slider
#' ts_plot(USVSales, slider = TRUE)

ts_plot <- function(ts.obj, line.mode = "lines", width = 2, 
                      dash = NULL, color = NULL, 
                      slider = FALSE, type = "single",
                      Xtitle = NULL, Ytitle = NULL, title = NULL,
                      Xgrid = FALSE, Ygrid = FALSE){
  `%>%` <- magrittr::`%>%`
  df <- p <- plot_list <- dim_flag <- plot_list <- obj.name <- NULL 
  col_class <- col_date <- col_POSIXt <- date_col <-  numeric_col <- NULL
  obj.name <- base::deparse(base::substitute(ts.obj))
  
  # Error handling
  if(!base::is.null(color)){
    if(!base::is.character(color)){
      warning("The value of the 'color' parameter is not valid")
      color = "#00526d"
    }
  } else{
    color = "#00526d"
  }
  
  
  if(!base::is.null(Xtitle)){
    if(!base::is.character(Xtitle)){
      warning("The value of the 'Xtitle' is not valid")
      Xtitle <- ""
    } 
  } else {
    Xtitle <- ""
  }
  
  if(!base::is.null(Ytitle)){
    if(!base::is.character(Ytitle)){
      warning("The value of the 'Ytitle' is not valid")
      Ytitle <- ""
    } 
  } else {
    Ytitle <- ""
  }
  

  if(line.mode != "lines" & 
     line.mode != "lines+markers" & 
     line.mode != "markers"){
    warning("The value of 'line.mode' is not valid, using the default option - 'lines'")
    line.mode <- "lines"
  }
  
  if(!base::is.numeric(width)){
    warning("The value of 'width' is not valude, using the default value - 2")
    width <- 2
  } else if(width%%1 != 0){
    warning("The value of 'width' is not valude, using the default value - 2")
    width <- 2
  }
  
  if(type != "single" & 
     type != "multiple"){
    warning("The value of 'type' is not valid, using the default option - 'multiple'")
    type <- "multiple"
  }
 
  
  
  if(stats::is.ts(ts.obj)){# Case 1 the object is a time series 
    # Check if the object has multiple time series
    if(stats::is.mts(ts.obj)){
      dim_flag <- TRUE # If multiple time series object, flag it
      # Create the data frame
      df <- data.frame(date = stats::time(ts.obj), ts.obj)
    } else {
      dim_flag <- FALSE
      # Create the data frame
      df <- data.frame(date = stats::time(ts.obj), y = as.numeric(ts.obj))
    }
    
    
  } else if(zoo::is.zoo(ts.obj) | xts::is.xts(ts.obj)) { # Case 2 the object is either a zoo or xts object
    # Check if the object has multiple time series
    if(base::is.null(base::dim(ts.obj))){
      dim_flag <- FALSE
      # Create the data frame
      df <- base::data.frame(date = zoo::index(ts.obj), y = as.numeric(ts.obj))
    } else if(base::dim(ts.obj)[2] > 1){
      dim_flag <- TRUE
      # Create the data frame
      df <- base::data.frame(date = zoo::index(ts.obj), as.data.frame(ts.obj))
    } else if(base::dim(ts.obj)[2] ==1 ){
      dim_flag <- FALSE
      # Create the data frame
      df <- base::data.frame(date = zoo::index(ts.obj), y = as.numeric(ts.obj))
    }
    
    
  } else if(base::is.data.frame(ts.obj) | 
            dplyr::is.tbl(ts.obj) | 
            data.table::is.data.table(ts.obj)){ # Case 3 the object is a data frame 
    # Identify the columns classes
    
    ts.obj <- base::as.data.frame(ts.obj)
    col_class <- base::lapply(ts.obj, class)
    col_date <- base::lapply(ts.obj, lubridate::is.Date)
    col_POSIXt <- base::lapply(ts.obj, lubridate::is.POSIXt)
    
    # Check if Date object exist
    if(any(col_date == TRUE) & any(col_POSIXt == TRUE)){
      d <- t <- NULL
      d <- base::min(base::which(col_date == TRUE))
      t <- base::min(base::which(col_POSIXt == TRUE))
      if(d > t){
        warning("The data frame contain multiple date or time objects,",
                "using the first one as the plot index")
        date_col <- t
      } else {
        warning("The data frame contain multiple date or time objects,",
                "using the first one as the plot index")
        date_col <- d
      }
    } else if(base::any(col_date == TRUE) | base::any(col_POSIXt == TRUE)){
      if(base::any(col_date == TRUE)){
        if(base::length(base::which(col_date == TRUE)) > 1){
          date_col <-  base::min(base::which(col_date == TRUE))
          warning("There are multipe 'date' objects in the data frame,",
                  "using the first one object as the plot index")
        } else {
          date_col <-  base::min(base::which(col_date == TRUE))
      }
      } else if(base::any(col_POSIXt == TRUE)){
        if(base::length(base::which(col_POSIXt == TRUE)) > 1){
          date_col <-  base::min(base::which(col_POSIXt == TRUE))
          warning("There are multipe 'POSIXt' objects in the data frame,",
                  "using the first one as the plot index")
        } else {
          date_col <-  base::min(base::which(col_POSIXt == TRUE))
        }
      } 
      }else {
        stop("No 'Date' or 'POSIXt' object available in the data frame,", 
             "please check if the data format defined properly")
      }
      

    # Identify the numeric/integer objects in the data frame  
    numeric_col <- base::which(col_class == "numeric" | col_class == "integer")
    # Stop if there is no any numeric values in the data frame, otherwise build the data frame 
    if(base::length(numeric_col) == 0){
      stop("None of the data frame columns is numeric,", 
           "please check if the data format is defined properly")
    }
    
    # Check if the object has multiple time series
    if(length(numeric_col) == 1){
      dim_flag <- FALSE
      df <- base::data.frame(date = ts.obj[, date_col], y =  ts.obj[, numeric_col])
    } else {
      dim_flag <- TRUE
      df <- base::data.frame(date = ts.obj[, date_col], ts.obj[, numeric_col])
    }
  } else{
    stop('Invalid class \n Please make sure the object class is either',  
         '"ts", "mts", "xts", "zoo" or data frame with date object') 
  } 
  
  if(base::ncol(df) == 2){
    showlegend <- FALSE
  } else {
    showlegend <- TRUE
  }
  
  if(dim_flag){
    if(type == "single"){
      p <- plotly::plot_ly()

      for(i in 2:base::ncol(df)){
        p <- p %>% plotly::add_trace(x = df[,1], y = df[,i],
                             name = names(df)[i],
                             mode = "lines",
                             type = 'scatter',
                             showlegend = showlegend)
      }
      p <- p %>% plotly::layout(
        xaxis = list(title = Xtitle, showgrid = Xgrid),
        yaxis = list(title = Ytitle, showgrid = Ygrid)
        
      )
      if(!base::is.null(p) & slider){
        p <- p %>% 
          plotly::layout(
            yaxis = list(showgrid = Ygrid),
            xaxis = list(showgrid = Xgrid,
              rangeslider = list(type = "date"))
          )
      }
    } else if(type == "multiple"){
      for(i in 2:ncol(df)){
        plot_list[[i-1]] <- plotly::plot_ly(x = df[,1], y = df[,i], 
                                    mode = "lines", 
                                    name = names(df)[i],
                                    type = 'scatter'
        )%>% 
          plotly::layout(
            xaxis = list(title = Xtitle, showgrid = Xgrid),
            yaxis = list(title = names(df)[i], showgrid = Ygrid)
          )
      }
      
      p <- plotly::subplot(plot_list, nrows = ncol(df) - 1,
                   titleY = TRUE, titleX = TRUE, shareX = TRUE,
                   margin = 0.05) %>%
        plotly::hide_legend()
      if(!base::is.null(p) & slider){
        warning('The slider option is not avilable for plot type "multiple"')
      }
      
    }
  } else {
    
    p <-  switch (line.mode,
                  "markers" = {
                    plotly::plot_ly(data = df, x = ~ date, y = ~y, 
                            mode = "markers", 
                            type = 'scatter',
                            marker = list(color = color, width = width)
                    )
                  },
                  "lines+markers" = {
                    plotly::plot_ly(data = df, x = ~ date, y = ~y, 
                            mode = "lines+markers", 
                            type = 'scatter',
                            marker = list(color = color),
                            line = list(width = width, dash = dash, color = color)
                    )
                  },
                  "lines" = {
                    plotly::plot_ly(data = df, x = ~ date, y = ~y, 
                            mode = "lines", 
                            type = 'scatter',
                            line = list(width = width, dash = dash, color = color)
                    )
                  }
                  
    )
    
    if(!base::is.null(p) & slider){
      p <- p %>% 
        plotly::layout(
          xaxis = list(rangeslider = list(type = "date"))
        )
      
    } 
    
    p <- p %>% 
      plotly::layout(
        xaxis = list(title = Xtitle, showgrid = Xgrid),
        yaxis = list(title = Ytitle, showgrid = Ygrid)
      )
  }

  if(base::is.null(title)){
    p <- p %>% plotly::layout(title = obj.name)
  } else {
    p <- p %>% plotly::layout(title = title)
  }
  
  if(base::is.null(p)){
    stop("Could not create the plot, please check the input")
  } else{
    return(p)
  }
}


#'  Plotting Forecast Object
#' @export plot_forecast
#' @description Visualization functions for forecast package forecasting objects
#' @param forecast_obj A forecast object from the forecast, forecastHybrid, or bsts packages
#' @param title A character, a plot title, optional
#' @param Xtitle Set the X axis title, default set to NULL
#' @param Ytitle Set the Y axis title, default set to NULL
#' @param color A character, the plot, support both name and expression
#' @param width An Integer, define the plot width, default is set to 2 
#' @examples
#' data(USgas)
#' library(forecast)
#' fit <- ets(USgas)
#' fc<- forecast(fit, h = 60)
#' plot_forecast(fc)

plot_forecast <- function(forecast_obj,
                          title = NULL,
                          Xtitle = NULL,
                          Ytitle = NULL,
                          color = NULL,
                          width = 2){

`%>%` <- magrittr::`%>%`  
  
# Error handling 
  
  if(!base::is.null(color)){
    if(!base::is.character(color)){
      warning("The value of the 'color' parameter is not valid")
      color = "#00526d"
    }
  } else{
    color = "#00526d"
  }
  
  if(!base::is.numeric(width)){
    warning("The value of 'width' is not valude, using the default value - 2")
    width <- 2
  } else if(width%%1 != 0){
    warning("The value of 'width' is not valude, using the default value - 2")
    width <- 2
  } 

  if(!base::is.null(title)){
    if(!base::is.character(title)){
      warning("The value of the 'title' argument is not valid")
      title <- ""
    } 
  } else {
    title <- ""
  }
  
  if(!base::is.null(Xtitle)){
    if(!base::is.character(Xtitle)){
      warning("The value of the 'Xtitle' is not valid")
      Xtitle <- ""
    } 
  } else {
    Xtitle <- ""
  }
  
  if(!base::is.null(Ytitle)){
    if(!base::is.character(Ytitle)){
      warning("The value of the 'Ytitle' is not valid")
      Ytitle <- ""
    } 
  } else {
    Ytitle <- ""
  }  
# Setting the plot
 if(forecast::is.forecast(forecast_obj)){ 
   p <- NULL
  p <- plotly::plot_ly() %>%
    plotly::add_lines(x = stats::time(forecast_obj$x) + stats::deltat(forecast_obj$x), y = forecast_obj$x,
                       name = "Observed",
                       mode = "lines", 
                       type = 'scatter',
                       line = list(width = width, color = color)
  ) 
  
# Checking if the object has confidence interval
  if("upper" %in% base::names(forecast_obj) &
     "lower" %in% base::names(forecast_obj)){
    # bug fix for if forecast has only one level
    lvls <- if(is.null(dim(forecast_obj$upper))) 1 else dim(forecast_obj$upper)[2]
    for(i in 1:lvls){
      p <- p %>% plotly::add_ribbons(x = stats::time(forecast_obj$mean) + stats::deltat(forecast_obj$mean), 
                                     ymin = if(lvls > 1) forecast_obj$lower[, i] else forecast_obj$lower, 
                                     ymax = if(lvls > 1) forecast_obj$upper[, i] else forecast_obj$upper,
                                     color = I(base::paste("gray", base::as.numeric(sub("%", "", (forecast_obj$level[i]))) - 5*i, sep = "")),
                                     name = base::paste(forecast_obj$level[i], "% confidence", sep = "")
                                     )
    }
  } else {
    warning("The forecasted object does not have a confidence interval")
  }
  
  p <- p %>%
    plotly::add_lines(x = stats::time(forecast_obj$mean) + stats::deltat(forecast_obj$mean), y = forecast_obj$mean, 
                      name = "Forecasted",
                      line = list(width = width, color = color, dash = "dash")
    ) %>%
    plotly::layout(title = title,
                     xaxis = list(title = Xtitle),
                     yaxis = list(title = Ytitle))
  return(p)
 } else if(class(forecast_obj) == "bsts.prediction"){
   p <- NULL

   x_index_start <- base::max(zoo::index(forecast_obj$original.series)) + 1
   x_forecast <- c(x_index_start:(x_index_start + base::length(forecast_obj$mean) -1))
   y_forecast <- forecast_obj$mean
   p <- plotly::plot_ly() %>%
      plotly::add_lines(x = zoo::index(forecast_obj$original.series), y = as.numeric(forecast_obj$original.series),
                        name = "Observed",
                        mode = "lines",
                        type = 'scatter',
                        line = list(width = width, color = color)
      )

    intervals_str <- rownames(forecast_obj$interval)
    intervals_numeric <- as.numeric(gsub("%", "", intervals_str))
    if(base::length(intervals_str) %% 2 == 0){
      intervals_temp <- c <- NULL
      intervals_temp <- intervals_numeric
      c <- 1
      while(base::length(intervals_temp) > 0){
      lower <- intervals_temp[which.min(intervals_temp)]
      upper <- intervals_temp[which.max(intervals_temp)]
      p <- p %>% plotly::add_ribbons(x = x_forecast,
                                     ymin = forecast_obj$interval[which(intervals_numeric == lower), ],
                                     ymax = forecast_obj$interval[which(intervals_numeric == upper), ],
                                     color = I(base::paste("gray", round(upper - c*5) , sep = "")),
                                     name = base::paste(100 - 2 * (100 - upper), "% confidence", sep = "")
      )
      c <- c + 1
      intervals_temp[base::which.min(intervals_temp)] <- NA
      intervals_temp[base::which.max(intervals_temp)] <- NA
      intervals_temp <- intervals_temp[!is.na(intervals_temp)]
      }
    } else if(base::length(intervals_str) > 2){
      warning("The number of the available confidence intervals is not symmetric, will use only the upper and lower intervals")
      intervals_temp <- c <- NULL
      intervals_temp <- intervals_numeric
      c <- 1
      lower <- intervals_temp[which.min(intervals_temp)]
      upper <- intervals_temp[which.max(intervals_temp)]
      p <- p %>% plotly::add_ribbons(x = x_forecast,
                                     ymin = forecast_obj$interval[which(intervals_numeric == lower), ],
                                     ymax = forecast_obj$interval[which(intervals_numeric == upper), ],
                                     color = I(base::paste("gray", round(upper - c*5) , sep = "")),
                                     name = base::paste(100 - 2 * (100 - upper), "% confidence", sep = "")
      )
    }
    p <- p %>%
      plotly::add_lines(x = x_forecast, y = y_forecast, 
                        name = "Forecasted",
                        line = list(width = width, color = color, dash = "dash")
      ) %>%
      plotly::layout(title = title,
                     xaxis = list(title = Xtitle),
                     yaxis = list(title = Ytitle))
    return(p)
 } else{
    stop("The input object is neither 'forecast' nor 'bsts.prediction' object")
  }
  
  
}
