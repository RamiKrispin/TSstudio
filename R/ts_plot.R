#'  Plotting Time Series Objects
#' @export ts_plot ts.plot_ly
#' @aliases  ts.plot_ly
#' @description Visualization functions for time series object
#' @param ts.obj A univariate or multivariate time series object of class "ts", "mts", "zoo" or "xts"
#' @param line.mode A plotly argument, define the plot type, c("lines", "lines+markers", "markers")
#' @param width The plot width, default is set to 1 (an integer)
#' @param dash A plotly argument, define the line style, c(NULL, "dot", "dash")
#' @param color The color of the plot, support both name and expression
#' @param slider Logic, add slider to modify the time axis (default set to FALSE)
#' @param type Applicable for multiple time series object, plot on a separate plot or all together c("single, "multiple) 
#' @param Xtitle Set the X axis title, default set to NULL
#' @param Ytitle Set the Y axis title, default set to NULL
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
                      slider = FALSE, type = "multiple",
                      Xtitle = NULL, Ytitle = NULL,
                      Xgrid = FALSE, Ygrid = FALSE){
  `%>%` <- magrittr::`%>%`
  df <- p <- plot_list <- dim_flag <- plot_list <- obj.name <- NULL 
  obj.name <- base::deparse(base::substitute(ts.obj))
  
  # Error handling
  if(!is.null(color)){
    if(!is.character(color)){
      warning("The value of the 'color' parameter is not valid")
      color = "#00526d"
    }
  } else{
    color = "#00526d"
  }
  
  
  if(!is.null(Xtitle)){
    if(!is.character(Xtitle)){
      warning("The value of the 'Xtitle' is not valid")
      Xtitle <- ""
    } 
  } else {
    Xtitle <- ""
  }
  
  if(!is.null(Ytitle)){
    if(!is.character(Ytitle)){
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
  
  if(!is.numeric(width)){
    warning("The value of 'width' is not valude, using the default value - 1")
    width <- 1
  } else if(width%%1 != 0){
    warning("The value of 'width' is not valude, using the default value - 1")
    width <- 1
  }
  
  if(type != "single" & 
     type != "multiple"){
    warning("The value of 'type' is not valid, using the default option - 'multiple'")
    type <- "multiple"
  }
  # Check if it is a multiple time series  
  if(!is.null(base::dim(ts.obj))){
    if(base::dim(ts.obj)[2] > 1){
      dim_flag <- TRUE
      if(stats::is.mts(ts.obj)){
        df <- data.frame(date = stats::time(ts.obj), as.data.frame(ts.obj))
      } else if(xts::is.xts(ts.obj) | zoo::is.zoo(ts.obj)){
        df <- data.frame(date = zoo::index(ts.obj), as.data.frame(ts.obj))
      } else{
        stop('Invalid class \n Please make sure the object class is either "ts", "mts", "xts" or "zoo"') 
      }
    } else {
      dim_flag <- FALSE
    } 
  }  else {
    dim_flag <- FALSE
  } 
  
  if(dim_flag){
    if(type == "single"){
      p <- plotly::plot_ly()
      for(i in 2:ncol(df)){
        p <- p %>% plotly::add_trace(x = df[,1], y = df[,i],
                             name = names(df)[i],
                             mode = "lines",
                             type = 'scatter')
      }
      p <- p %>% plotly::layout(
        xaxis = list(title = "Date", showgrid = Xgrid),
        yaxis = list(title = obj.name, showgrid = Ygrid)
        
      )
      if(!is.null(p) & slider){
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
            xaxis = list(title = "Date", showgrid = Xgrid),
            yaxis = list(title = names(df)[i], showgrid = Ygrid)
          )
      }
      
      p <- plotly::subplot(plot_list, nrows = ncol(df) - 1,
                   titleY = TRUE,
                   margin = 0.05) %>%
        plotly::hide_legend()
      if(!is.null(p) & slider){
        warning('The slider option is not avilable for plot type "multiple"')
      }
      
    }
  } else{
    if(zoo::is.zoo(ts.obj) | xts::is.xts(ts.obj)){
      df <- data.frame(date = zoo::index(ts.obj), y = as.numeric(ts.obj))
    } else if (stats::is.ts(ts.obj)){
      df <- data.frame(date = stats::time(ts.obj), y = as.numeric(ts.obj))
    } else {
      stop('Invalid class \n Please make sure the object class is either "ts", "mts", "xts" or "zoo"')
    }
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
    
    if(!is.null(p) & slider){
      p <- p %>% 
        plotly::layout(
          title = obj.name,
          yaxis = list(title = Ytitle, showgrid = Ygrid),
          xaxis = list(
            title = Xtitle, showgrid = Xgrid,
            rangeslider = list(type = "date"))
        )
    } else if(!is.null(p) & !slider){
          p <- p %>% 
          plotly::layout(
            xaxis = list(title = Xtitle, showgrid = Xgrid),
            yaxis = list(title = Ytitle, showgrid = Ygrid),
          title = obj.name
          )
      }
  }
  if(is.null(p)){
    stop("Could not create the plot, please check the input")
  } else{
    return(p)
  }
}

ts.plot_ly <- function(ts.obj, line.mode = "lines", width = 2, 
                       dash = NULL, color = NULL, 
                       slider = FALSE, type = "multiple",
                       Xtitle = NULL, Ytitle = NULL,
                       Xgrid = FALSE, Ygrid = FALSE){
  .Deprecated("ts_plot") 
  ts_plot(ts.obj, line.mode = line.mode, width = width, 
          dash = dash, color = color, 
          slider = slider, type = type,
          Xtitle = Xtitle, Ytitle = Ytitle,
          Xgrid = Xgrid, Ygrid = Ygrid)
}