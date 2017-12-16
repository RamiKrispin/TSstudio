ts_ly <- function(x, line.mode = "lines", width = 1, 
                      dash = NULL, color = "blue", 
                      slider = FALSE, type = "multiple"){
  `%>%` <- magrittr::`%>%`
  df <- p <- plot_list <- dim_flag <- plot_list <- obj.name <- NULL 
  
  # Error handling
  if(line.mode != "lines" & 
     line.mode != "lines+markers" & 
     line.mode != "markers"){
    warning("The value of 'line.mode' is not valide, using the default option - 'lines'")
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
    warning("The value of 'type' is not valide, using the default option - 'multiple'")
    type <- "multiple"
  }
  # Check if it is a multiple time series  
  if(!is.null(base::dim(x))){
    if(base::dim(x)[2] > 1){
      dim_flag <- TRUE
      if(is.mts(x)){
        obj.name <- base::deparse(base::substitute(x))
        df <- data.frame(date = stats::time(x), as.data.frame(x))
      } else if(xts::is.xts(x) | zoo::is.zoo(x)){
        obj.name <- deparse(substitute(x))
        df <- data.frame(date = zoo::index(x), as.data.frame(x))
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
        xaxis = list(title = "Date"),
        yaxis = list(title = obj.name)
        
      )
      if(!is.null(p) & slider){
        p <- p %>% 
          plotly::layout(
            xaxis = list(
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
            xaxis = list(title = "Date"),
            yaxis = list(title = names(df)[i])
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
    if(zoo::is.zoo(x) | xts::is.xts(x)){
      df <- data.frame(date = zoo::index(x), y = as.numeric(x))
    } else if (stats::is.ts(x)){
      df <- data.frame(date = stats::time(x), y = as.numeric(x))
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
          xaxis = list(
            rangeslider = list(type = "date"))
        )
    }
  }
  if(is.null(p)){
    stop("Could not create the plot, please check the input")
  } else{
    return(p)
  }
}
