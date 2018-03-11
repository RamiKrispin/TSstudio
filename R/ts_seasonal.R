#'  Seasonality Visualization of Time Series Object
#' @export ts_seasonal
#' @param ts.obj a univariate time series object of a class "ts", "zoo", or "xts" 
#' #' (support only series with either monthly or quarterly frequency). Also, this function support data frame objects with at least one "Date" and "numeric" objects (if there are more, by defualt will use the first of each)
#' @param type The type of the seasonal plot - 
#' "normal" to split the series by full cycle units, or
#' "cycle" to split by cycle units, or
#' "box" for box-plot by cycle units, or
#' "all" for all the three plots together
#' @param Ygrid logic,show the Y axis grid if set to TRUE
#' @param Xgrid logic,show the X axis grid if set to TRUE
#' @description Visualize time series object by it periodicity, currently support only monthly and quarterly frequency
#' @examples
#' data(USgas)
#' ts_seasonal(USgas)
#' 
#' # Seasonal box plot
#' ts_seasonal(USgas, type = "box") 




ts_seasonal <- function(ts.obj, type = "normal", Ygrid = FALSE, Xgrid = FALSE) {
  
  `%>%` <- magrittr::`%>%`
  df <- df1 <- df_wide <- p <- obj.name <- NULL
  diff_mean <- col_class <- date_col <-  numeric_col <- NULL
  obj.name <- base::deparse(base::substitute(ts.obj))
  # Error handling
  if(type != "normal" & type != "cycle" & 
     type != "box" & type != "all" ){
    type <- "normal"
    warning("The 'type' parameter is invalide,", 
            "using the default option - 'normal'")
  }
  
  # Case the input is a time series object
  if (stats::is.ts(ts.obj)) {
    if (stats::is.mts(ts.obj)) {
      warning("The 'ts.obj' has multiple columns, only the first column will be plot")
      ts.obj <- ts.obj[, 1]
    }
    df <- base::data.frame(dec_left = floor(stats::time(ts.obj)), 
                     dec_right = stats::cycle(ts.obj), value = base::as.numeric(ts.obj))
    if(stats::frequency(ts.obj) == 12){
      df$dec_right <- base::factor(df$dec_right,
                             levels = base::unique(df$dec_right),
                             labels = base::month.abb[as.numeric(base::unique(df$dec_right))])
    } else if(stats::frequency(ts.obj) == 4){
      df$dec_right <- base::paste("Qr.", df$dec_right, sep = " ")
    } else {
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
                       dec_right = lubridate::month(ts.obj), value = as.numeric(ts.obj))
      df$dec_right <- base::factor(df$dec_right,
                                   levels = base::unique(df$dec_right),
                                   labels = base::month.abb[as.numeric(base::unique(df$dec_right))])
    # } else if (freq == "weekly") {
    #   df <- data.frame(dec_left = lubridate::year(ts.obj), 
    #                    dec_right = lubridate::week(ts.obj), value = as.numeric(ts.obj))
    # } else if (freq == "daily") {
    #   df <- data.frame(dec_left = lubridate::month(ts.obj), 
    #                    dec_right = lubridate::day(ts.obj), value = as.numeric(ts.obj))
    } else if (freq != "quarterly" & freq != "monthly") {
      stop("The frequency of the series is invalid,",
           "the function support only 'monthly' or 'quarterly' frequencies")
    }
    
  } else if(base::is.data.frame(ts.obj)){
    col_class <- base::lapply(ts.obj, class)
    if("Date" %in%  col_class){
      date_col <- base::which(col_class == "Date")
      if(length(date_col) >1){
        warning("There are multipe 'date' objects in the data frame,",
                "using the first 'date' object in the data frame as the plot index")
        date_col <- date_col[1]
      }
      numeric_col <- base::which(col_class == "numeric" | col_class == "integer")
      if(base::length(numeric_col) == 0){
        stop("None of the data frame columns is numeric,",
             "please check if the data format is defined properly")
      } else {
        if(length(numeric_col) > 1){
          warning("There are more than one columns with numeric values,",
                  "only the first numeric column will be plot")
          numeric_col <- numeric_col[1]
        }
        
        
        df1 <- base::data.frame(date = ts.obj[, date_col], value = ts.obj[, numeric_col])
        df1$date_lag <- c(df1$date[-1], NA)
        df1$dif <- df1$date - df1$date_lag 
         
        diff_mean <- mean(df1$dif, na.rm = TRUE)
        
        if(diff_mean >= 28 & diff_mean <= 31 ){
          
          df <- data.frame(dec_left = lubridate::year(df1$date), 
                           dec_right = lubridate::month(df1$date), 
                           value = df1$value) 
          df$dec_right <- base::factor(df$dec_right,
                                       levels = base::unique(df$dec_right),
                                       labels = base::month.abb[as.numeric(base::unique(df$dec_right))])
        }else  if(diff_mean >= 89 & diff_mean <= 92 ){
          
          df <- data.frame(dec_left = lubridate::year(df1$date), 
                           dec_right = paste("Qr.", lubridate::quarter(df1$date), sep = " ") , 
                           value = df1$value) 
         
        }
      }
    }
    
  } 
seasonal_sub <- function(df, type, Xgrid, Ygrid){  
  p <- NULL
  if(type == "normal"){
    df_wide <- reshape2::dcast(df, dec_right ~ dec_left)
  } else if(type == "cycle" | type == "box"){
    df_wide <- reshape2::dcast(df, dec_left ~ dec_right)
  }
  
  p <- plotly::plot_ly()
  if(type == "box"){
    for (f in 2:ncol(df_wide)) {
    p <- p %>% plotly::add_trace(y = df_wide[, f], 
                                 type = "box", 
                                 name = colnames(df_wide)[f],
                                 boxpoints = "all", jitter = 0.3,
                                 pointpos = -1.8
                                 )
      }
  } else{
  for (f in 2:ncol(df_wide)) {
    p <- p %>% plotly::add_trace(x = df_wide[, 1], y = df_wide[, f], 
                                 name = names(df_wide)[f], 
                                 mode = "lines", 
                                 type = "scatter")
  }
  }
  p <- p %>% plotly::layout(title = paste("Seasonality Plot -", obj.name, 
                                          sep = " "), 
                            xaxis = list(title = "", autotick = F, 
                                         showgrid = Xgrid, 
                                         dtick = 1), 
                            yaxis = list(title = obj.name, showgrid = Ygrid))
  return(p)
}

if(type != "all"){
p <- seasonal_sub(df = df, type = type, Xgrid = Xgrid, Ygrid = Ygrid)
} else {
  n <- c <- b <- NULL
  n <- seasonal_sub(df = df, type = "normal", Xgrid = Xgrid, Ygrid = Ygrid) %>% plotly::layout(yaxis = list(title = "By Year"))
  c <- seasonal_sub(df = df, type = "cycle", Xgrid = Xgrid, Ygrid = Ygrid) %>% plotly::layout(yaxis = list(title = "By Month"))
  b <- seasonal_sub(df = df, type = "box", Xgrid = Xgrid, Ygrid = Ygrid) %>% plotly::layout(yaxis = list(title = "By Month"))
  p <- plotly::subplot(n,c,b, nrows = 3, titleY = T) %>% plotly::hide_legend()
}
  return(p)
}

#'  Polor Plot for Time Series Object
#' @export
#' @param ts.obj A univariate time series object of a class "ts", "zoo" or "xts" (support only series with either monthly or quarterly frequency)
#' @param title Add a title for the plot, default set to NULL
#' @param width The widht of the plot in pixels, default set to 600
#' @param height The height of the plot pixels, default set to 600
#' @param left Set the left margin of the plot in pixels, default set to 25 
#' @param right Set the right margin of the plot in pixels, default set to 25
#' @param top Set the top margin of the plot in pixels, default set to 25
#' @param bottom Set the bottom margin of the plot in pixels, default set to 25
#' @description Polor plot for time series object (ts, zoo, xts), currently support only monthly and quarterly frequency
#' @examples
#' data(USgas)
#' ts_polar(USgas)

ts_polar <- function(ts.obj, title = NULL, width = 600, height = 600, 
                     left = 25, right = 25, top = 25, bottom = 25) {
  
  `%>%` <- magrittr::`%>%`
  df <- df_wide <- p <- obj.name <-  NULL
  
  obj.name <- base::deparse(base::substitute(ts.obj))
  # Error handling
  if(is.null(title)){
    title <- paste("Polar Plot -", obj.name)
  } else if(!is.character(title)){
    title <- paste("Polar Plot -", obj.name)
    warning("The 'title' value is not valid, using the default title")
  }
  
  if (stats::is.ts(ts.obj)) {
    if (stats::is.mts(ts.obj)) {
      warning("The 'ts.obj' has multiple columns, only the first column will be plot")
      ts.obj <- ts.obj[, 1]
    }
    df <- base::data.frame(dec_left = floor(stats::time(ts.obj)), 
                           dec_right = stats::cycle(ts.obj), value = base::as.numeric(ts.obj))
    if(stats::frequency(ts.obj) == 12){
      df$dec_right <- base::factor(df$dec_right,
                                   levels = base::unique(df$dec_right),
                                   labels = base::month.abb[as.numeric(base::unique(df$dec_right))])
    } else if(stats::frequency(ts.obj) == 4){
      df$dec_right <- base::paste("Qr.", df$dec_right, sep = " ")
    } else {
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
                             dec_right = lubridate::month(ts.obj), value = as.numeric(ts.obj))
      df$dec_right <- base::factor(df$dec_right,
                                   levels = base::unique(df$dec_right),
                                   labels = base::month.abb[as.numeric(base::unique(df$dec_right))])
      # } else if (freq == "weekly") {
      #   df <- data.frame(dec_left = lubridate::year(ts.obj), 
      #                    dec_right = lubridate::week(ts.obj), value = as.numeric(ts.obj))
      # } else if (freq == "daily") {
      #   df <- data.frame(dec_left = lubridate::month(ts.obj), 
      #                    dec_right = lubridate::day(ts.obj), value = as.numeric(ts.obj))
    } else if (freq != "quarterly" & freq != "monthly") {
      stop("The frequency of the series is invalid,",
           "the function support only 'monthly' or 'quarterly' frequencies")
    }
    
  }
  
  p <- plotly::plot_ly(r = df$value, t = df$dec_right, 
                       width = width, height = height) %>% 
    plotly::add_area(color = factor(df$dec_left, ordered = TRUE)) %>%
    plotly::layout(orientation = -90, 
                   autosize = T,
                   title = title,
                   margin = list(
                     l = left,
                     r = right,
                     b = bottom,
                     t = top,
                     pad = 4
                   ))
  
  return(p)
}


#'  Heatmap Plot for Time Series
#' @export
#' @param ts.obj a univariate time series object of a class "ts", "zoo" or "xts" (support only series with either monthly or quarterly frequency)
#' @description Heatmap plot for time series object by it periodicity (currently support only monthly and quarterly frequency)
#' @examples
#' data(USgas)
#' ts_heatmap(USgas) 

ts_heatmap <- function(ts.obj) {
  
  `%>%` <- magrittr::`%>%`
  df <-  p <- obj.name <-  NULL
  
  obj.name <- base::deparse(base::substitute(ts.obj))
  df <- TSstudio::ts_reshape(ts.obj, type = "wide")
  z <- base::as.matrix(df[, -1])
  z_text <- base::matrix(NA, nrow = nrow(z), ncol = ncol(z))
  time_unit <- base::trimws(base::names(df)[1])
  time_unit_up <- base::paste(base::toupper(base::substr(time_unit, 1, 1)), 
                              base::substr(time_unit,2, base::nchar(time_unit)), sep = "")
  for(c in 1:base::ncol(z_text)){
    for(r in 1:base::nrow(z_text)){
      z_text[r, c] <- base::paste('Value: ', z[r,c],
                                  '<br> Year : ', base::colnames(z)[c],
                                  '<br>' ,time_unit_up, ' :', r, sep = " ")
    }
  }
  p <- plotly::plot_ly(z = z, x = colnames(df[,-1]), y = df[,1], type = "heatmap",
                       hoverinfo = 'text',
                       text = z_text
  ) %>% plotly::layout(
    title = base::paste("Heatmap -", obj.name, sep = " "),
    xaxis = list(title = "Year"),
    yaxis = list(title = time_unit_up)
  )
  
  
  return(p)
}



#' 3D Surface Plot for Time Series
#' @export
#' @param ts.obj a univariate time series object of a class "ts", "zoo" or "xts" (support only series with either monthly or quarterly frequency)
#' @description 3D surface plot for time series object by it periodicity (currently support only monthly and quarterly frequency)
#' @examples
#' ts_surface(USgas) 

ts_surface <- function(ts.obj) {
  
  `%>%` <- magrittr::`%>%`
  df <-  p <- obj.name <-  NULL
  
  obj.name <- base::deparse(base::substitute(ts.obj))
  df <- TSstudio::ts_reshape(ts.obj, type = "wide")
  z <- base::as.matrix(df[, -1])
  z_text <- base::matrix(NA, nrow = nrow(z), ncol = ncol(z))
  time_unit <- base::trimws(base::names(df)[1])
  time_unit_up <- base::paste(base::toupper(base::substr(time_unit, 1, 1)), 
                              base::substr(time_unit,2, base::nchar(time_unit)), sep = "")
  for(c in 1:base::ncol(z_text)){
    for(r in 1:base::nrow(z_text)){
      z_text[r, c] <- base::paste('Value: ', z[r,c],
                                  '<br> Year : ', base::colnames(z)[c],
                                  '<br>' ,time_unit_up, ' :', r, sep = " ")
    }
  }
  p <- plotly::plot_ly(z = z, x = colnames(df[,-1]), y = df[,1],
                       hoverinfo = 'text',
                       text = z_text
  ) %>%
    plotly::add_surface() %>% plotly::layout(
      title = base::paste("Surface Plot -", obj.name, sep = " "),
      scene = list(xaxis = list(title = "Years"),
                   yaxis= list(title = time_unit_up),
                   zaxis= list(title = "Value")
                   )
    )
  
  
  return(p)
}

