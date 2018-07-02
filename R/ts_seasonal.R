#'  Seasonality Visualization of Time Series Object
#' @export ts_seasonal
#' @param ts.obj A univariate time series object of a class "ts", "zoo", or "xts" 
#' (support only series with either monthly or quarterly frequency). 
#' Also, this function supports data frame objects as long as there is at least one "Date" and "numeric" objects 
#' (if there are more, by defualt will use the first of each)
#' @param type The type of the seasonal plot - 
#' "normal" to split the series by full cycle units, or
#' "cycle" to split by cycle units (applicable only for monthly and quarterly data), or
#' "box" for box-plot by cycle units, or
#' "all" for all the three plots together
#' @param Ygrid Logic,show the Y axis grid if set to TRUE
#' @param Xgrid Logic,show the X axis grid if set to TRUE
#' @param title Plot title - Character object
#' @param last Subset the data to the last number of observations
#' @description Visualize time series object by it periodicity, currently support only monthly and quarterly frequency
#' @examples
#' data(USgas)
#' ts_seasonal(USgas)
#' 
#' # Seasonal box plot
#' ts_seasonal(USgas, type = "box") 


# The ts_seasonal function ####

ts_seasonal <- function(ts.obj, type = "normal", Ygrid = FALSE, Xgrid = FALSE, title = NULL, last = NULL, daily.box = NULL) {
  
  `%>%` <- magrittr::`%>%`
  df <- df1 <- df_wide <- p <- obj.name <- NULL
  diff_mean <- col_class <- date_col <-  numeric_col <- NULL
  obj.name <- base::deparse(base::substitute(ts.obj))
  # Set the plot title
  if(base::is.null(title)){
  title <- paste("Seasonality Plot -", obj.name, sep = " ")
  } else if(!base::is.character(title)){
    warning("The 'title' object is not character object, using the default option")
    title <- paste("Seasonality Plot -", obj.name, sep = " ")
  }
  
  
  # Error handling
  # Checking the last parameter
  if(!base::is.null(last)){
  if(!base::is.numeric(last) | last <= 0){
    stop("The 'last' parameter is not valid")
  } else {
        if(last != round(last)){
          stop("The 'last' parameter is not integer")
        }
      }
  }
  
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
      freq <- "monthly"
      df$dec_right <- base::factor(df$dec_right,
                                   levels = base::unique(df$dec_right),
                                   labels = base::month.abb[as.numeric(base::unique(df$dec_right))])
    } else if(stats::frequency(ts.obj) == 4){
      freq <- "quarterly"
      df$dec_right <- base::paste("Qr.", df$dec_right, sep = " ")
    } else if(base::round(stats::frequency(ts.obj), 0 ) == 365){
      freq <- "daily"
      first_time <- time(ts.obj)[1]
      
      year <- as.integer(first_time)
      day <- (first_time - year) * 365 
      first_date <- as.Date(day, origin = lubridate::ymd(paste(year, "-01-01", sep = "")))
      
      df <- data.frame(date = seq.Date(from = first_date, length.out = length(ts.obj), by = "days"), value = as.numeric(ts.obj))
      df$month <- lubridate::month(df$date, label = TRUE)
      df$wday <- lubridate::wday(df$date, label = TRUE)
      df$week <- factor(lubridate::week(df$date), ordered = TRUE)
      df$dec_left <- lubridate::year(df$date)
      df$dec_right <- lubridate::yday(df$date)
      
    } else {
      stop("The frequency of the series is invalid, ",
           "the function support only 'daily', 'monthly' or 'quarterly' frequencies")
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
    } else {
      stop("None of the data frame columns is 'Date' object,",
           "please check if the data format is defined properly")
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
      }
        
        
        df1 <- base::data.frame(date = ts.obj[, date_col], value = ts.obj[, numeric_col])
        df1$date_lag <- c(NA, as.Date(df1$date[-nrow(df1)]))
        df1$dif <- df1$date - as.Date(df1$date_lag,  origin= "1970-01-01")
        
        diff_mean <- mean(df1$dif, na.rm = TRUE)
        
        if(diff_mean >= 28 & diff_mean <= 31 ){
          freq <- "monthly"
          df <- data.frame(dec_left = lubridate::year(df1$date), 
                           dec_right = lubridate::month(df1$date), 
                           value = df1$value) 
          df$dec_right <- base::factor(df$dec_right,
                                       levels = base::unique(df$dec_right),
                                       labels = base::month.abb[as.numeric(base::unique(df$dec_right))])
        }else  if(diff_mean >= 89 & diff_mean <= 92 ){
          freq <- "quarterly"
          df <- data.frame(dec_left = lubridate::year(df1$date), 
                           dec_right = paste("Qr.", lubridate::quarter(df1$date), sep = " ") , 
                           value = df1$value) 
          
        } else{
          stop("Couldn't identify the frequency of the data frame, ", 
               "please check if the  frequency of the date object is monthly or quarterly")
        }
      }

  if(!base::is.null(last)){
  df <- df[(base::nrow(df) - last):base::nrow(df),]  
  }
  
  
  seasonal_sub <- function(df, type, Xgrid, Ygrid, freq, title){  
    p <- NULL
    
    if(type == "normal"){
      df_wide <- reshape2::dcast(df, dec_right ~ dec_left)
    } else if(type == "cycle" | type == "box"){
      df_wide <- reshape2::dcast(df, dec_left ~ dec_right)
    }
    if(freq == "monthly"){
      color_ramp <- c(RColorBrewer::brewer.pal(6,"Dark2"), RColorBrewer::brewer.pal(6,"Set2"))
    } else if(freq == "quarterly"){
      color_ramp <- RColorBrewer::brewer.pal(4,"Dark2")
    }
    if(type == "normal"){
      color_ramp <- colormap::colormap_pal()(ncol(df_wide))
    }
    
    p <- plotly::plot_ly()
    if(type == "box"){
      for (f in 2:ncol(df_wide)) {
        
        p <- p %>% plotly::add_trace(y = df_wide[, f], 
                                     type = "box", 
                                     name = colnames(df_wide)[f],
                                     boxpoints = "all", jitter = 0.3,
                                     pointpos = -1.8,
                                     marker = list(color = color_ramp[(f -1)]),
                                     line = list(color = color_ramp[(f -1)])
        )
      }
    } else if(type == "cycle"){
      for (f in 2:ncol(df_wide)) {
        p <- p %>% plotly::add_trace(x = df_wide[, 1], y = df_wide[, f], 
                                     name = names(df_wide)[f], 
                                     mode = "lines", 
                                     type = "scatter",
                                     line = list(color = color_ramp[(f -1)]))
      }
    } else if(type == "normal"){
      for (f in 2:ncol(df_wide)) {
        p <- p %>% plotly::add_trace(x = df_wide[, 1], y = df_wide[, f], 
                                     name = names(df_wide)[f], 
                                     mode = "lines", 
                                     type = "scatter",
                                     line = list(color = color_ramp[(f -1)]))
        
        
        
                                     
      }
    }
    p <- p %>% plotly::layout(title = title, 
                              xaxis = list(title = "", autotick = F, 
                                           showgrid = Xgrid, 
                                           dtick = 1), 
                              yaxis = list(title = obj.name, showgrid = Ygrid))
    
    
    
    return(p)
  }
  
  if(type != "all"){
    p <- seasonal_sub(df = df, type = type, Xgrid = Xgrid, Ygrid = Ygrid, freq = freq, title = title)
    if(freq == "daily"){
      p <- p %>% plotly::layout(xaxis = list(autotick = FALSE, dtick = 25, title = "Day of the Year"))
    }
  } else {
    n <- c <- b <- NULL
    n <- seasonal_sub(df = df, type = "normal", Xgrid = Xgrid, Ygrid = Ygrid, freq = freq, title = title) %>% 
      plotly::layout(yaxis = list(title = "By Year"))
    if(freq == "daily"){
      n <- n %>% plotly::layout(xaxis = list(autotick = FALSE, dtick = 25, title = "Day of the Year"))
    }
    c <- seasonal_sub(df = df, type = "cycle", Xgrid = Xgrid, Ygrid = Ygrid, freq = freq, title = title) %>% 
      plotly::layout(yaxis = list(title = "By Month"))
    b <- seasonal_sub(df = df, type = "box", Xgrid = Xgrid, Ygrid = Ygrid, freq = freq, title = title) %>% 
      plotly::layout(yaxis = list(title = "By Month"))
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
#' @param ts.obj A univariate time series object of a class "ts", "zoo", "xts", and the data frame family (data.frame, data.table, tbl, tibble, etc.) with a 
#' Date column and at least one numeric column. This function support time series objects with a daily, weekly, monthly and quarterly frequencies 
#' @param last An integer, set the last number of observations to present
#' @param frequency An integer, relevant only if using daily data as input, ignore otherwise. 
#' Provides the ability to transform daily data to weekday (if set to 7) 
#' or day of the year (if set to 365) structure.If not set or NULL will use by default the weekday option.
#' @description Heatmap plot for time series object by it periodicity (currently support only daily, weekly, monthly and quarterly frequencies)
#' @examples
#' data(USgas)
#' ts_heatmap(USgas)
#' 
#' # Show only the last 4 years
#' ts_heatmap(USgas, last = 4 *12)   

# --- The ts_heatmap function ---

ts_heatmap <- function(ts.obj, last = NULL, frequency = NULL) {
  
  `%>%` <- magrittr::`%>%`
  df <-  p <- obj.name <-  NULL
  
  # Checking the last parameter
  if(!base::is.null(last)){
    if(!base::is.numeric(last) | last <= 0){
      stop("The 'last' parameter is not valid")
    } else {
      if(last != round(last)){
        stop("The 'last' parameter is not integer")
      }
    }
  }
  
  obj.name <- base::deparse(base::substitute(ts.obj))
  df <- TSstudio::ts_reshape(ts.obj, type = "wide", frequency = frequency)
  
  
  if(!base::is.null(last)){
    
    df <- df[, c(1, (base::ncol(df) - base::ceiling(last / stats::frequency(ts.obj))):ncol(df))]  
  }
  
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


#' Moving Average Time Series Data
#' @export
#' @param ts.obj a univariate time series object of a class "ts", "zoo" or "xts" (support only series with either monthly or quarterly frequency)
#' @param k A single or multiple integers (by default using 3, 6 and 9), 
#' the k argument set a symatric moving average
#' set the amount of past and future periods to be use to calculating the moving average 
#' @param k_left A single integer (optional argument, default set to NULL), can be used, 
#' along with the k_right argument, an unbalanced moving average. 
#' The k_left defines the number of lags to includes in the moving average.
#' @param k_right A single integer (optional argument, default set to NULL), can be used, 
#' along with the k_left argument, to set an unbalanced moving average. 
#' The k_right defines the number of negative lags to includes in the moving average.
#' @param double A single integer, an optional argument. If not NULL (by default), will apply a second moving average process on the initial moving average output
#' @param plot A boolean, if TRUE will plot the results
#' @param multiple A boolean, if TRUE (and k > 1) will create multiple plots, one for each moving average degree. By default is set to FALSE
#' @param title A character, if not NULL (by default), will use the input as the plot title
#' @param Xtitle A character, if not NULL (by default), will use the input as the plot x - axis title
#' @param Ytitle A character, if not NULL (by default), will use the input as the plot y - axis title
#' @description Calculate the moving average (and double moving average) for time series data
#' @examples
#' 
#' # Using moving average to smooth the USVsales dataset (US Monthly Total Vehicle Sales)
#' # Applying 4 degrees of moving average   
#' USVS_MA <- ts_ma(ts.obj = USVSales, k = c(3, 6, 9, 12))
#' 
#' # Display each moving average (by degree) on a separate plot and adding a title for the plot
#' USVS_MA <- ts_ma(ts.obj = USVSales, k = c(3, 6, 9, 12), 
#'                  multiple = TRUE, 
#'                  title = "Moving Average for the US Vehical Sales Series")
#' 
#' # Plot only the moving average with k = 12
#' ts_plot(USVS_MA$ma_12, title = "Moving Average, k = 12")
#' 
#' # Using moving average to smooth the USgas dataset (US Monthly Natural Gas Consuption)
#' # Adding double moving average with degree of 6 to smooth the strong seasonality
#' USgas_MA <- ts_ma(ts.obj = USgas, k = c(6, 9), double = 6)
#' 
#' # Extract the plot
#' USgas_MA$plot

ts_ma <- function(ts.obj, 
                  k = c(3, 6, 9), 
                  k_left = NULL,
                  k_right = NULL,
                  double = NULL, 
                  plot = TRUE, multiple = FALSE, 
                  title = NULL, Xtitle = NULL, Ytitle = NULL){
  
  `%>%` <- magrittr::`%>%`
  
  obj.name <- ts_merged <- ts_obj <- ts_temp <- ts_ma <- c <- p <- NULL
  obj.name <- base::deparse(base::substitute(ts.obj))
  
  # Error Handling  
  if (stats::is.ts(ts.obj)) {
    if (stats::is.mts(ts.obj)) {
      warning("The 'ts.obj' has multiple columns, only the first column will be plot")
      ts.obj <- ts.obj[, 1]
    }
  }
    
  if(!base::is.logical(plot)){
    warning("The value of the 'plot' argument is not valid (can apply either TRUE or FALSE) and will be ignore")
    plot <- TRUE
  }
  
  if(!base::is.logical(multiple)){
    warning("The value of the 'multiple' argument is not valid (can apply either TRUE or FALSE) and will be ignore")
    multiple <- FALSE
  } else if(base::length(k) == 1 & multiple){
    warning("The 'multiple' aregument cannot be used when k == 1")
    multiple <- FALSE
  } else if(base::length(k) > 1 & multiple){
    p_m <- list()
  }
  
  if(!base::is.null(title)){
    if(!base::is.character(title)){
      warning("The value of the 'title' is not valid (only character can be used as an input), and will be ignore")
      title <- NULL
    }
  } else {
    title <- paste(obj.name, "- Moving Average", sep = " ")
  }
  
  if(!base::is.null(Xtitle)){
    if(!base::is.character(Xtitle)){
      warning("The value of the 'Xtitle' is not valid (only character can be used as an input), and will be ignore")
      Xtitle <- NULL
    }
  }
  
  if(!base::is.null(Ytitle)){
    if(!base::is.character(Ytitle)){
      warning("The value of the 'Ytitle' is not valid (only character can be used as an input), and will be ignore")
      Ytitle <- NULL
    }
  }
  if(!base::is.null(double)){
    if(!base::is.numeric(double)){
      warning("The 'double' parameter is not a numeric number and will be ignore")
      double <- NULL
    } else if(!base::all(double %% 1 == 0)){
      warning("The 'double' parameter is not an integer number and will be ignore")
      double <- NULL
    }
  } else if(base::length(double) > 1){
    warning("The 'double' parameter is restricted to single value (integer), only the first one will be used")
    double <- dobule[1]
  }
  
  
  if(!base::is.numeric(k)){
    stop("The 'k' argument is not valid, please make sure that you are using only integers as input")
  } else if(!base::all(k %% 1 == 0)){
    stop("The 'k' argument is not valid, please make sure that you are using only integers as input")
  } else if(base::length(k) > 8){
    warning("The 'k' parameter is restricted up to 8 inputs (integers), only the first 8 values will be used")
    k <- k[1:8]
  }
  
  if(!base::is.null(k_left)){
    if(!base::is.numeric(k_left)){
      stop("The 'k_left' argument is not valid, please make sure that you are using only integers as input")
    } else if(base::length(k_left) != 1){
      warning("The 'k_left' argument has too many inputs, can hanlde only single integer. Will use only the first input")
      k_left <- k_left[1]
    } else if(k_left %% 1 != 0){
      stop("The 'k_left' argument is not an integer type")
    }
  }
  
  if(!base::is.null(k_right)){
    if(!base::is.numeric(k_right)){
      stop("The 'k_right' argument is not valid, please make sure that you are using only integers as input")
    } else if(!base::length(k_right) != 1){
      warning("The 'k_right' argument has too many inputs, can hanlde only single integer. Will use only the first input")
      k_right <- k_right[1]
    } else if(k_right %% 1 != 0){
      stop("The 'k_right' argument is not an integer type")
    }
  }
  
  if(base::max(k) * 2 + 1 > base::length(ts.obj)){
    stop("The length of the series is too short to apply the moving average with the given 'k' parameter")
  }
  
  # Setting function to calculate moving average
  ma_fun <- function(ts.obj, k){
    ts_obj <- ts_temp <- NULL
    ts_obj <- ts_temp <- ts.obj
    
    for(i in 1:k){
      ts_obj <- stats::ts.intersect(stats::lag(ts_temp, k = i), ts_obj, stats::lag(ts_temp, k = -i))
    }
    
    ts_ma <- NULL
    ts_ma <- TSstudio::ts_sum(mts.obj = ts_obj) / (2 * k + 1)
    return(ts_ma)
  }
  
  
  ts_merged <- ts.obj
  color_ramp <- RColorBrewer::brewer.pal(8,"Dark2")
  color_ramp_double <- RColorBrewer::brewer.pal(8,"Set1")
  output <- list()
  output$ts.obj <- ts.obj
  legend_flag <- base::ifelse(multiple, FALSE, TRUE)
  p <- plotly::plot_ly(x = stats::time(ts.obj), 
                       y = base::as.numeric(ts.obj), 
                       name = obj.name, 
                       type = "scatter", 
                       mode = "lines", 
                       line = list(color = "#00526d"),
                       showlegend = legend_flag)
  c <- 1
  
  for(i in k){
    ts_ma1 <- NULL
    ts_ma1 <- ma_fun(ts.obj = ts.obj, k = i)
    base::eval(base::parse(text = base::paste("output$ma_", i, " <- ts_ma1", sep = "")))
    if(!multiple){
    p <- p %>% plotly::add_lines(x = stats::time(ts_ma1), y = base::as.numeric(ts_ma1), 
                                 name = base::paste("MA - ", i, sep = " "), 
                                 line = list(dash = "dash", color = color_ramp[c], width = 4)) 
    } else if(multiple){
      if(base::is.null(double)){
        annotations_single <- list(
          text = base::paste("Moving Average - ", i, sep = " "),
          xref = "paper",
          yref = "paper",
          yanchor = "bottom",
          xanchor = "center",
          align = "center",
          x = 0.5,
          y = 0,
          showarrow = FALSE
        )
      } else {
        annotations_single <- NULL
      }

      p_m[[c]] <- p %>% plotly::add_lines(x = stats::time(ts_ma1), y = base::as.numeric(ts_ma1), 
                                          name = base::paste("MA -", i, sep = " "), 
                                          line = list(dash = "dash", color = color_ramp[c], 
                                                      width = 4),
                                          showlegend = TRUE)  %>% 
        plotly::layout(annotations = annotations_single)
    }
    if(!base::is.null(double)){
      ts_ma_d <- NULL
      ts_ma_d <- ma_fun(ts.obj = ts_ma1, k = double)
      base::eval(base::parse(text = base::paste("output$double_ma_", i, "_", double, " <- ts_ma_d", sep = "")))
      if(!multiple){
      p <- p %>% plotly::add_lines(x = stats::time(ts_ma_d), y = base::as.numeric(ts_ma_d),
                                   name = base::paste("Double MA - ", i, "/", double, sep = " "),
                                   line = list(dash = "dot", color = color_ramp_double[c], width = 4))
      } else if(multiple){
        annotations_double <- list(
          text = base::paste("Double Moving Average - ", i, "/", double, sep = " "),
          xref = "paper",
          yref = "paper",
          yanchor = "bottom",
          xanchor = "center",
          align = "center",
          x = 0.5,
          y = 0,
          showarrow = FALSE
        )

        p_m[[c]] <- p_m[[c]] %>% plotly::add_lines(x = stats::time(ts_ma_d), y = base::as.numeric(ts_ma_d),
                                            name = base::paste("Double MA - ", i, "/", double, sep = " "),
                                            line = list(dash = "dot", 
                                            color = color_ramp_double[c], width = 4),
                                            showlegend = TRUE) %>% 
          plotly::layout(annotations = annotations_double)
      }
    }
    c <- c + 1
  }
  
  if(!multiple){
  p <- p %>% plotly::layout(title = title, xaxis = list(title = Xtitle), yaxis = list(title = Ytitle),  showlegend = TRUE) 
  output$plot <- p 
  } else if(multiple){
    output$plot <- plotly::subplot(p_m, nrows = base::length(p_m), 
                                   shareX = TRUE, titleX = TRUE, titleY = TRUE) %>%
      plotly::layout(title = title)
  }
  
  
  
  if(plot){
    print(output$plot)
  }
  
  output$parameters <- list(k = k, double = double)
  class(output) <- "ts_ma"
  return(output)
}

