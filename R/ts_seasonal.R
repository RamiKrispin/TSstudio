#'  Seasonality Visualization of Time Series Object
#'  Seasonality Visualization of Time Series Object
#' @export ts_seasonal
#' @param ts.obj Input object, either a univariate time series object of a class "ts", "zoo", "xts", or a data frame object of a class
#' "data.frame", "tbl", "data.table" as long as there is at least one "Date"/"POSIXt" and a "numeric" objects 
#' (if there are more then one, by defualt will use the first of each). 
#' Currently support only daily, weekly, monthly, and quarterly frequencies 
#' @param type The type of the seasonal plot - 
#' "normal" to split the series by full cycle units, or
#' "cycle" to split by cycle units (applicable only for monthly and quarterly data), or
#' "box" for box-plot by cycle units, or
#' "all" for all the three plots together
#' @param title Plot title - Character object
#' @param Ygrid Logic,show the Y axis grid if set to TRUE (default)
#' @param Xgrid Logic,show the X axis grid if set to TRUE (defualt)
#' @param last Subset the data to the last number of observations
#' @param palette A character, the color palette to be used when the "cycle" or "box" plot are being selected 
#' (by setting the type to "cycle", "box", or "all"). 
#' All the palettes in the RColorBrewer and viridis packages are available to be use, the
#' default option is "Set1" from the RColorBrewer package
#' @param palette_normal A character, the color palette to be used when the "normal" plot is being selected 
#' (by setting the type to "normal" or "all"). 
#' All the palettes in the RColorBrewer and viridis packages are available to be used, the
#' default palette is "Spectral" from the RColorBrewer package
#' @description Visualize time series object by it periodicity, currently support only monthly and quarterly frequency
#' @examples
#' data(USgas)
#' ts_seasonal(USgas)
#' 
#' # Seasonal box plot
#' ts_seasonal(USgas, type = "box") 
#' 
#' # Plot all the types 
#' ts_seasonal(USgas, type = "all")


# The ts_seasonal function ####

ts_seasonal <- function(ts.obj, 
                        type = "normal", 
                        title = NULL,
                        Ygrid = TRUE, 
                        Xgrid = TRUE, 
                        last = NULL,
                        palette = "Set1",
                        palette_normal = "Spectral") {
  
  `%>%` <- magrittr::`%>%`
  df <- freq <- obj.name <- brewer_palettes <- viridis_palettes <- palette_type <- NULL
  n_colors <- NULL
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
      if(last != base::round(last)){
        stop("The 'last' parameter is not integer")
      }
    }
  }
  
  # Checking the type parameter
  if(type != "normal" && type != "cycle" && 
     type != "box" && type != "all" ){
    type <- "normal"
    warning("The 'type' parameter is invalide,", 
            "using the default option - 'normal'")
  }
  
  # Checking the Ygrid and Xgrid parameters
  if(!base::is.logical(Ygrid)){
    Ygrid <- TRUE
    warning("The 'Ygrid' argument is not a boolean operator, setting it to TRUE")
  }
  
  if(!base::is.logical(Xgrid)){
    Xgrid <- TRUE
    warning("The 'Xgrid' argument is not a boolean operator, setting it to TRUE")
  }
  
  # Stage 1 transforming the time series object to data frame
  # Input ts object
  if(stats::is.ts(ts.obj)){
    if(stats::is.mts(ts.obj)){
      ts.obj <- ts.obj[,1]
      warning("The input object is a 'mts' class, by defualt will use only the first series as an input")
    }
    
    freq <- stats::frequency(ts.obj)
    
    if(base::length(ts.obj) < freq){
      stop("The length of the series is smaller than the length of full cycle")
    }
    
    start_main <- stats::start(ts.obj)[1]
    start_minor <- stats::start(ts.obj)[2]
    
    if(freq %in% c(7, 52, 365, 12, 4)){
      minor1 <- base::seq(from = start_minor, to = freq, by = 1)
      minor2 <- base::rep(x = 1:freq, length.out = base::length(ts.obj) - base::length(minor1))
      main1 <- base::rep(x = start_main, length.out = base::length(minor1))
      main2 <- base::rep(x = (start_main + 1):stats::end(ts.obj)[1], 
                         each = freq, 
                         len = base::length(ts.obj) - base::length(minor1))
      df <- data.frame(main = c(main1, main2), 
                       minor = c(minor1, minor2), 
                       y = base::as.numeric(ts.obj))
      
      if(freq == 12){
        df$minor <-  base::factor(base::month.abb[df$minor], levels = month.abb)
      }
    } 
    # Input xts or zoo objects
  } else if(xts::is.xts(ts.obj) | zoo::is.zoo(ts.obj)){
    if(!base::is.null(base::ncol(ts.obj))){
      ts.obj <- ts.obj[,1]
      warning("The input object is a multiple time series object, by defualt will use only the first series as an input")
    }
    if(lubridate::is.Date(zoo::index(ts.obj))){
      if(xts::periodicity(ts.obj)$scale == "daily"){
        df <- data.frame(main = lubridate::year(zoo::index(ts.obj)), 
                         minor = lubridate::yday(zoo::index(ts.obj)), 
                         y = base::as.numeric(ts.obj[,1]))
      } else if(xts::periodicity(ts.obj)$scale == "weekly"){
        df <- data.frame(main = lubridate::year(zoo::index(ts.obj)), 
                         minor = lubridate::week(zoo::index(ts.obj)), 
                         y = base::as.numeric(ts.obj[,1]))
      } else if(xts::periodicity(ts.obj)$scale == "monthly"){
        df <- data.frame(main = lubridate::year(zoo::index(ts.obj)), 
                         minor = lubridate::month(zoo::index(ts.obj), label = TRUE), 
                         y = base::as.numeric(ts.obj[,1]))
      } else if(xts::periodicity(ts.obj)$scale == "quarterly"){
        df <- data.frame(main = lubridate::year(zoo::index(ts.obj)), 
                         minor = lubridate::quarter(zoo::index(ts.obj)), 
                         y = base::as.numeric(ts.obj[,1]))
      } 
    }
    # Input data.frame or tbl or data.table objects
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
    df_temp <- NULL
    if(length(numeric_col) == 1){
      df_temp <- base::data.frame(date = ts.obj[, date_col], y =  ts.obj[, numeric_col])
    } else {
      warning("The input object is a multiple time series object, by defualt will use only the first series as an input")
      df_temp <- base::data.frame(date = ts.obj[, date_col], ts.obj[, numeric_col[1]])
    }
    
    data_diff <- NULL
    date_diff <- base::diff(as.numeric(df_temp$date))
    
    if(base::min(date_diff) == base::max(date_diff) & base::mean(date_diff) == 1){
      # Daily
      df <- data.frame(main = lubridate::year(df_temp$date),
                       minor = lubridate::yday(df_temp$date),
                       y = df_temp$y)
    } else if(base::min(date_diff) == base::max(date_diff) & base::mean(date_diff) == 7){
      # Weekly
      df <- data.frame(main = lubridate::year(df_temp$date),
                       minor = lubridate::week(df_temp$date),
                       y = df_temp$y)
    } else if(base::min(date_diff) >= 28 &  base::max(date_diff) <= 31 & 
              base::mean(date_diff) < 31 & base::mean(date_diff) > 28){
      # Monthly
      df <- data.frame(main = lubridate::year(df_temp$date),
                       minor = lubridate::month(df_temp$date, label = TRUE),
                       y = df_temp$y)
    } else if(base::min(date_diff) >= 90 &  base::max(date_diff) <= 92 & 
              base::mean(date_diff) < 92 & base::mean(date_diff) > 90){
      # Quarterly
      df <- data.frame(main = lubridate::year(df_temp$date),
                       minor = lubridate::quarter(df_temp$date),
                       y = df_temp$y)
      
    } else{
      stop("The frequency of the input dataset is not valid, must be on of the following - daily, weekly, monthly or quarterly")
    }
  }
  
  if(!base::is.null(last)){
    df <- df[(base::nrow(df) - last + 1):base::nrow(df),]
  }
  
  # Checking colors setting
  brewer_palettes <- row.names(RColorBrewer::brewer.pal.info)
  viridis_palettes <- c("viridis", "magma", "plasma", "inferno", "cividis")
  
  if(type %in% c("cycle", "box", "all")){
    if(palette %in% brewer_palettes){
      n_colors <- NULL
      n_colors <- RColorBrewer::brewer.pal.info$maxcolors[row.names(RColorBrewer::brewer.pal.info)  == palette]
      colors_list <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(n_colors, palette))(base::length(base::unique(df$minor)))
    } else if (palette %in% viridis_palettes){
      color_list <- viridis::viridis_pal(option = base::eval(palette))(base::length(base::unique(df$minor)))  
    } else {
      warning("The value of the 'palette' argument is in valid, using the default option 'Set1'")
      palette <- "Set1"
      n_colors <- NULL
      n_colors <- RColorBrewer::brewer.pal.info$maxcolors[row.names(RColorBrewer::brewer.pal.info)  == palette]
      colors_list <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, palette))(base::length(base::unique(df$minor)))
    }
  }
  
  if(type %in% c("normal", "all")){
    if(palette_normal %in% brewer_palettes){
      n_colros <- NULL
      n_colors <- RColorBrewer::brewer.pal.info$maxcolors[row.names(RColorBrewer::brewer.pal.info)  == palette_normal]
      colors_list_normal <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(n_colors, palette_normal))(base::max(df$main) - base::min(df$main) + 1)
    } else if (palette_normal %in% viridis_palettes){
      color_list <- viridis::viridis_pal(option = base::eval(palette_normal))(base::max(df$main) - base::min(df$main) + 1)
    } else {
      warning("The value of the 'palette_normal' argument is in valid, using the default option 'Spectral'")
      palette_normal <- "Spectral"
      n_colors <- RColorBrewer::brewer.pal.info$maxcolors[row.names(RColorBrewer::brewer.pal.info)  == palette_normal]
      colors_list_normal <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(n_colors, palette_normal))(base::max(df$main) - base::min(df$main) + 1)
    }
  }  
  
  if(type == "normal" | type == "all"){
    p_normal <- plotly::plot_ly()
    for(i in base::min(df$main):base::max(df$main)){
      temp <- NULL
      temp <- df %>% dplyr::filter(main == i)
      
      p_normal <- p_normal %>%
        plotly::add_lines(x = temp$minor, y = temp$y, name = i, line = list(color = colors_list_normal[i + 1 - base::min(df$main)]))
    }
    p_normal <- p_normal %>% plotly::layout(yaxis = list(title = "By Frequency Cycle"))
  }
  
  if(type == "cycle" | type == "all"){
    df_t <- NULL
    df_t <- base::suppressMessages(df %>% reshape2::dcast(main ~ minor))
    p_cycle <- plotly::plot_ly()
    for(i in 2:ncol(df_t)){
      p_cycle <- p_cycle %>% 
        plotly::add_lines(x = df_t[, 1], y = df_t[, i], name = colnames(df_t)[i], line = list(color = colors_list[i - 1]))
    }
    
    p_cycle <- p_cycle %>% plotly::layout(yaxis = list(title = "By Frequency Unit"))
  }
  
  if(type == "box" | type == "all"){
    minor <- base::unique(df$minor)
    p_box <- plotly::plot_ly()
    c <- NULL
    c <- 1
    for(i in minor){
      p_box <- p_box %>% plotly::add_trace(data = df %>% dplyr::filter(minor == i), y = ~ y,  type = "box", 
                                           line = list(color = colors_list[c]), 
                                           boxpoints = "all",
                                           jitter = 0.3,
                                           pointpos = -1.8, 
                                           name = i)
      c <- c + 1
    }
    p_box <- p_box %>% plotly::layout(yaxis = list(title = "By Frequency Unit"))
  }
  
  if(type == "all"){
    p <- plotly::subplot(p_normal, p_cycle, p_box, nrows = 3, titleY = TRUE)
  } else if(type == "normal"){
    p <- p_normal
  } else if(type == "cycle"){
    p <- p_cycle
  } else if(type == "box"){
    p <- p_box
  }
  
  p <- p %>% plotly::layout(title = title)
  
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


#' Moving Average Method for Time Series Data
#' @export
#' @param ts.obj a univariate time series object of a class "ts", "zoo" or "xts" (support only series with either monthly or quarterly frequency)
#' @param n A single or multiple integers (by default using 3, 6, and 9 as inputs), 
#' define a two-sides moving averages by setting the number of past and future to use 
#' in each moving average window along with current observation. 
#' @param n_left A single integer (optional argument, default set to NULL), can be used, 
#' along with the n_right argument, an unbalanced moving average. 
#' The n_left defines the number of lags to includes in the moving average.
#' @param n_right A single integer (optional argument, default set to NULL), can be used, 
#' along with the n_left argument, to set an unbalanced moving average. 
#' The n_right defines the number of negative lags to includes in the moving average.
#' @param double A single integer, an optional argument. If not NULL (by default), will apply a second moving average process on the initial moving average output
#' @param plot A boolean, if TRUE will plot the results
#' @param multiple A boolean, if TRUE (and n > 1) will create multiple plots, one for each moving average degree. By default is set to FALSE
#' @param separate A boolean, if TRUE will separate the orignal series from the moving average output
#' @param title A character, if not NULL (by default), will use the input as the plot title
#' @param Xtitle A character, if not NULL (by default), will use the input as the plot x - axis title
#' @param Ytitle A character, if not NULL (by default), will use the input as the plot y - axis title
#' @param margin A numeric, set the plot margin when using the multiple or/and separate option, default value is 0.03
#' @param show_legend A boolean, if TRUE will show the plot legend
#' @description Calculate the moving average (and double moving average) for time series data
#' @return A list with the original series, the moving averages outputs and the plot
#' @details 
#' A one-side moving averages (also known as simple moving averages) calculation for Y[t] (observation Y of the series at time t):
#' 
#' MA[t|n] = (Y[t-n] + Y[t-(n-1)] +...+ Y[t]) / (n + 1), 
#' 
#' where n defines the number of consecutive observations to be used on each rolling window along with the current observation 
#' 
#' Similarly, a two-sided moving averages with an order of (2*n + 1) for Y[t]:
#' 
#' MA[t|n] = (Y[t-n] + Y[t-(n-1)] +...+ Y[t] +...+ Y[t+(n-1)] + Y[t+n]) / (2*n + 1)
#' 
#' Unbalanced moving averages with an order of (k1 + k2 + 1) for observation Y[t]:
#' 
#' MA[t|k1 & k2] = (Y[t-k1] + Y[t-(k1-1)] +...+ Y[t] +...+ Y[t+(k2-1)] + Y[t+k2]) / (k1 + k2 + 1)
#' 
#' The unbalanced moving averages is a special case of two-sides moving averages, 
#' where k1 and k2 represent the number of past and future periods, 
#' respectively to be used in each rolling window, and k1 != k2 
#' (otherwise it is a normal two-sided moving averages function)
#' 
#' @examples
#' 
#' # A one-side moving average order of 7
#' USgas_MA7 <- ts_ma(USgas, n_left = 6, n = NULL)
#' 
#' # A two-sided moving average order of 13
#' USgas_two_side_MA <- ts_ma(USgas, n = 6)
#' 
#' # Unbalanced moving average of order 12
#'  USVSales_MA12 <- ts_ma(USVSales, n_left = 6, n_right = 5, n = NULL, 
#'  title = "US Monthly Total Vehicle Sales - MA", 
#'  Ytitle = "Thousand of Units")
#'
#' # Adding double MA of order 2 to balanced the series:
#' USVSales_MA12 <- ts_ma(USVSales, n_left = 6, n_right = 5, n = NULL, 
#'  double = 2,
#'  title = "US Monthly Total Vehicle Sales - MA", 
#'  Ytitle = "Thousand of Units")
#' 
#' # Adding several types of two-sided moving averages along with the unblanced
#' # Plot each on a separate plot
#' USVSales_MA12 <- ts_ma(USVSales, n_left = 6, n_right = 5, n = c(3, 6, 9), 
#' double = 2, multiple = TRUE,
#' title = "US Monthly Total Vehicle Sales - MA", 
#' Ytitle = "Thousand of Units")


ts_ma <- function(ts.obj, 
                  n = c(3, 6, 9), 
                  n_left = NULL,
                  n_right = NULL,
                  double = NULL, 
                  plot = TRUE, show_legend = FALSE,
                  multiple = FALSE, separate = TRUE, margin = 0.03,
                  title = NULL, Xtitle = NULL, Ytitle = NULL){
  
  `%>%` <- magrittr::`%>%`
  
  obj.name <- ts_merged <- ts_obj <- ts_temp <- ts_ma <- c <- p <-  p_m <- ma_order <-  NULL
  output <- titles <- NULL
  left_flag <- right_flag <- k_flag <- FALSE
  obj.name <- base::deparse(base::substitute(ts.obj))
  
  # Error Handling  
  if (stats::is.ts(ts.obj)) {
    if (stats::is.mts(ts.obj)) {
      warning("The 'ts.obj' has multiple columns, only the first column will be plot")
      ts.obj <- ts.obj[, 1]
    }
  }
  
  if((base::is.null(n) & base::is.null(n_left) & base::is.null(n_right)) | 
     (!base::is.numeric(n) & !base::is.numeric(n_left) & !base::is.numeric(n_right))){
    stop("Neither of the moving averages arguments set properly ('n', 'n_left', 'n_right')")
  }
  
  if(!base::is.logical(plot)){
    warning("The value of the 'plot' argument is not valid (can apply either TRUE or FALSE) and will be ignore")
    plot <- TRUE
  }
  
  if(!base::is.logical(show_legend)){
    warning("The value of the 'show_legend' argument is not valid (can apply either TRUE or FALSE) and will be ignore")
    show_legend <- FALSE
  }
  
  if(!base::is.logical(separate)){
    warning("The value of the 'separate' argument is not valid (can apply either TRUE or FALSE) and will be ignore")
    separate <- TRUE
  }
  
  if(!base::is.logical(multiple)){
    warning("The value of the 'multiple' argument is not valid (can apply either TRUE or FALSE) and will be ignore")
    multiple <- FALSE
  } else if(base::length(n) == 1 & multiple & 
            base::is.null(n_left) &
            base::is.null(n_right)){
    warning("The 'multiple' aregument cannot be used when using multiple moving averages")
    multiple <- FALSE
  } else if((base::length(n) > 1 | (base::length(n) ==1 & 
                                    (!base::is.null(n_left) | !base::is.null(n_right)))) &
            multiple){
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
  
  
  if(!base::is.null(n_left)){
    if(!base::is.numeric(n_left)){
      stop("The 'n_left' argument is not valid, please make sure that you are using only integers as input")
    } else if(base::length(n_left) != 1){
      warning("The 'n_left' argument has too many inputs, can hanlde only single integer. Will use only the first input")
      n_left <- n_left[1]
    } else if(n_left %% 1 != 0){
      stop("The 'n_left' argument is not an integer type")
    } else {
      ma_order <- n_left
    }
  }
  
  if(!base::is.null(n_right)){
    if(!base::is.numeric(n_right)){
      stop("The 'n_right' argument is not valid, please make sure that you are using only integers as input")
    } else if(base::length(n_right) != 1){
      warning("The 'n_right' argument has too many inputs, can hanlde only single integer. Will use only the first input")
      n_right <- n_right[1]
    } else if(n_right %% 1 != 0){
      stop("The 'n_right' argument is not an integer type")
    } else if(!base::is.null(n_left)){
      ma_order <- n_left + n_right
    } else {
      ma_order <- n_right
    }
  }
  
  if(!base::is.null(n)){
    if(!base::is.numeric(n)){
      stop("The 'n' argument is not valid, please make sure that you are using only integers as input")
    } else if(!base::all(n %% 1 == 0)){
      stop("The 'n' argument is not valid, please make sure that you are using only integers as input")
    } else if(base::length(n) > 8){
      warning("The 'n' parameter is restricted up to 8 inputs (integers), only the first 8 values will be used")
      n <- n[1:8]
    } else if(base::max(n) * 2 + 1 > base::length(ts.obj)){
      stop("The length of the series is too short to apply the moving average with the given 'n' parameter")
    }
  }
  
  
  
  # Setting function to calculate moving average
  ma_fun <- function(ts.obj, n_left, n_right){
    
    ts_left <- ts_right <- ts_intersect <- ma_order <-  NULL
    if(!base::is.null(n_left)){
      for(i in 1:n_left){
        ts_left <- stats::ts.intersect(stats::lag(ts.obj, k = -i), ts_left)
      }
      ma_order <- n_left
    }
    
    if(!base::is.null(n_right)){
      for(i in 1:n_right){
        ts_right <- stats::ts.intersect(stats::lag(ts.obj, k =  i), ts_right)
      }
      if(!base::is.null(n_left)){
        ma_order <- ma_order + n_right
      } else {
        ma_order <- n_right
      }
    }
    ma_order <- ma_order + 1
    ts_intersect <- TSstudio::ts_sum(stats::ts.intersect(ts_left, ts.obj, ts_right)) / (ma_order)
    return(ts_intersect)
  }
  
  
  
  # Creating a list  
  output <- list()
  titles <- list()
  
  
  
  if(!base::is.null(n)){
    for(i in n){
      ts_ma1 <- ma_title <- ma_order <- NULL
      ma_order <- 2 * i + 1
      ts_ma1 <- ma_fun(ts.obj = ts.obj, n_left = i, n_right = i)
      ma_title <- paste("Two Sided Moving Average - Order", 2 * i + 1, sep = " ")
      base::eval(base::parse(text = base::paste("output$ma_", i, " <- ts_ma1", sep = "")))
      base::eval(base::parse(text = base::paste("titles$ma_", i, " <- ma_title", sep = "")))
      if(!base::is.null(double)){
        ts_ma_d <- ma_title <- NULL
        ts_ma_d <- ma_fun(ts.obj = ts_ma1, n_left = double, n_right = double)
        ma_title <- paste("Double Two Sided Moving Average - Order", 2 * double + 1, "x", ma_order, sep = " ")
        base::eval(base::parse(text = base::paste("output$double_ma_", 2 * double + 1,"_x_", ma_order, " <- ts_ma_d", sep = "")))
        base::eval(base::parse(text = base::paste("titles$double_ma_", 2 * double + 1,"_x_", ma_order, " <- ma_title", sep = "")))
      }
    }
  }
  
  if(!base::is.null(n_left) | !base::is.null(n_right)){
    ts_ma2 <- ma_title <- ma_order <- NULL
    
    ma_order <- 1
    
    if(!base::is.null(n_right)){
      ma_order <- ma_order + n_right
    } 
    
    if(!base::is.null(n_left)){
      ma_order <- ma_order + n_left
    } 
    
    ts_ma2 <- ma_fun(ts.obj = ts.obj, n_left = n_left, n_right = n_right)
    ma_title <- paste("Two Sided Moving Average - Order", ma_order, sep = " ")
    base::eval(base::parse(text = base::paste("output$unbalanced_ma_", ma_order, " <- ts_ma2", sep = "")))
    base::eval(base::parse(text = base::paste("titles$unbalanced_ma_", ma_order, " <- ma_title", sep = "")))
    if(!base::is.null(double)){
      ts_ma_d <- ma_title <- NULL
      ts_ma_d <- ma_fun(ts.obj = ts_ma2, n_left = double, n_right = double)
      ma_title <- paste("Double Two Sided Moving Average - Order", 2 * double + 1, "x",  ma_order, sep = " ")
      base::eval(base::parse(text = base::paste("output$double_unbalanced_ma_", 2 * double + 1,"_x_", ma_order, " <- ts_ma2", sep = "")))
      base::eval(base::parse(text = base::paste("titles$double_unbalanced_ma_", 2 * double + 1,"_x_", ma_order, " <- ma_title", sep = "")))
    }
  }
  
  
  ma_list <- base::names(output)[base::which(base::names(output) != "series")]
  if(separate & multiple){
    plots <- c <- NULL
    plots <- list()
    plots[[1]] <- plotly::plot_ly(x = stats::time(ts.obj), 
                                  y = base::as.numeric(ts.obj), 
                                  name = obj.name, 
                                  type = "scatter", 
                                  mode = "lines", 
                                  line = list(color = "#00526d"),
                                  showlegend = show_legend) %>%
      plotly::layout(annotations = list(text =  obj.name, 
                                        xref = "paper", 
                                        yref = "paper",
                                        yanchor = "bottom",
                                        xanchor = "center",
                                        align = "cneter",
                                        x = 0.5,
                                        y = 1, 
                                        showarrow = FALSE,
                                        font = list(size = 12)))
    
    c <- 2
    color_ramp <- viridis::inferno(base::length(output), alpha = 1, direction = 1, begin = 0, end = 0.9)
    for(i in names(output)){
      plots[[c]] <- plotly::plot_ly(x = stats::time(output[[i]]), 
                                    y = base::as.numeric(output[[i]]), 
                                    type = "scatter",
                                    mode = "line",
                                    line = list(color = color_ramp[c -1])) %>%
        plotly::layout(annotations = list(text =  titles[[i]], 
                                          xref = "paper", 
                                          yref = "paper",
                                          yanchor = "bottom",
                                          xanchor = "center",
                                          align = "cneter",
                                          x = 0.5,
                                          y = 1, 
                                          showarrow = FALSE,
                                          font = list(size = 12)))
      c <- c + 1
    }
    plot_rows <- ifelse(length(plots) > 5, base::ceiling(base::length(plots)/2), base::length(plots))
    if(show_legend){
      output$plot <- plotly::subplot(plots, nrows = plot_rows, margin = margin) 
    } else {
      output$plot <- plotly::subplot(plots, nrows = plot_rows, margin = margin) %>% plotly::hide_legend()
    }
    
  } else if(!separate & multiple){
    plots <- c <- NULL
    plots <- list()
    c <- 1
    color_ramp <- viridis::inferno(base::length(output), alpha = 1, direction = 1, begin = 0, end = 0.9)
    for(i in names(output)){
      plots[[c]] <- plotly::plot_ly(x = stats::time(ts.obj), 
                                    y = base::as.numeric(ts.obj), 
                                    name = obj.name, 
                                    type = "scatter", 
                                    mode = "lines", 
                                    line = list(color = "#00526d"),
                                    showlegend = show_legend) %>%
        plotly::add_lines(x = stats::time(output[[i]]), 
                          y = base::as.numeric(output[[i]]),
                          line = list(color = color_ramp[c - 1], dash = "dash")
        ) %>%
        plotly::layout(annotations = list(text =  titles[[i]], 
                                          xref = "paper", 
                                          yref = "paper",
                                          yanchor = "bottom",
                                          xanchor = "center",
                                          align = "cneter",
                                          x = 0.5,
                                          y = 1, 
                                          showarrow = FALSE,
                                          font = list(size = 12)))
      
      c <- c + 1
    }
    plot_rows <- ifelse(length(plots) > 5, base::ceiling(base::length(plots)/2), base::length(plots))
    if(show_legend){
      output$plot <- plotly::subplot(plots, nrows = plot_rows, margin = margin) 
    } else {
      output$plot <- plotly::subplot(plots, nrows = plot_rows, margin = margin) %>% plotly::hide_legend()
    }
  } else if(separate & !multiple){
    p1 <- p2 <- c <-  NULL
    p1 <- plotly::plot_ly(x = stats::time(ts.obj), 
                          y = base::as.numeric(ts.obj), 
                          name = obj.name, 
                          type = "scatter", 
                          mode = "lines", 
                          line = list(color = "#00526d"),
                          showlegend = TRUE) %>%
      plotly::layout(annotations = list(text =  obj.name, 
                                        xref = "paper", 
                                        yref = "paper",
                                        yanchor = "bottom",
                                        xanchor = "center",
                                        align = "cneter",
                                        x = 0.5,
                                        y = 1, 
                                        showarrow = FALSE,
                                        font = list(size = 12)))
    
    
    c <- 1 
    color_ramp <- viridis::inferno(base::length(output), alpha = 1, direction = 1, begin = 0, end = 0.9)
    p2 <- plotly::plot_ly()
    for(i in names(output)){
      p2 <- p2 %>% 
        plotly::add_lines(
          x = stats::time(output[[i]]), 
          y = base::as.numeric(output[[i]]),
          line = list(color = color_ramp[c], dash = "dash"),
          name = titles[[i]]
        )
      c <- c + 1
    }
    p2 <- p2 %>% 
      plotly::layout(annotations = list(text =  "Moving Average Output", 
                                        xref = "paper", 
                                        yref = "paper",
                                        yanchor = "bottom",
                                        xanchor = "center",
                                        align = "cneter",
                                        x = 0.5,
                                        y = 1, 
                                        showarrow = FALSE,
                                        font = list(size = 12)))
    
    
    
    if(show_legend){
      output$plot <- plotly::subplot(p1, p2, nrows = 2, margin = margin) 
    } else {
      output$plot <- plotly::subplot(p1, p2, nrows = 2, margin = margin) %>% plotly::hide_legend()
    }
    
  }else if(!separate & !multiple){
    p <- c <- NULL
    color_ramp <- viridis::inferno(base::length(output), alpha = 1, direction = 1, begin = 0, end = 0.9)
    p <- plotly::plot_ly(x = stats::time(ts.obj), 
                         y = base::as.numeric(ts.obj), 
                         name = obj.name, 
                         type = "scatter", 
                         mode = "lines", 
                         line = list(color = "#00526d"),
                         showlegend = TRUE) %>%
      plotly::layout(annotations = list(text =  obj.name, 
                                        xref = "paper", 
                                        yref = "paper",
                                        yanchor = "bottom",
                                        xanchor = "center",
                                        align = "cneter",
                                        x = 0.5,
                                        y = 1, 
                                        showarrow = FALSE,
                                        font = list(size = 12)))
    
    
    c <- 1 
    for(i in names(output)){
      p <- p %>% 
        plotly::add_lines(
          x = stats::time(output[[i]]), 
          y = base::as.numeric(output[[i]]),
          line = list(color = color_ramp[c], dash = "dash"),
          name = titles[[i]]
        )
      c <- c + 1
    }
    p <- p %>% 
      plotly::layout(annotations = list(text =  "Moving Average Output", 
                                        xref = "paper", 
                                        yref = "paper",
                                        yanchor = "bottom",
                                        xanchor = "center",
                                        align = "cneter",
                                        x = 0.5,
                                        y = 1, 
                                        showarrow = FALSE,
                                        font = list(size = 12)))
    
    
    
    if(show_legend){
      output$plot <- p 
    } else {
      output$plot <- p %>% plotly::hide_legend()
    }
  }
  
  
  if(plot){
    print(output$plot)
  }
  
  # Saving the original series
  output$series <- ts.obj
  
  class(output) <- "ts_ma"
  return(output)  
}  