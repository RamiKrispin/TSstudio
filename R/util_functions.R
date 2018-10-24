#'  Converting 'xts' object to 'ts' object
#' @export
#' @description Converting 'xts' object to 'ts' object
#' @param xts.obj a univariate 'xts' object
#' @examples
#' 
#' data("Michigan_CS", package = "TSstudio")
#' class(Michigan_CS)
#' ts_plot(Michigan_CS)
#' Michigan_CS_ts <- xts_to_ts(Michigan_CS)
#' ts_plot(Michigan_CS_ts)


xts_to_ts <- function(xts.obj){
  if(!xts::is.xts(xts.obj)){
    if(zoo::is.zoo(xts.obj)){
      warning("The class of the series is not 'xts' but 'zoo'")
    } else {
      stop("The object is not a valid 'xts' object")
    }
      
  }
  
  if (xts::is.xts(xts.obj) | zoo::is.zoo(xts.obj)) {
    if (!is.null(base::dim(xts.obj))) {
      if (base::dim(xts.obj)[2] > 1) {
        warning("The \"xts.obj\" has multiple columns, only the first will be convert to 'ts' object")
        xts.obj <- xts.obj[, 1]
      }
    }
  }
  ts.obj <- NULL
  ts.obj <- stats::as.ts(xts.obj, 
                         start = utils::head(zoo::index(xts.obj), 1), 
                         end = utils::tail(zoo::index(xts.obj), 1))
  return(ts.obj)
}




#'  Converting 'zoo' object to 'ts' object
#' @export
#' @description Converting 'zoo' object to 'ts' object
#' @param zoo.obj a univariate 'zoo' object
#' @examples
#' 
#' data("EURO_Brent", package = "TSstudio")
#' class(EURO_Brent)
#' ts_plot(EURO_Brent)
#' EURO_Brent_ts <- zoo_to_ts(EURO_Brent)
#' class(EURO_Brent_ts)
#' ts_plot(EURO_Brent_ts)

zoo_to_ts <- function(zoo.obj){
  if(!zoo::is.zoo(zoo.obj)){
    if(xts::is.xts(zoo.obj)){
      warning("The class of the series is not 'xts' but 'zoo'")
    } else {
      stop("The object is not a valid 'xts' object")
    }
  }
  
  if (xts::is.xts(zoo.obj) | zoo::is.zoo(zoo.obj)) {
    if (!is.null(base::dim(zoo.obj))) {
      if (base::dim(zoo.obj)[2] > 1) {
        warning("The \"xts.obj\" has multiple columns, only the first will be convert to 'ts' object")
        zoo.obj <- zoo.obj[, 1]
      }
    }
  }
  
  ts.obj <- NULL
  ts.obj <- stats::as.ts(zoo.obj, 
                         start = utils::head(zoo::index(zoo.obj), 1), 
                         end = utils::tail(zoo::index(zoo.obj), 1))
  return(ts.obj)
}






#'  Split Time Series Object for Training and Testing Partitions 
#' @export
#' @param ts.obj A univariate time series object of a class "ts"
#' @param sample.out An integer, set the number of periods of the testing or sample out partition, defualt set for 
#' 30 percent of the lenght of the series
#' @description Split a time series object into training and testing partitions
#' @examples
#' 
#' ## Split the USgas dataset into training and testing partitions
#' 
#' ## Set the last 12 months as a testing partition 
#' 
#' ## and the rest as a training partition
#' 
#' data(USgas, package = "TSstudio")
#' 
#' split_USgas <- ts_split(ts.obj = USgas, sample.out = 12)
#'
#' training <- split_USgas$train
#' testing <- split_USgas$test
#' 
#' length(USgas)
#' 
#' length(training)
#' length(testing)


ts_split <- function(ts.obj, sample.out = NULL){
  
  if (!stats::is.ts(ts.obj)) {
    stop("The 'ts.obj' is not a valid 'ts' object")
  }
  
  l <- train <- test <- split <- NULL
  l <- base::length(ts.obj)
  
  if(base::is.null(sample.out)){
    h <- base::round(l * 0.3)
  } else if(base::round(sample.out) != sample.out){
    stop("The 'sample.out' parameter is not a valid number (must be an integer)")
  } else if(sample.out >= l){
    warning("The length of the sample out period is",
            " longer than the length of the series, ",
            "using the default option (30% of the length of the series)")
    h <- base::round(l * 0.3)
  } else {
    h <- sample.out
  }
  
  split <- base::list(
    train <- stats::window(ts.obj, 
                           start = stats::time(ts.obj)[1], 
                           end = stats::time(ts.obj)[base::length(stats::time(ts.obj)) - h]),
    test <- stats::window(ts.obj, 
                          start = stats::time(ts.obj)[base::length(stats::time(ts.obj)) - h + 1], 
                          end = stats::time(ts.obj)[base::length(stats::time(ts.obj))])
  )
  base::names(split) <- c("train", "test")
  return(split)
}



#'  Transform Time Series Object to Data Frame Format
#' @export
#' @param ts.obj a univariate time series object of a class "ts", "zoo", "xts", and the data frame family (data.frame, data.table, tbl, tibble, etc.) with a 
#' Date column and at least one numeric column. This function support time series objects with a daily, weekly, monthly or quarterly frequencies 
#' @param type The reshape type - 
#' 
#' "wide" set the years as the columns and the cycle units (months or quarter) as the rows, or
#' 
#' "long" split the time object to year, cycle unit and value
#' 
#' @param frequency An integer, define the series frequency when more than one option is avaiable and the input is one of the data frame family. 
#' If set to NULL will use the first option by default when applicable - daily = c(7, 365) 
#' @description Transform time series object into data frame format
#' @examples
#' 
#' data(USgas)
#' USgas_df <- ts_reshape(USgas)

# ---- ts_reshape functions ----

ts_reshape <- function(ts.obj, 
                       type = "wide", 
                       frequency = NULL){
  
  `%>%` <- magrittr::`%>%`
  df <- df_table <- freq <-  freq_name <- df_temp <- NULL
  year <- epiweek <- NULL
  
  obj.name <- base::deparse(base::substitute(ts.obj))
  # ---- ts_reshape error handling ----
  if(!type %in% c("long", "wide")){
    warning("The 'type' parameter is not valid, using the default option - 'wide'")
    type <- "wide"
  }
  
  # Check if the input format is a data frame
  if(base::is.data.frame(ts.obj) | 
     dplyr::is.tbl(ts.obj) | 
     data.table::is.data.table(ts.obj)){
    
    # Identify the columns classes
    col_class <- base::lapply(ts.obj, class)
    # Check if Date object exist
    if("Date" %in%  col_class){
      date_col <- base::which(col_class == "Date")
    } else {
      stop("No 'Date' object available in the data frame,", 
           "please check if the data format is defined properly")
    }
    
    # If there is more than one Date object in the data frame will select the first one
    if(length(date_col) >1){
      warning("There are multipe 'date' objects in the data frame,",
              "using the first 'date' object as the plot index")
      date_col <- date_col[1]
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
      df_temp <- base::data.frame(date = ts.obj[, date_col], y =  ts.obj[, numeric_col])
    } else {
      warning("There are more than one numeric column in the input object,",
              "selecting the first numeric column as the input object")
      df_temp <- base::data.frame(date = ts.obj[, date_col], y = ts.obj[, numeric_col[1]])
    }
    names(df_temp) <- c("date", "y")
    # Check the frequnecy of the series
    df_temp <- df_temp %>% dplyr::arrange(date) # Setting the order of the data frame by the date
    df_temp$time_diff <- df_temp$date - dplyr::lag(df_temp$date, n = 1) # Creating a time diff object to check if the series is regular
    if(which(is.na(df_temp$time_diff) == TRUE) == 1){ # Check that only the first observation is missing after the first diff
      # Case 1 - the series is daily
      if(min(df_temp$time_diff, na.rm = TRUE) == max(df_temp$time_diff, na.rm = TRUE) &  
         mean(df_temp$time_diff, na.rm = TRUE) == max(df_temp$time_diff, na.rm = TRUE) &
         mean(df_temp$time_diff, na.rm = TRUE) == 1){
        if(is.null(frequency)){
          warning("The frequency argument is set to NULL, using the default value (frequency == 7)")
          frequency <- 7
        } else if(!base::is.numeric(frequency)){
          stop("The value of the 'frequency' argument is not numeric")
        } else if(!frequency %in% c(7, 365)){
          warning("The value of the 'frequency' argument is not valid, using the default value (frequency == 7)")
          frequency <- 7
        }
        
        if(frequency == 7){
          df_temp$year <- lubridate::year(df_temp$date)
          df_temp$week <- lubridate::week(df_temp$date)
          df_temp$epiweek <- lubridate::epiweek(df_temp$date)
          df_temp$year <- ifelse(df_temp$epiweek >50 & df_temp$week == 1, df_temp$year - 1, df_temp$year)
          df_temp$year <- ifelse(df_temp$epiweek == 1 & df_temp$week > 50, df_temp$year + 1, df_temp$year)
          
          df_temp <- df_temp %>% dplyr::left_join(df_temp %>% 
                                                    dplyr::group_by(year) %>%
                                                    dplyr::summarise(max_epiweek = max(epiweek)))
          
          df_temp$max_epiweek <- ifelse(df_temp$max_epiweek <52, 52, df_temp$max_epiweek)
          
          df_temp$dec_left <- df_temp$year + df_temp$epiweek / df_temp$max_epiweek
          df_temp$dec_right <- lubridate::wday(df_temp$date)
          
          df <- base::data.frame(dec_left = df_temp$dec_left, 
                                 dec_right = df_temp$dec_right, 
                                 value = df_temp$y)
          freq_name <- "day"
          cycle_type <- "year_week"
        } else if(frequency == 365){
          df_temp$dec_left <- lubridate::year(df_temp$date)
          df_temp$dec_right <- lubridate::yday(df_temp$date)
          
          df <- base::data.frame(dec_left = df_temp$dec_left, 
                                 dec_right = df_temp$dec_right, 
                                 value = df_temp$y)
          freq_name <- "day"
          cycle_type <- "year_day"
        }
        
        
      
        # Case 2 - the series is a weekly
        } else if(min(df_temp$time_diff, na.rm = TRUE) == max(df_temp$time_diff, na.rm = TRUE) &  
                  mean(df_temp$time_diff, na.rm = TRUE) == max(df_temp$time_diff, na.rm = TRUE) &
                  mean(df_temp$time_diff, na.rm = TRUE) == 7){ 
          df_temp$year <- lubridate::year(df_temp$date)
          df_temp$week <- lubridate::week(df_temp$date)
          df_temp$epiweek <- lubridate::epiweek(df_temp$date)
          df_temp$year <- ifelse(df_temp$epiweek >50 & df_temp$week == 1, df_temp$year - 1, df_temp$year)
          
          df_temp$dec_left <- lubridate::year(df_temp$date)
          df_temp$dec_right <- lubridate::week(df_temp$date)
          
          df <- base::data.frame(dec_left = df_temp$dec_left, 
                                 dec_right = df_temp$dec_right, 
                                 value = df_temp$y)
          freq_name <- "week"
          cycle_type <- "year"
        }
    }
  }
  if (stats::is.ts(ts.obj)) {
    if (stats::is.mts(ts.obj)) {
      warning("The 'ts.obj' has multiple columns, only the first column will be plot")
      ts.obj <- ts.obj[, 1]
    }
    if(stats::frequency(ts.obj) == 4){
      freq_name <- "quarter"
      cycle_type <- "year"
      df <- base::data.frame(dec_left = floor(stats::time(ts.obj)), 
                             dec_right = stats::cycle(ts.obj), 
                             value = base::as.numeric(ts.obj))
    } else if(stats::frequency(ts.obj) == 12){
      freq_name <- "month"
      cycle_type <- "year"
      df <- base::data.frame(dec_left = floor(stats::time(ts.obj)), 
                             dec_right = stats::cycle(ts.obj), 
                             value = base::as.numeric(ts.obj))
    } else if(stats::frequency(ts.obj)== 52 ){
      # Weekly data
      df <- base::data.frame(dec_left = NA,
                             dec_left_temp = base::as.integer(stats::time(ts.obj)), 
                             dec_right = stats::cycle(ts.obj), 
                             value = base::as.numeric(ts.obj))
      
      df$lag <- dplyr::lead(df$dec_left_temp, n = 1)
      
      df$dec_left <- ifelse((df$dec_left_temp != df$lag) & df$dec_right == 1, df$lag, df$dec_left_temp)
      df$dec_left_temp <- df$lag <- NULL
      
      freq_name <- "week"
      cycle_type <- "year"
    
    }else if(round(stats::frequency(ts.obj)) == 52 ){
      # Weekly data with non-integer frequency
      df <- base::data.frame(dec_left = base::floor(stats::time(ts.obj)),
                             dec_right = NA,
                             value = base::as.numeric(ts.obj)
      )
      
      for(i in 1:nrow(df)){
        if(i == 1){
          df$dec_right[i] <- stats::cycle(ts.obj)[1]
        } else if(df$dec_left[i] == df$dec_left[i - 1]){
          df$dec_right[i] <- df$dec_right[i - 1] + 1
        } else{
          df$dec_right[i] <- 1
        }
      }
      
      
      freq_name <- "week"
      cycle_type <- "year"
      
    }else if(stats::frequency(ts.obj) == 365 ){ 
      # Daily data
      freq_name <- "day"
      cycle_type <- "year"

        df <- base::data.frame(dec_left_temp = base::as.integer(stats::time(ts.obj)), 
                               dec_right = stats::cycle(ts.obj), 
                               value = base::as.numeric(ts.obj))
      
        df$lag <- dplyr::lead(df$dec_left_temp, n = 1)
        
        df$dec_left <- ifelse((df$dec_left_temp != df$lag) & df$dec_right == 1, df$lag, df$dec_left_temp)
        df$dec_left_temp <- df$lag <- NULL
        
    } else if(round(stats::frequency(ts.obj)) == 365 ){
      # Daily data with non-integer frequency
      freq_name <- "day"
      cycle_type <- "year"
        df <- base::data.frame(dec_left = base::floor(stats::time(ts.obj)),
                               dec_right = NA,
                               value = base::as.numeric(ts.obj)
        )
        
        for(i in 1:nrow(df)){
          if(i == 1){
            df$dec_right[i] <- stats::cycle(ts.obj)[1]
          } else if(df$dec_left[i] == df$dec_left[i - 1]){
            df$dec_right[i] <- df$dec_right[i - 1] + 1
          } else{
            df$dec_right[i] <- 1
          }
        }
      

    } else {
      stop("The frequency of the series is invalid, ",
           "the function support only 'weekly', 'monthly' or 'quarterly' frequencies")
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
      freq_name <- "quarter"
      cycle_type <- "year"
    } else if (freq == "monthly") {
      df <- base::data.frame(dec_left = lubridate::year(ts.obj), 
                             dec_right = lubridate::month(ts.obj), 
                             value = as.numeric(ts.obj))
      freq_name <- "month"
      cycle_type <- "year"
    } else if (freq == "weekly") {
      df <- data.frame(dec_left = lubridate::year(ts.obj),
                       dec_right = lubridate::week(ts.obj), value = as.numeric(ts.obj))
      freq_name <- "week"
      cycle_type <- "year"
    } else if (freq == "daily") {
      if(is.null(frequency)){
        warning("The frequency argument is set to NULL, using the default value (frequency == 7)")
        frequency <- 7
      } else if(!base::is.numeric(frequency)){
        stop("The value of the 'frequency' argument is not numeric")
      } else if(!frequency %in% c(7, 365)){
        warning("The value of the 'frequency' argument is not valid, using the default value (frequency == 7)")
        frequency <- 7
      }
      if(frequency == 7){
      df_temp <- NULL
      df_temp <- base::data.frame(date = zoo::index(ts.obj),
                                  y = as.numeric(ts.obj[, 1]),
                                  year = lubridate::year(zoo::index(ts.obj)),
                                  week = lubridate::week(zoo::index(ts.obj)),
                                  epiweek = lubridate::epiweek(zoo::index(ts.obj))
                                  )
      
      df_temp$year <- lubridate::year(df_temp$date)
      df_temp$week <- lubridate::week(df_temp$date)
      df_temp$epiweek <- lubridate::epiweek(df_temp$date)
      df_temp$year <- ifelse(df_temp$epiweek >50 & df_temp$week == 1, df_temp$year - 1, df_temp$year)
      df_temp$year <- ifelse(df_temp$epiweek == 1 & df_temp$week > 50, df_temp$year + 1, df_temp$year)
      
      df_temp$dec_left <- df_temp$year + df_temp$epiweek / 100
      df_temp$dec_right <- lubridate::wday(df_temp$date)
      
      df <- base::data.frame(dec_left = df_temp$dec_left, 
                             dec_right = df_temp$dec_right, 
                             value = df_temp$y)
      freq_name <- "day"
      cycle_type <- "year_week"
      }else if(frequency == 365){
        df_temp <- NULL
        df_temp <- base::data.frame(date = zoo::index(ts.obj),
                                    y = as.numeric(ts.obj[, 1])
                                    )
        df_temp$dec_left <- lubridate::year(df_temp$date)
        df_temp$dec_right <- lubridate::yday(df_temp$date)
        
        df <- base::data.frame(dec_left = df_temp$dec_left, 
                               dec_right = df_temp$dec_right, 
                               value = df_temp$y)
        freq_name <- "day"
        cycle_type <- "year_day"
      }
    
    } else if (!freq %in% c("daily", "weekly", "monthly", "quarterly")) {
      stop("The frequency of the series is invalid,",
           "the function support only 'daily', 'weekly', 'monthly' or 'quarterly' frequencies")
    }
    
  }
  # -------------- Setting the table for long or wide format --------------
  if(type == "long"){
    df_table <- df[base::order(df$dec_left, df$dec_right),]
    names(df_table)[1] <- cycle_type
    names(df_table)[2] <- freq_name
  } else if(type == "wide"){
    df_table <- reshape2::dcast(df, dec_right ~ dec_left, 
                                value.var = "value",
                                fill = NA_real_,
                                fun.aggregate = sum
                                )
    names(df_table)[1] <- freq_name
    
  }
  
  # -------------- Function end --------------
  return(df_table)
}


#' Summation of Multiple Time Series Object
#' @export
#' @param mts.obj A multivariate time series object of a class "mts"
#' @description A row sum function for multiple time series object ("mts"), return the
#' the summation of the "mts" object as a "ts" object
#' @examples
#' 
#' x <- matrix(c(1:100, 1:100, 1:100), ncol = 3)
#' mts.obj <- ts(x, start = c(2000, 1), frequency = 12)
#' ts_total <- ts_sum(mts.obj)
#' 


ts_sum <- function(mts.obj){
  if(!stats::is.mts(mts.obj)){
    stop("The input object is not 'mts' class")
  }
  
  tsSum <- stats::ts(rowSums(mts.obj), 
                     start = stats::start(mts.obj), 
                     frequency = stats::frequency(mts.obj))
  
  return(tsSum)
}


#' Get the Time Series Information
#' @export
#' @description Returning the time series object main characteristics 
#' @param ts.obj A time series object of a class "ts", "mts", "xts", or "zoo"
#' @return Text
#' @examples
#' 
#' # ts object
#' data("USgas")
#' ts_info(USgas)
#' 
#' # mts object
#' data("Coffee_Prices")
#' ts_info(Coffee_Prices)
#' 
#' # xts object
#' data("Michigan_CS")
#' ts_info(Michigan_CS)

ts_info <- function(ts.obj){
  
  # Error handling
  if(!stats::is.ts(ts.obj) & !xts::is.xts(ts.obj) & !zoo::is.zoo(ts.obj)){
    stop("The input object is not a valid time series object")
  }
  
  obj.name <- info <- NULL
  
  obj.name <- base::deparse(base::substitute(ts.obj))
  info <- list()
  if(stats::is.ts(ts.obj) & !stats::is.mts(ts.obj)){
    info$name <- obj.name
    info$class <- "ts"
    info$frequency <- stats::frequency(ts.obj)
    info$start <- base::paste(stats::start(ts.obj), collapse = " ")
    info$end <- base::paste(stats::end(ts.obj), collapse = " ")
    info$length <- base::length(ts.obj)
    info$var <- "1 variable"
  } else if(stats::is.ts(ts.obj) & stats::is.mts(ts.obj)){
    info$name <- obj.name
    info$class <- "mts"
    info$frequency <- stats::frequency(ts.obj)
    info$start <- base::paste(stats::start(ts.obj), collapse = " ")
    info$end <- base::paste(stats::end(ts.obj), collapse = " ")
    info$length <- base::length(ts.obj)
    info$var <- base::paste(dim(ts.obj)[2], "variables", sep = " ")
  } else if(xts::is.xts(ts.obj)){
    info$name <- obj.name
    info$class <- "xts"
    if(xts::periodicity(ts.obj)$scale != "minute"){
    info$frequency <- xts::periodicity(ts.obj)$scale
    } else {
      info$frequency <- base::paste(xts::periodicity(ts.obj)$frequency, xts::periodicity(ts.obj)$units, collapse = " ")
    }
    info$start <- base::paste(stats::start(ts.obj), collapse = " ")
    info$end <- base::paste(stats::end(ts.obj), collapse = " ")
    
    if(base::is.null(base::dim(ts.obj)) & !base::is.null(base::length(ts.obj))){
      info$var <- info$var <- "1 variable"
      info$length <- base::length(ts.obj)
    } else if(dim(ts.obj)[2] == 1){
      info$var <- base::paste(dim(ts.obj)[2], "variable", sep = " ")
      info$length <- base::dim(ts.obj)[1]
    } else if(dim(ts.obj)[2] > 1){
      info$var <- base::paste(dim(ts.obj)[2], "variables", sep = " ")
      info$length <- base::dim(ts.obj)[1]
    }
    
  } else if(zoo::is.zoo(ts.obj)){
    info$name <- obj.name
    info$class <- "zoo"
    info$frequency <- xts::periodicity(ts.obj)$scale
    info$start <- base::paste(stats::start(ts.obj), collapse = " ")
    info$end <- base::paste(stats::end(ts.obj), collapse = " ")
    info$length <- base::length(ts.obj)
    if(base::is.null(base::dim(ts.obj)) & !base::is.null(base::length(ts.obj))){
      info$var <- info$var <- "1 variable"
      info$length <- base::length(ts.obj)
    } else if(dim(ts.obj)[2] == 1){
      info$var <- base::paste(dim(ts.obj)[2], "variable", sep = " ")
      info$length <- base::dim(ts.obj)[1]
    } else if(dim(ts.obj)[2] > 1){
      info$var <- base::paste(dim(ts.obj)[2], "variables", sep = " ")
      info$length <- base::dim(ts.obj)[1]
    }
  }
  
  base::cat(base::paste(" The", info$name, "series is a",   
                        info$class, "object with", info$var, "and", info$length, "observations\n",
                        "Frequency:", info$frequency, "\n",
                        "Start time:", info$start, "\n",
                        "End time:", info$end, "\n"))
}

#'  Transform Time Series Object to Prophet input
#' @export
#' @param ts.obj A univariate time series object of a class "ts", "zoo", "xts", with a daily, weekly, monthly , quarterly or yearly frequency 
#' @param start A date object (optional), if the starting date of the series is known. Otherwise, the date would be derive from the series index
#' @description Transform a time series object to Prophet data frame input format
#' @return A data frame object
#' @examples
#' 
#' data(USgas)
#' 
#' ts_to_prophet(ts.obj = USgas)
#' 
#' # If known setting the start date of the input object
#' 
#' ts_to_prophet(ts.obj = USgas, start = as.Date("2000-01-01"))
#' 

ts_to_prophet <- function(ts.obj, start = NULL){
  
  if(xts::is.xts(ts.obj) | zoo::is.zoo(ts.obj)){
    if(!base::is.null(start) && lubridate::is.Date(start)){
      if(xts::periodicity(ts.obj)$scale == "yearly"){
        df <- base::data.frame(ds = base::seq.Date(from = start, 
                                                   by = "year", 
                                                   length.out = base::length(ts.obj)), 
                               y = base::as.numeric(ts.obj))
      }
    } else {
      if(lubridate::is.Date(zoo::index(x))){
        start <- zoo::index(ts.obj)[1]
      } else if(class(zoo::index(ts.obj)) == "yearmon"){
        start <- paste(base::substr(zoo::index(ts.obj)[1], 5, 8), 
                       substr(zoo::index(ts.obj)[1], 1, 3) %>% match(month.abb), 
                       "01", sep = "-") %>% base::as.Date()
      } else if(class(zoo::index(ts.obj)) == "yearqtr") {
        start <- zoo::index(ts.obj[1]) %>% zoo::as.Date.yearqtr()
      }
      
      
      if(xts::periodicity(ts.obj)$scale == "yearly"){
        if()
        start <- 
        df <- base::data.frame(ds = base::seq.Date(from = start, by = "year", length.out = base::length(ts.obj)), 
                               y = base::as.numeric(ts.obj))
      }
    }
    if(xts::periodicity(ts.obj)$scale == "daily")
    
  } else if(stats::is.ts(ts.obj)){
  
  if(!base::is.null(start) && lubridate::is.Date(start)){
    if(stats::frequency(ts.obj) == 1){
      df <- base::data.frame(ds = base::seq.Date(from = start, by = "year", length.out = base::length(ts.obj)), 
                             y = base::as.numeric(ts.obj))
    } else if(stats::frequency(ts.obj) == 4){
      df <- base::data.frame(ds = base::seq.Date(from = start, by = "quarter", length.out = base::length(ts.obj)),
                             y = base::as.numeric(ts.obj))
    } else if(stats::frequency(ts.obj) == 12){
      df <- base::data.frame(ds = base::seq.Date(from = start, by = "month", length.out = base::length(ts.obj)),
                             y = base::as.numeric(ts.obj))
    } else if(stats::frequency(ts.obj) == 52 | 
              stats::frequency(ts.obj) == 365.25 / 7 | 
              stats::frequency(ts.obj) == 365 / 7){
      df <- base::data.frame(ds = base::seq.Date(from = start, by = "week", length.out = base::length(ts.obj)),
                             y = base::as.numeric(ts.obj))
    } else if(stats::frequency(ts.obj) == 365 | 
              stats::frequency(ts.obj) == 365.25){
      df <- base::data.frame(ds = base::seq.Date(from = start, by = "days", length.out = base::length(ts.obj)),
                             y = base::as.numeric(ts.obj))
    }
  } else {
    if(stats::frequency(ts.obj) == 1){
      start <- lubridate::ymd(base::paste(stats::start(ts.obj)[1], "01-01", sep = "-"))
      df <- base::data.frame(ds = base::seq.Date(from = start, by = "year", length.out = base::length(ts.obj)), 
                             y = base::as.numeric(ts.obj))
    } else if(stats::frequency(ts.obj) == 4){
      start <- lubridate::ymd(base::paste(stats::start(ts.obj)[1], (stats::start(ts.obj)[2] * 3 -2), "01", sep = "-"))
      df <- base::data.frame(ds = base::seq.Date(from = start, by = "quarter", length.out = base::length(ts.obj)),
                             y = base::as.numeric(ts.obj))
    } else if(stats::frequency(ts.obj) == 12){
      start <- lubridate::ymd(base::paste(stats::start(ts.obj)[1], stats::start(ts.obj)[2] , "01", sep = "-"))
      df <- base::data.frame(ds = base::seq.Date(from = start, by = "month", length.out = base::length(ts.obj)),
                             y = base::as.numeric(ts.obj))
    } else if(stats::frequency(ts.obj) == 52 | 
              stats::frequency(ts.obj) == 365.25 / 7 | 
              stats::frequency(ts.obj) == 365 / 7){
      start <- base::as.Date(stats::start(ts.obj)[2] * 7, 
                             origin = base::as.Date(base::paste(stats::start(ts.obj)[1], "-01-01", sep = "")))
      df <- base::data.frame(ds = base::seq.Date(from = start, by = "week", length.out = base::length(ts.obj)),
                             y = base::as.numeric(ts.obj))
    } else if(stats::frequency(ts.obj) == 365 | 
              stats::frequency(ts.obj) == 365.25){
      start <- base::as.Date(stats::start(ts.obj)[2] - 1, 
                             origin = base::as.Date(base::paste(stats::start(ts.obj)[1], "-01-01", sep = "")))
      df <- base::data.frame(ds = base::seq.Date(from = start, by = "days", length.out = base::length(ts.obj)),
                             y = base::as.numeric(ts.obj))
    }
  }
  }
  
  return(df)
  
}
