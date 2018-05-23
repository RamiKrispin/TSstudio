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
#' @param ts.obj a univariate time series object of a class "ts", "zoo", "xts" (support only series with either monthly or quarterly frequency). 
#' In addition it support the data frame family (data.frame, data.table, tbl, tibble, etc.) as long as there is a date and numeric objects in the data frame.
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
                       type = "wide", frequency = NULL){
  
  `%>%` <- magrittr::`%>%`
  df <- df_table <- freq <-  freq_name <- df_temp <- NULL
  
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
          
          df_temp$dec_left <- df_temp$year + df_temp$epiweek / 100
          df_temp$dec_right <- lubridate::wday(df_temp$date)
          
          df <- base::data.frame(dec_left = df_temp$dec_left, 
                                 dec_right = df_temp$dec_right, 
                                 value = df_temp$y)
          freq_name <- "day"
          cycle_type <- "year_week"
        }
        
        
      }
    }
  }
  if (stats::is.ts(ts.obj)) {
    if (stats::is.mts(ts.obj)) {
      warning("The 'ts.obj' has multiple columns, only the first column will be plot")
      ts.obj <- ts.obj[, 1]
    }
    df <- base::data.frame(dec_left = floor(stats::time(ts.obj)), 
                           dec_right = stats::cycle(ts.obj), 
                           value = base::as.numeric(ts.obj))
    if(stats::frequency(ts.obj) == 4){
      freq_name <- "quarter"
      cycle_type <- "year"
    } else if(stats::frequency(ts.obj) == 12){
      freq_name <- "month"
      cycle_type <- "year"
    }else {
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
      df <- data.frame(dec_left = lubridate::month(ts.obj),
                       dec_right = lubridate::day(ts.obj), value = as.numeric(ts.obj))
      freq_name <- "day"
      cycle_type <- "year_week"
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
    df_table <- reshape2::dcast(df, dec_right ~ dec_left)
    names(df_table)[1] <- freq_name
  }
  
  # -------------- Function end --------------
  return(df_table)
}
