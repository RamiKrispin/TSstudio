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
#' # Split the USgas dataset into training and testing partitions
#' 
#' # Set the last 12 months as a testing partition 
#' 
#' # and the rest as a training partition
#' 
#' data(USgas, package = "TSstudio)
#' 
#' split_USgas <- ts_split(ts.obj = USgase, sample.out = 12)
#'
#' training <- split_USgas$train
#' testing <- split_USgas$test
#' 
#' length(USgas)
#' 
#' length(training)
#' 
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
#' @param ts.obj a univariate time series object of a class "ts", "zoo" or "xts" (support only series with either monthly or quarterly frequency)
#' @param type The reshape type - 
#' "wide" set the years as the columns and the cycle units (months or quarter) as the rows, or
#' "long" split the time object to year, cycle unit and value
#' @description Transform time series object into data frame format
#' @examples
#' 
#' data(USgas)
#' USgas_df <- ts_reshape(USgas)

ts_reshape <- function(ts.obj, type = "wide"){
  
  `%>%` <- magrittr::`%>%`
  df <- df_table <- freq <-  freq_name <-NULL
  
  obj.name <- base::deparse(base::substitute(ts.obj))
  # --------------Error handling --------------
  if(!type %in% c("long", "wide")){
    warning("The 'type' parameter is not valid, using the default option - 'wide'")
    type <- "wide"
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
    } else if(stats::frequency(ts.obj) == 12){
      freq_name <- "month"
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
    } else if (freq == "monthly") {
      df <- base::data.frame(dec_left = lubridate::year(ts.obj), 
                             dec_right = lubridate::month(ts.obj), 
                             value = as.numeric(ts.obj))
      freq_name <- "month"
    } else if (freq == "weekly") {
      df <- data.frame(dec_left = lubridate::year(ts.obj),
                       dec_right = lubridate::week(ts.obj), value = as.numeric(ts.obj))
      freq_name <- "week"
    } else if (freq == "daily") {
      df <- data.frame(dec_left = lubridate::month(ts.obj),
                       dec_right = lubridate::day(ts.obj), value = as.numeric(ts.obj))
      freq_name <- "day"
    } else if (!freq %in% c("daily", "weekly", "monthly", "quarterly")) {
      stop("The frequency of the series is invalid,",
           "the function support only 'daily', 'weekly', 'monthly' or 'quarterly' frequencies")
    }
    
  }
  # -------------- Setting the table for long or wide format --------------
  if(type == "long"){
    df_table <- df[base::order(df$dec_left, df$dec_right),]
    names(df_table)[1] <- "year"
    names(df_table)[2] <- freq_name
  } else if(type == "wide"){
    df_table <- reshape2::dcast(df, dec_right ~ dec_left)
    names(df_table)[1] <- freq_name
  }
  
  # -------------- Function end --------------
  return(df_table)
}
