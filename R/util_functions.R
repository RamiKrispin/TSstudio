#'  Converting 'xts' object to 'ts' object
#' @export
#' @description Converting 'xts' object to 'ts' object
#' @param xts.obj a univariate 'xts' object
#' @examples
#' 
#' data("Michigan_CS", package = "TSstudio")
#' class(Michigan_CS)
#' ts.plot_ly(Michigan_CS)
#' Michigan_CS_ts <- xts_to_ts(Michigan_CS)
#' ts.plot_ly(Michigan_CS_ts)


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
#' ts.plot_ly(EURO_Brent)
#' EURO_Brent_ts <- zoo_to_ts(EURO_Brent)
#' class(EURO_Brent_ts)
#' ts.plot_ly(EURO_Brent_ts)

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
#' # Split the AirPassengers into training and testing partitions
#' 
#' # Set the last 12 months as a testing partition 
#' 
#' # and the rest as a training partition
#' 
#' data(AirPassengers)
#' 
#' split_Air <- ts_split(ts.obj = AirPassengers, sample.out = 12)
#'
#' training <- split_Air$train
#' testing <- split_Air$test
#' 
#' length(AirPassengers)
#' 
#' length(training)
#' 
#' length(testing)


ts_split <- function(ts.obj, sample.out = NULL){
  
  if (stats::is.ts(ts.obj)) {
    if (stats::is.mts(ts.obj)) {
      warning("The 'ts.obj' has multiple columns, only the first column will be plot")
      ts.obj <- ts.obj[, 1]
    }
  } else {
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
                           end = stats::time(ts.obj)[base::length(ts.obj) - h]),
    test <- stats::window(ts.obj, 
                          start = stats::time(ts.obj)[base::length(ts.obj) - h + 1], 
                          end = stats::time(ts.obj)[base::length(ts.obj)])
  )
  base::names(split) <- c("train", "test")
  return(split)
}




