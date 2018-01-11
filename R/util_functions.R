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
#' ts.plot_ly(AirPassengers)
#' 


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

#' data("EURO_Brent", package = "TSstudio")
#' class(EURO_Brent)
#' ts.plot_ly(EURO_Brent)
#' EURO_Brent_ts <- xts_to_ts(EURO_Brent)
#' ts.plot_ly(EURO_Brent_xts)