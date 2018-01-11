#'  Visualization of the Decompose of a Time Series Object
#' @export
#' @param ts.obj a univariate time series object of a class "ts", "zoo" or "xts"
#' @param type Set the type of the seasonal component, can be set to either "additive",  "multiplicative" or "both" to compare between the first two options (default set to “additive”)
#' @param showline Logic, add a separation line between each of the plot components (default set to TRUE)
#' @description Interactive visualization the trend, seasonal and random components of a time series based on the decompose function from the stats package.
#' @examples
#' # Defualt decompose plot
#' decompose_ly(AirPassengers)
#' 
#' # Remove the sepration lines between the plot components
#' decompose_ly(AirPassengers, showline = FALSE)
#' 
#' # Plot side by side a decompose of additive and multiplicative series
#' decompose_ly(AirPassengers, type = "both")
#' 
decompose_ly <- function(ts.obj, type = "additive", showline = TRUE){

  # Error handling
  # Test if the object is either ts, zoo or xts
  if(!stats::is.ts(ts.obj) & !zoo::is.zoo(ts.obj) & !xts::is.xts(ts.obj)){
    stop("The 'ts.obj' is not a valid time series format (i.e. 'ts', 'xts', 'zoo')")
  }
  
  # If the object has multiple series select the first one
  if (stats::is.ts(ts.obj)) {
    if (stats::is.mts(ts.obj)) {
      warning("The \"ts.obj\" has multiple columns, only the first column will be plot")
      ts.obj <- ts.obj[, 1]
    }
  } else if (xts::is.xts(ts.obj) | zoo::is.zoo(ts.obj)) {
    if (!is.null(base::dim(ts.obj))) {
      if (base::dim(ts.obj)[2] > 1) {
        warning("The \"ts.obj\" has multiple columns, only the first column will be plot")
        ts.obj <- ts.obj[, 1]
      }
    }
  }
  
  # Test the function inputs are currect
  if(type != "additive" & 
     type != "multiplicative" & 
     type != "both"){
    warning("The value of 'type' is not valide, using the default option - 'additive'")
    type <- "additive"
  } 
  
  if(!is.logical(showline)){
    warning("The value of 'showline' is not valide, using the default option - TRUE")
    showline <- TRUE
  } 
  
  
  
`%>%` <- magrittr::`%>%`  
obj.name <- p <- p1 <- p2 <- NULL
obj.name <- base::deparse(base::substitute(ts.obj))
 
# Create a sub function for the decompose process
decompose_sub <- function(ts.obj, type, showline, obj.name, shareY = FALSE){
dec <- min <- max <- p_sub <- NULL

if(stats::is.ts(ts.obj)){
dec <- stats::decompose(ts.obj, type = type)

} else if(xts::is.xts(ts.obj) | zoo::is.zoo(ts.obj)){
  ts.obj <- stats::as.ts(ts.obj, 
                         start = utils::head(zoo::index(ts.obj), 1), 
                         end = utils::tail(zoo::index(ts.obj), 1))

  dec <- stats::decompose(ts.obj, type = type)
}

min <- min(stats::time(ts.obj))
max <- max(stats::time(ts.obj))

obs <- TSstudio::ts.plot_ly(dec$x) %>% 
  plotly::layout(yaxis = list(title = "Observed"),
  xaxis = list(range = c(min,max),
               showline = showline,
               showticklabels = FALSE)
                                  )

seasonal <- TSstudio::ts.plot_ly(dec$seasonal) %>% 
  plotly::layout(yaxis = list(title = "Seasonal"),
                                              xaxis = list(range = c(min,max),
                                                           showline = showline,
                                                           showticklabels = FALSE)
                 )
random <- TSstudio::ts.plot_ly(dec$random) %>% 
  plotly::layout(yaxis = list(title = "Random"),
                                          xaxis = list(range = c(min,max),
                                                       showline = showline)
  )

trend <- TSstudio::ts.plot_ly(dec$trend) %>% 
  plotly::layout(yaxis = list(title = "Trend"),
                                        xaxis = list(range = c(min,max),
                                                     showline = showline,
                                                     showticklabels = FALSE)
                 )

p_sub <- plotly::subplot(obs, trend, seasonal, random, nrows = 4, shareY = shareY) %>% 
  plotly::hide_legend() %>%
  plotly::layout(
    title = base::paste("Decomposition of", type, "time series -", obj.name)
  )

return(p_sub)
}

if(type == "additive" | type == "multiplicative" ){
  p <- decompose_sub(ts.obj = ts.obj, type = type, showline = showline, obj.name = obj.name, shareY = TRUE)
} else if(type == "both"){
  p1 <- decompose_sub(ts.obj = ts.obj, type = "additive", showline = showline, obj.name = obj.name, shareY = TRUE)
  p2 <- decompose_sub(ts.obj = ts.obj, type = "multiplicative", showline = showline, obj.name = obj.name, shareY = FALSE) %>%
    plotly::layout(yaxis = list(showlegend = FALSE))
  p <- plotly::subplot(p1, p2, titleY = T) %>% plotly::layout(
    title = base::paste("Decomposition of additive and multiplicative time series -", obj.name)
  )
}

return(p)
}
