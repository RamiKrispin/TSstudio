#'  Visualize of the Fitted and the Forecasted vs the Actual Values
#' @export
#' @param actual the full time series object (support "ts", "zoo" and "xts" formats)
#' @param forecast.obj The forecast output of the training set with horizon align to the length of the testing (support forecasted objects from the “forecast” package)
#' @param train the training partition, a subset of the first n observation in the series
#' @param test the testing (hold-out) partition 
#' @param Ygrid logic,show the Y axis grid if set to TRUE
#' @param Xgrid logic,show the X axis grid if set to TRUE
#' @description Visualize the fitted values of the training set and the forecast values of the testing set against the actual values of the series
#' @examples
#' \dontrun{
#' library(forecast)
#' h <- 12
#' train <- window(AirPassengers, 
#'                start = time(AirPassengers)[1], 
#'                end = time(AirPassengers)[length(AirPassengers) - h])
#'test <- window(AirPassengers, 
#'               start = time(AirPassengers)[length(AirPassengers) - h + 1], 
#'               end = time(AirPassengers)[length(AirPassengers)])
#'
#'fc <- forecast(auto.arima(train, lambda = BoxCox.lambda(train)), h = h)
#'
#'fortest_ly(actual = AirPassengers, forecast.obj = fc, train = train, test = test)
#'}

fortest_ly <- function(actual, forecast.obj, train, test, 
    Ygrid = FALSE, Xgrid = FALSE) {
    `%>%` <- magrittr::`%>%`
    # Error handling
    if (!forecast::is.forecast(forecast.obj)) {
        stop("The class of theforecast object is not \"forecast\"")
    }
    if (length(train) + length(test) != length(actual)) {
        stop("The length of the train and test sets are different from the length of the actual set")
    }
    if (!base::is.logical(Ygrid)) {
        warning("The value of \"Ygrid\" is not boolean, using the default option (FALSE)")
        Ygrid <- FALSE
    }
    
    if (!base::is.logical(Xgrid)) {
        warning("The value of \"Xgrid\" is not boolean, using the default option (FALSE)")
        Xgrid <- FALSE
    }
    time_actual <- obj.name <- NULL
    
    obj.name <- base::deparse(base::substitute(actual))
    
    if (stats::is.ts(actual)) {
        time_actual <- stats::time(actual)
    } else if (zoo::is.zoo(actual) | xts::is.xts(actual)) {
        time_actual <- zoo::index(actual)
    }
    
    p <- plotly::plot_ly() %>% 
    plotly::add_trace(x = time_actual, y = as.numeric(actual), 
        mode = "lines+markers", name = "Actual", type = "scatter") %>% 
        plotly::add_trace(x = time_actual, y = c(forecast.obj$fitted, 
            rep(NA, length(actual) - length(train))), mode = "lines+markers", 
            name = "Fitted", type = "scatter", line = list(color = "red")) %>% 
        plotly::add_trace(x = time_actual, y = c(rep(NA, 
            length(actual) - length(test)), forecast.obj$mean), 
            mode = "lines+markers", name = "Forecasted", type = "scatter") %>% 
        plotly::layout(title = "Actual vs Forecasted and Fitted", 
            xaxis = list(title = forecast.obj$method, showgrid = Xgrid), 
            yaxis = list(title = obj.name, showgrid = Ygrid))
    
    return(p)
}


