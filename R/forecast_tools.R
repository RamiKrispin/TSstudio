#'  Visualize of the Fitted and the Forecasted vs the Actual Values
#' @export test_forecast
#' @param actual The full time series object (supports "ts", "zoo" and "xts" formats)
#' @param forecast.obj The forecast output of the training set with horizon 
#' align to the length of the testing (support forecasted objects from the “forecast” package)
#' @param train Training partition, a subset of the first n observation in the series (not requiredthed)
#' @param test The testing (hold-out) partition 
#' @param Ygrid Logic,show the Y axis grid if set to TRUE
#' @param Xgrid Logic,show the X axis grid if set to TRUE
#' @param hover If TRUE add tooltip with information about the model accuracy
#' @description Visualize the fitted values of the training set and the forecast values of the testing set against the actual values of the series
#' @examples
#' \dontrun{
#' library(forecast)
#' data(USgas)
#' 
#' # Set the horizon of the forecast
#' h <- 12
#' 
#' # split to training/testing partition
#' split_ts <- ts_split(USgas, sample.out  = h)
#' train <- split_ts$train
#' test <- split_ts$test
#'
#' # Create forecast object
#' fc <- forecast(auto.arima(train, lambda = BoxCox.lambda(train)), h = h)
#'
#' # Plot the fitted and forecasted vs the actual values
#' test_forecast(actual = USgas, forecast.obj = fc, test = test)
#'}


test_forecast <- function(actual, forecast.obj,
                          train = NULL, test, 
                          Ygrid = FALSE, Xgrid = FALSE,
                          hover = TRUE) {
  `%>%` <- magrittr::`%>%`
  # Error handling
  if (!forecast::is.forecast(forecast.obj)) {
    stop("The class of the forecast object is not \"forecast\"")
  }
  if (base::length(forecast.obj$x) + base::length(test) != base::length(actual)) {
    stop("The length of the train and test sets are different from the length of the actual set")
  }
  if (!base::is.logical(Ygrid)) {
    warning("The value of \"Ygrid\" is not boolean, using the default option (FALSE)")
    Ygrid <- FALSE
  }
  
  if (!base::is.logical(Xgrid)) {
    warning("The value of 'Xgrid' is not boolean, using the default option (FALSE)")
    Xgrid <- FALSE
  }
  
  if(!base::is.logical(hover)) {
    warning("The value of 'hover' is not boolean, using the default option (TRUE)")
    hover <- TRUE
  }
  
  time_actual <- obj.name <- NULL
  
  obj.name <- base::deparse(base::substitute(actual))
  
  if (stats::is.ts(actual)) {
    time_actual <- stats::time(actual)
  } else if (zoo::is.zoo(actual) | xts::is.xts(actual)) {
    time_actual <- zoo::index(actual)
  }
  model_accuracy <- forecast::accuracy(forecast.obj, test)
  if(hover){
    text_fit <- base::paste("Model: ", forecast.obj$method,
                            "<br> Actual: ", base::round(actual,2),
                            "<br> Fitted Value: ", c(base::round(forecast.obj$fitted, 2), 
                                                     rep(NA, base::length(actual) - 
                                                           base::length(forecast.obj$x))),
                            "<br> Training Set",
                            "<br> MAPE: ",  base::round(model_accuracy[9], 2),
                            "<br> RMSE: ",  base::round(model_accuracy[3], 2),
                            "<br> Testing Set",
                            "<br> MAPE: ",  base::round(model_accuracy[10], 2),
                            "<br> RMSE: ",  base::round(model_accuracy[4], 2)
                            
    )
    
    text_forecast <- base::paste("Model: ", forecast.obj$method,
                                 "<br> Actual: ", base::round(actual,2),
                                 "<br> Forecasted Value: ", c(rep(NA, 
                                                                  base::length(actual) - 
                                                                    base::length(test)), 
                                                              base::round(forecast.obj$mean,2)),
                                 "<br> Training Set",
                                 "<br> MAPE: ",  base::round(model_accuracy[9], 2),
                                 "<br> RMSE: ",  base::round(model_accuracy[3], 2),
                                 "<br> Testing Set",
                                 "<br> MAPE: ",  base::round(model_accuracy[10], 2),
                                 "<br> RMSE: ",  base::round(model_accuracy[4], 2)
                                 
    )
    text_hover <- "text"
  } else {
    text_fit <- " "
    text_forecast <- " "
    text_hover <- "y"
  }
  p <- plotly::plot_ly() %>% 
    plotly::add_trace(x = time_actual, 
                      y = as.numeric(actual), 
                      mode = "lines+markers", 
                      name = "Actual", 
                      type = "scatter",
                      hoverinfo = "y",
                      line = list(color = "#00526d"),
                      marker = list(color = "#00526d")
    ) %>% 
    plotly::add_trace(x = time_actual, 
                      y = c(forecast.obj$fitted, 
                            rep(NA, base::length(actual) - 
                                  base::length(forecast.obj$x))), 
                      mode = "lines+markers", 
                      name = "Fitted", 
                      type = "scatter", 
                      line = list(color = "red"),
                      marker = list(color = "red"),
                      hoverinfo = ifelse(hover, "text", "y"),
                      text = text_fit
    ) %>% 
    plotly::add_trace(x = time_actual, 
                      y = c(rep(NA, 
                                base::length(actual) - 
                                  base::length(test)), 
                            forecast.obj$mean), 
                      mode = "lines+markers", 
                      name = "Forecasted", 
                      type = "scatter",
                      hoverinfo = ifelse(hover, "text", "y"),
                      text = text_forecast,
                      marker = list(color = "green"),
                      line = list(color = "green")
    ) %>% 
    plotly::layout(title = base::paste(obj.name, " - Actual vs Forecasted and Fitted", sep = ""), 
                   xaxis = list(title = forecast.obj$method, showgrid = Xgrid), 
                   yaxis = list(title = obj.name, showgrid = Ygrid))
  
  return(p)
}


#'  Histogram Plot of the Residuals Values
#' @export 
#' @param forecast.obj A fitted or forecasted object (of the forecast package) with residuals output 
#' @description Histogram plot of the residuals values 
#' @examples
#' \dontrun{
#' library(forecast)
#' data(USgas)
#' 
#' # Set the horizon of the forecast
#' h <- 12
#' 
#' # split to training/testing partition
#' split_ts <- ts_split(USgas, sample.out  = h)
#' train <- split_ts$train
#' test <- split_ts$test
#'
#' # Create forecast object
#' fc <- forecast(auto.arima(train, lambda = BoxCox.lambda(train)), h = h)
#'
#' # Plot the fitted and forecasted vs the actual values
#' res_hist(forecast.obj = fc)
#'}


res_hist <- function(forecast.obj){
  p <- dens <- at <- NULL
  `%>%` <- magrittr::`%>%`
  # Error handling
  if(is.null(forecast.obj)){
    stop("The 'forecast.obj' is not valid parameter")
  } else{
    at <- base::attributes(forecast.obj)
    if(base::is.null(at)){
      stop("The 'forecast.obj' is not valid parameter")
    } else if(!"residuals" %in% at$names){
      stop("The 'forecast.obj' is not valid parameter")
    }
  }
  
  dens <- stats::density(forecast.obj$residuals, kernel = "gaussian")
  p <- plotly::plot_ly(x = forecast.obj$residuals, type  = "histogram", name = "Histogram") %>%
    plotly::add_trace(x = dens$x, 
                      y = dens$y, 
                      type = "scatter", 
                      mode = "lines", 
                      fill = "tozeroy", 
                      yaxis = "y2", 
                      name = "Density") %>% 
    plotly::layout(yaxis2 = list(overlaying = "y", side = "right"),
                   xaxis = list(title = "Residuals")) %>%
    plotly::hide_legend()
  
  return(p)
}


#'  Visualization of the Residuals of a Time Series Model  
#' @export 
#' @param ts.model A time series model (or forecasted) object, support any model from the forecast package with a residuals output
#' @param lag.max The maximum number of lags to display in the residuals' autocorrelation function plot
#' @description Provides a visualization of the residuals of a time series model. 
#' That includes a time series plot of the residuals, and the plots of the  
#' autocorrelation function (acf) and histogram of the residuals
#' @examples
#' library(forecast)
#' data(USgas)
#'
#' # Create a model
#' fit <- auto.arima(USgas)
#' 
#' # Check the residuals of the model
#' check_res(fit)

check_res <- function(ts.model, lag.max = 36){
  `%>%` <- magrittr::`%>%`
  method <- NULL
  # Error handling
  if(is.null(ts.model)){
    stop("The 'ts.model' is not valid parameter")
  } else{
    at <- base::attributes(ts.model)
    if(base::is.null(at)){
      stop("The 'ts.model' is not valid parameter")
    } else if(!"residuals" %in% at$names){
      stop("The 'ts.model' is not valid parameter - the 'residuals' attribute is missing")
    } else if(!"method" %in% at$names){
      try(method <- forecast::forecast(ts.model)$method, silent = TRUE)
      if(is.null(method)){
        stop("The 'ts.model' is not valid parameter - the 'method' attribute is missing")
      }
    } else {
      method <- ts.model$method
    }
  }
  
  if(base::any(base::is.na(ts.model$residuals))){
    warning("Dropping missing values from the residuals")
    res <- stats::na.omit(ts.model$residuals)
  } else {
    res <- ts.model$residuals
  }
  if(!base::is.numeric(lag.max)){
    warning("The value of 'lag.max' is not valid, using the default")
    lag.max <- 36
  }
  if(lag.max %%1 != 0){
    warning("The value of 'lag.max' is not valid, using the default")
    lag.max <- 36
  }
  
  
  p1 <- TSstudio::ts_plot(res)
  p2 <- TSstudio::ts_acf(res, lag.max = lag.max) 
  p3 <- plotly::plot_ly(x = res, type = "histogram", 
                        name = "Histogram", 
                        marker = list(color = "#00526d")
  ) %>%
    plotly::layout(xaxis = list(title = "Residuals"),
                   yaxis = list(title = "Count")
    )
  
  p <- plotly::subplot(p1,
                       plotly::subplot(p2, p3, nrows = 1, margin = 0.04,
                                       titleX =  TRUE, titleY = TRUE), 
                       titleX =  TRUE, titleY = TRUE,
                       nrows = 2, margin = 0.04
  ) %>% plotly::hide_legend() %>%
    plotly::layout(
      title =  base::paste("Residuals Plot for", method, sep = " ")
    )
  return(p)
}


#' Forecasting simulation  
#' @export forecast_sim
#' @param model A forecasting model supporting \code{\link[forecast]{Arima}}, \code{\link[forecast]{auto.arima}}, 
#' \code{\link[forecast]{ets}}, and \code{\link[forecast]{nnetar}} models from the **forecast** package
#' @param h An integer, defines the forecast horizon
#' @param n An integer, set the number of iterations of the simulation
#' @param sim_color Set the color of the simulation paths lines
#' @param opacity Set the opacity level of the simulation path lines
#' @param plot Logical, if TRUE will desplay the output plot
#' @description Creating different forecast paths for forecast objects (when applicable), 
#' by utilizing the underline model distribution with the \code{\link[stats]{simulate}} function
#' @return The baseline series, the simulated values and a plot
#' @examples
#'  \dontrun{
#' library(forecast)
#' data(USgas)
#'
#' # Create a model
#' fit <- auto.arima(USgas)
#' 
#' # Simulate 100 possible forecast path, with horizon of 60 months
#' forecast_sim(model = fit, h = 60, n = 100)
#' }

forecast_sim <- function(model,h,n, sim_color = "blue", opacity = 0.05, plot = TRUE){
  
  `%>%` <- magrittr::`%>%`
  x <- y <- NULL
  
  
  # Setting variables
  s <- s1 <- sim_output <- p <- output <- NULL
  
  #Error handling
  if(!any(class(model) %in% c("ARIMA", "ets", "nnetar", "Arima"))){
    stop("The model argument is not valid")
  }
  
  if(opacity < 0 || opacity > 1){
    stop("The value of the 'opacity' argument is invalid")
  }
  
  if(!is.numeric(n)){
    stop("The value of the 'n' argument is not valid")
  }
  
  if(!is.numeric(h)){
    stop("The value of the 'h' argument is not valid")
  } else if(h %% 1 != 0){
    stop("The 'h' argument is not integer")
  } else if(h < 1){
    stop("The value of the 'h' argument is not valid")
  }
  
  if(!base::is.logical(plot)){
    warning("The value of the 'plot' parameter is invalid, using default option TRUE")
    plot <- TRUE
  }
  
  s <- lapply(1:n, function(i){
    sim <- sim_df <- x <- y <- NULL
    
    sim <- stats::simulate(model,nsim = h)
    sim_df <- base::data.frame(x = base::as.numeric(stats::time(sim)), 
                               y = base::as.numeric(sim))
    sim_df$n <- base::paste0("sim_", i)
    return(sim_df)
  }) 
  sim_output <- s %>% dplyr::bind_rows() %>% 
    tidyr::spread(key = n, value = y) %>% 
    dplyr::select(-x) %>% 
    stats::ts(start = stats::start(stats::simulate(model,nsim = 1)), 
              frequency = stats::frequency(stats::simulate(model,nsim = 1))) 
  
  p <- plotly::plot_ly()
  
  for(i in 1:n){
    p <- p %>% plotly::add_lines(x = s[[i]]$x, y = s[[i]]$y, 
                                 line = list(color = sim_color), 
                                 opacity = opacity, showlegend = FALSE, 
                                 name = paste("Sim", i, sep = " "))
  }
  s1 <- s %>% dplyr::bind_rows() %>% dplyr::group_by(x) %>%
    dplyr::summarise(p50 = stats::median(y))
  p <- p %>% plotly::add_lines(x = s1$x, y = s1$p50, 
                               
                               line = list(color = "#00526d", 
                                           dash = "dash", 
                                           width = 3), name = "Median") 
  
  p <- p %>% plotly::add_lines(x = stats::time(model$x), 
                               y = model$x, 
                               line = list(color = "#00526d"), 
                               name = "Actual")
  if(plot){
    print(p)
  }
  
  output <- list()
  output$plot <- p
  output$forecast_sim <- sim_output
  output$series <- model$x
  return(output)
}

#' Diagnostic Plots for ARIMA Models
#' @export
#' @param ts.obj A ts object
#' @param method A list, defines the transformation parameters of each plot. 
#' Each plot should be defined by a list, where the name of the list defines the plot ID. 
#' The plot parameters are:
#' 
#' diff - an integer, defines the degree of diffrence
#' log - a boolean, defines if log transformation should be used
#' title - optional, the plot title
#' @param cor A boolean, if TRUE (default), will plot the series ACF and PACF
#' @details The arima_diag function provides a set of diagnostic plots for identify the ARIMA model parameters.
#' The ACF and PACF can assist in identifying the AR and MA process, 
#' and the diffrence plotting hel in idenitfying the degree of differencing that required to make the series stationary
#' @return A plot
#' 
#' @examples 
#' 
#' data(USgas)
#' 
#' arima_diag(ts.obj = USgas)
#' 
#' # Can define more than one differencing plot using the 'method' argument
#' 
#' arima_diag(ts.obj = USgas,
#'            cor = TRUE,
#'            method = list(first = list(diff = 1, 
#'                                       log = TRUE,
#'                                       title = "First Diff with Log Transformation"),
#'                          Second = list(diff = c(1,1),
#'                                        log = TRUE,
#'                                        title = "Second Diff with Log Transformation")))

arima_diag <- function(ts.obj, method = list(first = list(diff = 1, log = TRUE, title = "First Difference with Log Transformation")), cor = TRUE){
  `%>%` <- magrittr::`%>%`
  obj.name <- base::deparse(base::substitute(ts.obj))
  
  plot_obj <- function(input, 
                       obj.name = NULL, 
                       color = "#00526d", 
                       annotations = NULL,
                       ann_x = 0.05,
                       ann_y = 0.9){
    p <- plotly::plot_ly(x = stats::time(input) + stats::deltat(input),
                         y = base::as.numeric(input),
                         type = "scatter",
                         mode = "lines",
                         name = obj.name,
                         line = list(color = "#00526d"),
                         showlegend = FALSE) 
    
    if(!base::is.null(annotations)){
      p <- p %>% plotly::layout(annotations = list(text = annotations, 
                                             xref = "paper", 
                                             yref = "paper",
                                             yanchor = "bottom",
                                             xanchor = "center",
                                             align = "cneter",
                                             x = ann_x,
                                             y = ann_y, 
                                             showarrow = FALSE,
                                             font = list(size = 12)))
    }
      
    return(p)
  }
  
  p1 <- plot_obj(input = ts.obj, annotations = obj.name)
  if(cor){
    lag.max <- ifelse(stats::frequency(ts.obj) * 3 > base::length(ts.obj), base::length(ts.obj), stats::frequency(ts.obj) * 3)
    p2 <- TSstudio::ts_cor(ts.obj = ts.obj, type = "both", lag.max = lag.max)
  }
  
  
  if(!base::is.null(method)){
    diff_obj <- diff_method <- diff_plot <- NULL
    diff_method <- base::names(method)
    
    diff_plot <- lapply(diff_method, function(i){
      if(!"diff" %in% base::names(method[[i]])){
        stop("Error on the 'method' argument: the 'diff' argument is missing")
      } else if(!"log" %in% base::names(method[[i]])){
        log <- FALSE
      }
      diff_obj <- ts.obj
      if(method[[i]]$log){
        diff_obj <- base::log(diff_obj)
      }
      
      for(d in method[[i]]$diff){
        diff_obj <- base::diff(diff_obj, d)
      }
      
      return(plot_obj(input = diff_obj, annotations =  method[[i]]$title, ann_x = 0.25))
      
      
    })
    output <- plotly::subplot(p1, p2, 
                              plotly::subplot(diff_plot, nrows = base::length(diff_plot), titleY = TRUE, margin = 0.1), 
                              nrows = 3, 
                              titleY = TRUE,
                              margin = 0.04)
  } else {
    output <- plotly::subplot(p1, p2, 
                              nrows = 2,
                              margin = 0.04)
  }
  
  output <- output %>% plotly::layout(title = base::paste("ARIMA Diagnostic Plot - ", obj.name, sep = ""))
  
  
  return(output)
  
}



