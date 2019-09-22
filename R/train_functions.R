#'  Evaluation Function for Forecasting Models
#' @export ts_backtesting
#' @param ts.obj A univariate time series object of a class "ts"
#' @param models String, define the type of models to use in the training function:
#' 
#'  'a' - auto.arima (forecast package)
#'  
#'  'b' - Bayesian Structural Time Series (bsts package)
#'  
#'  'e' - ets (forecast package) 
#'  
#'  'h' - hybrid timse series model (forecastHybrid package) 
#'  
#'  'n' - Neural Network Time Series (forecast package)
#'  
#'  't' - tbats (forecast package)
#'  
#'  'w' - Holt Winters (stats package)
#'  
#' @param periods The number of periods to evaluate the models (with a minimum of 2)
#' @param error The type of error to evaluate by - "MAPE"  (default) or "RMSE"
#' @param window_size An integer, the size of the backtesting window
#' @param h Integer, the horizon of the selected forecasting model
#' @param plot Logical, if TRUE desplay a plot with the backtesting progress
#' @param a.arg A list, an optional arguments to pass to the \code{\link[forecast]{auto.arima}} function
#' @param b.arg A list, an optional arguments to pass to the \code{\link[bsts]{bsts}} function 
#' @param e.arg A list, an optional argument to pass to the \code{\link[forecast]{ets}} function
#' @param h.arg A list, an optional argument to pass to the \code{\link[forecastHybrid]{hybridModel}} function
#' @param n.arg A list, an optional argument to pass to the \code{\link[forecast]{nnetar}} function
#' @param t.arg A list, an optional arguments to pass to the \code{\link[forecast]{tbats}} function
#' @param w.arg A list, an optional arguments to pass to the \code{\link[stats]{HoltWinters}} function
#' @param parallel Logical, if TRUE use parallel option when applicable (auto.arima, hybridModel)
#' @param xreg.h A data.frame or matrix, optional argument, 
#' set the futuer values external regressors in case using the 
#' 'xreg' argument in one of the models (auto.arima, nnetar, hybrid)
#' @description Performance evaluation function for forecasting models, by training and testing the performance
#' of each model over a sequence of periods to identify the performance of a model over time  
#' (both accuracy and stability)
#' @examples
#' \dontrun{
#' data(USgas)
#' USgas_backtesting <- ts_backtesting(USgas, 
#'                                     periods = 6, 
#'                                     window_size = 24, 
#'                                     h = 60, 
#'                                     error = "RMSE")
#' 
#' # Selecting a specific models (auto.arima, ets and nnetar)
#' USgas_backtesting <- ts_backtesting(USgas, 
#'                                     models = "aen", 
#'                                     periods = 6, 
#'                                     window_size = 24, 
#'                                     h = 60)
#'  
#' # Retrieve the models leaderboard
#' USgas_backtesting$leaderboard
#' 
#'
#' # Retrieve the best forecast results
#' USgas_backtesting$leadForecast$mean
#' 
#' # Retrieve the final forecast of the ets model
#' USgas_backtesting$Forecast_Final$ets$mean
#' 
#' # Retrieve the ets forecast during the first period of testing
#' USgas_backtesting$period_1$ets$forecast$mean
#' 
#' # Get the final plot of the models performance and the selected forecasting model
#' USgas_backtesting$summary_plot
#' }

ts_backtesting <- function(ts.obj, 
                           models = "abehntw", 
                           periods = 6, 
                           error = "MAPE", 
                           window_size = 3,
                           h = 3,
                           plot = TRUE,
                           a.arg = NULL,
                           b.arg = NULL,
                           e.arg = NULL,
                           h.arg = NULL,
                           n.arg = NULL,
                           t.arg = NULL,
                           w.arg = NULL,
                           xreg.h = NULL,
                           parallel = FALSE){
  
  `%>%` <- magrittr::`%>%` 
  
  a <- model_list <- model_char <- color_ramp <- forecast_list <- obj.name <- NULL
  variable <- value <- avgMAPE <- avgRMSE <- NULL
  obj.name <- base::deparse(base::substitute(ts.obj))
  
  
  # Define the model type
  for(s in 1:nchar(models)){
    if(!substr(models, s, s) %in% c("a", "w", "e", "n", "t", "b", "h")){
      stop("The 'models' argument is not valide")
    }
  }
  
  # Error handling
  # Check if xreg argument is valid
  if(!base::is.null(xreg.h)){
    if(!"xreg" %in% names(a.arg) &
       !"xreg" %in% names(n.arg) &
       !"xreg" %in% names(h.arg$a.args) &
       !"xreg" %in% names(h.arg$n.args) &
       !"xreg" %in% names(h.arg$s.args)){
      warning("There is no 'xreg' argument in any of the models arguments,", 
              "'xreg.h' will be ignored")
    } else {
      if(base::nrow(xreg.h) != h){
        stop("The length of the 'xreg.h' argument is not equal to the forecast horizon")
      }
    }
  }
  
  # Check the xreg in a.arg is valid (if exists)
  if("xreg" %in% names(a.arg)){
    xreg.arima <- NULL
    xreg.arima <- a.arg$xreg
    if(base::nrow(xreg.arima) != base::length(ts.obj)){
      stop("The length of the 'xreg' in the 'a.arg' argument is not equal to the series length")
    }
  }  
  
  if("xreg" %in% names(n.arg)){
    xreg.nnetar <- NULL
    xreg.nnetar <- n.arg$xreg
    if(base::nrow(xreg.nnetar) != base::length(ts.obj)){
      stop("The length of the 'xreg' in the 'n.arg' argument is not equal to the series length")
    }
  }  
  
  if("xreg" %in% names(h.arg$a.args)){
    xreg.hybrid.arima <- NULL
    xreg.hybrid.arima <- h.arg$a.args$xreg
    if(base::nrow(xreg.hybrid.arima) != base::length(ts.obj)){
      stop("The length of the 'xreg' of the auto.arima model in the 'h.arg' argument is not equal to the series length")
    }
  }  
  
  if("xreg" %in% names(h.arg$n.args)){
    xreg.hybrid.nnetar <- NULL
    xreg.hybrid.nnetar <- h.arg$n.args$xreg
    if(base::nrow(xreg.hybrid.nnetar) != base::length(ts.obj)){
      stop("The length of the 'xreg' of the nnetar model in the 'h.arg' argument is not equal to the series length")
    }
  }  
  
  if("xreg" %in% names(h.arg$s.args)){
    xreg.hybrid.stlm <- NULL
    xreg.hybrid.stlm <- h.arg$s.args$xreg
    if(base::nrow(xreg.hybrid.stlm) != base::length(ts.obj)){
      stop("The length of the 'xreg' of the stlm model in the 'h.arg' argument is not equal to the series length")
    }
  }  
  
  if(!base::is.numeric(periods) | periods != base::round(periods) | periods <= 0){
    stop("The value of the 'periods' parameters is no valid")
  } else {
    if((base::length(ts.obj) - periods - window_size) < 2 * stats::frequency(ts.obj)){
      stop("The length of the series is long enough to create a forecast")
    }
  }
  
  if(!base::is.numeric(window_size) | window_size != base::round(window_size) | window_size <= 0){
    stop("The value of the 'window_size' parameters is no valid")
  } else {
    if((base::length(ts.obj) - periods - window_size) < 2 * stats::frequency(ts.obj)){
      stop("The length of the series is long enough to create a forecast")
    }
  }
  
  if (stats::is.ts(ts.obj)) {
    if (stats::is.mts(ts.obj)) {
      warning("The 'ts.obj' has multiple columns, only the first column will be plot")
      ts.obj <- ts.obj[, 1]
    }
  }else {
    stop("The 'ts.obj' is not a 'ts' class")
  }
  
  if(!error %in% c("MAPE", "RMSE")){
    warning("The value of the 'error' parameter is invalid, using the default setting - 'MAPE'")
    error <- "MAPE"
  } 
  
  if(!base::is.logical(plot)){
    warning("The value of the 'plot' parameter is invalid, using default option TRUE")
    plot <- TRUE
  }
  
  
  # Setting the output object
  modelOutput <- list()
  
  # Define the plot colors
  if(base::nchar(models) < 3){
    color_ramp <- RColorBrewer::brewer.pal(3,"Dark2")[1:base::nchar(models)]
  } else{
    color_ramp <- RColorBrewer::brewer.pal(base::nchar(models),"Dark2")
  }
  model_char <-  base::unlist(base::strsplit(models, split = ""))
  modelOutput$Models_Final <- list()
  modelOutput$Forecast_Final <- list()
  
  # Final forecast
  if("a" %in% model_char){
    model_list <- c(model_list, "auto.arima")
    md_auto.arima <- fc_auto.arima <- NULL
    a.arg$parallel <- parallel
    md_auto.arima <- base::do.call(forecast::auto.arima, c(list(ts.obj), a.arg))
    
    if("xreg" %in% base::names(a.arg)){
      fc_auto.arima <- forecast::forecast(md_auto.arima, h = h, xreg = xreg.h)
    } else{
      fc_auto.arima <- forecast::forecast(md_auto.arima, h = h)
    }
    
    modelOutput$Models_Final$auto.arima <- md_auto.arima
    modelOutput$Forecast_Final$auto.arima <- fc_auto.arima
    
  }
  
  if("w" %in% model_char){
    model_list <- c(model_list, "HoltWinters")
    md_HoltWinters <- fc_HoltWinters <- NULL
    md_HoltWinters <- base::do.call(stats::HoltWinters, c(list(ts.obj), w.arg))
    fc_HoltWinters <- forecast::forecast(md_HoltWinters, h = h)
    modelOutput$Models_Final$HoltWinters <- md_HoltWinters
    modelOutput$Forecast_Final$HoltWinters <- fc_HoltWinters
  }
  
  if("e" %in% model_char){
    model_list <- c(model_list, "ets")
    md_ets <- fc_ets <- NULL
    md_ets <- base::do.call(forecast::ets, c(list(ts.obj), e.arg))
    fc_ets <- forecast::forecast(md_ets, h = h)
    modelOutput$Models_Final$ets <- md_ets
    modelOutput$Forecast_Final$ets <- fc_ets
  }
  
  if("n" %in% model_char){
    model_list <- c(model_list, "nnetar")
    md_nnetar <- fc_nnetar <- NULL
    md_nnetar <- base::do.call(forecast::nnetar, c(list(ts.obj), n.arg))
    if("xreg" %in% base::names(n.arg)){
      fc_nnetar <- forecast::forecast(md_nnetar, h = h, xreg = xreg.h)
    } else{
      fc_nnetar <- forecast::forecast(md_nnetar, h = h)
    }
    modelOutput$Models_Final$nnetar <- md_nnetar
    modelOutput$Forecast_Final$nnetar <- fc_nnetar
  }
  
  if("t" %in% model_char){
    model_list <- c(model_list, "tbats")
    md_tbats <- fc_tbats <- NULL
    t.arg$use.parallel <- parallel
    md_tbats <- base::do.call(forecast::tbats, c(list(ts.obj), t.arg))
    fc_tbats <- forecast::forecast(md_tbats, h = h)
    modelOutput$Models_Final$tbats <- md_tbats
    modelOutput$Forecast_Final$tbats <- fc_tbats
  }
  
  if("b" %in% model_char){
    
    # Check if the bsts arguments are valid
    if(is.null(b.arg)){
      b.arg <-  list(linear_trend = TRUE,
                     seasonal = TRUE,
                     niter = 1000,
                     ping = 0,
                     family = "gaussian",
                     seed=1234)
    } else{
      
      if("linear_trend" %in% names(b.arg)){
        if(!b.arg$linear_trend %in% c(TRUE, FALSE)){
          warning("The value of the 'linear_trend' argument of the bsts model is invalid, using default (TRUE)")
          b.arg$linear_trend <- TRUE
        }
      } else {
        warning("The 'linear_trend' was not defined, using TRUE as default")
        b.arg$linear_trend <- TRUE
      }
      
      if("seasonal" %in% names(b.arg)){
        if(!b.arg$seasonal %in% c(TRUE, FALSE)){
          warning("The value of the 'seasonal' argument of the bsts model is invalid, using TRUE as default")
          b.arg$seasonal <- TRUE 
        } 
      } else {
        warning("The 'seasonal' argument was not defined, using TRUE as default")
        b.arg$seasonal <- TRUE
      }
      
      if("niter" %in% names(b.arg)){
        if(!base::is.numeric(b.arg$niter)){
          warning("The value of the 'niter' argument of the bsts model is invalid, setting the argument to 1000")
          b.arg$niter <- 1000 
        } else if(b.arg$niter %% 1 != 0){
          warning("The value of the 'niter' argument of the bsts model is not integer, setting the argument to 1000")
          b.arg$niter <- 1000 
        }
      } else {
        warning("The 'niter' argument was not defined, setting the argument to 1000")
        b.arg$niter <- 1000
      }
      
      if("ping" %in% names(b.arg)){
        if(!base::is.numeric(b.arg$ping)){
          warning("The value of the 'ping' argument of the bsts model is invalid, setting the argument to 100")
          b.arg$ping <- 100 
        } else if(b.arg$ping %% 1 != 0){
          warning("The value of the 'ping' argument of the bsts model is not integer, setting the argument to 100")
          b.arg$ping <- 1000 
        }
      } else {
        warning("The 'ping' argument was not defined, setting the argument to 100")
        b.arg$ping <- 100
      }
      
      if("seed" %in% names(b.arg)){
        if(!base::is.numeric(b.arg$seed)){
          warning("The value of the 'seed' argument of the bsts model is invalid, setting the argument to 1234")
          b.arg$seed <- 1234 
        } else if(b.arg$seed %% 1 != 0){
          warning("The value of the 'seed' argument of the bsts model is not integer, setting the argument to 1234")
          b.arg$seed <- 1234 
        }
      } else {
        warning("The 'seed' argument was not defined, setting the argument to 1234")
        b.arg$seed <- 1234
      }
      
      
      if("family" %in% names(b.arg)){
        if(!b.arg$family %in% c("gaussian", "logit", "poisson", "student")){
          warning("The value of the 'family' argument of the bsts model is invalid, using 'gaussian' as default")
          b.arg$family <- "gaussian"
        }
      } else{
        warning("The value of the 'family' argument is missing, using 'gaussian' as default")
        b.arg$family <- "gaussian"
      }
      
      
    }
    
    
    model_list <- c(model_list, "bsts")
    md_bsts <- fc_bsts <- ss <- fit.bsts <- burn <-  NULL
    ss <- list()
    if(b.arg$linear_trend){
      ss <- bsts::AddLocalLinearTrend(ss, ts.obj) 
    }
    if(b.arg$seasonal){
      ss <- bsts::AddSeasonal(ss, ts.obj, 
                              nseasons = stats::frequency(ts.obj))
    }
    
    md_bsts <- bsts::bsts(ts.obj, 
                          state.specification = ss, 
                          niter = b.arg$niter, 
                          ping= b.arg$ping, 
                          seed= b.arg$seed,
                          family = b.arg$family)
    fc_bsts <- stats::predict(md_bsts, horizon = h, quantiles = c(.025, .975))
    modelOutput$Models_Final$bsts <- md_bsts
    modelOutput$Forecast_Final$bsts <- fc_bsts
  }
  
  if("h" %in% model_char){
    model_list <- c(model_list, "hybrid")
    md_hybrid <- fc_hybrid <- NULL
    h.arg$parallel <- parallel
    md_hybrid <- base::do.call(forecastHybrid::hybridModel, c(list(ts.obj), h.arg))
    
    
    if("xreg" %in% names(h.arg$a.args) ||
       "xreg" %in% names(h.arg$n.args) ||
       "xreg" %in% names(h.arg$s.args)){
      fc_hybrid <- forecast::forecast(md_hybrid, h = h, xreg = base::as.data.frame(xreg.h))
    } else{
      fc_hybrid <- forecast::forecast(md_hybrid, h = h)
    }
    
    modelOutput$Models_Final$hybrid <- md_hybrid
    modelOutput$Forecast_Final$hybrid <- fc_hybrid
  }
  
  
  s <- length(ts.obj) - periods + 1
  e <- length(ts.obj)
  MAPE_df <- NULL
  MAPE_df <- base::data.frame(matrix(NA, ncol = length(model_list) + 1 , nrow = periods))
  names(MAPE_df) <- c("Period", model_list)
  MAPE_df$Period <- s:e - s + 1
  
  RMSE_df <- NULL
  RMSE_df <- base::data.frame(matrix(NA, ncol = length(model_list) + 1 , nrow = periods))
  names(RMSE_df) <- c("Period", model_list)
  RMSE_df$Period <- s:e - s + 1
  
  
  # Loop over the series
  for(i in s:e){
    period_name <- NULL
    
    period_name <- paste("period", (i - s + 1), sep = "_")
    eval(parse(text = paste("modelOutput$", period_name, "<- list()", sep = ""))) 
    
    ts.subset <- split_ts <- train <- test <- NULL
    ts.subset <- stats::window(ts.obj, start = stats::time(ts.obj)[1], end = stats::time(ts.obj)[i])
    split_ts <- TSstudio::ts_split(ts.subset, sample.out = window_size)
    train <- split_ts$train
    test <- split_ts$test
    
    if("a" %in% model_char){
      md <- fc <- NULL
      if("xreg" %in% names(a.arg)){
        a.xreg.train <- xreg.arima[1:length(train),]
        a.xreg.test <- xreg.arima[(length(train) + 1):(length(train) + window_size),]
        a.arg.xreg <- a.arg
        a.arg.xreg$xreg <- a.xreg.train
        md <- base::do.call(forecast::auto.arima, c(list(train), a.arg.xreg))
        fc <- forecast::forecast(md, h = window_size, xreg = a.xreg.test)
      } else {
        md <- base::do.call(forecast::auto.arima, c(list(train), a.arg))
        fc <- forecast::forecast(md, h = window_size)
      }
      MAPE_df$auto.arima[i - s + 1] <-  base::round(forecast::accuracy(fc,test)[10], 2)
      RMSE_df$auto.arima[i - s + 1] <-  base::round(forecast::accuracy(fc,test)[4], 2)
      eval(parse(text = paste("modelOutput$", period_name, "$auto.arima <- list(model = md, forecast = fc)", sep = ""))) 
    }
    
    if("w" %in% model_char){
      md <- fc <- NULL
      md <- base::do.call(stats::HoltWinters, c(list(train), w.arg))
      fc <- forecast::forecast(md, h = window_size)
      MAPE_df$HoltWinters[i - s + 1] <- base::round(forecast::accuracy(fc, test)[10], 2)
      RMSE_df$HoltWinters[i - s + 1] <- base::round(forecast::accuracy(fc, test)[4], 2)
      eval(parse(text = paste("modelOutput$", period_name, "$HoltWinters <- list(model = md, forecast = fc)", sep = ""))) 
    }
    
    if("e" %in% model_char){
      md <- fc <- NULL
      md <- base::do.call(forecast::ets, c(list(train), e.arg))
      fc <- forecast::forecast(train, h = window_size)
      MAPE_df$ets[i - s + 1] <-  base::round(forecast::accuracy(fc, test)[10], 2)
      RMSE_df$ets[i - s + 1] <-  base::round(forecast::accuracy(fc, test)[4], 2)
      eval(parse(text = paste("modelOutput$", period_name, "$ets <- list(model = md, forecast = fc)", sep = "")))
    }
    
    
    if("n" %in% model_char){
      md <- fc <- NULL
      if("xreg" %in% names(n.arg)){
        n.xreg.train <- xreg.arima[1:length(train),]
        n.xreg.test <- xreg.arima[(length(train) + 1):(length(train) + window_size),]
        n.arg.xreg <- n.arg
        n.arg.xreg$xreg <- n.xreg.train
        md <- base::do.call(forecast::nnetar, c(list(train), n.arg.xreg))
        fc <- forecast::forecast(md, h = window_size, xreg = n.xreg.test)
      } else {
        md <- base::do.call(forecast::nnetar, c(list(train), n.arg))
        fc <- forecast::forecast(md, h = window_size)
      }
      
      
      MAPE_df$nnetar[i - s + 1] <-  base::round(forecast::accuracy(fc, test)[10],2)
      RMSE_df$nnetar[i - s + 1] <-  base::round(forecast::accuracy(fc, test)[4],2)
      eval(parse(text = paste("modelOutput$", period_name, "$nnetar <- list(model = md, forecast = fc)", sep = "")))
    }
    
    if("t" %in% model_char){
      md <- fc <- NULL
      md <- base::do.call(forecast::tbats, c(list(train), t.arg))
      fc <- forecast::forecast(md, h = window_size)
      MAPE_df$tbats[i - s + 1] <-  base::round(forecast::accuracy(fc, test)[10], 2)
      RMSE_df$tbats[i - s + 1] <-  base::round(forecast::accuracy(fc, test)[4], 2)
      eval(parse(text = paste("modelOutput$", period_name, "$tbats <- list(model = md, forecast = fc)", sep = "")))
    }
    
    if("b" %in% model_char){
      md <- fc <- ss <-  NULL
      ss <- list()
      if(b.arg$linear_trend){
        ss <- bsts::AddLocalLinearTrend(ss, ts.obj) 
      }
      if(b.arg$seasonal){
        ss <- bsts::AddSeasonal(ss, ts.obj, 
                                nseasons = stats::frequency(ts.obj))
      }
      
      md <- bsts::bsts(train, 
                       state.specification = ss, 
                       niter = b.arg$niter, 
                       ping= b.arg$ping, 
                       seed= b.arg$seed,
                       family = b.arg$family)
      
      fc <- stats::predict(md, horizon = window_size, quantiles = c(.025, .975))
      
      
      pred <- fc$mean
      MAPE_df$bsts[i - s + 1] <- base::round(mean(100 * base::abs((test - pred) / test)), 2)
      RMSE_df$bsts[i - s + 1] <- base::round((mean((test - pred)^ 2)) ^ 0.5, 2)
      eval(parse(text = paste("modelOutput$", period_name, "$bsts <- list(model = md, forecast = fc)", sep = "")))
    }
    
    if("h" %in% model_char){
      md <- fc <- NULL
      
      if("xreg" %in% names(h.arg$a.args) ||
         "xreg" %in% names(h.arg$n.args) ||
         "xreg" %in% names(h.arg$s.args)){
        h.arg.xreg <- h.test <-  NULL
        h.arg.xreg <- h.arg
        if("xreg" %in% names(h.arg$a.args)){
          h.arg.xreg$a.args$xreg <- xreg.hybrid.arima[1:length(train),]
          h.test <- xreg.hybrid.arima[(length(train) + 1):(length(train) + window_size),]
        }
        
        if("xreg" %in% names(h.arg$n.args)){
          h.arg.xreg$n.args$xreg <- xreg.hybrid.nnetar[1:length(train),]
          h.test <- xreg.hybrid.nnetar[(length(train) + 1):(length(train) + window_size),]
        }
        
        if("xreg" %in% names(h.arg$s.args)){
          h.arg.xreg$s.args$xreg <- xreg.hybrid.stlm[1:length(train),]
          h.test <- xreg.hybrid.stlm[(length(train) + 1):(length(train) + window_size),]
        }
        
        md <- base::do.call(forecastHybrid::hybridModel, c(list(train), h.arg.xreg))
        fc <- forecast::forecast(md, h = window_size, xreg = base::as.data.frame(h.test))
      } else {
        md <- base::do.call(forecastHybrid::hybridModel, c(list(train), h.arg))
        fc <- forecast::forecast(md, h = window_size)
      }
      
      eval(parse(text = paste("modelOutput$", period_name, "$hybrid <- list(model = md, forecast = fc)", sep = "")))
      MAPE_df$hybrid[i - s + 1] <-  base::round(forecast::accuracy(fc, test)[10], 2)
      RMSE_df$hybrid[i - s + 1] <-  base::round(forecast::accuracy(fc, test)[4], 2)
    }
    
    if((i -s + 1) >= 1){
      p <- p1 <- p2 <- p3 <- p4 <- p5 <- p6 <-NULL
      
      
      p <-   base::suppressWarnings(plotly::plot_ly(x = stats::time(train), y = base::as.numeric(train), mode = "lines", name = "Training", type = "scatter", line = list(color = "#00526d")) %>%
                                      plotly::add_lines(x = stats::time(test), y = base::as.numeric(test), line = list(color = "green", width = 4, dash = "dash"), name = "Testing") %>% 
                                      plotly::layout(xaxis = list(range = c(base::min(stats::time(ts.obj)), base::max(stats::time(ts.obj)))), 
                                                     title = base::paste(obj.name, " Backtesting - Error Distribution by Period/Model", sep = ""), annotations = a)) 
      
      p1 <- base::suppressWarnings(plotly::plot_ly(data = MAPE_df)) 
      
      for(r1 in 2:ncol(MAPE_df)){
        p1 <- base::suppressWarnings(p1 %>% plotly::add_lines(x = MAPE_df[, 1], 
                                                              y = MAPE_df[, r1], 
                                                              name = names(MAPE_df)[r1], 
                                                              line = list(color = color_ramp[(r1 -1)])))
      }
      
      
      p1 <- base::suppressWarnings(p1 %>% plotly::layout(xaxis = list(tickvals = MAPE_df[, 1], ticktext = MAPE_df[, 1],
                                                                      range = c(min(MAPE_df$Period), max(MAPE_df$Period)))))
      
      p2 <- base::suppressWarnings(plotly::plot_ly(data = MAPE_df))
      
      for(r2 in 2:base::ncol(MAPE_df)){
        p2 <- base::suppressWarnings(p2 %>% plotly::add_trace(y = MAPE_df[, r2], 
                                                              type = "box", 
                                                              boxpoints = "all", 
                                                              jitter = 0.3,
                                                              pointpos = -1.8, 
                                                              name =  names(MAPE_df)[r2], 
                                                              marker = list(color = color_ramp[(r2 -1)]),
                                                              line = list(color = color_ramp[(r2 -1)]),
                                                              showlegend=F
        ))
      }
      
      p1 <- base::suppressWarnings(p1 %>% plotly::layout(title = "Error by Period",
                                                         yaxis = list(title = "MAPE"),
                                                         xaxis = list(title = "Period", tickvals = MAPE_df[, 1], ticktext = MAPE_df[, 1])))
      p2 <- base::suppressWarnings(p2 %>% plotly::layout(title = "Error Distribution by Model",
                                                         yaxis = list(title = "MAPE")))
      p3 <- base::suppressWarnings(plotly::subplot(p1, p2, nrows = 2, titleY = TRUE, titleX = TRUE, margin = 0.06))
      
      p4 <- base::suppressWarnings(plotly::plot_ly(data = RMSE_df)) 
      
      for(r1 in 2:ncol(RMSE_df)){
        p4 <- base::suppressWarnings(p4 %>% plotly::add_lines(x = RMSE_df[, 1], 
                                                              y = RMSE_df[, r1], 
                                                              name = names(RMSE_df)[r1], 
                                                              line = list(color = color_ramp[(r1 -1)])))
      }
      
      p4 <- base::suppressWarnings(p4 %>% plotly::layout(xaxis = list(tickvals = RMSE_df[, 1], ticktext = RMSE_df[, 1],
                                                                      range = c(min(RMSE_df$Period), max(RMSE_df$Period)))))
      
      p5 <- base::suppressWarnings(plotly::plot_ly(data = RMSE_df))
      
      for(r2 in 2:base::ncol(RMSE_df)){
        p5 <- base::suppressWarnings(p5 %>% plotly::add_trace(y = RMSE_df[, r2], 
                                                              type = "box", 
                                                              boxpoints = "all", 
                                                              jitter = 0.3,
                                                              pointpos = -1.8, 
                                                              name =  names(RMSE_df)[r2], 
                                                              marker = list(color = color_ramp[(r2 -1)]),
                                                              line = list(color = color_ramp[(r2 -1)]),
                                                              showlegend=F
        ))
      }
      
      p4 <- base::suppressWarnings(p4 %>% plotly::layout(title = "Error by Period",
                                                         yaxis = list(title = "RMSE"),
                                                         xaxis = list(title = "Period", tickvals = RMSE_df[, 1], ticktext = RMSE_df[, 1])))
      p5 <- base::suppressWarnings(p5 %>% plotly::layout(title = "Error Distribution by Model",
                                                         yaxis = list(title = "RMSE")))
      p6 <- base::suppressWarnings(plotly::subplot(p4, p5, nrows = 2, titleY = TRUE, titleX = TRUE, margin = 0.1))
      
      if(error == "MAPE" & plot & periods > 1){
        
        p7 <- base::suppressWarnings(plotly::subplot(plotly::subplot(p1, p2, nrows = 1, titleY = TRUE, shareY = TRUE, margin = 0.02, titleX = TRUE), 
                                                     p, nrows = 2, margin = 0.08, titleY = TRUE))
        print(p7)
      } else if(error == "RMSE" & plot & periods > 1){
        p7 <- base::suppressWarnings(plotly::subplot(plotly::subplot(p4, p5, nrows = 1, titleY = TRUE, shareY = TRUE, margin = 0.02, titleX = TRUE), 
                                                     p, nrows = 2, margin = 0.08, titleY = TRUE))
        print(p7)
      }
    }
  }
  
  
  
  
  
  modelOutput$MAPE_score <- MAPE_df
  modelOutput$RMSE_score <- RMSE_df
  if(periods > 1){
    modelOutput$MAPE_plot <- p3
    modelOutput$RMSE_plot <- p6
  }
  
  leaderboard <- base::suppressMessages(
    (modelOutput$MAPE_score %>% reshape2::melt(id.vars = c("Period")) %>%
       dplyr::group_by(variable) %>%
       dplyr::summarise(avgMAPE = base::mean(value),
                        sdMAPE = stats::sd(value))) %>%
      dplyr::left_join(
        modelOutput$RMSE_score %>% reshape2::melt(id.vars = c("Period")) %>%
          dplyr::group_by(variable) %>%
          dplyr::summarise(avgRMSE = base::mean(value),
                           sdRMSE = stats::sd(value)) 
      )
  )
  
  names(leaderboard)[1] <- "Model_Name"
  modelOutput$leaderboard <- leaderboard
  
  
  forecast_final_plot_arg <- list(
    text = paste(obj.name, " Best Forecast by ", error, " - ", leaderboard$Model_Name[1], sep = ""),
    xref = "paper",
    yref = "paper",
    yanchor = "bottom",
    xanchor = "center",
    align = "center",
    x = 0.5,
    y = 1,
    showarrow = FALSE
  )
  
  if(error == "MAPE"){
    leaderboard <- leaderboard %>% dplyr::arrange(avgMAPE)
    eval(parse(text = paste("modelOutput$leadForecast <- modelOutput$Forecast_Final$", leaderboard$Model_Name[1], sep = ""))) 
    if(periods > 1){
      forecast_final_plot_arg <- list(
        text = paste(obj.name, " Best Forecast by ", error, " - ", leaderboard$Model_Name[1], sep = ""),
        xref = "paper",
        yref = "paper",
        yanchor = "bottom",
        xanchor = "center",
        align = "center",
        x = 0.5,
        y = 1,
        showarrow = FALSE
      )
      
      final_forecast_plot <- base::suppressWarnings(TSstudio::plot_forecast(modelOutput$leadForecast) %>% 
                                                      plotly::layout(annotations = forecast_final_plot_arg, 
                                                                     title = base::paste(obj.name, " Backtesting - Error Distribution by Period/Model", sep = "")))
      final_plot <- base::suppressWarnings(plotly::subplot(plotly::subplot(p1, p2, nrows = 1, titleY = TRUE, shareY = TRUE, margin = 0.02, titleX = TRUE), 
                                                           final_forecast_plot, nrows = 2, margin = 0.1, titleY = TRUE))
      
    }
    leaderboard <- leaderboard %>% dplyr::arrange(avgMAPE) %>% as.data.frame()
    modelOutput$leaderboard <- leaderboard
  } else if(error == "RMSE"){
    leaderboard <- leaderboard %>% dplyr::arrange(avgRMSE)
    eval(parse(text = paste("modelOutput$leadForecast <- modelOutput$Forecast_Final$", leaderboard$Model_Name[1], sep = ""))) 
    
    if(periods > 1){
      forecast_final_plot_arg <- list(
        text = paste(obj.name, " Best Forecast by ", error, " - ", leaderboard$Model_Name[1], sep = ""),
        xref = "paper",
        yref = "paper",
        yanchor = "bottom",
        xanchor = "center",
        align = "center",
        x = 0.5,
        y = 1,
        showarrow = FALSE
      )
      final_forecast_plot <- base::suppressWarnings(TSstudio::plot_forecast(modelOutput$leadForecast) %>% 
                                                      plotly::layout(annotations = forecast_final_plot_arg))
      final_plot <- base::suppressWarnings(plotly::subplot(plotly::subplot(p4, p5,  nrows = 1, titleY = TRUE, shareY = TRUE, margin = 0.02, titleX = TRUE), 
                                                           final_forecast_plot, nrows = 2, margin = 0.1, titleY = TRUE))
      
    }
    leaderboard <- leaderboard %>% dplyr::arrange(avgRMSE) %>% as.data.frame()
    modelOutput$leaderboard <- leaderboard
  }
  
  
  modelOutput$summary_plot <- final_plot
  if(plot){
    print(final_plot)
  }
  print(leaderboard)
  class(modelOutput) <- "ts_backtest"
  return(modelOutput)
}


#' Tuning Time Series Forecasting Models Parameters with Grid Search 
#' @export ts_grid
#' @param ts.obj A univariate time series object of a class "ts"
#' @param model A string, defines the model
#' @param optim A string, set the optimization method - c("MAPE", "RMSE")
#' @param periods A string, set the number backtesting periods
#' @param window_length An integer, defines the length of the backtesting training window.
#' If set to NULL (default) will use an expending window starting the from the first observation,
#'  otherwise will use a sliding window.
#' @param window_space An integer, set the space length between each of the backtesting training partition 
#' @param window_test An integer, set the length of the backtesting testing partition
#' @param hyper_params A list, defines the tuning parameters and their range
#' @param parallel Logical, if TRUE use multiple cores in parallel
#' @param n.cores Set the number of cores to use if the parallel argument is set to TRUE. 
#' @description Tuning time series models with grid search approach using backtesting method.
#'  If set to "auto" (default), will use all available cores in the system minus 1
#' @return A list
#' @examples 
#' \dontrun{
#'  data(USgas)
#'  
#'  # Starting with a shallow search (sequence between 0 and 1 with jumps of 0.1)
#'  # To speed up the process, will set the parallel option to TRUE 
#'  # to run the search in parallel using 8 cores
#'  
#'  hw_grid_shallow <- ts_grid(ts.obj = USgas,
#'                             periods = 6,
#'                             model = "HoltWinters",
#'                             optim = "MAPE",
#'                             window_space = 6,
#'                             window_test = 12,
#'                             hyper_params = list(alpha = seq(0.01, 1,0.1),
#'                                                 beta =  seq(0.01, 1,0.1),
#'                                                 gamma = seq(0.01, 1,0.1)),
#'                             parallel = TRUE,
#'                             n.cores = 8)
#'  
#'  
#'  # Use the parameter range of the top 20 models 
#'  # to set a narrow but more agressive search
#'  
#'  a_min <- min(hw_grid_shallow$grid_df$alpha[1:20])
#'  a_max <- max(hw_grid_shallow$grid_df$alpha[1:20])
#'  
#'  b_min <- min(hw_grid_shallow$grid_df$beta[1:20])
#'  b_max <- max(hw_grid_shallow$grid_df$beta[1:20])
#'  
#'  g_min <- min(hw_grid_shallow$grid_df$gamma[1:20])
#'  g_max <- max(hw_grid_shallow$grid_df$gamma[1:20])
#'  
#'  hw_grid_second <- ts_grid(ts.obj = USgas,
#'                            periods = 6,
#'                            model = "HoltWinters",
#'                            optim = "MAPE",
#'                            window_space = 6,
#'                            window_test = 12,
#'                            hyper_params = list(alpha = seq(a_min, a_max,0.05),
#'                                                beta =  seq(b_min, b_max,0.05),
#'                                                gamma = seq(g_min, g_max,0.05)),
#'                            parallel = TRUE,
#'                            n.cores = 8)
#'  
#'  md <- HoltWinters(USgas, 
#'                    alpha = hw_grid_second$alpha,
#'                    beta = hw_grid_second$beta,
#'                    gamma = hw_grid_second$gamma)
#'  
#'  library(forecast)
#'  
#'  fc <- forecast(md, h = 60)
#'  
#'  plot_forecast(fc)
#' 
#' }  


ts_grid <- function(ts.obj, 
                    model,
                    optim = "MAPE",
                    periods,
                    window_length = NULL, 
                    window_space,
                    window_test,
                    hyper_params,
                    parallel = TRUE,
                    n.cores = "auto"){
  
  error <- period <- start_time <- NULL
  
  `%>%` <- magrittr::`%>%` 
  
  # Error handling
  if(!stats::is.ts(ts.obj)){
    stop("The input object is not 'ts' object")
  } else if(stats::is.mts(ts.obj)){
    stop("The input object is 'mts' object, please use 'ts'")
  }
  
  if(!optim %in% c("MAPE", "RMSE") || base::length(optim) != 1){
    warning("The value of the optim argument is not valid, using default option (MAPE)")
    optim <- "MAPE"
  }
  if(!base::is.logical(parallel)){
    warning("The 'parallel' argument is not a boolean operator, setting it to TRUE")
    parallel <- TRUE
  }
  
  if(n.cores != "auto"){
    if(!base::is.numeric(n.cores)){
      warning("The value of the 'n.cores' argument is not valid,", 
              " setting it to 'auto' mode")
      n.cores <- "auto"
    } else if(base::is.numeric(n.cores) && 
              (n.cores %% 1 != 0 || n.cores < 1)){
      warning("The value of the 'n.cores' argument is not valid,", 
              " setting it to 'auto' mode")
      n.cores <- "auto"
    } else{
      if(future::availableCores() < n.cores){
        warning("The value of the 'n.cores' argument is not valid,", 
                "(the requested number of cores are greater than available)",
                ", setting it to 'auto' mode")
        n.cores <- "auto"
      }
    }
  }
  
  if(n.cores == "auto"){
    n.cores <- base::as.numeric(future::availableCores() - 1)
  }
  
  if(!base::exists("model")){
    stop("The 'model' argument is missing")
  } else if(!model %in% c("HoltWinters")){
    stop("The 'model' argument is not valid")
  }
  
  # Set the backtesting partitions
  s <- length(ts.obj) - window_space * (periods - 1) # the length of the first partition
  e <- length(ts.obj)  # the end of the backtesting partition
  w_end <- seq(from = s, by = window_space, to = e) # Set the cutting points for the backtesting partions
  
  if(!base::is.null(window_length)){
    w_start <- w_end - window_test - window_length + 1
  } else {
    w_start <- base::rep(1, base::length(w_end))
  }
  
  
  if(model == "HoltWinters"){
    hw_par <- hyper_input <- hyper_null <- hyper_false <- NULL
    hw_par <- c("alpha", "beta", "gamma")
    if(!base::all(base::names(hyper_params) %in% hw_par)){
      stop("The 'hyper_params' argument is invalid")
    }
    
    if("alpha" %in% base::names(hyper_params)){
      alpha <- NULL
      if(is.null(hyper_params$alpha)){
        hyper_null <- c(hyper_null, "alpha")
      } else if(base::is.logical(hyper_params$alpha)){
        stop("The value of the 'alpha' argument cannot be only numeric")
      } else { 
        if(base::any(which(hyper_params$alpha < 0)) || 
           base::any(which(hyper_params$alpha > 1))){
          stop("The value of the 'alpha' parameter is out of range,",
               " cannot exceed 1 or be less or equal to 0")
        } 
        if(any(which(hyper_params$alpha == 0))){
          hyper_params$alpha[base::which(hyper_params$alpha == 0)] <- 1e-5
          warning("The value of the 'alpha' parameter cannot be equal to 0",
                  " replacing 0 with 1e-5")
        }
        
        alpha <- hyper_params$alpha
        hyper_input <- c(hyper_input, "alpha")
      }
    }
    
    if("beta" %in% base::names(hyper_params)){
      beta <- NULL
      if(is.null(hyper_params$beta)){
        hyper_null <- c(hyper_null, "beta")
      } else if(base::is.logical(hyper_params$beta) && 
                !base::isTRUE(hyper_params$beta)){
        beta <- FALSE
        hyper_false <- c(hyper_false, "beta")
      } else { 
        if(base::any(which(hyper_params$beta < 0)) || 
           base::any(which(hyper_params$beta > 1))){
          stop("The value of the 'beta' parameter is out of range,",
               " cannot exceed 1 or be less or equal to 0")
        } 
        if(any(which(hyper_params$beta == 0))){
          hyper_params$beta[base::which(hyper_params$beta == 0)] <- 1e-5
          warning("The value of the 'beta' parameter cannot be equal to 0",
                  " replacing 0 with 1e-5")
        }
        
        beta <- hyper_params$beta
        hyper_input <- c(hyper_input, "beta")
      }
    }
    
    if("gamma" %in% base::names(hyper_params)){
      gamma <- NULL
      if(is.null(hyper_params$gamma)){
        hyper_null <- c(hyper_null, "gamma")
      } else if(base::is.logical(hyper_params$gamma) && 
                !base::isTRUE(hyper_params$gamma)){
        gamma <- FALSE
        hyper_false <- c(hyper_false, "beta")
      } else { 
        if(base::any(which(hyper_params$gamma < 0)) || 
           base::any(which(hyper_params$gamma > 1))){
          stop("The value of the 'gamma' parameter is out of range,",
               " cannot exceed 1 or be less or equal to 0")
        } 
        if(any(which(hyper_params$gamma == 0))){
          hyper_params$gamma[base::which(hyper_params$gamma == 0)] <- 1e-5
          warning("The value of the 'gamma' parameter cannot be equal to 0",
                  " replacing 0 with 1e-5")
        }
        
        gamma <- hyper_params$gamma
        hyper_input <- c(hyper_input, "gamma")
      }
    }
    
    
    
    grid_df <- base::eval(
      base::parse(text = base::paste("base::expand.grid(", 
                                     base::paste(hyper_input, collapse = ", "),
                                     ")", 
                                     sep = "")))
    base::names(grid_df) <- hyper_input
    
    if(!base::is.null(hyper_false)){
      for(f in hyper_false){
        grid_df[f] <- FALSE
      }
    }
    
    grid_model <- base::paste("stats::HoltWinters(x = train", sep = "")
    for(i in hw_par){
      if(i %in% base::names(grid_df)){
        grid_model <- base::paste(grid_model, ", ", i, " = search_df$", i, "[i]", 
                                  sep = "" )
      } else {
        grid_model <- base::paste(grid_model, ", ", i, " = NULL", sep = "")
      }
    }
    grid_model <- base::paste(grid_model, ")", sep = "")
  }
  
  
  grid_output <- NULL
  if(!parallel){
    grid_output <- base::lapply(1:periods, function(n){
      ts_sub <- train <- test <- search_df <- NULL
      
      search_df <- grid_df
      search_df$period <- n
      search_df$error <- NA
      ts_sub <- stats::window(ts.obj, 
                              start = stats::time(ts.obj)[w_start[n]], 
                              end = stats::time(ts.obj)[w_end[n]])
      partition <- TSstudio::ts_split(ts_sub, sample.out = window_test)
      train <- partition$train
      test <- partition$test
      
      for(i in 1:nrow(search_df)){
        md <- fc <- NULL
        md <- base::eval(base::parse(text = grid_model))
        fc <- forecast::forecast(md, h = window_test)
        if(optim == "MAPE"){
          search_df$error[i] <- forecast::accuracy(fc, test)[10]
        } else if(optim == "RMSE"){
          search_df$error[i] <- forecast::accuracy(fc, test)[4]
        }
      }
      
      return(search_df)
    }) %>% 
      dplyr::bind_rows() %>%
      tidyr::spread(key = period, value = error)
  } else if(parallel){
    future::plan(future::multiprocess, workers = n.cores)  
    start_time <- Sys.time()
    grid_output <- future.apply::future_lapply(1:periods, function(n){
      ts_sub <- train <- test <- search_df <- NULL
      
      search_df <- grid_df
      search_df$period <- n
      search_df$error <- NA
      ts_sub <- stats::window(ts.obj, 
                              start = stats::time(ts.obj)[w_start[n]], 
                              end = stats::time(ts.obj)[w_end[n]])
      partition <- TSstudio::ts_split(ts_sub, sample.out = window_test)
      train <- partition$train
      test <- partition$test
      
      for(i in 1:nrow(search_df)){
        md <- fc <- NULL
        md <- base::eval(base::parse(text = grid_model))
        fc <- forecast::forecast(md, h = window_test)
        if(optim == "MAPE"){
          search_df$error[i] <- forecast::accuracy(fc, test)[10]
        } else if(optim == "RMSE"){
          search_df$error[i] <- forecast::accuracy(fc, test)[4]
        }
      }
      
      return(search_df)
    }) %>% 
      dplyr::bind_rows() %>%
      tidyr::spread(key = period, value = error)
  }
  
  col_mean <- base::which(!base::names(grid_output)  %in% base::names(hyper_params) )
  grid_output$mean <- base::rowMeans(grid_output[, col_mean])
  grid_output <- grid_output %>% dplyr::arrange(mean)
  
  final_output <- list(grid_df = grid_output)
  
  for(i in base::names(hyper_params)){
    final_output[[i]] <- grid_output[1, i]
  }
  final_output[["parameters"]] <- list(series = ts.obj, 
                                       model = model,
                                       optim = optim,
                                       periods = periods,
                                       window_length = window_length, 
                                       window_space = window_space,
                                       window_test = window_test,
                                       hyper_params = hyper_params,
                                       parallel = parallel,
                                       n.cores = n.cores)
  
  base::class(final_output) <- "ts_grid" 
  return(final_output)
}

#' Visualizing Grid Search Results
#' @export plot_grid
#' @param grid.obj A ts_grid output object
#' @param top An integer, set the number of hyper-parameters combinations to visualize 
#' (ordered by accuracy). If set to NULL (default), will plot the top 100 combinations
#' @param type The plot type, either "3D" for 3D plot or 
#' "parcoords" for parallel coordinates plot. 
#' Note: the 3D plot option is applicable whenever there are three tuning parameters, 
#' otherwise will use a 2D plot for two tuning parameters. 
#' @param highlight A proportion between 0 (excluding) and 1, 
#' set the number of hyper-parameters combinations to highlight 
#' (by accuracy), if the type argument is set to "parcoords" 
#' @param colors A list of plotly arguments for the color scale setting: 
#' 
#' showscale -  display the color scale if set to TRUE. 
#' 
#' reversescale - reverse the color scale if set to TRUE 
#' 
#' colorscale set the color scale of the plot, possible palettes are:
#' Greys, YlGnBu,  Greens , YlOrRd,
#' Bluered, RdBu, Reds, Blues, Picnic,
#' Rainbow, Portland, Jet, Hot, Blackbody,
#' Earth, Electric, Viridis, Cividis



plot_grid <- function(grid.obj, 
                      top = NULL, 
                      highlight = 0.1, 
                      type = "parcoords", 
                      colors = list(showscale = TRUE,
                                    reversescale = FALSE,
                                    colorscale = "Jet")){
  
  # Setting the pipe operator
  `%>%` <- magrittr::`%>%`
  
  # Setting variables
  color_option <- p <- par_names <- sizeref <- NULL
  
  # List of optional color scale
  color_option <- c("Greys","YlGnBu", "Greens", "YlOrRd",
                    "Bluered", "RdBu", "Reds", "Blues", "Picnic",
                    "Rainbow", "Portland", "Jet", "Hot", "Blackbody",
                    "Earth", "Electric", "Viridis", "Cividis")
  
  
  # Error handling
  if(class(grid.obj) != "ts_grid"){
    stop("The input object is not a 'ts_grid' class")
  }
  
  if(!base::is.list(colors)){
    warning("The 'colors' argument is not valid, using default option")
    colors = base::list(showscale = TRUE,
                        reversescale = FALSE,
                        colorscale = "Jet")
  } else if(!all(base::names(colors) %in% c("showscale", "reversescale", "colorscale"))){
    warning("The 'colors' argument is not valid, using default option")
    colors = base::list(showscale = TRUE,
                        reversescale = FALSE,
                        colorscale = "Jet")
  } 
  
  if(!base::is.logical(colors$showscale)){
    warning("The 'showscale' parameter of the 'colors' argument is not logical, using default option (TRUE)")
    colors$showscale <- TRUE
  }
  
  if(!base::is.logical(colors$reversescale)){
    warning("The 'reversescale' parameter of the 'colors' argument is not logical, using default option (FALSE)")
    colors$reversescale <- FALSE
  }
  
  if(!base::is.character(colors$colorscale) || 
     base::length(colors$colorscale) != 1 || 
     !colors$colorscale %in% color_option){
    warning("The 'colorscale' parameter of the 'colors' argument is not logical, using default option (Jet)")
  }
  
  
  if(type != "parcoords" && type != "3D"){
    warning("The value of the 'type' argument is not valid, using default option (parcoords)")
    type <- "parcoords"
  }
  
  if(!base::is.null(top)){
    if(!base::is.numeric(top) || top %% 1 != 0){
      warning("The value of the 'top' argument is not valid, using default option (top 100 models)")
      top <- ifelse(base::nrow(grid.obj$grid_df) > 100, 100, base::nrow(grid.obj$grid_df))
    }
    if(top > base::nrow(grid.obj$grid_df)){
      warning("The value of the 'top' argument exceeding the number of models, using default option (top 100 models)")
      top <- ifelse(base::nrow(grid.obj$grid_df) > 100, 100, base::nrow(grid.obj$grid_df))
    }
  } else { 
    top <- ifelse(base::nrow(grid.obj$grid_df) > 100, 100, base::nrow(grid.obj$grid_df))
  }
  
  if(!base::is.numeric(highlight) || highlight <= 0 || highlight > 1){
    warning("The value of the 'highlight' argument is not valid, using default (0.1)")
    highlight <- 0.1
  }
  
  par_names <- base::names(grid.obj$parameters$hyper_params) 
  
  for(i in par_names){
    if(base::is.null(grid.obj$parameters$hyper_params[[i]]) ||
       grid.obj$parameters$hyper_params[[i]] == FALSE){
      par_names <- par_names[-which(par_names == i)]
    }
  }
  
  
  if(type == "parcoords"){
    
    if(grid.obj$parameters$model == "HoltWinters"){
      if(base::length(par_names) < 2){
        stop("Cannot create a parallel coordinates plot for a single hyper parameter")
      }
      hw_dim <- NULL
      hw_dim <- base::list()
      
      for(i in base::seq_along(par_names)){
        hw_dim[[i]] <-  base::eval(base::parse(text = base::paste("list(range = c(0,1),
                                                                  constraintrange = c(min(grid.obj$grid_df[1:", base::ceiling(top * highlight), ", i]),
                                                                  max(grid.obj$grid_df[1:", base::ceiling(top * highlight), ",i])),
                                                                  label = '", par_names[i],"', values = ~", 
                                                                  par_names[i],
                                                                  ")",
                                                                  sep = "")
        ))
      }
      
      p <- grid.obj$grid_df[1:top,] %>%
        plotly::plot_ly(type = 'parcoords',
                        line = list(color = ~ mean,
                                    colorscale = colors$colorscale,
                                    showscale = colors$showscale,
                                    reversescale = colors$reversescale,
                                    cmin = base::min(grid.obj$grid_df$mean),
                                    cmax = base::max(grid.obj$grid_df$mean[1:top]),
                                    colorbar=list(
                                      title= base::paste("Avg.", grid.obj$parameters$optim, sep = " ")
                                    )),
                        dimensions = hw_dim
        ) %>% plotly::layout(title = base::paste(grid.obj$parameters$model, 
                                                 " Parameters Grid Search Results (Avg. ",
                                                 grid.obj$parameters$optim,
                                                 ") for Top ", 
                                                 top, 
                                                 " Models", sep = ""),
                             xaxis = list(title = base::paste("Testing Over", grid.obj$parameters$periods, "Periods", sep = " ")))
      
      
    }
  }else if(type == "3D"){
    if(grid.obj$parameters$model == "HoltWinters"){
      if(base::length(par_names) == 3){
        p <- plotly::plot_ly(data = grid.obj$grid_df[1:top,],
                             type="scatter3d",
                             mode = "markers",
                             x = ~ alpha, 
                             y = ~ beta, 
                             z = ~ gamma,
                             hoverinfo = 'text',
                             text = paste(base::paste("Avg.", grid.obj$parameters$optim, sep = " "),
                                          ": ", base::round(grid.obj$grid_df[1:top, "mean"], 2), 
                                          "<br>", par_names[1],": ", grid.obj$grid_df[1:top, par_names[1]],
                                          "<br>", par_names[2],": ", grid.obj$grid_df[1:top, par_names[2]],
                                          "<br>", par_names[3],": ", grid.obj$grid_df[1:top, par_names[3]],
                                          sep = ""),
                             marker = list(color = ~ mean, 
                                           colorscale = colors$colorscale,
                                           showscale = colors$showscale,
                                           reversescale = colors$reversescale,
                                           colorbar=list(
                                             title= base::paste("Avg.", grid.obj$parameters$optim, sep = " ")
                                           ))) %>% 
          plotly::layout(title = base::paste(grid.obj$parameters$model, 
                                             " Parameters Grid Search Results (Avg. ",
                                             grid.obj$parameters$optim,
                                             ") for Top ", 
                                             top, 
                                             " Models", sep = ""),
                         xaxis = list(title = base::paste("Testing Over", grid.obj$parameters$periods, "Periods", sep = " ")))
      } else if(base::length(par_names) == 2){
        warning("Cannot create a 3D plot for two hyper parameters")
        # Scaling the bubbles size
        sizeref <- 2.0 * max(grid.obj$grid_df$mean[1:top]) / (20**2)
        
        p <- plotly::plot_ly(x = grid.obj$grid_df[1:top, par_names[1]],
                             y = grid.obj$grid_df[1:top, par_names[2]],
                             type = "scatter",
                             mode = "markers",
                             hoverinfo = 'text',
                             text = paste(base::paste("Avg.", grid.obj$parameters$optim, sep = " "),
                                          ": ", base::round(grid.obj$grid_df[1:top, "mean"], 2), 
                                          "<br>", par_names[1],": ", grid.obj$grid_df[1:top, par_names[1]],
                                          "<br>", par_names[2],": ", grid.obj$grid_df[1:top, par_names[2]],
                                          sep = ""),
                             marker = list(color = grid.obj$grid_df[1:top, "mean"],
                                           size = grid.obj$grid_df[1:top, "mean"],
                                           sizemode = 'area', sizeref = sizeref,
                                           colorscale = colors$colorscale,
                                           showscale = colors$showscale,
                                           reversescale = colors$reversescale,
                                           colorbar=list(
                                             title= base::paste("Avg.", grid.obj$parameters$optim, sep = " ")
                                           ))
        ) %>%
          plotly::layout(title = base::paste(grid.obj$parameters$model, 
                                             "Parameters Grid Search Results (Avg.",
                                             base::paste(grid.obj$parameters$optim, ")", sep = ""), 
                                             "for Top", 
                                             top,
                                             "Models",
                                             sep = " "),
                         xaxis = list(title = par_names[1]),
                         yaxis = list(title = par_names[2]))
      } else if(base::length(par_names) <= 1){
        stop("Cannot create a 3D plot for a single hyper parameter")
      }
    }
  }
  
  return(p)
}

#' Train, Test, Evaluate, and Forecast Multiple Time Series Forecasting Models
#' @export 
#' @description Method for train test and compare multiple time series models using either one partition (i.e., sample out) 
#' or multipe partitions (backtesting)
#' @param input A univariate time series object (ts class)
#' @param methods A list, defines the models to use for training and forecasting the series. 
#' The list must include a sub list with the model type, and the model's arguments (when applicable) and notes about the model. 
#' The sub-list name will be used as the model ID. Possible models:
#' 
#' \code{\link[stats]{arima}} - model from the stats package 
#' 
#' \code{\link[forecast]{auto.arima}} - model from the forecast package
#' 
#' \code{\link[forecast]{ets}} - model from the forecast package
#' 
#'  \code{\link[stats]{HoltWinters}} - model from the stats package 
#' 
#' \code{\link[forecast]{nnetar}} - model from the forecast package
#'
#' \code{\link[forecast]{tslm}} - model from the forecast package (note that the 'tslm' model must have the formula argument in the 'method_arg' argument)
#' 
#' @param train_method A list, defines the backtesting parameters:
#' 
#' partitions - an integer, set the number of training and testing partitions to be used in the backtesting process, 
#' where when partition is set to 1 it is a simple holdout training approach
#'  
#' space - an integer, defines the length of the backtesting window expansion
#'  
#' sample.in - an integer, optional, defines the length of the training partitions, and therefore the backtesting window structure. 
#' By default, it set to NULL and therefore, the backtesting using expending window. 
#' Otherwise, when the sample.in defined, the window structure is sliding
#'  
#' sample.in - an integer, optional, defines the length of the training partitions, and therefore the type of the backtesting window. 
#'By default, is set to NULL, which implay that the backtesting is using an expending window. Otherwise, when defining the size of the training partition, th
#' defines the train approach, either using a single testing partition (sample out) 
#' or use multiple testing partitions (backtesting). The list should include the training method argument, (please see 'details' for the structure of the argument)
#' @param horizon An integer, defines the forecast horizon
#' @param xreg Optional, a list with two vectors (e.g., data.frame or matrix) of external regressors, 
#' one vector corresponding to the input series and second to the forecast itself 
#' (e.g., must have the same length as the input and forecast horizon, respectively)
#' @param error A character, defines the error metrics to be used to sort the models leaderboard. Possible metric - "MAPE" or "RMSE"
#' @param level An integer, set the  confidence level of the prediction intervals
#' @examples 
#' 
#' # Defining the models and their arguments
#' methods <- list(ets1 = list(method = "ets",
#'                             method_arg = list(opt.crit = "lik"),
#'                             notes = "ETS model with opt.crit = lik"),
#'                 ets2 = list(method = "ets",
#'                             method_arg = list(opt.crit = "amse"),
#'                             notes = "ETS model with opt.crit = amse"),
#'                 arima1 = list(method = "arima",
#'                               method_arg = list(order = c(2,1,0)),
#'                               notes = "ARIMA(2,1,0)"),
#'                 arima2 = list(method = "arima",
#'                               method_arg = list(order = c(2,1,2),
#'                                                 seasonal = list(order = c(1,1,1))),
#'                               notes = "SARIMA(2,1,2)(1,1,1)"),
#'                 auto_arima = list(method = "auto.arima",
#'                                   method_arg = NULL,
#'                                   notes = "auto.arima model"),
#'                 hw = list(method = "HoltWinters",
#'                           method_arg = NULL,
#'                           notes = "HoltWinters Model"),
#'                 tslm = list(method = "tslm",
#'                             method_arg = list(formula = input ~ trend + season),
#'                             notes = "tslm model with trend and seasonal components"))
#' # Training the models with backtesting
#' md <- train_model(input = USgas,
#'                   methods = methods,
#'                   train_method = list(partitions = 6, 
#'                                       sample.out = 12, 
#'                                       space = 3),
#'                   horizon = 12,
#'                   error = "MAPE")
#' # View the model performance on the backtesting partitions
#' md$leaderboard
#' 


train_model <- function(input,
                        methods,
                        train_method,
                        horizon,
                        error = "MAPE",
                        xreg = NULL,
                        level = c(80, 95)){
  
  # Setting the pipe operator
  `%>%` <- magrittr::`%>%`
  
  method_list <- input_freq <- input_length <- w <- s1 <- s2 <- NULL
  grid_df <- models_df <- w_range <-  notes <- NULL
  methods_selected <- model_id <- start <- end <- partition <- NULL
  model <- avg_mape <- avg_rmse <- NULL
  # Whenever updating, need to update the add_method function as well
  method_list <- list("arima", "auto.arima", "ets", "HoltWinters", "nnetar", "tslm")
  
  
  ### Error Handling
  # Check the level argument
  if(base::all(!is.numeric(level)) ||
     base::any(level %% 1 != 0) ||
     base::any(level  <= 0 | level > 100)){
    stop("Error on the 'level' argument: the argument is out of range (0,100]")
  }
  
  # Check the error argument
  if(base::is.null(error) || !base::is.character(error) || base::length(error) !=1){
    stop("Error on the 'error' argument: the input is not valid, please use either 'RMSE' or 'MAPE'")
  } else if( error != "MAPE" && error != "RMSE"){
    stop("Error on the 'error' argument: the input is not valid, please use either 'RMSE' or 'MAPE'")
  }
  
  
  # Checking the input argument
  if(!stats::is.ts(input)){
    stop("The input argument is not a valid 'ts' object")
  } else if(stats::is.mts(input)){
    stop("Cannot use multiple time series object as input")
  }
  
  # Getting the attributes of the input object
  input_freq <- stats::frequency(input)
  input_length <- base::length(input)
  
  
  
  # Validating the methods argument
  
  if(!base::is.list(methods)){
    stop("Error on the 'methods' argument: the argument is not a list")
  } else if(base::is.null(base::names(methods))){
    stop("Error on the 'methods' argument: could not find the models IDs")
  } else if(base::any("NULL" %in% base::as.character(methods %>% purrr::map(~.x[["method"]])))){
    stop("Error on the 'methods' argument: at least one of the methods is missing the 'method' argument")
  }
  
  models_df <- base::data.frame(model_id = base::names(methods),
                                methods_selected  = base::as.character(methods %>% purrr::map_chr(~.x[["method"]])),
                                notes = base::as.character(methods %>% purrr::map(~.x[["notes"]])),
                                stringsAsFactors = FALSE)
  
  if(!base::all(models_df$methods_selected %in% method_list)){
    stop("Error on the 'methods' argument: at least one of the models methods is not valid")
  }
  
  # Checking the train argument
  
  if(!base::is.list(train_method)){
    stop("Error on the 'train_method' argument: the argument is not a list")
  } else if(!"partitions" %in% base::names(train_method)){
    stop("Error on the 'train_method' argument: the 'partition' argument is missing")
  } else if(!"space" %in% base::names(train_method)){
    stop("Error on the 'train_method' argument: the 'space' argument is missing")
  } else if(!"sample.out" %in% base::names(train_method)){
    stop("Error on the 'train_method' argument: the 'sample.out' argument is missing")
  } else if(!base::is.numeric(train_method$sample.out) || 
            train_method$sample.out < 1 ||
            train_method$sample.out %% 1 != 0){
    stop("Error on the 'train_method' argument: the 'sample.out' argument is not valide, please use a positive integer")
  } else if(!base::is.numeric(train_method$partitions) || 
            train_method$partitions < 1 ||
            train_method$partitions %% 1 != 0){
    stop("Error on the 'train_method' argument:  the 'partitions' argument is not valide, please use a positive integer")
  } else if(!base::is.numeric(train_method$space) || 
            train_method$space < 1 ||
            train_method$space %% 1 != 0){
    stop("Error on the 'train_method' argument:  the 'space' argument is not valide, please use a positive integer")
  } 
  
  
  w <-  seq(from = input_length - train_method$space * (train_method$partitions - 1), 
            by = train_method$space, 
            length.out = train_method$partitions)
  
  
  if(min(w) < input_freq * 2){
    stop("Error on the 'train_method' argument: the length of the first partition is not sufficient to train a model",
         " (must leave at least two full cycles for the sample in partition)")
  }
  
  # If not using sample.in, will define the start point as 1
  if(!"sample.in" %in% base::names(train_method) ||
     ("sample.in" %in% base::names(train_method) && 
      base::is.null(train_method$sample.in))){
    s1 <- s2 <- 1
    w_range <- base::data.frame(start = c(base::rep(s1, base::length(w)), s2), 
                                end = c(w, input_length), 
                                type = c(base::rep("train", base::length(w)), "forecast"),
                                partition = c(base::paste0("partition_", 1:base::length(w), sep = ""), "final_partition"),
                                stringsAsFactors = FALSE)
    # If defining the sample.in -> check that the argument is valid
  } else if("sample.in" %in% base::names(train_method)){
    # If defining the sample.in -> check that the argument is valid
    if(!base::is.numeric(train_method$sample.in) || 
       train_method$sample.in < 1 ||
       train_method$sample.in %% 1 != 0){
      stop("Error on the 'train_method' argument: the training partition length (sample in) of the backtesting is not valid. Please use a positive integer")
    } else if( train_method$sample.in < input_freq * 2){
      stop("Error on the 'train_method' argument: the training partition length (sample in) must have at least two cycles")
    }
    s1 <- w - train_method$sample.out - train_method$sample.in + 1
    s2 <- input_length - train_method$sample.in + 1
    w_range <- base::data.frame(start = c(s1, s2), 
                                end = c(w, input_length), 
                                type = c(base::rep("train", base::length(w)), "forecast"),
                                partition = c(base::paste0("partition_", 1:base::length(w), sep = ""), "final_partition"),
                                stringsAsFactors = FALSE)
  }
  
  
  
  # Checking the horizon argument
  if(horizon %% 1 != 0 || !base::is.numeric(horizon) || horizon <=0){
    stop("Error on the 'horizon' argument: the 'horizon' is not valid, please make sure using positive integer")
  }
  
  # Checking the xreg argument
  if(!base::is.null(xreg)){
    if(!all(c("train", "forecast") %in% base::names(xreg))){
      stop("Error on the 'xreg' argument: the 'xreg' list is not valid, please make sure setting the correspinding regressor",
           " inputs for the 'input' argument (train) and for the forecast horizon (forecast)")
    } else if(base::names(xreg$train) != base::names(xreg$forecast)){
      stop("Error on the 'xreg' argument: the regressors names in the train and forecast inputs are not aligned")
    } else if(base::nrow(xreg$train) != base::length(input)){
      stop("Error on the 'xreg' argument: the length of the xreg train input is not aligned with the length of the input series")
    } else if(base::nrow(xreg$forecast) != horizon){
      stop("Error on the 'xreg' argument: the length of the xreg forecast input is not aligned with the forecast horizon")
    }
  }
  
  # Creating grid of all the modeling combinations
  grid_df <- base::expand.grid(models_df$model_id, s1, train_method$sample.out, stringsAsFactors = FALSE) %>% 
    stats::setNames(c("model_id", "start", "horizon")) %>% 
    dplyr::left_join(models_df, by = c("model_id"))  %>%
    dplyr::mutate(type = "train") %>% dplyr::bind_rows(
      base::expand.grid(models_df$model_id, s2, horizon, stringsAsFactors = FALSE) %>% 
        stats::setNames(c("model_id", "start", "horizon")) %>% 
        dplyr::left_join(models_df, by = c("model_id")) %>%
        dplyr::mutate(type = "forecast")
    ) %>%
    dplyr::left_join(w_range, by = c("start", "type"))
  
  
  fc_output <-  lapply(base::seq_along(grid_df$model_id), function(i){
    
    ts.obj <- train <- test <- md <- fc <- arg <- NULL
    
    ts.obj <- stats::window(input, 
                            start = stats::time(input)[grid_df$start[i]], 
                            end = stats::time(input)[grid_df$end[i]])
    
    
    if(grid_df$type[i] == "train"){
      if(!base::is.null(xreg)){
        xreg_train <- xreg$train[grid_df$start[i]:grid_df$end[i],]
        xreg_test <- xreg$train[(grid_df$end[i] + 1):(grid_df$end[i] + grid_df$horizon[i]) ,]
      }
      ts_partitions <- TSstudio::ts_split(ts.obj = ts.obj, sample.out = train_method$sample.out)
      
      train <- ts_partitions$train
      test <- ts_partitions$test
      
      if(grid_df$methods_selected[i] == "arima"){
        if(!base::is.null(methods[[grid_df$model_id[i]]]$method_arg)){
          arg <- methods[[grid_df$model_id[i]]]$method_arg
        }
        if("xreg" %in% base::names(arg) && !base::is.null(xreg)){
          arg_xreg <- arg
          arg_xreg$xreg <- xreg_train[,arg$xreg]
          md <- do.call(stats::arima,c(base::list(train), arg_xreg))
          fc <- forecast::forecast(md, 
                                   h = grid_df$horizon[i], 
                                   xreg = xreg_test[,arg$xreg],
                                   level = level)
        } else {
          md <- do.call(stats::arima,c(base::list(train), arg))
          fc <- forecast::forecast(md, 
                                   h = grid_df$horizon[i],
                                   level = level)
        }
      }
      
      if(grid_df$methods_selected[i] == "HoltWinters"){
        if(!base::is.null(methods[[grid_df$model_id[i]]]$method_arg)){
          arg <- methods[[grid_df$model_id[i]]]$method_arg
        }
        md <- do.call(stats::HoltWinters,c(base::list(train), arg))
        fc <- forecast::forecast(md, 
                                 h = grid_df$horizon[i],
                                 level = level)
      }
      
      if(grid_df$methods_selected[i] == "auto.arima"){
        if(!base::is.null(methods[[grid_df$model_id[i]]]$method_arg)){
          arg <- methods[[grid_df$model_id[i]]]$method_arg
        }
        if("xreg" %in% base::names(arg) && !base::is.null(xreg)){
          arg_xreg <- arg
          arg_xreg$xreg <- xreg_train[,arg$xreg]
          md <- do.call(forecast::auto.arima,c(base::list(train), arg_xreg))
          fc <- forecast::forecast(md, 
                                   h = grid_df$horizon[i], 
                                   xreg = xreg_test[,arg$xreg],
                                   level = level)
        } else {
          md <- do.call(forecast::auto.arima,c(base::list(train), arg))
          fc <- forecast::forecast(md, 
                                   h = grid_df$horizon[i],
                                   level = level)
        }
      }
      
      
      if(grid_df$methods_selected[i] == "ets"){
        if(!base::is.null(methods[[grid_df$model_id[i]]]$method_arg)){
          arg <- methods[[grid_df$model_id[i]]]$method_arg
        }
        md <- do.call(forecast::ets,c(base::list(train), arg))
        fc <- forecast::forecast(md, 
                                 h = grid_df$horizon[i],
                                 level = level)
      } 
      
      
      if(grid_df$methods_selected[i] == "nnetar"){
        if(!base::is.null(methods[[grid_df$model_id[i]]]$method_arg)){
          arg <- methods[[grid_df$model_id[i]]]$method_arg
        }
        if("xreg" %in% base::names(arg) && !base::is.null(xreg)){
          arg_xreg <- arg
          arg_xreg$xreg <- xreg_train[,arg$xreg]
          md <- do.call(forecast::nnetar,c(base::list(train), arg_xreg))
          fc <- forecast::forecast(md, 
                                   h = grid_df$horizon[i], 
                                   xreg = xreg_test[,arg$xreg],
                                   level = level)
        } else {
          md <- do.call(forecast::nnetar,c(base::list(train), arg))
          fc <- forecast::forecast(md, 
                                   h = grid_df$horizon[i],
                                   level = level)
        }
      }
      
      
      if(grid_df$methods_selected[i] == "tslm"){
        if(!base::is.null(methods[[grid_df$model_id[i]]]$method_arg)){
          arg <- methods[[grid_df$model_id[i]]]$method_arg
          # Validate the formula
          if(!"formula" %in% base::names(arg)){
            stop("Error on the 'train_method' argument: cannot run 'tslm' model without the 'formula' argument")
          } else{
            f <-  base::Reduce(base::paste, base::deparse(arg$formula))
            tilde <- base::regexpr("~", f) %>% base::as.numeric()
            if(tilde == -1){
              stop("Error on the 'train_method' argument: cannot run 'tslm' model without the 'formula' argument")
            } else {
              # If formula good check the castumize the xreg argument
              if("xreg" %in% base::names(arg) && !base::is.null(xreg)){
                arg_xreg <- arg
                arg_xreg$formula <- base::paste("train ~ ", 
                                                base::substr(f, tilde + 1, base::nchar(f)), "+", 
                                                base::paste0(arg$xreg, collapse = "+"), ", data = xreg_train",
                                                sep = "")
                
                md <-  do.call(forecast::tslm,c(base::list(train), arg_xreg))
                fc <- forecast::forecast(md, 
                                         h = grid_df$horizon[i], 
                                         newdata = xreg_test,
                                         level = level)
              } else {
                arg$formula <- base::paste("train", base::substr(f, tilde + 1, base::nchar(f)), sep = "~")
                md <-  do.call(forecast::tslm,c(base::list(train), arg))
                fc <- forecast::forecast(md, 
                                         h = grid_df$horizon[i],
                                         level = level)
              }
            }
          }
        } else {
          stop("Error on the 'train_method' argument: cannot run 'tslm' model without the function's arguments")
        }
      }
      
      
    } else if(grid_df$type[i] == "forecast"){
      if(!base::is.null(xreg)){
        xreg_train <- xreg$train[grid_df$start[i]:grid_df$end[i],]
        xreg_forecast<- xreg$forecast
      }
      
      
      if(grid_df$methods_selected[i] == "arima"){
        if(!base::is.null(methods[[grid_df$model_id[i]]]$method_arg)){
          arg <- methods[[grid_df$model_id[i]]]$method_arg
        }
        if("xreg" %in% base::names(arg) && !base::is.null(xreg)){
          arg_xreg <- arg
          arg_xreg$xreg <- xreg_train[,arg$xreg]
          md <- do.call(stats::arima,c(base::list(ts.obj), arg_xreg))
          fc <- forecast::forecast(md, 
                                   h = grid_df$horizon[i], 
                                   xreg = xreg_forecast[,arg$xreg],
                                   level = level)
        } else {
          md <- do.call(stats::arima,c(base::list(ts.obj), arg))
          fc <- forecast::forecast(md, 
                                   h = grid_df$horizon[i],
                                   level = level)
        }
      }
      
      if(grid_df$methods_selected[i] == "HoltWinters"){
        if(!base::is.null(methods[[grid_df$model_id[i]]]$method_arg)){
          arg <- methods[[grid_df$model_id[i]]]$method_arg
        }
        md <- do.call(stats::HoltWinters,c(base::list(ts.obj), arg))
        fc <- forecast::forecast(md, 
                                 h = grid_df$horizon[i],
                                 level = level)
      }
      
      if(grid_df$methods_selected[i] == "auto.arima"){
        if(!base::is.null(methods[[grid_df$model_id[i]]]$method_arg)){
          arg <- methods[[grid_df$model_id[i]]]$method_arg
        }
        if("xreg" %in% base::names(arg) && !base::is.null(xreg)){
          arg_xreg <- arg
          arg_xreg$xreg <- xreg_train[,arg$xreg]
          md <- do.call(forecast::auto.arima,c(base::list(ts.obj), arg_xreg))
          fc <- forecast::forecast(md, 
                                   h = grid_df$horizon[i], 
                                   xreg = xreg_test[,arg$xreg],
                                   level = level)
        } else {
          md <- do.call(forecast::auto.arima,c(base::list(ts.obj), arg))
          fc <- forecast::forecast(md, 
                                   h = grid_df$horizon[i],
                                   level = level)
        }
      }
      
      
      if(grid_df$methods_selected[i] == "ets"){
        if(!base::is.null(methods[[grid_df$model_id[i]]]$method_arg)){
          arg <- methods[[grid_df$model_id[i]]]$method_arg
        }
        md <- do.call(forecast::ets,c(base::list(ts.obj), arg))
        fc <- forecast::forecast(md, 
                                 h = grid_df$horizon[i],
                                 level = level)
      } 
      
      
      if(grid_df$methods_selected[i] == "nnetar"){
        if(!base::is.null(methods[[grid_df$model_id[i]]]$method_arg)){
          arg <- methods[[grid_df$model_id[i]]]$method_arg
        }
        if("xreg" %in% base::names(arg) && !base::is.null(xreg)){
          arg_xreg <- arg
          arg_xreg$xreg <- xreg_train[,arg$xreg]
          md <- do.call(forecast::nnetar,c(base::list(ts.obj), arg_xreg))
          fc <- forecast::forecast(md, 
                                   h = grid_df$horizon[i], 
                                   xreg = xreg_test[,arg$xreg],
                                   level = level)
        } else {
          md <- do.call(forecast::nnetar,c(base::list(ts.obj), arg))
          fc <- forecast::forecast(md, 
                                   h = grid_df$horizon[i],
                                   level = level)
        }
      }
      
      
      if(grid_df$methods_selected[i] == "tslm"){
        if(!base::is.null(methods[[grid_df$model_id[i]]]$method_arg)){
          arg <- methods[[grid_df$model_id[i]]]$method_arg
          # Validate the formula
          if(!"formula" %in% base::names(arg)){
            stop("Error on the 'train_method' argument: cannot run 'tslm' model without the 'formula' argument")
          } else{
            f <-  base::Reduce(base::paste, base::deparse(arg$formula))
            tilde <- base::regexpr("~", f) %>% base::as.numeric()
            if(tilde == -1){
              stop("Error on the 'train_method' argument: cannot run 'tslm' model without the 'formula' argument")
            } else {
              # If formula good check the castumize the xreg argument
              if("xreg" %in% base::names(arg) && !base::is.null(xreg)){
                arg_xreg <- arg
                arg_xreg$formula <- base::paste("ts.obj ~ ", 
                                                base::substr(f, tilde + 1, base::nchar(f)), "+", 
                                                base::paste0(arg$xreg, collapse = "+"), ", data = xreg_train",
                                                sep = "")
                
                md <-  do.call(forecast::tslm,c(base::list(ts.obj), arg_xreg))
                fc <- forecast::forecast(md, 
                                         h = grid_df$horizon[i], 
                                         newdata = xreg_test,
                                         level = level)
              } else {
                arg$formula <- base::paste("ts.obj", base::substr(f, tilde + 1, base::nchar(f)), sep = "~")
                md <-  do.call(forecast::tslm,c(base::list(ts.obj), arg))
                fc <- forecast::forecast(md, 
                                         h = grid_df$horizon[i],
                                         level = level)
              }
            }
          }
        } else {
          stop("Error on the 'train_method' argument: cannot run 'tslm' model without the function's arguments")
        }
      }
      
      
    }
    
    output <- list(model = md, 
                   forecast = fc, 
                   parameters = base::list(
                     type = grid_df$type[i],
                     model_id = grid_df$model_id[i],
                     method = grid_df$methods_selected[i],
                     horizon = grid_df$horizon[i],
                     partition = grid_df$partition[i]))
    
    
    return(output)
    
  })
  
  input_window <- grid_df %>% dplyr::select(start, end, horizon, partition) %>% dplyr::distinct()
  
  t <- base::which(fc_output %>% purrr::map("parameters") %>% purrr::map_chr("type") == "train")
  p1 <- fc_output[t]  %>% purrr::map("parameters") %>% purrr::map_chr("partition") %>% base::unique()
  
  training <- lapply(base::seq_along(p1), function(i1){
    l <- NULL
    l <- base::which(fc_output[t] %>% purrr::map("parameters") %>% purrr::map_chr("partition") == p1[i1])
    md_id <- fc_output[l] %>% purrr::map("parameters") %>% purrr::map_chr("model_id") 
    
    
    ts.obj <- ts_partitions <- train <- test <- NULL
    ts.obj <- stats::window(input, 
                            start = stats::time(input)[input_window$start[which(input_window$partition == p1[i1])]],
                            end = stats::time(input)[input_window$end[which(input_window$partition == p1[i1])]])
    
    ts_partitions <- TSstudio::ts_split(ts.obj = ts.obj, sample.out = input_window$horizon[which(input_window$partition == p1[i1])])
    partition_output <- lapply(l, function(i2){
      x <- fc_output[[i2]]
      y <- base::list()
      y[[x$parameters$model_id]] <- list(model = x$model, forecast = x$forecast, parameters = x$parameters)
    }) %>% stats::setNames(md_id)
    partition_output$train <- ts_partitions$train
    partition_output$test <- ts_partitions$test
    
    return(partition_output)
  }) %>% stats::setNames(p1)
  
  f <- base::which(fc_output %>% purrr::map("parameters") %>% purrr::map_chr("type") == "forecast")
  p2 <- fc_output[f]  %>% purrr::map("parameters") %>% purrr::map_chr("partition") %>% base::unique()
  
  forecast <- lapply(base::seq_along(p2), function(i1){
    l <- NULL
    l <- base::which(fc_output[f] %>% purrr::map("parameters") %>% purrr::map_chr("partition") == p2[i1])
    md_id <- fc_output[l] %>% purrr::map("parameters") %>% purrr::map_chr("model_id") 
    partition_output <- lapply(l, function(i2){
      x <- fc_output[[i2]]
      y <- base::list()
      y[[x$parameters$model_id]] <- list(model = x$model, forecast = x$forecast, parameters = x$parameters)
    }) %>% stats::setNames(md_id)
    
    
    ts.obj <- NULL
    ts.obj <- stats::window(input, 
                            start = stats::time(input)[input_window$start[which(input_window$partition == p2[i1])]],
                            end = stats::time(input)[input_window$end[which(input_window$partition == p2[i1])]])
    
    partition_output$train <- ts.obj
    return(partition_output)
  }) %>% stats::setNames(p2)
  
  
  error_summary <- lapply(models_df$model_id, function(m){
    
    f <- training[p1] %>% purrr::map(m) %>% purrr::map("forecast") %>% purrr::map("mean") 
    p <- f %>% base::names()
    a <- training[p1] %>% purrr::map("test") 
    u <- training[p1] %>% purrr::map(m) %>% purrr::map("forecast") %>% purrr::map("upper")
    l <- training[p1] %>% purrr::map(m) %>% purrr::map("forecast") %>% purrr::map("lower")
    levels <- training[p1] %>% purrr::map(m) %>% purrr::map("forecast") %>% purrr::map("level")
    
    
    error_df <- lapply(base::seq_along(p),function(n){
      
      df <-  coverage_df <-  NULL
      
      if(base::is.null(base::colnames(u[[p[n]]]))){
        if(base::is.null(base::dim(u[[p[n]]]))){
          u[[p[n]]] <- u[[p[n]]] %>% as.matrix()
          l[[p[n]]] <- l[[p[n]]] %>% as.matrix()
        }
        base::colnames(u[[p[n]]]) <- base::paste0(levels[[p[n]]], "%")
        base::colnames(l[[p[n]]]) <- base::paste0(levels[[p[n]]], "%")
      }
      
      coverage_df <- lapply(base::colnames(u[[p[n]]]), function(i){
        df <-  base::data.frame(coverage = base::sum(ifelse(u[[p[n]]][, i] >=  a[[p[n]]] & l[[p[n]]][, i] <=  a[[p[n]]], 1, 0)) / base::length(u[[p[n]]][, i]))
        return(df)
      }) %>% dplyr::bind_rows() %>% 
        base::t() %>% 
        base::as.data.frame() %>% 
        stats::setNames(base::paste0("coverage", base::colnames(u[[p[n]]])))
      
      
      df <- base::cbind(base::data.frame(partition = n,
                                         model_id = m,
                                         mape = base::mean(base::abs(f[[p[n]]] - a[[p[n]]]) / a[[p[n]]]),
                                         rmse = (base::mean((a[[p[n]]] - f[[p[n]]]) ^ 2)) ^ 0.5,
                                         stringsAsFactors = FALSE), 
                        coverage_df)
      
      return(df)
    }) %>% dplyr::bind_rows()
    
    return(error_df)
  }) %>% stats::setNames(models_df$model_id)
  
  leaderboard <- error_summary %>% dplyr::bind_rows() %>% 
    dplyr::group_by(model_id) %>%
    dplyr::summarise_all(~mean(.)) %>% dplyr::select(-partition) %>%
    dplyr::left_join(models_df %>% 
                       dplyr::select(model_id, model = methods_selected, notes), 
                     by = "model_id") %>%
    dplyr::select(model_id, model, notes, dplyr::everything()) 
  
  base::names(leaderboard) <- c("model_id", 
                                "model", 
                                "notes",
                                base::paste0("avg_", base::names(leaderboard)[4:base::ncol(leaderboard)]))
  
  if(error == "MAPE"){
    leaderboard <- leaderboard %>% dplyr::arrange(avg_mape)
  } else if(error == "RMSE"){
    leaderboard <- leaderboard %>% dplyr::arrange(avg_rmse)
  }
  
  
  output <-   base::list(train = training,
                         forecast = forecast$final_partition,
                         input = input,
                         error_summary = error_summary,
                         leaderboard  = leaderboard,
                         parameters = list(methods = methods,
                                           train_method = train_method,
                                           horizon = horizon,
                                           xreg = xreg,
                                           error_metric = error,
                                           level = level))
  
  
  print(leaderboard)
  class(output) <- "train_model"
  return(output)
  
}

#' Build the Components of the \code{\link[TSstudio]{train_model}} Function 
#' @description  Add, edit, or remove the components of the  \code{\link[TSstudio]{train_model}} function
#' @export
#' @param model.obj The train_model skeleton, created by the create_model 
#' function or edited by add_input, add_methods, remove_methods, add_train_method or add_horizon
#' @param input A univariate time series object (ts class)
#' @param methods A list, defines the models to use for training and forecasting the series. 
#' The list must include a sub list with the model type, and the model's arguments (when applicable) and notes about the model. 
#' The sub-list name will be used as the model ID. Possible models:
#' 
#' \code{\link[stats]{arima}} - model from the stats package 
#' 
#' \code{\link[forecast]{auto.arima}} - model from the forecast package
#' 
#' \code{\link[forecast]{ets}} - model from the forecast package
#' 
#'  \code{\link[stats]{HoltWinters}} - model from the stats package 
#' 
#' \code{\link[forecast]{nnetar}} - model from the forecast package
#'
#' \code{\link[forecast]{tslm}} - model from the forecast package (note that the 'tslm' model must have the formula argument in the 'method_arg' argument)
#' 
#' @param train_method A list, defines the train approach, either using a single testing partition (sample out) 
#' or use multiple testing partitions (backtesting). The list should include the training method argument, (please see 'details' for the structure of the argument)
#' @param method_ids A character, defines the IDs of the model methods to be remove with the remove_methods function
#' @param horizon An integer, defines the forecast horizon
#' @param xreg Optional, a list with two vectors (e.g., data.frame or matrix) of external regressors, 
#' one vector corresponding to the input series and second to the forecast itself 
#' (e.g., must have the same length as the input and forecast horizon, respectively)
#' @param error A character, defines the error metrics to be used to sort the models leaderboard. Possible metric - "MAPE" or "RMSE"
#' @param level An integer, set the  confidence level of the prediction intervals
#' @examples 
#' 
#' ### Building train_model function by adding its different components
#' # Create a skeleton model
#' md <- create_model()
#' 
#' class(md) 
#' 
#' # Add input
#' data(USgas)
#' md <- add_input(model.obj = md, input = USgas)
#' 
#' # Add methods
#' methods <- list(ets1 = list(method = "ets", 
#'                             method_arg = list(opt.crit = "lik"), 
#'                             notes = "ETS model with opt.crit = lik"),
#'                 ets2 = list(method = "ets", 
#'                             method_arg = list(opt.crit = "amse"), 
#'                             notes = "ETS model with opt.crit = amse"),
#'                 arima1 = list(method = "arima", 
#'                               method_arg = list(order = c(1,1,1), 
#'                                         seasonal = list(order = c(1,0,1))), 
#'                               notes = "SARIMA(1,1,1)(1,0,1)"))
#'                               
#' md <- add_methods(model.obj = md, methods = methods)   
#' 

#' # Add additional methods
#' methods2 <- list(arima2 = list(method = "arima", 
#'                               method_arg = list(order = c(2,1,2), 
#'                                      seasonal = list(order = c(1,1,1))), 
#'                               notes = "SARIMA(2,1,2)(1,1,1)"),
#'                 hw = list(method = "HoltWinters", 
#'                           method_arg = NULL, 
#'                           notes = "HoltWinters Model"),
#'                 tslm = list(method = "tslm", 
#'                     method_arg = list(formula = input ~ trend + season), 
#'                     notes = "tslm model with trend and seasonal components"))
#'
#' md <- add_methods(model.obj = md, methods = methods2)
#' 
#' # Remove methods
#' md <- remove_methods(model.obj = md, method_ids = c("ets2", "auto_arima"))  
#'   
#' # Add train method
#' md <- add_train_method(model.obj = md, train_method = list(partitions = 6, 
#'                                                        sample.out = 12, 
#'                                                        space = 3))
#'                                                        
#'                                                        
#' # Set the forecast horizon
#' md <- add_horizon(model.obj = md, horizon = 12)
#' 
#' # Add the forecast prediction intervals confidence level
#' md <- add_level(model.obj = md, level = c(90, 95))
#'                                                         
#' ### Alternatively, pipe the function with the magrittr package  
#'                                                     
#' library(magrittr)
#' 
#' md <- create_model() %>%
#'       add_input(input = USgas) %>%
#'       add_methods(methods = methods) %>%
#'       add_methods(methods = methods2) %>%
#'       add_train_method(train_method = list(partitions = 6, 
#'                                            sample.out = 12, 
#'                                            space = 3)) %>%
#'        add_horizon(horizon = 12) %>%
#'        add_level(level = c(90, 95))
#'        
#' # Run the model
#' fc <- md %>% build_model()  



create_model <- function(){
  model_base <-base::list(input = NULL,
                          methods = NULL,
                          train_method = NULL,
                          horizon = NULL)
  class(model_base) <- "train_model"
  return(model_base)
}

#' @export
#' @rdname create_model
#' 
add_input <- function(model.obj, input){
  `%>%` <- magrittr::`%>%`
  # Error handling 
  # Checking the input object
  if(!stats::is.ts(input)){
    stop("The input argument is not a valid 'ts' object")
  } else if(stats::is.mts(input)){
    stop("Cannot use multiple time series object as input")
  }
  
  # Checking the model.obj 
  if(base::class(model.obj) != "train_model"){
    stop("The 'model.obj' is not valid 'train_model' object")
  } else if("input" %in% base::names(model.obj) && base::is.null(model.obj$input)){
    model.obj$input <- input
  } else if("input" %in% base::names(model.obj) && !base::is.null(model.obj$input)){
    q <- base::readline("The 'model.obj' already has input object, do you want to overwrite it? yes/no ") %>% base::tolower()
    if(q == "y" || q == "yes"){
      model.obj$input <- input
    } else if( q == "n" || q == "no"){
      warning("The 'input' was not added to the model object")
    } else {
      stop("Invalid input...")
    }
  }
  
  return(model.obj)
}


#' @export
#' @rdname create_model
#' 
add_methods <- function(model.obj, methods){
  `%>%` <- magrittr::`%>%`
  method_list <- models_df <- NULL
  method_list <- list("arima", "auto.arima", "ets", "HoltWinters", "nnetar", "tslm")
  # Error handling 
  # Checking the model.obj class
  if(base::class(model.obj) != "train_model"){
    stop("The 'model.obj' is not valid 'train_model' object")
  }
  
  # Validating the methods object
  if(!base::is.list(methods)){
    stop("Error on the 'methods' argument: the argument is not a list")
  } else if(base::is.null(base::names(methods))){
    stop("Error on the 'methods' argument: could not find the models IDs")
  } else if(base::any("NULL" %in% base::as.character(methods %>% purrr::map(~.x[["method"]])))){
    stop("Error on the 'methods' argument: at least one of the methods is missing the 'method' argument")
  } 
  
  if(!base::all(base::as.character(methods %>% purrr::map_chr(~.x[["method"]])) %in% method_list)){
    stop("Error on the 'methods' argument: at least one of the models methods is not valid")
  }
  
  # Adding the metods to the model.obj object
  if(("methods" %in% base::names(model.obj) && base::is.null(model.obj$methods))|| !"methods" %in% base::names(model.obj)){
    model.obj$methods <- methods
    # In case the object has existing methods
  } else if("methods" %in% base::names(model.obj) && !base::is.null(model.obj$methods)) {
    # Validating that object is not exist already
    for(i in base::names(methods)){
      if(i %in% base::names(model.obj$methods)){
        q <- base::readline(base::paste("The", i, "method already exists in the model object, do you wish to overwrite it? yes/no ", sep = " ")) %>% base::tolower()
        if(q == "y" || q == "yes"){
          model.obj$methods[[i]] <- methods[[i]]
        } else{
          warning(base::paste("Method", i, "were not added", sep = " "))
        } 
      } else {
        model.obj$methods[[i]] <- methods[[i]]
      }
    }
    
  }
  return(model.obj)
  
}


#' @export
#' @rdname create_model
#' 
remove_methods <- function(model.obj, method_ids){
  `%>%` <- magrittr::`%>%`
  
  # Error handling 
  # Checking the model.obj class
  if(base::class(model.obj) != "train_model"){
    stop("The 'model.obj' is not valid 'train_model' object")
  }
  
  # Checking the method_ids argument
  if(!is.character(method_ids)){
    stop("The 'method_ids' argument is not valid input")
  }
  
  
  if(!"methods" %in% base::names(model.obj) || base::is.null(model.obj$methods)){
    stop("The input model object does not have any available method")
  } 
  
  for(i in method_ids){
    if(i %in% base::names(model.obj$methods)){
      model.obj$methods[[i]] <- NULL
    } else {
      warning(base::paste("The", i, "does not exist on the model object"))
    }
  }
  
  return(model.obj)
}

#' @export
#' @rdname create_model
#' 
add_train_method <- function(model.obj, train_method){
  `%>%` <- magrittr::`%>%`
  q <- NULL
  # Error handling 
  # Checking the model.obj class
  if(base::class(model.obj) != "train_model"){
    stop("The 'model.obj' is not valid 'train_model' object")
  }
  
  # Checking the train argument
  if(!base::is.list(train_method)){
    stop("Error on the 'train_method' argument: the argument is not a list")
  } else if(!"partitions" %in% base::names(train_method)){
    stop("Error on the 'train_method' argument: the 'partition' argument is missing")
  } else if(!"space" %in% base::names(train_method)){
    stop("Error on the 'train_method' argument: the 'space' argument is missing")
  } else if(!"sample.out" %in% base::names(train_method)){
    stop("Error on the 'train_method' argument: the 'sample.out' argument is missing")
  } else if(!base::is.numeric(train_method$sample.out) || 
            train_method$sample.out < 1 ||
            train_method$sample.out %% 1 != 0){
    stop("Error on the 'train_method' argument: the 'sample.out' argument is not valide, please use a positive integer")
  } else if(!base::is.numeric(train_method$partitions) || 
            train_method$partitions < 1 ||
            train_method$partitions %% 1 != 0){
    stop("Error on the 'train_method' argument:  the 'partitions' argument is not valide, please use a positive integer")
  } else if(!base::is.numeric(train_method$space) || 
            train_method$space < 1 ||
            train_method$space %% 1 != 0){
    stop("Error on the 'train_method' argument:  the 'space' argument is not valide, please use a positive integer")
  } 
  
  
  # Adding the train object
  if(!"train_method" %in% base::names(model.obj) || base::is.null(model.obj$train_method)){
    model.obj$train_method <- train_method
  } else if(!base::is.null(model.obj$train_method)){
    q <- base::readline(base::paste("The model object already has train method, do you wish to overwrite it? (yes) ", sep = " ")) %>% base::tolower()
    if(q == "y" || q == "yes" || q == ""){
      model.obj$train_method <- train_method
    } else{
      warning("Did not update the train method")
    } 
  }
  return(model.obj)
  
}

#' @export
#' @rdname create_model
#' 
add_horizon <- function(model.obj, horizon){
  `%>%` <- magrittr::`%>%`
  
  # Error handling 
  # Checking the model.obj class
  if(base::class(model.obj) != "train_model"){
    stop("The 'model.obj' is not valid 'train_model' object")
  }
  
  if(!"horizon" %in% base::names(model.obj) || base::is.null(model.obj$horizon)){
    model.obj$horizon <- horizon
  } else if(!base::is.null(model.obj$horizon)){
    q <- base::readline(base::paste("The model object already has horizon, do you wish to overwrite it? yes/no ", sep = " ")) %>% base::tolower()
    if(q == "y" || q == "yes"){
      model.obj$horizon <- horizon
    } else{
      warning("No change had made on the model 'horizon' argument")
    }  
  }
  
  return(model.obj)
}

#' @export
#' @rdname create_model
#' 
build_model <- function(model.obj){
  `%>%` <- magrittr::`%>%`
  
  # Error handling 
  # Checking the model.obj class
  if(base::class(model.obj) != "train_model"){
    stop("The 'model.obj' is not valid 'train_model' object")
  }
  
  if(!"horizon" %in% base::names(model.obj) || base::is.null(model.obj$horizon)){
    stop("Cannot build a model, the 'horizon' argument is missing")
  }
  
  if(!"methods" %in% base::names(model.obj) || base::is.null(model.obj$methods)){
    stop("Cannot build a model, the 'methods' argument is missing")
  }
  
  if(!"train_method" %in% base::names(model.obj) || base::is.null(model.obj$train_method)){
    stop("Cannot build a model, the 'train_method' argument is missing")
  }
  
  if(!"input" %in% base::names(model.obj) || base::is.null(model.obj$input)){
    stop("Cannot build a model, the 'input' argument is missing")
  }
  
  
  output <- NULL
  
  output <- TSstudio::train_model(input = model.obj$input,
                                  methods = model.obj$methods,
                                  train_method = model.obj$train_method,
                                  horizon = model.obj$horizon)
  
  return(output)
}

#' @export
#' @rdname create_model
#' 
set_error <- function(model.obj, error){
  `%>%` <- magrittr::`%>%`
  
  # Error handling 
  # Checking the model.obj class
  if(base::class(model.obj) != "train_model"){
    stop("The 'model.obj' is not valid 'train_model' object")
  }
  
  # Check the error argument
  if(base::is.null(error) || !base::is.character(error) || base::length(error) !=1){
    stop("Error on the 'error' argument: the input is not valid, please use either 'RMSE' or 'MAPE'")
  } else if( error != "MAPE" && error != "RMSE"){
    stop("Error on the 'error' argument: the input is not valid, please use either 'RMSE' or 'MAPE'")
  }
  
  if(!"error" %in% base::names(model.obj) ||
     ("error" %in% base::names(model.obj) && base::is.null(model.obj$error))){
    model.obj$error <- error
  } else if("error" %in% base::names(model.obj) && !base::is.null(model.obj$error)){
    q <- base::readline(base::paste("The model object already has 'error' argument, do you wish to overwrite it? (yes) ", sep = " ")) %>% base::tolower()
    if(q == "y" || q == "yes" || q == ""){
      model.obj$error <- error
    } else{
      warning("No change had made on the model 'error' argument")
    }  
  }
  
  return(model.obj)
  
}


#' @export
#' @rdname create_model
#' 

add_xreg <- function(model.obj, xreg){
  `%>%` <- magrittr::`%>%`
  q <- NULL
  # Error handling 
  # Checking the model.obj class
  if(base::class(model.obj) != "train_model"){
    stop("The 'model.obj' is not valid 'train_model' object")
  }
  
  
  # Checking the xreg argument
  if(!base::is.null(xreg)){
    if(!all(c("train", "forecast") %in% base::names(xreg))){
      stop("Error on the 'xreg' argument: the 'xreg' list is not valid, please make sure setting the correspinding regressor",
           " inputs for the 'input' argument (train) and for the forecast horizon (forecast)")
    } else if(base::names(xreg$train) != base::names(xreg$forecast)){
      stop("Error on the 'xreg' argument: the regressors names in the train and forecast inputs are not aligned")
    } 
  }
  
  if(!"xreg" %in% base::names(model.obj) ||
     ("xreg" %in% base::names(model.obj) && base::is.null(model.obj$xreg))){
    model.obj$xreg <- xreg
  } else if("xreg" %in% base::names(model.obj) && !base::is.null(model.obj$xreg)){
    q <- base::readline(base::paste("The model object already has 'xreg' argument, do you wish to overwrite it? (yes) ", sep = " ")) %>% base::tolower()
    if(q == "y" || q == "yes" || q == ""){
      model.obj$xreg <- xreg
    } else{
      warning("No change had made on the model 'xreg' argument")
    }  
  }
  return(model.obj)
}



#' @export
#' @rdname create_model
#' 
add_level <- function(model.obj, level){
  `%>%` <- magrittr::`%>%`
  q <- NULL
 
  # Error handling 
  # Checking the model.obj class
  if(base::class(model.obj) != "train_model"){
    stop("The 'model.obj' is not valid 'train_model' object")
  }
  
  ### Error Handling
  # Check the level argument
  if(base::all(!is.numeric(level)) ||
     base::any(level %% 1 != 0) ||
     base::any(level  <= 0 | level > 100)){
    stop("Error on the 'level' argument: the argument is out of range (0,100]")
  }
  if(!"level" %in% base::names(model.obj) || base::is.null(model.obj$level)){
    model.obj$level <- level
  } else if(!base::is.null(model.obj$level)){
    q <- base::readline(base::paste("The model object already has 'level', do you wish to overwrite it? (yes) ", sep = " ")) %>% base::tolower()
    if(q == "y" || q == "yes" || q == ""){
      model.obj$level <- level
    } else{
      warning("No change had made on the model 'level' argument")
    }  
  }
  
  return(model.obj)
  
}

#' Plot the Models Performance on the Testing Partitions
#' @export
#' @details The plot_model provides a visualization of the models performance on the testing paritions for the train_model function output 
#' @param  model.obj A train_model object
#' @param model_ids A character, defines the trained models to plot, if set to NULL (default), will plot all the models
#' @return Animation of models forecast on the testing partitions compared to the actuals
#' @examples 
#' # Defining the models and their arguments
#' methods <- list(ets1 = list(method = "ets",
#'                             method_arg = list(opt.crit = "lik"),
#'                             notes = "ETS model with opt.crit = lik"),
#'                 ets2 = list(method = "ets",
#'                             method_arg = list(opt.crit = "amse"),
#'                             notes = "ETS model with opt.crit = amse"),
#'                 arima1 = list(method = "arima",
#'                               method_arg = list(order = c(2,1,0)),
#'                               notes = "ARIMA(2,1,0)"),
#'                 arima2 = list(method = "arima",
#'                               method_arg = list(order = c(2,1,2),
#'                                                 seasonal = list(order = c(1,1,1))),
#'                               notes = "SARIMA(2,1,2)(1,1,1)"),
#'                 hw = list(method = "HoltWinters",
#'                           method_arg = NULL,
#'                           notes = "HoltWinters Model"),
#'                 tslm = list(method = "tslm",
#'                             method_arg = list(formula = input ~ trend + season),
#'                             notes = "tslm model with trend and seasonal components"))
#' # Training the models with backtesting
#' md <- train_model(input = USgas,
#'                   methods = methods,
#'                   train_method = list(partitions = 6, 
#'                                       sample.out = 12, 
#'                                       space = 3),
#'                   horizon = 12,
#'                   error = "MAPE")
#' # Plot the models performance on the testing partitions
#' plot_model(model.obj = md)
#' 
#' # Plot only the ETS models
#' plot_model(model.obj = md , model_ids = c("ets1", "ets2))
#' 

plot_model <- function(model.obj, model_ids = NULL){
  `%>%` <- magrittr::`%>%`
  m <- p <- ac_df <- fc_df <- df <- output <- obj_name <- NULL
  
  obj_name <- obj.name <- base::deparse(base::substitute(model.obj))
  # Error handling 
  # Checking the model.obj class
  if(base::class(model.obj) != "train_model"){
    stop("The 'model.obj' is not valid 'train_model' object")
  }
  
  
  
  
  m <-  model.obj$parameters$methods %>% base::names()
  
  if(base::is.null(m)){
    stop("Error on the 'model.obj' argument: cannot find any method in the 'model.obj' argument")
  }
  
  if(!base::is.null(model_ids)){
    if(base::all(model_ids %in% m)){
      stop("Error on the 'model_ids' argument: cannot find some (or all) of the model ids in the 'model.obj' object")
    }
    
    m <- model_ids
  }
  
  
  
  
  p <- model.obj$parameters$train_method$partitions
  ac_df <- base::data.frame(y = rep(base::as.numeric(model.obj$input), p),
                            time = base::rep(base::as.numeric(stats::time(model.obj$input)), p),
                            partition = base::rep(1:p, each = base::length(model.obj$input)),
                            type = "actual")
  
  
  fc_df <-  lapply(m, function(i){
    df1 <- df2 <- NULL
    df1 <- model.obj$train %>% 
      purrr::map(~.x[[i]]) %>% 
      purrr::map(~.x[["forecast"]]) %>% 
      purrr::map(~.x[["mean"]]) %>%
      dplyr::bind_cols()
    
    for(c in 1:base::ncol(df1)){
      temp <- df3 <- NULL
      temp <- df1[, c] %>% 
        as.data.frame() %>% 
        stats::setNames("y") 
      
      df3 <- base::data.frame(y = base::as.numeric(temp$y),
                              time = base::as.numeric(stats::time(temp$y)),
                              partition = c,
                              type = i,
                              stringsAsFactors = FALSE)
      
      df2 <- dplyr::bind_rows(df2, df3)
    }
    
    
    df1 <- model.obj$train %>% 
      purrr::map(~.x[[i]]) %>% 
      purrr::map(~.x[["forecast"]]) %>% 
      purrr::map(~.x[["mean"]]) %>%
      dplyr::bind_cols()
    return(df2)    
    
  }) %>% dplyr::bind_rows()
  
  df <- rbind(fc_df, ac_df)
  
  output <- plotly::plot_ly(data = df,
                            x = ~ time,
                            y = ~ y,
                            split = ~ type,
                            frame = ~ partition,
                            type = 'scatter',
                            mode = 'lines',
                            line = list(simplyfy = F))%>%
    plotly::layout(title = base::paste(obj_name, "Models Performance by Testing Partitions", sep = " "),
                   margin = 50,
                   title = "",
                   xaxis = list(
                     title = "Date",
                     zeroline = F),
                   yaxis = list(
                     title = "",
                     zeroline = F
                   ),
                   font = list(color = "black"),
                   plot_bgcolor = "white",
                   paper_bgcolor = "white"
    ) %>%
    plotly::animation_opts(
      frame = 500,
      transition = 0,
      redraw = F
    ) %>%
    plotly::animation_slider(
      hide = F
    ) %>%
    plotly::animation_button(
      x = 1, xanchor = "right", y = 0, yanchor = "bottom"
    )
  
  return(output)
}


#' Plot the Models Error Metric on the Testing Partitions
#' @export
#' @details The plot_model provides a visualization of the models performance on the testing paritions for the train_model function output 
#' @param  model.obj A train_model object
#' @param error A character, defines the type of error metrics to plot, possible metric - "MAPE" or "RMSE"
#' @param pallete A character, defines the color type to used on the plot, use row.names(RColorBrewer::brewer.pal.info) to view possible color palletes
#' @return A plot with a summery of the models error rate by testing partition
#' @examples 
#' # Defining the models and their arguments
#' methods <- list(ets1 = list(method = "ets",
#'                             method_arg = list(opt.crit = "lik"),
#'                             notes = "ETS model with opt.crit = lik"),
#'                 ets2 = list(method = "ets",
#'                             method_arg = list(opt.crit = "amse"),
#'                             notes = "ETS model with opt.crit = amse"),
#'                 arima1 = list(method = "arima",
#'                               method_arg = list(order = c(2,1,0)),
#'                               notes = "ARIMA(2,1,0)"),
#'                 arima2 = list(method = "arima",
#'                               method_arg = list(order = c(2,1,2),
#'                                                 seasonal = list(order = c(1,1,1))),
#'                               notes = "SARIMA(2,1,2)(1,1,1)"),
#'                 hw = list(method = "HoltWinters",
#'                           method_arg = NULL,
#'                           notes = "HoltWinters Model"),
#'                 tslm = list(method = "tslm",
#'                             method_arg = list(formula = input ~ trend + season),
#'                             notes = "tslm model with trend and seasonal components"))
#' # Training the models with backtesting
#' md <- train_model(input = USgas,
#'                   methods = methods,
#'                   train_method = list(partitions = 6, 
#'                                       sample.out = 12, 
#'                                       space = 3),
#'                   horizon = 12,
#'                   error = "MAPE")
#'                   
#' # Plot the models performance on the testing partitions
#' plot_error(model.obj = md)
#' 

plot_error <- function(model.obj, error = "MAPE", palette = "Set1"){
  `%>%` <- magrittr::`%>%`
  m<- n_colors <- colors_list <- p1 <- p2 <- output <- NULL
  hex_to_rgb <- function(hex){
    rgb <- base::paste0(as.numeric(grDevices::col2rgb(hex) %>% base::t()), collapse = ",")
    return(rgb)
  }
  
  # Error handling 
  # Checking the model.obj class
  if(base::class(model.obj) != "train_model"){
    stop("The 'model.obj' is not valid 'train_model' object")
  }
  
  # Checking the error argument
  if(error != "MAPE" && error != "RMSE"){
    stop("Error on the 'error' argument: in valid error metric, can use either 'MAPE' or 'RMSE'")
  }
  
  
  m <- unique(error_df$model_id)
  palette_list <- base::row.names(RColorBrewer::brewer.pal.info)
  if(base::length(palette) != 1 || !palette %in% palette_list){
    stop("Error on the 'palette' argument: cannot find the color palette on the RColorBrewer palettes list, ", 
         "use row.names(RColorBrewer::brewer.pal.info) to view possible color palettes")
  }
  
  n_colors <- RColorBrewer::brewer.pal.info$maxcolors[row.names(RColorBrewer::brewer.pal.info)  == palette]
  colors_list <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(n_colors, palette))(base::length(m))
  
  p1 <- plotly::plot_ly()
  p2 <- plotly::plot_ly()
  
  if(error == "MAPE"){
    for(i in base::seq_along(m)){
      df <- NULL
      
      df <- error_df %>% dplyr::filter(model_id == m[i])
      
      p1 <- p1 %>% plotly::add_lines(x = df$partition, y = df$mape * 100, name = m[i],
                                     showlegend = TRUE,
                                     legendgroup = m[i], 
                                     line = list(color = colors_list[i])) 
      p2 <- p2 %>% plotly::add_trace(y = df$mape * 100, name = m[i], 
                                     type = "box", 
                                     fillcolor = base::paste("rgba(", hex_to_rgb(colors_list[i]), ", 0.5)", sep = ""),
                                     line = list(color = colors_list[i]),
                                     marker = list(color = colors_list[i]),
                                     boxpoints = "all", 
                                     jitter = 0.3,
                                     pointpos = -1.8,
                                     showlegend = FALSE,
                                     legendgroup = m[i])
      
      
      
      
      
    }
    p1 <- p1 %>% plotly::layout(yaxis = list(title = "MAPE", ticksuffix = '%'),
                                xaxis = list(title = "Partition"))
    p2 <- p2 %>% plotly::layout(yaxis = list(title = "MAPE", ticksuffix = '%'),
                                xaxis = list(title = "Partition"))
    
    output <- plotly::subplot(p1, p2, nrows = 1, shareY = T) %>% 
      plotly::layout(title = "Model Performance by Testing Partition - MAPE")
  } else if(error == "RMSE"){
    for(i in base::seq_along(m)){
      df <- NULL
      
      df <- error_df %>% dplyr::filter(model_id == m[i])
      
      p1 <- p1 %>% plotly::add_lines(x = df$partition, y = df$rmse, name = m[i],
                                     showlegend = TRUE,
                                     legendgroup = m[i], 
                                     line = list(color = colors_list[i])) 
      p2 <- p2 %>% plotly::add_trace(y = df$rmse, name = m[i], 
                                     type = "box", 
                                     fillcolor = base::paste("rgba(", hex_to_rgb(colors_list[i]), ", 0.5)", sep = ""),
                                     line = list(color = colors_list[i]),
                                     marker = list(color = colors_list[i]),
                                     boxpoints = "all", 
                                     jitter = 0.3,
                                     pointpos = -1.8,
                                     showlegend = FALSE,
                                     legendgroup = m[i])
      
      
      
      
      
    }
    p1 <- p1 %>% plotly::layout(yaxis = list(title = "RMSE"),
                                xaxis = list(title = "Partition"))
    p2 <- p2 %>% plotly::layout(yaxis = list(title = "RMSE"),
                                xaxis = list(title = "Partition"))
    
    output <- plotly::subplot(p1, p2, nrows = 1, shareY = T) %>% 
      plotly::layout(title = "Model Performance by Testing Partition - RMSE")
  }
  return(output)
}



