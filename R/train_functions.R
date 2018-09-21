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
#' @param a.arg List, an optional arguments to pass to the \code{\link[forecast]{auto.arima}} function
#' @param b.arg List, an optional arguments to pass to the \code{\link[bsts]{bsts}} function 
#' @param e.arg List, an optional argument to pass to the \code{\link[forecast]{ets}} function
#' @param h.arg List, an optional argument to pass to the \code{\link[forecastHybrid]{hybridModel}} function
#' @param n.arg List, an optional argument to pass to the \code{\link[forecast]{nnetar}} function
#' @param t.arg List, an optional arguments to pass to the \code{\link[forecast]{tbats}} function
#' @param w.arg List, an optional arguments to pass to the \code{\link[stats]{HoltWinters}} function
#' @param parallel Logical, if TRUE use parallel option when applicable (auto.arima, hybridModel)
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
color_ramp <- RColorBrewer::brewer.pal(base::nchar(models),"Dark2")

model_char <-  base::unlist(base::strsplit(models, split = ""))
modelOutput$Models_Final <- list()
modelOutput$Forecast_Final <- list()

# Final forecast
if("a" %in% model_char){
  model_list <- c(model_list, "auto.arima")
  md_auto.arima <- fc_auto.arima <- NULL
  a.arg$parallel <- parallel
  md_auto.arima <- base::do.call(forecast::auto.arima, c(list(ts.obj), a.arg))
  fc_auto.arima <- forecast::forecast(md_auto.arima, h = h)
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
  fc_nnetar <- forecast::forecast(md_nnetar, h = h)
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
                   ping = 100,
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
  fc_hybrid <- forecast::forecast(md_hybrid, h = h)
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
md <- base::do.call(forecast::auto.arima, c(list(train), a.arg))
fc <- forecast::forecast(md, h = window_size)
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
md <- base::do.call(forecast::nnetar, c(list(train), n.arg))
fc <- forecast::forecast(md, h = window_size)
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
  md <- base::do.call(forecastHybrid::hybridModel, c(list(train), h.arg))
  fc <- forecast::forecast(md, h = window_size)
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
return(modelOutput)
}

