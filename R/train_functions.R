#'  Evaluation Function for Forecasting Models
#' @export ts_backtesting
#' @param ts.obj A univariate time series object of a class "ts"
#' @param models String, define the type of models to use in the training function:
#'  'a' - auto.arima (forecast package)
#'  'b' - Bayesian Structural Time Series (bsts package)
#'  'e' - ETS (forecast package) 
#'  'h' - Hybrid (forecastHybrid package) 
#'  'n' - Neural Network Time Series (forecast package)
#'  't' - TBATS (forecast package)
#'  'w' - Holt Winters (stats package)
#' @param periods The number of periods to evaluate the models (with a minimum of 2)
#' @param error The type of error to evaluate by - "MAPE"  (default) or "RMSE"
#' @param h_training An integer, the horizon each model should be tested 
#' @param h Integer, the horizon of the selected forecasting model
#' @param plot Logical, if TRUE desplay a plot with the backtesting progress
#' @param a.arg List, an optional arguments to pass to the auto.arima function
#' @param b.arg List, an optional arguments to pass to the bsts function
#' @param e.arg List, an optional arguments to pass to the ets function
#' @param h.arg List, an optional arguments to pass to the hybridModel function
#' @param n.arg List, an optional arguments to pass to the nnetar function
#' @param t.arg List, an optional arguments to pass to the tbats function
#' @param w.arg List, an optional arguments to pass to the Holtwinters function
#' @param parallel Logical, if TRUE use parallel option when applicable (auto.arima, hybridModel)
#' @description Performance evaluation function for forecasting models, by training and testing the performance
#' of each model over a sequence of periods to identify the performance of a model over time  
#' (both accuracy and stability)
#' @examples
#' \dontrun{
#' data(USgas)
#' ts_evaluate(USgas, periods = 12, h = 12)
#' }

# the ts_evaluate function ####

ts_backtesting <- function(ts.obj, 
                        models = "abehntw", 
                        periods = 6, 
                        error = "MAPE", 
                        h_training = 3,
                        h = 3,
                        plot = TRUE,
                        a.arg = NULL,
                        b.arg = list(linear_trend = TRUE,
                                     seasonal = TRUE,
                                     niter = 1000,
                                     ping = 100,
                                     family = "gaussian",
                                     seed=1234),
                        e.arg = NULL,
                        h.arg = NULL,
                        n.arg = NULL,
                        t.arg = NULL,
                        w.arg = NULL,
                        parallel = FALSE){

`%>%` <- magrittr::`%>%`
  
a <- model_list <- model_char <- color_ramp <- forecast_list <- NULL

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
  if((base::length(ts.obj) - periods - h_training) < 2 * stats::frequency(ts.obj)){
    stop("The length of the series is long enough to create a forecast")
  }
}

if(!base::is.numeric(h_training) | h_training != base::round(h_training) | h_training <= 0){
  stop("The value of the 'h_training' parameters is no valid")
} else {
  if((base::length(ts.obj) - periods - h_training) < 2 * stats::frequency(ts.obj)){
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



if("a" %in% model_char){
  model_list <- c(model_list, "AUTO.ARIMA")
  md_AUTO.ARIMA <- fc_AUTO.ARIMA <- NULL
  a.arg$parallel <- parallel
  md_AUTO.ARIMA <- base::do.call(forecast::auto.arima, c(list(ts.obj), a.arg))
  fc_AUTO.ARIMA <- forecast::forecast(md_AUTO.ARIMA, h = h)
  modelOutput$Models_Final <- list(auto.arima = md_AUTO.ARIMA)
  modelOutput$Forecast_Final <- list(auto.arima = fc_AUTO.ARIMA)

}

if("w" %in% model_char){
  model_list <- c(model_list, "HoltWinters")
  md_HoltWinters <- fc_HoltWinters <- NULL
  md_HoltWinters <- base::do.call(stats::HoltWinters, c(list(ts.obj), w.arg))
  fc_HoltWinters <- forecast::forecast(md_HoltWinters, h = h)
  modelOutput$Models_Final <- list(HoltWinters = md_HoltWinters)
  modelOutput$Forecast_Final <- list(HoltWinters = fc_HoltWinters)
}

if("e" %in% model_char){
  model_list <- c(model_list, "ETS")
  md_ETS <- fc_ETS <- NULL
  md_ETS <- base::do.call(forecast::ets, c(list(ts.obj), e.arg))
  fc_ETS <- forecast::forecast(md_ETS, h = h)
  modelOutput$Models_Final <- list(ETS = md_ETS)
  modelOutput$Forecast_Final <- list(ETS = fc_ETS)
}

if("n" %in% model_char){
  model_list <- c(model_list, "NNETAR")
  md_NNETAR <- fc_NNETAR <- NULL
  md_NNETAR <- base::do.call(forecast::nnetar, c(list(ts.obj), n.arg))
  fc_NNETAR <- forecast::forecast(md_NNETAR, h = h)
  modelOutput$Models_Final <- list(NNETAR = md_NNETAR)
  modelOutput$Forecast_Final <- list(NNETAR = fc_NNETAR)
}

if("t" %in% model_char){
  model_list <- c(model_list, "TBATS")
  md_TBATS <- fc_TBATS <- NULL
  t.arg$use.parallel <- parallel
  md_TBATS <- base::do.call(forecast::tbats, c(list(ts.obj), t.arg))
  fc_TBATS <- forecast::forecast(md_TBATS, h = h)
  modelOutput$Models_Final <- list(TBATS = md_TBATS)
  modelOutput$Forecast_Final <- list(TBATS = fc_TBATS)
}

if("b" %in% model_char){
  model_list <- c(model_list, "BSTS")
  md_BSTS <- fc_BSTS <- ss <- fit.bsts <- burn <-  NULL
  ss <- list()
  if(b.arg$linear_trend){
    ss <- bsts::AddLocalLinearTrend(ss, ts.obj) 
  }
  if(b.arg$seasonal){
    ss <- bsts::AddSeasonal(ss, ts.obj, 
                            nseasons = stats::frequency(ts.obj))
  }
 
  md_BSTS <- bsts::bsts(ts.obj, 
                        state.specification = ss, 
                        niter = b.arg$niter, 
                        ping= b.arg$ping, 
                        seed= b.arg$seed,
                        family = b.arg$family)
  fc_BSTS <- stats::predict(md_BSTS, horizon = h, quantiles = c(.025, .975))
  modelOutput$Models_Final <- list(BSTS = md_BSTS)
  modelOutput$Forecast_Final <- list(BSTS = fc_BSTS)
}

if("h" %in% model_char){
  model_list <- c(model_list, "Hybrid")
  md_Hybrid <- fc_Hybrid <- NULL
  h.arg$parallel <- parallel
  md_Hybrid <- base::do.call(forecastHybrid::hybridModel, c(list(ts.obj), h.arg))
  fc_Hybrid <- forecast::forecast(md_Hybrid, h = h)
  modelOutput$Models_Final <- list(Hybrid = md_Hybrid)
  modelOutput$Forecast_Final <- list(Hybrid = fc_Hybrid)
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
split_ts <- TSstudio::ts_split(ts.subset, sample.out = h_training)
train <- split_ts$train
test <- split_ts$test

if("a" %in% model_char){
md <- fc <- NULL
md <- base::do.call(forecast::auto.arima, c(list(train), a.arg))
fc <- forecast::forecast(md, h = h_training)
MAPE_df$AUTO.ARIMA[i - s + 1] <-  base::round(forecast::accuracy(fc,test)[10], 2)
RMSE_df$AUTO.ARIMA[i - s + 1] <-  base::round(forecast::accuracy(fc,test)[4], 2)
eval(parse(text = paste("modelOutput$", period_name, "$auto.arima <- list(model = md, forecast = fc)", sep = ""))) 
}

if("w" %in% model_char){
md <- fc <- NULL
md <- base::do.call(stats::HoltWinters, c(list(train), w.arg))
fc <- forecast::forecast(md, h = h_training)
MAPE_df$HoltWinters[i - s + 1] <- base::round(forecast::accuracy(fc, test)[10], 2)
RMSE_df$HoltWinters[i - s + 1] <- base::round(forecast::accuracy(fc, test)[4], 2)
eval(parse(text = paste("modelOutput$", period_name, "$HoltWinters <- list(model = md, forecast = fc)", sep = ""))) 
}

if("e" %in% model_char){
md <- fc <- NULL
md <- base::do.call(forecast::ets, c(list(train), e.arg))
fc <- forecast::forecast(train, h = h_training)
MAPE_df$ETS[i - s + 1] <-  base::round(forecast::accuracy(fc, test)[10], 2)
RMSE_df$ETS[i - s + 1] <-  base::round(forecast::accuracy(fc, test)[4], 2)
eval(parse(text = paste("modelOutput$", period_name, "$ets <- list(model = md, forecast = fc)", sep = "")))
}


if("n" %in% model_char){
md <- fc <- NULL
md <- base::do.call(forecast::nnetar, c(list(train), n.arg))
fc <- forecast::forecast(md, h = h_training)
MAPE_df$NNETAR[i - s + 1] <-  base::round(forecast::accuracy(fc, test)[10],2)
RMSE_df$NNETAR[i - s + 1] <-  base::round(forecast::accuracy(fc, test)[4],2)
eval(parse(text = paste("modelOutput$", period_name, "$nnetar <- list(model = md, forecast = fc)", sep = "")))
}

if("t" %in% model_char){
md <- fc <- NULL
md <- base::do.call(forecast::tbats, c(list(train), t.arg))
fc <- forecast::forecast(md, h = h_training)
MAPE_df$TBATS[i - s + 1] <-  base::round(forecast::accuracy(fc, test)[10], 2)
RMSE_df$TBATS[i - s + 1] <-  base::round(forecast::accuracy(fc, test)[4], 2)
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

fc <- stats::predict(md, horizon = h_training, quantiles = c(.025, .975))


pred <- fc$mean
MAPE_df$BSTS[i - s + 1] <- base::round(mean(100 * base::abs((pred - test) / pred)), 2)
RMSE_df$BSTS[i - s + 1] <- base::round((mean((pred - test)^ 2)) ^ 0.5, 2)
}

if("h" %in% model_char){
  md <- fc <- NULL
  md <- base::do.call(forecastHybrid::hybridModel, c(list(train), h.arg))
  fc <- forecast::forecast(md, h = h_training)
  eval(parse(text = paste("modelOutput$", period_name, "$hybrid <- list(model = md, forecast = fc)", sep = "")))
  MAPE_df$Hybrid[i - s + 1] <-  base::round(forecast::accuracy(fc, test)[10], 2)
  RMSE_df$Hybrid[i - s + 1] <-  base::round(forecast::accuracy(fc, test)[4], 2)
}

if((i -s + 1) > 1){
p1 <- p2 <- p3 <- p4 <- p5 <- p6 <-NULL

p1 <- plotly::plot_ly(data = MAPE_df) 

for(r1 in 2:ncol(MAPE_df)){
  p1 <- p1 %>% plotly::add_lines(x = MAPE_df[, 1], 
                                 y = MAPE_df[, r1], 
                                 name = names(MAPE_df)[r1], 
                                 line = list(color = color_ramp[(r1 -1)]))
}
  
p1 <- p1 %>% plotly::layout(xaxis = list(tickvals = MAPE_df[, 1], ticktext = MAPE_df[, 1],
                                         range = c(min(MAPE_df$Period), max(MAPE_df$Period))))

p2 <- plotly::plot_ly(data = MAPE_df)

for(r2 in 2:base::ncol(MAPE_df)){
 p2 <- p2 %>% plotly::add_trace(y = MAPE_df[, r2], 
                    type = "box", 
                    boxpoints = "all", 
                    jitter = 0.3,
                    pointpos = -1.8, 
                    name =  names(MAPE_df)[r2], 
                    marker = list(color = color_ramp[(r2 -1)]),
                    line = list(color = color_ramp[(r2 -1)]),
                    showlegend=F
                    )
}

p1 <- p1 %>% plotly::layout(title = "Error by Period",
                            yaxis = list(title = "MAPE"),
                            xaxis = list(title = "Period", tickvals = MAPE_df[, 1], ticktext = MAPE_df[, 1]))
p2 <- p2 %>% plotly::layout(title = "Error Distribution by Model",
                            yaxis = list(title = "MAPE"))
p3 <- plotly::subplot(p1, p2, nrows = 2, titleY = TRUE, titleX = TRUE, margin = 0.06)

p4 <- plotly::plot_ly(data = RMSE_df) 

for(r1 in 2:ncol(RMSE_df)){
  p4 <- p4 %>% plotly::add_lines(x = RMSE_df[, 1], 
                                 y = RMSE_df[, r1], 
                                 name = names(RMSE_df)[r1], 
                                 line = list(color = color_ramp[(r1 -1)]))
}

p4 <- p4 %>% plotly::layout(xaxis = list(tickvals = RMSE_df[, 1], ticktext = RMSE_df[, 1],
                                         range = c(min(RMSE_df$Period), max(RMSE_df$Period))))

p5 <- plotly::plot_ly(data = RMSE_df)

for(r2 in 2:base::ncol(RMSE_df)){
  p5 <- p5 %>% plotly::add_trace(y = RMSE_df[, r2], 
                                 type = "box", 
                                 boxpoints = "all", 
                                 jitter = 0.3,
                                 pointpos = -1.8, 
                                 name =  names(RMSE_df)[r2], 
                                 marker = list(color = color_ramp[(r2 -1)]),
                                 line = list(color = color_ramp[(r2 -1)]),
                                 showlegend=F
  )
}

p4 <- p4 %>% plotly::layout(title = "Error by Period",
                            yaxis = list(title = "RMSE"),
                            xaxis = list(title = "Period", tickvals = RMSE_df[, 1], ticktext = RMSE_df[, 1]))
p5 <- p5 %>% plotly::layout(title = "Error Distribution by Model",
                            yaxis = list(title = "RMSE"))
p6 <- plotly::subplot(p4, p5, nrows = 2, titleY = TRUE, titleX = TRUE, margin = 0.06)

if(error == "MAPE" & plot){
print(p3)
} else if(error == "RMSE" & plot){
  print(p6)
}
}
}


modelOutput$MAPE_score <- MAPE_df
modelOutput$RMSE_score <- RMSE_df
modelOutput$MAPE_plot <- p3
modelOutput$RMSE_plot <- p6

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
if(error == "MAPE"){
  leaderboard <- leaderboard %>% dplyr::arrange(avgMAPE)
} else {
  leaderboard <- leaderboard %>% dplyr::arrange(avgRMSE)
}
modelOutput$leaderboard <- leaderboard
print(leaderboard)
return(modelOutput)
}

