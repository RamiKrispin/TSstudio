#'  Evaluation Function for Forecasting Models
#' @export ts_evaluate
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
#' @param h Integer, the horizon of the forecast
#' @description Performance evaluation function for forecasting models, by training and testing the performance
#' of each model over a sequence of periods to identify the performance of a model over time  
#' (both accuracy and stability)
#' @examples
#' data(USgas)
#' ts_evaluate(USgas, periods = 12, h = 12)

# the ts_evaluate function ####

ts_evaluate <- function(ts.obj, 
                        models = "abehntw", 
                        periods = 6, 
                        error = "MAPE", 
                        h = 3,
                        seed=1234,
                        a.arg = NULL,
                        b.arg = NULL,
                        e.arg = NULL,
                        h.arg = NULL,
                        n.arg = NULL,
                        t.arg = NULL,
                        w.arg = NULL){

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
  if((base::length(ts.obj) - periods - h) < 2 * stats::frequency(ts.obj)){
    stop("The length of the series is long enough to create a forecast")
  }
}

if(!base::is.numeric(h) | h != base::round(h) | h <= 0){
  stop("The value of the 'h' parameters is no valid")
} else {
  if((base::length(ts.obj) - periods - h) < 2 * stats::frequency(ts.obj)){
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
  warning("The 'error' parameter is invalid, using the default setting - 'MAPE'")
  error <- "MAPE"
} 

# Setting the output object
modelOutput <- list()

# Define the plot colors
color_ramp <- RColorBrewer::brewer.pal(base::nchar(models),"Dark2")

model_char <-  base::unlist(base::strsplit(models, split = ""))



if("a" %in% model_char){
  model_list <- c(model_list, "AUTO.ARIMA")
  md_AUTO.ARIMA <- fc_AUTO.ARIMA <- NULL
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
  md_TBATS <- base::do.call(forecast::tbats, c(list(ts.obj), t.arg))
  fc_TBATS <- forecast::forecast(md_TBATS, h = h)
  modelOutput$Models_Final <- list(TBATS = md_TBATS)
  modelOutput$Forecast_Final <- list(TBATS = fc_TBATS)
}

if("b" %in% model_char){
  model_list <- c(model_list, "BSTS")
  md_BSTS <- fc_BSTS <- train.bs <- ss <- fit.bsts <- burn <-  NULL
  train.bs <- base::data.frame(as.numeric(ts.obj))
  base::names(train.bs) <- c("obj")
  ss <- bsts::AddLocalLinearTrend(list(), train.bs$obj)
  ss <- bsts::AddSeasonal(ss, train.bs$obj, nseasons = stats::frequency(ts.obj))
  md_BSTS <- bsts::bsts(train.bs$obj, state.specification = ss, 
                          niter = 700, ping=0, seed= seed)
  burn <- bsts::SuggestBurn(0.1, md_BSTS)
  fc_BSTS <- stats::predict(md_BSTS, horizon = h, 
                            burn = burn, quantiles = c(.025, .975))
  modelOutput$Models_Final <- list(BSTS = md_BSTS)
  modelOutput$Forecast_Final <- list(BSTS = fc_BSTS)
}

if("h" %in% model_char){
  model_list <- c(model_list, "Hybrid")
  md_Hybrid <- fc <- NULL
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
split_ts <- TSstudio::ts_split(ts.subset, sample.out = h)
train <- split_ts$train
test <- split_ts$test

if("a" %in% model_char){
md <- fc <- NULL
md <- base::do.call(forecast::auto.arima, c(list(train), a.arg))
fc <- forecast::forecast(md, h = h)
MAPE_df$AUTO.ARIMA[i - s + 1] <-  base::round(forecast::accuracy(fc,test)[10], 2)
RMSE_df$AUTO.ARIMA[i - s + 1] <-  base::round(forecast::accuracy(fc,test)[4], 2)
eval(parse(text = paste("modelOutput$", period_name, "$auto.arima <- list(model = md, forecast = fc)", sep = ""))) 
}

if("w" %in% model_char){
md <- fc <- NULL
md <- stats::HoltWinters(train, alpha = 0.01, beta = 0.01, gamma = 0.01)
fc <- forecast::forecast(md, h = h)
MAPE_df$HoltWinters[i - s + 1] <- base::round(forecast::accuracy(fc, test)[10], 2)
RMSE_df$HoltWinters[i - s + 1] <- base::round(forecast::accuracy(fc, test)[4], 2)
eval(parse(text = paste("modelOutput$", period_name, "$HoltWinters <- list(model = md, forecast = fc)", sep = ""))) 
}

if("e" %in% model_char){
md <- fc <- NULL
md <- forecast::ets(train)
fc <- forecast::forecast(train, h = h)
MAPE_df$ETS[i - s + 1] <-  base::round(forecast::accuracy(fc, test)[10], 2)
RMSE_df$ETS[i - s + 1] <-  base::round(forecast::accuracy(fc, test)[4], 2)
eval(parse(text = paste("modelOutput$", period_name, "$ets <- list(model = md, forecast = fc)", sep = "")))
}


if("n" %in% model_char){
md <- fc <- NULL
md <- forecast::nnetar(train)
fc <- forecast::forecast(md, h = h)
MAPE_df$NNETAR[i - s + 1] <-  base::round(forecast::accuracy(fc, test)[10],2)
RMSE_df$NNETAR[i - s + 1] <-  base::round(forecast::accuracy(fc, test)[4],2)
eval(parse(text = paste("modelOutput$", period_name, "$nnetar <- list(model = md, forecast = fc)", sep = "")))
}

if("t" %in% model_char){
md <- fc <- NULL
md <- forecast::tbats(train)
fc <- forecast::forecast(md, h = h)
MAPE_df$TBATS[i - s + 1] <-  base::round(forecast::accuracy(fc, test)[10], 2)
RMSE_df$TBATS[i - s + 1] <-  base::round(forecast::accuracy(fc, test)[4], 2)
eval(parse(text = paste("modelOutput$", period_name, "$tbats <- list(model = md, forecast = fc)", sep = "")))
}

if("b" %in% model_char){
md <- fc <- train.bs <- test.bs <-  NULL
train.bs <- base::data.frame(as.numeric(train))
test.bs <- base::data.frame(as.numeric(test))
base::names(train.bs) <- c("obj")
base::names(test.bs) <- c("obj")
ss <- bsts::AddLocalLinearTrend(list(), train.bs$obj)
ss <- bsts::AddSeasonal(ss, train.bs$obj, nseasons = frequency(train))
fit.bsts <- bsts::bsts(train, state.specification = ss, 
                 data = train.bs , niter = 700, ping=0, seed=1234)
burn <- bsts::SuggestBurn(0.1, fit.bsts)
fc.bsts <- stats::predict(fit.bsts, horizon = h, 
                   burn = burn, quantiles = c(.025, .975))


pred <- base::as.numeric(fc.bsts$mean)
MAPE_df$BSTS[i - s + 1] <- base::round(mean(100 * base::abs((pred - test) / pred)), 2)
RMSE_df$BSTS[i - s + 1] <- base::round((mean((pred - test)^ 2)) ^ 0.5, 2)
}

if("h" %in% model_char){
  md <- forecastHybrid::hybridModel(train)
  fc <- forecast::forecast(md, h = h)
  eval(parse(text = paste("modelOutput$", period_name, "$hybrid <- list(model = md, forecast = fc)", sep = "")))
  MAPE_df$Hybrid[i - s + 1] <-  base::round(forecast::accuracy(fc, test)[10], 2)
  RMSE_df$Hybrid[i - s + 1] <-  base::round(forecast::accuracy(fc, test)[4], 2)
}

if((i -s + 1) > 1){
p1 <- p2 <- p3 <- NULL

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
                            yaxis = list(title = error),
                            xaxis = list(title = "Period", tickvals = MAPE_df[, 1], ticktext = MAPE_df[, 1]))
p2 <- p2 %>% plotly::layout(title = "Error Distribution by Model",
                            yaxis = list(title = error))
p3 <- plotly::subplot(p1, p2, nrows = 2, titleY = TRUE, titleX = TRUE, margin = 0.06)


print(p3)


}
}


modelOutput$MAPE_score <- MAPE_df
modelOutput$RMSE_score <- RMSE_df
modelOutput$score_plot <- p3

leaderboard <- (modelOutput$MAPE_score %>% reshape2::melt(id.vars = c("Period")) %>%
  dplyr::group_by(variable) %>%
  dplyr::summarise(avgMAPE = mean(value),
                   sdMAPE = sd(value))) %>%
  dplyr::left_join(
    modelOutput$RMSE_score %>% reshape2::melt(id.vars = c("Period")) %>%
      dplyr::group_by(variable) %>%
      dplyr::summarise(avgRMSE = mean(value),
                       sdRMSE = sd(value)) 
  )
names(leaderboard)[1] <- "Model_Name"
if(error == "MAPE"){
  leaderboard <- leaderboard %>% dplyr::arrange(avgMAPE)
} else {
  leaderboard <- leaderboard %>% dplyr::arrange(avgRMSE)
}
modelOutput$leaderboard <- leaderboard
modelOutput$leadModel <- 
return(modelOutput)
}

