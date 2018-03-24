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


ts_evaluate <- function(ts.obj, models = "abehntw", periods = 6, error = "MAPE", h = 3){

`%>%` <- magrittr::`%>%`
  
a <- model_list <- model_char <- color_ramp <- NULL

for(s in 1:nchar(models)){
  if(!substr(models, s, s) %in% c("a", "w", "e", "n", "t", "b", "h")){
    stop("The 'models' argument is not valide")
  }
}

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
  a <- 10
} else if(error == "MAPE"){
  a <- 10
} else if(error == "RMSE"){
  a <- 4
}




color_ramp <- RColorBrewer::brewer.pal(base::nchar(models),"Dark2")
model_char <-  base::unlist(base::strsplit(models, split = ""))


if("a" %in% model_char){
  model_list <- c(model_list, "AUTO.ARIMA")
}

if("w" %in% model_char){
  model_list <- c(model_list, "HoltWinters")
}

if("e" %in% model_char){
  model_list <- c(model_list, "ETS")
}

if("n" %in% model_char){
  model_list <- c(model_list, "NNETAR")
}

if("t" %in% model_char){
  model_list <- c(model_list, "TBATS")
}

if("b" %in% model_char){
  model_list <- c(model_list, "BSTS")
}

if("h" %in% model_char){
  model_list <- c(model_list, "Hybrid")
}


s <- length(ts.obj) - periods + 1
e <- length(ts.obj)
score_df <- NULL
score_df <- data.frame(matrix(NA, ncol = length(model_list) + 1 , nrow = periods))

names(score_df) <- c("Period", model_list)
score_df$Period <- s:e - s + 1

for(i in s:e){

ts.subset <- train <- test <- NULL
ts.subset <- stats::window(ts.obj, start = time(ts.obj)[1], end = time(ts.obj)[i])
split_ts <- TSstudio::ts_split(ts.subset, sample.out = h)
train <- split_ts$train
test <- split_ts$test

if("a" %in% model_char){
md1 <- forecast::auto.arima(train, stepwise = FALSE)
fc1 <- forecast::forecast(md1, h = h)
score_df$AUTO.ARIMA[i - s + 1] <-  base::round(forecast::accuracy(fc1,test)[a], 2)
}

if("w" %in% model_char){
md2 <- stats::HoltWinters(train, alpha = 0.01, beta = 0.01, gamma = 0.01)
fc2 <- forecast::forecast(md2, h = h)
score_df$HoltWinters[i - s + 1] <- base::round(forecast::accuracy(fc2, test)[a], 2)
}

if("e" %in% model_char){
md3 <- forecast::ets(train)
fc3 <- forecast::forecast(train, h = h)
score_df$ETS[i - s + 1] <-  base::round(forecast::accuracy(fc3, test)[a], 2)
}


if("n" %in% model_char){
md4 <- forecast::nnetar(train)
fc4 <- forecast::forecast(md4, h = h)
score_df$NNETAR[i - s + 1] <-  base::round(forecast::accuracy(fc4, test)[a],2)
}

if("t" %in% model_char){
md5 <- forecast::tbats(train)
fc5 <- forecast::forecast(md5, h = h)
score_df$TBATS[i - s + 1] <-  base::round(forecast::accuracy(fc5, test)[a], 2)
}

if("b" %in% model_char){
train.bs <- base::data.frame(as.numeric(train))
test.bs <- base::data.frame(as.numeric(test))
base::names(train.bs) <- c("obj")
base::names(test.bs) <- c("obj")
ss <- bsts::AddLocalLinearTrend(list(), train.bs$obj)
ss <- bsts::AddSeasonal(ss, train.bs$obj, nseasons = frequency(train))
fit.bsts <- bsts::bsts(train, state.specification = ss, 
                 data = train.bs , niter = 700, ping=0, seed=1234)
burn <- bsts::SuggestBurn(0.1, fit.bsts)
fc.bsts <- stats::predict(fit.bsts, newdata = test.bs, horizon = h, 
                   burn = burn, quantiles = c(.025, .975))


pred <- base::as.numeric(fc.bsts$mean)
if(error == "MAPE"){
score_df$BSTS[i - s + 1] <- base::round(mean(100 * base::abs((pred - test) / pred)), 2)
} else if(error == "RMSE"){
  score_df$BSTS[i - s + 1] <- base::round((mean((pred - test)^ 2)) ^ 0.5, 2)
}
}

if("h" %in% model_char){
  md6 <- forecastHybrid::hybridModel(train)
  fc6 <- forecast::forecast(md6, h = h)
  score_df$Hybrid[i - s + 1] <-  base::round(forecast::accuracy(fc6, test)[a], 2)
}

if((i -s + 1) > 1){
p1 <- p2 <- p3 <- NULL

p1 <- plotly::plot_ly(data = score_df) 

for(r1 in 2:ncol(score_df)){
  p1 <- p1 %>% plotly::add_lines(x = score_df[, 1], 
                                 y = score_df[, r1], 
                                 name = names(score_df)[r1], 
                                 line = list(color = color_ramp[(r1 -1)]))
}
  
p1 <- p1 %>% plotly::layout(xaxis = list(range = c(min(score_df$Period), max(score_df$Period))))

p2 <- plotly::plot_ly(data = score_df)

for(r2 in 2:base::ncol(score_df)){
 p2 <- p2 %>% plotly::add_trace(y = score_df[, r2], 
                    type = "box", 
                    boxpoints = "all", 
                    jitter = 0.3,
                    pointpos = -1.8, 
                    name =  names(score_df)[r2], 
                    marker = list(color = color_ramp[(r2 -1)]),
                    line = list(color = color_ramp[(r2 -1)]),
                    showlegend=F
                    )
}

p1 <- p1 %>% plotly::layout(title = "Error by Period",
                            yaxis = list(title = error),
                            xaxis = list(title = "Period"))
p2 <- p2 %>% plotly::layout(title = "Error Distribution by Model",
                            yaxis = list(title = error))
p3 <- plotly::subplot(p1, p2, nrows = 2, titleY = TRUE, titleX = TRUE, margin = 0.06)


print(p3)


}
}
}

