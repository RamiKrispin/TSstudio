seasonal_ly <- function(ts.obj){
df <- df_wide <- p <- obj.name <- NULL

obj.name <- base::deparse(base::substitute(ts.obj))

if(stats::is.ts(ts.obj)){
  if(stats::is.mts(ts.obj)){
    warning('The "ts.obj" has multiple columns, only the first column will be plot')
    ts.obj <- ts.obj[,1]
  }
  df <- data.frame(dec_left = floor(time(ts.obj)),
                   dec_right =  cycle(ts.obj), 
                   value = as.numeric(ts.obj)
  )
  
} else if(xts::is.xts(ts.obj) | zoo::is.zoo(ts.obj)){
  if(!is.null(base::dim(ts.obj))){
    if(base::dim(ts.obj)[2] > 1){
      warning('The "ts.obj" has multiple columns, only the first column will be plot')
      ts.obj <- ts.obj[,1]
    }
  }
  freq <- xts::periodicity(ts.obj)[[6]]
  if(freq == "quarterly" ){
    df <- data.frame(dec_left = lubridate::year(ts.obj),
                        dec_right =  lubridate::quarter(ts.obj), 
                        value = as.numeric(ts.obj)
    )
  } else if(freq == "monthly" ){
    df <- data.frame(dec_left = lubridate::year(ts.obj),
                     dec_right =  lubridate::month(ts.obj), 
                     value = as.numeric(ts.obj)
    )
  } else if(freq == "weekly"){
    df <- data.frame(dec_left = lubridate::year(ts.obj),
                        dec_right =  lubridate::week(ts.obj), 
                        value = as.numeric(ts.obj)
    ) 
  } else if(freq == "daily"){
    df <- data.frame(dec_left = lubridate::month(ts.obj),
                        dec_right =  lubridate::day(ts.obj), 
                        value = as.numeric(ts.obj)
    ) 
  } else if(freq != "quarterly" & freq != "monthly" & 
            freq != "weekly" & freq != "daily"){
    stop('The frequency of the series is invalid, the function support only "daily" or "monthly" frequencies')
  }
  
}

df_wide <- reshape2::dcast(df, dec_right ~ dec_left )

p <- plotly::plot_ly()

for(f in 2:ncol(df_wide)){
  p <- p %>% plotly::add_trace(x = df_wide[,1], y = df_wide[,f],
                       name = names(df_wide)[f],
                       mode = "lines",
                       type = "scatter")}
p <- p %>%  plotly::layout(
  title = paste("Seasonality Plot", sep = " "),
  xaxis = list(title = "", autotick = F, dtick = 1),
  yaxis = list(title = obj.name))

return(p)
}
