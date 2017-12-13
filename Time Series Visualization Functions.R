rm(list = ls())

# Load libraries
library(reshape2)
library(plotly)
library(dplyr)
library(xts)

# Load multiple time series formats
ts1 <- AirPassengers #ts
xts1 <- as.xts(AirPassengers) #xts
zoo1 <- as.zoo(AirPassengers) #zoo


class(ts1)
class(xts1)
class(zoo1)



x <- zoo1
x <- xts1
x <- ts1
ts_plotly <- function(x, line.mode = "lines", width = 1, dash = NULL, color = "blue", slider = FALSE){
if(is.zoo(x) | is.xts(x)){
df <- data.frame(date = zoo::index(x), y = as.numeric(x))
} else if (is.ts(x)){
df <- data.frame(date = time(x), y = as.numeric(x))
} else {
  stop('Invalid class \n Please make sure the object class is either "ts", "xts" or "zoo"')
}
p <- NULL  
p <-  switch (line.mode,
    "markers" = {
      plot_ly(data = df, x = ~ date, y = ~y, 
              mode = "markers", 
              type = 'scatter',
              marker = list(color = color, width = width)
      )
    },
    "lines+markers" = {
      plot_ly(data = df, x = ~ date, y = ~y, 
              mode = "lines+markers", 
              type = 'scatter',
              line = list(width = width, dash = dash, color = color)
      )
    },
    "lines" = {
      plot_ly(data = df, x = ~ date, y = ~y, 
              mode = "lines", 
              type = 'scatter',
              line = list(width = width, dash = dash, color = color)
      )
    }
      
  )

if(!is.null(p) & slider){
  p <- p %>% 
    layout(
      xaxis = list(
        rangeslider = list(type = "date"))
      )
}

return(p)
}



line_mode <- "lines"
line_mode <- "lines+markers"
line_mode <- "markers"
width <- 1
dash <- "dot"
dash <- "dash"
dash <- NULL
plot_ly(data = df, x = ~ date, y = ~y, 
        mode = line_mode, 
        line = list(width = width, dash = dash))



plot_ly(data = df, x = ~ date, y = ~y, 
        mode = "lines+markers", 
        type = 'scatter',
        line = list(width = width, dash = dash, color = "red")
        )

plot_ly(data = df, x = ~ date, y = ~y, 
        mode = "markers", 
        type = 'scatter',
        marker = list(color = "green")
        #line = list(width = width, dash = dash)
)

dates <- seq.Date(from = lubridate::ymd("2017-01-01"), to = lubridate::ymd("2017-12-31"), by = "days")

xts2 <- as.xts(x = data.frame(x = runif(n = 365, min = 0, max = 1), 
                              y = runif(n = 365, min = 1, max = 2)),
               order.by = dates)
zoo2 <- as.zoo(xts2)
x <- xts2
class(x)

 