x <- c(100:1)
my_func <- function(x) {
  0.6*x+20*sin(x*pi/10)
}
y <- my_func(x)
plot(y_xts)
x_xts <- xts(x, order.by=Sys.Date()-1:100)
y_xts <- my_func(x_xts)

fitted <- HoltWinters(y_xts, gamma=FALSE)
forecast <- forecast:::forecast.HoltWinters(fitted, h=20)
forecast:::plot.forecast(forecast))

lines(fitted(fitted)[,1], col = 2)
plot(fitted)
y_ts <- ts(y_xts, frequency = 20)
plot(y_ts)

m <- HoltWinters(y_ts, gamma=TRUE)
p <- predict(m, 10, prediction.interval = TRUE)
plot(m, p)

?HoltWinters

aapl <- getSymbols("AAPL", src = "yahoo", from = "2016-01-01", to = Sys.Date(), auto.assign = FALSE)
aapl[seq(as.Date("2016-08-17"), as.Date("2016-08-29"), "days")]
