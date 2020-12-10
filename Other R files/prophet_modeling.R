library(prophet)
library(quantmod)
library(data.table)
library(ggplot2)
library(dplyr)

input_sym <- "AAPL"
input_date <- as.Date("2013-01-01")
input_seasonal <- FALSE

mydf <- getSymbols(input_sym, src = "yahoo", from = input_date, to = "2016-12-30", auto.assign = FALSE)

mydf <- data.frame(mydf[,6])
setDT(mydf, keep.rownames = TRUE)
colnames(mydf) <- c("ds", "y")
m <- prophet(mydf, yearly.seasonality = input_seasonal, weekly.seasonality = FALSE)
future <- make_future_dataframe(m, periods = 177)
forecast <- predict(m, future)
mydf2 <- getSymbols(input_sym, src = "yahoo", from = input_date, to = "2017-06-23", auto.assign = FALSE)
mydf2 <- data.frame(mydf2[,6])
colnames(mydf2) <- "Price"
mydf2$ds <- rownames(mydf2)
rownames(mydf2) <- NULL
mydf2$ds <- as.Date(mydf2$ds)
mydf2$ds
forecast$ds <- as.Date(forecast$ds)
perry = left_join(mydf2, forecast, by="ds")
rownames(perry) <- perry$ds
perry$PriceX <- perry$Price

perry$Price[perry$ds > "2016-12-30"] = NA
perry$PriceX[perry$ds < "2017-01-01"] = NA


priceref = perry$Price[perry$ds == "2016-12-30"]
yhatend = tail(perry$yhat,1)
priceend = tail(perry$PriceX,1)

theme_set(theme_gray(base_size = 18))

if (((yhatend>priceref) && (priceend>priceref)) | ((yhatend<priceref) && (priceend<priceref))) {
  ggplot(perry, aes(x=ds)) + geom_point(aes(y=Price)) + geom_point(aes(y=PriceX), col="forestgreen", size=3) +
    geom_line(aes(y=yhat)) + geom_ribbon(aes(ymin=yhat_lower, ymax=yhat_upper), fill="deepskyblue", alpha=0.5) +
    geom_line(aes(y=priceref), cex=0.8, col="steelblue4") +
    geom_vline(aes(xintercept=as.numeric(as.Date("2017-01-01"))), cex=0.8, col="steelblue4")}
else {
  ggplot(perry, aes(x=ds)) + geom_point(aes(y=Price)) + geom_point(aes(y=PriceX), col="firebrick1", size=3) +
    geom_line(aes(y=yhat)) + geom_ribbon(aes(ymin=yhat_lower, ymax=yhat_upper), fill="deepskyblue", alpha=0.5) +
    geom_line(aes(y=priceref), cex=0.8, col="steelblue4") +
    geom_vline(aes(xintercept=as.numeric(as.Date("2017-01-01"))), cex=0.8, col="steelblue4")}
