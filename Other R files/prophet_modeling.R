library(prophet)
library(quantmod)
library(data.table)
library(ggplot2)
library(dplyr)

symbols <- c("AAPL")

data <- hash()

for (symbol in symbols) {
  data[symbol] <- getSymbols(symbol, src = "yahoo", auto.assign = FALSE)
}

##INPUTS IN THE APP
start_date <- as.Date("2016-01-01")
end_date <- as.Date("2018-12-31")
input_seasonal <- TRUE
h <- 20
##_________

my_symbol <- "AAPL"
mydf <- data[[my_symbol]][seq(start_date, end_date, "days")]
#mydf <- getSymbols(my_symbol, src = "yahoo", from = input$date, to = "2016-12-30", auto.assign = FALSE)
mydf <- data.frame(mydf[,6])
setDT(mydf, keep.rownames = TRUE)
colnames(mydf) <- c("ds", "y")
m <- prophet(mydf, yearly.seasonality = input_seasonal, weekly.seasonality = FALSE)
future <- make_future_dataframe(m, periods = 50)
forecast <- predict(m, future)

mydf2 <- data[[my_symbol]][seq(start_date,end_date+h, "days")]
#mydf2 <- getSymbols(my_symbol, src = "yahoo", from = input$date, to = "2017-06-23", auto.assign = FALSE)
mydf2 <- data.frame(mydf2[,6])
colnames(mydf2) <- "Price"
mydf2$ds <- rownames(mydf2)
rownames(mydf2) <- NULL
mydf2$ds <- as.Date(mydf2$ds)
forecast$ds <- as.Date(forecast$ds)
joint_dfs = left_join(mydf2, forecast, by="ds")
rownames(joint_dfs) <- joint_dfs$ds
joint_dfs$PriceX <- joint_dfs$Price

joint_dfs$Price[joint_dfs$ds > "2016-12-30"] = NA
joint_dfs$PriceX[joint_dfs$ds < "2017-01-01"] = NA


priceref = joint_dfs$Price[joint_dfs$ds == "2016-12-30"]
yhatend = tail(joint_dfs$yhat,1)
priceend = tail(joint_dfs$PriceX,1)

theme_set(theme_gray(base_size = 18))

if (((yhatend>priceref) && (priceend>priceref)) | ((yhatend<priceref) && (priceend<priceref))) {
  ggplot(joint_dfs, aes(x=ds)) + geom_point(aes(y=Price), size=1) + geom_point(aes(y=PriceX), col="forestgreen", size=1) +
    geom_line(aes(y=yhat)) + geom_ribbon(aes(ymin=yhat_lower, ymax=yhat_upper), fill="deepskyblue", alpha=0.5) +
    geom_line(aes(y=priceref), cex=0.8, col="steelblue4") +
    geom_vline(aes(xintercept=as.numeric(as.Date("2017-01-01"))), cex=0.8, col="steelblue4")+
    ggtitle(my_symbol)
    }
else {
  ggplot(joint_dfs, aes(x=ds)) + geom_point(aes(y=Price)) + geom_point(aes(y=PriceX), col="firebrick1", size=3) +
    geom_line(aes(y=yhat)) + geom_ribbon(aes(ymin=yhat_lower, ymax=yhat_upper), fill="deepskyblue", alpha=0.5) +
    geom_line(aes(y=priceref), cex=0.8, col="steelblue4") +
    geom_vline(aes(xintercept=as.numeric(as.Date("2017-01-01"))), cex=0.8, col="steelblue4")+
    ggtitle(my_symbol)
  }

