install.packages("quantmod")
install.packages('bizdays')
install.packages("RQuantLib")
install.packages("TTR")
install.packages("Mcomp")
library(RQuantLib)
library(bizdays)
library(quantmod)
library(xts)
library(forecast)
library(ggplot2)
require(smooth)
require(Mcomp)
library(TTR)

load_quantlib_calendars('UnitedStates/NYSE',from='2000-01-01', to='2020-11-03')
getSymbols("GOOG", src ="yahoo")

init <- as.Date("2019-01-01")
end <- as.Date("2019-12-31")
start_range <- bizdays::bizdays(index(GOOG[1]), init, 'QuantLib/UnitedStates/NYSE')+1
end_range <- start_range + bizdays::bizdays(init, end, 'QuantLib/UnitedStates/NYSE')-1

fitted_stock <- HoltWinters(GOOG$GOOG.Adjusted[c(start_range:end_range)], gamma=FALSE)
stock_forecast <- forecast:::forecast.HoltWinters(fitted_stock, h=20)
forecast:::plot.forecast(stock_forecast)

point_predictions <- stock_forecast$mean
(tail(point_predictions)[1] - point_predictions[1]) / point_predictions[1]
returns <- mapply(function(x,y) {(y-x)/x}, point_predictions, dplyr::lead(point_predictions,1))

returns
dplyr::lag(c(1,2,3),n=1)
?lag
returns <- 
?mapply()
stock_forecast$lower[,2]

aapl <- getSymbols("GOOG", src = "yahoo", from = init_date, to = end_date, auto.assign = FALSE )
#chartSeries(AAPL)
summary(aapl)


init <- as.Date("2019-01-01")
end <- as.Date("2019-12-31")
start_range <- bizdays::bizdays(init_date, init, 'QuantLib/UnitedStates/NYSE')+1
end_range <- start_range + bizdays::bizdays(init, end, 'QuantLib/UnitedStates/NYSE')-1

mean(dailyReturn(AAPL))

selected_data <- aapl[,6][c(start_range:end_range)]
plot(selected_data)
sma_ttr <- TTR::SMA(selected_data,n=10)
forecast::forecast(sma_ttr)

plot(forecast:::forecast(sma_ttr))

plot(sma_aapl)
plot(sma_aapl$forecast)
aapl$AAPL.Adjusted[c(start_range:end_range)]

holtwinters_stock <- HoltWinters(aapl$AAPL.Adjusted[c(start_range:end_range)], gamma=FALSE)
holtwinters_stock
plot(holtwinters_stock)

holtwinters_stock$fitted
print(holtwinters_stock)

forecast_stock <- forecast:::forecast.HoltWinters(holtwinters_stock, h=20)
plot(forecast_stock)
print(forecast_stock)

plot(SMA(aapl$AAPL.Adjusted[c(start_range:end_range)],n=10))

ggplot(aapl, aes(x = index(aapl), y = aapl[,6])) + 
  geom_line(color = "darkblue") + ggtitle("Apple prices series") + 
  xlab("Date") + ylab("Price") + theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_date(date_labels = "%b %y", date_breaks = "2 years")

aapl_mm <- subset(aapl, index(aapl) >= "2019-01-01")

aapl_mm102 <- rollmean(aapl_mm[,6], 10, fill = list(NA, NULL, NA), align = "right")
aapl_mm30 <- rollmean(aapl_mm[,6], 30, fill = list(NA, NULL, NA), align = "right")

?rollmean

aapl_mm$mm10 <- coredata(aapl_mm10)
aapl_mm$mm30 <- coredata(aapl_mm30)

ggplot(aapl_mm, aes(x = index(aapl_mm))) +
  geom_line(aes(y = aapl_mm[,6], color = "aapl")) + ggtitle("Apple prices series") +
  geom_line(aes(y = aapl_mm$mm10, color = "MM10")) +
  geom_line(aes(y = aapl_mm$mm30, color = "MM30")) + xlab("Date") + ylab("Price") +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank()) +
  scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
  scale_colour_manual("Series", values=c("aapl"="gray40", "MM10"="firebrick4", "MM30"="darkcyan"))



?HoltWinters
