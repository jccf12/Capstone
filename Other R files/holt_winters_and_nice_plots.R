#install.packages("quantmod")
#install.packages('bizdays')
#install.packages("RQuantLib")
#install.packages("TTR")
#install.packages("Mcomp")
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

#selecting sub-interval of data given dates
init <- as.Date("2019-01-01")
end <- as.Date("2019-12-31")
start_range <- bizdays::bizdays(index(GOOG[1]), init, 'QuantLib/UnitedStates/NYSE')+1
end_range <- start_range + bizdays::bizdays(init, end, 'QuantLib/UnitedStates/NYSE')-1

#fitting data using HoltWinters exponential smoothing
fitted_stock <- HoltWinters(GOOG$GOOG.Adjusted[c(start_range:end_range)], gamma=FALSE)
stock_forecast <- forecast:::forecast.HoltWinters(fitted_stock, h=20)
forecast:::plot.forecast(stock_forecast)

?HoltWinters

#predictions and returns based on those predictions
point_predictions <- stock_forecast$mean
returns <- mapply(function(x,y) {(y-x)/x}, point_predictions, dplyr::lead(point_predictions,1))

#plotting google prices
ggplot(GOOG, aes(x = index(GOOG), y = GOOG[,'GOOG.Adjusted'])) + 
  geom_line(color = "darkblue") + ggtitle("Google prices series") + 
  xlab("Date") + ylab("Price") + theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_date(date_labels = "%b %y", date_breaks = "2 years")

#calculating moving averages
goog_mm <- subset(GOOG, index(GOOG) >= "2018-01-01")

goog_mm10 <- rollmean(goog_mm[,6], 10, fill = list(NA, NULL, NA), align = "right")
goog_mm30 <- rollmean(goog_mm[,6], 30, fill = list(NA, NULL, NA), align = "right")

goog_mm$mm10 <- coredata(goog_mm10)
goog_mm$mm30 <- coredata(goog_mm30)

#plotting moving averages
ggplot(goog_mm, aes(x = index(goog_mm))) +
  geom_line(aes(y = goog_mm[,6], color = "goog")) + ggtitle("Petrobras prices series") +
  geom_line(aes(y = goog_mm$mm10, color = "MM10")) +
  geom_line(aes(y = goog_mm$mm30, color = "MM30")) + xlab("Date") + ylab("Price") +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank()) +
  scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
  scale_colour_manual("Series", values=c("goog"="gray40", "MM10"="firebrick4", "MM30"="darkcyan"))

table(head(GOOG))


