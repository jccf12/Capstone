library(shiny)
library(quantmod)
library(xts)
library(forecast)
library(callr)
library(pkgbuild)
library(hash)
library(grid)
library(gridExtra)
library(ggplot2)
library(data.table)
library(prophet)
library(dplyr)
library(shinycssloaders)
library(timeDate)
library(plotly)
library(gridExtra)
library(ggthemes)
library(RColorBrewer)
library(readr)
library(shinythemes)

?HoltWinters
?prophet

max_plots <- 20 #max plots to display
my_colors <- brewer.pal(n = 12, name = "Paired") #colors for single stocks that compose the porfolio

algorithm_names <- c("Holt's Exponential Smoothing","Prophet Time Series Model")
risk_levels <- c('Manual Risk Portfolio', 'Low Risk Portfolio', 'Medium Risk Portfolio', 'High Risk Portfolio')
seasonality_types <- c('additive','multiplicative')
symbols <- scan("data/top50_market_cap_usa_names.txt", what = 'character', sep = "\n") #company symbols
symbols1 <- scan("data/top50_market_cap_usa.txt", what = 'character') #company names

syms_lo_risk <- c()
syms_me_risk <- c()
syms_hi_risk <- c()

data <- hash() #data dictionary that will contain all the retireved data from the Yahoo API

#retrieving Yahoo data
i <- 1
for (symbol in symbols1) {
  data[symbols[i]] <- getSymbols(symbol, src = "yahoo", auto.assign = FALSE)
  std <- sd(yearlyReturn(data[[symbols[i]]]))
  if (!is.na(std)) {
    if ( std < 0.25 ) {
      syms_lo_risk <- append(syms_lo_risk, symbols[i])
    } 
    else if (std >= 0.25 && 0.45 > std) {
      syms_me_risk <- append(syms_me_risk, symbols[i])
    }
    else if (std >= 0.45) {
      syms_hi_risk <- append(syms_hi_risk, symbols[i])
    }
  }
  i <- i+1
}
