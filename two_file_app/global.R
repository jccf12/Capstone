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

max_plots <- 20
#my_colors <- c("cornsilk3",'coral1','chocolate1','chartreuse4','chartreuse','cadetblue4','cadetblue','brown4',
               #'blueviolet','dodgerblue4','dodgerblue1','dimgray','deepskyblue1','deeppink4','deeppink','darkviolet',
               #'darkturquoise','darkslateblue','darkseagreen3','darkorchid','orange1','tomato1','olivedrab4','red4','red1','tan')
my_colors <- brewer.pal(n = 12, name = "Paired")
algorithm_names <- c("Holt's Exponential Smoothing","Prophet Time Series Model")
risk_levels <- c('Manual Risk Portfolio', 'Low Risk Portfolio', 'Medium Risk Portfolio', 'High Risk Portfolio')
symbols <- scan("data/top50_market_cap_usa_mex.txt", what = 'character')

syms_lo_risk <- c()
syms_me_risk <- c()
syms_hi_risk <- c()

data <- hash()
my_string <- read_file("data/example.txt")

for (symbol in symbols) {
  data[symbol] <- getSymbols(symbol, src = "yahoo", auto.assign = FALSE)
  std <- sd(yearlyReturn(data[[symbol]]))
  if (!is.na(std)) {
    if ( std < 0.25 ) {
      syms_lo_risk <- append(syms_lo_risk, symbol)
    } 
    else if (std >= 0.25 && 0.45 > std) {
      syms_me_risk <- append(syms_me_risk, symbol)
    }
    else if (std >= 0.45) {
      syms_hi_risk <- append(syms_hi_risk, symbol)
    }
  }
}
