# app
library(shiny)
library(RQuantLib)
library(bizdays)
library(quantmod)
library(xts)
library(forecast)
library(callr)
library(pkgbuild)

algorithm_names <- c("Holt's Exponential Smoothing")
load_quantlib_calendars('UnitedStates/NYSE',from='2000-01-01', to='2020-11-03')

investing_options <- c("Apple", "Google")
symbols <- c("AAPL", "GOOG")

for (symbol in symbols) {
  getSymbols(symbol, src = "yahoo")
}


ui <- fluidPage(
  headerPanel('Header Panel'),
  sidebarPanel(
    "Inputs section title 1",
    dateInput('start_date', 'Start date', value = "2019-01-01"),
    dateInput('end_date', 'End date', value = "2019-12-31"),
    checkboxGroupInput('selected_stocks', "Selected Stocks",
                       investing_options),
    numericInput('init_capital', 'Initial amount to invest ($MXN)', min = 0, value = 100)
  ),
  sidebarPanel(
    "Inputs section title 2",
    selectInput('forecast_alg', 'Holt Exponential Smoothing', algorithm_names),
    numericInput('h_holt', 'Prediction Horizon', min = 1, value = 20)
  ),
  mainPanel(
    plotOutput('plot1'),
    plotOutput('plot2'),
    plotOutput('plot3')
  )
)

server <- function(input, output) {
  
  start_ranges <- reactive({
    c(
      bizdays::bizdays(index(AAPL[1]), input$start_date, 'QuantLib/UnitedStates/NYSE')+1,
      bizdays::bizdays(index(GOOG[1]), input$start_date, 'QuantLib/UnitedStates/NYSE')
      )
  })
  
  end_ranges <- reactive({
    c(
      start_ranges()[1] + bizdays::bizdays(input$start_date, input$end_date, 'QuantLib/UnitedStates/NYSE')-1,
      start_ranges()[2] + bizdays::bizdays(input$start_date, input$end_date, 'QuantLib/UnitedStates/NYSE')-1
    )
  })
  
  selected_h <- reactive({
    input$h_holt
  })
  
  output$plot1 <- renderPlot({
    
  })
  
  output$plot1 <- renderPlot({
    if (length(input$selected_stocks) > 0 ){
      #Apple
      if (is.element("Apple", input$selected_stocks)) {
        fitted_stock <- HoltWinters(AAPL$AAPL.Adjusted[c(start_ranges()[2]:end_ranges()[2])], gamma=FALSE)
        stock_forecast <- forecast:::forecast.HoltWinters(fitted_stock, h=selected_h())
        point_predictions <- stock_forecast$mean
        returns_aapl <- mapply(function(x,y) {(y-x)/x}, point_predictions, dplyr::lead(point_predictions,1))
  
        starting_aapl <- input$init_capital/length(input$selected_stocks)
        current <- starting_aapl
        portfolio_prediction_aapl <- c(starting_aapl)
        for (return in returns_aapl) {
          current <- current*(1+return)
          portfolio_prediction_aapl <- append(portfolio_prediction_aapl, current)
          print(portfolio_prediction_aapl)
        }
      } else {
        portfolio_prediction_aapl = c(1:input$h_holt)*0
      }
      #Google
      if (is.element("Google", input$selected_stocks)) {
        fitted_stock <- HoltWinters(GOOG$GOOG.Adjusted[c(start_ranges()[2]:end_ranges()[2])], gamma=FALSE)
        stock_forecast <- forecast:::forecast.HoltWinters(fitted_stock, h=selected_h())
        point_predictions <- stock_forecast$mean
        returns_goog <- mapply(function(x,y) {(y-x)/x}, point_predictions, dplyr::lead(point_predictions,1))
        
        starting_goog <- input$init_capital/length(input$selected_stocks)
        current <- starting_goog
        portfolio_prediction_goog <- c(starting_goog)
        for (return in returns_goog) {
          current <- current*(1+return)
          portfolio_prediction_goog <- append(portfolio_prediction_goog, current)
        }
      } else {
        portfolio_prediction_goog = c(1:input$h_holt)*0
      }
      print(portfolio_prediction_goog)
      plot(portfolio_prediction_aapl+portfolio_prediction_goog)
    }
  })
  
  output$plot2 <- renderPlot({
    if (length(input$selected_stocks) > 0 ) {
      if (is.element("Apple", input$selected_stocks)) {
        fitted_stock <- HoltWinters(AAPL$AAPL.Adjusted[c(start_ranges()[2]:end_ranges()[2])], gamma=FALSE)
        stock_forecast <- forecast:::forecast.HoltWinters(fitted_stock, h=selected_h())
        forecast:::plot.forecast(stock_forecast, main = "Apple")
      }
    }
  })
  
  output$plot3 <- renderPlot({
    if (length(input$selected_stocks) > 0 ) {
      if (is.element("Google", input$selected_stocks)) {
        fitted_stock <- HoltWinters(GOOG$GOOG.Adjusted[c(start_ranges()[2]:end_ranges()[2])], gamma=FALSE)
        stock_forecast <- forecast:::forecast.HoltWinters(fitted_stock, h=selected_h())
        forecast:::plot.forecast(stock_forecast, main = "Google")
      }
    }
    
  })
  
}

shinyApp(ui = ui, server = server)
