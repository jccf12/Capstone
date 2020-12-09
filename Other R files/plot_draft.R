output$plot1 <- renderPlot({
  if (length(input$selected_stocks) > 0 ){
    #Apple
    if (is.element("AAPL", input$selected_stocks)) {
      fitted_stock <- HoltWinters(data[['APPL']]$AAPL.Adjusted[c(start_ranges()[2]:end_ranges()[2])], gamma=FALSE)
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
    if (is.element("GOOG", input$selected_stocks)) {
      fitted_stock <- HoltWinters(data[['GOOG']]$GOOG.Adjusted[c(start_ranges()[2]:end_ranges()[2])], gamma=FALSE)
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