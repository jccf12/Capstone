library(prophet)
library(quantmod)
library(data.table)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  titlePanel("Playing with Prophet on Financial Time Series (II)"),
  fluidRow(
    column(3, wellPanel(
      h3("\"Prediction is very difficult, especially about the future.\""), 
      helpText("Prophet is a procedure for forecasting time series data developed by Facebook's Core Data Science team."),
      helpText("Using this app you can compare de Prophet prediction for financial time series (from Yahoo Finance) with the real prices"),
      helpText("You can choose the initial date for the train period (ending December 30th, 2016). We forecast price for June 23rd, 2017 (Green color same direction, red color different)."),
      dateInput("date", "Initial Date:", "2014-01-01", max="2016-01-01"),
      textInput("sym", "Symbol (Yahoo Finance!)", "AAPL"),
      helpText("Nice Examples: TLT (4 years), AMZN (5), BND (3)"),
      helpText("Ugly Examples: XOM (3 years), CVX (4), NKE (5)"),
      checkboxInput("seasonal","Use yearly seasonal factor:", TRUE),
      h3("Links"),
      p(tags$a(href="https://mydata.shinyapps.io/ShinyProphetV2/", "Playing with Prophet on Financial Time Series")),
      p(tags$a(href="https://research.fb.com/prophet-forecasting-at-scale", "Research Facebook Prophet")),
      p(tags$a(href="https://facebookincubator.github.io/prophet", "Github")),
      p(tags$a(href="https://cran.r-project.org/web/packages/prophet/index.html", "Prophet R Package")),
      helpText("Data from Yahoo Finance!")
    )),
    column(6,
           plotOutput("plot1", width = "1217px", height = "800px")
    )
  )
)
server <- function(input, output) {
  output$plot1 <- renderPlot({
    mydf <- getSymbols(input$sym, src = "yahoo", from = input$date, to = "2016-12-30", auto.assign = FALSE)
    
    mydf <- data.frame(mydf[,6])
    setDT(mydf, keep.rownames = TRUE)
    colnames(mydf) <- c("ds", "y")
    m <- prophet(mydf, yearly.seasonality = input$seasonal, weekly.seasonality = FALSE)
    future <- make_future_dataframe(m, periods = 177)
    forecast <- predict(m, future)
    mydf2 <- getSymbols(input$sym, src = "yahoo", from = input$date, to = "2017-06-23", auto.assign = FALSE)
    mydf2 <- data.frame(mydf2[,6])
    colnames(mydf2) <- "Price"
    mydf2$ds <- rownames(mydf2)
    rownames(mydf2) <- NULL
    mydf2$ds <- as.Date(mydf2$ds)
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
    
  })
}

shinyApp(ui = ui, server = server)
