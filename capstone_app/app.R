# app
library(shiny)
library(RQuantLib)
library(bizdays)
library(quantmod)
library(xts)
library(forecast)
library(callr)
library(pkgbuild)
library(hash)
library(grid)
library(gridExtra)

max_plots <- 20

algorithm_names <- c("Holt's Exponential Smoothing","Prophet Time Series Model")
load_quantlib_calendars('UnitedStates/NYSE',from='2000-01-01', to='2020-12-10')

symbols <- scan("/Users/juan/Documents/GitHub/Capstone/capstone_app/data/top50_market_cap_usa_mex.txt", what = 'character')

data <- hash()

for (symbol in symbols) {
  data[symbol] <- getSymbols(symbol, src = "yahoo", auto.assign = FALSE)
}
# -----------------# -----------------# -----------------# -----------------# -----------------
# -----------------# -----------------# _______UI________# -----------------# -----------------
# -----------------# -----------------# -----------------# -----------------# -----------------

ui <- fluidPage(
  tabsetPanel(
    id = 'main-tabs',
    tabPanel(
      id = 'analysis-tab',
      title = 'Análisis',
      headerPanel('Análisis de datos'),
      sidebarPanel(
        "Training period",
        dateInput('start_date', 'Fecha inicial', value = "2016-01-01", max = Sys.Date()),
        dateInput('end_date', 'Fecha final', value = "2018-12-31", max = Sys.Date()),
        #tags$h1("Search Input"),
        #br(),
        #helpText("Puedes econtrar los símbolos en la siguiente página"),
        #p(tags$a(href="https://es-us.finanzas.yahoo.com/acciones-mas-populares", "Finanzas Yahoo")),
        #br(),
        #textInput("sym", "Symbol (Yahoo Finance!)", "AAPL"),
        checkboxInput("seasonal","Use yearly seasonal factor:", TRUE),
        "Pronósticos",
        selectInput('forecast_alg', 'Algoritmo de Predicción', algorithm_names),
        numericInput('h', 'Horizonte de Predicción', min = 1, value = 60),
        wellPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 300px",
                  checkboxGroupInput('selected_stocks', "Selección de stocks para visualización",
                           symbols, selected = symbols[1:1])),
        numericInput('init_capital', 'Capital de inversión inicial ($MXN)', min = 0, value = 100)
      ),
      mainPanel(
        id = 'inner-main-analysis',
        tabsetPanel(
          id = 'analysis-inner-tab',
          tabPanel(
            id = 'data-panel',
            title = 'Datos',
            tableOutput('table1'),
            icon = icon("table")
          ),
          tabPanel(
            id = 'visualization-panel',
            title = 'Visualización',
            uiOutput('plots'),
            icon = icon("chart-line")
          )
        ) # <- end tabsetPanel 'analysis-inner-tab'
      ) # <- end mainPanel 'inner-main-analysis'
    ) # <-- end tabPanel 'analysis-tab'
  ) # <-- end tabsetPanel 'main-tabs'
) # <-- end fluid Page


# ---------------------# ---------------------# ---------------------# ---------------------# ---------------------
# ---------------------# ---------------------# _______SERVER________# ---------------------# --------------------- 
# ---------------------# ---------------------# ---------------------# ---------------------# ---------------------


server <- function(input, output) {

  # ____________________ DATOS ____________________
  
  output$table1 <- renderTable({
    output_table1 <- NULL
    for (symbol in input$selected_stocks) {
      last2 <- tail(data[[symbol]],2)
      names(last2) <- c("open","high","low","close","volume","adjusted")
      diff_abs <- as.numeric(last2[2,"adjusted"])-as.numeric(last2[1,"adjusted"])
      change_abs <- xts(diff_abs+0:0, order.by = as.Date(index(last2[2,]))+0:0)
      diff_pct <- diff_abs/as.numeric(last2[1,"adjusted"])
      change_pct <- xts(diff_pct+0:0, order.by = as.Date(index(last2[2,]))+0:0)
      new_row <- cbind(last2[2,"adjusted"],change_abs,change_pct)
      new_row_df <- data.frame(date = index(new_row), coredata(new_row))
      new_row_df['symbol'] = symbol
      names(new_row_df) <- c("Last Date","Last Price", "Change", "Change (%)", "Symbol")
      new_row_df <- new_row_df[c("Symbol","Last Price", "Change", "Change (%)", "Last Date")]
      output_table1 <- rbind(output_table1,new_row_df)
    }
    return(output_table1)
  })
  
  # ____________________ VISUALIZACION ____________________
  
  output$plots <- renderUI({
    plot_output_list <- lapply(1:length(input$selected_stocks), function(i) {
      plotname <- paste("plot", i, sep="")
      plotOutput(plotname, height = 400, width = 1050)
    })
    
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, plot_output_list)
  })
  
  # Call renderPlot for each one. Plots are only actually generated when they
  # are visible on the web page.
  for (i in 1:max_plots) {
    # Need local so that each item gets its own number. Without it, the value
    # of i in the renderPlot() will be the same across all instances, because
    # of when the expression is evaluated.
    local({
      my_i <- i
      plotname <- paste("plot", my_i, sep="")
      output[[plotname]] <- renderPlot({
        
        my_symbol <- input$selected_stocks[my_i]
        
        prophet_df <- data[[my_symbol]][seq(input$start_date, as.Date(input$end_date), "days")]
        #prophet_df <- getSymbols(my_symbol, src = "yahoo", from = input$date, to = input$end_date, auto.assign = FALSE)
        prophet_df <- data.frame(prophet_df[,6])
        setDT(prophet_df, keep.rownames = TRUE)
        colnames(prophet_df) <- c("ds", "y")
        m_prophet <- prophet(prophet_df, yearly.seasonality = input$seasonal, weekly.seasonality = FALSE)
        future_prophet <- make_future_dataframe(m_prophet, periods = input$h)
        forecast_prophet <- predict(m_prophet, future_prophet)
        
        holt_df <- data[[my_symbol]][seq(input$start_date, input$end_date, "days")]
        holt_df <- data[[my_symbol]][seq(as.Date("2016-01-01"), as.Date("2018-12-31"), "days")]
        #prophet_df <- getSymbols(my_symbol, src = "yahoo", from = input$date, to = input$end_date, auto.assign = FALSE)
        holt_df <- data.frame(holt_df[,6])
        setDT(holt_df, keep.rownames = TRUE)
        colnames(holt_df) <- c("ds", "y")
        holt_ts <- ts(holt_df$y, frequency = as.integer(nrow(holt_df)/3))
        
        for_actual_values<-data[[my_symbol]][seq(input$end_date, input$end_date+input$h, "days")]
        
        hw <-HoltWinters(holt_ts)
        forecast<-predict(hw, n.ahead = dim(for_actual_values)[1],  prediction.interval=T,  level=.95)
        for_values<-data.frame(time=round(time(forecast),  3),value_forecast=as.data.frame(forecast)$fit,  dev=as.data.frame(forecast)$upr-as.data.frame(forecast)$fit)
        
        fitted_values<-data.frame(time=round(time(hw$fitted),  3),  value_fitted=as.data.frame(hw$fitted)$xhat)
        actual_values<-data.frame(time=round(time(hw$x),  3),  Actual=c(hw$x))
        
        for_actual_values<-data.frame(time=round(time(forecast), 3),value_actual=as.data.frame(for_actual_values)[,6])
        
        graphset<-merge(actual_values,  fitted_values,  by='time',  all=TRUE)
        graphset<-merge(graphset,  for_values,  all=TRUE,  by='time')
        graphset<-merge(graphset,  for_actual_values,  all=TRUE,  by='time')
        
        graphset[is.na(graphset$dev),  ]$dev<-0
        graphset$Fitted<-c(rep(NA,  NROW(graphset)-(NROW(for_values) + NROW(fitted_values))),  fitted_values$value_fitted,  for_values$value_forecast)
        actual2_size <- NROW(graphset)-NROW(for_actual_values)
        graphset$Actual2<-c(graphset$Actual[1:actual2_size],  for_actual_values$value_actual)
        
        graphset.melt<-melt(graphset[, c('time', 'Actual2', 'Fitted')], id='time')
      
        
        real_df <- data[[my_symbol]][seq(input$start_date, input$end_date+input$h, "days")]
        #real_df <- getSymbols(my_symbol, src = "yahoo", from = input$date, to = "2017-06-23", auto.assign = FALSE)
        real_df <- data.frame(real_df[,6])
        colnames(real_df) <- "Price"
        real_df$ds <- rownames(real_df)
        rownames(real_df) <- NULL
        real_df$ds <- as.Date(real_df$ds)
        
        
        forecast_prophet$ds <- as.Date(forecast_prophet$ds)
        joint_prophet_df = left_join(real_df, forecast_prophet, by="ds")
        rownames(joint_prophet_df) <- joint_prophet_df$ds
        joint_prophet_df$PriceX <- joint_prophet_df$Price
       
        joint_prophet_df$Price[joint_prophet_df$ds > input$end_date] = NA
        joint_prophet_df$PriceX[joint_prophet_df$ds < input$end_date+1] = NA
      
    
        priceref = joint_prophet_df$Price[joint_prophet_df$ds == input$end_date]
        yhatend = tail(joint_prophet_df$yhat,1)
        priceend = tail(joint_prophet_df$PriceX,1)
        
        theme_set(theme_gray(base_size = 18))
        
        if (((yhatend>priceref) && (priceend>priceref)) | ((yhatend<priceref) && (priceend<priceref))) {
            ggp1 <- ggplot(joint_prophet_df, aes(x=ds)) + geom_point(aes(y=Price), size=1) + geom_point(aes(y=PriceX), col="forestgreen", size=1) +
                    geom_line(aes(y=yhat)) + geom_ribbon(aes(ymin=yhat_lower, ymax=yhat_upper), fill="deepskyblue", alpha=0.5) +
                    geom_line(aes(y=priceref), cex=0.8, col="steelblue4") +
                    geom_vline(aes(xintercept=as.numeric(as.Date(input$end_date+1))), cex=0.8, col="steelblue4")+
                    ggtitle("Prophet's Time Series Forecasting")+ theme(plot.title = element_text(size = 15))
            ggp2 <- ggplot(graphset.melt,  aes(x=time,  y=value)) + geom_ribbon(data=graphset, aes(x=time, y=Fitted, ymin=Fitted-dev,  ymax=Fitted + dev),  alpha=.2,  fill='green') + 
                    geom_line(aes(colour=variable), size=1) + geom_vline(xintercept=max(actual_values$time),  lty=2) + 
                    xlab('Time') + ylab('Value') +ggtitle("Holt Winters Exponential Smoothing")+ theme(plot.title = element_text(size = 15))
            grid.arrange(ggp1,ggp2, ncol=2, top = textGrob(my_symbol,gp=gpar(fontsize=20,font=2)) )
          }
        else {
            ggp1 <- ggplot(joint_prophet_df, aes(x=ds)) + geom_point(aes(y=Price), size=1) + geom_point(aes(y=PriceX), col="firebrick1", size=1) +
                    geom_line(aes(y=yhat)) + geom_ribbon(aes(ymin=yhat_lower, ymax=yhat_upper), fill="deepskyblue", alpha=0.5) +
                    geom_line(aes(y=priceref), cex=0.8, col="steelblue4") +
                    geom_vline(aes(xintercept=as.numeric(as.Date(input$end_date+1))), cex=0.8, col="steelblue4")+
                    ggtitle("Prophet's Time Series Forecasting")+ theme(plot.title = element_text(size = 15))
            x <- data.frame(x = 1:5, y = 1:5)
            ggp2 <- ggplot(graphset.melt,  aes(x=time,  y=value)) + geom_ribbon(data=graphset, aes(x=time, y=Fitted, ymin=Fitted-dev,  ymax=Fitted + dev),  alpha=.2,  fill='green') + 
                    geom_line(aes(colour=variable), size=1) + geom_vline(xintercept=max(actual_values$time),  lty=2) + 
                    xlab('Time') + ylab('Value') + scale_colour_hue('') +ggtitle("Holt Winters Exponential Smoothing")+ theme(plot.title = element_text(size = 15))
            grid.arrange(ggp1,ggp2, ncol=2, top = textGrob(my_symbol,gp=gpar(fontsize=20,font=2)) )
          }
      })
    })
  }
  
}

shinyApp(ui = ui, server = server)

#my_symbol <- input$selected_stocks[my_i]
#my_col <- paste(my_symbol,"Adjusted",sep = ".")

#start_range <- bizdays::bizdays(index(data[[my_symbol]][1]), input$start_date, 'QuantLib/UnitedStates/NYSE')+1
#end_range <- start_range + bizdays::bizdays(input$start_date, input$end_date, 'QuantLib/UnitedStates/NYSE')-1

#fitted_stock <- HoltWinters(data[[my_symbol]][,my_col][c(start_range:end_range)], gamma=FALSE)
#stock_forecast <- forecast:::forecast.HoltWinters(fitted_stock, h=selected_h())
#forecast:::plot.forecast(stock_forecast, main = my_symbol)
