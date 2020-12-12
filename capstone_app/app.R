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

my_colors <- c("cornsilk3",'coral1','chocolate1','chartreuse4','chartreuse','cadetblue4','cadetblue','brown4','blueviolet','dodgerblue4','dodgerblue1','dimgray','deepskyblue1','deeppink4','deeppink','darkviolet','darkturquoise','darkslateblue','darkseagreen3','darkorchid','orange1','tomato1','olivedrab4','red4','red1','tan')

algorithm_names <- c("Holt's Exponential Smoothing","Prophet Time Series Model")
risk_levels <- c('Manual Risk Portfolio', 'Low Risk Portfolio', 'Medium Risk Portfolio', 'High Risk Portfolio')
load_quantlib_calendars('UnitedStates/NYSE',from='2000-01-01', to='2020-12-10')

symbols <- scan("data/top50_market_cap_usa_mex.txt", what = 'character')

syms_lo_risk <- c()
syms_me_risk <- c()
syms_hi_risk <- c()

data <- hash()

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
        #selectInput('forecast_alg', 'Algoritmo de Predicción', algorithm_names),
        numericInput('h_ana', 'Horizonte de Predicción', min = 1, value = 60),
        wellPanel(id = "checkbox_panel1",style = "overflow-y:scroll; max-height: 300px",
                  checkboxGroupInput('selected_stocks_ana', "Selección de stocks para visualización",
                           symbols, selected = symbols[1:1]))
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
    ), # <-- end tabPanel 'analysis-tab'
    
    tabPanel(
      id = 'analysis-tab',
      title = 'Portafolio',
      headerPanel("Mi Portafolio de Inversión"),
      sidebarPanel(
        selectInput('risk_level', 'Nivel de riesgo', risk_levels),
        wellPanel(id = "checkbox_panel2",style = "overflow-y:scroll; max-height: 300px",
        checkboxGroupInput('selected_stocks_inv', "Selección de stocks en mi portafolio",
                           symbols)),
        wellPanel(id = "checkbox_panel3",style = "overflow-y:scroll; max-height: 500px",
                  uiOutput("portfolio_dist")),
        numericInput('init_capital', 'Capital de inversión inicial ($MXN)', min = 0, value = 100),
        numericInput('h_inv', 'Horizonte de Predicción', min = 1, value = 60)
        ),
      mainPanel(
        id = 'inner-main-portfolio',
        plotOutput("pie_chart"),
        plotOutput("portfolio_forecast")
      ) # <- end mainPanel 'inner-main-portfolio
      
    ) # <-- end tabPanel 'protfolio-tab'
  ) # <-- end tabsetPanel 'main-tabs'
) # <-- end fluid Page


# ---------------------# ---------------------# ---------------------# ---------------------# ---------------------
# ---------------------# ---------------------# _______SERVER________# ---------------------# --------------------- 
# ---------------------# ---------------------# ---------------------# ---------------------# ---------------------


server <- function(input, output, session) {

  
  # _______________________________________________________________________________________________________________________
  # ___________________________________________________ANALISIS DE DATOS___________________________________________________
  # _______________________________________________________________________________________________________________________
  
  # ____________________ DATOS ____________________
  
  output$table1 <- renderTable({
    output_table1 <- NULL
    for (symbol in input$selected_stocks_ana) {
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
    plot_output_list <- lapply(1:length(input$selected_stocks_ana), function(i) {
      plotname <- paste("plot", i, sep="")
      plotOutput(plotname, height = 400, width = 800)
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
        
        my_symbol <- input$selected_stocks_ana[my_i]
        
        prophet_df <- data[[my_symbol]][seq(input$start_date, input$end_date, "days")]
        #prophet_df <- getSymbols(my_symbol, src = "yahoo", from = input$date, to = input$end_date, auto.assign = FALSE)
        prophet_df <- data.frame(prophet_df[,6])
        setDT(prophet_df, keep.rownames = TRUE)
        colnames(prophet_df) <- c("ds", "y")
        m_prophet <- prophet(prophet_df, yearly.seasonality = input$seasonal, weekly.seasonality = FALSE)
        future_prophet <- make_future_dataframe(m_prophet, periods = input$h_ana)
        forecast_prophet <- predict(m_prophet, future_prophet)
        
        holt_df <- data[[my_symbol]][seq(input$start_date, input$end_date, "days")]
        #prophet_df <- getSymbols(my_symbol, src = "yahoo", from = input$date, to = input$end_date, auto.assign = FALSE)
        holt_df <- data.frame(holt_df[,6])
        setDT(holt_df, keep.rownames = TRUE)
        colnames(holt_df) <- c("ds", "y")
        holt_ts <- ts(holt_df$y, frequency = as.integer(nrow(holt_df)/3))
        
        for_actual_values<-data[[my_symbol]][seq(input$end_date, input$end_date+input$h_ana, "days")]
        
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
      
        
        real_df <- data[[my_symbol]][seq(input$start_date, input$end_date+input$h_ana, "days")]
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
                    xlab('Time') + ylab('Value') +ggtitle("Holt Winters Exponential Smoothing")+ theme(plot.title = element_text(size = 15), legend.position = "none")
            grid.arrange(ggp1,ggp2, ncol=2, top = textGrob(my_symbol,gp=gpar(fontsize=20,font=2)) )
          }
        else {
            ggp1 <- ggplot(joint_prophet_df, aes(x=ds)) + geom_point(aes(y=Price), size=1) + geom_point(aes(y=PriceX), col="firebrick1", size=1) +
                    geom_line(aes(y=yhat)) + geom_ribbon(aes(ymin=yhat_lower, ymax=yhat_upper), fill="deepskyblue", alpha=0.5) +
                    geom_line(aes(y=priceref), cex=0.8, col="steelblue4") +
                    geom_vline(aes(xintercept=as.numeric(as.Date(input$end_date+1))), cex=0.8, col="steelblue4")+
                    ggtitle("Prophet's Time Series Forecasting")+ theme(plot.title = element_text(size = 15))
            ggp2 <- ggplot(graphset.melt,  aes(x=time,  y=value)) + geom_ribbon(data=graphset, aes(x=time, y=Fitted, ymin=Fitted-dev,  ymax=Fitted + dev),  alpha=.2,  fill='green') + 
                    geom_line(aes(colour=variable), size=1) + geom_vline(xintercept=max(actual_values$time),  lty=2) + 
                    xlab('Time') + ylab('Value') + scale_colour_hue('') +ggtitle("Holt Winters Exponential Smoothing")+ theme(plot.title = element_text(size = 15), legend.position = "none")
            grid.arrange(ggp1,ggp2, ncol=2, top = textGrob(my_symbol,gp=gpar(fontsize=20,font=2)) )
          }
      })
    }) #<- end local
    
  } #<- end for loop max_plots
  
  # _______________________________________________________________________________________________________________________
  # ___________________________________________________MI PORTAFOLIO_______________________________________________________
  # _______________________________________________________________________________________________________________________
  
  observe({
    if(input$risk_level == 'Manual Risk Portfolio') {
      updateCheckboxGroupInput(session,"selected_stocks_inv",choices=symbols)
    }
    else if (input$risk_level == 'Low Risk Portfolio') {
      updateCheckboxGroupInput(session,"selected_stocks_inv",choices=symbols, selected = syms_lo_risk)
    }
    else if (input$risk_level == 'Medium Risk Portfolio') {
      updateCheckboxGroupInput(session,"selected_stocks_inv","Choose campaign(s):",choices=symbols, selected = syms_me_risk)
    }
    else if (input$risk_level == 'High Risk Portfolio') {
      updateCheckboxGroupInput(session,"selected_stocks_inv","Choose campaign(s):",choices=symbols, selected = syms_hi_risk)
    }
    
  })
  
  output$portfolio_dist <- renderUI({
    numStocks <- length(input$selected_stocks_inv)
    if (numStocks > 0) {
      lapply(1:numStocks, function(i) {
        sliderInput(paste("slider",i,sep=""),input$selected_stocks_inv[i],min =0, max=100, val =0)
      })
    }
  })
  
  observe({
    sum_dist <- 0
    for (i in 1:length(input$selected_stocks_inv)){
      slidername <- paste("slider",i,sep="")
      current_val <- input[[slidername]]
      updateSliderInput(session, slidername, value = current_val, min = 0, max = 100 - sum_dist, step = 1)
      sum_dist <- sum_dist+current_val
    }
  })
  
  output$pie_chart <-renderPlot({
    distribution <- c()
    for (i in 1:length(input$selected_stocks_inv)) {
      slidername <- paste("slider",i,sep="")
      val <- input[[slidername]]
      distribution <- append(distribution,val)
    }
    if (sum(distribution) < 100) {
      df <- data.frame(
      Stocks = append(input$selected_stocks_inv,"REMAINING"),
      portfolio_distribution = append(distribution,100-sum(distribution))
      )
    } 
    else {
      df <- data.frame(
      Stocks = input$selected_stocks_inv,
      portfolio_distribution = distribution
      )
    }
    
    bp <- ggplot(df, aes(x="", y=portfolio_distribution, fill=Stocks))+ geom_bar(width = 1, stat = "identity")
    piechart <- bp + coord_polar("y", start=0)
    return(piechart)
  })
  
  output$portfolio_forecast <- renderPlot({
    numStocks <- length(input$selected_stocks_inv)
    main_prophet_df <- FALSE
    main_holt_df <- FALSE
    if (numStocks > 0) {
      positive_stocks <- c()
      for (i in 1:length(input$selected_stocks_inv)) {
        if (input[[paste("slider",i,sep = "")]] > 0) {
          positive_stocks <- append(positive_stocks,input$selected_stocks_inv[i])
        }
      }
      counter <- as.numeric(1)
      if (length(positive_stocks > 0)) {
        for (symbol in positive_stocks) {
            
            prophet_df <- data[[symbol]][seq(input$start_date, input$end_date, "days")]
            
            prophet_df <- data.frame(prophet_df[,6])
            setDT(prophet_df, keep.rownames = TRUE)
            colnames(prophet_df) <- c("ds", "y")
            m_prophet <- prophet(prophet_df, yearly.seasonality = input$seasonal, weekly.seasonality = FALSE)
            future_prophet <- make_future_dataframe(m_prophet, periods = input$h_inv)
          
            forecast_prophet <- predict(m_prophet, future_prophet)
            
            holt_df <- data[[symbol]][seq(input$start_date, input$end_date, "days")]
            #prophet_df <- getSymbols(my_symbol, src = "yahoo", from = input$date, to = input$end_date, auto.assign = FALSE)
            holt_df <- data.frame(holt_df[,6])
            setDT(holt_df, keep.rownames = TRUE)
            colnames(holt_df) <- c("ds", "y")
            holt_ts <- ts(holt_df$y, frequency = as.integer(nrow(holt_df)/3))
            
            for_actual_values<-data[[symbol]][seq(input$end_date, input$end_date+input$h_inv, "days")]
            
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
            graphset.melt
            
            real_df <- data[[symbol]][seq(input$start_date, input$end_date+input$h_inv, "days")]
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
            
            
            multiplier <- input$init_capital*input[[paste("slider",counter,sep="")]]/(100*joint_prophet_df$PriceX[!is.na(joint_prophet_df$PriceX)][1])

 
            if (typeof(main_prophet_df) == 'logical') {
              main_prophet_df <- data.frame(ds = joint_prophet_df$ds)
  
            }
        
            if (typeof(main_holt_df) == 'logical'){
              main_holt_df <- data.frame(time = graphset$time)
            }
            
            main_prophet_df[[paste("Price",symbol,sep="")]] <- joint_prophet_df$Price*multiplier
            main_prophet_df[[paste("PriceX",symbol,sep="")]] <- joint_prophet_df$PriceX*multiplier
            main_prophet_df[[paste("yhat",symbol,sep="")]] <- joint_prophet_df$yhat*multiplier
            main_prophet_df[[paste("yhat_lower",symbol,sep="")]] <- joint_prophet_df$yhat_lower*multiplier
            main_prophet_df[[paste("yhat_upper",symbol,sep="")]] <- joint_prophet_df$yhat_upper*multiplier
            
            main_holt_df[[paste("Actual",symbol,sep= "")]] <- graphset$Actual2*multiplier
            main_holt_df[[paste("Fitted",symbol, sep = "")]] <- graphset$Fitted*multiplier
            main_holt_df[[paste("dev", symbol, sep = "")]] <- graphset$dev*multiplier
            counter <- counter+1
        } # <- end for loop statement
        
        ggp1 <- ggplot(main_prophet_df, aes(x=ds)) + geom_vline(aes(xintercept=as.Date(input$end_date+1)), cex=0.8, col="steelblue4")+
                          ggtitle("Prophet's Time Series Forecasting")+ theme(plot.title = element_text(size = 15),legend.position="bottom")
        ggp2 <- ggplot(main_holt_df,  aes(x=time)) + geom_vline(aes(xintercept=max(actual_values$time)),  lty=2) + 
                          xlab('Time') + ylab('Value') +ggtitle("Holt Winters Exponential Smoothing")+ theme(plot.title = element_text(size = 15),legend.position="bottom")
        
        color_counter <- 1
        
        main_prophet_df$combined_yhat <- c(rep(NA,NROW(main_prophet_df)-length(joint_prophet_df$PriceX[!is.na(joint_prophet_df$PriceX)])),rep(0,length(joint_prophet_df$PriceX[!is.na(joint_prophet_df$PriceX)])))
        main_prophet_df$combined_yhat_lower <- c(rep(NA,NROW(main_prophet_df)-length(joint_prophet_df$PriceX[!is.na(joint_prophet_df$PriceX)])),rep(0,length(joint_prophet_df$PriceX[!is.na(joint_prophet_df$PriceX)])))
        main_prophet_df$combined_yhat_upper <- c(rep(NA,NROW(main_prophet_df)-length(joint_prophet_df$PriceX[!is.na(joint_prophet_df$PriceX)])),rep(0,length(joint_prophet_df$PriceX[!is.na(joint_prophet_df$PriceX)])))
        
        main_holt_df$combined_fit <- c(rep(NA,NROW(main_holt_df)-length(joint_prophet_df$PriceX[!is.na(joint_prophet_df$PriceX)])),rep(0,length(joint_prophet_df$PriceX[!is.na(joint_prophet_df$PriceX)])))
        main_holt_df$combined_dev <- c(rep(NA,NROW(main_holt_df)-length(joint_prophet_df$PriceX[!is.na(joint_prophet_df$PriceX)])),rep(0,length(joint_prophet_df$PriceX[!is.na(joint_prophet_df$PriceX)])))
        
        for (symbol in positive_stocks) {
          
          ggp1 <- ggp1 + geom_point(aes(y=.data[[paste("Price",symbol,sep="")]]), size=0.5,alpha=0.5, col=my_colors[color_counter], shape=4) + geom_point(aes(y=.data[[paste("PriceX",symbol,sep="")]]), col=my_colors[color_counter], size=0.5, shape =4) +
                          geom_line(aes(y=.data[[paste("yhat",symbol,sep="")]]), size=0.5, alpha=0.5, col=my_colors[color_counter]) + geom_ribbon(aes(ymin=.data[[paste("yhat_lower",symbol,sep="")]], ymax=.data[[paste("yhat_upper",symbol,sep="")]]), fill="deepskyblue", alpha=0.25)
          
          ggp2 <- ggp2 + geom_point(aes(y=.data[[paste("Actual",symbol,sep="")]]), size=0.5,alpha=0.5, col=my_colors[color_counter], shape =4) +
                          geom_line(aes(y=.data[[paste("Fitted", symbol,sep="")]]),size=0.5, alpha=0.5, col=my_colors[color_counter])+ geom_ribbon(aes(ymin=.data[[paste("Fitted",symbol,sep="")]]-.data[[paste("dev",symbol,sep="")]],  ymax=.data[[paste("Fitted",symbol,sep="")]] + .data[[paste("dev",symbol,sep="")]]),  alpha=0.25,  fill='green')
                          
          color_counter <- color_counter+1
          
          main_holt_df$combined_fit <- main_holt_df$combined_fit+main_holt_df[[paste("Fitted", symbol,sep="")]]
          main_holt_df$combined_dev <- main_holt_df$combined_dev+main_holt_df[[paste("dev", symbol,sep="")]]
          
          main_prophet_df$combined_yhat <- main_prophet_df$combined_yhat + main_prophet_df[[paste("yhat",symbol,sep="")]]
          main_prophet_df$combined_yhat_lower <- main_prophet_df$combined_yhat_lower + main_prophet_df[[paste("yhat_lower",symbol,sep="")]]
          main_prophet_df$combined_yhat_upper <- main_prophet_df$combined_yhat_upper + main_prophet_df[[paste("yhat_upper",symbol,sep="")]]
        }
        
        
        #main_holt_df$combined <- main_holt_df$
        
        ggp1 <- ggp1 + 
          geom_line(data = main_prophet_df, aes(y=combined_yhat), size=0.75, alpha=1, col='red3') + geom_ribbon(data = main_prophet_df,aes(ymin=combined_yhat_lower, ymax=combined_yhat_upper), fill="violetred2", alpha=0.4)+
          geom_line(data = main_prophet_df, aes(y=combined_yhat/length(positive_stocks)), size=0.75, alpha=1, col='dodgerblue4') + geom_ribbon(data = main_prophet_df,aes(ymin=combined_yhat_lower/length(positive_stocks), ymax=combined_yhat_upper/length(positive_stocks)), fill="dodgerblue1", alpha=0.4)+
          scale_x_date(date_breaks = "1 month", 
           limits = c(input$end_date-min(as.numeric(input$end_date-input$start_date),input$h_inv), input$end_date+input$h_inv),
           date_labels="%b-%Y" )
        ggp2 <- ggp2 + 
          geom_line(data = main_holt_df, aes(y=combined_fit),size=0.75, alpha=1, col='red3')+ geom_ribbon(data = main_holt_df, aes(ymin=combined_fit-combined_dev,  ymax=combined_fit + combined_dev),  alpha=0.4,  fill='violetred2')+
          geom_line(data = main_holt_df, aes(y=combined_fit/length(positive_stocks)),size=0.75, alpha=1, col='dodgerblue4')+ geom_ribbon(data = main_holt_df, aes(ymin=combined_fit/length(positive_stocks)-combined_dev/length(positive_stocks),  ymax=combined_fit/length(positive_stocks) + combined_dev/length(positive_stocks)),  alpha=0.4,  fill='dodgerblue1')+
          xlim(max(actual_values$time)-input$h_inv*(main_holt_df$time[2]-main_holt_df$time[1]),max(actual_values$time)+input$h_inv*(main_holt_df$time[2]-main_holt_df$time[1]))
  
        
        return(grid.arrange(ggp1,ggp2, ncol=2, top = textGrob("Portfolio Forecasted",gp=gpar(fontsize=20,font=2)) ))

      } # end 2nd if statement
    }# <- end 1st if statment
  })
  
}

shinyApp(ui = ui, server = server)

#my_symbol <- input$selected_stocks_ana[my_i]
#my_col <- paste(my_symbol,"Adjusted",sep = ".")

#start_range <- bizdays::bizdays(index(data[[my_symbol]][1]), input$start_date, 'QuantLib/UnitedStates/NYSE')+1
#end_range <- start_range + bizdays::bizdays(input$start_date, input$end_date, 'QuantLib/UnitedStates/NYSE')-1

#fitted_stock <- HoltWinters(data[[my_symbol]][,my_col][c(start_range:end_range)], gamma=FALSE)
#stock_forecast <- forecast:::forecast.HoltWinters(fitted_stock, h=selected_h())
#forecast:::plot.forecast(stock_forecast, main = my_symbol)
