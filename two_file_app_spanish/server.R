server <- function(input, output, session) {
  
  
  # _______________________________________________________________________________________________________________________
  # ___________________________________________________ANALISIS DE DATOS___________________________________________________
  # _______________________________________________________________________________________________________________________
  
  # ____________________ DATOS ____________________
  
  output$table1 <- renderTable({
    output_table1 <- NULL
    for (symbol in symbols) {
      last2 <- tail(data[[symbol]],2)
      names(last2) <- c("open","high","low","close","volume","adjusted")
      diff_abs <- as.numeric(last2[2,"adjusted"])-as.numeric(last2[1,"adjusted"])
      Cambio_abs <- xts(diff_abs+0:0, order.by = as.Date(index(last2[2,]))+0:0)
      diff_pct <- diff_abs/as.numeric(last2[1,"adjusted"])
      Cambio_pct <- xts(diff_pct+0:0, order.by = as.Date(index(last2[2,]))+0:0)
      new_row <- cbind(last2[2,"adjusted"],Cambio_abs,Cambio_pct)
      new_row_df <- data.frame(date = as.Date(index(new_row)), coredata(new_row))
      new_row_df$date <- as.character(new_row_df$date)
      new_row_df['symbol'] = symbol
      names(new_row_df) <- c("Última Fecha","Último Precio", "Cambio", "Cambio (%)", "Compañía")
      new_row_df <- new_row_df[c("Compañía","Último Precio", "Cambio", "Cambio (%)", "Última Fecha")]
      output_table1 <- rbind(output_table1,new_row_df)
    }
    return(output_table1)
  })
  
  # _______________________________________________________________________________________________________________________
  # ___________________________________________________VISUALIZACION_______________________________________________________
  # _______________________________________________________________________________________________________________________
  
  #the following variables will be updated every time the runAnalysisButton is clicked
  selected_stocks_ana <- eventReactive(input$runAnalysisButton,{
    input$selected_stocks_ana
  })
  start_date_ana <- eventReactive(input$runAnalysisButton, {
    return(input$start_date_ana)
  })
  end_date_ana <- eventReactive(input$runAnalysisButton, {
    return(input$end_date_ana)
  })
  h_ana <- eventReactive(input$runAnalysisButton, {
    return(input$h_ana)
  })
  input_seasonal <- eventReactive(input$runAnalysisButton, {
    return(input$seasonal)
  })
  input_seasonal_type <- eventReactive(input$runAnalysisButton, {
    return(input$seasonal_type)
  })
  
  observe({
    # use tabsetPanel 'id' argument to Cambio tabs
    if (input$runAnalysisButton > 0) {
      updateTabsetPanel(session, "analysis-inner-tabset", selected = "visualization-panel")
    } 
  })
  
  output$plots <- renderUI({
    plot_output_list <- lapply(1:length(selected_stocks_ana()), function(i) {
      plotname <- paste("plot", i, sep="")
      plotlyOutput(plotname) %>% withSpinner(color="#0dc5c1")
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
      output[[plotname]] <- renderPlotly({
        
        my_symbol <- selected_stocks_ana()[my_i]
        
        biz_day_seq <- seq(start_date_ana(), end_date_ana()+h_ana(), "days")[isBizday(timeSequence(start_date_ana(),end_date_ana()+h_ana()), holidayNYSE())]
        #day_seq <- seq(start_date_ana(), end_date_ana()+h_ana(), "days")
        #NOTE: IF THERE'S AN ERROR REGARDING THE DATES OF THE DATA.FRAMES TRY UNCOMMENTING ABOVE LINE AND COMMENTING THE ONE ABOVE THAT ONE AND CHANGING biz_day_seq TO day_seq BELOW
        all_data <- data[[my_symbol]][biz_day_seq,6] #selecting only the rows in the given time frame of the training data + prediction horizon
        all_data_df <- data.frame(all_data) #converting all_data into a data.frame
        colnames(all_data_df) <- "y" #naming the adjusted price column "y"
        all_data_df$ds <- rownames(all_data_df) #creating a new column called "ds" that contains the date
        all_data_df$ds <- as.Date(all_data_df$ds) #converting data from chr into Date type
        rownames(all_data_df) <- NULL #removing the date from the index of the data.frame
        all_data_df <- all_data_df[c("ds","y")]

        training_df <- data.frame(all_data[seq(start_date_ana(), end_date_ana(), "days")]) #selecting only the data for the training period
        setDT(training_df, keep.rownames = TRUE) #converts data frame to data.table by reference (essentially converts index to a column called "rn")
        colnames(training_df) <- c("ds", "y") #ds is date, y is price
        
        y_actual <- all_data_df$y #storing actual y values into array
        
        #PROPHET
        prophet_model <- prophet(training_df, yearly.seasonality = input_seasonal(), weekly.seasonality = FALSE)
        future_prophet <- make_future_dataframe(prophet_model, periods = h_ana()) #data frame with future dates for forecasting
        #the following line use the model to forecast and fill in the future dataframe. Future prophet has several columns that contain information about
        #the prediction and the model. The relevant columns for this app are: ds, yhat_lower, yhat_upper, yhat
        forecast_prophet <- predict(prophet_model, future_prophet)
        forecast_prophet$ds <- as.Date(forecast_prophet$ds) #changing format of ds from POSIXct to Date
        prophet_df <- full_join(all_data_df, forecast_prophet, by="ds") #joining forecast and actual data into one data.frame
        rownames(prophet_df) <- prophet_df$ds
        prophet_df <- prophet_df[isBizday( as.timeDate(prophet_df$ds), holidayNYSE()),] #subsetting only business days
        length(y_actual) <- dim(prophet_df)[1] #making array the same size as the training period + prediction, filling with NA to reach the length if necessary
        prophet_df$y <- y_actual #y column is NA after the full_join because it does not appear on forecast_prophet. This lines fills in the column
        prophet_df <- prophet_df[c("ds","yhat_lower","yhat_upper","yhat","y")] #dropping the irrelevant columns
        rownames(prophet_df) <- prophet_df$ds #setting rownames equal to the dates
        prophet_df$y_train <- prophet_df$y 
        prophet_df$y_train[prophet_df$ds > end_date_ana()] = NA #y_train only
        prophet_df$y_test <- prophet_df$y
        prophet_df$y_test[prophet_df$ds < end_date_ana()+1] = NA #y_test only
        
        prophet_df$yhat_train <- prophet_df$yhat
        prophet_df$yhat_train[prophet_df$ds > end_date_ana()] = NA
        
        prophet_df$yhat_test <- prophet_df$yhat
        prophet_df$yhat_test[prophet_df$ds < end_date_ana()+1] = NA
        
        prophet_df$yhat_upper_test <- prophet_df$yhat_upper
        prophet_df$yhat_upper_test[prophet_df$ds < end_date_ana()+1] = NA
        prophet_df$yhat_lower_test <- prophet_df$yhat_lower
        prophet_df$yhat_lower_test[prophet_df$ds < end_date_ana()+1] = NA
        
        
        #HOLT WINTERS
        data_length <- as.integer(end_date_ana() - start_date_ana())/365.25 #data length in number of years of the training data
        holt_ts <- ts(training_df$y, frequency = as.integer(nrow(training_df)/data_length)) #time series of the training data to use HoltWinters method
        holt_model <-HoltWinters(holt_ts, gamma = input_seasonal(), seasonal = input_seasonal_type())
        forecast_holt <- predict(holt_model, n.ahead = h_ana(),  prediction.interval=T,  level=.95)
        forecast_holt <- data.frame(ds = seq(end_date_ana()+1, end_date_ana()+h_ana(),"days"), 
                                    yhat = as.data.frame(forecast_holt)$fit, 
                                    yhat_lower = as.data.frame(forecast_holt)$lwr,
                                    yhat_upper = as.data.frame(forecast_holt)$upr)
        holt_df <- full_join(all_data_df, forecast_holt, by ="ds")
        rownames(holt_df) <- holt_df$ds
        holt_df <- holt_df[isBizday( as.timeDate(holt_df$ds), holidayNYSE()),] #subsetting only business days
        length(y_actual) <- dim(holt_df)[1]
        holt_df$y <- y_actual #y column is NA after the full_join because it does not appear on forecast_prophet. This lines fills in the column
        rownames(holt_df) <- holt_df$ds
        holt_df$y_train <- holt_df$y
        holt_df$y_train[holt_df$ds > end_date_ana()] = NA #y_train only
        holt_df$y_test <- holt_df$y
        holt_df$y_test[holt_df$ds < end_date_ana()+1] = NA #y_test only 
        holt_df$yhat_test <- holt_df$yhat
        yhat_train <- as.data.frame(holt_model$fitted)$xhat
        holt_df$yhat_train <- c(rep(NA,dim(holt_df)[1]-length(yhat_train)-length(holt_df$yhat_test[!is.na(holt_df$yhat_test)])), yhat_train, rep(NA,length(holt_df$yhat_test[!is.na(holt_df$yhat_test)])))
        
   
        #PLOTS
    
        theme_set(theme_fivethirtyeight(base_size=18, base_family = "Arial"))
        
        prediction_start = prophet_df[prophet_df$ds == end_date_ana(),"y"]
        yhat_end = tail(prophet_df$yhat,1)
        y_train_end = tail(na.omit(prophet_df$y_train),1)
        
        min_y <- min(min(holt_df$y_train[!is.na(holt_df$y_train)]),min(prophet_df$y_train[!is.na(holt_df$y_train)]))
        max_y <- max(max(holt_df$yhat_upper[!is.na(holt_df$yhat_upper)]),max(prophet_df$yhat_upper[!is.na(prophet_df$yhat_upper)]))
        
      
        prophet_df_readable <- data.frame(prophet_df)
        colnames(prophet_df_readable) <- c("Fecha","Límite Inferior y", "Límite Superior y", "yhat", "y", "Precio Real ($)", "Precio Futuro Real ($)", "Predicted ($)", "Fitted ($)","Límite Inferior ($)", "Límite Superior ($)")
        
        prophet_ana_plot <- ggplot(prophet_df_readable, aes(x=`Fecha`)) +
          geom_line(aes(y=`Precio Real ($)`), col="#c269f5", size=0.5, alpha=0.7) +
          geom_line(aes(y=`Precio Futuro Real ($)`), col="#8e69f5", size=0.5, alpha=0.7) +
          geom_line(aes(y=`Fitted ($)`), col="#edb83e",size = 0.5) +
          geom_line(aes(y=`Predicted ($)`), col="#ed8d3e",size = 0.5) +
          geom_ribbon(aes(ymin=`Límite Inferior ($)`, ymax=`Límite Superior ($)`), fill="#ed8d3e", alpha=0.5) +
          geom_hline(aes(yintercept=prediction_start), cex=0.3, col="steelblue4", linetype = "dashed") +
          geom_vline(aes(xintercept=as.numeric(as.Date(end_date_ana()+1))), cex=0.3, col="steelblue4", linetype = "dashed")+
          ylim(min_y,max_y)
        
        ply_prophet_ana <- ggplotly(prophet_ana_plot) %>% layout(
          xaxis = list(
            title = "Fecha",
            tickfont = list(size=12)
            ), 
          yaxis = list(
            title = "Precio por acción (USD $)",
            tickfont = list(size=12)
            ),
          font = list(
            size = 12
            )
          )
        
        holt_df_readable <- data.frame(holt_df)
        colnames(holt_df_readable) <- c("Fecha","y", "yhat","Límite Inferior ($)", "Límite Superior ($)", "Precio Real ($)", "Precio Futuro Real ($)", "Predicted ($)", "Fitted ($)")
        
        holt_ana_plot <- ggplot(holt_df_readable,  aes(x=`Fecha`)) +
          geom_line(aes(y=`Precio Real ($)`), col="#c269f5", size = 0.5) +
          geom_line(aes(y=`Precio Futuro Real ($)`), col="#8e69f5", size=0.5, alpha=0.7) +
          geom_line(aes(y=`Fitted ($)`), col="#69aaf5",size = 0.5) +
          geom_line(aes(y=`Predicted ($)`), col="#1c5bba") +
          geom_ribbon(aes(ymin=`Límite Inferior ($)`,  ymax=`Límite Superior ($)`), fill='#1c5bba', alpha=0.5) +
          geom_hline(aes(yintercept=prediction_start), cex=0.3, col="steelblue4", linetype = "dashed") +
          geom_vline(aes(xintercept=as.numeric(as.Date(end_date_ana()+1))), cex=0.3, col='steelblue4', linetype = "dashed")+
          ylim(min_y,max_y) +
          theme(legend.position="bottom")

        ply_holt_ana <- ggplotly(holt_ana_plot) %>% layout(
          xaxis = list(
            title = "Fecha",
            tickfont = list(size=12)
            ), 
          yaxis = list(
            title = "Precio por acción (USD $)",
            tickfont = list(size=12)
          ),
          font = list(
            size = 12
          )
        )
  
        subplot(ply_prophet_ana, ply_holt_ana, nrows=1, titleX = TRUE, titleY=TRUE, shareY=F) %>% layout(
          title = list(
            text = my_symbol,
            font = list(size=20),
            x = 0.5,
            xanchor = 'center',
            yanchor = 'top'
          ),
          annotations = list(
            list(
              x = 0.225, 
              y = 1.0, 
              font = list(size = 16), 
              text = "Pronóstico Prophet", 
              xref = "paper", 
              yref = "paper", 
              xanchor = "center", 
              yanchor = "bottom", 
              showarrow = FALSE
            ), 
            list(
              x = 0.775, 
              y = 1.0, 
              font = list(size = 16), 
              text = "Pronóstico Holt Winters", 
              xref = "paper", 
              yref = "paper", 
              xanchor = "center", 
              yanchor = "bottom", 
              showarrow = FALSE
            )
           )
        ) #<- end layout
      })
    }) #<- end local
    
  } #<- end for loop max_plots
  
  # _______________________________________________________________________________________________________________________
  # ___________________________________________________MI Portfolio_______________________________________________________
  # _______________________________________________________________________________________________________________________
  
  observe({
    if(input$risk_level == 'Manual') {
      updateCheckboxGroupInput(session,"selected_stocks_inv",choices=symbols)
    }
    else if (input$risk_level == 'Riesgo Bajo') {
      updateCheckboxGroupInput(session,"selected_stocks_inv",choices=symbols, selected = syms_lo_risk)
    }
    else if (input$risk_level == 'Riesgo Medio') {
      updateCheckboxGroupInput(session,"selected_stocks_inv","Choose campaign(s):",choices=symbols, selected = syms_me_risk)
    }
    else if (input$risk_level == 'Riesgo Alto') {
      updateCheckboxGroupInput(session,"selected_stocks_inv","Choose campaign(s):",choices=symbols, selected = syms_hi_risk)
    }
  })
  
  
  #the following variables will be updated every time the selectStocksButton is clicked
  selected_stocks_inv <- eventReactive(input$selectStocksButton,{
    input$selected_stocks_inv
  })
  
  #the following variables will be updated every time the selectStocksButton is clicked
  input_sliders <- eventReactive(input$runPortfolio, {
    sliders <- hash()
    sum_sliders <- 0
    for (i in 1:length(selected_stocks_inv())) {
      slidername <- paste("slider",i,sep="")
      val <- input[[slidername]]
      sliders[[slidername]] <- val
      sum_sliders <- sum_sliders+val
    }
    for (i in 1:length(selected_stocks_inv())) {
      slidername <- paste("slider",i,sep="")
      sliders[[slidername]] <- sliders[[slidername]]/sum_sliders
    }
    return(sliders)
  })
  input_init_capital <- eventReactive(input$runPortfolio, {
    return(input$init_capital)
  })
  start_date_port <- eventReactive(input$runPortfolio, {
    return(input$start_date_port)
  })
  end_date_port <- eventReactive(input$runPortfolio, {
    return(input$end_date_port)
  })
  h_Portfolio <- eventReactive(input$runPortfolio, {
    return(input$h_Portfolio)
  })
  input_seasonal2 <- eventReactive(input$runPortfolio, {
    return(input$seasonal2)
  })
  input_seasonal_type2 <- eventReactive(input$runPortfolio, {
    return(input$seasonal_type2)
  })
  
  #Initializing Sliders based on selected_stocks_inv()
  output$portfolio_dist <- renderUI({
    numStocks <- length(selected_stocks_inv())
    if (numStocks > 0) {
      lapply(1:numStocks, function(i) {
        sliderInput(paste("slider",i,sep=""),selected_stocks_inv()[i],min =0, max=100, val =100/numStocks)
      })
    }
  })
  
  #Pie Chart of Portfolio distribution
  output$pie_chart <-renderPlotly({
    distribution <- c()
    for (i in 1:length(selected_stocks_inv())) {
      slidername <- paste("slider",i,sep="")
      val <- input_sliders()[[slidername]]
      distribution <- append(distribution,val)
    }
    df <- data.frame(
      Stocks = selected_stocks_inv(),
      portfolio_distribution = distribution
    )
    
    df <- df %>% 
      arrange(desc(Stocks)) %>%
      mutate(prop = portfolio_distribution / sum(df$portfolio_distribution) *100) %>%
      mutate(ypos = cumsum(prop)- 0.5*prop )
    
    df <- data.frame(lapply(df, function(y) if(is.numeric(y)) round(y, 2) else y))
    
    piechartly <- plot_ly(df, labels = ~Stocks, values = ~prop, type = 'pie',
                          textposition = 'inside',
                          textinfo = 'label',
                          insidetextfont = list(color = '#FFFFFF'),
                          hoverinfo = 'text',
                          text = ~paste(prop,'%'),
                          marker = list(line=list(color = '#FFFFFF', width=1)),
                          showlegend = FALSE)
    
    piechartly <- piechartly %>% layout(title = 'Portfolio Distribution',
                                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    piechartly
    return(piechartly)
  
  })
  
  #Porfolio Forecast Graphs
  output$portfolio_forecast <- renderPlotly({
    numStocks <- length(selected_stocks_inv())
    main_prophet_df <- FALSE
    main_holt_df <- FALSE
    if (numStocks > 0) {
      positive_stocks <- c()
      for (i in 1:length(selected_stocks_inv())) {
        if (input_sliders()[[paste("slider",i,sep = "")]] > 0) {
          positive_stocks <- append(positive_stocks,selected_stocks_inv()[i])
        }
      }
      counter <- as.numeric(1)
      if (length(positive_stocks > 0)) {
        for (symbol in positive_stocks) {
          biz_day_seq <- seq(start_date_port(), end_date_port()+h_Portfolio(), "days")[isBizday( timeSequence(start_date_port(),end_date_port()+h_Portfolio()), holidayNYSE())]
          #day_seq <- seq(start_date_port(), end_date_port()+h_Portfolio(), "days")
          #NOTE: IF THERE'S AN ERROR REGARDING THE DATES OF THE DATA.FRAMES TRY UNCOMMENTING ABOVE LINE AND COMMENTING THE ONE ABOVE THAT ONE AND CHANGING biz_day_seq TO day_seq BELOW
          all_data <- data[[symbol]][biz_day_seq,6] #selecting only the rows in the given time frame of the training data + prediction horizon
          all_data_df <- data.frame(all_data) #converting all_data into a data.frame
          colnames(all_data_df) <- "y" #naming the adjusted price column "y"
          all_data_df$ds <- rownames(all_data_df) #creating a new column called "ds" that contains the date
          all_data_df$ds <- as.Date(all_data_df$ds) #converting data from chr into Date type
          rownames(all_data_df) <- NULL #removing the date from the index of the data.frame
          all_data_df <- all_data_df[c("ds","y")]

          training_df <- data.frame(all_data[seq(start_date_port(), end_date_port(), "days")]) #selecting only the data for the training period
          setDT(training_df, keep.rownames = TRUE) #converts data frame to data.table by reference (essentially converts index to a column called "rn")
          colnames(training_df) <- c("ds", "y") #ds is date, y is price
          
          y_actual <- all_data_df$y #storing actual y values into array
          
          #PROPHET
          prophet_model <- prophet(training_df, yearly.seasonality = input_seasonal2(), weekly.seasonality = FALSE)
          future_prophet <- make_future_dataframe(prophet_model, periods = h_Portfolio()) #data frame with future dates for forecasting
          #the following line use the model to forecast and fill in the future dataframe. Future prophet has several columns that contain information about
          #the prediction and the model. The relevant columns for this app are: ds, yhat_lower, yhat_upper, yhat
          forecast_prophet <- predict(prophet_model, future_prophet)
          forecast_prophet$ds <- as.Date(forecast_prophet$ds) #changing format of ds from POSIXct to Date
          prophet_df <- full_join(all_data_df, forecast_prophet, by="ds") #joining forecast and actual data into one data.frame
          rownames(prophet_df) <- prophet_df$ds
          prophet_df <- prophet_df[isBizday( as.timeDate(prophet_df$ds), holidayNYSE()),] #subsetting only business days
          length(y_actual) <- dim(prophet_df)[1] #making array the same size as the training period + prediction, filling with NA to reach the length if necessary
          prophet_df$y <- y_actual #y column is NA after the full_join because it does not appear on forecast_prophet. This lines fills in the column
          prophet_df <- prophet_df[c("ds","yhat_lower","yhat_upper","yhat","y")] #dropping the irrelevant columns
          rownames(prophet_df) <- prophet_df$ds #setting rownames equal to the dates
          
          prophet_df$y_train <- prophet_df$y 
          prophet_df$y_train[prophet_df$ds > end_date_port()] = NA #y_train only
          prophet_df$y_test <- prophet_df$y
          prophet_df$y_test[prophet_df$ds < end_date_port()+1] = NA #y_test only 
          
          
          #prophet_df$yhat_upper_test <- prophet_df$yhat_upper
          prophet_df$yhat_upper[prophet_df$ds < end_date_port()+1] = NA
          #prophet_df$yhat_lower_test <- prophet_df$yhat_lower
          prophet_df$yhat_lower[prophet_df$ds < end_date_port()+1] = NA
          prophet_df$yhat[prophet_df$ds < end_date_port()+1] = NA
                
          
          #HOLT WINTERS
          data_length <- as.integer(end_date_port() - start_date_port())/365.25 #data length in years of the training data
          holt_ts <- ts(training_df$y, frequency = as.integer(nrow(training_df)/data_length)) #time series of the training data to use HoltWinters method
          
          holt_model <-HoltWinters(holt_ts, gamma = input_seasonal2(), seasonal = input_seasonal_type2())

          forecast_holt <- predict(holt_model, n.ahead = h_Portfolio(),  prediction.interval=T,  level=.95)
          forecast_holt <- data.frame(ds = seq(end_date_port()+1, end_date_port()+h_Portfolio(),"days"), 
                                      yhat = as.data.frame(forecast_holt)$fit, 
                                      yhat_lower = as.data.frame(forecast_holt)$lwr,
                                      yhat_upper = as.data.frame(forecast_holt)$upr)
          holt_df <- full_join(all_data_df, forecast_holt, by ="ds")
          rownames(holt_df) <- holt_df$ds
          holt_df <- holt_df[isBizday( as.timeDate(holt_df$ds), holidayNYSE()),] #subsetting only business days
          length(y_actual) <- dim(holt_df)[1]
          holt_df$y <- y_actual #y column is NA after the full_join because it does not appear on forecast_prophet. This lines fills in the column
          rownames(holt_df) <- holt_df$ds
          holt_df$y_train <- holt_df$y
          holt_df$y_train[holt_df$ds > end_date_port()] = NA #y_train only
          holt_df$y_test <- holt_df$y
          holt_df$y_test[holt_df$ds < end_date_port()+1] = NA #y_test only 
          
          stock_percent <- input_sliders()[[paste("slider",counter,sep="")]]
          y_train_end <- tail(na.omit(prophet_df$y_train),1)
          multiplier <- input_init_capital()*stock_percent/(y_train_end)
          
          if (typeof(main_prophet_df) == 'logical') {
            main_prophet_df <- data.frame(ds = prophet_df$ds)
          }
          
          if (typeof(main_holt_df) == 'logical'){
            main_holt_df <- data.frame(ds = holt_df$ds)
          }
          
          
          main_prophet_df[[paste("Precio Real ($) ",symbol,sep="")]] <- round(prophet_df$y_train*multiplier,2)
          main_prophet_df[[paste("Precio Futuro Real ($) ",symbol,sep="")]] <- round(prophet_df$y_test*multiplier,2)
          main_prophet_df[[paste("Precio Pronosticado ($) ",symbol,sep="")]] <- round(prophet_df$yhat*multiplier,2)
          main_prophet_df[[paste("Límite Inferior ($) ",symbol,sep="")]] <- round(prophet_df$yhat_lower*multiplier,2)
          main_prophet_df[[paste("Límite Superior ($) ",symbol,sep="")]] <- round(prophet_df$yhat_upper*multiplier,2)
          
          main_holt_df[[paste("y_train",symbol,sep="")]] <- round(holt_df$y_train*multiplier,2)
          main_holt_df[[paste("y_test",symbol,sep="")]] <- round(holt_df$y_test*multiplier,2)
          main_holt_df[[paste("yhat",symbol,sep="")]] <- round(holt_df$yhat*multiplier,2)
          main_holt_df[[paste("yhat_lower",symbol,sep="")]] <- round(holt_df$yhat_lower*multiplier,2)
          main_holt_df[[paste("yhat_upper",symbol,sep="")]] <- round(holt_df$yhat_upper*multiplier,2)
  
          counter <- counter+1
        } # <- end for loop statement
        
        prediction_start = prophet_df[prophet_df$ds == end_date_port(),"y"]
        yhat_end = tail(prophet_df$yhat,1)
        y_train_end = tail(na.omit(prophet_df$y_train),1)
        
        prophet_plot <- ggplot(main_prophet_df, aes(x=ds)) + 
          theme(plot.title = element_text(size = 15),legend.position="bottom")
        
        holt_plot <- ggplot(main_holt_df,  aes(x=ds)) + 
          theme(plot.title = element_text(size = 15),legend.position="bottom")
        
        color_counter <- 1
        
        #initializing combined columns
        main_prophet_df$combined_y_train <- prophet_df$y_train*0 #this will initialize all the values to 0 but will leave the NAs as NAs
        main_prophet_df$combined_y_test <- prophet_df$y_test*0
        main_prophet_df$combined_yhat <- prophet_df$yhat*0
        main_prophet_df$combined_yhat_lower <- prophet_df$yhat_lower*0
        main_prophet_df$combined_yhat_upper <- prophet_df$yhat_upper*0
        
        main_holt_df$combined_y_train <- holt_df$y_train*0 #this will initialize all the values to 0 but will leave the NAs as NAs
        main_holt_df$combined_y_test <- holt_df$y_test*0
        main_holt_df$combined_yhat <- holt_df$yhat*0
        main_holt_df$combined_yhat_lower <- holt_df$yhat_lower*0
        main_holt_df$combined_yhat_upper <- holt_df$yhat_upper*0
        
        
        for (symbol in positive_stocks) {
          
          prophet_plot <- prophet_plot + 
            geom_point(aes(y=.data[[paste("Precio Real ($) ",symbol,sep="")]]), size=0.5,alpha=0.8, col=my_colors[color_counter], shape=4) +
            geom_point(aes(y=.data[[paste("Precio Futuro Real ($) ",symbol,sep="")]]), size=0.8,alpha=0.8, col=my_colors[color_counter], shape =4) +
            geom_line(aes(y=.data[[paste("Precio Pronosticado ($) ",symbol,sep="")]]), size=0.5, alpha=0.8, col="#ed8d3e") + 
            geom_ribbon(aes(ymin=.data[[paste("Límite Inferior ($) ",symbol,sep="")]], ymax=.data[[paste("Límite Superior ($) ",symbol,sep="")]]), fill=my_colors[color_counter], alpha=0.25)
          
          holt_plot <- holt_plot + 
            geom_point(aes(y=.data[[paste("y_train",symbol,sep="")]]), size=0.5,alpha=0.8, col=my_colors[color_counter], shape =4) +
            geom_point(aes(y=.data[[paste("y_test",symbol,sep="")]]), size=0.5,alpha=0.8, col=my_colors[color_counter], shape =4) +
            geom_line(aes(y=.data[[paste("yhat", symbol,sep="")]]),size=0.5, alpha=0.8, col="#1c5bba")+ 
            geom_ribbon(aes(ymin=.data[[paste("yhat_lower",symbol,sep="")]], ymax=.data[[paste("yhat_upper",symbol,sep="")]]),  fill=my_colors[color_counter],  alpha=0.25)
          
          color_counter <- color_counter+1
      
          main_prophet_df$combined_y_train <- main_prophet_df$combined_y_train + main_prophet_df[[paste("Precio Real ($) ",symbol,sep="")]]
          main_prophet_df$combined_y_test <- main_prophet_df$combined_y_test + main_prophet_df[[paste("Precio Futuro Real ($) ",symbol,sep="")]]
          main_prophet_df$combined_yhat <- main_prophet_df$combined_yhat + main_prophet_df[[paste("Precio Pronosticado ($) ",symbol,sep="")]]
          main_prophet_df$combined_yhat_lower <- main_prophet_df$combined_yhat_lower + main_prophet_df[[paste("Límite Inferior ($) ",symbol,sep="")]]
          main_prophet_df$combined_yhat_upper <- main_prophet_df$combined_yhat_upper + main_prophet_df[[paste("Límite Superior ($) ",symbol,sep="")]]
          
          main_holt_df$combined_y_train <- main_holt_df$combined_y_train + main_holt_df[[paste("y_train",symbol,sep="")]]
          main_holt_df$combined_y_test <- main_holt_df$combined_y_test + main_holt_df[[paste("y_test",symbol,sep="")]]
          main_holt_df$combined_yhat <- main_holt_df$combined_yhat + main_holt_df[[paste("yhat",symbol,sep="")]]
          main_holt_df$combined_yhat_lower <- main_holt_df$combined_yhat_lower + main_holt_df[[paste("yhat_lower",symbol,sep="")]]
          main_holt_df$combined_yhat_upper <- main_holt_df$combined_yhat_upper + main_holt_df[[paste("yhat_upper",symbol,sep="")]]
        }
        
        main_prophet_future <- main_prophet_df[main_prophet_df$ds > end_date_port(),]
        main_holt_future <- main_holt_df[main_holt_df$ds > end_date_port(),]
        
        main_prophet_future_readable <- data.frame(lapply(main_prophet_future, function(y) if(is.numeric(y)) round(y, 2) else y)) 
        main_holt_future_readable <- data.frame(lapply(main_holt_future, function(y) if(is.numeric(y)) round(y, 2) else y))
        
        names(main_prophet_future_readable)[names(main_prophet_future_readable) == 'combined_y_test'] <- 'Precio Futuro Real del Portafolio ($)'
        names(main_prophet_future_readable)[names(main_prophet_future_readable) == 'combined_yhat'] <- 'Pronóstico del Portafolio ($)'
        names(main_prophet_future_readable)[names(main_prophet_future_readable) == 'combined_yhat_lower'] <- 'Límite Inferior Pronosticado ($)'
        names(main_prophet_future_readable)[names(main_prophet_future_readable) == 'combined_yhat_upper'] <- 'Límite Superior Pronosticado ($)'
        
        names(main_holt_future_readable)[names(main_holt_future_readable) == 'combined_y_test'] <- 'Precio Futuro Real del Portafolio ($)'
        names(main_holt_future_readable)[names(main_holt_future_readable) == 'combined_yhat'] <- 'Pronóstico del Portafolio ($)'
        names(main_holt_future_readable)[names(main_holt_future_readable) == 'combined_yhat_lower'] <- 'Límite Inferior Pronosticado ($)'
        names(main_holt_future_readable)[names(main_holt_future_readable) == 'combined_yhat_upper'] <- 'Límite Superior Pronosticado ($)'
        
        names(main_prophet_future_readable)[names(main_prophet_future_readable) == 'ds'] <- 'Fecha'
        names(main_holt_future_readable)[names(main_holt_future_readable) == 'ds'] <- 'Fecha'
        
        main_prophet_future_readable %>% mutate_if(is.numeric, round, digits=2)
        main_holt_future_readable %>% mutate_if(is.numeric, round, digits=2)
      
        prophet_plot_combined <- ggplot(main_prophet_future_readable, aes(x=`Fecha`)) + 
          geom_line(data = main_prophet_future_readable, aes(y=`Pronóstico del Portafolio ($)`), size=0.75, alpha=1, col='blue') +
          geom_point(data = main_prophet_future_readable, aes(y=`Precio Futuro Real del Portafolio ($)`), size=0.75,col = 'black') +
          geom_ribbon(data = main_prophet_future_readable, aes(ymin=`Límite Inferior Pronosticado ($)`, ymax=`Límite Superior Pronosticado ($)`), fill="#ed8d3e", alpha=0.25) +
          theme(plot.title = element_text(size = 15),legend.position="bottom")
          
        
        holt_plot_combined <- ggplot(main_holt_future_readable, aes(x=`Fecha`)) + 
          geom_line(data = main_holt_future_readable, aes(y=`Pronóstico del Portafolio ($)`), size=0.75, alpha=1, col='blue') + 
          geom_point(data = main_holt_future_readable, aes(y=`Precio Futuro Real del Portafolio ($)`), size=0.75,col = 'black') +
          geom_ribbon(data = main_holt_future_readable, aes(ymin=`Límite Inferior Pronosticado ($)`, ymax=`Límite Superior Pronosticado ($)`),  col ='#1c5bba',  alpha=0.25) +
          theme(plot.title = element_text(size = 15),legend.position="bottom")
        
        prophet_plotly_combined <- ggplotly(prophet_plot_combined) %>% layout(
          xaxis = list(
            title = "Fecha",
            tickfont = list(size=12)
          ), 
          yaxis = list(
            title = "Valor del Portafolio (USD $)",
            tickfont = list(size=12)
          ),
          font = list(
            size = 12
          )
        )
        
        holt_plotly_combined <- ggplotly(holt_plot_combined) %>% layout(
          xaxis = list(
            title = "Fecha",
            tickfont = list(size=12)
          ), 
          yaxis = list(
            title = "Valor del Portafolio (USD $)",
            tickfont = list(size=12)
          ),
          font = list(
            size = 12
          )
        )
        
        prophet_plot <- prophet_plot + 
          geom_vline(aes(xintercept=as.numeric(as.Date(end_date_port()+1))), cex=0.3, col="steelblue4", linetype = "dashed")
        
        prophet_plotly <- ggplotly(prophet_plot)%>% layout(
          xaxis = list(
            title = "Fecha",
            tickfont = list(size=12)
          ), 
          yaxis = list(
            title = "Precio de Acción (USD $)",
            tickfont = list(size=12)
          ),
          font = list(
            size = 12
          )
        )
        
        holt_plot <- holt_plot +
          geom_vline(aes(xintercept=as.numeric(as.Date(end_date_port()+1))), cex=0.3, col="steelblue4", linetype = "dashed")
        
        holt_plotly <- ggplotly(holt_plot) %>% layout(
          xaxis = list(
            title = "Fecha",
            tickfont = list(size=12)
          ), 
          yaxis = list(
            title = "Precio de Acción (USD $)",
            tickfont = list(size=12)
          ),
          font = list(
            size = 12
          )
        )
        
        profit <- round(tail(main_prophet_future$combined_y_test,1), digits = 2) - input_init_capital()
        if (profit >= 0) {
          text_in_title = paste0("Genial! Tuviste una ganacia de $", profit," con este portafolio virtual en el transcurso de ",h_Portfolio()," días.\n\n")
        } else {
          text_in_title = paste0("Oh no! Tuviste una pérdida de $", -profit, " con este portafolio virtual en el transcurso de ",h_Portfolio()," días.\n\n")
        }
        
        subplot(prophet_plotly_combined, holt_plotly_combined,  prophet_plotly, holt_plotly, nrows = 2, margin=0.08, titleX = TRUE, titleY=TRUE, shareY=F) %>% layout(
          title = list(
            text = text_in_title,
            font = list(size=24),
            x = 0.5,
            xanchor = 'center',
            yanchor = 'top'
          ),
          margin = 0.5,
          annotations = list(
            list(
              x = 0.225, 
              y = 1.0, 
              font = list(size = 16), 
              text = "Pronóstico del Portafolio por el modelo Prophet", 
              xref = "paper", 
              yref = "paper", 
              xanchor = "center", 
              yanchor = "bottom", 
              showarrow = FALSE
            ), 
            list(
              x = 0.775, 
              y = 1.0, 
              font = list(size = 16), 
              text = "Pronóstico del Portafolio por el modelo Holt Winters", 
              xref = "paper", 
              yref = "paper", 
              xanchor = "center", 
              yanchor = "bottom", 
              showarrow = FALSE
            ),
            list(
              x = 0.225, 
              y = 0.425, 
              font = list(size = 16), 
              text = "Pronóstico por acción por el modelo Prophet", 
              xref = "paper", 
              yref = "paper", 
              xanchor = "center", 
              yanchor = "bottom", 
              showarrow = FALSE
            ), 
            list(
              x = 0.775, 
              y = 0.425, 
              font = list(size = 16), 
              text = "Pronóstico por acción por el modelo Holt Winters", 
              xref = "paper", 
              yref = "paper", 
              xanchor = "center", 
              yanchor = "bottom", 
              showarrow = FALSE
            )
          )
        ) #<- end layout
        
      } # end 2nd if statement
    }# <- end 1st if statment
  }) # <- end output Plotly
  
}