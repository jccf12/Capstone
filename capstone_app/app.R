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

max_plots <- 5

algorithm_names <- c("Holt's Exponential Smoothing")
load_quantlib_calendars('UnitedStates/NYSE',from='2000-01-01', to='2020-12-10')

investing_options <- c("Apple", "Google")
symbols <- c("AAPL", "GOOG", "ICON.L", "VOD.L")

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
        "Selección de datos",
        dateInput('start_date', 'Fecha inicial', value = "2019-01-01"),
        dateInput('end_date', 'Fecha final', value = "2019-12-31"),
        tags$h1("Search Input"),
        br(),
        searchInput(
          inputId = "search_symbol", label = "Enter your text",
          placeholder = "A placeholder",
          btnSearch = icon("search"),
          btnReset = icon("remove"),
          width = "450px"
        ),
        br(),
        checkboxGroupInput('selected_stocks', "Selección de stocks",
                           symbols),
        numericInput('init_capital', 'Capital de inversión inicial ($MXN)', min = 0, value = 100),
        "Pronósticos",
        selectInput('forecast_alg', 'Suavizamiento exponencial Holt Winters', algorithm_names),
        numericInput('h_holt', 'Horizonte de Predicción', min = 1, value = 20)
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

  symbol <- reactive({
    input$search
  })
  
  selected_h <- reactive({
    input$h_holt
  })
  
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
      plotOutput(plotname, height = 380, width = 350)
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
        my_col <- paste(my_symbol,"Adjusted",sep = ".")
        
        start_range <- bizdays::bizdays(index(data[[my_symbol]][1]), input$start_date, 'QuantLib/UnitedStates/NYSE')+1
        end_range <- start_range + bizdays::bizdays(input$start_date, input$end_date, 'QuantLib/UnitedStates/NYSE')-1
        
        fitted_stock <- HoltWinters(data[[my_symbol]][,my_col][c(start_range:end_range)], gamma=FALSE)
        stock_forecast <- forecast:::forecast.HoltWinters(fitted_stock, h=selected_h())
        forecast:::plot.forecast(stock_forecast, main = my_symbol)
      })
    })
  }
  
}

shinyApp(ui = ui, server = server)
