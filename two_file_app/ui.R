ui <- fluidPage(
  theme = shinytheme("flatly"),
  tabsetPanel(
    id = 'main-tabs',
    
    tabPanel(
      id = 'landing-tab',
      title = 'Home',
      div(
        column(width=2),
        column(
          h1('Welcome!',
             style = "text-align:center;"),
          div(
              h4("Are interested in exploring investing opportunities in the stock market?
                  Do you already know about the stock market but you would like to see if it is predictable?
                  Do you just want to learn more about the stock market or prediction algoritghms?",
                style="text-align:justify;line-height:1.6"),
              h4(strong("Then, this is the right place for you!"),
                style="text-align:center;line-height:1.6"),
             style="color:white;background-color:#02A4D3;padding:15px;border-radius:10px"),
          br(),
          "It is possible that you have been curious about investing in the stock market, but you never really know how it works. 
          Maybe you do have an idea how it works but you don't know if it's for you. Or maybe you are already an expert on it, and are already excited
          about predicting the stock market, and you want to know what tools are out there that can help you with it. Whatever is the case for you, I hope that this site 
          will be somehow useful for you.",
          h2("What you will find here"),
          column(
            div(
              h4("Analysis"),
              p("In this tab, you will be able to explore up to 50 different of the biggest companies in Mexico and United States."),
              p("You can look at the value of each their shares (which refer to a small fraction of the company) and how it was evolved through time."),
              p("You will also be able to test two popular tools that have been used to make predictions of these share prices and compare them."),
              style="color:black;background-color:lavender;padding:15px;border-radius:10px"
            ),
            width = 4,
            style="padding:15px;border-radius:10px"
          ),
          column(
            div(
              h4("Portfolio"),
              p("Here you will have an opportunity to create your own virtual portfolio ! A hypothetical one, of course, which means that you won't need to 
              actually invest any money."),
              p("You will be able to go 'back to the past' and see what would have happened if you had invested on some of these 50 companies. Would you have made money?
              Would you have become rich? Or maybe you lost it all... Let's find out!"),
              p("You can also use the tools that you explored in the Analysis tab to make predictions into the future! Again you can create whichever hypothetical porfolio
              you like."),
              style="color:black;background-color:lavender;padding:15px;border-radius:10px"
            ),
            style="padding:15px;border-radius:10px",
            width = 4
          ),
          column(
            div(
              h4("About"),
              p("On this page you can find more information about what happens 'under the hood' to generate the interactive graphs on the Analysis and Portfolio tabs."),
              p("If you have time, I would suggest checking this page out before you dive into looking at the data and simulating portfolios."),
              style="color:black;background-color:lavender;padding:15px;border-radius:10px"
            ),
            width = 4,
            style="padding:15px;border-radius:10px"
          ),
          width = 8
        )
        
        )
    ),
    
    #ANALYSIS TAB
    tabPanel(
      id = 'analysis-tab',
      title = 'Analysis',
      headerPanel('Data Analysis'),
      sidebarPanel(
        "Training period",
        helpText("DO NOT CHOOSE WEEKEND DATES"),
        dateInput('start_date_ana', 'Initial date', value = "2018-01-01", max = Sys.Date()),
        dateInput('end_date_ana', 'End date', value = "2020-12-31", max = Sys.Date()),
        numericInput('h_ana', 'Prediction Horizon', min = 1, value = 180),
        wellPanel(id = "checkbox_panel1",
                  style = "overflow-y:scroll; max-height: 300px",
                  checkboxGroupInput('selected_stocks_ana', 
                                     "Stock selection for visualization",
                                     symbols, selected = symbols[1:1]
                  )
        ),
        helpText("The two following inputs have to do with the seasonality component of the prediction algorithms. If you do not know
                 what this means, don't worry! Just leave the pre-selected values. If you want to learn more about them, just visit the About tab!"),
        checkboxInput("seasonal","Use yearly seasonal factor (both Prophet and Holt Winters)", TRUE),
        selectInput('seasonal_type', 'Type of seasonal model used for the Holt Winters algorithm', seasonality_types),
        actionButton("runAnalysisButton", "Run Analysis"),
        width = 3
      ), # end sidebarPanel
      
      mainPanel(
        id = 'inner-main-analysis',
        tabsetPanel(
          id = 'analysis-inner-tabset',
          tabPanel(
            value = 'data-panel',
            title = 'Data',
            tableOutput('table1'),
            icon = icon("table")
          ),
          tabPanel(
            value = 'visualization-panel',
            title = 'Visualization',
            br(),
            p("Each pair of graphs for each of the selected stocks, shows the historical data over the stock for the selected training period as well as a
              fitted model to the data. On the Prophet graph (on the left of each pair), the fitted model to the training data is shown in yellow. This model 
              extends to the prediction zone (to the right of the dashed vertical line within the same graph) where it turns orange, which is simply the result 
              of 'continuing' the line for the prediction horizon. This line (the yellow-orange line) is what we call the model. On the Holt graphs (right side
              of the pairs), the model is shown in two shades of blue (the lighter shade for the fitted model to the training data, and the darker blue for the 
              predicted values)", 
              style = "font-size:1.1em;line-height: 1.8"),
            div(tags$ul("Thing to focus on:",
                        tags$li("How has the price evolved for each of the selected stocks? Does it seem to go up and down a lot? Does is have any sharp increase
                                or decrease?"),
                        tags$li("How well does each the models predict what actually happened. Did the prediction agree about the portfolio going up or down? 
                                Is the actual value of the price within the margins of error of the prediction?"),
                        tags$li("What could have affected bad or good predictions? Was there an important world event that might have influence the price (e.g., a
                                global pandemic)?")
                        ),
                style="text-align:justify;color:white;background-color:#02A4D3;padding:20px;border-radius:7px;font-size:1.2em;line-height: 1.8"),
            br(),
            uiOutput('plots'),
            br(),
            br(),
            icon = icon("chart-line")
          )
        ), # <- end tabsetPanel 'analysis-inner-tabset'
        width = 9
      ) # <- end mainPanel 'inner-main-analysis'
    ), # <-- end tabPanel 'analysis-tab'
    
    #PORTFOLIO TAB
    tabPanel(
      id = 'portfolio-tab',
      title = 'Portfolio',
      headerPanel("My Virtual Portfolio"),
      sidebarPanel(
        helpText("Choose a pre-defined portfolio composition based on the desired risk level or manually choose stocks you wish to be in your portfolio."),
        helpText("The risk level of a stock is assessed based on their anual volatility and the categorization in 'Low',
                 'Medium','High' is based Stockopedia Risk Ratings"),
        p(tags$a(href="https://help.stockopedia.com/product-guide/stockranks/advanced/the-riskratings/", 
                 "Stockopedia Risk Ratings")),
        selectInput('risk_level', 'Risk level', risk_levels),
        wellPanel(id = "checkbox_panel2",style = "overflow-y:scroll; max-height: 300px",
                  checkboxGroupInput('selected_stocks_inv', "Stock Selecton for Virtual Portfolio",
                                     symbols)),
        actionButton('selectStocksButton', 'Select Stocks'),
        helpText("Select an intial investing amount "),
        numericInput('init_capital', 'Initial Investment Amount ($USD)', min = 0, value = 100),
        "Training data",
        helpText("DO NOT CHOOSE WEEKEND DATES"),
        dateInput('start_date_port', 'Start Date', value = "2018-01-01", max = Sys.Date()),
        dateInput('end_date_port', 'End Date', value = "2020-12-31", max = Sys.Date()),
        helpText("Select a prediction horizon. This is the number of days into the future for which you want to see the evolution of the selected portfolio."),
        numericInput('h_Portfolio', 'Prediction Horizon', min = 1, value = 60),
        wellPanel(id = "checkbox_panel3",style = "overflow-y:scroll; max-height: 500px",
                  uiOutput("portfolio_dist")),
        helpText("The two following inputs that have to do with the seasonality component of the prediction algorithms. If you do not know
                 what this means, don't worry! Just leave the pre-selected values. If you want to learn more about them, just visit the About tab!"),
        checkboxInput("seasonal2","Use yearly seasonal factor (both Prophet and Holt Winters)", TRUE),
        selectInput('seasonal_type2', 'Type of seasonal model used for the Holt Winters algorithm', seasonality_types),
        actionButton('runPortfolio', 'Run Portfolio'),
        width = 3
        ),
      mainPanel(
        id = 'inner-main-portfolio',
        br(),
        plotlyOutput("pie_chart") %>% withSpinner(color="#0dc5c1"),
        helpText("Hover over the slices on the pie chart above to see the exact percentage of your portfolio assigned to each of the individual stocks", style="text-align:center"),
        br(),
        p("The top two graphs below show the evolution of your portfolio over the prediction horizon. This means that whatever the amount that you chose to start with 
          (Initial Investment Amount ($USD)), if you were to invest it on the selected stocks and selected portions of that amount (shown on the pie chart), would change
          as shown by the black dots on the top two graphs below. Each of those graphs, also shows you what each of the models would tell you it would happen.", style = "font-size:1.1em;line-height: 1.8"),
        div(tags$ul("There are three things you should focus on what is shown in the figure below:",
           tags$li("The actual evolution of the selected porfolio shown in black in either of the top two graphs. This shows what would have",
                   strong("actually"),"happened if someone had invested the amount of money selected based on the porfolio distribution shown in the pie chart"),
           tags$li("How well each of the models predicted what actually happened. Did the prediction agree about the portfolio going up or down? Is the actual value 
                   of the porfolio within the the margins of error of the model predictions?"),
           tags$li("On the two graphs below, is there any specific stock that seemed to be reponsible for the increase or decrease of the porfolio?")
           ),
          style="text-align:justify;color:white;background-color:#02A4D3;padding:20px;border-radius:7px;font-size:1.2em;line-height: 1.8"),
        br(),
        plotlyOutput("portfolio_forecast", height = "800px") %>% withSpinner(color="#0dc5c1"),
        br(),
        helpText("Hover over the graphs on the figure above to get more information about what each line shows and the exact value at a given time", style="text-align:center"),
        p("You can try modifying your input to see what would have happened if you had assigned more (or less) of the initial amount to a given stock.
           Maybe add or remove stocks to see how this would change. You can also change the training data or choose a different time period.", style="text-align:justify;color:white;background-color:#02A4D3;padding:20px;border-radius:7px;font-size:1.2em;line-height: 1.8"),
        width = 9
      ) # <- end mainPanel 'inner-main-portfolio
    ), # <-- end tabPanel 'protfolio-tab'
    
    #ABOUT TAB
    tabPanel(
      id = 'about-tab',
      title = 'About',
      headerPanel('How to use this app'),
      tabsetPanel(
        id = 'about-inner-tabset',
        tabPanel(
          id = 'about-how-tab',
          title = 'How to Use',
          fluidRow(column(
                     #tags$img(src="Antioquia.png",width="200px",height="260px"),
                     width=2),
                   column(
                     
                     br(),
                     p("The aim of this application is for you to be able to explore the stock market in an interactive way that will 
                       hopefully allow you to better understand your investing options. Simply talking about the stock market, let alone investing in it, 
                       might seem mind-boggling. This application, at first, with a bunch of graphs and numbers, could be a little scary as well. However, 
                       upon a little bit of inspection, through which I will guide you, you will be able to make better-informed financial desions. ", 
                       style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                     p("The aim of this application is", strong("NOT"), "to persuade you to invest in the stock market. The only purpose
                       of this application is to expand your horizon of financial options. You might, upon learning more about the stock market through the app,
                       decide that the stock market is not for you, and that is also completely fine.\n 
                       My only goal is for you to", em("learn"), "and, based on your learnings, decide
                       what works best for you, financially speaking. This app does is not meant to be taken as financial advise and I
                       am not responsible for the financial decisions you make.",
                       style="text-align:justify;color:black;background-color:papayawhip;padding:30px;border-radius:10px"),
                     width=8),
                   column(
                     width=2)
                   #02A4D3
          ),
          fluidRow(
            column(width=2),
            column(
              h1("Analysis Tab"),
              width = 8)
          ),
          fluidRow(column(
            width=2),
            column(
              p("In this tab, you will be able to look at the price of different stocks. At first, under the 'Data' tab, you will see a general overview in the form
                of a table that shows the current price of the stock and the % change in the last day, which means by what percentage the price
                of the stock changed in the last day. For example, if Apple's stock was valued at $100 yesterday, and today is valued at $102,
                the % change will show 2%, because the increase of $2 represents 2% of the price yesterday."),
              p("Under the tab 'Visualization', you can now see the", em("historical data"), "of the stocks that you can choose using the sidebar Panel.
                On the sidebar panel you will also be able to select the", em("training period"),". The training period will determine the historical data
                that is given to the models for predictions. This application uses Prophet's Time Series Forecasting by Facebook and Holt Winters Exponential
                Smoothing. To better understand how these models work, refer to the tabs 'Prophet' and 'Holt'."),
              p("The models arre used to make predictions about what will happen to the price of the stock in the following days, weeks, or years.
                The", em("prediction horizon") ,"on the sidebar Panel lets you choose how many days into the future you want the models to make the predictions for. 
                For example, if you choose 180, you are going to obtain what each model predicts is going to happen to the share price of the stocks in the next
                180 days (around 6 months)."),
              h2("How to Interpret Graphs"),
              p("For each of the selected stocks, you will be able to see the historical data for the selected training period, and the prediction. These will
                be easily identifyable by the dashed vertical line on each of the plots, which denotes the end of the training data. That is, the part of the
                graph to the left of this line refers to the historical data, and the part to the right of line refers to the predictions."),
              width=5),
            column(
              p(strong("Historical Data"),"in the context of stocks, simply refers to the price of the stock in the past. The historical data
                shows us what has the price been each day during a given period of time in the",em("past"),". We look at this data, because we
                hope to identify", em("trends"), "or", em("patterns"), "and potentially use that information to", em("predict"),"the future.",
                style="text-align:justify;color:white;background-color:#02A4D3;padding:15px;border-radius:10px"),
              p(strong("Training Period"), "refers to the period of historical data that will be used to", em("predict"),"the future. It
                is called ", em("training"),"period because the predictions will be made by a ", em("model"),"that we have to teach, or train,
                to make good predictions. We train the model by showing it some data in the past, and let it identify patterns. More information
                about the models used in this application can be found under the tabs for Holt and Prophet.",
                style="text-align:justify;color:white;background-color:#02A4D3;padding:15px;border-radius:10px"),
              width=3),
            column(
              width=2)
          ), #<- end fkuid Row
          fluidRow(
            column(width=2),
            column(
              tags$img(src="how_to1.png", width ="100%"),
              width = 8)
          ),
          fluidRow(
            column(width=2),
            column(
              br(),
              p("Each of the graphs has a fitted model to the data. On the Prophet graph (left), the fitted model to the training data is shown in yellow.
                This model extends to the prediction zone (to the right of the dashed vertical line) where it turns orange, which is simply the result of
                'continuing' the line for the next 180 days. This line (the yellow-orange line) is what we call the model. The part that is in orange refers
                prediction that the model makes for the following 180 days (first half of the year 2018), based on data from 2010. It is a prediction because
                to create this line, we had only the training data available. That is, we knew nothing about what actually happened to the price after the end
                of the training period (marked by the horizontal dashed line). The shaded region around the orange section refers to the margin of error of 
                the predictions. In purple, we have the actual Share Price of the stocks. To the left of the vertical dashed line, we can see how the actual data
                and the model are very close together. This is because the model", em("uses"), "the historical data to draw the yellow line. To the right, we can
                compare the predictions agains the real data to see how good were the predictions made by the model."),
              p("Everything said about the Prophet graph (left), applies to the Holt graph (right). In the case of Holt, the model and predictions are shown in
                shades of blue. The main difference between the two is that the method used to build each of the models is different. For details on how these
                models make predictions refer to the corresponding tabs."),
              br(),
              width = 8)
          ),
          icon = icon("info-circle")
        ),
        tabPanel(
          id = 'about-holt-tab',
          title = 'Holt Winters',
          fluidRow(
            column(width=2),
            column(
              h1('Holt Winters Exponential Smoothing'),
              p("Holt-Winters forecasting is a technique to model and predict the behavior of a time series, and is one of the most popular forecasting 
                techniques for time series. I will attempt to explain to you, in a high-level and as-easy-as-possible way how it works. To do so we will
                go, step by step, through the components of the algorithm: weighted average, exponetial smoothing, Holt exponential smoothing, and Holt 
                Winters exponential smoothing."),
              h2("Weighted Average"),
              p("A weighted average is simply an average of numbers where each of the numbers has a higher or smaller influence on the final average. For example, 
                a weighted average of the numbers [3,5,7] that assigns most influence to the first number, medium influence to the second, and small influence to 
                the last would be:"),
              tags$img(src="weighted_average.png", width ="60%", align = "center"),
              p("where the weights are 1.5, 1, and 0.5, respectively. In the context of the price of a stock, by assigning different influences to the price of the
                 stock at different times, we change how much the price of given days affects the future price since it is possible that some days are more influential 
                 than others. The algorithm will find which of these influences are best by testing different ones and seeing which result in better predictions using 
                 historical data."),
              h2("Exponential Smoothing"),
              p("This technique builds on the weighted-average discussed above by assigning an exponential decay to the prices of the stock. What this means is that 
                 we are going to care more about the more recent prices since we think that those have greater influence on tomorrow’s price than the prices further 
                 into the past. The way we reduce the influence on previous values is exponential! This means that they are very quick at decreasing the influence of 
                 past values."),
              h2("Holt Exponential Smoothing"),
              p("his variation of the Exponential Smoothing allows for trends to be identified. If we simply reduced the influence of past data exponentially, a lot 
                of value information of the past could be lost. Holt Exponential Smoothing helps us recover trends that happened in the past to compensate for the 
                exponential decay that was associated with past data."),
              h2("Holt Winters Exponential Smoothing"),
              p("Finally, the Holt-Winters Exponential smoothing adds one more component to the algorithm: seasonality. In real life, prices are very likely influenced
                by seasons (and here we do not talk about weather seasons necessarily, but rather different periods in the year which are associated with a periodic 
                behavior, that is, a behavior that repeats itself). For example, people might receive bonuses by the end of the year, and so it is a great opportunity 
                to use some of that money to make investments, or simply to spend it away. Either way, economic activity might be increased every winter due to this 
                phenomenon (which, of course, is more nuanced than how it’s put here) which affects the stock market. The seasonal component of the algorithm aims to 
                capture this behavior and incorporate it into the model to make better predictions."),
              style="line-height:1.5;font-size:1.2em",
              width = 8)
              ),
          icon = icon("chart-line")
        ),
        tabPanel(
          id = 'about-prophet-tab',
          title = 'Prophet',
          fluidRow(
          column(width=2),
          column(
            h1('Prophet Time Series Model'),
            p("Prophet uses a decomposable time series model with three main model components: growth, seasonality and holidays."),
            h2("Growth"),
            p("Prophet performs a regression using piecewise linear or logistic growth curve trend to detect changes in trends on the data"),
            h2("Seasonality"),
            p("Very similar to the Holt-Winters model, Prophet tries to identify seasonality that can be either yearly, weekly, or daily, 
              depending on the selected input."),
            h2("Holidays"),
            p("Remember the example about the end of the year and the bonuses and how they might have an effect on the stock market? Well, 
              Prophet tries to take a step further by allowing the user of the algorithm to provide a list of important holidays so that the 
              model can add influence to the prices on these dates and adjust the model accordingly."),
            style="line-height:1.5;font-size:1.2em",
            width = 8)
            ),
          icon = icon("chart-line")
        )
      ) # <- end tabsetPanel 'about-inner-tabset'
    ) # <-- end tabPanel 'about-tab'
      ) # <-- end tabsetPanel 'main-tabs'
) # <-- end fluid Page