library("shiny")
library("shinyWidgets")

ui <- fluidPage(
  br(),
  sliderTextInput(
    inputId = "mySliderText",
    label = "Month range slider:",
    choices = month.name,
    selected = month.name[c(4, 7)]
  ),
  verbatimTextOutput(outputId = "result")
)

server <- function(input, output, session) {
  output$result <- renderPrint(str(input$mySliderText))
}

shinyApp(ui = ui, server = server)