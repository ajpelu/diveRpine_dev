ui <- fluidPage(
  sliderInput("sb", "Small birds", 0, min = 0, max = 100),
  sliderInput("mb", "Medium birds", 0, min = 0, max = 100),
  sliderInput("ma", "Mammals", 0, min = 0, max = 100),
  actionButton("reset", "Reset")
)

server <- function(input, output, session) {
  observeEvent(input$reset, {
    updateSliderInput(input = "sb", value = 0)
    updateSliderInput(input = "mb", value = 0)
    updateSliderInput(input = "ma", value = 0)
  })
}


  shinyApp(ui, server)
