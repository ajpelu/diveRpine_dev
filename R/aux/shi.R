ui <- pageWithSidebar(
  headerPanel("Click the button"),
  sidebarPanel(
    sliderInput("obs", "Number of observations:",
                min = 0, max = 1000, value = 500),
    actionButton("goButton", "Go!")
  ),
  mainPanel(
    plotOutput("distPlot")
  )
)

server <- function(input, output) {
  output$distPlot <- renderPlot({

    if (input$goButton == 0)
      return()

    # Use isolate() to avoid dependency on input$obs
    dist <- isolate(rnorm(input$obs))
    hist(dist)
  })
}

shinyApp(ui, server)