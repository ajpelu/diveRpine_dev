
library(shiny)
library(data.table)

ui <- fluidPage(

  mainPanel(
    titlePanel("Disperser"),
    tags$p(h4(strong("Small birds"))),
    sliderInput(inputId = "sb",
                label =
                  tags$p(h4(strong("Small"),
                            tags$img(src="smallbird.svg", height = '50', widht = '30'))),
                min = 0, max = 100, value = 0, step = 1),
    uiOutput("mb"),
    dataTableOutput("disptable"),
  )
)

server <- function(input, output, session) {

  output$mb <- renderUI({
    tagList(
      #tags$img(src="diveRpine_v1.svg", height = '50', widht = '30'),
    # tags$p(h4(strong("Medium birds")),
    #       ),
    sliderInput(inputId = "mb",
                label =
                  tags$p(h4(strong("Medium birds"),
                            tags$img(src="garrulus.png", height = '50', widht = '30'))),
                min = 0, max = 100 - input$sb, value = 0)
    )
  })

  perma <- reactive({
    100-(input$sb + input$mb)
  })





  output$disptable <- DT::renderDataTable({
    name_disperser <- c("Small birds", "Medium birds", "Mammals")
    dispersers <- c(
      as.character(
        tags$img(src="smallbird.svg", height = '50', widht = '30')),
      as.character(
        tags$img(src="garrulus.svg", height = '50', widht = '30')),
      as.character(
        tags$img(src="vulpes.svg", height = '50', widht = '30'))
      )
    percentage <- c(input$sb, input$mb, perma())

    datatable(cbind(Dispersers = name_disperser,
                    icon = dispersers,
                    Percentage = percentage),
              colnames = c("Disperser type", "", "%"),
              escape = FALSE,
              options = list(dom = 't'))
  })




  # }
  #   disperser_table(),
  #   options = list(dom = 't'),
  #   escape = FALSE)
  #
  #   #hover = TRUE, spacing = 'xs', align = 'c', digits = 0)
}

shinyApp(ui = ui, server = server)
