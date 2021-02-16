library("shiny")
library("shinydashboard")
library("shinyWidgets")


############################ FUNCTIONS
source("init_params.R")
source("aux.R")
source("diveR_landscape.R")
source("plot_landscape_pine.R")

# UI ----------------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "diveRpine_dev"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      box(title = "Target pine plantation",
        sliderInput(
          inputId = "pp_size", label = "Patch Area",
          min = 200, max = 1500, value = 750
        ),
        awesomeRadio(
          inputId = "pp_density", label = "Tree density",
          choices = c("low", "medium", "high"),
          selected = "medium", status = "success", inline = TRUE
        ),
        awesomeRadio(
          inputId = "pp_pastUse", label = "Past Land Use of the Pine plantation",
          choices = c("Natural Forests", "Shrublands", "Pasture", "Croplands"),
          selected = "Shrublands", status = "success"
        )
       # actionButton("plotTargetPine", "Configure pine plantation")
      ),

      box(title = "Natural Forests",
          sliderInput(
            inputId = "nf_n", label = "Natural forests patch number",
            min = 1, max = 5, value = 2),
          sliderInput(
            inputId = "nf_size", label = "Natural forests patch size",
            min = 50, max = 500, value = c(100,250)),
          actionButton("addNatForest", "Add Natural forests")
      ),

      # box(plotOutput('initial_pine')),
      box(plotOutput('initial_landscape'))
    )
  )
)


server <- function(input, output) {

### ----------------------------------------------

# Size

# Density
pp_denR <- reactive({
  list(
    den = switch(input$pp_density,
                 "low" = "low",
                 "medium" = "medium",
                 "high" = "high"),
                 # "low" = 100, "medium" = 1250, "high" = 3000),
    col = switch(input$pp_density,
                 "low" = "#a1d99b",
                 "medium" = "#238b45",
                 "high" = "#00441b")
  )
})

# Past Use
pp_pastUseR <- reactive({
    switch(input$pp_pastUse,
           'Natural Forests' = 'Oak',
           'Shrublands' = 'Shrubland',
           'Pasture' = 'Pasture',
           'Croplands' = 'Crop')
  })


# # Create pine patch
# pine <- reactive({
#       create_pine(empty_landscape, pine_size = input$pp_size)
#   })

# isolate(input$pp_size) 1000

creaLandscape <- reactive({
  landscape <- landscapeR::makePatch(
    empty_landscape,
    val = 1, rast = TRUE, bgr = 0,
    size = isolate(input$pp_size),
    spt = matrix(c(alto/2, ancho/2), nrow=1, ncol=2)
    )

  landscape <- landscapeR::makePatch(
    empty_landscape,
    val = 1, rast = TRUE, bgr = 0,
    size = ,
    spt = matrix(c(alto/2, ancho/2), nrow=1, ncol=2)
  )





  seed_points_nf <-
    sample(which(raster::as.matrix(landscape) == 0),
           isolate(input$nf_n))

  sizes_nf <- runif(isolate(input$nf_n),
                    isolate(input$nf_size[1]),
                    isolate(input$nf_size[2]))

  landscape_full <-
    landscapeR::makeClass(
      landscape,
      val = 2, rast = TRUE,
      npatch = isolate(input$nf_n),
      pts = seed_points_nf,
      size = sizes_nf)

  list(landscape, landscape_full)
  })



# landscape_init <- reactive({
#   landscapeR::makeClass(isolate(pine()),
#                         val = 2,
#                         rast = TRUE,
#                         npatch = isolate(patchs_nf()),
#                         size = isolate(sizes_nf()),
#                         pts = isolate(seed_points_nf()))
# })





### EndPoints
  output$plotMaps <- renderUI({
    plotOutput("initial_map")
  })

#
#   output$initial_pine <- renderPlot({
#
#     if (input$plotTargetPine == 0)
#       return()
#
#     landscape_colors <- c("0" = "white",
#                           "1" = pp_denR()$col,
#                           "2" = "green")
#     #"3" = "lightgoldenrod1")
#
#     plot_landscape_pine(
#       isolate(pine())
#     ) +
#       scale_fill_manual(
#         values = landscape_colors,
#         labels = c("Other", "Pine plantation"),
#         name = "Present land uses"
#       )
#
#   })
#

  output$initial_landscape <- renderPlot({

    if (input$addNatForest == 0)
      return()

    landscape_colors <- c("0" = "white",
                          "1" = pp_denR()$col,
                          "2" = "green")

    plot_landscape_pine(
      isolate(creaLandscape()$landscape_full)
    ) +
      scale_fill_manual(
        values = landscape_colors,
        labels = c("Other", "Pine plantation"),
        name = "Present land uses"
      )

  })








}

shinyApp(ui, server)


##### Para lo de actualizar los shiny inputs
# https://mastering-shiny.org/action-dynamic.html#dynamic-visibility

#map(col_names, ~sliderInput(.x, NULL))
# https://stackoverflow.com/questions/35579439/dynamic-number-of-sliders-in-shiny
