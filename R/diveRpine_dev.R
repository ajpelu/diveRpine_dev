library("shiny")
library("shinydashboard")
library("shinyWidgets")

library("reactlog")
reactlog_enable()


############################ FUNCTIONS
source("init_params.R")

source("aux.R")
source("diveR_landscape.R")
source("plot_landscape.R")


# UI ----------------------------------------------------------------------
ui <- dashboardPage(
  ## Header ---------------------------------------------------------------
  dashboardHeader(
    title = span(img(src = "diveRpine_v1.svg", heigth = 35), "diveRpine_dev"),
    tags$li(
      a(
        strong("About diveRpine"),
        height = 35,
        href = "https://github.com/ajpelu/diveRpine",
        title = "",
        targer = "_blank"
      ),
      class = "dropdown"
    )
  ),

  ## Header ---------------------------------------------------------------
  dashboardSidebar(disable = TRUE),

  ## Body -----------------------------------------------------------------
  dashboardBody(
    fluidRow(
      column(width = 5,
             fluidRow(
               box(
                 tags$p(h4(strong("Pine plantation"))),
                 sliderInput( # Size of pine plantation
                   inputId = "pp_size", label = "Patch Area",
                   min = 200, max = 1500, value = 750),
                 awesomeRadio(
                   inputId = "pp_den", label = "Tree density",
                   choices = c("low", "medium", "high"),
                   selected = "medium", status = "success", inline = TRUE
                 ),
                 awesomeRadio(
                   inputId = "pp_use", label = "Past Land Use of the Pine plantation",
                   choices = c("Natural Forests", "Shrublands", "Pasture", "Croplands"),
                   selected = "Shrublands", status = "success"
                 ),
                 # Natural forest configuration
                 tags$p(h4(strong("Natural Forests"))),
                 sliderInput(
                   inputId = "nf_n", label = "Natural forests patch number",
                   min = 1, max = 5, value = 2),
                 sliderInput(
                   inputId = "nf_size", label = "Natural forests patch size (min and max)",
                   min = 50, max = 500, value = c(100,200)),
                 tags$br(),
                 tags$br(),

                 actionBttn(
                   inputId = "doRiquezaInit",
                   label = "Compute Initial Richness",
                   color = "success",
                   style = "material-flat",
                   size = "xs"
                 )
                 )),

             box(
               tags$p(h4(strong("Dispersers")))
             )
               ),
      column(width = 7,
             box(width = NULL,
                 plotOutput('initial_landscape'))
             )
    )
    )
)





server <- function(input, output, session) {

  pp_denR <- reactive({
    list(
      den = switch(input$pp_den,
                   "low" = "100",
                   "medium" = "1250",
                   "high" = "3000"),
      # "low" = 100, "medium" = 1250, "high" = 3000),
      col = switch(input$pp_den,
                   "low" = "#a1d99b",
                   "medium" = "#238b45",
                   "high" = "#00441b")
    )
  })

  # Past Use
  pastUse <- reactive({
    switch(input$pp_use, 'Natural Forests' = 'Oak', 'Shrublands' = 'Shrubland',
           'Pasture' = 'Pasture','Croplands' = 'Crop')
  })

  ### Pine target submodule -----------------------
  pine <- reactive({
    landscapeR::makePatch(empty_landscape,
                          val = 1, rast = TRUE, bgr = 0,
                          size = input$pp_size,
                          spt = position_pine)
  })


  ### Natural forests submodule -----------------------
  #### Get the positions for the creation of the NF patches.
  nf_n <- reactive(input$nf_n)

  #### Get the positions for the creation of the NF patches.
  positions_nf <- reactive({
    sample(
      which(
        t(raster::as.matrix(pine())) == 0),
      nf_n()
      )
  })

  #### Generate the sizes of the natural forests patch
  nf_sizes <- reactive({
    round(runif(nf_n(),
            input$nf_size[1],
            input$nf_size[2]),
      digits = 0)
  })

  #### Generate pine + oak landscape
  pine_oak <- reactive({
    makeClass(pine(),
              val = 2, rast = TRUE,
              npatch = nf_n(),
              pts = positions_nf(),
              size = nf_sizes()
    )
  })

  crops_size <- reactive({
    sample(10:ceiling(
      length(which(t(raster::as.matrix(pine_oak())) == 0))*0.05),
                        size = n_crops)
    })

  landscape <- reactive({
    makeClass(pine_oak(),
                   val = 3, rast = TRUE,
                   npatch = n_crops,
                   size = crops_size())
    })

  ### ----------------------------------------------
  ## Distance raster
  dist_raster <- reactive({
    dist2nf(landscape(), nf_value = nf_value) # nf defined at init_params
  })

  ### ----------------------------------------------
  ## Compute initial Richnness
  rasterRich <- reactive({
    initRichness(r = landscape(), draster = dist_raster(),
                 r_range = ri_range, treedensity = den_pp()$den,
                 pastUse = pastUse(), rescale = FALSE)
  })

  ## Get bouondary of pp
  limit_pp <- reactive({
    rasterToPolygons(landscape, fun=function(x){x==pp_value}, dissolve = TRUE)
  })

  ### ----------------------------------------------
  output$initial_landscape <- renderPlot({

    plot_landscape(landscape()) +
      scale_fill_manual(
        values =
          c("0" = "#FFFFe5",
            "1" = pp_denR()$col,
            "2" = "green",
            "3" = "lightgoldenrod1"),
        labels = c("Other", "Pine plantation", "Natural Forests", "Shrublands"),
        name = "Present land uses"
      )
    })
}


shinyApp(ui, server)
