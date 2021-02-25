library("shiny")
library("shinydashboard")
library("shinyWidgets")
library("raster")
library("landscapeR")
library("tidyverse")
library("DT")
library("rasterVis")
library('RColorBrewer')

library("reactlog")
reactlog_enable()



############################ FUNCTIONS
source("init_params.R")
source("aux.R")
source("diveR_landscape.R")
source("plot_landscape.R")
source("plot_richness.R")
source("dist2nf.R")
source("initRichness.R")
source("disper.R")

# UI ----------------------------------------------------------------------
ui <- dashboardPage(
  skin = "green",
  ## Header ---------------------------------------------------------------
  dashboardHeader(
    title = span(img(src = "diveRpine_v1.svg", height = '50'), "diveRpine_dev"),
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
  ## Sidebar ---------------------------------------------------------------
  dashboardSidebar(disable = TRUE),

  ## Body -----------------------------------------------------------------
  dashboardBody(
    fluidRow(
      column(width = 5,
        fluidRow(
          setSliderColor(c(rep("#5DB85C", 7)), c(1:7)), # change color sliders
          box(
            tags$p(h4(strong("Pine plantation"))),
            sliderInput( # Size of pine plantation
              inputId = "pp_size", label = "Patch Area",
              min = 200, max = 1500, value = 750
            ),
            awesomeRadio(
              inputId = "pp_den", label = "Tree density",
              choices = c("low", "medium", "high"),
              selected = "medium", status = "success", inline = TRUE
            ),
            awesomeRadio(
              inputId = "pp_use", label = "Past Land-use of the pine plantation",
              choices = c("Natural Forests", "Shrublands", "Pasture", "Croplands"),
              selected = "Shrublands", status = "success"
            ),
            tags$p(h4(strong("Natural Forests"))), # Natural forest configuration
            sliderInput(
              inputId = "nf_n", label = "Natural forests patch number",
              min = 1, max = 5, value = 2
            ),
            sliderInput(
              inputId = "nf_size",
              label = HTML(paste0("Natural forests patch size", br(), "(min and max)")),
              min = 50, max = 500, value = c(100, 200)
            ),
            tags$br(),
            actionBttn(
              inputId = "doPaisaje",
              label = "Create Landscape",
              color = "success",
              style = "material-flat",
              size = "xs"
            ),
            tags$br(),
            tags$br(),
            actionBttn(
              inputId = "doRiquezaInit",
              label = "Compute Initial Richness",
              color = "success",
              style = "material-flat",
              size = "xs"
            )
          ),
          box(
            tags$p(h4(strong("Dispersers"))),
            sliderInput(inputId = "sb",label = "Small-size Birds",
                        min = 0, max = 100, value = 0, step = 1),
            uiOutput("mb"),
            dataTableOutput("disptable"),
            tags$br(),
            tags$br(),
            tags$p(h4(strong("Simulation"))),
            sliderInput(inputId = "timeRange", label = "Simulation years:", min=10, max=50, value=30),

            actionBttn(inputId = "doPropagulo", label = "Input seed propagules",
                       color = "success", style = "material-flat", size = "xs")
          )
        ),
        fluidRow(
          box(
            width = 12,
            tags$p(h4(strong("Plant richness values by forest type"))),
            infoBoxOutput("rich_ppInitBox"),
            infoBoxOutput("rich_nfBox")
            # infoBoxOutput("rich_ppEndBox")
          )
        )
      ),
      column(
          width = 7,
          box(
            width = NULL,
            uiOutput('plotMaps')
            # plotOutput("initial_landscape", height = h_plots)
          )
        )
      )
  )
)


server <- function(input, output, session) {

  pp_denR <- reactive({
    list(
      den = switch(input$pp_den,
                   "low" = 100,
                   "medium" = 1250,
                   "high" = 3000),
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
  ## Compute initial Richness
  rasterRich <- reactive({
    initRichness(r = landscape(), draster = dist_raster(),
                 r_range = ri_range, treedensity = pp_denR()$den,
                 pastUse = pastUse(), rescale = FALSE)
  })


  ## Richness nf
  rich_nf <- reactive({
    summaryRaster(
      calc(stack(landscape(), rasterRich()),
           fun=function(x) ifelse(x[1] == nf_value, (x[1]/nf_value)*x[2], NA))
    )
    })


  ## Compute Richness pine plantations
  rich_pp <- reactive({
    summaryRaster(
      calc(stack(landscape(), rasterRich()),
           fun=function(x) ifelse(x[1] == pp_value, x[1]*x[2], NA))
      )
  })

  ## Get bouondary of pp
  limit_pp <- reactive({
    limit_pp <- rasterToPolygons(landscape(), fun=function(x){x==pp_value}, dissolve = TRUE)
    fortify(limit_pp, region = "layer") %>%
      rename(x=long, y=lat)
  })


  ## Disperser mammals
  perma <- reactive({
    100-(input$sb + input$mb)
  })

  ## Compute dispersion rasters
  rasterDisp <- reactive({
    disper(x = landscape(), xr = rasterRich(), nf_value = nf_value, pp_value = pp_value)
  })

  ### contribution of each disperser category
  propagule_sb <- reactive({
    rasterDisp()[['msb']] * as.numeric(input$sb)
  })

  propagule_mb <- reactive({
    rasterDisp()[['mmb']] * as.numeric(input$mb)
  })

  propagule_ma <- reactive({
    rasterDisp()[['mma']] * as.numeric(perma())
  })

  propagule_bird_aux <- reactive({
    raster::calc(stack(propagule_sb(), propagule_mb()),sum)
  })

  propagule_bird <- reactive({
    propagule_bird_aux() * piBird
  })

  propagule_mammal <- reactive({
    propagule_ma() * piMammal
  })

  propagule <- reactive({
    raster::calc(stack(propagule_bird(), propagule_mammal()), sum)
  })

  ### ----------------------------------------------
  observeEvent(input$doPaisaje, {
    output$plotMaps <- renderUI({
        plotOutput("initial_landscape", height = h_plots)})

    output$initial_landscape <- renderPlot({

      #limites_pp <- fortify(limit_pp(), region = "layer") %>% rename(x=long, y=lat)

      plot_landscape(landscape()) +
        scale_fill_manual(
          values =
            c("0" = "#FFFFe5",
              "1" = pp_denR()$col,
              "2" = "green",
              "3" = "lightgoldenrod1"),
          labels = c("Other", "Pine plantation", "Natural Forests", "Croplands"),
          name = "Present land uses"
        ) +
        geom_polygon(data=limit_pp(),
                     aes(x, y, group=group), fill=NA, colour="black", lwd=.8) +
        ggtitle("Initial Landscape configuration") +
        theme(plot.title = element_text(size = 24, face = "bold", hjust= 0.5),
              legend.text = element_text(size = 16),
              legend.title = element_text(size = 16))
      })
  })

  ### ----------------------------------------------

  observeEvent(input$doRiquezaInit, {
    output$plotMaps <- renderUI({
      plotOutput("richness_map", height = h_plots)})

    output$richness_map <- renderPlot({
      plot_richness(rasterRich()) +
        geom_polygon(data=limit_pp(),
                     aes(x, y, group=group), fill=NA, colour="black", lwd=.8) +
        ggtitle("Initial Richness") +
        theme(plot.title = element_text(size = 24, face = "bold", hjust= 0.5),
              legend.text = element_text(size = 16),
              legend.title = element_text(size = 16)) +
        labs(fill = " Nº plant species")
    })
  })

  ### ----------------------------------------------
  observeEvent(input$doPropagulo, {
    output$plotMaps <- renderUI({
        plotOutput("richness_disper", height = h_plots)
        })

    output$richness_disper <- renderPlot({
      levelplot(propagule(),
                margin=FALSE,  par.settings = propagule_theme,
                scales=list(draw=FALSE), colorkey = list(space = "bottom"),
                main = list(expression("Input propagule (n seed" ~ m^-2 ~ year^-1*")"), cex=2.2)
                )
    })
  })


  output$rich_ppInitBox <- renderValueBox({
    valueBox(value = rich_pp()$mean,
             subtitle =
               HTML(paste0(
                 paste0(rich_pp()$min, " - ", rich_pp()$max),
                 br(), tags$strong("Initial Pine plantation"))),
             icon = icon('tree-conifer', lib='glyphicon'), color = 'green')
  })

  output$rich_nfBox <- renderValueBox({
    valueBox(value = rich_nf()$mean,
             subtitle =
               HTML(paste0(
                 paste0(rich_nf()$min, " - ", rich_nf()$max),
                 br(), tags$strong("Natural Forest"))),
             icon = icon('tree-deciduous', lib='glyphicon'), color = 'yellow')
  })


  output$mb <- renderUI({
    tagList(
      sliderInput(inputId = "mb",
                  label = "Medium-size birds",
                  min = 0, max = 100 - input$sb, value = 0)
    )
  })

  output$disptable <- DT::renderDataTable({
    name_disperser <- c("Small birds", "Medium birds", "Mammals")
    dispersers <- c(
      as.character(tags$img(src="smallbird.svg", height = '40', width = '50')),
      as.character(tags$img(src="garrulus.svg", height = '40', width = '50')),
      as.character(tags$img(src="vulpes.svg", height = '40', width = '50'))
    )
    percentage <- c(input$sb, input$mb, perma())

    datatable(cbind(Dispersers = name_disperser,
                    icon = dispersers,
                    Percentage = percentage),
              colnames = c("Dispersers", "", "%"),
              escape = FALSE,
              options = list(dom = 't'))
  })
}


shinyApp(ui, server)
