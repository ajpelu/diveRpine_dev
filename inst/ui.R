library("DT")
library("landscapeR")
library("raster")
library("rasterVis")
library("RColorBrewer")
library('rgeos')
library('rintrojs')
library('knitr')
library('markdown')
library('sp')
library("sf")
library('shiny')
library('shinythemes')
library('shinycssloaders')
library('shinydashboard')
library('shinyjs')
library('shinyWidgets')
library("tidyverse")

# Set heigth plots
h_plots <- 700

## Header ---------------------------------------------------------------
header <- dashboardHeader(
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
)

## Sidebar ---------------------------------------------------------------
sidebar <- dashboardSidebar(disable = TRUE)

## Body ---------------------------------------------------------------
body <- dashboardBody(
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
                 inputId = "createLandscape",
                 label = "Create Landscape",
                 color = "success",
                 style = "material-flat",
                 size = "xs"
               ),
               tags$br(),
               tags$br(),
               actionBttn(
                 inputId = "computeInitialRichness",
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

               actionBttn(inputId = "createPropagule", label = "Input seed propagules",
                          color = "success", style = "material-flat", size = "xs"),
               tags$br(),
               tags$br(),
               actionBttn(inputId = "computeEndRichness", label = "Compute Final Richness",
                          color = "success", style = "material-flat", size = "xs")
             )
           ),
           fluidRow(
             box(
               width = 12,
               tags$p(h4(strong("Plant richness values by forest type"))),
               infoBoxOutput("rich_ppInitBox"),
               infoBoxOutput("rich_nfBox"),
               infoBoxOutput("rich_ppEndBox")
             )
           )
    ),
    column(width = 7,
           fluidRow(
             box(width = NULL,
                 uiOutput('plotMaps')
             ))
    )
  )
)

dashboardPage(header, sidebar, body,
              title = "diveRpine_dev",
              skin = "green")
