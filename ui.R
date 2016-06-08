library(shiny)
library(leaflet)
library(markdown)

shinyUI(
  navbarPage(
    "AusPlot data explorer",
    id="nav",

    tabPanel("Map",
             div(class="outer",
                 tags$head(includeCSS("style.css")),
                 leafletOutput("map", width="100%", height="100%"),
                 absolutePanel(
                   id = "controls", class = "panel panel-default",
                   fixed = TRUE, draggable = TRUE,
                   top = 80, left = "auto", right = 20, bottom = "auto",
                   width = 280, height = "auto",

                   h3("Upload .db"),
                   uiOutput("upload"),
                   uiOutput("download")
                 ) # absolutePanel
             ) # div.outer
    ), # tabPanel Map

    tabPanel("Site Profiles", DT::dataTableOutput("table_sp")),
    tabPanel("Transect Profiles", DT::dataTableOutput("table_tp")),
    tabPanel("Species Records", DT::dataTableOutput("table_sr")),
    tabPanel("Vouchered Vegetation", DT::dataTableOutput("table_vv")),
    tabPanel("Transects", DT::dataTableOutput("table_tx")),
    tabPanel("Sites", DT::dataTableOutput("table_si")),
    tabPanel("Help", includeMarkdown("README.md"))
  )
)

