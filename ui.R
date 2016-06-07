library(shiny)
library(markdown)
library(leaflet)

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
                   fileInput('infile', label = 'Open AusPlot .db file'),
                   uiOutput("download")
                 ) # absolutePanel
             ) # div.outer
    ), # tabPanel Map

    tabPanel("Data", dataTableOutput("table_sr")),
    tabPanel("Help", includeMarkdown("README.md"))
  )
)

