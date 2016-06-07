source("global.R")
library(shiny)
library(leaflet)

shinyServer(
  function(input, output){

    # Data --------------------------------------------------------------------#
    data <- reactive({
      if (is.null(input$infile)) return(NULL)
      get_data(input$infile$datapath)
    })

    output$table_sr <- renderDataTable({data()$species_records})
    output$table_vv <- renderDataTable({data()$vouchered_vegetation})
    output$table_tx <- renderDataTable({data()$transects})

    # Map object --------------------------------------------------------------#
    output$map <- renderLeaflet({
      if (is.null(data())) return(NULL)
      leaflet() %>%
        leaflet(data()$transects) %>%
        addTiles(urlTemplate = paste0("//{s}.tiles.mapbox.com/v3/jcheng.map",
                                      "-5ebohr46/{z}/{x}/{y}.png"),
                 attribution = paste0('Maps by <a href="http://www.mapbox.com/',
                                      '">Mapbox</a>')) %>%
        addScaleBar(position="bottomleft") %>%
        setView(lng = 120, lat = -25, zoom = 5) %>%
        addAwesomeMarkers(data()$transects$lon, data()$transects$lat,
                          label=data()$transects$name, clusterOptions=T,
                          popup=data()$transects$popup)
    })

    # Dataframe to CSV --------------------------------------------------------#
    output$download_sr <- downloadHandler(
      filename = function() {
        paste0(input$infile$name, '-species_records.csv')},
      content = function(file) {
        write.csv(data()$species_records, file, row.names = F)}
    )

    output$download_vv <- downloadHandler(
      filename = function() {
        paste0(input$infile$name, '-vouchered_vegetation.csv') },
      content = function(file) {
        write.csv(data()$vouchered_vegetation, file, row.names = F)}
    )

    output$download_tx <- downloadHandler(
      filename = function() {
        paste0(input$infile$name, '-transects.csv') },
      content = function(file) {
        write.csv(data()$transects, file, row.names = F)}
    )

    # Download panel ----------------------------------------------------------#
    output$download <- renderUI({
      if (is.null(data())) return(NULL)
      wellPanel(
        h3("Download CSV"),
        downloadButton('download_sr', 'Species records'),
        downloadButton('download_vv', 'Vouchered vegetation'),
        downloadButton('download_tx', 'Transects')
      )
    })

  }
)
