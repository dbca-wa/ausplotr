source("global.R")
library(DT)
library(shiny)
library(leaflet)

shinyServer(
  function(input, output){

    # Data --------------------------------------------------------------------#
    data <- reactive({
      if (is.null(input$infile)) return(NULL)
      get_data(input$infile$datapath)
    })

    output$table_sp <- DT::renderDataTable(data()$site_profiles, filter="top")
    output$table_tp <- DT::renderDataTable(data()$transect_profiles, filter="top")
    output$table_sr <- DT::renderDataTable(data()$species_records, filter="top")
    output$table_vv <- DT::renderDataTable(data()$vouchered_vegetation, filter="top")
    output$table_tx <- DT::renderDataTable(data()$transects, filter="top")
    output$table_si <- DT::renderDataTable(data()$sites, filter="top")

    # Map object --------------------------------------------------------------#
    output$map <- renderLeaflet({
      if (is.null(data())) return(NULL)
      leaflet(data()$transects_sites) %>%
        addTiles(
          urlTemplate="//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
          attribution='Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>%
        addScaleBar(position="bottomleft") %>%
        setView(lng = 120, lat = -25, zoom = 5) %>%
        addMiniMap(toggleDisplay=T) %>%
        addMarkers(data()$transects_sites$lon,
                   data()$transects_sites$lat,
                   label=data()$transects_sites$name,
                   popup=data()$transects_sites$popup,
                   clusterOptions=T)
    })

    # Dataframe to CSV --------------------------------------------------------#
    output$download_sp <- downloadHandler(
      filename = function() {
        paste0(input$infile$name, '-site_profiles.csv') },
      content = function(file) {
        write.csv(data()$site_profiles, file, row.names = F)}
    )

    output$download_tp <- downloadHandler(
      filename = function() {
        paste0(input$infile$name, '-transect_profiles.csv') },
      content = function(file) {
        write.csv(data()$transect_profiles, file, row.names = F)}
    )

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
        write.csv(data()$vouchered_vegetation_sites, file, row.names = F)}
    )

    output$download_tx <- downloadHandler(
      filename = function() {
        paste0(input$infile$name, '-transects.csv') },
      content = function(file) {
        write.csv(data()$transects_sites, file, row.names = F)}
    )

    # Download panel ----------------------------------------------------------#
    output$download <- renderUI({
      if (is.null(data())) return(NULL)
      wellPanel(
        h3("Download CSV"),
        downloadButton('download_sp', 'Site profiles'),
        downloadButton('download_tp', 'Transect profiles'),
        downloadButton('download_sr', 'Species records'),
        downloadButton('download_vv', 'Vouchered vegetation'),
        downloadButton('download_tx', 'Transects and Sites')
      )
    })

    output$upload <- renderUI({
      fileInput('infile', multiple=F, label = 'Open AusPlot .db file')
    })
  }
)
