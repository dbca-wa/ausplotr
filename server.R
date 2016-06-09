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

    output$siteSelector <- renderUI({
      if (is.null(data())) return(NULL)
      selectInput("sitepicker", "Show Site", c("All", data()$sites$plotName))
    })

    filteredData <- reactive({
      if (is.null(data())) return(NULL)
      d <- get_filtered_data(data(), pn=input$sitepicker)
      d
    })

    output$table_sp <- make_dt(data()$site_profiles)
    output$table_tp <- make_dt(data()$transect_profiles)
    output$table_sr <- make_dt(data()$species_records)
    output$table_bw <- make_dt(data()$basal_wedge)
    output$table_vv <- make_dt(data()$vouchered_vegetation)
    output$table_tx <- make_dt(data()$transects)
    output$table_si <- make_dt(data()$sites)

    # Map object --------------------------------------------------------------#
    output$map <- renderLeaflet({
      leaflet(data()$transects_sites) %>%
        # Provider tiles: pick any from
        # http://leaflet-extras.github.io/leaflet-providers/preview/index.html
        addProviderTiles("OpenStreetMap.Mapnik") %>%
        addProviderTiles("Esri.WorldImagery",
                         options=providerTileOptions(opacity=0.8)) %>%
        setView(lng = 120, lat = -25, zoom = 5) %>%
        addMiniMap(toggleDisplay=T) %>%
        clearShapes() %>%
        addMarkers(d$transects_sites$lon,
                   d$transects_sites$lat,
                   label=d$transects_sites$name,
                   popup=d$transects_sites$popup,
                   clusterOptions=T)
    })

    # react to "Show Site"
    observeEvent(input$sitepicker, {
      if (is.null(filteredData())) return(NULL)
      print(input$sitepicker)
      print(min(filteredData()$sites$lon))
      leafletProxy("map") %>%
        fitBounds(min(filteredData()$sites$lon),
                  min(filteredData()$sites$lat),
                  max(filteredData()$sites$lon),
                  max(filteredData()$sites$lat))
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

    output$download_bw <- downloadHandler(
      filename = function() {
        paste0(input$infile$name, '-basal_wedge.csv')},
      content = function(file) {
        write.csv(data()$basal_wedge, file, row.names = F)}
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
        write.csv(data()$transects_sites, file, row.names = F)}
    )

    # Download panel ----------------------------------------------------------#
    output$download <- renderUI({
      if (is.null(data())) return(NULL)
      wellPanel(
        h3("Download CSV"),
        downloadButton('download_sp', 'Site Profiles'),
        downloadButton('download_tp', 'Transect Profiles'),
        downloadButton('download_sr', 'Species Records'),
        downloadButton('download_bw', 'Basal Wedge'),
        downloadButton('download_vv', 'Vouchered Vegetation'),
        downloadButton('download_tx', 'Transects and Sites')
      )
    })

    output$upload <- renderUI({
      fileInput('infile', multiple=F, label=NULL)
    })
  }
)
