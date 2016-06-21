source("global.R")
library(shiny)
library(DT)
library(leaflet)

shinyServer(
  function(input, output){

    # Data --------------------------------------------------------------------#
    data <- reactive({
      if (is.null(input$infile)) return(NULL)
      get_data(input$infile)
    })

    data_sites <- reactive({
      if (is.null(input$infile_dgps)) return(NULL)
      get_sites(input$infile_dgps)
    })

    data_centroids <- reactive({
      if (is.null(input$infile_dgps)) return(NULL)
      get_site_centroids(input$infile_dgps)
    })

#     data_polys <- reactive({
#       if (is.null(input$infile_dgps)) return(NULL)
#       get_site_polys(input$infile_dgps)
#     })

    data_site_centroids <- reactive({
      do <- data()
      if (is.null(do)) return(NULL)
      dc <- data_centroids()
      if (is.null(dc)) return(NULL)
      right_join(do$sites, dc, by="plotName")
    })

    site_centroids <- reactive({
      d <- data_site_centroids()
      if (is.null(d)) return(NULL) else return(make_spdf(d))
    })

    output$siteSelector <- renderUI({
      if (is.null(data())) return(NULL)
      selectInput("sitepicker", "Show Site", c("All", data()$sites$plotName))
    })

    filteredData <- reactive({
      d <- data()
      if (is.null(d)) return(NULL)
      get_filtered_data(d, pn=input$sitepicker)
    })

    output$table_sp <- make_dt(data()$site_profiles)
    output$table_tp <- make_dt(data()$transect_profiles)
    output$table_sr <- make_dt(data()$species_records)
    output$table_bw <- make_dt(data()$basal_wedge)
    output$table_vv <- make_dt(data()$vouchered_vegetation)
    output$table_tx <- make_dt(data()$transects)
    output$table_si <- make_dt(data_site_centroids())
    output$table_sg <- make_dt(data_sites())

    # Map object --------------------------------------------------------------#

    # Base map, zoom to WA, minimap, scalebar
    # Provider tiles:
    #   http://leaflet-extras.github.io/leaflet-providers/preview/index.html
    output$map <- renderLeaflet({
      leaflet() %>%
        addProviderTiles("OpenStreetMap.Mapnik") %>%
        addProviderTiles("Esri.WorldImagery",
                         options=providerTileOptions(opacity=0.8)) %>%
        setView(lng = 120, lat = -25, zoom = 5) %>%
        addMiniMap(toggleDisplay=T, zoomLevelOffset=-8) %>%
        addScaleBar() %>%
        clearShapes()
    })

    # Overlay Ausplot sites once Ausplot .db are loaded
    observe({
      d <- data()
      if (is.null(d)) return(NULL)
      leafletProxy("map", data=d) %>%
        addAwesomeMarkers(d$sites$lon,
                          d$sites$lat,
                          clusterOptions=T,
                          group="Ausplot records",
                          label=d$sites$plotName,
                          popup=paste0("<h3>", d$sites$plotName, "</h3>",
                                       "<p>Ausplot site", d$sites$point, "</p>"))
    })

    # Overlay dGPS points onve dGPS .txt are loaded, with legend
    observe({
      d <- data_sites()
      if (is.null(d)) return(NULL)
      pal <-  colorFactor(palette="YlGnBu", domain=d, levels=unique(d$plotName))
      leafletProxy("map", data=d) %>%
        addAwesomeMarkers(d$lon_dd,
                          d$lat_dd,
                          clusterOptions=T,
                          group="dGPS points",
                          label=d$plotName,
                          popup=paste0("<h3>", d$plotName, "</h3>",
                                       "<p>dGPS point", d$point, "</p>")) %>%
        addLegend("topleft", pal=pal, values=~d$plotName,
                  title="dGPS points", opacity=0.8)
    })


    # React to "Show Site": zoom map to selected site
    observeEvent(input$sitepicker, {
      if (is.null(filteredData())) return(NULL)
      message(paste("Selecting site", input$sitepicker, "at",
        min(filteredData()$sites$lat), "/", min(filteredData()$sites$lon)))
      leafletProxy("map") %>%
        fitBounds(min(filteredData()$sites$lon),
                  min(filteredData()$sites$lat),
                  max(filteredData()$sites$lon),
                  max(filteredData()$sites$lat))
    })


    # Dataframe to CSV --------------------------------------------------------#
    output$download_sp <- downloadHandler(
      filename = function() {paste0('site_profiles.csv') },
      content = function(file) {
        write.csv(data()$site_profiles, file, row.names = F)}
    )

    output$download_sr <- downloadHandler(
      filename = function() {paste0('species_records.csv')},
      content = function(file) {
        write.csv(data()$species_records, file, row.names=F)}
    )

    output$download_bw <- downloadHandler(
      filename = function() {paste0(input$infile$name, '-basal_wedge.csv')},
      content = function(file) {
        write.csv(data()$basal_wedge, file, row.names=F)}
    )

    output$download_vv <- downloadHandler(
      filename = function() {paste0('vouchered_vegetation.csv')},
      content = function(file) {
        write.csv(data()$vouchered_vegetation, file, row.names=F)}
    )

    output$download_tx <- downloadHandler(
      filename = function() {paste0('transects.csv')},
      content = function(file) {
        write.csv(data()$transects_sites, file, row.names=F)}
    )

    output$download_sg <- downloadHandler(
      filename = function() {paste0('dgps_points.csv')},
      content = function(file) {
        write.csv(data_sites(), file, row.names=F)}
    )

#     output$download_sg_gj <- downloadHandler(
#       filename = function() {paste0('dgps_points.geojson')},
#       content = writeOGR(site_centroids(), file, 'sites', driver='GeoJSON')
#     )

    # Download panel ----------------------------------------------------------#
    output$download <- renderUI({
      if (is.null(data())) return(NULL)
      cl = "btn-xs btn-info"
      wellPanel(
        h3("Download CSV"),
        downloadButton('download_sp', 'Site Profiles', class=cl),
        downloadButton('download_sr', 'Species Records', class=cl),
        downloadButton('download_bw', 'Basal Wedge', class=cl),
        downloadButton('download_vv', 'Vouchered Vegetation', class=cl),
        downloadButton('download_tx', 'Transects and Sites', class=cl),
        downloadButton('download_sg', 'dGPS Sites CSV', class=cl)
        # downloadButton('download_sg_gj', 'dGPS Sites GeoJSON', class=cl)
      )
    })

    output$upload_ausplot <- renderUI({
      fileInput('infile', multiple=T,
                label="Upload one or several Ausplot field app .db files")
    })

    output$upload_dgps <- renderUI({
      fileInput('infile_dgps', multiple=T,
                label="Upload one or several dGPS .txt files")
    })

    output$tx_pca <- renderPlot({
      if (is.null(data())) return(NULL)
      make_pca_plot(filteredData()$site_profiles, input$sitepicker)
    })

    output$plot <- renderUI({
      if (is.null(filteredData())) return(NULL)
      absolutePanel(
        id = "ordinationplot", class = "panel panel-default",
        fixed = TRUE, draggable = TRUE, cursor="move",
        top = "auto", left = 20, right = "auto", bottom = 20,
        width = "350", height = "auto",
        plotOutput("tx_pca")
      )
    })

  }
)
