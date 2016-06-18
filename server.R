source("global.R")
library(shiny)
library(DT)
library(leaflet)
library(rgdal)
library(vegan)

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
      left_join(do$sites, dc, by="plotName")
    })

    site_centroids <- reactive({
      d <- data_site_centroids()
      if (is.null(d)) return(NULL)
      s <- as.data.frame(d[which(!is.null(d$latitude)),])
      sdf <- select(s, latitude, longitude)
      wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
      pts <- SpatialPointsDataFrame(SpatialPoints(sdf, proj4string=wgs84), s)
    })

    output$siteSelector <- renderUI({
      if (is.null(data())) return(NULL)
      selectInput("sitepicker", "Show Site", c("All", data_sites()$plotName))
    })

    filteredData <- reactive({
      if (is.null(data())) return(NULL)
      get_filtered_data(data(), pn=input$sitepicker)
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
    output$map <- renderLeaflet({
      d <- data()
      leaflet() %>%
        # Provider tiles: pick any from
        # http://leaflet-extras.github.io/leaflet-providers/preview/index.html
        addProviderTiles("OpenStreetMap.Mapnik") %>%
        addProviderTiles("Esri.WorldImagery",
                         options=providerTileOptions(opacity=0.8)) %>%
        setView(lng = 120, lat = -25, zoom = 5) %>%
        addMiniMap(toggleDisplay=T, zoomLevelOffset=-8) %>%
        clearShapes()
    })

    observe({
      d <- data()
      if (is.null(d)) return(NULL)
      leafletProxy("map", data=d) %>%
        addAwesomeMarkers(d$sites$lon,
                          d$sites$lat,
                          label=d$sites$plotName,
                          popup=paste0("<h3>", d$sites$plotName, "</h3>"),
                          clusterOptions=T)
    })

    observe({
      d <- data_sites()
      if (is.null(d)) return(NULL)
      pal <-  colorFactor(palette = "YlGnBu", domain = d, levels=unique(d$plotName))
      leafletProxy("map", data=data_sites()) %>%
        addAwesomeMarkers(d$lon_dd,
                          d$lat_dd,
                          label=d$plotName,
                          popup=paste0("<h3>", d$plotName, "</h3>",
                                       "<p> dGPS point ", d$point, "</p>"),
                          clusterOptions=T) %>%
        addLegend("topleft", pal = pal, values = ~d$plotName,
                  title = "dGPS points",
                  opacity = 1
        )
    })


    # react to "Show Site"
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
      filename = function() {paste0(input$infile$name, '-site_profiles.csv') },
      content = function(file) {write.csv(data()$site_profiles, file, row.names = F)}
    )

    output$download_tp <- downloadHandler(
      filename = function() {paste0(input$infile$name, '-transect_profiles.csv') },
      content = function(file) {write.csv(data()$transect_profiles, file, row.names = F)}
    )

    output$download_sr <- downloadHandler(
      filename = function() {paste0(input$infile$name, '-species_records.csv')},
      content = function(file) {write.csv(data()$species_records, file, row.names=F)}
    )

    output$download_bw <- downloadHandler(
      filename = function() {paste0(input$infile$name, '-basal_wedge.csv')},
      content = function(file) {write.csv(data()$basal_wedge, file, row.names=F)}
    )

    output$download_vv <- downloadHandler(
      filename = function() {paste0(input$infile$name, '-vouchered_vegetation.csv')},
      content = function(file) {write.csv(data()$vouchered_vegetation, file, row.names=F)}
    )

    output$download_tx <- downloadHandler(
      filename = function() {paste0(input$infile$name, '-transects.csv')},
      content = function(file) {write.csv(data()$transects_sites, file, row.names=F)}
    )

    output$download_sg <- downloadHandler(
      filename = function() {paste0('dgps_points.csv')},
      content = function(file) {write.csv(data_sites(), file, row.names=F)}
    )

    output$download_sg_gj <- downloadHandler(
      filename = function() {paste0('dgps_points.geojson')},
      content = function(file) {
        writeOGR(site_centroids(), file, 'sites', driver='GeoJSON')}
    )

    # Download panel ----------------------------------------------------------#
    output$download <- renderUI({
      if (is.null(data())) return(NULL)
      cl = "btn-xs btn-info"
      wellPanel(
        h3("Download CSV"),
        downloadButton('download_sp', 'Site Profiles', class=cl),
        downloadButton('download_tp', 'Transect Profiles', class=cl),
        downloadButton('download_sr', 'Species Records', class=cl),
        downloadButton('download_bw', 'Basal Wedge', class=cl),
        downloadButton('download_vv', 'Vouchered Vegetation', class=cl),
        downloadButton('download_tx', 'Transects and Sites', class=cl),
        downloadButton('download_sg', 'dGPS Sites CSV', class=cl),
        downloadButton('download_sg_gj', 'dGPS Sites GeoJSON', class=cl)
      )
    })

    output$upload_ausplot <- renderUI({
      fileInput('infile', multiple=T,
                label="Upload one or several Ausplot field app .db files")
    })

    output$upload_dgps <- renderUI({
      fileInput('infile_dgps', multiple=T,
                label="Upload one or several dGPS txt files")
    })

    output$tx_pca <- renderPlot({
      if (is.null(data())) return(NULL)
      d <- filteredData()
      Y <- d$transect_profiles %>%
        dplyr::select(-starts_with("transect"),
                      -starts_with("plot"),
                      -completionDateTime, -lat, -lon, -txUid)

      Y[is.na(Y)] <- 0

      tryCatch(
        # this could get messy with dirty data
        plt <- vegan::decostand(Y, "hellinger", na.rm=T) %>%
          rda() %>%
          plot(type = "t",
               main="PCA of Hellinger-tf Tx profiles",
               sub=paste("Selected sites:", input$sitepicker)),
        finally = return(NULL)
      )
      plt
    })

    output$plot <- renderUI({
      if (is.null(data())) return(NULL)
      absolutePanel(
        id = "controls", class = "panel panel-default",
        fixed = TRUE, draggable = TRUE,
        top = "auto", left = 20, right = "auto", bottom = 20,
        width = "350", height = "auto",
        plotOutput("tx_pca")
      ) # absolutePanel
    })

  }
)
