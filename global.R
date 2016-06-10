## Libraries
# if (!require("pacman")) install.packages("pacman")
# pacman::p_load("shiny", "readr", "dplyr", "tidyr", "stringr", "lubridate",
               # "RSQLite", "DT", "markdown", update=T)
# devtools::install_github('rstudio/leaflet')
library(RSQLite)
library(tidyr)
library(dplyr)
library(lubridate)
library(shiny)
library(DT)

#' Extract data from a raw Ausplot SQLite .db file
#'
#' Species Records are joined with transect point, transect, and plot details.
#' Vouchered Vegetation records are joined with plot details.
#' Transect records are joined with plot details, plus a column "popup" is added
#' containing HTML for a map popup.
#'
#' Returns a list of data frames:
#'
#' species_records: Observations and measurements of individuals within a
#'  transect, plus date, location, tx point details, tx and site names.
#' transects: all Transect details plus selected plot details
#' sites: all Plot details
#' transects_sites: all tx and site details plus HTML for map popup
#' transect_profiles: transect by species pivot table
#' site_profiles: site by species pivot table
#' vouchered_vegetation: vv per site
#'
#'
#' @param f A file path, such as RShiny's input$infile$datapath
get_data <- function(f){
    require(RSQLite)
    con <- dbConnect(RSQLite::SQLite(), dbname=f)

    pl <- dbGetQuery(con, 'SELECT pl.id as plotId, pl.name as plotName,
      pl.lat as lat, pl.lon as lon,
      pl.current, pl.finished, pl.completionDate,
      pl.completionTime, pl.uploaded, pl.vegObserver,
      pl.vegAffiliation, pl.plotComment, pl.permanent, pl.aligned,
      pl.landformPattern, pl.landformElement, pl.siteSlope, pl.siteAspect,
      pl.outcropLithology, pl.outcropSubLithology, pl.surfaceStrewSize,
      pl.surfaceStrewLithology, pl.plotDimension100x100, pl.plotDimensionX,
      pl.plotDimensionY, pl.climaticConditions, pl.vegetationConditions,
      pl.physicalStatusComments,
      ss.upper1, ss.upper2, ss.upper3, ss.middle1, ss.middle2, ss.middle3,
      ss.lower1, ss.lower2, ss.lower3, ss.massFloweringEventEvidence
      FROM plots AS pl
      LEFT JOIN sitesummary AS ss ON pl.id = ss.plotId') %>%
      mutate(plotCompletionDateTime=paste(completionDate, completionTime)) %>%
      mutate(plotCompletionDateTime=parse_date_time(
        plotCompletionDateTime, orders=c("YmdHMS"), tz="Australia/Perth")) %>%
      tbl_df()

    pl_simple <- pl %>%
      select(plotId, plotName, lat, lon, plotCompletionDateTime)

    tx <- dbGetQuery(con, 'SELECT tx.id as transectId,
      tx.startPoint as transectStartPoint, tx.endPoint as transectEndPoint,
      tx.completionDateTime,
      pl.id as plotId, pl.name as plotName, pl.lat as lat, pl.lon as lon
      FROM transects AS tx LEFT JOIN plots AS pl ON tx.plotId = pl.id') %>%
      mutate(transectCompletionDateTime=parse_date_time(
        completionDateTime, orders="mdYHMS", tz="Australia/Perth")) %>%
      tbl_df()

    tx_simple <- tx %>%
      select(transectId, transectStartPoint, transectEndPoint,
             transectCompletionDateTime, plotId)

    sr <- dbGetQuery(con, 'SELECT sr.id, sr.fieldName, sr.inCanopySky,
      sr.senescent, sr.growthForm, sr.height, sr.transectPointId,
      tp.number as transectPointNumber, tp.substrateType, tp.transectId
      FROM speciesRecord AS sr
      LEFT JOIN transectPoints AS tp ON sr.transectPointId = tp.id') %>%
      left_join(tx_simple, by="transectId") %>%
      left_join(pl_simple, by="plotId") %>% tbl_df()

    # vouchered vegetation with basic site details
    vv <- dbGetQuery(con, 'SELECT * FROM voucheredVeg') %>%
      left_join(pl_simple, by="plotId") %>% tbl_df()

    # basal wedge with basic site details
    bw = dbGetQuery(con, 'SELECT * FROM bwRecords') %>%
      left_join(pl_simple, by="plotId") %>% tbl_df()

    # transects with full site details
    ts <- tx_simple %>% left_join(pl, by="plotId") %>% tbl_df()

    # add HTML popup content to transects
    ts$popup = paste(
        '<h3>', ts$plotName, '-', ts$transectId,  '</h3>',
        '<p><strong>Observed by</strong>',
        ts$vegObserver, '(', ts$vegAffiliation, ') on',
        ts$transectCompletionDateTime, '</p>',
        '<p><strong>Landform</strong>',
        ts$landformPattern, '-', ts$landformElement, '</p>')

    # transect profile = species records counts by transect
    tp <- tbl_df(sr) %>%
      group_by(plotName, transectId, fieldName) %>%
      tally(sort=T) %>%
      spread(fieldName, n) %>%
      ungroup() %>%
      select(-starts_with("plotName")) %>%
      left_join(tx, by="transectId")
    row.names(tp) <- paste(tp$plotId, "-", tp$transectId)
    tp[is.na(tp)] <- 0

    # site profile = species records counts by site
    sp <- tbl_df(sr) %>%
      group_by(plotName, fieldName) %>%
      tally(sort=T) %>%
      spread(fieldName, n) %>%
      ungroup() %>%
      left_join(pl_simple, by="plotName")
    sp[is.na(sp)] <- 0

    data <- list(species_records=sr,
                 basal_wedge=bw,
                 vouchered_vegetation=vv,
                 transects=tx,
                 transect_profiles=tp,
                 transects_sites=ts,
                 sites=pl,
                 site_profiles=sp)
    data
}

#' Filter a dataframe d returning rows where column col matches value val
filterDf <- function(d, val){
  filtered <- d[which(d$plotName %in% val),]
}

#' Filter a list of dataframes ld to one plotName pn
get_filtered_data <- function(ld, pn="All"){
  if (pn=="All") return(ld)
  filtered <- lapply(ld, filterDf, pn)
  filtered
}

#' Prepare a DT datatable with sensible defaults
make_dt <- function(x, filter="top", pageLength=10){
  out <- DT::renderDataTable(
    DT::datatable(
      x,
      filter=filter,
      options=list(
        pageLength = pageLength,
        autoWidth = TRUE,
        columnDefs = list(list(width='500px', targets=c("plotComment")))
      )))
  out
}
