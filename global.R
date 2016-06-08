## Libraries
# if (!require("pacman")) install.packages("pacman")
# pacman::p_load("shiny", "readr", "dplyr", "tidyr", "stringr", "lubridate",
               # "RSQLite", "DT", "markdown", update=T)
# devtools::install_github('rstudio/leaflet')
library(RSQLite)
library(tidyr)
library(dplyr)
library(lubridate)

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
      pl.physicalStatusComments
      FROM plots AS pl') %>%
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

    # vouchered vegetation
    vv <- dbGetQuery(con, 'SELECT * FROM voucheredVeg') %>% tbl_df()

    # vv with site details
    vs <- vv %>% left_join(pl, by="plotId") %>% tbl_df()

    # transects with site details
    ts <- tx_simple %>% left_join(pl, by="plotId") %>% tbl_df()

    # add HTML popup content
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
      left_join(tx, by="transectId")

    # site profile = species records counts by site
    sp <- tbl_df(sr) %>%
      group_by(plotName, fieldName) %>%
      tally(sort=T) %>%
      spread(fieldName, n) %>%
      left_join(pl_simple, by="plotName")


    data <- list(species_records=sr,
                 vouchered_vegetation=vv,
                 vouchered_vegetation_sites=vs,
                 transects=tx,
                 transect_profiles=tp,
                 transects_sites=ts,
                 sites=pl,
                 site_profiles=sp)
    data
}
