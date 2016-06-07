## Libraries
# if (!require("pacman")) install.packages("pacman")
# pacman::p_load("shiny", "readr", "dplyr", "tidyr", "stringr", "lubridate",
#                "RSQLite", "DT", "markdown", update=T)
# devtools::install_github('rstudio/leaflet')
library(RSQLite)

#' Extract data from a raw Ausplot SQLite .db file
#'
#' Species Records are joined with transect point, transect, and plot details.
#' Vouchered Vegetation records are joined with plot details.
#' Transect records are joined with plot details, plus a column "popup" is added
#' containing HTML for a map popup.
#'
#' Returns a list of three data frames.
#'
#' @param f A file path, such as RShiny's input$infile$datapath
get_data <- function(f){
    require(RSQLite)
    con <- dbConnect(RSQLite::SQLite(), dbname=f)
    sr <- dbGetQuery(con, 'SELECT sr.id, sr.fieldName, sr.inCanopySky,
      sr.senescent, sr.growthForm, sr.height, sr.transectPointId,
      tp.number as transectPointNumber, tp.substrateType,
      tx.id as transectId, tx.startPoint as transectStartPoint,
      tx.endPoint as transectEndPoint, tx.completionDateTime,
      pl.id as plotId, pl.name as plotName, pl.lat as lat, pl.lon as lon,
      pl.current, pl.finished, pl.completionDate as plotCompletionDate,
      pl.completionTime as plotCompletionTime, pl.uploaded, pl.vegObserver,
      pl.vegAffiliation, pl.plotComment, pl.permanent, pl.aligned,
      pl.landformPattern, pl.landformElement, pl.siteSlope, pl.siteAspect,
      pl.outcropLithology, pl.outcropSubLithology, pl.surfaceStrewSize,
      pl.surfaceStrewLithology, pl.plotDimension100x100, pl.plotDimensionX,
      pl.plotDimensionY, pl.climaticConditions, pl.vegetationConditions,
      pl.physicalStatusComments
      FROM speciesRecord AS sr
      LEFT JOIN transectPoints AS tp ON sr.transectPointId = tp.id
      LEFT JOIN transects as tx ON tp.transectId = tx.id
      LEFT JOIN plots as pl ON tx.plotId = pl.id')

    vv = dbGetQuery(con, 'SELECT vv.*,
      pl.id as plotId, pl.name as plotName, pl.lat as lat, pl.lon as lon,
      pl.current, pl.finished, pl.completionDate as plotCompletionDate,
      pl.completionTime as plotCompletionTime, pl.uploaded, pl.vegObserver,
      pl.vegAffiliation, pl.plotComment, pl.permanent, pl.aligned,
      pl.landformPattern, pl.landformElement, pl.siteSlope, pl.siteAspect,
      pl.outcropLithology, pl.outcropSubLithology, pl.surfaceStrewSize,
      pl.surfaceStrewLithology, pl.plotDimension100x100, pl.plotDimensionX,
      pl.plotDimensionY, pl.climaticConditions, pl.vegetationConditions,
      pl.physicalStatusComments
      FROM voucheredVeg AS vv
      LEFT JOIN plots AS pl ON vv.plotId = pl.id')

    tx = dbGetQuery(con, 'SELECT tx.id as transectId,
      tx.startPoint as transectStartPoint,
      tx.endPoint as transectEndPoint, tx.completionDateTime,
      pl.id as plotId, pl.name as plotName, pl.lat as lat, pl.lon as lon,
      pl.current, pl.finished, pl.completionDate as plotCompletionDate,
      pl.completionTime as plotCompletionTime, pl.uploaded, pl.vegObserver,
      pl.vegAffiliation, pl.plotComment, pl.permanent, pl.aligned,
      pl.landformPattern, pl.landformElement, pl.siteSlope, pl.siteAspect,
      pl.outcropLithology, pl.outcropSubLithology, pl.surfaceStrewSize,
      pl.surfaceStrewLithology, pl.plotDimension100x100, pl.plotDimensionX,
      pl.plotDimensionY, pl.climaticConditions, pl.vegetationConditions,
      pl.physicalStatusComments
      FROM transects AS tx
      LEFT JOIN plots AS pl ON tx.plotId = pl.id')
    tx$popup <- paste(
      '<h3>', tx$plotName, '-', tx$transectId,  '</h3>',
      '<p><strong>Observed by</strong>', tx$vegObserver, '(', tx$vegAffiliation, ')</p>',
      '<p><strong>Landform</strong>', tx$landformPattern, '-', tx$landformElement, '</p>'
#       ,
#       '<p><strong>Description</strong>', tx$description, '</p>',
#       '<p><strong>Comment</strong>', tx$plotComment, '</p>',
#       '<p><strong>Physical Status</strong>', tx$physicalStatusComments, '</p>'
      )

    data <- list(species_records=sr, vouchered_vegetation=vv, transects=tx)
    data
}
