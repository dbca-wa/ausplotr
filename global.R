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
#'  transect, plus date, location, tx point details, tx and site names, and
#'  voucher barcodes
#' basal_wedge: basal wedge records with voucher barcodes
#' transects: all Transect details plus selected plot details
#' sites: all Plot details
#' transects_sites: all tx and site details plus HTML for map popup
#' transect_profiles: transect by species pivot table
#' site_profiles: site by species pivot table
#' vouchered_vegetation: barcodes for specimen vouchers, linking field names to
#'  properly identified species names later
#'
#' @param f A file path, such as RShiny's input$infile$datapath
get_one_data <- function(filename, datapath){
    message(paste("Loading" , filename, "from", datapath))

    require(RSQLite)
    con <- dbConnect(RSQLite::SQLite(), dbname=datapath)

    pl <- dbGetQuery(con, paste0(
      'SELECT pl.id as plotId, "', filename, '-site-"||pl.id as plotUid,
      pl.name as plotName, pl.lat as lat, pl.lon as lon,
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
      FROM plots AS pl LEFT JOIN sitesummary AS ss ON pl.id = ss.plotId')) %>%
      mutate(plotCompletionDateTime=paste(completionDate, completionTime)) %>%
      mutate(plotCompletionDateTime=parse_date_time(
        plotCompletionDateTime, orders=c("YmdHMS"), tz="Australia/Perth")) %>%
      tbl_df()
    rownames(pl) <- pl$plotUid

    pl_simple <- select(pl, plotId, plotName, lat, lon, plotCompletionDateTime)

    tx <- dbGetQuery(con, paste0(
      'SELECT tx.id as transectId, "', filename, '-tx-"||tx.id as txUid,
      tx.startPoint as transectStartPoint, tx.endPoint as transectEndPoint,
      tx.completionDateTime,
      pl.id as plotId, pl.name as plotName, pl.lat as lat, pl.lon as lon
      FROM transects AS tx LEFT JOIN plots AS pl ON tx.plotId = pl.id')) %>%
      mutate(transectCompletionDateTime=parse_date_time(
        completionDateTime, orders="mdYHMS", tz="Australia/Perth")) %>%
      tbl_df()
    rownames(tx) <- tx$txUid

    tx_simple <- select(tx, transectId, transectStartPoint, transectEndPoint,
      transectCompletionDateTime, plotId)

    # vouchered vegetation with basic site details
    vv_simple <- dbGetQuery(con, paste0('SELECT *, "', filename,
      '-vv-"||vv.id as vvUid FROM voucheredVeg as vv')) %>% tbl_df()
    rownames(vv_simple) <- vv_simple$vvUid
    vv <- left_join(vv_simple, pl_simple, by="plotId") %>% tbl_df()

    sr <- dbGetQuery(con, paste0(
      'SELECT sr.id, "', filename, '-sr-"||sr.id as srUid,
      sr.fieldName, sr.inCanopySky,
      sr.senescent, sr.growthForm, sr.height, sr.transectPointId,
      tp.number as transectPointNumber, tp.substrateType, tp.transectId
      FROM speciesRecord AS sr
      LEFT JOIN transectPoints AS tp ON sr.transectPointId = tp.id')) %>%
      left_join(tx_simple, by="transectId") %>%
      left_join(pl_simple, by="plotId") %>%
      left_join(vv_simple, by=c("fieldName", "plotId")) %>% tbl_df()
    rownames(sr) <- sr$srUid

    # basal wedge with basic site details
    bw = dbGetQuery(con, paste0('SELECT *, "', filename,
                    '-bw-"||bw.id as bwUid FROM bwRecords as bw')) %>%
      left_join(pl_simple, by="plotId") %>%
      left_join(vv_simple, by=c("fieldName", "plotId")) %>% tbl_df()
    rownames(bw) <- bw$bwUid

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
    row.names(tp) <- paste(tp$plotName, "-", tp$transectId)
    # tp[is.na(tp)] <- 0

    # site profile = species records counts by site
    sp <- tbl_df(sr) %>%
      group_by(plotName, fieldName) %>%
      tally(sort=T) %>%
      spread(fieldName, n) %>%
      ungroup() %>%
      left_join(pl_simple, by="plotName")
    # sp[is.na(sp)] <- 0

    d <- list(species_records=sr,
         basal_wedge=bw,
         vouchered_vegetation=vv,
         transects=tx,
         transects_sites=ts,
         transect_profiles=tp,
         sites=pl,
         site_profiles=sp)
    d
}


#' Row-bind dataframes with a shared list name `ln` across a list of lists `lol`
#'
#' Arguments:
#' lol  A list of lists of dataframes, e.g. a list of `get_one_data` outputs
#' ln   A name of a dataframe in the named list, e.g. `species_records`
#'
#' Returns:
#'      The sum of all dataframes row-bound as one dataframe, e.g. all
#'      `species_records` in one dataframe
combine_df <- function(ln, lol) {bind_rows(lapply(lol, "[[", ln))}


#' Combine data read from multiple .db files into one list of dataframes
#'
#' Steps:
#'
#' * read each input file (.db) as list of dataframes into a list of lists
#' * get the names of dataframes
#' * combine dataframes of each name (all species_records, etc.)
#' * restore the names and return the list of merged dataframes
get_data <- function(fup){
  m <- mapply(get_one_data, fup$name, fup$datapath, SIMPLIFY=F)
  ln <- names(m[[1]])
  z <- lapply(ln, combine_df, m)
  names(z) <- ln
  z
}


#' Filter a dataframe `d` returning rows where column matches value `val`
filterDf <- function(d, val){filtered <- d[which(d$plotName %in% val),]}


#' Filter a list of dataframes `ld` to one plotName `pn`
get_filtered_data <- function(ld, pn="All"){
  if (pn=="All") return(ld) else lapply(ld, filterDf, pn)
}


#' Prepare a DT datatable with sensible defaults
make_dt <- function(x, filter="top", pageLength=10){
  DT::renderDataTable(
    DT::datatable(
      x,
      filter=filter,
      options=list(
        pageLength = pageLength,
        autoWidth = TRUE,
        columnDefs = list(list(width='500px', targets=c("plotComment")))
      )))
}
