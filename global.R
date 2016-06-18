## Libraries
# if (!require("pacman")) install.packages("pacman")
# pacman::p_load("shiny", "readr", "dplyr", "tidyr", "stringr", "lubridate",
               # "RSQLite", "DT", "markdown", update=F)
# devtools::install_github('rstudio/leaflet')
library(RSQLite)
library(tidyr)
library(dplyr)
library(readr)
library(lubridate)
library(shiny)
library(DT)
library(sp)
library(rgeos)

#------------------------------------------------------------------------------#
# Read observations from Ausplot field data app

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

#------------------------------------------------------------------------------#
# Filter observations

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


#------------------------------------------------------------------------------#
# Sites from dGPS

#' Convert strings like `119.31'8.14"` to `119°31'8.14"`
repair_dms <- function(dms_string){sub("[.]", "°", dms_string)}

#' Read one dGPS file into a SpatialPointsDataFrame of plotName and centroid
#'
#' The site name must the in line 3 as 4th token
#' The points must begin in line 9
#' The points column names must be:
#' "point","easting","northing","rl", "lat","lon","code","att1","3DCQ"
#' The coordinates are assumed to be of format `119.31'8.14"`
read_one_site <- function(filename, datapath){
  pn <- stringr::str_replace_all(
    names(read_delim(datapath, skip=2, n_max = 1, delim=" "))[4], "-"," ")
  cols <- c("point","easting","northing","rl", "lat","lon","code","att1","3DCQ")
  gpspoints <- read_csv(datapath, skip=8, col_names=cols) %>%
    mutate(plotName=pn,
           lon_dd = as.numeric(char2dms(repair_dms(lon), chd="°")),
           lat_dd = as.numeric(char2dms(repair_dms(lat), chd="°")),
           lon_dms = as.character(char2dms(repair_dms(lon), chd="°")),
           lat_dms = as.character(char2dms(repair_dms(lat), chd="°"))) %>%
    tbl_df()
  gpspoints
}

#' Return plotName and centroid lon/lat as tbl_df from a read_one_site tbl_df
site_centroid <- function(sitedf){
  wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
  gps.dd.df <- as.data.frame(select(sitedf, lat_dd, lon_dd))
  sitecen <- gCentroid(SpatialPoints(gps.dd.df, proj4string=wgs84))
  # site <- SpatialPointsDataFrame(sitecen, as.data.frame(list(plotName=pn)))
  site <- tbl_df(
    as.data.frame(
      list(plotName=sitedf[1,]$plotName,
           longitude=sitecen$x,
           latitude=sitecen$y)))
  site
}

get_one_site_centroid <- function(filename, datapath){
  site_centroid(read_one_site(filename, datapath))
}

#' Read all dGPS text files into one tbl_df
get_sites <- function(fup){
  bind_rows(
    as.list(
      mapply(read_one_site, fup$name, fup$datapath, SIMPLIFY=F)))
}

#' Read all dGPS text files into one tbl_df of centroids
get_site_centroids <- function(fup){
  bind_rows(
    as.list(
      mapply(get_one_site_centroid, fup$name, fup$datapath, SIMPLIFY=F)))
}

## Create convex hull from points
# wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
# gps.dd.df <- as.data.frame(select(gpspoints, lat_dd, lon_dd))
# gps.dms.df <- as.data.frame(select(gpspoints, lat_dms, lon_dms))
# gps.df <- as.data.frame(select(gpspoints, plotName, point, lon_dd, lat_dd,
#                                lon_dms, lat_dms, easting, northing))
# sitepol <- gConvexHull(SpatialPoints(gps.dd.df, proj4string=wgs84))
# sitecen <- gCentroid(SpatialPoints(gps.dd.df, proj4string=wgs84))

# sitesdf <- SpatialPointsDataFrame(SpatialPoints(gps.dd.df, proj4string=wgs84), gps.df)
# site <- SpatialPointsDataFrame(sitecen, as.data.frame(sitename))
## Write to GeoJSON
# writeOGR(site, paste0(n, '.geojson'), 'sites', driver='GeoJSON')
# writeOGR(sitesdf, paste0(n, '-txpoints.geojson'), 'transectpoints', driver='GeoJSON')


