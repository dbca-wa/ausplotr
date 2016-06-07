# install.packages("downloader")
# install.packages("RSQLite")
# devtools::install_github("rstudio/leaflet")
# installed.packages("rio")

## Download and extract .zip archives
# library(downloader)
# download(url, dest="dataset.zip", mode="wb")
# unzip("dataset.zip", exdir = "./")

## Libraries
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(leaflet)
library(RSQLite)
# if (!require("DT")) install.packages('DT')



get_data <- function(f){
    require(RSQLite)
    con <- dbConnect(RSQLite::SQLite(), dbname=f)
    sr <- dbGetQuery(con, 'SELECT * FROM speciesRecord AS sr
      LEFT JOIN transectPoints AS tp ON sr.transectPointId = tp.id
      LEFT JOIN transects as tx ON tp.transectId = tx.id
      LEFT JOIN plots as pl ON tx.plotId = pl.id')

    vv = dbGetQuery(con, 'SELECT * FROM voucheredVeg AS vv
                    LEFT JOIN plots AS pl ON vv.plotId = pl.id')

    tx = dbGetQuery(con, 'SELECT * FROM transects AS tx
                    LEFT JOIN plots AS pl ON tx.plotId = pl.id')
    tx$popup <- paste('<h3>', tx$name, '-', tx$id,  '</h3>',
                      '<p><strong>Observed by</strong>', tx$vegObserver,
                      '(', tx$vegAffiliation, ')</p>',
                      '<p><strong>Landform</strong>', tx$landformPattern, '-',
                      tx$landformElement, '</p>',
                      '<p><strong>Description</strong>', tx$description, '</p>',
                      '<p><strong>Comment</strong>', tx$plotComment, '</p>',
                      '<p><strong>Physical Status</strong>', tx$physicalStatusComments, '</p>')

    data <- list(species_records=sr, vouchered_vegetation=vv, transects=tx)
    data
}



# d <- get_data("data/WAA COO 0001_dump_sqlite_222726_662013.db")


