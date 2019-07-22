library(sf)
library(tidyverse)
library(dbplyr)

#' Save processed WQP data with COMID information in SQLite format -----------------------------
#'
#' Reads in saved files from wqpIndexing script and stores as CSVs and SQLite format
#' 
#' @param dir String, path of directory with datasets
#' @param constituents String vector, names of constituent files

saveWQPSQLite <- function(dir, constituents) {
  
  setwd(dir)
  
  for (i in constituents) {
    path <- paste0("wqp_", i, "_indexed.gpkg")
    data <- st_read(path)
    coords <- as.data.frame(st_coordinates(data))
    data_xy <- mutate(data, X = pull(coords, X), Y = pull(coords, Y)) %>% st_set_geometry(NULL)
    write_csv(data_xy, paste0("wqp_", i, "_indexed", ".csv"))
  }
  
  flowlines <- readRDS("NHDPlusNationalData/nhdplus_flowline.rds")
  coverageInfo <- select(flowlines, COMID, TotDASqKM, Pathlength) %>%
    st_set_geometry(NULL)
  
  # Writing to sqlite files - written individually for each constiutent due to issues with naming tables within sql file
  All_data <- read_csv("CSVs/wqp_All_indexed.csv") %>% left_join(coverageInfo, by="COMID")
  All_data$date <- as.character(All_data$date)
  All_data$date_time <- as.character(All_data$date_time)
  file <- "wqp_All_indexed.sqlite"
  db <- src_sqlite(file, create=T)
  copy_to(db, All_data, temporary = F)
  
  secchi_data <- read_csv("CSVs/wqp_secchi_indexed.csv") %>% left_join(coverageInfo, by="COMID")
  secchi_data$date <- as.character(secchi_data$date)
  secchi_data$date_time <- as.character(secchi_data$date_time)
  file <- "wqp_secchi_indexed.sqlite"
  db <- src_sqlite(file, create=T)
  copy_to(db, secchi_data, temporary = F)
  
  doc_data <- read_csv("CSVs/wqp_doc_indexed.csv") %>% left_join(coverageInfo, by="COMID")
  doc_data$date <- as.character(doc_data$date)
  doc_data$date_time <- as.character(doc_data$date_time)
  file <- "wqp_doc_indexed.sqlite"
  db <- src_sqlite(file, create=T)
  copy_to(db, doc_data, temporary = F)
  
  chlorophyll_data <- read_csv("CSVs/wqp_chlorophyll_indexed.csv") %>% left_join(coverageInfo, by="COMID")
  chlorophyll_data$date <- as.character(chlorophyll_data$date)
  chlorophyll_data$date_time <- as.character(chlorophyll_data$date_time)
  file <- "wqp_chlorophyll_indexed.sqlite"
  db <- src_sqlite(file, create=T)
  copy_to(db, chlorophyll_data, temporary = F)
  
  tss_data <- read_csv("CSVs/wqp_tss_indexed.csv") %>% left_join(coverageInfo, by="COMID")
  tss_data$date <- as.character(tss_data$date)
  tss_data$date_time <- as.character(tss_data$date_time)
  file <- "wqp_tss_indexed.sqlite"
  db <- src_sqlite(file, create=T)
  copy_to(db, tss_data, temporary = F)
}

dir <- ""
constituents <- c("doc", "chlorophyll", "tss", "All", "secchi")
saveWQPSQLite(dir, constituents)