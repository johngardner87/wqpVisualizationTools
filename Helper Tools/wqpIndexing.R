library(tidyverse)    # For data manipulation, plotting, pretty much everything
library(feather)      # Working with .feather location data
library(sf)           # For geographical information data (shapes, boundaries, flowlines)
library(nhdplusTools)

#' Index WQP Data by flowline COMID -----------------------------
#'
#' Assigns flowline COMID to WQP measurements
#'
#' @param dir String, path of directory with datasets
#' @param constituents String vector, constituent types to filter by, currently accepts "chlorophyll", "doc",                                        "secchi", "tss"
#'
wqpIndexing <- function(dir, constituents) {
  
  #Comment this out and set working directory manually if it throws an error
  setwd(dir)
  
  network <- readRDS("Datasets/NHDPlusNationalData/nhdplus_flowline.rds")

  for (i in constituents) {
    const_measurements <- st_read(paste0("Datasets/wqp_Constituents/wqp_", i, ".gpkg"))
    
    start <- Sys.time()
    nhdIndices <- get_flowline_index(flines = network,  search_radius=0.5, points = const_measurements)
    Sys.time() - start
    
    measurements_indexed <- mutate(const_measurements, COMID = pull(nhdIndices, COMID))
    st_write(measurements_indexed, paste0("Datasets/wqp_Constituents/wqp_", i, "_indexed.gpkg"), delete_layer = T)
  }
}

wqpCombining <- function(dir, constituents) {
  
  #Comment this out and set working directory manually if it throws an error
  setwd(dir)
  
  all_measurements_indexed <- NULL
  for (i in constituents) {
    const_measurement_indexed <- st_read(paste0(dir, "Datasets/wqp_Constituents/wqp_", i, "_indexed.gpkg"))
    all_measurements_indexed <- rbind(all_measurements_indexed, const_measurement_indexed)
  }
  st_write(all_measurements_indexed, paste0("Datasets/wqp_Constituents/wqp_", "All", "_indexed.gpkg"), delete_layer = T)
}

const <- c("chlorophyll", "doc", "tss", "secchi")
directory <- ""

Start <- Sys.time()
wqpIndexing(directory, const)
Sys.time() - Start

# Start <- Sys.time()
# wqpCombining(directory, const)
# Sys.time() - Start