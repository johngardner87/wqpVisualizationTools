library(tidyverse)
library(sf)           # For geographical information data (shapes, boundaries, flowlines)
library(nhdplusTools) # For accessing National Hydrography Dataset w/ flowline information

#' Slice flowline data by HUC boundary ------------------------------------
#'
#' Separates NHD flowline data into HUC boundaries, saves as .RDS
#'
#' @param nhd_path String, filepath for NHD dataset
#' @param level Numeric, HUC level for trimming
#'
saveHUCFlowlines <- function(wbd_path, nhd_path, level) {
  
  nhdplus_path(nhd_path)
  nhd_paths <- stage_national_data()
  network <- readRDS(nhd_paths$flowline)
  
  for (i in sprintf("%02i", 1:22)) {
    hucFlowlines <- filter(network, startsWith(REACHCODE, i))
    saveRDS(hucFlowlines, file = paste0("flowlines_simplified_", i, ".rds"))
  }
}

setwd("")
path_flow <- "Datasets/NHDPlusNationalData/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb"

start <- Sys.time()
saveHUCFlowlines(path_flow, 2)
Sys.time() - start

