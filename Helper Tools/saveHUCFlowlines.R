library(tidyverse)
library(sf)           # For geographical information data (shapes, boundaries, flowlines)
library(nhdplusTools) # For accessing National Hydrography Dataset w/ flowline information

#' Slice flowline data by HUC boundary ------------------------------------
#'
#' Separates NHD flowline data into HUC boundaries, saves as .RDS
#'
#' @param wbd_path String, filepath for folder containing simplified HUC boundaries
#' @param nhd_path String, filepath for NHD dataset
#' @param level Numeric, HUC level for trimming
#'
saveHUCFlowlines <- function(wbd_path, nhd_path, level) {
  
  # nhdplus_path(nhd_path)
  # nhd_paths <- stage_national_data()
  # network <- readRDS(nhd_paths$flowline)
  network <- readRDS("Datasets/NHDPlusNationalData/nhdplus_flowline_trimmed.rds")
  
  # hucName <- paste("WBDHU", level, "Counts.gpkg", sep="")
  # boundaries <- st_read(paste(wbd_path, hucName, sep=""))
  
  for (i in sprintf("%02i", 1:22)) {
    # hucBound <- boundaries[i,]
    hucFlowlines <- filter(network, startsWith(REACHCODE, i))
    saveRDS(hucFlowlines, file = paste0("flowlines_simplified_", i, ".rds"))
  }
}

setwd("~/Documents/School/Duke/Summer 2019/Data+/")
path_flow <- "Datasets/NHDPlusNationalData/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb"
path_wbd <- "Datasets/WBD_Simplified/"

start <- Sys.time()
saveHUCFlowlines(path_wbd, path_flow, 2)
end <- Sys.time()
end - start

