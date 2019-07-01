library(tidyverse)
library(sf)

#' Count available measurements by constituent in given HUC boundaries ----
#'
#' Adds column of measurements count for each HUC boundary by constituent
#'
#' @param dir String, sets directory path
#' @param constituents String, subsection of measurements to be measured
#' 
countMeasurements <- function(dir, constituents) {
  setwd(dir)
  for (i in seq(2, 12, 2)) {
    bounds <- st_read(paste("Datasets/WBD_Simplified/WBDHU", i, ".gpkg", sep=""))
    for (const in constituents) {
      measurements <- st_read(paste("Datasets/wqp_Constituents/wqp_", const, ".gpkg", sep=""))
      counts <- st_join(measurements, bounds) %>% 
        count(NAME)
      st_geometry(counts) <- NULL
      bounds <- left_join(bounds, select(counts, NAME, n), by="NAME")
      bounds$n[is.na(bounds$n)] <- 0
      bounds <- plyr::rename(bounds, c("n" = paste(const, "MeasCount",sep="")))
    }
    st_write(bounds, paste("WBDHU", i, "Counts.gpkg", sep=""), delete_layer=T)
  }
}

directory = "~/Documents/School/Duke/Summer 2019/Data+/"
const <- c("chlorophyll", "tss", "doc", "secchi")
countMeasurements(directory, const)
