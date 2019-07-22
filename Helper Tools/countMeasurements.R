library(tidyverse)
library(sf)

#' Count available measurements by constituent in given HUC boundaries ----
#'
#' Adds column of measurements count for each HUC boundary by constituent
#'
#' @param dir String, sets directory path
#' @param constituents String, subsection of measurements to be measured
#' 
countMeasurements <- function(dir) {
  setwd(dir)
  constituents <- c("chlorophyll", "tss", "doc", "secchi")
  for (i in seq(2, 8, 2)) {
    bounds <- st_read(paste0("Datasets/WBD_Simplified/WBDHU", i, ".gpkg"))
    for (const in constituents) {
      # Original counting method, inaccurate due to non-unique vals in NAME column
      #
      # measurements <- st_read(paste0("Datasets/wqp_Constituents/wqp_", const, "_indexed.gpkg"))
      # counts <- st_join(measurements, bounds) %>% 
      #   count(NAME)
      # st_geometry(counts) <- NULL
      # bounds <- left_join(bounds, select(counts, NAME, n), by="NAME")
      # bounds$n[is.na(bounds$n)] <- 0
      # bounds <- plyr::rename(bounds, c("n" = paste(const, "MeasCount",sep="")))
      counts <- sapply(bounds[[paste0("HUC", i)]], function(j) {
        wqp_query <- sprintf("SELECT * FROM wqp_%s_indexed WHERE HUCEightDigitCode LIKE '%02s%%'", const, j)
        measurements <- st_read(paste0("Datasets/wqp_Constituents/wqp_", const, "_indexed.gpkg"), query = wqp_query) %>% nrow()
        return(measurements)
      })
      bounds <- mutate(bounds, counts)
      bounds <- plyr::rename(bounds, c("counts" = paste0(const, "MeasCount")))
    }
    cols <- c("chlorophyllMeasCount", "docMeasCount", "secchiMeasCount", "tssMeasCount")
    allCounts <- bounds[cols] %>% st_set_geometry(NULL) %>% reduce(`+`)
    bounds <- mutate(bounds, AllMeasCount = allCounts)
    st_write(bounds, paste("WBDHU", i, "Counts.gpkg", sep=""), delete_layer=T)
  }
}

directory = ""
start <- Sys.time()
countMeasurements(directory)
print(Sys.time() - start)