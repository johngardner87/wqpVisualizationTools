
library(tidyverse)    # For data manipulation, plotting, pretty much everything
library(feather)      # Working with .feather location data
library(sf)           # For geographical information data (shapes, boundaries, flowlines)

#' Filter, join, and save sections of WQP Data -----------------------------
#'
#' Filters measurement values and site location information by constituents, joins, and saves files
#' Stores joined data as longitude/latitude columns or shape files, saves as .csv or .gpkg respectively
#'
#' @param dir String, path of directory with datasets
#' @param constituent String vector, constituent types to filter by, currently accepts "chlorophyll", "doc",                                       "secchi", "tss"
#' @param shape Boolean, saves sf files when TRUE, defaults to FALSE
#'
wqpJoin <- function(dir, constituents, shape = FALSE) {
  
  setwd(dir)
  
  # Measurements: https://www.dropbox.com/s/916qkiihkf9vcij/wqp_long_with_methods.csv?dl=0
  wqp_measurements <- read_csv("Datasets/wqp_long_with_methods.csv")
  # Locations: https://drive.google.com/file/d/17RXovXq2_7qrwHC31wfhSKhmZT2n64sF/view
  wqp_locations <- read_feather("Datasets/wqp_inventory.feather")
  
  if ("chlorophyll" %in% constituents) {
    meas_constituents <- constituents
    meas_constituents[meas_constituents == "chlorophyll"] <- "chl.a"
  } else {
    meas_constituents <- constituents
  }
  
  measurements_filtered <- filter(wqp_measurements, harmonized_parameter %in% meas_constituents)
  locations_filtered <- filter(wqp_locations, Constituent %in% constituents)
  
  # Now each measurement value is matched with corresponding location info from locations variable
  measByLocations <- left_join(measurements_filtered, locations_filtered, by = c("SiteID" = "MonitoringLocationIdentifier"))
  measByLocationsNoNA <- na.omit(measByLocations, cols=c("LatitudeMeasure", "LongitudeMeasure"))
  
  fileName <- paste(constituents, collapse="_") %>% 
    paste("wqp", . , sep="_")
  
  if (shape) {
    # Make geometry out of latitude, longitude points
    measByLocGeoms <- st_as_sf(measByLocationsNoNA, coords = c("LongitudeMeasure", "LatitudeMeasure"), crs=4269)
    st_write(measByLocGeoms, paste(fileName, ".gpkg", sep=""))
  } else {
    write_csv(measByLocationsNoNA, paste(fileName, ".csv", sep=""))
  }
  
}

directory = "~/Documents/School/Duke/Summer 2019/Data+/"
wqpJoin(directory, "tss", T)