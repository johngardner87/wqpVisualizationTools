
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
  
  #Comment this out and set working directory manually if it throws an error
  setwd(dir)
  
  # Measurements
  wqp_measurements <- read_csv("Datasets/wqp_long_with_methods.csv")
  # Locations
  wqp_locations <- read_feather("Datasets/wqp_inventory.feather")
  # Satellite Info
  wqp_sat <- read_feather("Datasets/wide_pull.feather")
  
  if ("chlorophyll" %in% constituents) {
    meas_constituents <- constituents
    meas_constituents[meas_constituents == "chlorophyll"] <- "chl.a"
  } else {
    meas_constituents <- constituents
  }
  
  measurements_filtered <- filter(wqp_measurements, harmonized_parameter %in% meas_constituents)
  locations_filtered <- filter(wqp_locations, Constituent %in% constituents)
  sat_data <- select(wqp_sat, SiteID, date, landsat_id)
  
  # Now each measurement value is matched with corresponding location info from locations variable
  measByLocations <- left_join(measurements_filtered, locations_filtered, by = c("SiteID" = "MonitoringLocationIdentifier")) %>% 
    drop_na(LatitudeMeasure, LongitudeMeasure) %>% 
    left_join(sat_data, by = c("SiteID", "date"))

  fileName <- paste(constituents, collapse="_") %>% 
    paste("wqp", . , sep="_")
  
  if (shape) {
    # Make geometry out of latitude, longitude points
    measByLocGeoms <- st_as_sf(measByLocations, coords = c("LongitudeMeasure", "LatitudeMeasure"), crs=4269)
    st_write(measByLocGeoms, paste(fileName, ".gpkg", sep=""))
  } else {
    write_csv(measByLocations, paste(fileName, ".csv", sep=""))
  }
  
}

directory = ""
wqpJoin(directory, "chlorophyll", T)
wqpJoin(directory, "tss", T)
wqpJoin(directory, "doc", T)
wqpJoin(directory, "secchi", T)