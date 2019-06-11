
# -------------------------------------------------------------------------
library(tidyverse)
library(feather)
library(shiny)
library(plotly)
library(leaflet)
library(sf)
library(rmapshaper)
library(tmap)

# Server ------------------------------------------------------------------

setwd("~/Documents/School/Duke/Summer 2019/Data+/")
# measByLocGeoms <- st_read("Datasets/wqp_chlorophyll.gpkg")

function(input, output, session) {
  
  wbd_path <- "Datasets/WBD_National_GDB/WBD_National_GDB.gdb/"
  
  # tmap_mode("view")
  
  output$wqpMap <- renderLeaflet({
    hucLevel <- input$hucInput
    
    bounds <- paste("WBDHU", hucLevel, ".shp", sep = "") %>% 
      paste("Datasets/WBD_Simplified/", ., sep="") %>% 
      st_read()
    
    # counts <- st_join(measByLocGeoms, bounds) %>% 
    #   count(NAME)
    # st_geometry(counts) <- NULL
    # 
    # bounds <- left_join(bounds, counts, by="NAME") %>% 
    #   plyr::rename(c("n" = "measCount"))
    # 
    # pal <- colorNumeric("Blues", domain=bounds$measCount)
    
    bounds %>%
      leaflet(data = bounds) %>%
      addTiles() %>%
      addPolygons(fillColor = topo.colors(10), #pal
                  stroke=FALSE,
                  highlight=highlightOptions(
                    weight=3,
                    color = "#666",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = bounds$NAME,
                  labelOptions = labelOptions(
                    textsize = "15px"
                  )
                  ) 
    
    # map <- tm_shape(bounds) + tm_polygons("MAP_COLORS")
    # tmap_leaflet(map)
      
  })
}