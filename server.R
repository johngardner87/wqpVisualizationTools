
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
    
    # Bounds fit continental US
    leaflet() %>% addTiles() %>% fitBounds(-125, 25, -70, 50)
    
    # map <- tm_shape(bounds) + tm_polygons("MAP_COLORS")
    # tmap_leaflet(map)
      
  })
  
  observe({
    hucLevel <- input$hucInput
    constituents <- input$constInput
    
    boundaries <- paste("WBDHU", hucLevel, "Counts", ".gpkg", sep = "") %>% 
      paste("Datasets/WBD_Simplified/", ., sep="") %>% 
      st_read()
    
    bins <- c(0, 1000, 3000, 6000, 20000, 100000, Inf)
    # pal <- colorBin("YlOrRd", domain=boundaries$chlorophyllMeasCount, bins = bins)
    pal <- colorBin("PuBu", domain=boundaries$chlorophyllMeasCount, bins = bins)
    
    leafletProxy("wqpMap", data = boundaries) %>%
      clearShapes() %>%
      addPolygons(fillColor = ~pal(chlorophyllMeasCount), #topo.colors(10)
                  stroke = FALSE,
                  fillOpacity = 0.6,
                  highlight = highlightOptions(
                    weight=2,
                    color = "#666",
                    fillOpacity = 1,
                    bringToFront = TRUE),
                  label = paste(boundaries$NAME, ": ", boundaries$chlorophyllMeasCount, " measurements", sep=""),
                  labelOptions = labelOptions(
                    textsize = "12px"
                  )
      )
  })
}