
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
function(input, output, session) {
  
  # tmap_mode("view")
  
  output$wqpMap <- renderLeaflet({
    
    # Bounds fit continental US
    leaflet() %>% addProviderTiles(providers$Esri.WorldGrayCanvas) %>% fitBounds(-100, 25, -75, 55)
    
    # map <- tm_shape(bounds) + tm_polygons("MAP_COLORS")
    # tmap_leaflet(map)
  })
  
  observe({
    hucLevel <- input$hucInput
    hucColumn <- paste("HUC", hucLevel, sep="")
    constituents <- input$constInput
    
    boundaries <- paste("WBDHU", hucLevel, "Counts", ".gpkg", sep = "") %>% 
      paste("Datasets/WBD_Simplified/", ., sep="") %>% 
      st_read()
    
    bins <- c(0, 1000, 3000, 6000, 20000, 100000, Inf)
    pal <- colorBin("YlOrRd", domain=boundaries$chlorophyllMeasCount, bins = bins)
    # pal <- colorBin("PuBu", domain=boundaries$chlorophyllMeasCount, bins = bins)
    
    leafletProxy("wqpMap", data = boundaries) %>%
      clearShapes() %>%
      addPolygons(fillColor = ~pal(chlorophyllMeasCount), #topo.colors(10)
                  color = "black",
                  weight = 1,
                  opacity = 1,
                  fillOpacity = 0.7,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "black",
                    fillOpacity = 1,
                    bringToFront = TRUE,
                    sendToBack = TRUE),
                  label = paste(boundaries$NAME, ": ", boundaries$chlorophyllMeasCount, " measurements", sep=""),
                  
                  layerId = boundaries[[hucColumn]],
                  
                  labelOptions = labelOptions(
                    textsize = "12px"
                  )
      )
    
    hucSelected <- -1
    print("reset")
    observeEvent(input$wqpMap_shape_click, {
      
      event <- input$wqpMap_shape_click
      if (event$id != hucSelected) {
        oldHuc <- hucSelected
        hucSelected <<- event$id
        selectedHucBound <- boundaries %>% 
          filter(.data[[hucColumn]] == hucSelected)
        leafletProxy("wqpMap", data=selectedHucBound) %>% 
          # {if (hucSelected != -1) {removeShape(as.character(selectedHucBound[[hucColumn]]))}} %>%
          removeShape(paste(oldHuc, "Selected", sep="")) %>% 
          addPolygons(layerId = paste(selectedHucBound[[hucColumn]], "Selected", sep=""),
                      fillColor = ~pal(chlorophyllMeasCount),
                      color = "red",
                      weight = 3,
                      opacity = 1
          )
        print(class(event$id))
        print(paste("you've selected: ", event$id, sep=""))
      }
    })
  })
  
}