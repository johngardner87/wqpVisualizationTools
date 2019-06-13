
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
  
  palette = "Blues" #Any pallette, I like YlOrRd or Blues
  
  observe({
    hucLevel <- input$hucInput
    hucColumn <- paste("HUC", hucLevel, sep="")
    constituent <- input$constInput
    constCol <- paste(constituent, "MeasCount", sep="")
    
    boundaries <- paste("WBDHU", hucLevel, "Counts", ".gpkg", sep = "") %>% 
      paste("Datasets/WBD_Simplified/", ., sep="") %>% 
      st_read()
    
    bins <- c(0, 10, 50, 1000, 3000, 6000, 20000, 100000, Inf)
    
    if (constituent == "All") {
      cols <- c("chlorophyllMeasCount", "docMeasCount", "secchiMeasCount", "tssMeasCount")
      allCounts <- boundaries[cols] %>% st_set_geometry(NULL) %>% reduce(`+`)
      boundaries <- mutate(boundaries, AllMeasCount = allCounts)
      pal <- colorBin(palette, domain=allCounts, bins=bins)
      
      leafletProxy("wqpMap", data = boundaries) %>%
        clearShapes() %>%
        addPolygons(fillColor = ~pal(allCounts), #topo.colors(10)
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
                    label = paste(boundaries$NAME, ": ", boundaries$AllMeasCount, " total measurements", sep=""),
                    
                    layerId = boundaries[[hucColumn]],
                    
                    labelOptions = labelOptions(
                      textsize = "12px"
                    )
        )
    } else {
      pal <- colorBin(palette, domain=boundaries[[constCol]], bins=bins)
      
      leafletProxy("wqpMap", data = boundaries) %>%
        clearShapes() %>%
        addPolygons(fillColor = ~pal(boundaries[[constCol]]), #topo.colors(10)
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
                    label = paste(boundaries$NAME, ": ", boundaries[[constCol]], " ", constituent," measurements", sep=""),
                    
                    layerId = boundaries[[hucColumn]],
                    
                    labelOptions = labelOptions(
                      textsize = "12px"
                    )
        )
    }
    
    hucSelected <- -1
    observeEvent(input$wqpMap_shape_click, {
      
      event <- input$wqpMap_shape_click
      if (event$id != hucSelected) {
        oldHuc <- hucSelected
        hucSelected <<- event$id
        selectedHucBound <- boundaries %>% 
          filter(.data[[hucColumn]] == hucSelected)
        
        selectedPoly <- leafletProxy("wqpMap", data=selectedHucBound) %>%
          # {if (hucSelected != -1) {removeShape(as.character(hucSelected))}} %>%
          removeShape(paste(oldHuc, "Selected", sep=""))
        if(constituent == "All") {
          selectedPoly %>%
            addPolygons(layerId = paste(selectedHucBound[[hucColumn]], "Selected", sep=""),
                        fillColor = ~pal(allCounts),
                        color = "red",
                        weight = 3,
                        opacity = 1,
                        label = paste(selectedHucBound$NAME, ": ", selectedHucBound$AllMeasCount, " total measurements", sep=""),
                        labelOptions = labelOptions(
                          textsize = "12px"
                        )
            )
        } else {
          selectedPoly %>%
            addPolygons(layerId = paste(selectedHucBound[[hucColumn]], "Selected", sep=""),
                        fillColor = ~pal(selectedHucBound[[constCol]]),
                        color = "red",
                        weight = 3,
                        opacity = 1,
                        label = paste(selectedHucBound$NAME, ": ", selectedHucBound[[constCol]], " ", constituent," measurements", sep=""),
                        labelOptions = labelOptions(
                          textsize = "12px"
                        )
            )
        }
        print(class(event$id))
        print(paste("you've selected: ", event$id, sep=""))
      }
    })
  })
  
}