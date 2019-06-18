
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
  
  output$wqpMap <- renderLeaflet({
    # Bounds fit continental US
    leaflet() %>% addProviderTiles(providers$Esri.WorldGrayCanvas) %>% fitBounds(-100, 25, -75, 55)
  })
  
  palette = "Blues" #Any pallette, I like YlOrRd or Blues
  
  hucSelected <<- -1
  selectedHucBound <<- NULL
  
  output$select <- reactive({F})
  outputOptions(output, "select", suspendWhenHidden = FALSE)
  
  observe({
    hucLevel <- input$hucInput
    # hucLevelUp <- as.numeric(hucLevel) - 2
    hucLevelUp <- 2
    hucColumn <- paste("HUC", hucLevel, sep="")
    constituent <- input$constInput
    constCol <- paste(constituent, "MeasCount", sep="")
    
    boundaries <- paste("WBDHU", hucLevel, "Counts", ".gpkg", sep = "") %>% 
      paste("Datasets/WBD_Simplified/", ., sep="") %>% 
      st_read()
    
    if (hucLevel > 2) {
      upperBoundaries <- paste("WBDHU", hucLevelUp, "Counts", ".gpkg", sep = "") %>% 
        paste("Datasets/WBD_Simplified/", ., sep="") %>% 
        st_read()
    }
    
    bins <- c(0, 10, 50, 1000, 3000, 6000, 20000, 100000, Inf)
    
    hucMap <- leafletProxy("wqpMap", data = boundaries) %>% 
      clearShapes() %>% 
      addMapPane("larger", zIndex = 440) %>% 
      addMapPane("main", zIndex = 420) %>% 
      addMapPane("selections", zIndex = 430)
    
    if(hucLevel > 2) {
      hucMap %>% 
        addPolylines(data = upperBoundaries,
                    fillOpacity = 0,
                    weight = 3,
                    color = "gray",
                    group = "upperBoundaries",
                    options = pathOptions(pane = "larger")
        )
    }
    
    if (constituent == "All") {
      cols <- c("chlorophyllMeasCount", "docMeasCount", "secchiMeasCount", "tssMeasCount")
      allCounts <- boundaries[cols] %>% st_set_geometry(NULL) %>% reduce(`+`)
      boundaries <- mutate(boundaries, AllMeasCount = allCounts)
      pal <- colorBin(palette, domain=allCounts, bins=bins)
      
      hucMap %>%
        addPolygons(fillColor = ~pal(allCounts), #topo.colors(10)
                    color = "black",
                    weight = 1,
                    opacity = 1,
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                      weight = 2,
                      color = "black",
                      fillOpacity = 1,
                      bringToFront = TRUE,
                      sendToBack = TRUE),
                    label = paste(boundaries$NAME, ": ", boundaries$AllMeasCount, " total measurements", sep=""),
                    
                    layerId = boundaries[[hucColumn]],
                    
                    labelOptions = labelOptions(
                      textsize = "12px"
                    ),
                    options = pathOptions(pane = "main")
        ) %>% addLegend("bottomleft", pal, values = allCounts, layerId = "legend")
    } else {
      pal <- colorBin(palette, domain=boundaries[[constCol]], bins=bins)
      
      hucMap %>%
        addPolygons(fillColor = ~pal(boundaries[[constCol]]), #topo.colors(10)
                    color = "black",
                    weight = 1,
                    opacity = 1,
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                      weight = 2,
                      color = "black",
                      fillOpacity = 1,
                      bringToFront = TRUE,
                      sendToBack = TRUE),
                    label = paste(boundaries$NAME, ": ", boundaries[[constCol]], " ", constituent," measurements", sep=""),
                    
                    layerId = boundaries[[hucColumn]],
                    
                    labelOptions = labelOptions(
                      textsize = "12px"
                    ),
                    options = pathOptions(pane = "main")
        ) %>% addLegend("bottomleft", pal, boundaries[[constCol]], layerId = "legend")
    }
    
    observeEvent(input$wqpMap_shape_click, {
      
      event <- input$wqpMap_shape_click
      
      if(is.null(event$id)) {                                           # If the user clicks on a boundary
        print("boundary selected")
      } else if (nchar(hucSelected) != as.numeric(hucLevel)) {          # If the user changes the HUC
        output$select <- reactive({F})
      } else if (event$id != paste(hucSelected, "Selected", sep="")) {  # If the user clicks on a new HUC
        oldHuc <- hucSelected
        hucSelected <<- event$id
        selectedHucBound <<- boundaries %>% 
          filter(.data[[hucColumn]] == hucSelected)
        
        selectedPoly <- leafletProxy("wqpMap", data=selectedHucBound) %>%
          removeShape(paste(oldHuc, "Selected", sep=""))
        
        if(constituent == "All") {
          selectedPoly %>%
            addPolygons(layerId = paste(selectedHucBound[[hucColumn]], "Selected", sep=""),
                        fillColor = ~pal(allCounts),
                        color = "red",
                        weight = 3,
                        opacity = 1,
                        label = paste(selectedHucBound$NAME, ": ", selectedHucBound$AllMeasCount, " total measurements",
                                      sep=""),
                        labelOptions = labelOptions(
                          textsize = "12px"
                        ),
                        options = pathOptions(pane = "selections")
            )
        } else {
          selectedPoly %>%
            addPolygons(layerId = paste(selectedHucBound[[hucColumn]], "Selected", sep=""),
                        fillColor = ~pal(selectedHucBound[[constCol]]),
                        color = "red",
                        weight = 3,
                        opacity = 1,
                        label = paste(selectedHucBound$NAME, ": ",
                                      selectedHucBound[[constCol]], " ", constituent," measurements", sep=""),
                        labelOptions = labelOptions(
                          textsize = "12px"
                        ),
                        options = pathOptions(pane = "selections")
            )
        }
        print(paste("you've selected: ", hucSelected, sep=""))
        output$select <- reactive({T})
      } else {                                                          # If the user clicks on the selected HUC
        print(paste("removed:", event$id))
        leafletProxy("wqpMap") %>% removeShape(event$id)
        output$select <- reactive({F})
      }
    })
  })
  
  observeEvent(input$zoom, {
    output$hucDetail <- renderLeaflet({
      # Bounds fit continental US
      hucDetailMap <- leaflet() %>% addProviderTiles(providers$Esri.WorldGrayCanvas) %>% 
        addPolygons(data = selectedHucBound,
                    layerId = hucSelected,
                    label = hucSelected,
                    color = "black",
                    weight = 3)
      
    })
    output$zoomedIn <- renderUI({
      fluidPage(
        class = "details",
        actionButton("back", "Take me back!"),
        leafletOutput(outputId = "hucDetail", height = "400px", width="400px")
        # plotlyOutput("coverage")
      )
    })
  })
  
  observeEvent(input$back, {
    output$zoomedIn <- NULL
    selectedHucBound <<- NULL
  })
  
  # output$coverage <- renderPlotly({
  #   
  # })
  
}