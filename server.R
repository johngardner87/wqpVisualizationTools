
# Packages ----------------------------------------------------------------
library(tidyverse)
library(feather)
library(shiny)
library(plotly)
library(leaflet)
library(sf)
library(rmapshaper)
library(tmap)
library(shinycssloaders)
library(shinyalert)
library(crosstalk)

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
  hucRegions <- tibble("02" = "",
                       "04" = "Subregion",
                       "06" = "Basin",
                       "08" = "Subbasin",
                       "10" = "Watershed",
                       "12" = "Subwatershed")
  
  options(opacityDim = 0, persistent = F, selected = attrs_selected(fill="toself", fillcolor = "green"))

  output$select <- reactive({F})
  outputOptions(output, "select", suspendWhenHidden = FALSE)
  
  # Main map ---------------------------------------------------------------
  observe({
    hucLevel <- input$hucInput
    # hucLevelUp <- as.numeric(hucLevel) - 2
    hucLevelUp <- 2
    hucColumn <- paste("HUC", hucLevel, sep="")
    constituent <- input$constInput
    constCol <<- paste(constituent, "MeasCount", sep="")
    
    # Load boundary data for selected HUC and HUC2 boundaries for context
    boundaries <- paste("WBDHU", hucLevel, "Counts", ".gpkg", sep = "") %>% 
      paste("Datasets/WBD_Simplified/", ., sep="") %>% 
      st_read()
    
    if (hucLevel > 2) {
      upperBoundaries <- paste("WBDHU", hucLevelUp, "Counts", ".gpkg", sep = "") %>% 
        paste("Datasets/WBD_Simplified/", ., sep="") %>% 
        st_read()
    }
    
    bins <- c(0, 10, 50, 1000, 3000, 6000, 20000, 100000, Inf)
    
    # Map panes for proper layering of UI
    hucMap <- leafletProxy("wqpMap", data = boundaries) %>% 
      clearShapes() %>% 
      addMapPane("larger", zIndex = 440) %>% # Region boundaries for context
      addMapPane("main", zIndex = 420) %>%   # Main boundaries
      addMapPane("selections", zIndex = 430) # Selected boundaries
    
    if(hucLevel > 2) { # Only adds context boundaries for HUCs greater than 2
      hucMap %>% 
        addPolylines(data = upperBoundaries,
                    fillOpacity = 0,
                    weight = 3,
                    color = "gray",
                    group = "upperBoundaries",
                    options = pathOptions(pane = "larger")
        )
    }
    
    # Drawing of main polygons with coloring based on constituent selection
    if (constituent == "All") {
      cols <- c("chlorophyllMeasCount", "docMeasCount", "secchiMeasCount", "tssMeasCount")
      allCounts <- boundaries[cols] %>% st_set_geometry(NULL) %>% reduce(`+`)
      boundaries <- mutate(boundaries, AllMeasCount = allCounts)
      pal <- colorBin(palette, domain=allCounts, bins=bins)
      # pal <- colorQuantile(palette, domain=allCounts, n = 6)
      
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
    
    # HUC selection (redraws polygon with red boundary, behaves differently depending on current selection)
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
  
  # Secondary window ---------------------------------------------------------------
  observeEvent(input$zoom, {
    if(is.null(selectedHucBound[[constCol]]) || selectedHucBound[[constCol]] == 0) {
      shinyalert(
        text = "<div style='padding-left:40px; padding-right:40px'> 
        There are no measurements of the chosen constituent type in the chosen region. </div>",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = TRUE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#001A57",
        timer = 0,
        # imageUrl = "",
        animation = TRUE
      )
    } else {
      output$zoomedIn <- renderUI({
        fluidPage(
          class = "details",
          tags$h1(paste("Coverage in the", selectedHucBound$NAME, hucRegions[sprintf("%02s", str_length(hucSelected))])),
          tags$h3(paste("HUC:", hucSelected)),
          actionButton("back", "Take me back!", icon = icon("arrow-left"), style="position:absolute; top:40px; right:90px"),
          fluidRow(
            column(6, 
              leafletOutput(outputId = "hucDetail") %>% withSpinner(type=2, color.background="white"),
              checkboxInput("cluster", "Cluster ", F)
            ),
            column(6,
                   plotlyOutput("coverage")
            )
          )
        )
      })
      
      # Loading relevant wqp data and flowline data
      wqp_path <- sprintf(
        "~/Documents/School/Duke/Summer 2019/Data+/Datasets/wqp_Constituents/wqp_%s_indexed.gpkg", input$constInput)
      wqp_query <- sprintf(
        "SELECT * FROM wqp_%s_indexed WHERE HUCEightDigitCode LIKE '%s%%'", input$constInput, hucSelected)
      selected_wqp_data <- st_read(wqp_path, query=wqp_query)
      
      nhd_path <- "~/Documents/School/Duke/Summer 2019/Data+/Datasets/NHDPlusNationalData/Flowlines/"
      nhd_file_path <- paste0(nhd_path, "flowlines_simplified_", substr(hucSelected, 1, 2),".rds")
      regionFlowlines <- readRDS(nhd_file_path)
      hucFlowlines <- filter(regionFlowlines, startsWith(REACHCODE, as.character(hucSelected)))
      
      coverageInfo <- select(hucFlowlines, COMID, TotDASqKM, Pathlength) %>% 
        st_set_geometry(NULL)
      selected_wqp_data_coverage <- left_join(selected_wqp_data, coverageInfo, by="COMID")
      key <- highlight_key(selected_wqp_data_coverage, group = "coverage")
      # covg <- ggplot(key) + geom_point(mapping = aes(x=date, y = TotDASqKM))
      
      # Secondary detail map with markers for site locations
      output$hucDetail <- renderLeaflet({
        # Bounds fit continental US
        hucDetailMap <- leaflet(key) %>% 
          addProviderTiles(providers$Esri.WorldGrayCanvas,
                           options = providerTileOptions(updateWhenZooming=F, updateWhenIdle = T)) %>% 
          addPolygons(data = selectedHucBound,
                      layerId = hucSelected,
                      group = "bounds",
                      #label = hucSelected,
                      color = "black",
                      fillOpacity = 0.1,
                      weight = 3) #%>% 
          # addCircleMarkers(radius = 3,
          #                  data = key,
          #                  stroke = F,
          #                  color = "red",
          #                  opacity = 0.8,
          #                  fillOpacity = 0.2,
          #                  group = "markers",
          #                  label = ~MonitoringLocationName
          # )
      })
      
      markers <- leafletProxy("hucDetail", data = key) %>% 
        addCircleMarkers(radius = 3,
                          data = key,
                          stroke = F,
                          color = "red",
                          opacity = 0.8,
                          fillOpacity = 0.2,
                          group = "markers",
                          label = ~MonitoringLocationName
                        )
      
      # Cluster selection option
      observeEvent(input$cluster, {
        if(!is.null(input$cluster)) {
          # if(input$cluster == pastCluster) {return()}
          clusterBool <- input$cluster
          markers <- leafletProxy("hucDetail", data = key)
          clearGroup(markers, group="markers")
          if(clusterBool) {
            markers %>%
              addCircleMarkers(
                               stroke = F,
                               # color = "black",
                               clusterOptions = markerClusterOptions(),
                               # layerId = ~SiteID,
                               group = "markers",
                               label = ~MonitoringLocationName)
          } else {
            markers %>%
              addCircleMarkers(radius = 3,
                         stroke = F,
                         color = "red",
                         opacity = 0.8,
                         fillOpacity = 0.2,
                         group = "markers",
                         label = ~MonitoringLocationName
              )
          }
        }
      })
      
      # Coverage plot
      output$coverage <- renderPlotly({
        covg <- plot_ly(key, x=~date, y=~TotDASqKM, source="subset") %>% 
          add_markers(alpha = 0.5) %>% 
          highlight("plotly_selected") %>% 
          toWebGL()
      })
      
      # output$timeSeries <- renderplotly({
      #   event.data <- event_data("plotly_selected", source = "subset")
      # })
    }
  })
  
  # Back button
  observeEvent(input$back, {
    output$zoomedIn <- NULL
    # selectedHucBound <<- NULL
  })
}