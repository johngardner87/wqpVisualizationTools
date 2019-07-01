
# Packages ----------------------------------------------------------------
library(tidyverse)
library(feather)
library(shiny)
library(plotly)
library(leaflet)
library(sf)
library(rmapshaper)
library(shinycssloaders)
library(shinyalert)
library(crosstalk)

# Server ------------------------------------------------------------------

getRegionName <- function(huc) {
  huc2Names <- tibble("01" = "New England Region", "02" = "Mid Atlantic Region", "03" = "South Atlantic-Gulf Region", 
                      "04" = "Great Lakes Region", "05" = "Ohio Region", "06" = "Tennessee Region", 
                      "07" = "Upper Mississippi Region", "08" = "Lower Mississippi Region", "09" = "Souris-Red-Rainy Region", 
                      "10" = "Missouri Region", "11" = "Arkansas-White-Red Region", "12" = "Texas-Gulf Region", 
                      "13" = "Rio Grande Region", "14" = "Upper Colorado Region", "15" = "Lower Colorado Region", 
                      "16" = "Great Basin Region", "17" = "Pacific Northwest Region", "18" = "California Region", 
                      "19" = "Alaska Region", "20" = "Hawaii Region", "21" = "Caribbean Region", 
                      "22" = "South Pacific Region")
  if (str_length(huc) == 2) {
    return("")
  } else {
    return (paste0("in the ", huc2Names[substr(huc, 1, 2)]))
  }
}

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
  
  # options(opacityDim = 0, persistent = F, selected = attrs_selected(fill="toself", fillcolor = "green"))
  options(opacityDim = 0)

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
                    weight = 5,
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
        confirmButtonCol = "#012169",
        timer = 0,
        # imageUrl = "",
        animation = TRUE
      )
    } else {
      print("Rendering second page")
      output$zoomedIn <- renderUI({
        fluidPage(
          class = "details",
          fluidRow(
            column(10,
                   tags$h1("Coverage in the ", strong(selectedHucBound$NAME), hucRegions[sprintf("%02s", str_length(hucSelected))])
                   ),
            column(2, 
                   actionButton("back", "Take me back!", width = "100%", icon = icon("arrow-left"), style="position:absolute; top:28px")
                   )
          ),
          fluidRow(
            column(10,
                   tags$h3(paste0("HUC", str_length(hucSelected), ": "),
                           strong(hucSelected), " — ", selectedHucBound$AREASQKM,
                           "sq. km ", getRegionName(hucSelected))
            ),
            column(2, 
                    h3("Filters")
            )
          ),
          fluidRow(
            column(5,
              div(class="widget",
                  leafletOutput(outputId = "hucDetail") %>% withSpinner(type=2, color.background="white")
              )
            ),
            column(5,
             div(class="widget",
                plotlyOutput("coverage")
             )
            ),
            column(2,
              div(class = "widget",
                h4("Global Settings"),
                # filter_select("selectLocationType", "Filter by monitoring location type:", 
                #               key, ~ResolvedMonitoringLocationTypeName, multiple = F),
                selectizeInput("selectLocationType", "Filter by monitoring location type:", choices=locationTypeNames, width = "100%"),
                checkboxInput("cluster", "Cluster ", F)
              )
            )
          ),
          # dateRangeInput("dateFilter", "Date range:"),
          fluidRow(
            column(10,
              div(class = "widget",
                plotlyOutput("timeSeries")
              )
            )
          )
        )
      })
      
      # Loading relevant wqp data and flowline data
      wqp_path <- sprintf(
        "Datasets/wqp_Constituents/wqp_%s_indexed.gpkg", input$constInput)
      wqp_query <- sprintf(
        "SELECT * FROM wqp_%s_indexed WHERE HUCEightDigitCode LIKE '%s%%'", input$constInput, hucSelected)
      selected_wqp_data <- st_read(wqp_path, query=wqp_query)
      
      start <- Sys.time()
      nhd_path <- "Datasets/NHDPlusNationalData/Flowlines/"
      nhd_file_path <- paste0(nhd_path, "flowlines_simplified_", substr(hucSelected, 1, 2),".rds")
      regionFlowlines <- readRDS(nhd_file_path)
      hucFlowlines <- filter(regionFlowlines, startsWith(REACHCODE, as.character(hucSelected)))
      print(Sys.time() - start)
      
      # start <- Sys.time()
      # nhd_path <- "Datasets/NHDPlusNationalData/Flowlines_HUC8"
      # hucFlowlines <- list.files(nhd_path, pattern=paste0("^HUC8_", hucSelected), full.names = T) %>%
      #   map(readRDS) %>%
      #   do.call(rbind, .)
      # print(Sys.time() - start)
      # 
      # start <- Sys.time()
      # nhd_path <- "Datasets/NHDPlusNationalData/nhdplus_flowline.gpkg"
      # nhd_query <- sprintf("SELECT * FROM nhdplus_flowline WHERE REACHCODE LIKE '%s%%'", hucSelected)
      # hucFlowlines <- st_read(nhd_path, query = nhd_query)
      # print(Sys.time() - start)
      # 
      coverageInfo <- select(hucFlowlines, COMID, TotDASqKM, Pathlength) %>% 
        st_set_geometry(NULL)
      selected_wqp_data_coverage <- left_join(selected_wqp_data, coverageInfo, by="COMID")
      
      # Lists for select inputs
      locationTypeNames <- as.list(levels(pull(selected_wqp_data_coverage, ResolvedMonitoringLocationTypeName)))
      
      key <- highlight_key(selected_wqp_data_coverage, group = "coverage")
      # covg <- ggplot(key) + geom_point(mapping = aes(x=date, y = TotDASqKM))
      
      # Secondary detail map with markers for site locations
      output$hucDetail <- renderLeaflet({
        # Bounds fit continental US
        hucDetailMap <- leaflet(key) %>% 
          addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Esri.WorldGrayCanvas",
                           options = providerTileOptions(updateWhenZooming=F, updateWhenIdle = T)) %>%
          addProviderTiles(providers$Esri.OceanBasemap, group = "Esri.OceanBasemap",
                           options = providerTileOptions(updateWhenZooming=F, updateWhenIdle = T)) %>%
          addProviderTiles(providers$CartoDB.DarkMatter, group = "DarkMatter (CartoDB)",
                           options = providerTileOptions(updateWhenZooming=F, updateWhenIdle = T)) %>%
          addProviderTiles(providers$Esri.WorldImagery, group = "Esri.WorldImagery",
                           options = providerTileOptions(updateWhenZooming=F, updateWhenIdle = T)) %>%
          addLayersControl(baseGroups = c("Esri.WorldGrayCanvas","Esri.OceanBasemap", "Esri.WorldImagery",
                                          "DarkMatter (CartoDB)"),
                           options = layersControlOptions(collapsed = TRUE, autoZIndex = T)) %>%
          addPolygons(data = selectedHucBound,
                      layerId = hucSelected,
                      group = "bounds",
                      #label = hucSelected,
                      color = "black",
                      fillOpacity = 0.1,
                      weight = 3) %>% 
          addCircleMarkers(group="markers", radius = 3, stroke = F, color="red", opacity=0.8, fillOpacity=0.2)
      })

      # Cluster selection option
      observeEvent(input$cluster, {
        if(!is.null(input$cluster)) {
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
        } else {
          print("nope")
        }
      })

      # Coverage plot
      output$coverage <- renderPlotly({
        covg <- plot_ly(key, x=~date, y=~TotDASqKM) %>% 
          add_markers(alpha = 0.5) %>%
          highlight("plotly_selected") %>%
          event_register("plotly_relayout") %>%
          # rangeslider() #%>% 
          toWebGL()
      })
      
      # observeEvent(event_data("plotly_relayout"), {
      #   d <- event_data("plotly_relayout")
      #   # unfortunately, the data structure emitted is different depending on
      #   # whether the relayout is triggered from the rangeslider or the plot
      #   xmin <- if (length(d[["xaxis.range[0]"]])) d[["xaxis.range[0]"]] else d[["xaxis.range"]][1]
      #   xmax <- if (length(d[["xaxis.range[1]"]])) d[["xaxis.range[1]"]] else d[["xaxis.range"]][2]
      #   if (is.null(xmin) || is.null(xmax)) return(NULL)
      # 
      #   # compute the y-range based on the new x-range
      #   idx <- with(key$data(), xmin <= date & date <= xmax)
      #   yrng <- extendrange(key$data()$TotDASqKM[idx])
      # 
      #   plotlyProxy("coverage", session) %>%
      #     plotlyProxyInvoke("relayout", list(yaxis = list(range = yrng)))
      # })
      # 
      # yRange <- range(key$data()$TotDASqKM, na.rm = TRUE)
      # observeEvent(event_data("plotly_doubleclick"), {
      #     plotlyProxy("coverage", session) %>%
      #     plotlyProxyInvoke("relayout", list(yaxis = list(range = yRange)))
      #
      # })
      
      output$timeSeries <- renderPlotly({
        siteValsPlot <- ggplot(key) + 
                        geom_point(mapping = aes(x=date_time, y=harmonized_value, color=harmonized_parameter)) + 
                        labs(x="Date")#,y="Chlorophyll - ug/L")
        ggplotly(siteValsPlot, dynamicTicks = TRUE)
      })
    }
  })
  
  # Back button
  observeEvent(input$back, {
    # key <- NULL
    # print(paste("Is key null:", is.null(key)))
    # updateCheckboxInput(session, "cluster", T)
    output$zoomedIn <- NULL
    # output$timeSeries <- NULL
    # output$hucDetail <- NULL
    # output$coverage <- NULL
    # selectedHucBound <<- NULL
  })
}