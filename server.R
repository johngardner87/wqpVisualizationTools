
# Packages ----------------------------------------------------------------
library(tidyverse)
library(feather)
library(shiny)
library(plotly)
library(leaflet)
library(sf)
library(shinycssloaders)
library(shinyalert)
library(crosstalk)
library(DBI)
library(RSQLite)

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

getUniquePoints <- function(sf_data) {
  # if(any(is.na(st_dimension(sf_data)))) {return(sf_data)}
  if(nrow(sf_data) == 0) {return(sf_data)}
  coords <- as.data.frame(st_coordinates(sf_data))
  sf_xy <- mutate(sf_data, X = pull(coords, X), Y = pull(coords, Y)) %>% st_set_geometry(NULL)
  unique_points <- st_as_sf(sf_xy %>% distinct(X, Y, .keep_all=T), coords = c("X", "Y"), crs = st_crs(sf_data))
  return(unique_points)
}

getBins <- function(hucLevel) {
  if(hucLevel == 2) {
    return(c(0, 1000, 10000, 100000, 250000, 500000, 1000000, 2000000, Inf))
  } else {
    return(c(0, 10, 50, 1000, 2000, 5000, 20000, 100000, Inf))
  }
}

getConstChoices <- function(constSelected) {
  choices <- c("All", 
               "Chlorophyll" = "chlorophyll", 
               "Dissolved Organic Carbon (doc)" = "doc", 
               "Turbidity (secchi)" = "secchi", 
               "Total Suspended Solids (tss)" = "tss")
  if (constSelected == "All") {
    return (choices)
  } else {
    return (choices[choices == constSelected])
  }
}

#sliderInput("selectDates", "Filter by date:", min = firstDate, max = lastDate, 
#value = c(firstDate, lastDate), width = "95%"),

# sliderInput2 <- function(inputId, label, min, max, value, widthCSS, min_range, max_range){
#   x <- sliderInput(inputId, label, min, max, value, width = widthCSS)
#   x$children[[2]]$attribs <- c(x$children[[2]]$attribs, 
#                                "data-min-interval" = min_range, 
#                                "data-max-interval" = max_range)
#   x
# }

function(input, output, session) {
  
  # Main map ---------------------------------------------------------------
  
  output$select <- reactive({F})
  output$showCoverage <- reactive({F})
  
  output$wqpMap <- renderLeaflet({
    # Bounds fit continental US
    leaflet() %>% 
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri.WorldTopoMap") %>%
      addProviderTiles(providers$Esri.OceanBasemap, group = "Esri.OceanBasemap") %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Esri.WorldGrayCanvas") %>% 
      # addProviderTiles(providers$CartoDB.DarkMatter, group = "DarkMatter (CartoDB)") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri.WorldImagery") %>%
      # options = providerTileOptions(updateWhenZooming=F, updateWhenIdle = T)) %>%
      addLayersControl(baseGroups = c("Esri.WorldTopoMap", "Esri.OceanBasemap", "Esri.WorldGrayCanvas", 
                                      "Esri.WorldImagery"), # , "DarkMatter (CartoDB)"),
                       options = layersControlOptions(collapsed = TRUE, autoZIndex = T)) %>%
      fitBounds(-125, 25, -75, 55) 
  })
  
  output$selectedHUCName <- renderText("Select a watershed...")
  
  palette = "Blues" #Any pallette, I like YlOrRd or Blues
  
  # Variables used by throughout
  hucSelected <- 0
  hucLevel <- -1
  selectedHucBound <- NULL
  filtered_wqp_data_coverage <- NULL
  filtered_unique <- NULL
  # key <- NULL
  
  hucRegions <- tibble("02" = "",
                       "04" = "Subregion",
                       "06" = "Basin",
                       "08" = "Subbasin",
                       "10" = "Watershed",
                       "12" = "Subwatershed")
  
  # options(opacityDim = 0, persistent = F, selected = attrs_selected(fill="toself", fillcolor = "green"))
  options(opacityDim = 0)
  outputOptions(output, "select", suspendWhenHidden = FALSE, priority = 1)
  outputOptions(output, "showCoverage", suspendWhenHidden = FALSE, priority = 1)

  observe({
    hucLevel <<- input$hucInput
    # hucLevelUp <- as.numeric(hucLevel) - 2
    hucLevelUp <- 2
    hucColumn <<- paste("HUC", hucLevel, sep="")
    constituent <<- input$constInput
    constCol <<- paste(constituent, "MeasCount", sep="")
    
    # Load boundary data for selected HUC and HUC2 boundaries for context
    boundaries <<- paste("WBDHU", hucLevel, "Counts", ".gpkg", sep = "") %>% 
      paste("Datasets/WBD_Simplified/", ., sep="") %>% 
      st_read()
    
    if (hucLevel > 2) {
      upperBoundaries <- paste("WBDHU", hucLevelUp, "Counts", ".gpkg", sep = "") %>% 
        paste("Datasets/WBD_Simplified/", ., sep="") %>% 
        st_read()
    }
    
    bins <- getBins(hucLevel)
    
    # Clearing old shapes and adding map panes for proper layering of UI
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
      
      pal <<- colorBin(palette, domain=boundaries[["AllMeasCount"]], bins=bins)
      # pal <- colorQuantile(palette, domain=allCounts, n = 6)
      
      hucMap %>%
        addPolygons(fillColor = ~pal(boundaries[["AllMeasCount"]]), #topo.colors(10)
                    color = "black",
                    weight = 0.5,
                    opacity = 1,
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                      weight = 2,
                      color = "black",
                      fillOpacity = 0,
                      bringToFront = TRUE,
                      sendToBack = TRUE),
                    label = paste(boundaries$NAME, ": ", boundaries$AllMeasCount, " total measurements", sep=""),
                    
                    layerId = boundaries[[hucColumn]],
                    
                    labelOptions = labelOptions(
                      textsize = "12px"
                    ),
                    options = pathOptions(pane = "main")
        ) %>% addLegend("bottomleft", pal, values = boundaries[["AllMeasCount"]], layerId = "legend")
    } else {
      pal <<- colorBin(palette, domain=boundaries[[constCol]], bins=bins)
      
      hucMap %>%
        addPolygons(fillColor = ~pal(boundaries[[constCol]]), #topo.colors(10)
                    color = "black",
                    weight = 0.5,
                    opacity = 1,
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                      weight = 2,
                      color = "black",
                      fillOpacity = 0.2,
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
    
    if (nchar(hucSelected) != as.numeric(hucLevel)) {          # If the user changes the HUC
      print("setting to false")
      print(paste("hucSelected length", nchar(hucSelected)))
      print(paste("hucLevel", as.numeric(hucLevel)))
      output$select <- reactive({F})
    } else {                                                   # If the user changed the constituent, has to redraw selection
      selectedPoly <- leafletProxy("wqpMap", data=selectedHucBound) %>%
        removeShape(paste(hucSelected, "Selected", sep=""))
      print("non-selected poly removed")
      if(constituent == "All") {
        selectedPoly %>%
          addPolygons(layerId = paste(selectedHucBound[[hucColumn]], "Selected", sep=""),
                      fillColor = ~pal(boundaries[["AllMeasCount"]]),
                      color = "red",
                      weight = 3,
                      opacity = 1,
                      label = paste(selectedHucBound$NAME, ": ", selectedHucBound$AllMeasCount, " total measurements",
                                    sep=""),
                      labelOptions = labelOptions(
                        textsize = "12px"
                      ),
                      highlight = highlightOptions(
                        fillOpacity = 0,
                        bringToFront = TRUE,
                        sendToBack = TRUE
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
                      highlight = highlightOptions(
                        fillOpacity = 0,
                        bringToFront = TRUE,
                        sendToBack = TRUE
                      ),
                      options = pathOptions(pane = "selections")
          )
      }
      output$select <- reactive({T})
    }
  })
  
  observe({
    click <- input$wqpMap_shape_click
    if(is.null(click)) {return()}
    print("CLICK REGISTERED --------------")
    print(click$id)
  })
  
  # HUC selection (redraws polygon with red boundary, behaves differently depending on current selection)
  observeEvent(input$wqpMap_shape_click, {
    
    print("NEW EVENT START -------------------------------------")
    start <- Sys.time()
    event <- input$wqpMap_shape_click
    
    if(is.null(event$id)) {                                           # If the user clicks on a boundary
      print("boundary selected")
      print(paste("hucSelected length", nchar(hucSelected)))
      print(paste("hucLevel", as.numeric(hucLevel)))
    } else if (event$id != paste(hucSelected, "Selected", sep="")) {  # If the user clicks on a new HUC
      oldHuc <- hucSelected
      hucSelected <<- event$id
      selectedHucBound <<- boundaries %>% 
        filter(.data[[hucColumn]] == hucSelected)
      output$select <- reactive({T})
      output$showCoverage <- reactive({T})
      print("select: T")
      print("selected  huc loaded")
      selectedPoly <- leafletProxy("wqpMap", data=selectedHucBound) %>%
        removeShape(paste(oldHuc, "Selected", sep=""))
      print("non-selected poly removed")
      if(constituent == "All") {
        selectedPoly %>%
          addPolygons(layerId = paste(selectedHucBound[[hucColumn]], "Selected", sep=""),
                      fillColor = ~pal(boundaries[["AllMeasCount"]]),
                      color = "red",
                      weight = 3,
                      opacity = 1,
                      label = paste(selectedHucBound$NAME, ": ", selectedHucBound$AllMeasCount, " total measurements",
                                    sep=""),
                      labelOptions = labelOptions(
                        textsize = "12px"
                      ),
                      highlight = highlightOptions(
                        fillOpacity = 0,
                        bringToFront = TRUE,
                        sendToBack = TRUE
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
                      highlight = highlightOptions(
                        fillOpacity = 0,
                        bringToFront = TRUE,
                        sendToBack = TRUE
                      ),
                      options = pathOptions(pane = "selections")
          )
      }
      print(paste("you've selected: ", hucSelected, sep=""))
      output$selectedHUCName <- renderText(paste(selectedHucBound$NAME, hucRegions[sprintf("%02s", str_length(hucSelected))]))
      print(Sys.time() - start)
      
      if(!is.null(selectedHucBound[[constCol]]) && selectedHucBound[[constCol]] != 0) {
        
        # -------------- LOADING IN WQP AND NHD DATA
        # Loading relevant wqp data and flowline data
        # wqp_path <- sprintf(
        #   "Datasets/wqp_Constituents/GPKGs/wqp_%s_indexed.gpkg", input$constInput)
        # wqp_query <- sprintf(
        #   "SELECT * FROM wqp_%s_indexed WHERE HUCEightDigitCode LIKE '%s%%'", input$constInput, hucSelected)
        # selected_wqp_data <- st_read(wqp_path, query=wqp_query)
        wqp_path <- sprintf(
          "Datasets/wqp_Constituents/wqp_%s_indexed.sqlite", input$constInput)
        db <- dbConnect(SQLite(), wqp_path)
        wqp_query <- sprintf(
          "SELECT * FROM %s_data WHERE HUCEightDigitCode LIKE '%s%%'", input$constInput, hucSelected)
        selected_wqp_data_coverage <<- tbl(db, sql(wqp_query)) %>%
          as_tibble() %>%
          mutate(date_time = as.POSIXct(date_time, format = "%F %T")) %>%
          mutate(date = as.POSIXct(date, format = "%F")) %>%
          st_as_sf(coords = c("X", "Y"), crs=4269)
        
        coverage_properRange <- selected_wqp_data_coverage %>% 
          filter(date_time > as.Date("1930-01-01") & date_time < as.Date("2018-04-23")) %>% 
          filter(TotDASqKM > 0)
        
        # ------------- END LOADING IN WQP AND NHD DATA
        
        output$coverage1 <- renderPlot({
          if (input$covgAxis == "catchment") {
            ggplot(coverage_properRange, aes(x = date, y = TotDASqKM)) +
              geom_point(aes(color = harmonized_parameter)) + scale_y_log10() + 
              theme(legend.position = "bottom", legend.title=element_blank()) + labs(x = "Date", y = "Upstream Catchment Area")
          } else {
            ggplot(coverage_properRange, aes(x = date, y = Pathlength)) +
              geom_point(aes(color = harmonized_parameter)) + 
              theme(legend.position = "bottom", legend.title=element_blank()) + labs(x = "Date", y = "Distance to outlet")
          }
        })
        
        # output$coverage1 <- renderPlotly({
        #   if (input$covgAxis == "catchment") {
        #     covg <- plot_ly(coverage_properRange, x=~date, y=~TotDASqKM, text=~MonitoringLocationName) %>%
        #       add_markers(color=~harmonized_parameter) %>%
        #       layout(xaxis=list(title = "Date"), yaxis=list(title="Upstream Catchment Area", type = "log")) %>%
        #       toWebGL()
        #   } else {
        #     covg <- plot_ly(coverage_properRange, x=~date, y=~Pathlength, text=~MonitoringLocationName) %>%
        #       add_markers(color=~harmonized_parameter) %>%
        #       layout(xaxis=list(title = "Date"), yaxis=list(title="Distance to outlet")) %>%
        #       highlight("plotly_selected", off = "plotly_deselect") %>%
        #       toWebGL()
        #   }
        #   covg
        # })
      } else {
        output$coverage1 <- NULL
      }
    } else {                                                          # If the user clicks on the selected HUC
      print(paste("removed:", event$id))
      leafletProxy("wqpMap") %>% removeShape(event$id)
      output$select <- reactive({F})
      output$showCoverage <- reactive({F})
      output$selectedHUCName <- renderText("Select a watershed...")
      print("select: F")
    }
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
      #selectDates tolower(input$restrict)

      # jsSlider <- paste0("$(function() {
      #                     setTimeout(function(){ 
      #                     if (", "true",")
      #                    {$('#selectDates').data('ionRangeSlider').update({'data-min-interval':7, 'data-max-interval':7})}
      #                     else
      #                     {$('#selectDates').data('ionRangeSlider').update({'data-min-interval':null, 'data-max-interval':null})}
      #                    }, 5)}")
      #"$('#selectDates').data('ionRangeSlider').update({'min_interval':7, 'max_interval':7})"
      
      output$zoomedIn <- renderUI({
        fluidPage(
          class = "details",
          ## Header ---------
          fluidRow(
            column(8,
                   tags$h1("Coverage in the ", strong(selectedHucBound$NAME), hucRegions[sprintf("%02s", str_length(hucSelected))])
                   ),
            column(4,
                   actionButton("back", "Take me back!", 
                                width = "100%", icon = icon("arrow-left"), style="width:100%, position:absolute; margin-top:28px")
                   )
          ),
          fluidRow(
            column(8,
                   tags$h3(paste0("HUC", str_length(hucSelected), ": "),
                           strong(hucSelected), " — ", prettyNum(selectedHucBound$AREASQKM, big.mark=","),
                           "sq. km ", getRegionName(hucSelected))
            ),
            column(4,
                   h3("Site Map")
            )
          ),
          ## Coverage and Map ---------
          fluidRow(
            column(8,
                   div(class="widget",
                       plotlyOutput("coverage")
                   )
            ),
            column(4,
                   div(class="widget",
                       leafletOutput(outputId = "hucDetail") %>% withSpinner(type=2, color.background="white")
                   )
            )
          ),
          fluidRow(
            column(8,
                   div(class = "widget",
                       h4(class = "sidebarTitles", "Measurement Site Selection"),
                       splitLayout(cellWidths = c("60%", "40%"),
                         sliderInput("selectDates", "Filter by date:", min = firstDate, max = lastDate, 
                                     value = c(firstDate, lastDate), width = "90%"),
                         sliderInput("selectCatchment", "Filter by upstream catchment area:", min = 0, max = highCatchment, 
                                     value = c(0, highCatchment), width = "100%")
                       ),
                       # sliderInput("selectDates", "Filter by date:", min = firstDate, max = lastDate, 
                       #             value = c(firstDate, lastDate), width = "95%"),
                       splitLayout(selectizeInput("selectLocationType", "Filter by site location type:",
                                                  choices=locationTypeNames, multiple=T, width = "100%", 
                                                  options = list(
                                                    placeholder = 'Choose a site location type',
                                                    onInitialize = I('function() { this.setValue(""); }'))),
                                   selectInput(inputId = "constInput2",
                                               label = "Filter by constituent:", width = "100%",
                                               choices = getConstChoices(input$constInput),
                                               multiple = F)
                                   ),
                       selectizeInput("selectStreamNames", "Filter by site location name:",
                                      choices=streamNames, multiple=T, width = "100%", 
                                      options = list(
                                        placeholder = 'Choose a site location'
                                      ))
                       )
            ),
            column(4,
                   div(class = "widget",
                       h4(class="sidebarTitles", "Map Settings"),
                       checkboxInput("cluster", "Cluster", F),
                       splitLayout(checkboxInput("showHUC10", "Show HUC10 Boundaries", F),
                                   checkboxInput("showHUC12", "Show HUC12 Boundaries", F)),
                       actionButton("showTimeSeries", "Show time series for selected points")
                   )
            )
          ),
          uiOutput("pageThree")
        )
      })
      
      # Old ui ---------
      # output$zoomedIn <- renderUI({
      #   fluidPage(
      #     class = "details",
      #     fluidRow(
      #       column(10,
      #              tags$h1("Coverage in the ", strong(selectedHucBound$NAME), hucRegions[sprintf("%02s", str_length(hucSelected))])
      #              ),
      #       column(2, 
      #              actionButton("back", "Take me back!", 
      #                           width = "100%", icon = icon("arrow-left"), style="width:100%, position:absolute; top:28px")
      #              )
      #     ),
      #     fluidRow(
      #       column(10,
      #              tags$h3(paste0("HUC", str_length(hucSelected), ": "),
      #                      strong(hucSelected), " — ", prettyNum(selectedHucBound$AREASQKM, big.mark=","),
      #                      "sq. km ", getRegionName(hucSelected))
      #       ),
      #       column(2, 
      #               h3("Filters")
      #       )
      #     ),
      #     fluidRow(
      #       column(5,
      #         div(class="widget",
      #             leafletOutput(outputId = "hucDetail") %>% withSpinner(type=2, color.background="white")
      #         )
      #       ),
      #       column(5,
      #        div(class="widget",
      #           plotlyOutput("coverage")
      #        )
      #       ),
      #       column(2,
      #         div(class = "widget",
      #           h4(class="sidebarTitles", "Global Settings"),
      #           selectizeInput("selectLocationType", "Filter by site location type:",
      #                          choices=locationTypeNames, multiple=T, width = "100%", 
      #                          options = list(
      #                            placeholder = 'Choose a site location type',
      #                            onInitialize = I('function() { this.setValue(""); }'))),
      #           selectizeInput("selectStreamNames", "Filter by site location name:",
      #                          choices=streamNames, multiple=T, width = "100%", 
      #                          options = list(
      #                            placeholder = 'Choose a site location'
      #                          )),
      #           # dateRangeInput("selectDates", "Filter by date:", start = firstDate, end = lastDate, startview = "decade"),
      #           sliderInput("selectDates", "Filter by date:", min = firstDate, max = lastDate, value = c(firstDate, lastDate)),
      #           h4(class="sidebarTitles", "Measurement Site Map"),
      #           checkboxInput("cluster", "Cluster", F),
      #           splitLayout(checkboxInput("showHUC10", "HUC10s", F),
      #                       checkboxInput("showHUC12", "HUC12s", F))
      #         )
      #       )
      #     ),
      #     fluidRow(
      #       column(10,
      #         div(class = "widget",
      #             tabsetPanel(type = "tabs", 
      #                         tabPanel("Time Series", plotlyOutput("timeSeries")), 
      #                         tabPanel("Histogram", plotlyOutput("histogram"))
      #                         # tabPanel("Annual Trend")
      #             )
      #         )
      #       ),
      #       column(2,
      #              div(class="widget",
      #                 h4(class = "sidebarTitles", "Measurement Plots"),
      #                 selectizeInput("timeSeriesLog", "Y-Axis: ", choices=c("Linear", "Log"), multiple = F, width = "100%"),
      #                 downloadButton("dl", "Download filtered data", style="width:100%")
      #                 # actionButton("plotUpdate", "Redraw plots!", icon=icon("sync"), width="100%")
      #                 # selectizeInput("selectConstituent", "Filter by constituent:", 
      #                 #                choices = c("Chlorophyll" = "chlorophyll", 
      #                 #                            "Dissolved Organic Carbon (doc)" = "doc", 
      #                 #                            "Turbidity (secchi)" = "secchi", 
      #                 #                            "Total Suspended Solids (tss)" = "tss"),
      #                 #                multiple=T, width="100%")
      #             )
      #       )
      #     )
      #   )
      # })
      
      # Operations for filtering -------
      
      # Lists and dates for select inputs
      locationTypeNames <- as.list(unique(pull(selected_wqp_data_coverage, ResolvedMonitoringLocationTypeName)))
      streamNames <- as.list(unique(pull(selected_wqp_data_coverage, MonitoringLocationName)))
      highCatchment <- max(pull(selected_wqp_data_coverage, TotDASqKM))
      
      selectedDates <- as.Date(pull(selected_wqp_data_coverage, date_time))
      #Filter out unwanted dates
      firstDate <- max(c(min(selectedDates, na.rm = T), as.Date("1930-01-01"))) #Earliest date or 1930, whichever is later
      lastDate <- min(c(max(selectedDates, na.rm = T), as.Date("2019-04-23"))) #Latest date or 2019, whichever is earlier
      
      filtered_wqp_data_coverage <<- reactive({
        print("updating")
        filtered <- selected_wqp_data_coverage
        if (!is.null(input$selectDates)) {
          filtered <- filter(selected_wqp_data_coverage,
                             as.Date(date_time) >= input$selectDates[1] & as.Date(date_time) <= input$selectDates[2])
        }
        if (!is.null(input$selectCatchment)) {
          filtered <- filter(filtered,
                             TotDASqKM >= input$selectCatchment[1] & TotDASqKM <= input$selectCatchment[2])
        }
        if (!is.null(input$selectLocationType)) {
          filtered <- filter(filtered, ResolvedMonitoringLocationTypeName %in% input$selectLocationType)
        }
        if (!is.null(input$selectStreamNames)) {
          filtered <- filter(filtered, MonitoringLocationName %in% input$selectStreamNames)
        }
        if (!is.null(input$constInput2) && input$constInput2 != "All") {
          if (input$constInput2 == "chlorophyll") {
            filtered <- filter(filtered, harmonized_parameter == "chl.a")
          } else {
            filtered <- filter(filtered, harmonized_parameter == input$constInput2)
          }
        }
        filtered
      }) %>% debounce(100)
      filtered_unique <<- reactive({
        filtered <- selected_wqp_data_coverage
        if (!is.null(input$selectDates)) {
          filtered <- filter(selected_wqp_data_coverage,
                             as.Date(date_time) >= input$selectDates[1] & as.Date(date_time) <= input$selectDates[2])
        }
        if (!is.null(input$selectCatchment)) {
          filtered <- filter(filtered,
                             TotDASqKM >= input$selectCatchment[1] & TotDASqKM <= input$selectCatchment[2])
        }
        if (!is.null(input$selectLocationType)) {
          filtered <- filter(filtered, ResolvedMonitoringLocationTypeName %in% input$selectLocationType)
        }
        if (!is.null(input$selectStreamNames)) {
          filtered <- filter(filtered, MonitoringLocationName %in% input$selectStreamNames)
        }
        if (!is.null(input$constInput2) && input$constInput2 != "All") {
          if (input$constInput2 == "chlorophyll") {
            filtered <- filter(filtered, harmonized_parameter == "chl.a")
          } else {
            filtered <- filter(filtered, harmonized_parameter == input$constInput2)
          }
        }
        getUniquePoints(filtered)
      }) %>% debounce(100)

      # key <- highlight_key(filtered_wqp_data_coverage, group = "coverage")
      # map_key <<- SharedData$new(filtered_unique, group = "coverage", key=~SiteID)
      # key <<- SharedData$new(filtered_wqp_data_coverage, group = "coverage", key=~SiteID)

      # Secondary detail map with markers for site locations
      output$hucDetail <- renderLeaflet({
        print("Map being drawn")
        # Bounds fit continental US
        hucDetailMap <- leaflet() %>% 
          addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri.WorldTopoMap", 
                           options = providerTileOptions(updateWhenZooming=F, updateWhenIdle = T)) %>%
          addProviderTiles(providers$Esri.OceanBasemap, group = "Esri.OceanBasemap",
                           options = providerTileOptions(updateWhenZooming=F, updateWhenIdle = T)) %>%
          addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Esri.WorldGrayCanvas",
                           options = providerTileOptions(updateWhenZooming=F, updateWhenIdle = T)) %>%
          # addProviderTiles(providers$CartoDB.DarkMatter, group = "DarkMatter (CartoDB)",
          #                  options = providerTileOptions(updateWhenZooming=F, updateWhenIdle = T)) %>%
          addProviderTiles(providers$Esri.WorldImagery, group = "Esri.WorldImagery",
                           options = providerTileOptions(updateWhenZooming=F, updateWhenIdle = T)) %>%
          addLayersControl(baseGroups = c("Esri.WorldTopoMap", "Esri.OceanBasemap", "Esri.WorldGrayCanvas", 
                                          "Esri.WorldImagery"), #, "DarkMatter (CartoDB)"),
                           options = layersControlOptions(collapsed = TRUE, autoZIndex = T)) %>%
          addMapPane("bounds", zIndex = 410) %>% 
          addMapPane("points", zIndex = 420) %>% 
          addPolygons(data = selectedHucBound,
                      layerId = hucSelected,
                      group = "bounds",
                      options = pathOptions(pane = "bounds"),
                      #label = hucSelected,
                      color = "black",
                      fillOpacity = 0.1,
                      weight = 3)
          
      })

      leafletProxy("hucDetail", data=filtered_unique()) %>%
        addCircleMarkers(radius = 3,
                         stroke = F,
                         color = "red",
                         # opacity = 0.8,
                         # fillOpacity = 0.2,
                         opacity = 1,
                         fillOpacity = 1,
                         group = "markers",
                         label = ~MonitoringLocationName)

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
      
      output$coverage <- renderPlotly({
        covg <- plot_ly(filtered_wqp_data_coverage(), x=~date, y=~TotDASqKM, text=~MonitoringLocationName) %>% 
          add_markers(color=~harmonized_parameter) %>%
          layout(xaxis=list(title = "Date"), yaxis=list(title="Upstream Catchment Area", type = "log"), showlegend = T) %>% 
          highlight("plotly_selected", off = "plotly_deselect") %>%
          event_register("plotly_relayout") %>%
          # rangeslider() #%>% 
          toWebGL()
      })
      
      output$timeSeries <- renderPlotly({
        if(input$timeSeriesLog == "Log") {
          timeSeries <- plot_ly(filtered_wqp_data_coverage(), x=~date, y=~harmonized_value) %>%
            add_markers(color=~factor(harmonized_parameter)) %>%
            layout(xaxis=list(title="Date"), yaxis=list(title="Harmonized Unit", type="log"), showlegend = T) %>%
            highlight("plotly_selected", off = "plotly_deselect", selected=attrs_selected(showlegend=T))
        } else {
          timeSeries <- plot_ly(filtered_wqp_data_coverage(), x=~date, y=~harmonized_value) %>%
            add_markers(color=~factor(harmonized_parameter)) %>%
            layout(xaxis=list(title="Date"), yaxis=list(title="Harmonized Unit"), showlegend = T) %>%
            highlight("plotly_selected", off = "plotly_deselect", selected=attrs_selected(showlegend=T))
        }
      })

      # output$histogram <- renderPlotly({
      #   histogram <- plot_ly(key, x=~harmonized_value) %>% 
      #     add_histogram(color=~harmonized_parameter, xbins = list(start = 0, size = 10)) %>% 
      #     layout(xaxis=list(title="Measurement Value"), yaxis=list(title="Measurement Count"), showlegend = T)
      # })
      
      output$dl <- downloadHandler(
        filename = function() {
          paste0(format(Sys.Date(), "%Y_%m_%d"), "_HUC", hucSelected, "_filtered.csv")
        },
        content = function(file) {
          data <- filtered_wqp_data_coverage() %>% cbind(., st_coordinates(.)) %>% 
            rename(LongitudeMeasure = X, LatitudeMeasure = Y) %>% st_set_geometry(NULL)
          write.csv(data, file, row.names = F)
        }
      )
    }
  })
  
  # Drawing of points based on selection options
  observeEvent(c(input$cluster, input$selectLocationType, input$selectDates, input$selectCatchment, input$selectStreamNames, input$constInput2), {
    
    #Fix for crosstalk / leaflet issue when filter selects zero points
    req(nrow(filtered_unique()) != 0)
    
    # Fix for crosstalk / leaflet attempts to filter after a plot-based selection was made
    # Doesn't actually work
    # key$clearSelection()
    # map_key$clearSelection("hucDetail")
    
    if(!is.null(input$cluster) && input$cluster) {
      markers <- leafletProxy("hucDetail", data = filtered_unique())
      clearGroup(markers, group="markers")
      markers %>%
        addCircleMarkers(
          stroke = F,
          # color = "black",
          clusterOptions = markerClusterOptions(),
          options = pathOptions(pane = "points"),
          # layerId = ~SiteID,
          group = "markers",
          label = ~MonitoringLocationName)
    } else {
      markers <- leafletProxy("hucDetail", data = filtered_unique())
      clearGroup(markers, group="markers")
      markers %>% addCircleMarkers(radius = 3,
                                   stroke = F,
                                   color = "red",
                                   options = pathOptions(pane = "points"),
                                   # opacity = 0.8,
                                   # fillOpacity = 0.2,
                                   opacity = 1,
                                   fillOpacity = 1,
                                   group = "markers",
                                   label = ~MonitoringLocationName)
    }
  })
  
  observeEvent(input$showHUC10, {
    if(is.null(input$showHUC10)) {return()}
    if(input$showHUC10) {
      bounds <- st_read("Datasets/WBD_Simplified/WBDHU10.gpkg", query=sprintf("SELECT * FROM WBDHU10 WHERE HUC10 LIKE '%s%%'", hucSelected))
      leafletProxy("hucDetail", data = bounds) %>% 
        addPolygons(group = "huc10s", color = "black", fillOpacity = 0.1, weight = 0.5, options = pathOptions(pane = "bounds"))
    } else {
      leafletProxy("hucDetail") %>% clearGroup(group="huc10s")
    }
  })
  
  observeEvent(input$showHUC12, {
    if(is.null(input$showHUC12)) {return()}
    if(input$showHUC12) {
      bounds <- st_read("Datasets/WBD_Simplified/WBDHU12.gpkg", query=sprintf("SELECT * FROM simplified_WBDHU12 WHERE HUC12 LIKE '%s%%'", hucSelected))
      leafletProxy("hucDetail", data = bounds) %>% 
        addPolygons(group = "huc12s", color = "black", fillOpacity = 0.1, weight = 0.25, options = pathOptions(pane = "bounds"))
    } else {
      leafletProxy("hucDetail") %>% clearGroup(group="huc12s")
    }
  })
  
  # Page 3 - Time Series -----------
  observeEvent(input$showTimeSeries, {
    output$pageThree <- renderUI({
      fluidPage(
        class = "details",
        fluidRow(
          column(8, h1("Time Series")),
          column(4, actionButton("back2", "Take me back!", 
                                 width = "100%", icon = icon("arrow-left"), style="width:100%, position:absolute; margin-top:28px"))
        ),
        fluidRow(
          column(12, plotlyOutput("timeSeries"))
        ),
        fluidRow(
          selectizeInput("timeSeriesLog", "Y-Axis: ", choices=c("Linear", "Log"), multiple = F, width = "100%"),
          downloadButton("dl", "Download filtered data", style="width:100%")
        )
      )
    })
  })
  
  # Back button
  observeEvent(input$back, {
    leafletProxy("hucDetail") %>% clearGroup(group="markers")
    filtered_wqp_data_coverage <<- NULL
    filtered_unique <<- NULL
    # key <<- NULL
    # map_key <<- NULL
    
    # output$coverage1 <- NULL
    # output$selectedHUCName <- renderText("Select a watershed...")
    # 
    # output$timeSeries <- NULL
    output$hucDetail <- NULL
    output$coverage <- NULL
    output$zoomedIn <- NULL
    # selectedHucBound <<- NULL
  })
  
  observeEvent(input$back2, {
    output$pageThree <- NULL
  })
}