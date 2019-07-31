
# Packages ----------------------------------------------------------------
library(tidyverse)
library(shiny)
library(plotly)
library(leaflet)
library(sf)
library(shinyalert)
library(shinyjs)
library(shinybusy)

# UI Layout ---------------------------------------------------------------

fluidPage(
  
  class="hucMap",
  useShinyalert(),
  add_busy_bar(color = "#012169"),
  
  useShinyjs(),
  extendShinyjs(text = "shinyjs.selectionReset = function() { Shiny.setInputValue('plotly_selected-selection', 'null'); }"),
  
  tags$head(
    includeCSS("styles.css")
  ),
  
  # Inspired by https://github.com/akl21/hbef
  # This is here and not the the css file because for some reason the default
  # styles for the range sliders overwrite the custom style sheet, not sure why
  tags$style(type = "text/css", "
    .irs-bar {border-radius: 0;}
    .irs-bar-edge {border-radius: 0;}
    .irs-single, .irs-bar-edge, .irs-bar, .irs-from,  .irs-to {border-radius: 0;}
    .irs-slider.from, .irs-slider.to {border: 0px; width: 4px; height: 20px; border-radius: 0;}
    .irs-grid-pol.small {height: 0px;}
    .irs-grid-pol{height: 3px;}
  "),
  
  # Map Output
  leafletOutput(outputId = "wqpMap", height = "100%", width="100%"),
  
  # For whatever reason displaying output.select in this way fixes major bug 
  # when displaying pop-up coverage plot for second time
  # absolutePanel(h4(textOutput("select"))),
    
  absolutePanel(id = "inputs", class = "inputs", fixed = T, draggable = TRUE, top = 75, left = "auto", right = 10, 
                bottom = "auto", width = 400, height = "auto",
                
                h2("pondr"),
                
                splitLayout(cellWidths = c("25%", "75%"),
                  selectInput(inputId = "hucInput",
                              label = "HUC Level",
                              # choices = c(2, 4, 6, 8, 10, 12)),
                              choices = c(2, 4, 6, 8),
                              selected = 4),
                  
                  selectInput(inputId = "constInput",
                              label = "Constituent",
                              choices = c("All", 
                                          "Chlorophyll" = "chlorophyll", 
                                          "Dissolved Organic Carbon (doc)" = "doc", 
                                          "Turbidity (secchi)" = "secchi", 
                                          "Total Suspended Solids (tss)" = "tss"),
                              multiple = F,
                              selected = "All")
                ),
                
                div(class = "coveragePlot",
                    h4(textOutput("selectedHUCName")),
                    conditionalPanel(condition = "output.showCoverage", style = "margin-bottom: 10px;",
                                     plotOutput("coverage1", width = "auto"),
                                     selectInput("covgAxis", "Coverage Metric:", width = "auto",
                                                 choices = c("Upstream Catchment Area" = "catchment", "Distance to Outlet" = "Pathlength"))
                    )
                ),
                conditionalPanel(condition = "output.select",
                                 actionButton("zoom", "Show me more!", style = "margin-top:20px", width = "100%")
                )
  ),
  uiOutput("zoomedIn")
)