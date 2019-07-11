
# Packages ----------------------------------------------------------------
library(tidyverse)
library(shiny)
library(plotly)
library(leaflet)
library(sf)
library(rmapshaper)
library(shinyalert)

# UI Layout ---------------------------------------------------------------

fluidPage(
  
  class="hucMap",
  useShinyalert(),
  tags$head(
    includeCSS("styles.css")
  ),
  
  # Inspired from https://github.com/akl21/hbef
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
  # leafletOutput(outputId = "wqpMap", height = "400px", width="800px"),
  
  absolutePanel(id = "inputs", fixed = T, draggable = TRUE, top = 450, left = "auto", right = 40, bottom = "auto",
                width = 200, height = "auto",
                
                h2("Water Quality Portal"),
                
                selectInput(inputId = "hucInput",
                            label = "HUC Level",
                            # choices = c(2, 4, 6, 8, 10, 12)),
                            choices = c(2, 4, 6, 8)),
                
                selectInput(inputId = "constInput",
                            label = "Constituent",
                            choices = c("All", 
                                        "Chlorophyll" = "chlorophyll", 
                                        "Dissolved Organic Carbon (doc)" = "doc", 
                                        "Turbidity (secchi)" = "secchi", 
                                        "Total Suspended Solids (tss)" = "tss"),
                            multiple = F,
                            selected = "All"),
                
                conditionalPanel(condition = "output.select",
                                actionButton("zoom", "Show me more!")
                )
  ),
  
  uiOutput("zoomedIn")
)