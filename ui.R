
# Packages ----------------------------------------------------------------
library(tidyverse)
library(shiny)
library(plotly)
library(leaflet)
library(sf)
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
  
  absolutePanel(id = "inputs", class = "inputs", fixed = T, draggable = TRUE, top = 400, left = "auto", right = 175, 
                bottom = "auto", width = 200, height = "auto",
                
                h2("Water Quality Portal"),
                
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
                            selected = "All"),
                
                conditionalPanel(condition = "output.select",
                                actionButton("zoom", "Show me more!", style = "margin-bottom: 20px;")
                )
  ),
  # conditionalPanel(condition = "output.showCoverage", 
  #   absolutePanel(id = "coveragePlot", class = "inputs", draggable = T, top = "auto", left = 40, right = "auto",
  #                 bottom = 200, width = 400, height = "auto", 
  #                 
  #                 # h4(paste0("Coverage in the", output$selectedHUCName, ":")),
  #                 h4(textOutput("selectedHUCName")),
  #                 
  #                 plotOutput("coverage1") #%>% withSpinner(type=2, color.background="white")
  #   )
  # ),
  absolutePanel(id = "coveragePlot", class = "coveragePlot", draggable = T, top = 325, left = 40, right = "auto",
                bottom = "auto", width = "auto", height = "auto",
                h4(textOutput("selectedHUCName")),
                conditionalPanel(condition = "output.showCoverage", style = "margin-bottom: 20px;",
                                 plotOutput("coverage1"), #%>% withSpinner(type=2, color.background="white")
                                 # plotlyOutput("coverage1"), #%>% withSpinner(type=2, color.background="white")
                                 selectInput("covgAxis", "Y-Axis:", 
                                             choices = c("Upstream Catchment Area" = "catchment", "Distance to network mouth" = "Pathlength"))
                                 )
  ),
  uiOutput("zoomedIn")
)