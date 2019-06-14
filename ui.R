
# Packages ----------------------------------------------------------------
library(tidyverse)
library(shiny)
library(plotly)
library(leaflet)
library(sf)
library(rmapshaper)
library(tmap)

# UI Layout ---------------------------------------------------------------

fluidPage(
  
  # titlePanel("Water Quality Portal"),
  class="outer",
  
  tags$head(
    includeCSS("styles.css")
  ),
  
  # Map Output
  leafletOutput(outputId = "wqpMap", height = "100%", width="100%"),
  
  absolutePanel(id = "Inputs", fixed = TRUE, draggable = TRUE, top = 450, left = "auto", right = 40, bottom = "auto",
                width = 200, height = "auto", class = "pan",
                
                h2("Water Quality Portal"),
                
                selectInput(inputId = "hucInput",
                            label = "HUC Level",
                            choices = c(2, 4, 6, 8, 10, 12)),
                
                selectInput(inputId = "constInput",
                            label = "Constituent",
                            choices = c("All", 
                                        "Chlorophyll" = "chlorophyll", 
                                        "Dissolved Oxygen Concentration (DOC)" = "doc", 
                                        "Turbidity (secchi)" = "secchi", 
                                        "Total Suspended Solids (tss)" = "tss"),
                            multiple = F,
                            selected = "All"),
                
                conditionalPanel(condition = "output.select",
                                actionButton("zoom", "Show me more!")
                )
  )
  
  # conditionalPanel(condition = "output.show == 'show'",
  #   leafletOutput(outputId = "testMap")
  # )
)