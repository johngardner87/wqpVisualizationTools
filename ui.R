
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
  
  titlePanel("Water Quality Portal"),
  
  # Map Output
  leafletOutput(outputId = "wqpMap"),
  
  selectInput(inputId = "hucInput",
              label = "HUC Level",
              choices = c(2, 4, 6, 8, 10, 12)),
  
  selectInput(inputId = "constInput",
              label = "Constituent",
              choices = c("Chlorophyll" = "chlorophyll", "Dissolved Oxygen Concentration (DOC)" = "doc", "Turbidity (secchi)" = "secchi", "Total Suspended Solids (tss)" = "tss"),
              multiple = T,
              selected = "chlorophyll")
  # fluidRow(
  #   
  #   column(4,
  #     
  #     
  #     
  #   ),
  #   
  # )
  
)