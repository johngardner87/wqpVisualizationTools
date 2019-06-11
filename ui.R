
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
  
  selectInput(inputId = "hucInput",
              label = "HUC Level",
              choices = c(2, 4, 8, 10, 12)),
  
  # Map Output
  leafletOutput(outputId = "wqpMap")
  
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