# WQP_VizTool
```
logo
```
WQP_VizTool is a visualization tool for the US national water quality data. Based upon R and R Shiny, it provides a user-friendly interface to select, sort, download and explore the patterns of water quality and hydrology data, by linking it to the [Water Quality Portal](https://www.waterqualitydata.us/) sponsored by the United States Geological Survey (USGS), the Environmental Protection Agency (EPA), and the National Water Quality Monitoring Council (NWQMC). 

## Motivation


## Installation
- Install necessary libraries in R
```
install.packages("tidyverse", "feather", "shiny", "plotly", "leaflet", "sf", "rmapshaper", "tmap", "shinycssloaders", "shinyalert")
```
- Downloading the package `(and necessary data? )` `or are we including data in the package?`
- Setting the working directory
```
setwd("~/your_working_directory")
```

## Databases used in this visualization tool
In Water Quality Data Visualization Tool, three databases are used as main sources: the Water Quality Portal (WQP); the National Hydrography Dataset (NHD); and the Watershed Boundary Dataset (WBD). 

The Water Quality Portal (WQP) is a cooperative service sponsored by the United States Geological Survey (USGS), the Environmental Protection Agency (EPA), and the National Water Quality Monitoring Council (NWQMC). It serves data collected by over 400 states, federal, tribal, and local agencies.

The datasets are credited to NSGS and EPA. 




## Harmonized Data

Why do I have to add the Organization ID in front of my STORET site id when searching?

The site ids in the STORET and NWIS systems have not been harmonized (unlike the case with characteristics). Therefore, a site id may be duplicated across the two systems. Furthermore, the site id within STORET is unable to serve as a unique identifier for a site because STORET aggregates data from different organizations who have not harmonized their identifiers. Because of these reasons, the WQP has chosen to prefix the simple site id in order to make it a suitable unique identifier. 

## Features
- Shows harmonized data from WQP
- Filters by time, location, waterbody type, watersheds, constituents, etc. 
- Shows the temporal and spatial coverage of water quality data
- Summarizes the value of the constituent and demonstrates its trends in time series
- ...

## How to use
The first page of the visualization tool consists mainly of a map of the United States and a panel for selection. Users can select the level of [Hydrologic Unit Code](https://en.wikipedia.org/wiki/Hydrological_code) and the water quality data constituent to display on the map. 


```
need a screenshot
```
Due to the size of HUC10 and HUC12 areas, the selection of HUC codes is limited to levels below HUC8. One can use the visualization tools on the second page to sort out the data for a certain HUC10 or 12 only. 

After selecting the HUC level and constituent to show, users can see a map split by HUC level, and colored according to the number of measurement data in that region. The darkness of the region indicates the number of measurement data that has been recorded in this HUC area. When the cursor hovers over the region, details are shown about the name, code and number of observations of that HUC area. 








## Contributing
Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

## License
[MIT](https://choosealicense.com/licenses/mit/)