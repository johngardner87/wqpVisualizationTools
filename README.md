# WQP_VizTool
```
logo
```
Developed by a team of data scientists and environmental scientists at Duke University, WQP_VizTool is a visualization tool for the US national water quality data. Based upon R and R Shiny, it provides a user-friendly interface to **select**, **filter**, **download** and **explore the pattern** of water quality and hydrology data, by linking it to the [Water Quality Portal](https://www.waterqualitydata.us/). 

---
## Motivation
WQP_VizTool was created to assist ecologists in assessing data coverage on a national scale. Areas with well-covered data, both temporally and spatially, can be identified as of potential interest to be studied. Coverage is also crucial for validating the satellite remote sensing data, which is useful for estimating water quality metrics in areas without field measurements. 

Besides the academia, the VizTool also see a potential use by riverkeepers, government officials, fishery managers or the general public. 

## Installation
- Install necessary libraries in R
```
install.packages("tidyverse", "feather", "shiny", "plotly", "leaflet", "sf", "rmapshaper", "tmap", "shinycssloaders", "shinyalert")
```
- Downloading the package `(and necessary data? )` `or are we including data in the package?`
- Setting the working directory in R
```
setwd("~/your_working_directory")
```

## Databases used in this visualization tool
Data used in WQP-VizTool are chemical and physical measurements held in publicly accessible government databases. Three databases are used as main sources: the [Water Quality Portal (WQP)](https://www.waterqualitydata.us/); the [National Hydrography Dataset (NHD)](https://www.usgs.gov/core-science-systems/ngp/national-hydrography/national-hydrography-dataset?qt-science_support_page_related_con=0#qt-science_support_page_related_con); and the [Watershed Boundary Dataset (WBD)](https://www.usgs.gov/core-science-systems/ngp/national-hydrography/watershed-boundary-dataset?qt-science_support_page_related_con=4#qt-science_support_page_related_con). 

Maintained by the USGS and EPA, WQP has 265 million results from over 2.2 million locations collected by hundreds of government and non-government agencies. 
Harmonized data for total suspended solids (tss), chlorophyll-a (chl-a), dissolved organic carbon (doc), and water turpidity (secchi) were imported from WQP 
The WQP field sites will be networked to waterbodies using the National Hydrography
Dataset ( NHD) developed by the USGS. This geospatial database includes all rivers and
waterbodies within the US as well as watershed boundaries of different sizes.
We will focus on subset of measurements that are widespread pollutants (e.g. nutrients, salts, algae), detectable by NASA satellites, and/or have important ecological implications.

### Harmonized Data

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

## Copyright and licensing information
## Contact information for the programmer
## Known bugs
## Credits and acknowledgments
`cite the proposal`
`give the names for the team`
