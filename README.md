# wqpVisualizationTools

Developed by a team of data scientists and ecologists at Duke University, wqpVisualizationTools is an RShiny based visualization tool for US national water quality data. The web app provides a user-friendly interface to **explore**, **filter**, and **download** water quality and hydrology data from the [Water Quality Portal](https://www.waterqualitydata.us), [Watershed Boundary Dataset](https://www.usgs.gov/core-science-systems/ngp/national-hydrography/watershed-boundary-dataset), and the [National Hydrography Dataset](https://www.usgs.gov/core-science-systems/ngp/national-hydrography).

---
<!-- ## Motivation and target user groups
WQP_VizTool was first created with the intention to assist ecologists in assessing data coverage on a national scale. Areas with well-covered data, both temporally and spatially, can be identified as of potential interest to be studied. Analysis of data coverage is also crucial for validating the satellite remote sensing data, which is useful for estimating water quality metrics in areas without field measurements. The VizTool also see potential uses by riverkeepers, government officials, fishery managers and the general public.

## Databases used in this visualization tool
Data used in WQP-VizTool are chemical and physical measurements held in publicly accessible government databases. Three databases are used as main sources: the [Water Quality Portal (WQP)](https://www.waterqualitydata.us/); the [National Hydrography Dataset (NHD)](https://www.usgs.gov/core-science-systems/ngp/national-hydrography/national-hydrography-dataset?qt-science_support_page_related_con=0#qt-science_support_page_related_con); and the [Watershed Boundary Dataset (WBD)](https://www.usgs.gov/core-science-systems/ngp/national-hydrography/watershed-boundary-dataset?qt-science_support_page_related_con=4#qt-science_support_page_related_con).

[WQP](https://www.waterqualitydata.us/) has 265 million results from over 2.2 million locations collected by hundreds of government and non-government agencies. Harmonized data for total suspended solids (tss), chlorophyll-a (chl-a), dissolved organic carbon (doc), water turpidity (secchi) and site locations were included in WQP_VizTool. [WBD dataset](https://www.usgs.gov/core-science-systems/ngp/national-hydrography/watershed-boundary-dataset?qt-science_support_page_related_con=4#qt-science_support_page_related_con) contains spatial data of multipolygons describing watershed boundaries at all HUC levels. In [NHD dataset](https://www.usgs.gov/core-science-systems/ngp/national-hydrography/national-hydrography-dataset?qt-science_support_page_related_con=0#qt-science_support_page_related_con), spatial data describing flowlines is extensively used, as well as the numerical flowline attributes, from which the upstream catchment area data were extracted to construct the coverage plots.

### Harmonized Data
Harmonized WQP data is used in this visualization tool as default, which is comprised of total suspended solids (tss), chlorophyll-a (chl-a), dissolved organic carbon (doc), water turpidity (secchi). These data are included for their detectablity by remote sensing satellites.
**What is harmonization?**
Harmonization is the process of bringing together data of varying file formats, naming conventions, and columns, and transforming it into one cohesive data set. Especially in the case of WQP data, harmonization includes merging measurements of the same constituent coming from different agencies and institutions, converting the different systems of unit, changing the column names, and unifying the location data of the sampling sites with the measurements.

For the script of harmonization, please see [code]().  -->

## Run the app locally
- Install necessary libraries in R
```R
install.packages("tidyverse", "feather", "shiny", "plotly", "leaflet", "sf", "shinycssloaders", "shinyalert", "DBI", "RSQLite")
```
- Clone the repository
- Download the datasets
    - [Download the processed dataset from the Google Drive]()

    - OR: Download the datasets from their original sources:
        - [Harmonized AquaSAT dataset with data from the WQP]()
        - [Watershed Boundary Dataset]()
        - [National Hydrography Dataset]()
        - Go through the process outlined in the [helper tool documentation](docs/HELPERTOOLS.md)
- Set the working directory
```R
setwd("/your/working/directory")
```
- Run the app
```R
runApp("wqpVisualizationTools")
```
<!-- ## What to do if you want to switch to datasets of your choice?
how to harmonize the data and switch the column names
### What to do to add water quality parameters

---
## What to do to add plots and graphs of your own design in the visualization process?
Add tabs?

---
## Contact information for the programmer
## Known bugs and  -->
<!-- ## Credits and acknowledgments
`cite the proposal`
`give the names for the team` -->
