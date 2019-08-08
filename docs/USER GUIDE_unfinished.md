# WQP_VizTool
## User Guide
```
logo
```
Developed by a team of data scientists and environmental scientists at Duke University, WQP_VizTool is a visualization tool for the US national water quality data. By linking it to the [Water Quality Portal](https://www.waterqualitydata.us/), it provides a user-friendly interface to **select**, **filter**, **download** and **explore the pattern** of water quality and hydrology data. 

---
## Databases used in this visualization tool
Data used in WQP-VizTool are chemical and physical measurements held in publicly accessible government databases. Three databases are used as main sources: the [Water Quality Portal (WQP)](https://www.waterqualitydata.us/); the [National Hydrography Dataset (NHD)](https://www.usgs.gov/core-science-systems/ngp/national-hydrography/national-hydrography-dataset?qt-science_support_page_related_con=0#qt-science_support_page_related_con); and the [Watershed Boundary Dataset (WBD)](https://www.usgs.gov/core-science-systems/ngp/national-hydrography/watershed-boundary-dataset?qt-science_support_page_related_con=4#qt-science_support_page_related_con). 

[WQP](https://www.waterqualitydata.us/) has 265 million results from over 2.2 million locations collected by hundreds of government and non-government agencies. Harmonized data for total suspended solids (tss), chlorophyll-a (chl-a), dissolved organic carbon (doc), water turpidity (secchi) and site locations were included in WQP_VizTool. [WBD dataset](https://www.usgs.gov/core-science-systems/ngp/national-hydrography/watershed-boundary-dataset?qt-science_support_page_related_con=4#qt-science_support_page_related_con) contains spatial data of multipolygons describing watershed boundaries at all HUC levels. In [NHD dataset](https://www.usgs.gov/core-science-systems/ngp/national-hydrography/national-hydrography-dataset?qt-science_support_page_related_con=0#qt-science_support_page_related_con), spatial data describing flowlines is extensively used, as well as the numerical flowline attributes, from which the upstream catchment area data were extracted to construct the coverage plots. 

## Features
### First page - HUC region selection
```
screenshot of first page
```
A map of [water resource region](https://en.wikipedia.org/wiki/Water_resource_region) of the US will be loaded once the app is run. Areas are colored according to the number of measurement data in that region. On the right side, there is a selection panel, where the user can select a more detailed [HUC level](https://en.wikipedia.org/wiki/Hydrological_code) to show on the map. One can also select specific water quality constituents (chl, tss, doc, secchi, etc. )to demonstrate instead of all the measurements in the dataset. 

Due to the small size of HUC10 and HUC12 areas, the selection of HUC codes is limited to levels below 8. One can use the filter tools on the second page to sort out the data for a certain HUC10 or HUC12 only. 

Upon selection, one can see the name of the HUC region and the number of measurement data in that region, when he/she hovers the cursor over the regions. The user can then select the water resource area of interest and the water quality constituent to investigate by clicking. A "Show me more!" button will pop up to take the user to the detailed data on the second page. 

`Note: Certain areas might take a longer time to load due to the large number of observations`

### Second page - data visualization
```
screenshot of second page example
```
The second page is mainly comprised of three interactive graphs, a map that shows the spatial information of the sampling points, a coverage plot, and a time series graph for the value of the measurements. On the very top, the user can see the hydrological unit code and name of the water resource region he selected. On the right, there is a panel of filters and selections that can be used to narrow down the selection of data to be visualized. 

Funtionalities for the interactive graphs: 
**The map:** 
- Zooming
- Change basemaps to see waterways or state borders
- Box selection of unique sampling points: the other two maps will be updated according to the selection

**The coverage plot:**
The x-axis signifies the date of the measurement and the y-axis is the upstream catchment area
- Box selection of coverage points will be reflected on the other two graphs

**The time series:**
The x-axis stands for time, and the y-axis stands for the value of the measurements
- Zoom in to see the detailed patterns of data
- Double click on the legend to select display for only one constituent, the whole graph will re-scale according to the values. 

**The filter panel:**
The filter panel contains filters for the data
- Filter by site location type: includes choices on stream, lake, estuary, etc. 
- Filter by site location name: serves similar purpose of box selection on the map, but instead uses a dropdown menu of the names of the sampling sites
- Filter by date
- Show cluster: will change in map into cluster mode, with circles and numbers that indicate the number of observations in the area
- HUC10 and HUC12: selection to show watershed boundaries of HUC10 or HUC12 on the map. `Note: for large regions, loading could take some time`

## Contact information for the programmer

## Credits and acknowledgments
