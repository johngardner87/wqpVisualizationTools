# Helper Tools

Downloading and processing raw datasets and setting up the Datasets directory used by the wqpVisualizationTools Shiny app.

---

## Downloading the raw datasets
- Create `Datasets` folder in the main `wqpVisualizationTools` directory
- Harmonized WQP Dataset from AquaSAT
  - Harmonized dataset based on Water Quality Portal data for matching satellite imagery to water quality measurements
  - Can be found [TO BE ADDED]
- Water Boundary Dataset
  - Files available from the [USGS website](https://www.usgs.gov/core-science-systems/ngp/national-hydrography/access-national-hydrography-products) under 'Watershed Boundary Dataset (WBD) Direct Download Links' (look for WBD_National_GDB.zip)
- National Hydrography Dataset
  - Files available from [EPA](https://www.epa.gov/waterdata/nhdplus-national-data) and [USGS](https://www.usgs.gov/core-science-systems/ngp/national-hydrography/access-national-hydrography-products) websites - read more about the files nhdPlusTools expects [here](https://github.com/USGS-R/nhdplusTools)

## Processing the datasets
- Install the necessary packages in R
```R
install.packages("tidyverse", "feather", "sf", "rmapshaper", "dbplyr", "devtools")

devtools::install_github("USGS-R/nhdplusTools")
```
- Simplify WBD shapefiles
  - Run `simplifyHUCBoundary.R`
- Process WQP Data
  - Run `wqpJoin.R`
  - Run `wqpIndexing.R`
    - Functions `wqpIndexing` and `wqpCombining` must both be called, but can be run sequentially one at a time if memory issues arise.
    - This process assigns flowline COMIDs to all individual measurements and takes a long time (on the order of a few hours).
  - Run `countMeasurements.R`
  - Run `saveWQPSQLite.R`
- Clean intermediary data
  - The following directories/files should remain in the `Datasets` directory:
    - `/WBD_Simplified/WBDHUxCounts.gpkg` for HUCs 2-8
    - `/WBD_Simplified/WBDHUx.gpkg` for HUCs 10 and 12
    - `/wqp_Constituents/wqp_x_indexed.sqlite` for constituents `All`, `chlorophyll`, `doc`, `secchi`, and `tss`
  - All other files can be removed.
