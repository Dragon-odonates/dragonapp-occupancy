# dragonapp-occupancy

Shiny app for exploring [Dragon](https://www.fondationbiodiversite.fr/en/the-frb-in-action/programs-and-projects/le-cesab/funbiodiv/) occupancy model. You can run the shinyApp locally, or use it [online](https://rfrelat-cesab.shinyapps.io/Dragon_occ/).

<p align="left">
• <a href="#overview">Overview</a><br> •
  <a href="#get-started">Get started</a><br> • 
  <a href="#acknowledgments">Acknowledgments</a><br> •
</p>


## Overview

This repository is structured as follow:

- :file_folder: &nbsp;`analyses/`: contains R script to update data to the shiny app;
- :file_folder: &nbsp;`app/`: contains the shiny app (no data stored online);
- :file_folder: &nbsp;`R/`: contains functions to simplify the integration of occupancy model output;
- :file_folder: &nbsp;`man/`: contains the documentation of the functions.


## Get started

> Follow this 6-step procedure to update occupancy model data in the shiny app.  

#### 1. Clone this repository

Click on the Code icon on the top right of this repository. Then load it in you favorite R coding environment and load the functions.

```r
devtools::load_all()
```

#### 2. Select the data repository

Data repository should obey to the following structure:

```
data/
└─ speciesA/
      ├─ psi_speciesA.qs
      ├─ psi_coef_speciesA.qs
      └─ pheno_speciesA.qs
└─ speciesB/
      ├─ psi_speciesB.qs
      ├─ psi_coef_speciesB.qs
      └─ pheno_speciesB.qs
```

Set the directory of the root folder containing data for all species, for instance:

```r
dir_sp <- here::here("data")
```

#### 3. Provide the grid used for the occupancy model

The grid must be a spatial file (geopackage format is recommended) and contain the attribute `grid_id`. Ideally, it should be in the projection system [EPSG:4326](https://epsg.io/4326). 

```r
grid_file <- here::here("data", "grid.gpkg")
```

To transform the raw [EEA Reference grid](https://ec.europa.eu/eurostat/web/gisco/geodata/grids) into a grid that match your occupancy data, you can follow the short script in analysis [01_transform_grid.R](https://github.com/Dragon-odonates/dragonapp-occupancy/blob/main/analysis/01_transform_grid.R)


#### 4. Update the dataset in the shiny app

```r
add_shiny_data(dir_sp, grid_file, overwrite = TRUE)
```

#### 5. Run the shiny app

```r
app_path <- here::here("app")
# run the Shiny app locally
shiny::runApp(app_path, display.mode = "normal")
```

#### 6. (Optional) deploy the shiny app to shinyapps.io

```r
rsconnect::deployApp(
  appDir = app_path,
  appFiles = rsconnect::listDeploymentFiles(app_path),
  appName = "Dragon_occ",
  appTitle = "Dragon Species distribution"
)
# 3.2Mb
```



## Acknowledgments

This research is a product of the [Dragon group](https://www.fondationbiodiversite.fr/en/the-frb-in-action/programs-and-projects/le-cesab/dragon/) funded from the [2022 FRB/MTE/OFB Impacts call](https://www.fondationbiodiversite.fr/la-frb-en-action/programmes-et-projets/impacts-sur-la-biodiversite-terrestre-dans-lanthropocene/).