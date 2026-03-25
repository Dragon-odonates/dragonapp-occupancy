# dragonapp-psi

Shiny app for exploring [Dragon](https://www.fondationbiodiversite.fr/en/the-frb-in-action/programs-and-projects/le-cesab/funbiodiv/) occupancy model. You can run the shinyApp locally, or use it [online](https://rfrelat-cesab.shinyapps.io/Dragon_occ/).



## Add or update occupancy data  

First, you need to clone this repository.

#### 1. Add the occupancy data into the shiny app

```r
# Select the folder with raw output of occupancy model
dir_sp <- here::here("data", "psi_data")
grid_file <- here::here("data", "psi_data", "grid.gpkg")

# Transform the raw data to dataset readable to shiny app
# The output is saved in the app repository (= you must be able to write on it)
add_shiny_data(dir_sp, grid_file, overwrite = TRUE)
```

#### 2. Run the shiny app

```r
app_path <- here::here("app")
# run the Shiny app locally
shiny::runApp(app_path, display.mode = "normal")
```

#### 3. (Optional) deploy the shiny app to shinyapps.io

*This feature has not been tested yet*
```r
rsconnect::deployApp(
  appDir = app_path,
  appFiles = rsconnect::listDeploymentFiles(app_path),
  appName = "Dragon_occ",
  appTitle = "Dragon Species distribution"
)
# 3.2Mb
```