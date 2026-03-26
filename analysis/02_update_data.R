# load the functions from this repository
devtools::load_all()

# select the data repository
dir_sp <- here::here("data", "psi_data")

# provide the grid
grid_file <- here::here("data", "psi_data", "grid.gpkg")

# update the dataset in the shiny app
add_shiny_data(dir_sp, grid_file, overwrite = TRUE)

# Run the shiny app locally
app_path <- here::here("app")
shiny::runApp(app_path, display.mode = "normal")

# Deploy the shinyapp to online server
rsconnect::deployApp(
  appDir = app_path,
  appFiles = rsconnect::listDeploymentFiles(app_path),
  appName = "Dragon_occ",
  appTitle = "Dragon Species distribution"
)
