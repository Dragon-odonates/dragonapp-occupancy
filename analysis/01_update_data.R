devtools::load_all()
dir_sp <- here::here("data", "psi_data")
grid_file <- here::here("data", "psi_data", "grid.gpkg")

add_shiny_data(dir_sp, grid_file, overwrite = TRUE)

app_path <- here::here("app")

# test
shiny::runApp(app_path, display.mode = "normal")

# deploy the shinyapp to online server
rsconnect::deployApp(
  appDir = app_path,
  appFiles = rsconnect::listDeploymentFiles(app_path),
  appName = "Dragon_occ",
  appTitle = "Dragon Species distribution"
)
