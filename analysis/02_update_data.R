# load the functions from this repository
devtools::load_all()

sub <- "Europe"
scale <- 20000
model <- "18_occ_yearenvsitetime_envquadra_fac_site_time_von_mises_ll_missing_binom_7d"
data_folder <- file.path("/media/seagate/lnicvert/dragonocc/outputs/02_occupancy_stan/02_real", 
                         sub, scale, model)

# provide the grid
grid_file <- here::here("data", "grid.gpkg")

# update the dataset in the shiny app
add_shiny_data(data_folder, grid_file, overwrite = TRUE)

# Run the shiny app locally
app_path <- here::here("app")
shiny::runApp(app_path, display.mode = "normal")

# # Deploy the shinyapp to online server
# rsconnect::deployApp(
#   appDir = app_path,
#   appFiles = rsconnect::listDeploymentFiles(app_path),
#   appName = "Dragon_occ",
#   appTitle = "Dragon Species distribution"
# )
