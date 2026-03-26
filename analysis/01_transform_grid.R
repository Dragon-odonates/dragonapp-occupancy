# Transform the EEA grid for use in the Shiny app
#
# Select only the relevant cells, and project it to EPSG:4326
#
# input: "grid_XXkm_surf.gpkg"
#    from https://ec.europa.eu/eurostat/web/gisco/geodata/grids
# output: "grid.gpkg"

# Parameters to be updated ---------------------
sub <- "Europe"
scale <- 20000
model <- "18_occ_yearenvsitetime_envquadra_fac_site_time_von_mises_ll_missing_binom_7d"
data_folder <- file.path("/media/seagate/lnicvert/dragonocc/outputs/02_occupancy_stan/02_real", 
                         sub, scale, model)

eea_grid <- file.path("/home/lnicvert/code/dragonocc/outputs/01_prepare_data/02_real", 
                      sub, scale, 
                      "grid.gpkg")

out_folder <- here::here("data")
output <- file.path(out_folder, "grid.gpkg")
# ----------------------------------------------

# Load the grid
grid <- terra::vect(eea_grid)

# Simplify and rename the attributes
grid <- grid[, "grid_id"]

# Load the data file
psi_file <- list.files(
  data_folder,
  "^psi_.*qs$",
  full.names = TRUE,
  recursive = TRUE
)
psi <- qs2::qs_read(psi_file[1])

# select grid cells that are in psi data file
grid <- grid[grid$grid_id %in% psi$grid_id, ]

# project grid to latlong
grid_4326 <- terra::project(grid, "EPSG:4326")

# export as grid.gpkg
terra::writeVector(grid_4326, output, overwrite = TRUE)
