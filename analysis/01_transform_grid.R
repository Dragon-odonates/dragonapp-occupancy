# Transform the EEA grid for use in the Shiny app
#
# Select only the relevant cells, and project it to EPSG:4326
#
# input: "grid_XXkm_surf.gpkg"
#    from https://ec.europa.eu/eurostat/web/gisco/geodata/grids
# output: "grid.gpkg"

# Parameters to be updated ---------------------
data_folder <- here::here("data")
eea_grid <- here::here("data", "raw", "grid_20km_surf.gpkg")
output <- file.path(data_folder, "grid.gpkg")
# ----------------------------------------------

# Load the grid
grid <- terra::vect(eea_grid)

# Simplify and rename the attributes
grid <- grid[, "GRD_ID"]
names(grid) <- "grid_id"

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
