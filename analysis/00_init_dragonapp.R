folder <- here::here("data", "psi_data")

## Create the grid ---------------
# Load the grid (EEA grid from https://ec.europa.eu/eurostat/web/gisco/geodata/grids
grid <- terra::vect(here::here("data", "raw", "grid_20km_surf.gpkg"))

# Simplify and rename the attributes
grid <- grid[, "GRD_ID"]
names(grid) <- "grid_id"

psif <- list.files(folder, "^psi_.*qs$", full.names = TRUE, recursive = TRUE)
ag <- qs2::qs_read(psif[1])
# select grid cell in ag
grid <- grid[grid$grid_id %in% ag$grid_id, ]
dim(grid)
# project grid to latlong
grid_4326 <- terra::project(grid, "EPSG:4326")
# export as grid.gpkg
terra::writeVector(grid_4326, file.path(folder, "grid.gpkg"), overwrite = TRUE)

## Initialize the shiny app -----------
devtools::load_all()
remotes::install_github("Dragon-odonates/dragonapp", force = TRUE)
library(dragonapp)

# get the psi files
psif <- list.files(folder, "^psi_.*qs$", full.names = TRUE, recursive = TRUE)
psif <- psif[!grepl("coef", psif)]
# get the species list
spl <- sapply(strsplit(psif, "/"), function(x) x[length(x)])
spl <- gsub(".qs$", "", gsub("^psi_", "", spl))
# get the grid
gridf <- file.path(folder, "grid.gpkg")

add_shiny_data(
  sp_files = psif,
  grid_file = gridf,
  sp_list = spl,
  label = "model",
  overwrite = TRUE
)

rm_shiny_data("obs")

save_app(here::here(), compress = FALSE)
