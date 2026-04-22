suppressPackageStartupMessages({
  require(shiny)
  require(bslib)
  require(leaflet)
  require(leafgl)
  require(plotly)
  require(here)
  require(sf)
  require(htmltools)
  require(markdown)
  require(shinycssloaders)
  require(dragonapp)
})

folder <- "data"
# folder <- here::here("app", "data")

# load gis data
df <- read.csv(file.path(folder, "ts_country.csv"))

pt <- sf::st_read(
  file.path(folder, "grid.gpkg"),
  quiet = TRUE
)
pt <- sf::st_cast(pt, "POLYGON", warn = FALSE)
pt_df <- readRDS(file.path(folder, "grid_df.rds"))
pt <- cbind(pt, pt_df[, -1])

ph <- read.csv(file.path(folder, "pheno.csv"))

pcoef <- read.csv(file.path(folder, "psi_coef.csv"))

sp_choices <- sort(unique(df$species))

yr_range <- sort(unique(df$year, na.rm = TRUE))

# Leaflet zoom parameter
Zmin <- 2
Zmax <- 7
Z <- 4

# type of maps
map_choices <- c("mean occupancy" = "average", 
                 "occupancy trend" = "slope", 
                 "dynamic occupancy" = "dynamic",
                 "site deviation (mean)" = "beta_psi", 
                 "site deviation (trend)" = "beta_psi_slope")
