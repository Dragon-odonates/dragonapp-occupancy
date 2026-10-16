suppressPackageStartupMessages({
  require(shiny)
  require(shinyWidgets)
  require(bslib)
  require(leaflet)
  require(leafgl)
  require(plotly)
  require(ggiraph)
  require(sf)
  require(htmltools)
  require(markdown)
  require(shinycssloaders)
  require(qs2)
  require(dragonapp.occupancy)
})


# Keep as-is for deployment
folder <- "data"
set_theme(theme_minimal())

# Maps data ---------------------------------------------------------------
df <- qs2::qs_read(file.path(folder, "ts_country.qs"))

grid <- sf::st_read(
  file.path(folder, "grid.gpkg"),
  quiet = TRUE
)
grid <- sf::st_cast(grid, "POLYGON", warn = FALSE)
pt_df <- qs2::qs_read(file.path(folder, "grid_df.qs"))
pt <- cbind(grid, pt_df[, -1])


# Countries ---------------------------------------------------------------
countries <- sf::st_read(
  file.path(folder, "countries.gpkg"),
  quiet = TRUE
)
countries <- sf::st_cast(countries, "POLYGON", warn = FALSE)

# Pheno -------------------------------------------------------------------
ph <- qs2::qs_read(file.path(folder, "pheno.qs"))

# Model coefficients ------------------------------------------------------
psicoef <- qs2::qs_read(file.path(folder, "psi_coef.qs"))

psibio <- qs2::qs_read(file.path(folder, "psi_bioclim_curve.qs"))

pcoef <- qs2::qs_read(file.path(folder, "p_coef.qs"))

# App UI ------------------------------------------------------------------

# Leaflet zoom parameter
Zmin <- 3
Zmax <- 8

bb <- sf::st_bbox(pt)

# Species list
sp_choices <- sort(unique(df$species))
names(sp_choices) <- gsub("_", " ", sp_choices)

# Countries list plot
df$country[df$country == "Global trend"] <- "European trend"

country_choices <- sort(unique(df$country))
country_choices <- country_choices[country_choices != "European trend"]
country_choices <- c("European trend", country_choices)


# Map
map_choices <- c("mean occupancy" = "average", 
                 "occupancy trend" = "slope")

leg_names <- c("Mean occupancy" = "average", 
               "Mean yearly trend" = "slope")
