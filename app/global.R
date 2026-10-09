suppressPackageStartupMessages({
  require(shiny)
  require(shinyWidgets)
  require(bslib)
  require(leaflet)
  require(leafgl)
  require(plotly)
  require(ggiraph)
  require(here)
  require(sf)
  require(htmltools)
  require(markdown)
  require(shinycssloaders)
  require(dragonapp.occupancy)
})

# folder <- "data"
folder <- here::here("app", "data")

set_theme(theme_minimal())

# Maps data ---------------------------------------------------------------
df <- read.csv(file.path(folder, "ts_country.csv"))

pt <- sf::st_read(
  file.path(folder, "grid.gpkg"),
  quiet = TRUE
)
pt <- sf::st_cast(pt, "POLYGON", warn = FALSE)
pt_df <- readRDS(file.path(folder, "grid_df.rds"))
pt <- cbind(pt, pt_df[, -1])


# Pheno -------------------------------------------------------------------
ph <- read.csv(file.path(folder, "pheno.csv"))


# Model coefficients ------------------------------------------------------
psicoef <- read.csv(file.path(folder, "psi_coef.csv"))
psibio <- read.csv(file.path(folder, "psi_bioclim_curve.csv"))

pcoef <- read.csv(file.path(folder, "p_coef.csv"))


# App UI ------------------------------------------------------------------

# Leaflet zoom parameter
Zmin <- 2
Zmax <- 7
Z <- 4

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


# Tests -------------------------------------------------------------------

# # country <- c("European trend", "Germany")
# country <- c("Germany")
# spe <- df$species[1]
# 
# df_sub <- df[df$species == spe & df$country %in% country, ]
# # df_eur <- df[df$species == spe & df$country == "European trend", ]
# 
# g <- plot_trend(df_sub)
# girafe(ggobj = g)


