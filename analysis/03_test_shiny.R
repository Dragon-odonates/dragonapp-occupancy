library(here)

source(here("app/global.R"), chdir = TRUE)

# Mock user inputs --------------------------------------------------------
country <- c("European trend", "Germany")
spe <- df$species[1]

map <- "slope"

# Trends ------------------------------------------------------------------
df_sub <- df[df$species == spe & df$country %in% country, ]

g <- plot_trend(df_sub)
girafe(ggobj = g)


# Maps --------------------------------------------------------------------
sub_pt <- pt[grepl(spe, names(pt))]

m <- plot_base_map(pt, Zmin, Zmax, bounds = bb)
plot_polygons_map(m, map_type = map, map_data = sub_pt, leg_names = leg_names)

