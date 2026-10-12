library(here)

source(here("app/global.R"), chdir = TRUE)

# Reload functions for local tests
devtools::load_all()

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

# psi coef ----------------------------------------------------------------
sub_psi_coef <-  psicoef[species == spe, ]

sub_psi_coef_time <- sub_psi_coef[large_variable == "beta_psi_time", ]
plot_errorbar(sub_psi_coef_time)

sub_psi_coef_gssite <- sub_psi_coef[large_variable == "beta_psi_gssite", ]
plot_spatial(
  sub_psi_coef_gssite, 
  grid, 
  ggtitle = "ddd"
  )

gglist <- plot_coef(sub_psi_coef, grid = grid)
patchwork::wrap_plots(gglist)

sub_bio <- psibio[species == spe, ]

gglist <- list()
for (v in unique(sub_bio$var)) {
  sub_bio_sub <- sub_bio[var == v, ]
  g <- plot_errorline(sub_bio_sub, x = "x", ggtitle = v)
  gglist <- c(gglist, g)
}
patchwork::wrap_plots(gglist)


# p coef ------------------------------------------------------------------
sub_pcoef <-  pcoef[species == spe, ]

gglist <- plot_coef(sub_pcoef, grid = grid)
patchwork::wrap_plots(gglist)

sub_ph <- ph[species == spe, ]

plot_errorline(sub_ph, x = "x", ggtitle = "Phenology") +
  scale_x_date(date_labels = "%d %b")

# # Run app -----------------------------------------------------------------
# 
# # Run the shiny app locally
# app_path <- here::here("app")
# shiny::runApp(app_path, display.mode = "normal")
