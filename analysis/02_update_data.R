# load the functions from this repository
devtools::load_all()
# library(here)

sub <- "Europe"
scale <- 20000
# model <- "11-1_psi_fac_bioclim_quadra_clc_site_siteslope_p_von_mises_ll_spread_missing_binom_7d"
model <- "14_psi_rw_bioclim_quadra_clc_gsslope_p_von_mises_ll_missing_gs30"
data_folder <- file.path("/media/seagate/lnicvert/dragonocc/outputs/02_occupancy_stan/02_real",
                         sub, scale, model)
# data_folder <- here("data/raw")

# # provide the grid
grid_file <- here::here("data", "grid.gpkg")

# sp_list <- list.dirs(file.path(data_folder))
# sp_list <- sp_list[2:length(sp_list)]
# psi_coef <- get_coef("psi_coef_", sp_list)
# 
# psi_coef_sp <- psi_coef[psi_coef$species == "Sympetrum_danae", ]
# 
# lv <- unique(psi_coef_sp$large_variable)
# 
# plist <- vector(mode = "list", length = length(lv))
# for (i in 1:length(lv)) {
#   pl <- plot_ly_scatter(psi_coef_sp[psi_coef_sp$large_variable == lv[i],])
#   if (lv[i] == "beta_psi_gsslope") {
#      pl <- pl |> 
#       layout(xaxis = list(showticklabels = FALSE))
#   }
#   plist[[i]] <- pl
# }
# 
# plotly::subplot(plist,
#                 nrows = 2)

# debug(get_coef)
# p_coef <- get_coef("p_coef_", sp_list)
# p_coef <- get_coef("p_coef_", sp_list)
# pl <- p_coef[p_coef$species == "Sympetrum_danae",]
# plot_ly_scatter(pl)

# update the dataset in the shiny app
add_shiny_data(data_folder, grid_file, overwrite = TRUE)

# # Run the shiny app locally
app_path <- here::here("app")
shiny::runApp(app_path, display.mode = "normal")

# # Deploy the shinyapp to online server
# rsconnect::deployApp(
#   appDir = app_path,
#   appFiles = rsconnect::listDeploymentFiles(app_path),
#   appName = "dragon-occupancy",
#   appTitle = "Dragonflies occupancy (DRAGON project)"
# )