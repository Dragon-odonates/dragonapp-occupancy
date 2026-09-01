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

# provide the grid
grid_file <- here::here("data", "grid.gpkg")

# Get env
env_file <- file.path("~", "code", "dragonocc", "outputs", "01_prepare_data", "02_real",
                      sub, scale, "env_sd.rds")
env <- readRDS(env_file)
env <- as.data.frame(env)

# Get scaling
sc_file <- file.path("~", "code", "dragonocc", "outputs", "01_prepare_data", "02_real",
                     sub, scale, "scaling.rds")
sc <- readRDS(sc_file)

# sp_list <- list.dirs(data_folder)
# sp_list <- sp_list[2:length(sp_list)]
# sp <- sp_list[1]
# # dfi <- qs2::qs_read(file.path(sp, paste0("psi_coef_", basename(sp), ".qs")))
# 
# psi_coef <- get_coef("psi_coef_", sp_list)
# bio <- get_bioclim_seq(env)
# bioclim_df <- get_bioclim_curve(bio, psi_coef = psi_coef, scaling = sc)
# bioclim_df_sp <- bioclim_df[bioclim_df$species == basename(sp), ]
# ubio <- unique(bioclim_df_sp$var)
# plist <- vector(mode = "list", length = length(ubio))
# for (i in 1:length(ubio)) {
#   pl <- plot_ly_lines(bioclim_df_sp[bioclim_df_sp$var == ubio[i],])
#   plist[[i]] <- pl
# }
# plotly::subplot(plist,
#                 nrows = 2)
# 
# p_coef <- get_coef("p_coef_", sp_list)
# pl <- p_coef[p_coef$species == "Sympetrum_danae",]
# plot_ly_scatter(pl)
# 



# update the dataset in the shiny app
# debug(add_shiny_data)
add_shiny_data(data_folder, grid_file, env = env, sc = sc, overwrite = TRUE)

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