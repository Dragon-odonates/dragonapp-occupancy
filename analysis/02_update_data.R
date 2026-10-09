# load the functions from this repository
devtools::load_all()
library(rsconnect)
library(here)

library(data.table)

# Parameters --------------------------------------------------------------
sub <- "Europe"
scale <- 20000
visit_sub <- "visit_sub"
model <- "24-2_psi_rw_bioclim_quadra_clc_gssite_gsslope_p_von_mises_ll_site_binom_missing_grid_gs30_gsi20"

inf_folder <- file.path("/media/seagate/lnicvert/dragonocc/outputs/02_occupancy_stan/02_real",
                         sub, scale, visit_sub,
                         model)

precomputed_folder <- file.path("~/code/dragonocc/outputs/03_analysis/01_get_trends")

precomputed_country_folder <- file.path(precomputed_folder, "psi_country")
precomputed_all_folder <- file.path(precomputed_folder, "psi_year")

# provide the grid
grid_file <- here::here("data", "grid.gpkg")

dirdata <- here::here("app", "data")

verbose <- TRUE

# Read files --------------------------------------------------------------

# Get env
env_file <- file.path("~", "code", "dragonocc", "outputs", "01_prepare_data", "02_real",
                      sub, scale, visit_sub, "env_sd.rds")
env <- readRDS(env_file)
env <- as.data.frame(env)

# Get scaling
sc_file <- file.path("~", "code", "dragonocc", "outputs", "01_prepare_data", "02_real",
                     sub, scale, visit_sub, "scaling.rds")
sc <- readRDS(sc_file)

# Get grid
grid <- terra::vect(grid_file)

# Get inf folders with species --------------------------------------------
sp_files <- list.files(inf_folder, recursive = TRUE, full.names = TRUE)

# Keep only complete folders (check only pheno which is written last)
pheno_files <- grep("pheno_", sp_files, value = TRUE)
sp_list <- dirname(pheno_files)

sp_list <- sp_list[1:3]
sp_list_names <- basename(sp_list)

# Format data -------------------------------------------------------------
## Mean occupancy and spacial slope ------
gd <- get_poly_occupancy(grid, sp_list, verbose = verbose)

saveRDS(data.frame(gd), file.path(dirdata, "grid_df.rds"))
terra::writeVector(
  gd[, "grid_id"],
  file.path(dirdata, "grid.gpkg"),
  overwrite = TRUE
)

## Calculate the weighted mean per country -----
# country <- dragon_country()
# plot(country[, "sov_a3"])
# 
# df <- get_ts_country(grid, sp_list, country)
# utils::write.csv(df, file.path(dirdata, "ts_country.csv"), row.names = FALSE)

country_files <- list.files(precomputed_country_folder)
country_files <- grep(paste0(sp_list_names, collapse = "|"), country_files, value = TRUE)

df2 <- lapply(file.path(precomputed_country_folder, country_files), qs2::qs_read)
df2 <- rbindlist(df2)
df2 <- df2[, .(species, year, country_name, median, qmin, qmax)]

df_all <- lapply(file.path(precomputed_all_folder, country_files), qs2::qs_read)
df_all <- rbindlist(df_all)
df_all[, country_name := "European trend"]
df_all <- df_all[, .(species, year, country_name, median, qmin, qmax)]

df2 <- rbind(df2, df_all)

df2[, species := gsub(" ", "_", species)]
setnames(df2, 
         old = c("country_name", "median"),
         new = c("country", "mean"))

df2 <- df2[!country %in% c("Andorra"), ]

df2 <- as.data.frame(df2)
utils::write.csv(df2, file.path(dirdata, "ts_country.csv"), row.names = FALSE)

## Get phenological data -----
pheno <- get_pheno(sp_list)
utils::write.csv(pheno, file.path(dirdata, "pheno.csv"), row.names = FALSE)

# Get psi coefficients ---
psi_coef <- get_coef("psi_coef_", sp_list)

# Get bioclim curve
# stopifnot("Please provide environment and scaling" = {!is.null(env) | !is.null(scaling)})
bio <- get_bioclim_seq(env)
bioclim_df <- get_bioclim_curve(bio, psi_coef = psi_coef, scaling = sc)

# Remove bioclim coefficients from psi_coef
rm_var <- c("beta_psi_bioclim", "beta_psi_bioclim_sq")
all_var <- unique(psi_coef$large_variable)
needed_var <- all_var[!all_var %in% rm_var]
psi_coef <- psi_coef[psi_coef$large_variable %in% needed_var, ]

utils::write.csv(psi_coef, file.path(dirdata, "psi_coef.csv"), row.names = FALSE)
utils::write.csv(bioclim_df, file.path(dirdata, "psi_bioclim_curve.csv"), row.names = FALSE)

# Get p coefficients ---
p_coef <- get_coef("p_coef_", sp_list)
utils::write.csv(p_coef, file.path(dirdata, "p_coef.csv"), row.names = FALSE)

# update the dataset in the shiny app
# add_shiny_data(data_folder, grid_file, env = env, sc = sc, overwrite = TRUE)

# Run the shiny app locally
app_path <- here::here("app")
shiny::runApp(app_path, display.mode = "normal")

