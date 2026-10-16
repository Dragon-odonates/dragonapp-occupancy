# load the functions from this repository
devtools::load_all()
library(rsconnect)
library(here)

library(data.table)
library(qs2)

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
countries_file <- here::here("data", "countries.gpkg")

dirdata <- here::here("app", "data")

verbose <- TRUE

sp_list <- c("Sympetrum danae",
              "Calopteryx virgo",
              "Calopteryx xanthostoma",
              "Ischnura elegans",
              "Aeshna juncea",
              "Somatochlora metallica",
              "Leucorrhinia dubia",
              "Oxygastra curtisii",
              "Ophiogomphus cecilia",
              "Crocothemis erythraea",
              "Coenagrion mercuriale",
              "Lestes barbarus",
              "Aeshna cyanea",
              "Aeshna grandis",
              "Sympetrum flaveolum",
              "Sympetrum striolatum",
              "Sympetrum vulgatum",
              "Somatochlora arctica",
              "Leucorrhinia rubicunda",
              "Gomphus pulchellus",
              "Coenagrion hastulatum",
              "Coenagrion castellani",
              "Enallagma cyathigerum",
              "Lestes sponsa")
sp_list <- gsub(" ", "_", sp_list)


# Large variables names ---------------------------------------------------
psicoef_var <- data.frame(
  old = c("psi_intercept", "beta_psi_time", "beta_psi_clc", "beta_psi_gssite",  "beta_psi_gsslope"),
  new = c("General intercept", "Year trend", "Landcover", "Spatial intercept", "Spatial slope"))
psicoef_var <- as.data.table(psicoef_var)

pcoef_var <- data.frame(
  old = c("beta_p_ll", "beta_p_site", "sigma_p_site"),
  new = c("List length", "Site intercept", "Site intercept sd."))
pcoef_var <- as.data.table(pcoef_var)

psibio_var <- data.frame(
  old = c("bio10_temp_warmQ_dC", "bio12_annual_prec_mm", "bio15_annual_sd_prec", "bio4_annual_sd_temp"),
  new = c("Temperature", "Precipitations", "Precipitations sd.", "Temperature sd."))
psibio_var <- as.data.table(psibio_var)

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

# Get countries
file.copy(countries_file,
          file.path(dirdata, "countries.gpkg"))

# Get inf folders with species --------------------------------------------
sp_files <- list.files(inf_folder, recursive = TRUE, full.names = TRUE)

# Keep only complete folders (check only pheno which is written last)
pheno_files <- grep("pheno_", sp_files, value = TRUE)
sp_list_all <- dirname(pheno_files)
sp_list_all <- basename(sp_list_all)

sp_list <- sp_list[sp_list %in% sp_list_all]
sp_list <- file.path(inf_folder, sp_list)

sp_list <- sp_list[1:3]
sp_list_names <- basename(sp_list)

# Format data -------------------------------------------------------------
## Mean occupancy and spacial slope ------
gd <- get_poly_occupancy(grid, sp_list, verbose = verbose)

# saveRDS(data.frame(gd), file.path(dirdata, "grid_df.rds"))
qs2::qs_save(data.frame(gd), file.path(dirdata, "grid_df.qs"))
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
# utils::write.csv(df2, file.path(dirdata, "ts_country.csv"), row.names = FALSE)
qs2::qs_save(df2, file.path(dirdata, "ts_country.qs"))

## Get phenological data -----
ph <- get_pheno(sp_list)
ph$x <- as.Date(paste("2000", ph$doy), format = "%Y %j")
ph <- data.table::as.data.table(ph)

# utils::write.csv(ph, file.path(dirdata, "pheno.csv"), row.names = FALSE)
qs2::qs_save(ph, file.path(dirdata, "pheno.qs"))

# Get psi coefficients ---
psicoef <- get_coef("psi_coef_", sp_list)

# Get bioclim curve
# stopifnot("Please provide environment and scaling" = {!is.null(env) | !is.null(scaling)})
bio <- get_bioclim_seq(env)
bioclim_df <- get_bioclim_curve(bio, psi_coef = psicoef, scaling = sc)
bioclim_df <- data.table::as.data.table(bioclim_df)

# Rename variables
bioclim_df <- psibio_var[bioclim_df, on = c("old" = "var")]
bioclim_df[is.na(new), new := old]
bioclim_df[, old := NULL]
setnames(bioclim_df,
         old = "new",
         new = "var")

# Remove bioclim coefficients from psicoef
rm_var <- c("beta_psi_bioclim", "beta_psi_bioclim_sq")
all_var <- unique(psicoef$large_variable)
needed_var <- all_var[!all_var %in% rm_var]
psicoef <- psicoef[psicoef$large_variable %in% needed_var, ]
psicoef <- data.table::as.data.table(psicoef)

# Rename variables
psicoef[, large_variable := as.character(large_variable)]
psicoef <- psicoef_var[psicoef, on = c("old" = "large_variable")]
psicoef[is.na(new), new := old]
psicoef[, old := NULL]
setnames(psicoef,
         old = "new",
         new = "large_variable")
psicoef[, large_variable := factor(large_variable)]

# utils::write.csv(psicoef, file.path(dirdata, "psi_coef.csv"), row.names = FALSE)
# utils::write.csv(bioclim_df, file.path(dirdata, "psi_bioclim_curve.csv"), row.names = FALSE)
qs2::qs_save(psicoef, file.path(dirdata, "psi_coef.qs"))
qs2::qs_save(bioclim_df, file.path(dirdata, "psi_bioclim_curve.qs"))

# Get p coefficients ---
pcoef <- get_coef("p_coef_", sp_list)
pcoef <- data.table::as.data.table(pcoef)

# Rename variables
pcoef[, large_variable := as.character(large_variable)]
pcoef <- pcoef_var[pcoef, on = c("old" = "large_variable")]
pcoef[is.na(new), new := old]
pcoef[, old := NULL]
setnames(pcoef,
         old = "new",
         new = "large_variable")
pcoef[, large_variable := factor(large_variable)]

# utils::write.csv(pcoef, file.path(dirdata, "p_coef.csv"), row.names = FALSE)
qs2::qs_save(pcoef, file.path(dirdata, "p_coef.qs"))

# update the dataset in the shiny app
# add_shiny_data(data_folder, grid_file, env = env, sc = sc, overwrite = TRUE)

