#' Calculate the average occupancy per country and per time step
#'
#' @param dir_sp Directory with output of occupancy model (qs)
#' @param grid_file Path to a grid file (gpkg object)
#' @param country a `terra::SpatVector` object with the country definition
#' @param overwrite whether existing data will be overwritten
#' @param verbose Print messages?
#' @param env Environment data (must have bioclimatic columns as bioxx_... columns)
#' @param scaling Scaling data (must have values corresponding to bioclimatic variables in the column variable)
#'
#' @returns A `data.frame` with the grid_id in rows and country in columns
#'
#' @export
#'
add_shiny_data <- function(
  dir_sp,
  grid_file,
  env,
  scaling,
  country = dragon_country(),
  overwrite = FALSE,
  verbose = TRUE
) {
  # Checking the grid ------------
  grid <- terra::vect(grid_file)
  stopifnot("`grid` must contains 'polygons`." = {
    terra::is.polygons(grid)
  })
  if (terra::crs(grid, proj = TRUE) != "+proj=longlat +datum=WGS84 +no_defs") {
    grid <- terra::project(grid, "EPSG:4326")
    warning("The grid was projected to EPSG:4326")
  }
  # Checking the occupancy files -------------------------
  sp_files <- list.files(dir_sp, recursive = TRUE, full.names = TRUE)
  
  # Keep only complete folders (check only pheno which is written last)
  pheno_files <- grep("pheno_", sp_files, value = TRUE)
  sp_list <- dirname(pheno_files)
  
  # Create folder to save the dataset
  dirdata <- here::here("app", "data")

  if (file.exists(dirdata) & !overwrite) {
    stop("A dataset already exist.")
  } else {
    unlink(dirdata, recursive = TRUE)
    dir.create(dirdata)
  }

  # Format occupancy per grid as spatial vector
  if (verbose) {
    message("get_poly_occupancy ------")
  }
  gd <- get_poly_occupancy(grid, sp_list, verbose = verbose)
  saveRDS(data.frame(gd), file.path(dirdata, "grid_df.rds"))
  terra::writeVector(
    gd[, "grid_id"],
    file.path(dirdata, "grid.gpkg"),
    overwrite = overwrite
  )
  
  if (verbose) {
    message("get_ts_country ------")
  }
  # Calculate the weighted mean per country
  df <- get_ts_country(grid, sp_list, country)
  utils::write.csv(df, file.path(dirdata, "ts_country.csv"), row.names = FALSE)

  if (verbose) {
    message("get_pheno and get_coef ------")
  }
  # Get phenological data ---
  pheno <- get_pheno(sp_list)
  utils::write.csv(pheno, file.path(dirdata, "pheno.csv"), row.names = FALSE)

  # Get psi coefficients ---
  psi_coef <- get_coef("psi_coef_", sp_list)
  
  # Get bioclim curve
  stopifnot("Please provide environment and scaling" = {!is.null(env) | !is.null(scaling)})
  bio <- get_bioclim_seq(env)
  bioclim_df <- get_bioclim_curve(bio, psi_coef = psi_coef, scaling = scaling)
  
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
  

  invisible(list("pt" = gd, "ts" = df, "pheno" = pheno))
}
