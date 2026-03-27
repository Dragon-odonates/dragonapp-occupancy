#' Calculate the average occupancy per country and per time step
#'
#' @param dir_sp Directory with output of occupancy model (qs)
#' @param grid_file Path to a grid file (gpkg object)
#' @param country a `terra::SpatVector` object with the country definition
#' @param overwrite whether existing data will be overwritten
#' @param verbose Print messages?
#'
#' @returns A `data.frame` with the grid_id in rows and country in columns
#'
#' @export
#'
add_shiny_data <- function(
  dir_sp,
  grid_file,
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
    message("get_pheno and get_psi_coef ------")
  }
  # Get phenological data
  pheno <- get_pheno(sp_list)
  utils::write.csv(pheno, file.path(dirdata, "pheno.csv"), row.names = FALSE)

  # Get environmental drivers data
  env <- get_psi_coef(sp_list)
  utils::write.csv(env, file.path(dirdata, "psi_coef.csv"), row.names = FALSE)

  invisible(list("pt" = gd, "ts" = df, "pheno" = pheno))
}
