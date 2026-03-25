#' Calculate the average occupancy per country and per time step
#'
#' @param dir_sp Directory with output of occupancy model (qs)
#' @param grid_file Path to a grid file (gpkg object)
#' @param country a `terra::SpatVector` object with the country definition
#' @param overwrite whether existing data will be overwritten
#'
#' @returns A `data.frame` with the grid_id in rows and country in columns
#'
#' @export
#'
add_shiny_data <- function(
  dir_sp,
  grid_file,
  country = dragon_country(),
  overwrite = FALSE
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
  sp_list <- list.dirs(dir_sp, recursive = FALSE, full.names = FALSE)
  sp_files <- list.files(dir_sp, recursive = TRUE)

  # get psi file
  check_psi <- file.path(sp_list, paste0("psi_", sp_list, ".qs"))
  stopifnot(
    "All species must have a psi_genus_species.qs file" = {
      all(check_psi %in% sp_files)
    }
  )
  # get psi file
  check_psi_coef <- file.path(sp_list, paste0("psi_coef_", sp_list, ".qs"))
  stopifnot(
    "All species must have a psi_coef_genus_species.qs file" = {
      all(check_psi_coef %in% sp_files)
    }
  )
  # get pheno file
  check_pheno <- file.path(sp_list, paste0("pheno_", sp_list, ".qs"))
  stopifnot(
    "All species must have a psi_genus_species.qs file" = {
      all(check_pheno %in% sp_files)
    }
  )
  # Create folder to save the dataset
  dirdata <- here::here("app", "data")

  if (file.exists(dirdata) & !overwrite) {
    stop("A dataset already exist.")
  } else {
    unlink(dirdata, recursive = TRUE)
    dir.create(dirdata)
  }

  # Format occupancy per grid as spatial vector
  gd <- get_poly_occupancy(grid, dir_sp)
  saveRDS(data.frame(gd), file.path(dirdata, "grid_df.rds"))
  terra::writeVector(
    gd[, "grid_id"],
    file.path(dirdata, "grid.gpkg"),
    overwrite = overwrite
  )
  # Calculate the weighted mean per country
  df <- get_ts_country(grid, dir_sp, country)
  utils::write.csv(df, file.path(dirdata, "ts_country.csv"), row.names = FALSE)

  # Get phenological data
  pheno <- get_pheno(dir_sp)
  utils::write.csv(pheno, file.path(dirdata, "ts_pheno.csv"), row.names = FALSE)

  # Get environmental drivers data
  env <- get_env(dir_sp)
  utils::write.csv(env, file.path(dirdata, "psi_env.csv"), row.names = FALSE)

  invisible(list("pt" = gd, "ts" = df, "pheno" = pheno))
}
