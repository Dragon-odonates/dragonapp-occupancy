#' Attach the occupancy outputs to the grid
#'
#' @param grid a `terra::SpatVector` object with the grid
#' @param dir_sp directory with output of occupancy model (qs)
#' @param digits integer indicating the number of decimal places to be kept.
#'
#' @returns A `terra::SpatVector` with occupancy summarized per species
#'
#' @export
#'
get_poly_occupancy <- function(grid, dir_sp, digits = 5) {
  # Checking the inputs ------------
  stopifnot("`grid` must be a `SpatVector`." = {
    "SpatVector" %in% class(grid)
  })
  stopifnot("`grid` must contains 'polygons`." = {
    terra::is.polygons(grid)
  })
  stopifnot("`grid_id` must be in `grid`." = {
    "grid_id" %in% names(grid)
  })
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
  # transform projection if not in EPSG:4326
  if (terra::crs(grid, proj = TRUE) != "+proj=longlat +datum=WGS84 +no_defs") {
    grid <- terra::project(grid, "EPSG:4326")
  }

  # output
  gdout <- data.frame("grid_id" = grid$grid_id)

  for (i in sp_list) {
    # load psi data
    psi_file <- file.path(dir_sp, i, paste0("psi_", i, ".qs"))
    df <- qs2::qs_read(psi_file)
    # rapid check
    msg <- paste0(
      psi_file,
      " must have columns `median`, `grid_id`, `year`."
    )
    stopifnot(msg = {
      all(c("median", "grid_id", "year") %in% names(df))
    })
    # transform to wide
    wide <- tapply(df$median, list(df$grid_id, df$year), mean)
    # replace NA by 0
    wide[is.na(wide)] <- 0
    # match with gdout
    wide <- wide[match(gdout$grid_id, row.names(wide)), ]
    # get characteristics
    average <- apply(wide, 1, mean)
    slope <- apply(wide, 1, get_slope)
    # load psi coef
    coef_file <- file.path(dir_sp, i, paste0("psi_coef_", i, ".qs"))
    df2 <- qs2::qs_read(coef_file)
    # rapid check
    msg <- paste0(
      paste0("psi_coef_", i, ".qs"),
      " must have large_variable `beta_psi_site` and `beta_psi_site_slope`."
    )
    stopifnot(msg = {
      all(c("beta_psi_site", "beta_psi_site_slope") %in% df2$large_variable)
    })
    bsite <- df2[df2$large_variable == "beta_psi_site", ]
    # make sure one value per grid_id
    bsite_w <- tapply(bsite$median, bsite$variable_name, mean)
    bsite_m <- bsite_w[match(gdout$grid_id, names(bsite_w))]

    bslope <- df2[df2$large_variable == "beta_psi_site_slope", ]
    # make sure one value per grid_id
    bslope_w <- tapply(bslope$median, bslope$variable_name, mean)
    bslope_m <- bslope_w[match(gdout$grid_id, names(bslope_w))]

    outi <- data.frame(average, slope, bsite_m, bslope_m, wide)
    names(outi) <- paste(
      i,
      c("average", "slope", "beta_psi", "beta_psi_slope", colnames(wide)),
      sep = "."
    )
    # round values to make dataset smaller
    outi <- apply(outi, 2, round, digits = digits)
    # attach with grid
    gdout <- cbind(gdout, outi[match(gdout$grid_id, row.names(outi)), ])
  }

  # attach to the original grid
  gd <- terra::merge(grid, gdout, by = "grid_id")
  # keep only relevant columns
  gd <- gd[, names(gdout)]
  return(gd)
}
