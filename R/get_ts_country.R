#' Calculate the country coverage per grid cell
#'
#' @param grid a `terra::SpatVector` object with the grid
#' @param country a `terra::SpatVector` object with the country definition
#' @param th coverage threshold value to keep countries in
#' @returns A `data.frame` with the grid_id in rows and country in columns
#'
#' @export
#'
get_area_country <- function(
  grid,
  country = dragon_country(),
  th = 0.5
) {
  stopifnot("`grid` must be a `SpatVector`." = {
    "SpatVector" %in% class(grid)
  })
  stopifnot("`grid_id` must be in `grid`." = {
    "grid_id" %in% names(grid)
  })
  if ("sf" %in% class(country)) {
    country <- terra::vect(country)
  }
  stopifnot("`country` must be a `SpatVector`." = {
    "SpatVector" %in% class(country)
  })
  stopifnot("`admin` must be in `country`." = {
    "admin" %in% names(country)
  })

  # calculate the intersection
  int_country <- terra::intersect(country, grid)
  # transform to wide per grid_id and country
  area_country <- tapply(
    terra::expanse(int_country),
    list(int_country$grid_id, int_country$admin),
    mean
  )
  # replace NA per 0
  area_country[is.na(area_country)] <- 0
  # transform to data.frame
  area_country <- data.frame(area_country, check.names = FALSE)
  # remove countries that are too partially covered
  sum_country <- tapply(terra::expanse(int_country), int_country$admin, sum)
  perc_country <- sum_country /
    terra::expanse(country[match(names(sum_country), country$admin)])
  area_country <- area_country[, names(perc_country)[perc_country > th]]
  # add information on total surface on countries of interest
  area_country$All <- rowSums(area_country)
  # should we remove grid cell with no coverage ?
  # table(area_country$All == 0)
  return(area_country)
}

#' Calculate the average occupancy per country and per time step
#'
#' @param grid a `terra::SpatVector` object with the grid
#' @param dir_sp Directory with output of occupancy model (qs)
#' @param country a `terra::SpatVector` object with the country definition
#' @param digits integer indicating the number of decimal places to be kept.
#' @returns A `data.frame` with the grid_id in rows and country in columns
#'
#' @export
#'
get_ts_country <- function(
  grid,
  dir_sp,
  country = dragon_country(),
  digits = 5
) {
  # Checking the inputs ------------
  # transform sf object into terra::vect
  if ("sf" %in% class(grid)) {
    grid <- terra::vect(grid)
  }
  stopifnot("`grid` must be a `SpatVector`." = {
    "SpatVector" %in% class(grid)
  })
  stopifnot("`grid` must contains 'polygons`." = {
    terra::is.polygons(grid)
  })
  stopifnot("`grid_id` must be in `grid`." = {
    "grid_id" %in% names(grid)
  })
  sp_list <- list.dirs(dir_sp, recursive = FALSE, full.names = FALSE)
  sp_files <- list.files(dir_sp, recursive = TRUE)
  check_psi <- file.path(sp_list, paste0("psi_", sp_list, ".qs"))
  stopifnot(
    "All species must have a psi_genus_species.qs file" = {
      all(check_psi %in% sp_files)
    }
  )
  check_psi_coef <- file.path(sp_list, paste0("psi_coef_", sp_list, ".qs"))
  stopifnot(
    "All species must have a psi_coef_genus_species.qs file" = {
      all(check_psi_coef %in% sp_files)
    }
  )
  if ("sf" %in% class(country)) {
    country <- terra::vect(country)
  }
  stopifnot("`country` must be a `SpatVector`." = {
    "SpatVector" %in% class(country)
  })
  stopifnot("`admin` must be in `country`." = {
    "admin" %in% names(country)
  })
  area_country <- get_area_country(grid, country)
  # Make a large
  tsout <- list()
  for (i in sp_list) {
    # load psi data
    psi_file <- file.path(dir_sp, i, paste0("psi_", i, ".qs"))
    dfi <- qs2::qs_read(psi_file)
    # rapid check
    msg <- paste0(
      psi_file,
      " must have columns `median`, `grid_id`, `year`."
    )
    stopifnot(msg = {
      all(c("median", "grid_id", "year") %in% names(dfi))
    })

    # load psi coef
    coef_file <- file.path(dir_sp, i, paste0("psi_coef_", i, ".qs"))
    dfi2 <- qs2::qs_read(coef_file)
    # rapid check
    msg <- paste0(
      paste0("psi_coef_", i, ".qs"),
      " must have large_variable `beta_psi_time`."
    )
    stopifnot(msg = {
      "beta_psi_time" %in% dfi2$large_variable
    })
    psi_time <- dfi2[dfi2$large_variable == "beta_psi_time", ]

    for (y in sort(unique(dfi$year))) {
      dfy <- dfi[dfi$year == y, c("grid_id", "median")]
      med <- dfy$median[match(row.names(area_country), dfy$grid_id)]
      # replace NA by 0
      med[is.na(med)] <- 0

      outi <- data.frame(
        "species" = i,
        "year" = y,
        "country" = names(area_country),
        "mean" = sapply(1:ncol(area_country), function(i) {
          stats::weighted.mean(med, area_country[, i], na.rm = TRUE)
        })
      )
      # add psi_time
      psi_y <- data.frame(
        "species" = i,
        "year" = y,
        "country" = "psi_time",
        "mean" = ifelse(
          y %in% psi_time$variable_name,
          psi_time$mean[psi_time$variable_name == y],
          NA
        )
      )

      tsout[[length(tsout) + 1]] <- rbind(outi, psi_y)
    }
  }
  # merge into a data.frame
  df <- do.call(rbind, tsout)
  # round values to make file smaller (does that actually work?)
  df$mean <- round(df$mean, digits)
  return(df)
}
