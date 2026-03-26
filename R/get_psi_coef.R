#' Merge the phenological data per day
#'
#' @param dir_sp Directory with output of occupancy model (qs)
#' @param digits integer indicating the number of decimal places to be kept.
#' @returns A `data.frame` with the grid_id in rows and country in columns
#'
#' @export
#'
get_psi_coef <- function(
  dir_sp,
  digits = 5
) {
  sp_list <- list.dirs(dir_sp, recursive = FALSE, full.names = FALSE)
  sp_files <- list.files(dir_sp, recursive = TRUE)
  check_psi_coef <- file.path(sp_list, paste0("psi_coef_", sp_list, ".qs"))
  stopifnot(
    "All species must have a psi_coef_genus_species.qs file" = {
      all(check_psi_coef %in% sp_files)
    }
  )
  coef_out <- list()
  for (i in sp_list) {
    # load pheno data
    psi_file <- file.path(dir_sp, i, paste0("psi_coef_", i, ".qs"))
    dfi <- qs2::qs_read(psi_file)

    # check that needed variables are present
    needed_var <- c("beta_psi_env", "beta_psi_clc", "beta_psi_time")
    msg <- paste0(
      paste0("psi_coef_", i, ".qs"),
      " must have large_variable: ",
      paste0(needed_var, collapse = ", "),
      "."
    )
    stopifnot(msg = {
      all(needed_var %in% dfi$large_variable)
    })
    psi_time <- dfi[dfi$large_variable == "beta_psi_time", ]
    # format it
    dfi <- dfi[dfi$large_variable %in% needed_var, ]
    dfi$variable_name <- ifelse(
      dfi$large_variable == "beta_psi_clc",
      paste0("clc_", dfi$variable_name),
      as.character(dfi$variable_name)
    )

    out <- data.frame(
      "var" = dfi$variable_name,
      "species" = i,
      "median" = round(dfi$median, digits),
      "qmin" = round(dfi$qmin, digits),
      "qmax" = round(dfi$qmax, digits)
    )
    coef_out[[length(coef_out) + 1]] <- out
  }
  # merge into a data.frame
  df <- do.call(rbind, coef_out)
  return(df)
}
