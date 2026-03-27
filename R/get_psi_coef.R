#' Merge the phenological data per day
#'
#' @param sp_list List of species directories to read occupancy from
#' @param digits integer indicating the number of decimal places to be kept.
#' @returns A `data.frame` with the grid_id in rows and country in columns
#'
#' @export
#'
get_psi_coef <- function(
  sp_list,
  digits = 5
) {

  coef_out <- list()
  for (dir_i in sp_list) {
    i <- basename(dir_i)
    # load psi_coef data
    dfi <- qs2::qs_read(file.path(dir_i, paste0("psi_coef_", i, ".qs")))

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
