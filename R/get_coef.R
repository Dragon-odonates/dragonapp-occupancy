#' Get coefficients
#'
#' @param sp_list List of species directories to read occupancy from
#' @param basename File basename (part before species name)
#' @param digits integer indicating the number of decimal places to be kept.
#'
#' @returns A `data.frame` with the grid_id in rows and country in columns
#'
#' @export
#'
get_coef <- function(
  basename,
  sp_list,
  digits = 5
) {

  coef_out <- list()
  for (dir_i in sp_list) {
    i <- basename(dir_i)
    # load psi_coef data
    dfi <- qs2::qs_read(file.path(dir_i, paste0(basename, i, ".qs")))
    
    if (basename == "p_coef_") {
      rm_var <- c("kappa", "mu0")
      all_var <- unique(dfi$large_variable)
      needed_var <- all_var[!all_var %in% rm_var]
      
      msg <- paste0(
        paste0(basename, i, ".qs"),
        " must have large_variable: ",
        paste0(needed_var, collapse = ", "),
        "."
      )
      stopifnot(msg = {
        all(needed_var %in% dfi$large_variable)
      })
      
      dfi <- dfi[dfi$large_variable %in% needed_var, ]
      
      dfi[large_variable == "p_intercept", `:=`(large_variable = "beta_p_ll",
                                                variable_name = "=1")]
    }

    out <- data.frame(
      "large_variable" = dfi$large_variable,
      "var" = dfi$variable_name,
      "species" = i,
      "median" = round(dfi$median, digits),
      "qmin" = round(dfi$qmin, digits),
      "qmax" = round(dfi$qmax, digits),
      "dmax" = round(dfi$qmax - dfi$median, digits),
      "dmin" = round(dfi$median - dfi$qmin, digits)
    )
    out$popup <- paste0(
      "<b>",
      out$var,
      "</b><br>median: ",
      out$median,
      "<br>CI: [",
      out$qmin,
      ":",
      out$qmax,
      "]"
    )
    
    coef_out[[length(coef_out) + 1]] <- out
  }
  
  # merge into a data.frame
  df <- do.call(rbind, coef_out)
  return(df)
}
