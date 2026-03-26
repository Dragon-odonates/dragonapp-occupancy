#' Merge the phenological data per day
#'
#' @param sp_list List of species directories to read occupancy from
#' @param digits integer indicating the number of decimal places to be kept.
#' @returns A `data.frame` with the grid_id in rows and country in columns
#'
#' @export
#'
get_pheno <- function(
  sp_list,
  digits = 5
) {

  tsout <- list()
  for (dir_i in sp_list) {
    i <- basename(dir_i)
    # load pheno data
    dfi <- qs2::qs_read(file.path(dir_i, paste0("pheno_", i, ".qs")))
    # format it
    out <- data.frame(
      "doy" = dfi$doy,
      "species" = i,
      "median" = round(dfi$median, digits),
      "qmin" = round(dfi$qmin, digits),
      "qmax" = round(dfi$qmax, digits)
    )
    tsout[[length(tsout) + 1]] <- out
  }
  # merge into a data.frame
  df <- do.call(rbind, tsout)
  return(df)
}
