#' Merge the phenological data per day
#'
#' @param dir_sp Directory with output of occupancy model (qs)
#' @param digits integer indicating the number of decimal places to be kept.
#' @returns A `data.frame` with the grid_id in rows and country in columns
#'
#' @export
#'
get_pheno <- function(
  dir_sp,
  digits = 5
) {
  sp_list <- list.dirs(dir_sp, recursive = FALSE, full.names = FALSE)
  sp_files <- list.files(dir_sp, recursive = TRUE)
  check_pheno <- file.path(sp_list, paste0("pheno_", sp_list, ".qs"))
  stopifnot(
    "All species must have a psi_genus_species.qs file" = {
      all(check_pheno %in% sp_files)
    }
  )
  tsout <- list()
  for (i in sp_list) {
    # load pheno data
    psi_file <- file.path(dir_sp, i, paste0("pheno_", i, ".qs"))
    dfi <- qs2::qs_read(psi_file)
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
