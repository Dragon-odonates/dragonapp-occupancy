#' Get bioclim seq
#'
#' @param env Environment data (must have bioclimatic columns as bioxx_... columns)
#' @param length.out Length of the sequence of values to get
#'
#' @returns List of sequences of bioclim variables (named with the variables)
get_bioclim_seq <- function(env, length.out = 100) {
  # Get sequence of bioclim
  
  bioclim <- env[, grep("^bio", colnames(env), value = TRUE)]
  bioclim_range <- apply(bioclim, 2, range)
  bioclim_seq <- lapply(1:ncol(bioclim_range),
                        function(i) seq(bioclim_range[1, i], bioclim_range[2, i], length.out = length.out))
  names(bioclim_seq) <- colnames(bioclim_range)
  return(bioclim_seq)
}