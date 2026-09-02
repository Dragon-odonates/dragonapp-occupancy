#' Get bioclim curve
#'
#' Get values computed from bioclimatic coefficients that make the curve
#' 
#' @param psi_coef psi coefficients file (original format)
#' @param bioclim_seq List of bioclimatic variables values (names correspond to bioclimatic variables)
#' @param digits integer indicating the number of decimal places to be kept.
#' @param scaling Scaling data (must have values corresponding to bioclimatic variables in the column variable)
#'
#' @returns A dataframe with columns median, qmin, qmax, x_scaled, var (name of the bioclimatic variable) and species
#' @export
get_bioclim_curve <- function(bioclim_seq, psi_coef, scaling, digits = 5) {
  sp_all <- unique(psi_coef$species)
  
  df_all <- list()
  for (i in 1:length(sp_all)) {
    # Subset species
    sp <- sp_all[i]
    psi_coef_sp <- psi_coef[psi_coef$species == sp, ]
    
    dfi <- data.frame()
    # Compute curve for each variable
    for(name in names(bioclim_seq)) {
      x <- bioclim_seq[[name]]
      median <- x*psi_coef_sp[psi_coef_sp$var == name, "median"] + x^2*psi_coef_sp[psi_coef_sp$var == paste(name, "sq", sep = "_"), "median"]
      qmin <- x*psi_coef_sp[psi_coef_sp$var == name, "qmin"] + x^2*psi_coef_sp[psi_coef_sp$var == paste(name, "sq", sep = "_"), "qmin"]
      qmax <- x*psi_coef_sp[psi_coef_sp$var == name, "qmax"] + x^2*psi_coef_sp[psi_coef_sp$var == paste(name, "sq", sep = "_"), "qmax"]
      
      dmin <- median - qmin
      dmax <- qmax - median
      
      df <- data.frame(
        var = name, 
        species = sp,
        x_scaled = x,
        median = round(median, digits), 
        qmin = round(qmin, digits), 
        qmax = round(qmax, digits),
        dmin = round(dmin, digits),
        dmax = round(dmax, digits))
      df$popup <- paste0(
        "<b>",
        df$var,
        "</b><br>median: ",
        df$median,
        "<br>CI: [",
        df$qmin,
        ":",
        df$qmax,
        "]"
      )
      
      # Scale values back
      df <- merge(df, scaling, by.x = "var", by.y = "variable", all.x = TRUE)
      df$x <- df$x_scaled * df$scale + df$center
      df$center <- df$scale <- NULL
      
      dfi <- rbind(dfi, df)
    }
    df_all[[length(df_all) + 1]] <- dfi
  }
  
  df_all <- do.call(rbind, df_all)
  
  return(df_all)
}