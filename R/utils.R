# calculate slope -------------------------------
#' calculate linear slope of a numeric vector
#'
#' @param x numeric vector
#'
#' @export
get_slope <- function(x) {
  if (sum(is.na(x)) <= length(x) * 0.5) {
    df <- data.frame(
      x = seq_along(x),
      y = as.numeric(x)
    )
    out <- stats::coef(stats::lm(y ~ x, df))[2]
    return(as.numeric(out))
  } else {
    return(NA)
  }
}

# get first element -------------------------------
#' get the first element of a string
#'
#' @param x string vector
#' @param split separator
#'
#' @export
first_str <- function(x, split = ";") {
  strsplit(x, split = split) |>
    sapply(function(y) y[[1]])
}

# get last element -------------------------------
#' get the last element of a string
#'
#' @param x string vector
#' @param split separator
#'
#' @export
last_str <- function(x, split = ";") {
  strsplit(x, split = split) |>
    sapply(function(y) y[[length(y)]])
}

dragon_country <- function() {
  rnaturalearth::ne_countries(
    country = c(
      "Austria",
      "Belgium",
      "Cyprus",
      "Czechia",
      "Denmark",
      "Finland",
      "France",
      "Germany",
      "Ireland",
      "Isle of Man",
      "Italy",
      "Luxembourg",
      "Netherlands",
      "Norway",
      "Portugal",
      "Slovenia",
      "Spain",
      "Sweden",
      "Switzerland",
      "United Kingdom"
    ),
    scale = 10
  )
}

# add decreasing legend in Leaflet ------------
#' Customized leaflet::addLegend() function to add a color legend to a map
#'
#' When a color palette function is used in a map (e.g.,
#' [colorNumeric()]), a color legend can be automatically derived from
#' the palette function. You can also manually specify the colors and labels for
#' the legend.
#'
#' The `labFormat` argument is a function that takes the argument
#' `type = c("numeric", "bin", "quantile", "factor")`, plus, arguments for
#' different types of color palettes. For the `colorNumeric()` palette,
#' `labFormat` takes a single argument, which is the breaks of the numeric
#' vector, and returns a character vector of the same length. For
#' `colorBin()`, `labFormat` also takes a vector of breaks of length
#' `n` but should return a character vector of length `n - 1`, with
#' the `i`-th element representing the interval `c(x[i], x[i + 1])`.
#' For `colorQuantile()`, `labFormat` takes two arguments, the quantiles
#' and the associated probabilities (each of length `n`), and should return
#' a character vector of length `n - 1` (similar to the `colorBin()`
#' palette). For `colorFactor()`, `labFormat` takes one argument, the
#' unique values of the factor, and should return a character vector of the same
#' length.
#'
#' By default, `labFormat` is basically `format(scientific = FALSE,
#' big.mark = ",")` for the numeric palette, `as.character()` for the
#' factor palette, and a function to return labels of the form \samp{x[i] - x[i
#' + 1]} for bin and quantile palettes (in the case of quantile palettes,
#' `x` is the probabilities instead of the values of breaks).
#' @param map a leaflet map
#' @param position the position of the legend
#' @param pal the color palette function, generated from
#'   [colorNumeric()], `colorBin()`, `colorQuantile()`, or
#'   `colorFactor()`
#' @param values the values used to generate colors from the palette function
#' @param na.label the legend label for `NA`s in `values`
#' @param bins an approximate number of tick-marks on the color gradient for the
#'   `colorNumeric` palette if it is of length one; you can also provide a
#'   numeric vector as the pre-defined breaks (equally spaced)
#' @param colors a vector of (HTML) colors to be used in the legend if
#'   `pal` is not provided
#' @param opacity the opacity of colors
#' @param labels a vector of text labels in the legend corresponding to
#'   `colors`
#' @param labFormat a function to format the labels derived from `pal` and
#'   `values` (see Details below to know what `labelFormat()` returns
#'   by default; you can either use the helper function `labelFormat()`, or
#'   write your own function)
#' @param title the legend title
#' @param className extra CSS classes to append to the control, space separated
#' @param layerId the ID of the legend; subsequent calls to `addLegend()`
#'   or `addControl()` with the same `layerId` will replace this
#'   legend. The ID can also be used with `removeControl()`.
#' @param group `group` name of a leaflet layer group.
#'   Supplying this value will tie the legend to the leaflet layer group
#'   with this name and will auto add/remove the legend as the
#'   group is added/removed, for example via `layerControl()`.
#'   You will need to set the `group` when you add a layer
#'   (e.g., [addPolygons()]) and supply the same name here.
#' @param data data from the map
#' @param decreasing whether the legend should be in decreasing order
#'
#' @export
addLegend_decreasing <- function(
  map,
  position = c("topright", "bottomright", "bottomleft", "topleft"),
  pal,
  values,
  na.label = "NA",
  bins = 7,
  colors,
  opacity = 0.5,
  labels = NULL,
  labFormat = leaflet::labelFormat(),
  title = NULL,
  className = "info legend",
  layerId = NULL,
  group = NULL,
  data = leaflet::getMapData(map),
  decreasing = FALSE
) {
  position <- match.arg(position)
  type <- "unknown"
  na.color <- NULL
  extra <- NULL
  if (!missing(pal)) {
    if (!missing(colors)) {
      stop("You must provide either 'pal' or 'colors' (not both)")
    }
    if (missing(title) && inherits(values, "formula")) {
      title <- deparse(values[[2]])
    }
    values <- leaflet::evalFormula(values, data)
    type <- attr(pal, "colorType", exact = TRUE)
    args <- attr(pal, "colorArgs", exact = TRUE)
    na.color <- args$na.color
    if (
      !is.null(na.color) && grDevices::col2rgb(na.color, alpha = TRUE)[[4]] == 0
    ) {
      na.color <- NULL
    }
    if (type != "numeric" && !missing(bins)) {
      warning("'bins' is ignored because the palette type is not numeric")
    }
    if (type == "numeric") {
      cuts <- if (length(bins) == 1) {
        pretty(values, bins)
      } else {
        bins
      }
      if (length(bins) > 2) {
        if (
          !all(abs(diff(bins, differences = 2)) <= sqrt(.Machine$double.eps))
        ) {
          stop("The vector of breaks 'bins' must be equally spaced")
        }
      }
      n <- length(cuts)
      r <- range(values, na.rm = TRUE)
      cuts <- cuts[cuts >= r[1] & cuts <= r[2]]
      n <- length(cuts)
      p <- (cuts - r[1]) / (r[2] - r[1])
      extra <- list(p_1 = p[1], p_n = p[n])
      p <- c("", paste0(100 * p, "%"), "")
      if (decreasing == TRUE) {
        colors <- pal(rev(c(r[1], cuts, r[2])))
        labels <- rev(labFormat(type = "numeric", cuts))
      } else {
        colors <- pal(c(r[1], cuts, r[2]))
        labels <- rev(labFormat(type = "numeric", cuts))
      }
      colors <- paste(colors, p, sep = " ", collapse = ", ")
    } else if (type == "bin") {
      cuts <- args$bins
      n <- length(cuts)
      mids <- (cuts[-1] + cuts[-n]) / 2
      if (decreasing == TRUE) {
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "bin", cuts))
      } else {
        colors <- pal(mids)
        labels <- labFormat(type = "bin", cuts)
      }
    } else if (type == "quantile") {
      p <- args$probs
      n <- length(p)
      cuts <- stats::quantile(values, probs = p, na.rm = TRUE)
      mids <- stats::quantile(values, probs = (p[-1] + p[-n]) / 2, na.rm = TRUE)
      if (decreasing == TRUE) {
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "quantile", cuts, p))
      } else {
        colors <- pal(mids)
        labels <- labFormat(type = "quantile", cuts, p)
      }
    } else if (type == "factor") {
      v <- sort(unique(stats::na.omit(values)))
      colors <- pal(v)
      labels <- labFormat(type = "factor", v)
      if (decreasing == TRUE) {
        colors <- pal(rev(v))
        labels <- rev(labFormat(type = "factor", v))
      } else {
        colors <- pal(v)
        labels <- labFormat(type = "factor", v)
      }
    } else {
      stop("Palette function not supported")
    }
    if (!any(is.na(values))) {
      na.color <- NULL
    }
  } else {
    if (length(colors) != length(labels)) {
      stop("'colors' and 'labels' must be of the same length")
    }
  }
  legend <- list(
    colors = I(unname(colors)),
    labels = I(unname(labels)),
    na_color = na.color,
    na_label = na.label,
    opacity = opacity,
    position = position,
    type = type,
    title = title,
    extra = extra,
    layerId = layerId,
    className = className,
    group = group
  )
  leaflet::invokeMethod(map, data, "addLegend", legend)
}
