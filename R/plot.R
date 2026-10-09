#' plot_ly_scatter
#'
#' @param df dataframe with plotting data. Must have columns x, qmin, qmax, dmin, dmax, median and popup.
#'
#' @returns A plot_ly plot with pojnts and error bars
#' @export
plot_ly_scatter <- function(df) {
  res <- plot_ly(
    data = df,
    x = ~var,
    y = ~median,
    type = 'scatter',
    mode = 'markers',
    marker = list(color = 'rgb(0,100,80)'),
    text = ~popup,
    hoverinfo = 'text',
    error_y = list(
      type = "data",
      symmetric = FALSE,
      array = ~dmax,
      arrayminus = ~dmin,
      color = 'rgb(0,100,80)'
    )
  ) |>
    layout(
      xaxis = list(title = 'Variables'),
      yaxis = list(title = '')
    ) |>
    config(
      modeBarButtons = list(list("toImage")),
      displaylogo = FALSE
    )
  return(res)
}

#' plot_ly_lines
#'
#' @param df dataframe with plotting data. Must have columns x, qmin, qmax, median and popup.
#'
#' @returns A plot_ly plot with lines and error ribbon
#' @export
plot_ly_lines <- function(df) {
  res <- plot_ly(
    df,
    x = ~x,
    y = ~qmax,
    type = 'scatter',
    mode = 'lines',
    line = list(color = 'transparent'),
    showlegend = FALSE,
    name = 'qmax',
    hoverinfo = 'none'
  ) |>
    add_trace(
      x = ~x,
      y = ~qmin,
      type = 'scatter',
      mode = 'lines',
      fill = 'tonexty',
      fillcolor = 'rgba(0,100,80,0.2)',
      line = list(color = 'transparent'),
      showlegend = FALSE,
      name = 'qmin',
      hoverinfo = 'none'
    ) |>
    add_trace(
      x = ~x,
      y = ~median,
      type = 'scatter',
      mode = 'lines',
      line = list(color = 'rgb(0,100,80)'),
      name = 'median',
      text = ~popup,
      hoverinfo = 'text'
    ) |>
    layout(
      yaxis = list(title = ''),
      hovermode = "x unified"
    ) |>
    config(
      modeBarButtons = list(list("toImage")),
      displaylogo = FALSE
    )
  return(res)
}


# ggplot ------------------------------------------------------------------

#' Plot trend
#' 
#' Plot occupancy trends
#'
#' @param dat_psi A data.frame with columns year, mean, qmin, qmax, country
#'
#' @returns A ggplot (created with ggiraph) with one trend line + CI pre country
#' 
#' @export
plot_trend <- function(dat_psi) {
  country_u <- unique(dat_psi$country)
  years_u <- unique(dat_psi$year)
  
  if (length(country_u) > 1) {
    # Several countries
    cols <- scales::hue_pal()(length(country_u))
    names(cols) <- country_u
    cols["European trend"] <- "black"
  } else {
    cols <- "black"
    names(cols) <- country_u
  }
  
  
  lw <- stats::setNames(rep(0.6, length(country_u)), country_u)
  lw["European trend"] <- 1 
  
  g <- ggplot(dat_psi, aes(x = year, group = country)) +
    geom_ribbon(aes(ymin = qmin, ymax = qmax, fill = country),
                alpha = 0.2, show.legend = FALSE) +
    geom_line_interactive(aes(
      y = mean, colour = country, linewidth = country,
      tooltip = country)) +
    scale_color_manual(values = cols) +
    scale_fill_manual(values = cols) +
    scale_linewidth_manual(values = lw) +
    xlab("Year") +
    ylab("Mean occupancy") +
    scale_x_continuous(breaks = seq(min(years_u), max(years_u), by = 4)) +
    theme(legend.title = element_blank(),
          legend.position = "bottom")
  return(g)
}



# Leaflet -----------------------------------------------------------------

#' Plot base map
#'
#' @param map_data the map data
#' @param Zmin min zoom
#' @param Zmax max zoom
#' @param bounds bounds (xmin, ymin, xmax, ymax)
#'
#' @returns a leaflet map
#' @export
plot_base_map <- function(map_data, Zmin, Zmax, bounds) {
  bounds <- unname(bounds)
  m <- leaflet(map_data, options = leafletOptions(minZoom = Zmin, maxZoom = Zmax)) |>
    addTiles() |>
    fitBounds(bounds[1], bounds[2], bounds[3], bounds[4])
  return(m)
}


#' Plot polygons on map
#'
#' @param leaflet_map A leaflet map (e.g. created with `plot_base_map`)
#' @param map_type Map type (slope or average)
#' @param map_data Data to use for plotting polygons
#' @param layer_id layer id for the plotted polygons
#' @param leg_names A named vector for the legend names 
#' (names are the titles to display, values correspond to `map_type`)
#'
#' @returns A leaflet map
#' @export
plot_polygons_map <- function(leaflet_map, 
                              map_type, 
                              map_data, 
                              leg_names,
                              layer_id = "mapid") {
  
  # Get the columns to plot depending on type
  ind <- names(map_data)[grepl(map_type, names(map_data))]
  
  # Get legend title
  leg <- names(leg_names[leg_names == map_type])
  
  if (map_type == "slope") {
    max_abs <- max(abs(data.frame(map_data)[, ind]), na.rm = TRUE)

    pal <- leaflet::colorNumeric(
      palette = "RdBu",
      domain = c(-max_abs, max_abs),
      na.color = "transparent"
    )
  } else {
    pal <- leaflet::colorNumeric(
      palette = "viridis",
      domain = unlist(data.frame(map_data)[, ind]),
      na.color = "transparent"
    )
  }
  
  m <- leaflet_map |>
    removeGlPolygons(layerId = layer_id) |>
    addGlPolygons(
      data = map_data,
      fillColor = pal(map_data[[ind]]),
      fillOpacity = 0.7,
      popup = map_data[[ind]],
      layerId = layer_id
    ) |>
    clearControls() |>
    # fmt:skip
    addLegend_decreasing(
      position = "bottomright",
      values = map_data[[ind]],
      pal = pal,
      opacity = 1,
      title = leg,
      decreasing = TRUE,
      percent = (map_type == "slope")
    )
  
  return(m)
}