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
    geom_ribbon_interactive(
      aes(ymin = qmin, ymax = qmax, fill = country, tooltip = country),
      alpha = 0.2, show.legend = FALSE
      ) +
    geom_line(aes(y = mean, colour = country, linewidth = country)) +
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


#' Plot coefficients
#'
#' @param coef_dt coefficients data.table
#' @param grid spatial grid (must have `grid_id`)
#' @param variable column name containing variables (x-axis)
#' @param large_variable_col column name containing large variables (each ggplot from the list = 'facets')
#' @param min column name containing min quantile
#' @param max column name containing max quantile
#' @param reorder_var variables that should be reordered along the x-axis
#' @param var_spatial large variables that have spatial values (to be displayed as map)
#' @param plot_hline plot horizontal line at zero?
#'
#' @returns a list of ggplot
#' @export
plot_coef <- function(
    coef_dt, 
    grid,
    variable = "var",
    large_variable_col = "large_variable",
    min = "qmin",
    max = "qmax",
    reorder_var = NULL,
    var_spatial = c("beta_psi_gssite", "beta_psi_gsslope", "beta_p_site"),
    plot_hline = FALSE) {
  
  # Initialize empty plot list
  gg <- list()
  
  for (lv in sort(unique(coef_dt[[large_variable_col]]))) {
    # One plot for each large_variable
    
    # Filter for that level
    df_sub <- coef_dt[coef_dt[[large_variable_col]] == lv, ]
    
    if (lv %in% var_spatial) {
      g <- plot_spatial(
        dt = df_sub, 
        variable = variable,
        grid = grid,
        ggtitle = lv)
    } else {
      if (lv %in% reorder_var) {
        reorder <- TRUE
      } else {
        reorder <- FALSE
      }
      g <- plot_errorbar(
        df_sub,
        variable = variable,
        min = min,
        max = max,
        ggtitle = lv,
        reorder = reorder,
        plot_hline = plot_hline)
    }
    
    
    gg <- c(gg, list(g))
  }
  return(gg)
}


#' Plot errorbar
#' 
#' Plot coefficients with error bars
#'
#' @param dt data.table with coefficients
#' @param variable column name containing variables (x-axis)
#' @param min column name containing min quantile
#' @param max column name containing max quantile
#' @param reorder reorder the x-axis?
#' @param plot_hline plot horizontal line at zero?
#' @param ggtitle the plot title
#'
#' @returns a ggplot
#' @export
plot_errorbar <- function(
    dt, 
    variable = "var",
    min = "qmin",
    max = "qmax",
    ggtitle = NULL,
    reorder = FALSE,
    plot_hline = FALSE) {
  
  if (reorder) {
    data.table::setorder(dt, median)
    data.table::set(
      dt, 
      j = variable, 
      value = factor(dt[[variable]], levels = dt[[variable]])
    )
  }
  
  g <- ggplot(dt,
              aes(y = median, x = .data[[variable]])) +
    geom_errorbar_interactive(
      data = dt,
      aes(
        ymin = .data[[min]],
        ymax = .data[[max]],
        tooltip = paste0(
          .data[[variable]], "\n",
          "value: ", round(median, 2),
          " [", round(.data[[min]], 2), "; ",
          round(.data[[max]], 2), "]"
        )
      ),
      alpha = .3) +
    geom_point() +
    ggtitle(ggtitle) +
    theme(axis.title = element_blank())
  
  if (nrow(dt) > 5) {
    g <- g + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  if (plot_hline) {
    g <- g + ggplot2::geom_hline(aes(yintercept = 0),
                                 linetype = "dashed")
  }
  
  if (nrow(dt) > 50) {
    g <- g + 
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank())
  }
  return(g)
}

#' Plot errorbar
#' 
#' Plot coefficients with error bars
#'
#' @param dt data.table with coefficients
#' @param min column name containing min quantile
#' @param max column name containing max quantile
#' @param plot_hline plot horizontal line at zero?
#' @param ggtitle the plot title
#' @param x the variable to use for the x axis
#'
#' @returns a ggplot
#' @export
plot_errorline <- function(
    dt, 
    x,
    min = "qmin",
    max = "qmax",
    ggtitle = NULL,
    plot_hline = FALSE) {
  
  g <- ggplot(dt,
              aes(y = median, x = .data[[x]])) +
    geom_ribbon(
      data = dt,
      aes(
        ymin = .data[[min]],
        ymax = .data[[max]]
      ),
      alpha = .3) +
    geom_line() +
    ggtitle(ggtitle) +
    theme(axis.title = element_blank())
  
  if (plot_hline) {
    g <- g + ggplot2::geom_hline(aes(yintercept = 0),
                                 linetype = "dashed")
  }
  
  return(g)
}

#' Plot spatial
#' 
#' Plot spatial coefficients
#'
#' @param dt a data.table
#' @param grid a grid (must have column grid_id)
#' @param range values range 
#' @param legend_title legend title
#' @param na_color color for NA grid values
#' @param ggtitle the plot title
#' @param variable Name of the variable column containing `grid_id` (in the `grid`)
#'
#' @returns a ggplot
#' @export
plot_spatial <- function(
    dt, 
    grid,
    variable = "var",
    ggtitle = NULL,
    range = NULL,
    legend_title = NULL,
    na_color = "grey") {
  
  # Put data on grid
  grid_var <- grid |> 
    dplyr::left_join(dt, by = c("grid_id" = variable))
  
  # Get bbox
  bbox <- st_bbox(grid_var)
  
  g <- ggplot() +
    geom_sf(data = grid_var, aes(fill = .data[["median"]]), color = "transparent") +
    coord_sf(xlim = bbox[c("xmin", "xmax")],
             ylim = bbox[c("ymin", "ymax")]) +
    theme_void() +
    theme(legend.ticks = element_blank()) +
    scale_fill_gradient2(
      name = legend_title,
      low = "darkred",
      mid = "white",
      high = "darkblue",
      na.value = na_color,
      midpoint = 0) +
    ggtitle(ggtitle)
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
    leafgl::removeGlPolygons(layerId = layer_id) |>
    leafgl::addGlPolygons(
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