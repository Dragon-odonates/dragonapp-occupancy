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
