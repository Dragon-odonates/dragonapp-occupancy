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