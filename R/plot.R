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