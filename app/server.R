function(input, output, session) {
  ## Reactive input ----------------
  output$inYear <- renderUI({
    sliderInput(
      "year",
      "Year",
      min = min(yr_range),
      max = max(yr_range),
      value = min(yr_range),
      step = 1,
      sep = "",
      animate = animationOptions(
        interval = 1500,
        loop = TRUE
      )
    )
  })

  ## Reactive data subset ----------
  sub_pt <- reactive({
    req(input$spe)
    # if statement to avoid issue when changing dataset
    if (any(grepl(input$spe, names(pt)))) {
      spt <- pt[grepl(input$spe, names(pt))]
    } else {
      sp_choices <- gsub(".average", "", names(pt)[grepl("average", names(pt))])
      # sort(unique(get_ts()$species))
      spt <- pt[grepl(sp_choices[1], names(pt))]
    }
    return(spt)
  })

  sub_ts <- reactive({
    req(input$spe)
    # if statement to avoid issue when changing dataset
    if (input$spe %in% df$species) {
      sdf <- df[df$species == input$spe, ]
    } else {
      sdf <- df[df$species == sort(df$species)[1], ]
    }
    return(sdf)
  })

  colpal <- reactive({
    pts <- sub_pt()
    if (input$map == "dynamic") {
      ind <- names(pts)[-c(1:4, ncol(pts))]
      # paste0(input$spe, ".", yr_shape)
    } else {
      ind <- names(pts)[grep(input$map, names(pts))[1]]
    }

    if (input$map %in% c("slope", "beta_psi", "beta_psi_slope")) {
      max_abs <- max(abs(data.frame(pts)[, ind]), na.rm = TRUE)
      pal <- leaflet::colorNumeric(
        palette = "RdYlBu",
        domain = c(-max_abs, max_abs),
        na.color = "transparent"
      )
    } else {
      pal <- leaflet::colorNumeric(
        palette = "viridis",
        domain = unlist(data.frame(pts)[, ind]),
        na.color = "transparent"
      )
    }
    return(pal)
  })

  sub_ph <- reactive({
    req(input$spe)
    # if statement to avoid issue when changing dataset
    if (input$spe %in% ph$species) {
      sph <- ph[ph$species == input$spe, ]
    } else {
      sph <- ph[ph$species == sort(ph$species)[1], ]
    }
    return(sph)
  })

  sub_env <- reactive({
    req(input$spe)
    # if statement to avoid issue when changing dataset
    if (input$spe %in% env$species) {
      senv <- env[env$species == input$spe, ]
    } else {
      senv <- env[env$species == sort(env$species)[1], ]
    }
    return(senv)
  })

  # Maps --------------------------------------------------------------------
  output$mapdistri <- renderLeaflet({
    req(input$spe)
    leaflet(pt, options = leafletOptions(minZoom = Zmin, maxZoom = Zmax)) |>
      addTiles() |>
      setView(lng = 15, lat = 55, zoom = Z)
  })

  observe({
    pts <- sub_pt()
    pal <- colpal()
    ind <- ifelse(
      input$map == "dynamic",
      names(pts)[grepl(input$year, names(pts))],
      names(pts)[grepl(input$map, names(pts))]
    )

    leg <- ifelse(input$map == "dynamic", input$year, input$map)

    leafletProxy("mapdistri", data = pts) |>
      #clearShapes() |>
      removeGlPolygons(layerId = 'mapid') |>
      addGlPolygons(
        data = pts,
        fillColor = pal(pts[[ind]]),
        fillOpacity = 0.7,
        popup = pts[[ind]],
        layerId = 'mapid'
      ) |>
      clearControls() |>
      # fmt:skip
      addLegend_decreasing(
        position = "bottomright",
        values = pts[[ind]],
        pal = pal,
        opacity = 1,
        title = leg,
        decreasing = TRUE
      )
  })

  # Trends per species ------------------------------------------------------
  output$countryts <- renderPlotly({
    req(input$year)
    dts <- sub_ts()
    # remove psi_time
    dts <- dts[dts$country != "psi_time", ]
    num_countries <- length(unique(dts$country)) - 1
    pal <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(num_countries)

    plot_ly(
      dts[dts$country != "All", ],
      x = ~year,
      y = ~mean,
      color = ~country,
      colors = pal,
      type = "scatter",
      mode = "lines+markers"
    ) |>
      add_trace(
        data = dts[dts$country == "All", ],
        x = ~year,
        y = ~mean,
        name = "all",
        type = "scatter",
        mode = "lines+markers",
        line = list(color = "black", width = 4),
        marker = list(color = "black")
      ) |>
      layout(
        xaxis = list(title = 'Year'),
        yaxis = list(title = 'Average probability'),
        shapes = list(list(
          type = "line",
          x0 = input$year,
          x1 = input$year,
          y0 = 0,
          y1 = max(dts$mean, na.rm = TRUE),
          line = list(color = "black")
        ))
      ) |>
      config(
        modeBarButtons = list(list("toImage")),
        displaylogo = FALSE
      )
  })

  # Psi_time  ------------------------------------------------------
  output$psits <- renderPlotly({
    req(input$year)
    dts <- sub_ts()

    dpsi <- dts[dts$country == "psi_time", ]
    plot_ly(
      dpsi,
      x = ~year,
      y = ~mean,
      type = "scatter",
      mode = "lines+markers",
      line = list(color = 'rgb(0,100,80)', width = 2),
      marker = list(color = 'rgb(0,100,80)')
    ) |>
      layout(
        xaxis = list(title = 'Year'),
        yaxis = list(title = ''),
        shapes = list(list(
          type = "line",
          x0 = input$year,
          x1 = input$year,
          y0 = min(dpsi$mean, na.rm = TRUE),
          y1 = max(dpsi$mean, na.rm = TRUE),
          line = list(color = "black")
        ))
      ) |>
      config(
        modeBarButtons = list(list("toImage")),
        displaylogo = FALSE
      )
  })

  # Phenology  ------------------------------------------------------
  output$phenots <- renderPlotly({
    req(input$year)
    sph <- sub_ph()
    sph$date <- as.Date(paste("2000", sph$doy), format = "%Y %j")

    sph$popup <- paste(
      "<b>median:</b>",
      sph$median,
      "<br>CI:",
      sph$qmin,
      "-",
      sph$qmax
    )
    plot_ly(
      sph,
      x = ~date,
      y = ~qmax,
      type = 'scatter',
      mode = 'lines',
      line = list(color = 'transparent'),
      showlegend = FALSE,
      name = 'qmax',
      hoverinfo = 'none'
    ) |>
      add_trace(
        x = ~date,
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
        x = ~date,
        y = ~median,
        type = 'scatter',
        mode = 'lines',
        line = list(color = 'rgb(0,100,80)'),
        name = 'median',
        text = ~popup,
        hoverinfo = 'text'
      ) |>
      layout(
        xaxis = list(
          title = 'Date',
          dtick = "M1",
          tickformat = "%b",
          ticklabelmode = "period"
        ),
        yaxis = list(title = ''),
        hovermode = "x unified"
      ) |>
      config(
        modeBarButtons = list(list("toImage")),
        displaylogo = FALSE
      )
  })

  # Env. drivers  ------------------------------------------------------
  output$envplot <- renderPlotly({
    req(input$year)
    senv <- sub_env()
    senv$dmax <- senv$qmax - senv$median
    senv$dmin <- senv$median - senv$qmin
    senv$popup <- paste(
      "<b>",
      senv$env,
      "</b><br>median:",
      senv$median,
      "<br>CI:",
      senv$qmin,
      "-",
      senv$qmax
    )
    plot_ly(
      data = senv,
      x = ~env,
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
  })
}
