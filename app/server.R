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

    if (input$map == "slope") {
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
  
  legend_name <- reactive({
    names(leg_names[leg_names == input$map])
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

  sub_coef <- reactive({
    req(input$spe)
    # if statement to avoid issue when changing dataset
    if (input$spe %in% psicoef$species) {
      scoef <- psicoef[psicoef$species == input$spe, ]
    } else {
      scoef <- psicoef[psicoef$species == sort(psicoef$species)[1], ]
    }
    return(scoef)
  })
  
  sub_bio <- reactive({
    req(input$spe)
    # if statement to avoid issue when changing dataset
    if (input$spe %in% psibio$species) {
      sbio <- psibio[psibio$species == input$spe, ]
    } else {
      sbio <- psibio[psibio$species == sort(psibio$species)[1], ]
    }
    return(sbio)
  })
  
  sub_p_coef <- reactive({
    req(input$spe)
    # if statement to avoid issue when changing dataset
    if (input$spe %in% pcoef$species) {
      scoef <- pcoef[pcoef$species == input$spe, ]
    } else {
      scoef <- pcoef[pcoef$species == sort(pcoef$species)[1], ]
    }
    return(scoef)
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

    leg <- ifelse(input$map == "dynamic", paste0(legend_name(), " (", input$year, ")"), legend_name())

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
        decreasing = TRUE,
        percent = input$map == "slope"
      )
  })

  # Trends per species ------------------------------------------------------
  output$countryts <- renderPlotly({
    req(input$year)
    dts <- sub_ts()
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
        xaxis = list(title = 'Year',
                     tickvals = seq(2000, 2024, by = 4)),
        yaxis = list(title = 'Mean occupancy probability'),
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


  # Detection ---------------------------------------------------------------
  ## Coefficients -----
  output$pcoef <- renderPlotly({
    req(input$year)
    scoef <- sub_p_coef()
    lv <- unique(scoef$large_variable)
    
    if (length(lv) == 1) {
      res <- plot_ly_scatter(scoef) |> 
        layout(xaxis = list(title = lv))
    } else {
      plist <- vector(mode = "list", length = length(lv))
      for (i in 1:length(lv)) {
        pl <- plot_ly_scatter(scoef[scoef$large_variable == lv[i],]) |> 
          layout(showlegend = FALSE,
                 xaxis = list(title = lv[i]))
        plist[[i]] <- pl
      }
      res <- plotly::subplot(plist,
                      titleX = TRUE,
                      nrows = 2,
                      margin = 0.08)
    }
    return(res)
  })
  
  ## Phenology -----
  output$phenots <- renderPlotly({
    req(input$year)
    sph <- sub_ph()
    sph$x <- as.Date(paste("2000", sph$doy), format = "%Y %j")

    sph$popup <- paste0(
      "<b>",
      format(sph$x, "%d %b"),
      "</b> <br>median: ",
      sph$median,
      "<br>CI: [",
      sph$qmin,
      ":",
      sph$qmax,
      "]"
    )
    plot_ly_lines(sph) |> 
      layout(
        xaxis = list(
          title = 'Date',
          dtick = "M1",
          tickformat = "%b",
          ticklabelmode = "period"
        ),
        yaxis = list(title = ''),
        hovermode = "x unified"
      )
  })


  # Occupancy ---------------------------------------------------------------
  
  ## Other coefs -----
  output$psicoef_plot <- renderPlotly({
    req(input$year)
    scoef <- sub_coef()
    # scoef <- scoef[nchar(scoef$var) > 4, ]
    lv <- unique(scoef$large_variable)
    
    plist <- vector(mode = "list", length = length(lv))
    for (i in 1:length(lv)) {
      pl <- plot_ly_scatter(scoef[scoef$large_variable == lv[i],]) |> 
        layout(showlegend = FALSE,
               xaxis = list(title = lv[i],
                            matches = NULL))
      if (lv[i] %in% c("beta_psi_gsslope", "psi_intercept")) {
        pl <- pl |> 
          layout(xaxis = list(showticklabels = FALSE,
                              title = lv[i],
                              matches = NULL))
      }
      plist[[i]] <- pl
    }
    sub1 <- plotly::subplot(plist[1:2],
                            widths = c(0.2, 0.8),
                            titleX = TRUE,
                            nrows = 1,
                            margin = 0.04)
    sub2 <- plotly::subplot(plist[3:4],
                            widths = c(0.5, 0.5),
                            titleX = TRUE,
                            nrows = 1,
                            margin = 0.04)
    
    plotly::subplot(sub1, sub2, 
                    nrows = 2,
                    titleX = TRUE,
                    margin = 0.08)
  })
  
  ## Bioclim -----
  output$bioclim_plot <- renderPlotly({
    req(input$year)
    
    sbio <- sub_bio()
    
    ubio <- unique(sbio$var)
    plist <- vector(mode = "list", length = length(ubio))
    for (i in 1:length(ubio)) {
      pl <- plot_ly_lines(sbio[sbio$var == ubio[i],]) |> 
        layout(xaxis = list(title = ubio[i]))
      plist[[i]] <- pl
    }
    plotly::subplot(plist,
                    titleX = TRUE,
                    nrows = 1,
                    margin = 0.04)
  })

}


