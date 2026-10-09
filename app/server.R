function(input, output, session) {
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
    if (input$spe %in% df$species & all(input$country %in% df$country)) {
      sdf <- df[df$species == input$spe & df$country %in% input$country, ]
    } else {
      sdf <- df[df$species == sort(df$species)[1] & df$country == df$country[1], ]
    }
    return(sdf)
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
    # Map backgroud
    req(input$spe)
    plot_base_map(pt, Zmin, Zmax, bounds = bb)
  })

  observe({
    # Map reactive update
    leafletProxy("mapdistri") |>
      plot_polygons_map(
        map_type = input$map, 
        map_data = sub_pt(), 
        leg_names = legend_name()
        )
  })

  # Trends per species ------------------------------------------------------
  output$countryts <- renderGirafe({
    g <- plot_trend(sub_ts())
    girafe(ggobj = g,
           # Set aspect ratio
           width_svg  = 7,
           height_svg = 5,
           options = list(opts_sizing(rescale = TRUE, width = 1))
           )
    
  })
  
  # Detection ---------------------------------------------------------------
  ## Coefficients -----
  output$pcoef <- renderPlotly({
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


