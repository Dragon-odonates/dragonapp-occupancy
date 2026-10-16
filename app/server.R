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
    validate(
      # Print a message if no country is selected
      need(input$country != "", "Please select at least one country")
    )
    
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

  sub_psi_coef <- reactive({
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
  output$pcoef <- renderPlot({
    gglist <- plot_coef(sub_p_coef(), grid = grid, countries = countries)
    patchwork::wrap_plots(gglist)
  })
  
  ## Phenology -----
  output$phenots <- renderPlot({
    plot_errorline(sub_ph(), x = "x", ggtitle = "Phenology") +
      scale_x_date(date_labels = "%d %b")
  })


  # Occupancy ---------------------------------------------------------------
  
  ## Bioclim -----
  output$bioclim_plot <- renderPlot({
    
    sbio <- sub_bio()
    
    gglist <- list()
    for (v in unique(sbio$var)) {
      sub_bio_sub <- sbio[var == v, ]
      g <- plot_errorline(sub_bio_sub, x = "x", ggtitle = v)
      gglist <- c(gglist, g)
    }
    patchwork::wrap_plots(gglist, nrow = 1)
  })
  
  ## Other coefs -----
  output$psicoef_plot <- renderPlot({
    gglist <- plot_coef(sub_psi_coef(), grid = grid, countries = countries)
    patchwork::wrap_plots(gglist)
  })
  
}


