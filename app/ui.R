fluidPage(
       # run bslib::bs_theme_preview() to customize
       # theme = bs_theme(preset = "cosmo"),
       tags$head(
         tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
         ),

       # Application title
       titlePanel(
         title = tags$div(
           htmltools::tags$img(src = "dragon_logo.png", height = "50px", style = "margin-right: 10px;"),
           "Distribution of odonates across Europe",
           htmltools::tags$span("DRAGON, FRB-CESAB", class = "subtitle")
         ),
         windowTitle = "Distribution of odonates across Europe"
       ),
       tabsetPanel(
              id = 'main',
              tabPanel(
                     "Distribution",
                     fluidRow(
                            column(
                                   6,
                                   selectInput(
                                     "spe",
                                     "Species",
                                     choices = sp_choices,
                                     selected = sp_choices[1],
                                     multiple = FALSE
                                     ),
                                   br()
                            ),
                            column(
                                   3,
                                   htmltools::tags$span(
                                     htmltools::tags$div(
                                       class = "inline",
                                       selectInput(
                                              "map",
                                              "Map",
                                              choices = map_choices,
                                              selected = map_choices[1],
                                              multiple = FALSE
                                       )
                                     ),
                                     htmltools::tags$div(
                                       class = "mytooltip",
                                       icon("circle-info"),
                                       htmltools::tags$p(class = "mytooltiptext",
                                                         "mean occupancy: species mean occupancy; 
                                                          occupancy slope: mean yearly trend of occupancy (%); 
                                                          dynamic occupancy: occupancy values for each year (choose)" )
                                       )
                                     )
                            ),
                            column(
                                   3,
                                   conditionalPanel(
                                          'input.map === "dynamic"',
                                          uiOutput('inYear')
                                   )
                            )
                     ),
                     fluidRow(
                            column(
                                   6,
                                   navset_card_tab(
                                          full_screen = TRUE,
                                          nav_panel(
                                                 title = htmltools::span(
                                                   "Temporal trends",
                                                   htmltools::tags$div(
                                                     class = "mytooltip",
                                                     icon("circle-info"),
                                                     htmltools::tags$p(class = "mytooltiptext",
                                                                       "Occupancy trend for each country (average across all cells)" )
                                                   )
                                                 ),
                                                 plotly::plotlyOutput(
                                                        'countryts',
                                                        height = "600px"
                                                 )
                                          ),
                                          nav_panel(
                                            title = htmltools::span(
                                              "Detection coefficients",
                                              htmltools::tags$div(
                                                class = "mytooltip",
                                                icon("circle-info"),
                                                htmltools::tags$p(class = "mytooltiptext",
                                                                  "Coefficients affecting species' detection probability")
                                                
                                                )
                                              ),
                                            fluidRow(
                                              column(8,
                                                     plotly::plotlyOutput(
                                                       'phenots', # Inferred phenology (variation in detection probability across the year)
                                                       height = "600px"
                                                     )),
                                              column(4,
                                                     plotly::plotlyOutput(
                                                       'pcoef', # Inferred phenology (variation in detection probability across the year)
                                                       height = "600px"
                                                     ))
                                            )
                                          ),
                                          nav_panel(
                                            title = htmltools::span(
                                              "Occupancy coefficients",
                                              htmltools::tags$div(
                                                class = "mytooltip",
                                                icon("circle-info"),
                                                htmltools::tags$p(class = "mytooltiptext",
                                                                  "Coefficients affecting species' probability of occurrence")
                                                # Estimated influence on occupancy (logit scale)
                                              )
                                            ),
                                                 plotly::plotlyOutput(
                                                        'envplot',
                                                        height = "600px"
                                                 )
                                          )
                                   )
                            ),
                            column(
                                   6,
                                   card(
                                          shinycssloaders::withSpinner(
                                                 leafgl::leafglOutput(
                                                        'mapdistri',
                                                        height = "600px"
                                                 ),
                                                 type = 4
                                          ),
                                          full_screen = TRUE
                                   )
                            )
                     )
              ),
              tabPanel(
                     title = "About",
                     htmltools::includeMarkdown("about.md"),
              ),
       )
)
