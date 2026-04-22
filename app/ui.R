fluidPage(
       # run bslib::bs_theme_preview() to customize
       # theme = bs_theme(preset = "cosmo"),
       tags$head(
         tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
         ),

       # Application title
       titlePanel("Distribution of odonates across Europe. DRAGON, FRB-CESAB"),
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
                                                         "mean occupancy: species mean occupancy in 2000-2024; 
                                                          occupancy slope: linear slope of occupancy between 2000 and 2024; 
                                                          dynamic occupancy: occupancy values for each year betweem 2000 and 2024 (choose); 
                                                          site deviation (mean): deviation from the mean occupancy of similar sites; 
                                                          site deviation (mean): deviation of sites from the global temporal trend" )
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
                                                   "Trends",
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
                                              "Phenology",
                                              htmltools::tags$div(
                                                class = "mytooltip",
                                                icon("circle-info"),
                                                htmltools::tags$p(class = "mytooltiptext",
                                                                  "Inferred phenology (variation in detection probability across the year)" )
                                              )
                                            ),
                                                 plotly::plotlyOutput(
                                                        'phenots',
                                                        height = "600px"
                                                 )
                                          ),
                                          nav_panel(
                                            title = htmltools::span(
                                              "Environmental variables",
                                              htmltools::tags$div(
                                                class = "mytooltip",
                                                icon("circle-info"),
                                                htmltools::tags$p(class = "mytooltiptext",
                                                                  "Estimated influence on occupancy (logit scale)" )
                                              )
                                            ),
                                                 plotly::plotlyOutput(
                                                        'envplot',
                                                        height = "600px"
                                                 )
                                          ),
                                          nav_panel(
                                            title = htmltools::span(
                                              "Time coefficients",
                                              htmltools::tags$div(
                                                class = "mytooltip",
                                                icon("circle-info"),
                                                htmltools::tags$p(class = "mytooltiptext",
                                                                  "Deviation from the mean occupancy for each year (logit scale)" )
                                              )
                                            ),
                                            plotly::plotlyOutput(
                                              'psits',
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
