fluidPage(
       # run bslib::bs_theme_preview() to customize
       # theme = bs_theme(preset = "cosmo"),

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
                                          "Species:",
                                          choices = sp_choices,
                                          selected = sp_choices[1],
                                          multiple = FALSE
                                   ),
                                   br()
                            ),
                            column(
                                   3,
                                   selectInput(
                                          "map",
                                          "Map:",
                                          choices = map_choices,
                                          selected = map_choices[1],
                                          multiple = FALSE
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
                                                 "Country",
                                                 plotly::plotlyOutput(
                                                        'countryts',
                                                        height = "600px"
                                                 )
                                          ),
                                          nav_panel(
                                                 "Psi_time",
                                                 plotly::plotlyOutput(
                                                        'psits',
                                                        height = "600px"
                                                 )
                                          ),
                                          nav_panel(
                                                 "Phenology",
                                                 plotly::plotlyOutput(
                                                        'phenots',
                                                        height = "600px"
                                                 )
                                          ),
                                          nav_panel(
                                                 "Drivers",
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
