ui = function() {
  TITLE = "ICCAT interactive catch-and-effort data browser v1.0"
  return(
    fluidPage(
      title = TITLE,
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      ),
      tags$div(
        class = "main-container",
        conditionalPanel(
          condition = "$('html').hasClass('shiny-busy')",
          tags$div(id = "glasspane",
                   tags$div(class = "loading", "Filtering data and preparing output...")
          )
        ),
        tags$div(
          fluidRow(
            column(
              width = 8,
              h2(
                img(src = "iccat-logo.jpg", height = "96px"),
                span(TITLE)
              )
            )
          ),
          fluidRow(
            column(
              width = 2,
              fluidRow(
                column(
                  width = 12,
                  sliderInput("years", "Year range",
                              width = "100%",
                              min = MIN_YEAR, max = MAX_YEAR,
                              value = c(max(MIN_YEAR, MAX_YEAR - 30 + 1), MAX_YEAR),
                              sep = "",
                              step  = 1)
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  tabsetPanel(
                    tabPanel("Main filters",
                             style = "padding-top: 1em", 
                             fluidRow(
                               column(
                                 width = 12,
                                 virtualSelectInput("datasetTypes", "Dataset type(s)",
                                                    width = "100%",
                                                    multiple = TRUE,
                                                    autoSelectFirstOption = FALSE,
                                                    choices = ALL_DATASET_TYPES,
                                                    search = TRUE,
                                                    showValueAsTags = TRUE)
                                 )
                               ),
                               fluidRow(
                                 column(
                                   width = 12,
                                   virtualSelectInput("catchUnits", "Catch unit(s)",
                                                      width = "100%",
                                                      multiple = TRUE,
                                                      autoSelectFirstOption = FALSE,
                                                      choices = ALL_CATCH_UNITS,
                                                      search = TRUE,
                                                      showValueAsTags = TRUE)
                                 )
                               ),
                               fluidRow(
                                 column(
                                   width = 12,
                                   virtualSelectInput("flags", "Flag(s)",
                                                      width = "100%",
                                                      multiple = TRUE,
                                                      autoSelectFirstOption = FALSE,
                                                      choices = ALL_FLAGS,
                                                      search = TRUE,
                                                      showValueAsTags = TRUE)
                                 )
                               ),
                               fluidRow(
                                 column(
                                   width = 12,
                                   virtualSelectInput("gearGroups", "Gear group(s)",
                                                      width = "100%",
                                                      multiple = TRUE,
                                                      autoSelectFirstOption = FALSE,
                                                      choices = ALL_GEAR_GROUPS,
                                                      search = TRUE,
                                                      showValueAsTags = TRUE)
                                 )
                               ),
                               fluidRow(
                                 column(
                                   width = 12,
                                   virtualSelectInput("fishingModes", "Fishing mode(s)",
                                                      width = "100%",
                                                      multiple = TRUE,
                                                      autoSelectFirstOption = FALSE,
                                                      choices = ALL_FISHING_MODES,
                                                      search = TRUE,
                                                      showValueAsTags = TRUE)
                                 )
                               ),
                               fluidRow(
                                 column(
                                   width = 12,
                                   virtualSelectInput("effortTypesI", "Primary effort type(s)",
                                                      width = "100%",
                                                      multiple = TRUE,
                                                      autoSelectFirstOption = FALSE,
                                                      choices = ALL_EFFORT_TYPES,
                                                      search = TRUE,
                                                      showValueAsTags = TRUE)
                                 )
                               )
                    ),
                    tabPanel("Other filters",
                             style = "padding-top: 1em", 
                             fluidRow(
                               column(
                                 width = 12,
                                 virtualSelectInput("timeGroups", "Time period type(s)",
                                                    width = "100%",
                                                    multiple = TRUE,
                                                    autoSelectFirstOption = FALSE,
                                                    choices = ALL_TIME_GROUPS,
                                                    search = TRUE,
                                                    showValueAsTags = TRUE)
                               )
                             ),
                             fluidRow(
                               column(
                                 width = 12,
                                 virtualSelectInput("timePeriods", "Time period(s)",
                                                    width = "100%",
                                                    multiple = TRUE,
                                                    autoSelectFirstOption = FALSE,
                                                    choices = ALL_TIME_PERIODS,
                                                    search = TRUE,
                                                    showValueAsTags = TRUE)
                               )
                             ),
                             fluidRow(
                               column(
                                 width = 12,
                                 virtualSelectInput("squareTypes", "Square type(s)",
                                                    width = "100%",
                                                    multiple = TRUE,
                                                    autoSelectFirstOption = FALSE,
                                                    choices = ALL_SQUARE_TYPES,
                                                    search = TRUE,
                                                    showValueAsTags = TRUE)
                               )
                             ),
                             fluidRow(
                               column(
                                 width = 12,
                                 virtualSelectInput("fleets", "Fleet(s)",
                                                    width = "100%",
                                                    multiple = TRUE,
                                                    autoSelectFirstOption = FALSE,
                                                    choices = ALL_FLEETS,
                                                    search = TRUE,
                                                    showValueAsTags = TRUE)
                               )
                             ),
                             fluidRow(
                               column(
                                 width = 12,
                                 virtualSelectInput("gears", "Gear(s)",
                                                    width = "100%",
                                                    multiple = TRUE,
                                                    autoSelectFirstOption = FALSE,
                                                    choices = ALL_GEARS,
                                                    search = TRUE,
                                                    showValueAsTags = TRUE)
                               )
                             ),
                             fluidRow(
                               column(
                                 width = 12,
                                 virtualSelectInput("effortTypesII", "Secondary effort type(s)",
                                                    width = "100%",
                                                    multiple = TRUE,
                                                    autoSelectFirstOption = FALSE,
                                                    choices = ALL_EFFORT_TYPES,
                                                    search = TRUE,
                                                    showValueAsTags = TRUE)
                               )
                             )
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  span("Data last updated on:"), 
                  strong(META$LAST_UPDATE)
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  hr(),
                  h5(strong("Download (.csv.gz)"))
                )
              ),
              fluidRow(
                div(
                  style = "margin-top: 1em; font-size: 12px; text-align: center",
                  column(
                    width = 4,
                    strong("Data", style = "vertical-align: -8px")
                  ), 
                  column(
                    width = 4, 
                    strong("Summary", style = "vertical-align: -8px")
                  ), 
                  column(
                    width = 4, 
                    strong("Det. summary", style = "vertical-align: -8px")
                  )
                )
              ),
              fluidRow(
                div(
                  style = "margin-top: 1em",
                  column(
                    width = 4,
                    downloadButton("downloadDataFiltered", "Filtered", style = "width: 100px")
                  ),
                  column(
                    width = 4, 
                    downloadButton("downloadSummaryFiltered", "Filtered", style = "width: 100px")
                  ),
                  column(
                    width = 4, 
                    downloadButton("downloadDetailedSummaryFiltered", "Filtered", style = "width: 100px")
                  )
                )
              ),
              fluidRow(
                div(
                  style = "margin-top: 1em",
                  column(
                    width = 4,
                    downloadButton("downloadDataAll", "All", style = "width: 100px")
                  ),
                  column(
                    width = 4,
                    downloadButton("downloadSummaryAll", "All", style = "width: 100px")
                  ),
                  column(
                    width = 4,
                    downloadButton("downloadDetailedSummaryAll", "All", style = "width: 100px")
                  )
                )
              )
            ),
            column(
              width = 10,
              tabsetPanel(
                tabPanel("Data",
                         tags$div(id = "filtered_data_container",
                                  dataTableOutput("filtered_data")
                         )
                ),
                tabPanel("Summary",
                         tags$div(id = "filtered_summary_container",
                                  dataTableOutput("summary_data"))
                ),
                tabPanel("Detailed summary",
                         tags$div(id = "filtered_detailed_summary_container",
                                  dataTableOutput("detailed_summary_data")
                         )
                )
              )
            )
          )
        )
      )
    )
  )
}
