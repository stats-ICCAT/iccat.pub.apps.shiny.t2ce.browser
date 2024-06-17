ui = function() {
  TITLE = paste0("ICCAT data browser / T2CE / ", META$LAST_UPDATE)
  return(
    fluidPage(
      shinyjs::useShinyjs(),
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
                img(src = "iccat-logo.jpg", height = "48px"),
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
                                 UI_select_input("datasetTypes", "Dataset type(s)", ALL_DATASET_TYPES)
                               )
                             ),
                             fluidRow(
                               column(
                                 width = 12,
                                 UI_select_input("catchUnits", "Catch unit(s)", ALL_CATCH_UNITS)
                               )
                             ),
                             fluidRow(
                               column(
                                 width = 12,
                                 UI_select_input("flags", "Flag(s)", ALL_FLAGS)
                               )
                             ),
                             fluidRow(
                               column(
                                 width = 12,
                                 UI_select_input("gearGroups", "Gear group(s)", ALL_GEAR_GROUPS)
                               )
                             ),
                             fluidRow(
                               column(
                                 width = 12,
                                 UI_select_input("fishingModes", "Fishing mode(s)", ALL_FISHING_MODES)
                               )
                             ),
                             fluidRow(
                               column(
                                 width = 12,
                                 UI_select_input("effortTypesI", "Primary effort type(s)", ALL_EFFORT_TYPES)
                               )
                             )
                    ),
                    tabPanel("Other filters",
                             style = "padding-top: 1em", 
                             fluidRow(
                               column(
                                 width = 12,
                                 UI_select_input("timeGroups", "Time period type(s)", ALL_TIME_GROUPS)
                               )
                             ),
                             fluidRow(
                               column(
                                 width = 12,
                                 UI_select_input("timePeriods", "Time period(s)", ALL_TIME_PERIODS)
                               )
                             ),
                             fluidRow(
                               column(
                                 width = 12,
                                 UI_select_input("squareTypes", "Square type(s)", ALL_SQUARE_TYPES)
                               )
                             ),
                             fluidRow(
                               column(
                                 width = 12,
                                 UI_select_input("fleets", "Fleet(s)", ALL_FLEETS)
                               )
                             ),
                             fluidRow(
                               column(
                                 width = 12,
                                 UI_select_input("gears", "Gear(s)", ALL_GEARS)
                               )
                             ),
                             fluidRow(
                               column(
                                 width = 12,
                                 UI_select_input("effortTypesII", "Secondary effort type(s)", ALL_EFFORT_TYPES)
                               )
                             )
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h5(strong("Download current dataset:"))
                )
              ),
              fluidRow(
                column(
                  width = 4,
                  downloadButton("downloadFiltered", "Filtered", style = "width: 100px")
                ),
                column(
                  width = 4,
                  downloadButton("downloadFull",     "Full",     style = "width: 100px")
                ),
                column(
                  width = 4,
                  span("as ", style = "vertical-align: -5px",
                       code(".csv.gz")
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  hr(),
                  span("Data last updated on:"), 
                  strong(META$LAST_UPDATE)
                )
              )
            ),
            column(
              width = 10,
              tabsetPanel(
                id = "dataset",
                tabPanel(TAB_DATA,
                         tags$div(id = "filtered_data_container",
                                  dataTableOutput("filtered_data")
                         )
                ),
                tabPanel(TAB_SUMMARY,
                         tags$div(id = "filtered_summary_container",
                                  dataTableOutput("summary_data"))
                ),
                tabPanel(TAB_DETAILED_SUMMARY,
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
