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
                span(TITLE),
                downloadButton("downloadData", "Download")
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
                  virtualSelectInput("timePeriods", "Time period(s)",
                                     width = "100%",
                                     multiple = TRUE,
                                     autoSelectFirstOption = FALSE,
                                     choices = ALL_TIME_PERIODS,
                                     search = TRUE,
                                     showValueAsTags = TRUE)
                )
              )
            ),
            column(
              width = 10,
              tags$div(id = "filtered_data_container",
                       dataTableOutput("filtered_data")
              )
            )
          )
        )
      )
    )
  )
}
