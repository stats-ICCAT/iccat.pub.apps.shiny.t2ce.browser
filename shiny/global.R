library(stringr)

library(iccat.pub.base)
library(iccat.pub.data)

library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinycssloaders)
library(DT)

options(scipen = 9999)

# THIS IS ***FUNDAMENTAL*** TO HAVE THE DOCKER CONTAINER CORRECTLY LOAD THE .RData FILE WITH THE ORIGINAL UTF-8 ENCODING
Sys.setlocale(category = "LC_ALL", locale = "en_US.UTF-8")

TAB_DATA             = "Data"
TAB_SUMMARY          = "Summary"
TAB_DETAILED_SUMMARY = "Detailed summary"

load("./META.RData")
load("./CE.RData")
load("./CE_w.RData")

ALL_FLAGS        = setNames(as.character(REF_FLAGS$CODE),        paste0(REF_FLAGS$CODE,              " - ", REF_FLAGS$NAME_EN))
ALL_FLEETS       = setNames(as.character(REF_FLEETS$CODE),       paste0(REF_FLEETS$CODE,             " - ", REF_FLEETS$NAME_EN))
ALL_GEAR_GROUPS  = setNames(as.character(REF_GEAR_GROUPS$CODE),  paste0(REF_GEAR_GROUPS$CODE,        " - ", REF_GEAR_GROUPS$NAME_EN))
ALL_GEARS        = setNames(as.character(REF_GEARS$CODE),        paste0(REF_GEARS$CODE,              " - ", REF_GEARS$NAME_EN))
ALL_EFFORT_TYPES = setNames(as.character(REF_EFFORT_TYPES$CODE), paste0(REF_EFFORT_TYPES$CODE,       " - ", REF_EFFORT_TYPES$NAME_EN))
ALL_TIME_PERIODS = setNames(REF_TIME_PERIODS$CODE,               paste0(REF_TIME_PERIODS$CODE,       " - ", REF_TIME_PERIODS$NAME_EN, " (", REF_TIME_PERIODS$TIME_PERIOD_GROUP_CODE, ")"))
ALL_TIME_GROUPS  = setNames(REF_TIME_PERIOD_GROUPS$CODE,         paste0(REF_TIME_PERIOD_GROUPS$CODE, " - ", REF_TIME_PERIOD_GROUPS$NAME_EN))
ALL_SQUARE_TYPES = setNames(REF_SQUARE_TYPES$CODE,               paste0(REF_SQUARE_TYPES$CODE,       " - ", REF_SQUARE_TYPES$NAME_EN))

ALL_DATASET_TYPES  = setNames(c("..", "n.", ".w", "nw"), 
                              c(".. (efforts only)", "n. (catches in numbers only)", ".w (catches in weights only)", "nw (catches in numbers and weights)"))

ALL_CATCH_UNITS    = setNames(c("KG", "NO"), 
                              c("KG - Weight", "NO - Number of fish"))

ALL_FISHING_MODES  = setNames(c("FS", "LS", "UNK"), 
                              c("FS - Free-swimming schools", "LS - Log-associated schools", "UNK - Unknown / Unavailable"))


UI_select_input = function(id, label, choices) {
  return(
    virtualSelectInput(
      inputId = id, 
      label = label,
      width = "100%",
      multiple = TRUE,
      autoSelectFirstOption = FALSE,
      choices = choices,
      search = TRUE,
      showValueAsTags = FALSE,
      updateOn = "close"
    )
  )
}

INITIAL_NUM_ENTRIES = 45

CSV_DATA_AVAILABLE   = "1"
CSV_DATA_UNAVAILABLE = ""

UI_DATA_AVAILABLE    = "◼"
UI_DATA_UNAVAILABLE  = "◻"

set_log_level(LOG_INFO)

MIN_YEAR = 1950 #min(CA_ALL$Year)
MAX_YEAR = max(CE_w$YEAR)

INFO(paste0(nrow(CE_w), " rows loaded from CE_w"))