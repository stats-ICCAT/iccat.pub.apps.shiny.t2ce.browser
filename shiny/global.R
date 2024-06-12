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

load("./REF_TIME_PERIODS.RData")
load("./META.RData")
load("./CE.RData")
load("./CE_w.RData")

ALL_FLAGS        = setNames(as.character(REF_FLAGS$CODE),        paste0(REF_FLAGS$CODE,        " - ", REF_FLAGS$NAME_EN))
ALL_GEAR_GROUPS  = setNames(as.character(REF_GEAR_GROUPS$CODE),  paste0(REF_GEAR_GROUPS$CODE,  " - ", REF_GEAR_GROUPS$NAME_EN))
ALL_TIME_PERIODS = setNames(REF_TIME_PERIODS$CODE,               paste0(REF_TIME_PERIODS$CODE, " - ", REF_TIME_PERIODS$NAME_EN, " (", REF_TIME_PERIODS$TYPE_CODE, ")"))
#ALL_GEARS       = setNames(as.character(REF_GEARS$CODE),       paste(REF_GEARS$CODE,       "-", REF_GEARS$NAME_EN))

ALL_DATASET_TYPES  = setNames(c("..", "n.", ".w", "nw"), 
                              c(".. (efforts only)", "n. (catches in numbers only)", ".w (catches in weights only)", "nw (catches in numbers and weights)"))

ALL_CATCH_UNITS    = setNames(c("KG", "NO"), 
                              c("KG - Weight", "NO - Number of fish"))

ALL_FISHING_MODES  = setNames(c("FS", "LS", "UNK"), 
                              c("FS - Free-swimming schools", "LS - Log-associated schools", "UNK - Unknown / Unavailable"))


set_log_level(LOG_INFO)

MIN_YEAR = 1950 #min(CA_ALL$Year)
MAX_YEAR = max(CE_w$YEAR)

INFO(paste0(nrow(CE_w), " rows loaded from CE_w"))