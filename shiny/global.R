library(stringr)

library(iccat.pub.base)
library(iccat.pub.data)

library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinycssloaders)
library(DT)

library(dplyr)

options(scipen = 9999)

# THIS IS ***FUNDAMENTAL*** TO HAVE THE DOCKER CONTAINER CORRECTLY LOAD THE .RData FILE WITH THE ORIGINAL UTF-8 ENCODING
Sys.setlocale(category = "LC_ALL", locale = "en_US.UTF-8")

ALL_FLAGS       = setNames(as.character(REF_FLAGS$CODE),       paste(REF_FLAGS$CODE,       "-", REF_FLAGS$NAME_EN))
ALL_GEAR_GROUPS = setNames(as.character(REF_GEAR_GROUPS$CODE), paste(REF_GEAR_GROUPS$CODE, "-", REF_GEAR_GROUPS$NAME_EN))
#ALL_GEARS       = setNames(as.character(REF_GEARS$CODE),       paste(REF_GEARS$CODE,       "-", REF_GEARS$NAME_EN))

set_log_level(LOG_INFO)

load("./CE_w.RData")

MIN_YEAR = 1950 #min(CA_ALL$Year)
MAX_YEAR = max(CE_w$YEAR)

INFO(paste0(nrow(CE_w), " rows loaded from CE_w"))