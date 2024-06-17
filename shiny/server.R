server = function(input, output, session) {
  SPECIES_ORDERED = c("BFT", "ALB", # Temperate tunas
                      "YFT", "BET", "SKJ", # Tropical tunas
                      "SWO", "BUM", "SAI", "SPF", "WHM", # Billfish
                      "BLF", "BLT", "BON", "BOP", "BRS", "CER", "FRI", "KGM", "LTA", "MAW", "SLT", "SSM", "WAH",  "DOL", # Small tunas
                      "BIL", "BLM", "MSP", "MLS", "RSP", # Other billfish
                      "SBF", # Southern bluefin tuna
                      "oTun", # Other tunas
                      "BSH", "POR", "SMA", # Main shark species
                      "oSks", # Other sharks
                      "oFis", # Other fish
                      "rest" # Everything else
  )
  
  EMPTY_FILTER = 
    list(years = c(),
         datasetTypes = c(),
         catchUnits = c(),
         flags = c(),
         fleets = c(),
         gearGroups = c(),
         gears = c(),
         fishingModes = c(),
         effortTypesI = c(),
         effortTypesII = c(),
         squareTypes = c(),
         timeGroups = c(),
         timePeriods = c()
    )
  
  observeEvent(input$resetFilters, { session$reload() })
  
  default_filter_data = function(data, input = EMPTY_FILTER) {
    INFO(paste0("Years          : ", paste0(input$years,         collapse = "-")))
    INFO(paste0("Dataset types  : ", paste0(input$datasetTypes,  collapse = ", ")))
    INFO(paste0("Catch units    : ", paste0(input$catchUnits,    collapse = ", ")))
    INFO(paste0("Flags          : ", paste0(input$flags,         collapse = ", ")))
    INFO(paste0("Fleets         : ", paste0(input$fleets,        collapse = ", ")))
    INFO(paste0("Gear groups    : ", paste0(input$gearGroups,    collapse = ", ")))
    INFO(paste0("Gears          : ", paste0(input$gears,         collapse = ", ")))
    INFO(paste0("Fishing modes  : ", paste0(input$fishingModes,  collapse = ", ")))
    INFO(paste0("Effort types I : ", paste0(input$effortTypesI,  collapse = ", ")))
    INFO(paste0("Effort types II: ", paste0(input$effortTypesII, collapse = ", ")))
    INFO(paste0("Square types   : ", paste0(input$squareTypes,   collapse = ", ")))
    INFO(paste0("Time groups    : ", paste0(input$timeGroups,    collapse = ", ")))
    INFO(paste0("Time periods   : ", paste0(input$timePeriods,   collapse = ", ")))
    
    start = Sys.time()
    
    filtered = data
    
    has_years = length(input$years) == 2
    
    if(has_years) {
      first_year = input$years[1]
      last_year  = input$years[2]
    
      filtered = filtered[YEAR >= first_year & YEAR <= last_year]
    } else {
      first_year = min(data$YEAR)
      last_year  = max(data$YEAR)
    }
    
    if(!is.null(input$datasetTypes)) {
      filtered = filtered[DATASET_TYPE_CODE %in% input$datasetTypes]
    }
    
    if(!is.null(input$catchUnits)) {
      filtered = filtered[CATCH_UNIT_CODE %in% input$catchUnits]
    }
    
    if(!is.null(input$flags)) {
      filtered = filtered[FLAG_CODE %in% input$flags]
    }
    
    if(!is.null(input$fleets)) {
      filtered = filtered[FLEET_CODE %in% input$fleets]
    }
    
    if(!is.null(input$gearGroups)) {
      filtered = filtered[GEAR_GROUP_CODE %in% input$gearGroups]
    }

    if(!is.null(input$gears)) {
      filtered = filtered[GEAR_CODE %in% input$gears]
    }

    if(!is.null(input$fishingModes)) {
      filtered = filtered[FISHING_MODE_CODE %in% input$fishingModes]
    }

    if(!is.null(input$effortTypesI)) {
      filtered = filtered[PRIMARY_EFFORT_UNIT_CODE %in% input$effortTypesI]
    }

    if(!is.null(input$effortTypesII)) {
      filtered = filtered[SECONDARY_EFFORT_UNIT_CODE %in% input$effortTypesII]
    }

    if(!is.null(input$timeGroups)) {
      filtered = filtered[TIME_PERIOD_TYPE_CODE %in% input$timeGroups]
    }
    
    if(!is.null(input$timePeriods)) {
      filtered = filtered[TIME_PERIOD_CODE %in% input$timePeriods]
    }
    
    if(!is.null(input$squareTypes)) {
      filtered = filtered[SQUARE_TYPE_CODE %in% input$squareTypes]
    }
    
    end = Sys.time()
    
    INFO(paste0("Filtering data: ", end - start))
    
    INFO(paste0("Filtered data size: ", nrow(filtered)))
    
    return(filtered)
  }
  
  filter_ce_data = reactive({
    return(
      filter_ce_data_(input)
    )
  })
  
  filter_ce_data_ = function(input = EMPTY_FILTER) {
    filtered = default_filter_data(CE_w, input)

    return(filtered)
  }
  
  filter_summary_data = reactive({
    return(
      filter_summary_data_(input, TRUE)
    )
  })
  
  filter_summary_data_ = function(input = EMPTY_FILTER, use_symbols = FALSE) {
    filtered = default_filter_data(CE, input)
    filtered = 
      filtered[, .(CATCH = sum(CATCH, na.rm = TRUE)), keyby = .(FLAG_CODE,
                                                                FLAG_NAME_EN, 
                                                                GEAR_GROUP_CODE, 
                                                                CATCH_UNIT_CODE, 
                                                                DATASET_TYPE_CODE, 
                                                                YEAR, YEAR_SHORT)][CATCH > 0]
    
    has_years = length(input$years) == 2
    
    if(has_years) {
      first_year = input$years[1]
      last_year  = input$years[2]
      
      filtered = filtered[YEAR >= first_year & YEAR <= last_year]
    } else {
      first_year = min(filtered$YEAR)
      last_year  = max(filtered$YEAR)
    }
    
    FILTERED_YEAR_SHORTS = lapply(first_year:last_year, function(y) { return (str_sub(as.character(y), 3, 4) ) })
    
    filtered[, YEAR_SHORT := str_sub(as.character(YEAR), 3, 4)]
    
    filtered$YEAR_SHORT =
      factor(
        filtered$YEAR_SHORT,
        labels = as.character(FILTERED_YEAR_SHORTS),
        levels = as.character(FILTERED_YEAR_SHORTS),
        ordered = TRUE
      )
    
    filtered = filtered[, .(FLAG_CODE, FLAG_NAME_EN, GEAR_GROUP_CODE, CATCH_UNIT_CODE, DATASET_TYPE_CODE, YEAR_SHORT, CATCH)]
    
    filtered_w =
      dcast.data.table(
        filtered,
        FLAG_CODE + FLAG_NAME_EN + GEAR_GROUP_CODE + CATCH_UNIT_CODE + DATASET_TYPE_CODE ~ YEAR_SHORT,
        fun.aggregate = function(v) { return (ifelse(use_symbols, UI_DATA_AVAILABLE, CSV_DATA_AVAILABLE)) },
        drop = c(TRUE, FALSE),
        value.var = "CATCH",
        fill = ifelse(use_symbols, UI_DATA_UNAVAILABLE, CSV_DATA_UNAVAILABLE)
      )
    
    return(filtered_w)
  }
  
  filter_detailed_summary_data = reactive({
    return(
      filter_detailed_summary_data_(input, TRUE)
    )
  })
  
  filter_detailed_summary_data_ = function(input = EMPTY_FILTER, use_symbols = FALSE) {
    filtered = default_filter_data(CE, input)
    
    filtered = 
      filtered[, .(CATCH = sum(CATCH, na.rm = TRUE)), keyby = .(FLAG_CODE, FLAG_NAME_EN,
                                                                FLEET_CODE, 
                                                                GEAR_GROUP_CODE,
                                                                GEAR_CODE, 
                                                                TIME_PERIOD_TYPE_CODE, 
                                                                SQUARE_TYPE_CODE, 
                                                                PRIMARY_EFFORT_UNIT_CODE, 
                                                                SECONDARY_EFFORT_UNIT_CODE, 
                                                                CATCH_UNIT_CODE, 
                                                                DATASET_TYPE_CODE, 
                                                                YEAR, YEAR_SHORT)][CATCH > 0]
    
    has_years = length(input$years) == 2
    
    if(has_years) {
      first_year = input$years[1]
      last_year  = input$years[2]
      
      filtered = filtered[YEAR >= first_year & YEAR <= last_year]
    } else {
      first_year = min(filtered$YEAR)
      last_year  = max(filtered$YEAR)
    }
    
    FILTERED_YEAR_SHORTS = lapply(first_year:last_year, function(y) { return (str_sub(as.character(y), 3, 4) ) })
    
    filtered[, YEAR_SHORT := str_sub(as.character(YEAR), 3, 4)]
    
    filtered$YEAR_SHORT =
      factor(
        filtered$YEAR_SHORT,
        labels = as.character(FILTERED_YEAR_SHORTS),
        levels = as.character(FILTERED_YEAR_SHORTS),
        ordered = TRUE
      )
    
    filtered_w =
      dcast.data.table(
        filtered,
        FLAG_CODE + FLAG_NAME_EN + FLEET_CODE + GEAR_GROUP_CODE + GEAR_CODE + TIME_PERIOD_TYPE_CODE + SQUARE_TYPE_CODE + PRIMARY_EFFORT_UNIT_CODE + SECONDARY_EFFORT_UNIT_CODE + CATCH_UNIT_CODE + DATASET_TYPE_CODE ~ YEAR_SHORT,
        fun.aggregate = function(v) { return (ifelse(use_symbols, UI_DATA_AVAILABLE, CSV_DATA_AVAILABLE)) },
        drop = c(TRUE, FALSE),
        value.var = "CATCH",
        fill = ifelse(use_symbols, UI_DATA_UNAVAILABLE, CSV_DATA_UNAVAILABLE)
      )
    
    return(filtered_w)
  }

  validate_filtering = function(filtered_data) {
    filtered_rows = nrow(filtered_data)
    
    if(filtered_rows == 0) {
      shinyjs::disable("downloadFiltered")
      
      showModal(modalDialog(title  = "No matching records", 
                            footer = NULL,
                            easyClose = TRUE,
                            fade = FALSE,
                            "Please refine your current filtering criteria!"))
    } else {
      shinyjs::enable("downloadFiltered")
    }
    
    #validate(need(filtered_rows > 0, "Current filtering criteria do not identify any valid record!"))
    
    return(filtered_data)
  }
  
  output$filtered_data =
    renderDataTable({
      filtered_data = validate_filtering(filter_ce_data())

      filtered_data$DATASET_ID = NULL
      filtered_data$STRATA_ID = NULL
      filtered_data$FLAG_CODE = NULL
      
      return(
        DT::datatable(
          filtered_data,
          options = list(
            pageLength = INITIAL_NUM_ENTRIES, 
            autoWidth = TRUE,
            scrollX = TRUE,
            dom = "ltipr" # To remove the 'search box' - see: https://rstudio.github.io/DT/options.html and https://datatables.net/reference/option/dom
          ),
          filter    = "none",
          selection = "none",
          rownames = FALSE,
          colnames = c("Flag name", "Fleet code", 
                       "Gear group", "Gear",
                       "Year", "Time period", "Time",
                       "Square", "Quad", "Lat", "Lon", "CWP grid",
                       "Fishing mode",
                       "Effort #1", "Effort unit #1",
                       "Effort #2", "Effort unit #2",
                       "Catch unit",
                       "Dataset type", 
                       "Total",
                       SPECIES_ORDERED)
        ) 
        %>% DT::formatCurrency(columns = c("PRIMARY_EFFORT", "SECONDARY_EFFORT", "TOTAL", SPECIES_ORDERED), currency = "")
        %>% DT::formatRound(columns = c("LAT", "LON"), digits = 6)
      )
    })
  
  output$summary_data =
    renderDataTable({
      filtered_data = validate_filtering(filter_summary_data())
      
      filtered_data$FLAG_CODE = NULL
      
      return(
        DT::datatable(
          filtered_data,
          options = list(
            pageLength = INITIAL_NUM_ENTRIES, 
            autoWidth = TRUE,
            scrollX = TRUE,
            dom = "ltipr" # To remove the 'search box' - see: https://rstudio.github.io/DT/options.html and https://datatables.net/reference/option/dom
          ),
          filter    = "none",
          selection = "none",
          rownames = FALSE,
          colnames = c("Flag name", 
                       "Gear group",
                       "Catch unit",
                       "Dataset type",
                       colnames(filtered_data[, 5:ncol(filtered_data)]))
        )
      )
    })
  
  output$detailed_summary_data =
    renderDataTable({
      filtered_data = validate_filtering(filter_detailed_summary_data())
      
      filtered_data$FLAG_CODE = NULL
      
      return(
        DT::datatable(
          filtered_data,
          options = list(
            pageLength = INITIAL_NUM_ENTRIES, 
            autoWidth = TRUE,
            scrollX = TRUE,
            dom = "ltipr" # To remove the 'search box' - see: https://rstudio.github.io/DT/options.html and https://datatables.net/reference/option/dom
          ),
          filter    = "none",
          selection = "none",
          rownames = FALSE,
          colnames = c("Flag name", "Fleet code", 
                       "Gear group", "Gear",
                       "Time period", "Square",
                       "Effort unit #1", "Effort unit #2",
                       "Catch unit",
                       "Dataset type",
                       colnames(filtered_data[, 11:ncol(filtered_data)]))
        )
      )
    })
  
  get_filename_components = function(input) {
    dataset_types = paste0(input$datasetTypes, collapse = "+")
    
    dataset_types = str_replace_all(dataset_types, "\\.\\.", "EF")
    dataset_types = str_replace_all(dataset_types, "n\\.",   "N")
    dataset_types = str_replace_all(dataset_types, "\\.w",   "W")
    dataset_types = str_replace_all(dataset_types, "nw",     "NW")
    
    components = c(paste0(input$years,         collapse = "-"), 
                   paste0(dataset_types,       collapse = "+"), 
                   paste0(input$catchUnits,    collapse = "+"),
                   paste0(input$flags,         collapse = "+"),
                   paste0(input$fleets,        collapse = "+"),
                   paste0(input$gearGroups,    collapse = "+"),
                   paste0(input$gears,         collapse = "+"),
                   paste0(input$fishingModes,  collapse = "+"),
                   paste0(input$effortTypesI,  collapse = "+"),
                   paste0(input$effortTypesII, collapse = "+"),
                   paste0(input$timeGroups,    collapse = "+"),
                   paste0(input$timePeriods,   collapse = "+"),
                   paste0(input$squareTypes,   collapse = "+"))
    
    components = components[which(components != "")]
    
    return(paste0(str_replace_all(META$LAST_UPDATE, "\\-", ""), "_", paste0(components, collapse = "_")))
  }
  
  output$downloadFiltered = downloadHandler(
    filename = function() {
      dataset = input$dataset
      
      if(dataset == TAB_DATA)
        return(paste0("ICCAT_T2CE_", get_filename_components(input), ".csv.gz"))
      else if(dataset == TAB_SUMMARY) 
        return(paste0("ICCAT_T2CE_summary_", get_filename_components(input), ".csv.gz"))
      else # Detailed summary
        return(paste0("ICCAT_T2CE_detailed_summary_", get_filename_components(input), ".csv.gz"))
    },
    content = function(file) {
      dataset = input$dataset
      
      if(dataset == TAB_DATA)
        to_download = filter_ce_data()
      else if(dataset == TAB_SUMMARY)
        to_download = filter_summary_data_(input, FALSE)
      else # Detailed summary
        to_download = filter_detailed_summary_data_(input, FALSE)
      
      write.csv(to_download, gzfile(file), row.names = FALSE, na = "")
    }
  )
  
  output$downloadFull = downloadHandler(
    filename = function() {
      dataset = input$dataset
      
      if(dataset == TAB_DATA)
        return(META$FILENAME)
      else if(dataset == TAB_SUMMARY) 
        return(paste0("ICCAT_T2CE_summary_", str_replace_all(META$LAST_UPDATE, "\\-", ""), "_full.csv.gz"))
      else # Detailed summary
        return(paste0("ICCAT_T2CE_detailed_summary_", str_replace_all(META$LAST_UPDATE, "\\-", ""), "_full.csv.gz"))
    },
    content = function(file) {
      dataset = input$dataset
      
      if(dataset == TAB_DATA)
        file.copy(paste0("www/", META$FILENAME), file)
      else {
        if(dataset == "Summary") 
          to_download = filter_summary_data_(EMPTY_FILTER, FALSE)
        else # Detailed summary
          to_download = filter_detailed_summary_data_(EMPTY_FILTER, FALSE)
        
        write.csv(to_download, gzfile(file), row.names = FALSE, na = "")
      } 
    }
  )
}
