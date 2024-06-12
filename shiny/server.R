server = function(input, output, session) {
  filter_ce_data = reactive({
    DEBUG("CD")
    INFO(paste0("Years         : ", paste0(input$years,        collapse = "-")))
    INFO(paste0("Dataset types : ", paste0(input$datasetTypes, collapse = ", ")))
    INFO(paste0("Catch units   : ", paste0(input$catchUnits,   collapse = ", ")))
    INFO(paste0("Flags         : ", paste0(input$flags,        collapse = ", ")))
    INFO(paste0("Gear groups   : ", paste0(input$gearGroups,   collapse = ", ")))
    INFO(paste0("Fishing modes : ", paste0(input$fishingModes, collapse = ", ")))
    INFO(paste0("Time periods  : ", paste0(input$timePeriods,  collapse = ", ")))

    start = Sys.time()
     
    filtered_CE = CE_w

    first_year = input$years[1]
    last_year  = input$years[2]
    
    filtered_CE = filtered_CE[YEAR >= first_year & YEAR <= last_year]
    
    if(!is.null(input$datasetTypes)) {
      filtered_CE = filtered_CE[DATASET_TYPE_CODE_CALC %in% input$datasetTypes]
    }
    
    if(!is.null(input$catchUnits)) {
      filtered_CE = filtered_CE[CATCH_UNIT_CODE %in% input$catchUnits]
    }
    
    if(!is.null(input$flags)) {
      filtered_CE = filtered_CE[FLAG_CODE %in% input$flags]
    }
    
    if(!is.null(input$gearGroups)) {
      filtered_CE = filtered_CE[GEAR_GROUP_CODE %in% input$gearGroups]
    }

    if(!is.null(input$fishingModes)) {
      filtered_CE = filtered_CE[FISHING_MODE_CODE %in% input$fishingModes]
    }
    
    if(!is.null(input$timePeriods)) {
      filtered_CE = filtered_CE[TIME_PERIOD_CODE %in% input$timePeriods]
    }

    end = Sys.time()
    
    INFO(paste0("Filtering data: ", end - start))
    
    INFO(paste0("Filtered data size: ", nrow(filtered_CE)))

    filtered = 
      filtered_CE[, .(DATASET_ID, STRATA_ID,
                      DATASET_TYPE_CODE = DATASET_TYPE_CODE_CALC,
                      FLAG_NAME_EN,
                      FLEET_CODE,
                      GEAR_GROUP_CODE,
                      GEAR_CODE,
                      YEAR, TIME_PERIOD_CODE, 
                      SQUARE_TYPE_CODE, QUADRANT_CODE, LAT, LON,
                      FISHING_MODE_CODE,
                      PRIMARY_EFFORT, PRIMARY_EFFORT_UNIT_CODE,
                      SECONDARY_EFFORT, SECONDARY_EFFORT_UNIT_CODE,
                      CATCH_UNIT_CODE,
                      BFT, ALB,
                      YFT, BET, SKJ,
                      SWO, BUM, SAI, SPF, WHM,
                      BLF, BLT, BON, BOP, BRS, CER, FRI, KGM, LTA, 
                      MAW, SLT, SSM, WAH, DOL,
                      BIL, BLM, MSP, MLS, RSP, 
                      SBF, 
                      oTun,
                      BSH, POR, SMA, 
                      oSks)]
    
    validate(need(nrow(filtered) > 0, "Current filtering criteria do not identify any valid record!"))
    
    return(filtered)
  })
  
  filter_summary_data = reactive({
    DEBUG("CD")
    INFO(paste0("Years         : ", paste0(input$years,        collapse = "-")))
    INFO(paste0("Dataset types : ", paste0(input$datasetTypes, collapse = ", ")))
    INFO(paste0("Catch units   : ", paste0(input$catchUnits,   collapse = ", ")))
    INFO(paste0("Flags         : ", paste0(input$flags,        collapse = ", ")))
    INFO(paste0("Gear groups   : ", paste0(input$gearGroups,   collapse = ", ")))
    INFO(paste0("Fishing modes : ", paste0(input$fishingModes, collapse = ", ")))
    INFO(paste0("Time periods  : ", paste0(input$timePeriods,  collapse = ", ")))
    
    start = Sys.time()
    
    filtered_CE = CE
    
    first_year = input$years[1]
    last_year  = input$years[2]
    
    filtered_CE = filtered_CE[YEAR >= first_year & YEAR <= last_year]
    
    if(!is.null(input$datasetTypes)) {
      filtered_CE = filtered_CE[DATASET_TYPE_CODE_CALC %in% input$datasetTypes]
    }
    
    if(!is.null(input$catchUnits)) {
      filtered_CE = filtered_CE[CATCH_UNIT_CODE %in% input$catchUnits]
    }
    
    if(!is.null(input$flags)) {
      filtered_CE = filtered_CE[FLAG_CODE %in% input$flags]
    }
    
    if(!is.null(input$gearGroups)) {
      filtered_CE = filtered_CE[GEAR_GROUP_CODE %in% input$gearGroups]
    }
    
    if(!is.null(input$fishingModes)) {
      filtered_CE = filtered_CE[FISHING_MODE_CODE %in% input$fishingModes]
    }
    
    if(!is.null(input$timePeriods)) {
      filtered_CE = filtered_CE[TIME_PERIOD_CODE %in% input$timePeriods]
    }
    
    end = Sys.time()
    
    INFO(paste0("Filtering data: ", end - start))
    
    INFO(paste0("Filtered data size: ", nrow(filtered_CE)))
    
    filtered = 
      filtered_CE[, .(CATCH = sum(CATCH, na.rm = TRUE)), keyby = .(FLAG_CODE, 
                                                                   GEAR_GROUP_CODE, 
                                                                   CATCH_UNIT_CODE, 
                                                                   DATASET_TYPE_CODE_CALC, 
                                                                   YEAR_SHORT)][CATCH > 0]

    validate(need(nrow(filtered)  > 0,    "Current filtering criteria do not identify any valid record!"))
    
    FILTERED_YEAR_SHORTS = sort(unique(filtered_CE$YEAR_SHORT))
    
    filtered$YEAR_SHORT =
      factor(
        filtered$YEAR_SHORT,
        labels = as.character(FILTERED_YEAR_SHORTS),
        levels = as.character(FILTERED_YEAR_SHORTS),
        ordered = TRUE
      )
    
    filtered =
      merge(
        filtered, REF_FLAGS,
        by.x = "FLAG_CODE", by.y = "CODE",
        all.x = TRUE
      )
    
    filtered = filtered[, .(FLAG_NAME_EN = NAME_EN, GEAR_GROUP_CODE, CATCH_UNIT_CODE, DATASET_TYPE_CODE_CALC, YEAR_SHORT, CATCH)]
  
    filtered_w =
      dcast.data.table(
        filtered,
        FLAG_NAME_EN + GEAR_GROUP_CODE + CATCH_UNIT_CODE + DATASET_TYPE_CODE_CALC ~ YEAR_SHORT,
        fun.aggregate = function(v) { return ("✅")},
        drop = c(TRUE, FALSE),
        value.var = "CATCH",
        fill = "▢"
      )
    
    return(filtered_w)
  })
  
  filter_detailed_summary_data = reactive({
    DEBUG("CD")
    INFO(paste0("Years         : ", paste0(input$years,        collapse = "-")))
    INFO(paste0("Dataset types : ", paste0(input$datasetTypes, collapse = ", ")))
    INFO(paste0("Catch units   : ", paste0(input$catchUnits,   collapse = ", ")))
    INFO(paste0("Flags         : ", paste0(input$flags,        collapse = ", ")))
    INFO(paste0("Gear groups   : ", paste0(input$gearGroups,   collapse = ", ")))
    INFO(paste0("Fishing modes : ", paste0(input$fishingModes, collapse = ", ")))
    INFO(paste0("Time periods  : ", paste0(input$timePeriods,  collapse = ", ")))
    
    start = Sys.time()
    
    filtered_CE = CE
    
    first_year = input$years[1]
    last_year  = input$years[2]
    
    filtered_CE = filtered_CE[YEAR >= first_year & YEAR <= last_year]
    
    if(!is.null(input$datasetTypes)) {
      filtered_CE = filtered_CE[DATASET_TYPE_CODE_CALC %in% input$datasetTypes]
    }
    
    if(!is.null(input$catchUnits)) {
      filtered_CE = filtered_CE[CATCH_UNIT_CODE %in% input$catchUnits]
    }
    
    if(!is.null(input$flags)) {
      filtered_CE = filtered_CE[FLAG_CODE %in% input$flags]
    }
    
    if(!is.null(input$gearGroups)) {
      filtered_CE = filtered_CE[GEAR_GROUP_CODE %in% input$gearGroups]
    }
    
    if(!is.null(input$fishingModes)) {
      filtered_CE = filtered_CE[FISHING_MODE_CODE %in% input$fishingModes]
    }
    
    if(!is.null(input$timePeriods)) {
      filtered_CE = filtered_CE[TIME_PERIOD_CODE %in% input$timePeriods]
    }
    
    end = Sys.time()
    
    INFO(paste0("Filtering data: ", end - start))
    
    INFO(paste0("Filtered data size: ", nrow(filtered_CE)))
    
    filtered = 
      filtered_CE[, .(CATCH = sum(CATCH, na.rm = TRUE)), keyby = .(FLEET_CODE, 
                                                                   GEAR_CODE, 
                                                                   TIME_PERIOD_TYPE_CODE, 
                                                                   SQUARE_TYPE_CODE, 
                                                                   PRIMARY_EFFORT_UNIT_CODE, 
                                                                   SECONDARY_EFFORT_UNIT_CODE, 
                                                                   CATCH_UNIT_CODE, 
                                                          DATASET_TYPE_CODE_CALC, 
                                                          YEAR_SHORT)][CATCH > 0]

    validate(need(nrow(filtered)  > 0,    "Current filtering criteria do not identify any valid record!"))

    FILTERED_YEAR_SHORTS = sort(unique(filtered_CE$YEAR_SHORT))
    
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
        FLEET_CODE + GEAR_CODE + TIME_PERIOD_TYPE_CODE + SQUARE_TYPE_CODE + PRIMARY_EFFORT_UNIT_CODE + SECONDARY_EFFORT_UNIT_CODE + CATCH_UNIT_CODE + DATASET_TYPE_CODE_CALC ~ YEAR_SHORT,
        fun.aggregate = function(v) { return ("✅")},
        drop = c(TRUE, FALSE),
        value.var = "CATCH",
        fill = "▢"
      )
     
    return(filtered_w)
  })
  
  output$filtered_data =
    renderDataTable(
      DT::datatable(
        filter_ce_data(),
        options = list(
          pageLength = 30, 
          autoWidth = TRUE,
          scrollX = TRUE
        ),
        selection = "none",
        rownames = FALSE,
        colnames = c("Dataset ID", "Strata ID",
                     "Dataset type", 
                     "Flag", "Fleet code", 
                     "Gear group", "Gear code",
                     "Year", "Time",
                     "Square", "Quad", "Lat", "Lon",
                     "Fishing mode",
                     "Effort (1)", "E. type (1)",
                     "Effort (2)", "E. type (2)",
                     "Catch unit",
                     "BFT", "ALB",
                     "YFT", "BET", "SKJ",
                     "SWO", "BUM", "SAI", "SPF", "WHM",
                     "BLF", "BLT", "BON", "BOP", "BRS", "CER", "FRI", "KGM", "LTA",
                     "MAW", "SLT", "SSM", "WAH",  "DOL",
                     "BIL", "BLM", "MSP", "MLS", "RSP",
                     "SBF",
                     "oTun",
                     "BSH", "POR", "SMA",
                     "oSks")
      ) %>% DT::formatCurrency(columns = c(15, 17, 20:54), currency = "")
    )
  
  output$summary_data =
    renderDataTable({
      filtered_data = filter_summary_data()
      
      DT::datatable(
        filtered_data,
        options = list(
          pageLength = 30, 
          autoWidth = TRUE,
          scrollX = TRUE
        ),
        selection = "none",
        rownames = FALSE,
        colnames = c("Flag code", 
                     "Gear group",
                     "Catch unit",
                     "DS type",
                     colnames(filtered_data[, 5:ncol(filtered_data)]))
      )
    })
  
  output$detailed_summary_data =
    renderDataTable({
      filtered_data = filter_detailed_summary_data()
    
      DT::datatable(
        filtered_data,
        options = list(
          pageLength = 30, 
          autoWidth = TRUE,
          scrollX = TRUE
        ),
        selection = "none",
        rownames = FALSE,
        colnames = c("Fleet code", 
                     "Gear",
                     "Time", "Geo",
                     "Effort unit #1", "Effort unit #2",
                     "Catch unit",
                     "DS type",
                     colnames(filtered_data[, 9:ncol(filtered_data)]))
      )
    })

  output$downloadFiltered = downloadHandler(
    filename = function() {
        dataset_types = paste0(input$datasetTypes, collapse = "+")
        
        dataset_types = str_replace_all(dataset_types, "\\.\\.", "EF")
        dataset_types = str_replace_all(dataset_types, "n\\.",   "N")
        dataset_types = str_replace_all(dataset_types, "\\.w",   "W")
        dataset_types = str_replace_all(dataset_types, "nw",     "NW")
      
        components = c(paste0(input$flags,        collapse = "+"),
                       paste0(input$gearGroups,   collapse = "+"),
                       paste0(input$fishingModes, collapse = "+"),
                       paste0(input$timePeriods,  collapse = "+"),
                       paste0(dataset_types,      collapse = "+"),
                       paste0(input$catchUnits,   collapse = "+"),
                       paste0(input$years,        collapse = "-"))
        
        components = components[which(components != "")]
        
        paste0("T2CE_", paste0(components, collapse = "_"), ".csv.gz")
      },
      content = function(file) {
        to_download = filter_ce_data()
        
        write.csv(to_download, gzfile(file), row.names = FALSE, na = "")
      }
  )
  
  output$downloadAll = downloadHandler(
    filename = function() {
      return(META$FILENAME)
    },
    content = function(file) {
      file.copy(paste0("www/", META$FILENAME), file)
    }
  )
  
  output
}
