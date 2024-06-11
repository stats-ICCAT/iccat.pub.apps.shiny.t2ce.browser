library(iccat.dev.data)

REF_TIME_PERIODS = 
  tabular_query(
    connection = DB_STAT(server = "ATENEA\\SQL22"),
    statement = "
    SELECT
      TimePeriodID AS CODE,
      TimePeriod AS NAME_EN,
      TimePeriodGroup AS TYPE_CODE
    FROM
      dbSTAT.dbo.TimePeriods
    ORDER BY
      1 ASC"
  )

save("REF_TIME_PERIODS", file = "./shiny/REF_TIME_PERIODS.RData", compress = "gzip")

SPECIES_ORDERED = c("BFT",
                    "ALB",
                    "YFT",
                    "BET",
                    "SKJ",
                    "SWO",
                    "BUM",
                    "SAI",
                    "SPF",
                    "WHM",
                    "BLF",
                    "BLT",
                    "BON",
                    "BOP",
                    "BRS",
                    "CER",
                    "FRI",
                    "KGM",
                    "LTA",
                    "MAW",
                    "SLT",
                    "SSM",
                    "WAH", 
                    "DOL",
                    "BIL",
                    "BLM",
                    "MSP",
                    "MLS",
                    "RSP",
                    "SBF",
                    "oTun",
                    "BSH",
                    "POR",
                    "SMA",
                    "oSks")

# Loads the data from dbSTAT
CE = 
  tabular_query(
    connection = DB_GIS(server = "ATENEA\\SQL22"),
    statement = "SELECT * FROM V_T2CE_WEB"
  )

# Fixes 'empty' column values
CE[is.na(CATCH_UNIT_CODE), CATCH_UNIT_CODE := ".."]
CE[is.na(PRIMARY_EFFORT_UNIT_CODE),   PRIMARY_EFFORT_UNIT_CODE   := "--"]
CE[is.na(SECONDARY_EFFORT_UNIT_CODE), SECONDARY_EFFORT_UNIT_CODE := "--"]

# Converts weights from tons to kilograms
CE[CATCH_UNIT_CODE == "T", `:=`(CATCH_UNIT_CODE = "KG", CATCH = CATCH * 1000.0)]

# Removes all records related to catches of non-relevant species
CE = CE[is.na(SPECIES_CODE) | SPECIES_CODE %in% REF_SPECIES[SPECIES_GROUP_ICCAT %in% c("Tunas and billfish (major)",
                                                                                       "Tunas (small)",
                                                                                       "Tunas (other)",
                                                                                       "Sharks (major)",
                                                                                       "Sharks (other)")]$CODE]

# Updates the species codes for those species that belong to large / secondary groups ('other tunas' and 'other sharks')
CE[SPECIES_CODE %in% REF_SPECIES[SPECIES_GROUP_ICCAT == "Tunas (other)"]$CODE,  SPECIES_CODE := "oTun"]
CE[SPECIES_CODE %in% REF_SPECIES[SPECIES_GROUP_ICCAT == "Sharks (other)"]$CODE, SPECIES_CODE := "oSks"]

# Determines if catches are available in weight / number / both for each stratum
CE_units      = unique(CE[, .(DATASET_ID, STRATA_ID)])
CE_CA_units_N = CE[CATCH_UNIT_CODE == "NO", .(N = sum(CATCH, na.rm = TRUE)), keyby = .(DATASET_ID, STRATA_ID)]
CE_CA_units_W = CE[CATCH_UNIT_CODE == "KG", .(W = sum(CATCH, na.rm = TRUE)), keyby = .(DATASET_ID, STRATA_ID)]

CE_units = 
  merge(
    CE_units, CE_CA_units_W,
    all.x = TRUE
  )

CE_units = 
  merge(
    CE_units, CE_CA_units_N,
    all.x = TRUE
  )

CE_units[, DATASET_TYPE_CODE_CALC := paste0(ifelse(is.na(N), ".", "n"), ifelse(is.na(W), ".", "w"))]

# Attaches the calculated dataset type code to the original dataset
CE = merge(CE, CE_units[, .(DATASET_ID, STRATA_ID, DATASET_TYPE_CODE_CALC)],
           by = c("DATASET_ID", "STRATA_ID"),
           all.x = TRUE)

# Extracts effort strata columns
CE_EF = unique(CE[, c(1:26, 32)])
CE_EF[is.na(PRIMARY_EFFORT), PRIMARY_EFFORT := 0]

# Extracts catch strata columns
CE_CA = CE[!is.na(SPECIES_CODE), c(1, 2, 27:31)]

# Removes all species metadata columns and attempts to convert all catches into 'float' numbers...
CE_CA = CE_CA[, .(CATCH = sum(CATCH * 1.0, na.rm = TRUE)), keyby = .(SPECIES_CODE, DATASET_ID, STRATA_ID, CATCH_UNIT_CODE)]

# Factorises the species code according to the order set by SPECIES_ORDERED
CE_CA$SPECIES_CODE =
  factor(
    CE_CA$SPECIES_CODE,
    levels = SPECIES_ORDERED,
    labels = SPECIES_ORDERED,
    ordered = TRUE
  )

# Long-to-wide conversion of catch records
CE_CA_w = 
  dcast.data.table(
    CE_CA,
    DATASET_ID + STRATA_ID + CATCH_UNIT_CODE ~ SPECIES_CODE,
    fun.aggregate = sum,
    drop = c(TRUE, FALSE),
    value.var = "CATCH",
    fill = 0
  )

# Attaches effort and strata columns to the wide catch records
CE_w =
  merge(CE_EF, CE_CA_w,
        by = c("DATASET_ID", "STRATA_ID"),
        all.x = TRUE)

# Attaches flag metadata to the wide catch records
CE_w = 
  merge(
    CE_w, REF_FLAGS,
    by.x = "FLAG_CODE", by.y = "CODE",
    all.x = TRUE
  )

# Returns the final dataset 
CE_w = CE_w[, .(DATASET_ID, STRATA_ID, DATASET_TYPE_CODE_DEFAULT, DATASET_TYPE_CODE_CALC, 
                FLAG_CODE, FLAG_NAME_EN = NAME_EN, 
                FLEET_CODE,
                GEAR_GROUP_CODE, GEAR_CODE, 
                TIME_PERIOD_CODE, TIME_PERIOD_TYPE_CODE, YEAR, MONTH_START, MONTH_END,
                SQUARE_TYPE_CODE, QUADRANT_CODE, LAT, LON, CWP_GRID_CODE,
                PRIMARY_EFFORT,
                PRIMARY_EFFORT_UNIT_CODE,
                SECONDARY_EFFORT,
                SECONDARY_EFFORT_UNIT_CODE,
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

save("CE_w", file = "./shiny/CE_w.RData", compress = "gzip")