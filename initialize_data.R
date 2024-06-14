library(iccat.dev.data)

SPECIES_MAPPINGS =
  tabular_query(
    connection = DB_GIS(server = "ATENEA\\SQL22"),
    statement = "
    SELECT *
    FROM
      NewGIS.dbo.SPECIES_MAPPING_T2CE"
  )

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

# Loads the data from dbSTAT
CE = 
  tabular_query(
    connection = DB_GIS(server = "ATENEA\\SQL22"),
    statement = "SELECT * FROM V_T2CE_WEB"
  )

# Converts weights from tons to kilograms
CE[CATCH_UNIT_CODE == "T", `:=`(CATCH_UNIT_CODE = "KG", CATCH = CATCH * 1000.0)]

# Fixes 'empty' column values
CE[ is.na(PRIMARY_EFFORT_UNIT_CODE)   & !is.na(PRIMARY_EFFORT),   PRIMARY_EFFORT_UNIT_CODE   := "--"]
CE[ is.na(SECONDARY_EFFORT_UNIT_CODE) & !is.na(SECONDARY_EFFORT), SECONDARY_EFFORT_UNIT_CODE := "--"]

#CE = CE[is.na(SPECIES_CODE) | SPECIES_CODE %in% SPECIES_MAPPINGS$SRC_CODE]

CE = merge(CE, SPECIES_MAPPINGS[, .(SRC_CODE, SPECIES_CODE = TRG_CODE)],
           by.x = "SPECIES_CODE", by.y = "SRC_CODE",
           all.x = TRUE)

CE[!is.na(SPECIES_CODE) & is.na(SPECIES_CODE.y), SPECIES_CODE.y := "rest"]

CE$SPECIES_CODE = NULL
CE$SPECIES_CODE = CE$SPECIES_CODE.y
CE$SPECIES_CODE.y = NULL

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

CE[, DATASET_TYPE_CODE := DATASET_TYPE_CODE_CALC]
CE$DATASET_TYPE_CODE_DEFAULT = NULL
CE$DATASET_TYPE_CODE_CALC    = NULL

# Attaches flag metadata to the wide catch records
CE = 
  merge(
    CE, REF_FLAGS[, .(CODE, FLAG_NAME_EN = NAME_EN)],
    by.x = "FLAG_CODE", by.y = "CODE",
    all.x = TRUE
  )

CE_EF = 
  CE[, .(DATASET_ID, STRATA_ID, 
         FLAG_CODE, FLAG_NAME_EN, 
         FLEET_CODE,
         GEAR_GROUP_CODE, GEAR_CODE, 
         YEAR, TIME_PERIOD_TYPE_CODE, TIME_PERIOD_CODE,
         SQUARE_TYPE_CODE, QUADRANT_CODE, LAT, LON, CWP_GRID_CODE,
         FISHING_MODE_CODE,
         PRIMARY_EFFORT, PRIMARY_EFFORT_UNIT_CODE,
         SECONDARY_EFFORT, SECONDARY_EFFORT_UNIT_CODE,
         DATASET_TYPE_CODE)]

# Extracts unique effort strata
CE_EF = unique(CE_EF)

# Commented out to identify strata with primary effort not set (should have been fixed by now...)
#CE_EF[is.na(PRIMARY_EFFORT), PRIMARY_EFFORT := 0]

# Extracts catch strata columns
CE_CA = CE[!is.na(SPECIES_CODE), .(DATASET_ID, STRATA_ID, CATCH_TYPE_CODE, CATCH, CATCH_UNIT_CODE, SPECIES_CODE)]

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

CE_CA_TOTALS = CE_CA[, .(TOTAL = sum(CATCH, na.rm = TRUE)), keyby = .(DATASET_ID, STRATA_ID, CATCH_UNIT_CODE)]

# Long-to-wide conversion of catch records
CE_CA_w = 
  dcast.data.table(
    CE_CA,
    DATASET_ID + STRATA_ID + CATCH_UNIT_CODE ~ SPECIES_CODE,
    fun.aggregate = sum,
    drop = c(TRUE, FALSE),
    value.var = "CATCH",
    fill = NA
  )

# Attaches total catches to the wide catch records
CE_CA_w =
  merge(
    CE_CA_w, CE_CA_TOTALS,
    by = c("DATASET_ID", "STRATA_ID", "CATCH_UNIT_CODE"),
    all.x = TRUE
  )

# Attaches effort and strata columns to the wide catch records
CE_w =
  merge(CE_EF, CE_CA_w,
        by = c("DATASET_ID", "STRATA_ID"),
        all.x = TRUE)

CE_w[is.na(CATCH_UNIT_CODE) & !is.na(TOTAL), CATCH_UNIT_CODE := "--"]

CE_w = CE_w[, .(DATASET_ID, STRATA_ID, 
                FLAG_CODE, FLAG_NAME_EN, 
                FLEET_CODE,
                GEAR_GROUP_CODE, GEAR_CODE, 
                YEAR, TIME_PERIOD_TYPE_CODE, TIME_PERIOD_CODE,
                SQUARE_TYPE_CODE, QUADRANT_CODE, LAT, LON, CWP_GRID_CODE,
                FISHING_MODE_CODE,
                PRIMARY_EFFORT, PRIMARY_EFFORT_UNIT_CODE,
                SECONDARY_EFFORT, SECONDARY_EFFORT_UNIT_CODE,
                CATCH_UNIT_CODE,
                DATASET_TYPE_CODE, 
                TOTAL,
                BFT, ALB,
                YFT, BET, SKJ,
                SWO, BUM, SAI, SPF, WHM,
                BLF, BLT, BON, BOP, BRS, CER, FRI, KGM, LTA, 
                MAW, SLT, SSM, WAH, DOL,
                BIL, BLM, MSP, MLS, RSP, 
                SBF, 
                oTun,
                BSH, POR, SMA, 
                oSks,
                oFis,
                rest)][order(FLAG_NAME_EN, FLEET_CODE, 
                             GEAR_GROUP_CODE, GEAR_CODE, 
                             YEAR, TIME_PERIOD_TYPE_CODE, TIME_PERIOD_CODE,
                             SQUARE_TYPE_CODE, QUADRANT_CODE, LAT, LON,
                             FISHING_MODE_CODE,
                             PRIMARY_EFFORT_UNIT_CODE, SECONDARY_EFFORT_UNIT_CODE,
                             CATCH_UNIT_CODE,
                             DATASET_TYPE_CODE)]

CE[, YEAR_SHORT := str_sub(as.character(YEAR), 3, 4)]
YEAR_SHORT_FACTORS = unique(CE[order(YEAR)]$YEAR_SHORT)

CE$YEAR_SHORT =
  factor(
    CE$YEAR_SHORT,
    levels = YEAR_SHORT_FACTORS,
    labels = YEAR_SHORT_FACTORS,
    ordered = TRUE
  )

META = list(LAST_UPDATE = "2024-01-31", FILENAME = "ICCAT_T2CE_20240131_full.csv.gz")

save("META", file = "./shiny/META.RData", compress = "gzip")
save("CE",   file = "./shiny/CE.RData",   compress = "gzip")
save("CE_w", file = "./shiny/CE_w.RData", compress = "gzip")

write.table(CE_w, file = gzfile(paste0("./shiny/www/", META$FILENAME)), sep = ",", na = "", row.names = FALSE)