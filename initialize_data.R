library(iccat.dev.data)

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

CE = 
  tabular_query(
    connection = DB_GIS(server = "ATENEA\\SQL22"),
    statement = "SELECT * FROM V_T2CE_WEB"
  )

CE[is.na(CATCH_UNIT_CODE), CATCH_UNIT_CODE := ".."]
CE[is.na(PRIMARY_EFFORT_UNIT_CODE),   PRIMARY_EFFORT_UNIT_CODE   := "--"]
CE[is.na(SECONDARY_EFFORT_UNIT_CODE), SECONDARY_EFFORT_UNIT_CODE := "--"]

CE_EF = unique(CE[, 1:25])
CE_EF[is.na(PRIMARY_EFFORT), PRIMARY_EFFORT := 0]

CE_CA = CE[!is.na(SPECIES_CODE), c(1, 2, 26:30)]
CE_CA[CATCH_UNIT_CODE == "T", `:=`(CATCH_UNIT_CODE = "KG", CATCH = CATCH * 1000.0)]

CE_CA = 
  merge(CE_CA, REF_SPECIES,
        by.x = "SPECIES_CODE", by.y = "CODE",
        all.x = TRUE)

CE_CA = CE_CA[SPECIES_GROUP_ICCAT %in% c("Tunas and billfish (major)",
                                         "Tunas (small)",
                                         "Tunas (other)",
                                         "Sharks (major)",
                                         "Sharks (other)")]

CE_CA[SPECIES_GROUP_ICCAT == "Tunas (other)",  SPECIES_CODE := "oTun"]
CE_CA[SPECIES_GROUP_ICCAT == "Sharks (other)", SPECIES_CODE := "oSks"]

CE_CA = CE_CA[, 1:7][, .(CATCH = sum(CATCH * 1.0, na.rm = TRUE)), keyby = .(SPECIES_CODE, DATASET_ID, STRATA_ID, CATCH_UNIT_CODE)]

CE_CA$SPECIES_CODE =
  factor(
    CE_CA$SPECIES_CODE,
    levels = SPECIES_ORDERED,
    labels = SPECIES_ORDERED,
    ordered = TRUE
  )

CE_CA_w = 
  dcast.data.table(
    CE_CA,
    DATASET_ID + STRATA_ID + CATCH_UNIT_CODE ~ SPECIES_CODE,
    fun.aggregate = sum,
    drop = c(TRUE, FALSE),
    value.var = "CATCH",
    fill = 0
  )

CE_w =
  merge(CE_EF, CE_CA_w,
        by = c("DATASET_ID", "STRATA_ID"),
        all.x = TRUE)

CE_w = 
  merge(
    CE_w, REF_FLAGS,
    by.x = "FLAG_CODE", by.y = "CODE",
    all.x = TRUE
  )

CE_w = CE_w[, .(DATASET_ID, DATASET_TYPE_CODE, STRATA_ID, 
                FLAG_CODE, FLAG_NAME_EN = NAME_EN, 
                FLEET_CODE,
                GEAR_GROUP_CODE, GEAR_CODE, 
                TIME_CODE, YEAR, MONTH_START, MONTH_END,
                SQUARE_TYPE_CODE, QUADRANT_CODE, LAT, LON, CWP_GRID_CODE = GRID_CODE,
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