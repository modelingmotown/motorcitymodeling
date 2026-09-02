# Calculate and add features for cod_23_25.Rmd

library(tidyverse)   # data wrangling & ggplot2
library(janitor)     # functions for examining and cleaning dirty data
library(here)        # Find files within a project
library(rlang)       # Functions for Base Types and Core R and 'Tidyverse' Features

df <- read_csv(here("data", "salesSFapr2023toMarch2025Updated.csv")) |>
    rename_with(toupper)    # clean_names()

df <- df |>  
    # ─── 2. Filter to Relevant Styles    Lines 60 -> 67
    dplyr::filter(SRESB_STYLE %in% c("SINGLE FAMILY", "1/2 DUPLEX", "ROW HOUSE")) |> 
    # ─── 3. Deduplicate Records    Lines 93 -> 122
    arrange(PARCELNO, SALEPRICE, SALEDATE) |>
    group_by(PARCELNO, SALEPRICE, SALEDATE) |>
    mutate(PRIMARYFIRST = row_number() == 1, PRIMARYLAST = row_number() == n()) |>
    ungroup() |> dplyr::filter(PRIMARYLAST) |> 
    # ─── 4. Exclude Assessment‐Equal Sales    Lines 129 -> 140
    mutate(
        PRICEISASMT_RATIO  = AVWHENSOLD / SALEPRICE,
        PRICEISASMT_RATIO2 = if_else(between(PRICEISASMT_RATIO, .90, 1.10), 1, PRICEISASMT_RATIO),
        PRICEISASMT        = if_else(PRICEISASMT_RATIO == 1, 0L, 1L)
    ) |>
    dplyr::filter(PRICEISASMT == 1) |> 
    # ─── 5. Outlier Removal    Lines 71 -> 87
    mutate(
        OUT = case_when(
            STOTALSQFT      < 400   ~ 1L,
            SRESB_FLOORAREA < 400   ~ 1L,
            SALEPRICE       < 10000 & SALEPRICE > 1000000 ~ 1L,
            TRUE                    ~ 0L
        )
    ) |>
    dplyr::filter(OUT == 0) |> 
    # ─── 6. Story‐Style Dummies    9. Year‐Built Era  Lines 225 -> 330    
    mutate(
        ONESTORY            = as.integer(SRESB_STYHGT <= 2),
        TWOSTORY            = as.integer(SRESB_STYHGT > 2 & SRESB_STYHGT <= 5),
        SINGLE_FAMILY1STORY = as.integer(SRESB_STYLE == "SINGLE FAMILY" & ONESTORY == 1),
        SINGLE_FAMILY2STORY = as.integer(SRESB_STYLE == "SINGLE FAMILY" & TWOSTORY == 1),
        HALF_DUPLEX1STORY   = as.integer(SRESB_STYLE == "1/2 DUPLEX" & ONESTORY == 1),
        HALF_DUPLEX2STORY   = as.integer(SRESB_STYLE == "1/2 DUPLEX" & TWOSTORY == 1),
        ERABUILT = case_when(
            SRESB_YEARBUILT <  1910                           ~ 1,
            SRESB_YEARBUILT <  1930 & SRESB_YEARBUILT >= 1910 ~ 2,
            SRESB_YEARBUILT <  1946 & SRESB_YEARBUILT >= 1930 ~ 3,
            SRESB_YEARBUILT <  1961 & SRESB_YEARBUILT >= 1946 ~ 4,
            SRESB_YEARBUILT <= 1990 & SRESB_YEARBUILT >= 1961 ~ 5,
            SRESB_YEARBUILT >  1990                           ~ 6
        ),
        YB_PRE_1929     = as.integer(ERABUILT %in% c(1,2)), YB_1930_TO_1945 = as.integer(ERABUILT == 3),
        YB_1946_TO_1960 = as.integer(ERABUILT == 4), YB_POST_1961 = as.integer(ERABUILT %in% c(5,6))
    ) |> 
    # ─── 7. Condition Dummies    Lines 334 -> 386
    mutate(
        EXCELLENT = as.integer(SCOND == 0), VERY_GOOD = as.integer(SCOND == 1),
        GOOD      = as.integer(SCOND == 2), AVERAGE   = as.integer(SCOND == 3),
        FAIR      = as.integer(SCOND == 4), POOR      = as.integer(SCOND == 5),
        VERY_POOR_UNSOUND  = as.integer(SCOND %in% c(6,7)),
        TWO_PLUS_BATHS = as.integer(SRESB_FULLBATHS >= 2), TWO_BATHS = as.integer(SRESB_FULLBATHS == 2),
        POWDER_ROOM    = as.integer(SRESB_HALFBATHS >= 1), ONE_BATH  = as.integer(SRESB_FULLBATHS == 1),
        TOTAL_BATHS    = as.integer(SRESB_FULLBATHS + 0.5 * SRESB_HALFBATHS),
        FIREPLACE      = as.integer(SRESB_FIREPLACE >= 1)
    ) |> 
    # ─── 8. Basement & Garage    Lines 393 -> 473
    mutate(
        PCTCRAWL       = SRESB_CRAWSPACE / SRESB_GROUNDAREA,
        PCTSLAB        = SRESB_SLABAREA / SRESB_GROUNDAREA,
        PCTBSMNT       = SRESB_BASEMENTAREA / SRESB_GROUNDAREA,
        CRAWL          = as.integer(PCTCRAWL > PCTBSMNT & PCTCRAWL > PCTSLAB),
        SLAB           = as.integer(PCTSLAB  > PCTCRAWL & PCTSLAB  > PCTBSMNT),
        GARAGE         = as.integer(SRESB_GARAGEAREA >= 200),
        ONE_CAR_GARAGE = as.integer(SREB_GARTYPE == 1),
        TWO_CAR_GARAGE = as.integer(SREB_GARTYPE == 2),
        GARAGESPACES   = case_when(
            SRESB_GARAGEAREA > 200 & SRESB_GARAGEAREA < 420 ~ 1,
            SRESB_GARAGEAREA > 419 & SRESB_GARAGEAREA < 600 ~ 2,
            SRESB_GARAGEAREA > 599 & SRESB_GARAGEAREA < 820 ~ 3, SRESB_GARAGEAREA > 819 ~ 4
        ),
        LN_GARAGESPACES = log(GARAGESPACES)
    ) |> 
    # ─── 9. Quality & Street Class    Lines 478 -> 544
    mutate(
        QABOVE_AVG      = as.integer(SRESB_BLDGCLASS %in% c(3,4,5)),
        Q_AVG           = as.integer(SRESB_BLDGCLASS == 2),
        QBELOW_AVG      = as.integer(SRESB_BLDGCLASS %in% c(0,1)),
        ARTERIAL        = as.integer(RD_CLASS == "A31"),
        FORCED_AIR      = as.integer(SRESB_HEAT %in% c(0,1)),
        HOT_WATER       = as.integer(SRESB_HEAT == 2), ELEC_BASEBOARD = as.integer(SRESB_HEAT == 3),
        ELEC_RADIANT = as.integer(SRESB_HEAT == 4), RADIANT_FLOOR  = as.integer(SRESB_HEAT == 5),
        ELEC_WALL       = as.integer(SRESB_HEAT == 6), SPACE_HEATER   = as.integer(SRESB_HEAT == 7),
        WALL            = as.integer(SRESB_HEAT == 8), CENTRAL_AIR    = as.integer(SRESB_HEAT == 9),
        SIZE = case_when(
            SRESB_FLOORAREA <   830                            ~ 1,
            SRESB_FLOORAREA <   1018 & SRESB_FLOORAREA >= 830  ~ 2,
            SRESB_FLOORAREA <   1266 & SRESB_FLOORAREA >= 1018 ~ 3,
            SRESB_FLOORAREA <   1659 & SRESB_FLOORAREA >= 1266 ~ 4,
            SRESB_FLOORAREA >=  1659                           ~ 5
        ),
        SMALLEST = as.integer(SIZE == 1), SMALL = as.integer(SIZE == 2),
        AVG = as.integer(SIZE == 3),      LARGE = as.integer(SIZE == 4),
        LARGEST = as.integer(SIZE == 5)
    ) |> 
    # ─── 10. Building Size & Lot Ratios    Lines 551 -> 593
    mutate(
        RATIO                  = SESTTCV / SALEPRICE,
        SPPSF                  = SALEPRICE / SRESB_FLOORAREA,
        BASE_BLDGSIZE          = case_when(
            SRESB_STYLE == "SINGLE FAMILY" ~ 1056,
            SRESB_STYLE == "1/2 DUPLEX"    ~ 840,
            TRUE                           ~ NA_real_
        ),
        BASE_BLDGSIZE_RATIO    = SRESB_FLOORAREA / BASE_BLDGSIZE,
        LN_BASE_BLDGSIZE_RATIO = log(BASE_BLDGSIZE_RATIO),
        LANDRATIO              = STOTALSQFT / case_when(
            SRESB_STYLE == "SINGLE FAMILY" ~ 4599,
            SRESB_STYLE == "1/2 DUPLEX"    ~ 3180,
            TRUE                           ~ NA_real_
        ),
        LANDRATIO2             = pmin(LANDRATIO, if_else(SRESB_STYLE == "SINGLE FAMILY",4,3)),
        LN_LANDRATIO           = log(LANDRATIO2),
        SALEDATE      = as_date(SALEDATE),
        MONTHS        = interval(min(SALEDATE), SALEDATE) %/% months(1), # LINE 498
        MONTHS_1TO9   = case_when(MONTHS <= 9  ~ MONTHS, TRUE ~ 9),
        MONTHS_10TO16 = case_when(MONTHS <= 9  ~ 0, MONTHS <= 16 ~ MONTHS - 9, TRUE ~ 7),
        MONTHS_17TO24 = case_when(MONTHS <= 16 ~ 0, TRUE ~ MONTHS - 16),
        LN_SPPSF      = log(SPPSF),
        LN_PRICE      = log(SALEPRICE),
        LNSBLDSF      = log(SRESB_FLOORAREA), SBLDSFINT = SRESB_FLOORAREA - 432,
        # LNSBLDSFINT   = log(SBLDSFINT),  # LINEs 650 -> 658
    )

save(df, file = here("data/rdata", "cod_23_25_data.RData"))
