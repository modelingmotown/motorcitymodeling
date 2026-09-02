# DETROIT RESIDENTIAL MODEL FOR CITY COUNCIL DISTRICT 1
# SALE DATES: APRIL 1, 2023 - MARCH 31ST, 2025

# Load required packages
library(tidyverse)
library(lubridate)
library(car)      # For standard outlier tools

# Load your custom modeling functions
# load(here::here("rmds/modfunctions", "modfunctions12.RData"))

# Assuming your starting dataframe is called 'sales_data'
# df <- read.csv("your_sales_file.csv") 

# ******************************************************************************
# RECORD FILTERING
# ******************************************************************************
df <- sales_data %>%
  # Force all original column names to lowercase to prevent case-mismatch errors
  rename_with(tolower) %>% 
  
  # Filter for District 1
  filter(ccd == 1) %>%
  
  # CREATE PRICE CATEGORIES
  mutate(
    priceclass = case_when(
      saleprice < 45000 ~ 1,
      saleprice >= 45000 & saleprice < 80000 ~ 2,
      saleprice >= 80000 & saleprice < 115000 ~ 3,
      saleprice >= 115000 & saleprice < 180000 ~ 4,
      saleprice >= 180000 & saleprice < 250000 ~ 5,
      saleprice >= 250000 ~ 6,
      TRUE ~ 0
    )
  ) %>%
  
  # Property Style & Bad Data Filtering
  filter(
    sresb_style %in% c('SINGLE FAMILY', '1/2 DUPLEX', 'ROW HOUSE'),
    stotalsqft >= 400,
    sresb_floorarea >= 400,
    saleprice >= 10000
  ) %>%
  
  # REMOVE DUPLICATE CASES
  # Arranges by Parcel, Price, Date, then takes the last instance per parcel
  arrange(parcelno, saleprice, saledate) %>%
  group_by(parcelno) %>%
  slice_tail(n = 1) %>% 
  ungroup() %>%
  
  # Eliminate non-market sales (Sale close to assessment)
  mutate(priceisasmt_ratio = avwhensold / saleprice) %>%
  filter(!(priceisasmt_ratio >= 0.90 & priceisasmt_ratio <= 1.10)) %>%
  
  # ******************************************************************************
  # SETUP CALCULATIONS
  # ******************************************************************************
  mutate(
    ratio = sesttcv / saleprice,
    sppsf = saleprice / sresb_floorarea
  )

# ******************************************************************************
# CATEGORICAL BINARY TRANSFORMATIONS
# ******************************************************************************
df <- df %>%
  mutate(
    # Property Type
    onestory = ifelse(sresb_styhgt <= 2, 1, 0),
    twostory = ifelse(sresb_styhgt > 2 & sresb_styhgt <= 5, 1, 0),
    twoplusstory = ifelse(sresb_styhgt > 5 & sresb_styhgt <= 8, 1, 0),
    bilevelsplit = ifelse(sresb_styhgt > 8 & sresb_styhgt <= 10, 1, 0),
    
    # Base is single_family1story
    single_family2story = ifelse(sresb_style == 'SINGLE FAMILY' & twostory == 1, 1, 0),
    half_duplex1story   = ifelse(sresb_style == '1/2 DUPLEX' & onestory == 1, 1, 0),
    half_duplex2story   = ifelse(sresb_style == '1/2 DUPLEX' & twostory == 1, 1, 0),
    
    # Building Eras
    erabuilt = case_when(
      sresb_yearbuilt < 1910 ~ 1,
      sresb_yearbuilt >= 1910 & sresb_yearbuilt < 1930 ~ 2,
      sresb_yearbuilt >= 1930 & sresb_yearbuilt < 1946 ~ 3,
      sresb_yearbuilt >= 1946 & sresb_yearbuilt < 1961 ~ 4, # Base
      sresb_yearbuilt >= 1961 & sresb_yearbuilt <= 1990 ~ 5,
      sresb_yearbuilt > 1990 ~ 6,
      TRUE ~ 0
    ),
    yb_pre_1910     = ifelse(erabuilt == 1, 1, 0),
    yb_1910_to_1929 = ifelse(erabuilt == 2, 1, 0),
    yb_1930_to_1945 = ifelse(erabuilt == 3, 1, 0),
    yb_1961_to_1990 = ifelse(erabuilt == 5, 1, 0),
    yb_post_1990    = ifelse(erabuilt == 6, 1, 0),
    
    # Condition (Base is 3 / avg)
    good    = ifelse(scond == 2, 1, 0),
    fair    = ifelse(scond == 4, 1, 0),
    poor    = ifelse(scond == 5, 1, 0),
    unsound = ifelse(scond == 7, 1, 0),
    
    # Bathrooms (Base is one_bath)
    two_baths        = ifelse(sresb_fullbaths == 2, 1, 0),
    three_plus_baths = ifelse(sresb_fullbaths >= 3, 1, 0),
    powder_room      = ifelse(sresb_halfbaths >= 1, 1, 0),
    
    # Fireplaces (Base is no fireplace)
    fireplace = ifelse(sresb_fireplace >= 1, 1, 0),
    
    # Basements (Base is basement)
    pctcrawl = sresb_crawspace / sresb_groundarea,
    pctslab  = sresb_slabarea / sresb_groundarea,
    pctbsmnt = sresb_basementarea / sresb_groundarea,
    crawl    = ifelse(pctcrawl > pctbsmnt & pctcrawl > pctslab, 1, 0),
    slab     = ifelse(pctslab > pctcrawl & pctslab > pctbsmnt, 1, 0),
    
    # Building Class (Base is 2 / C)
    qbest      = ifelse(sresb_bldgclass == 5, 1, 0),
    qabove_avg = ifelse(sresb_bldgclass %in% c(3, 4), 1, 0),
    qbelow_avg = ifelse(sresb_bldgclass == 1, 1, 0),
    qpoor      = ifelse(sresb_bldgclass == 0, 1, 0),
    
    # Garages
    garage = ifelse(sresb_garagearea >= 200, 1, 0),
    garagespaces = case_when(
      sresb_garagearea > 200 & sresb_garagearea < 420 ~ 1,
      sresb_garagearea >= 420 & sresb_garagearea < 600 ~ 2,
      sresb_garagearea >= 600 & sresb_garagearea < 820 ~ 3,
      sresb_garagearea >= 820 ~ 4,
      TRUE ~ 0
    ),
    # R NOTE: log(0) is -Inf. We use NA to match SPSS missing handling.
    ln_garagespaces = ifelse(garagespaces > 0, log(garagespaces), NA_real_),
    
    # Road Class
    arterial = ifelse(rd_class %in% c('A21', 'A31'), 1, 0),
    
    # Heating Cooling (Base is forced_air)
    hot_water      = ifelse(sresb_heat == 2, 1, 0),
    elec_baseboard = ifelse(sresb_heat == 3, 1, 0),
    wall           = ifelse(sresb_heat %in% c(6, 8), 1, 0),
    central_air    = ifelse(sresb_heat == 9, 1, 0),
    
    # Building Size (Base is 3 / avg)
    size = case_when(
      sresb_floorarea < 830 ~ 1,
      sresb_floorarea >= 830 & sresb_floorarea < 1018 ~ 2,
      sresb_floorarea >= 1018 & sresb_floorarea < 1266 ~ 3,
      sresb_floorarea >= 1266 & sresb_floorarea < 1659 ~ 4,
      sresb_floorarea >= 1659 ~ 5,
      TRUE ~ 0
    ),
    smallest = ifelse(size == 1, 1, 0),
    small    = ifelse(size == 2, 1, 0),
    large    = ifelse(size == 4, 1, 0),
    largest  = ifelse(size == 5, 1, 0)
  )

# ******************************************************************************
# TIME ANALYSES
# ******************************************************************************
df <- df %>%
  mutate(
    smonth = month(saledate),
    syear  = year(saledate),
    months = case_when(
      syear == 2023 ~ smonth - 3,
      syear == 2024 ~ smonth + 9,
      syear == 2025 ~ smonth + 21,
      TRUE ~ 0
    ),
    months_1to9   = ifelse(months > 9, 9, months),
    months_10to18 = case_when(
      months > 18 ~ 9,
      months > 9 ~ months - 9,
      TRUE ~ 0
    ),
    months_19to24 = ifelse(months > 18, months - 18, 0),
    ln_sppsf = log(sppsf)
  )

# Define your rates from the initial Time Regression
rate1 <- 0
rate2 <- 0.017448
rate3 <- 0
end_index <- 1.1684

df <- df %>%
  mutate(
    price_index = (1+rate1)^months_1to9 * (1+rate2)^months_10to18 * (1+rate3)^months_19to24,
    taf         = end_index / price_index,
    tasp        = saleprice * taf,
    tasppsf     = tasp / sresb_floorarea,
    ln_tasp     = log(tasp),
    ln_price    = log(saleprice)
  )


# ******************************************************************************
# BUILDING SIZE (SBLDSF) & LOT SIZE TRANSFORMATIONS
# ******************************************************************************
df <- df %>%
  mutate(
    lnsbldsf = log(sresb_floorarea),
    sbldsfint = sresb_floorarea - 541,
    lnsbldsfint = log(ifelse(sresb_floorarea - 541 <= 0, NA, sresb_floorarea - 541)),
    sbldsfmed = sresb_floorarea - 1104,
    lnsbldsfmed = log(ifelse(sresb_floorarea - 1104 <= 0, NA, sresb_floorarea - 1104)),
    
    base_bldgsize = case_when(
      sresb_style == 'SINGLE FAMILY' ~ 1108,
      sresb_style == '1/2 DUPLEX' ~ 842,
      TRUE ~ 1 # Prevents div/0
    ),
    base_bldgsize_ratio = sresb_floorarea / base_bldgsize,
    ln_base_bldgsize_ratio = log(base_bldgsize_ratio),
    
    base_lotsize = case_when(
      sresb_style == 'SINGLE FAMILY' ~ 5184,
      sresb_style == '1/2 DUPLEX' ~ 3006,
      TRUE ~ 1
    ),
    base_lotsize_ratio = stotalsqft / base_lotsize,
    landratio = stotalsqft / base_lotsize,
    landratio2 = case_when(
      landratio >= 0.90 & landratio <= 1.10 ~ 1,
      sresb_style != 'SINGLE FAMILY' & landratio > 3 ~ 3,
      sresb_style == 'SINGLE FAMILY' & landratio > 4 ~ 4,
      TRUE ~ landratio
    ),
    ln_landratio = log(landratio2)
  )

# ******************************************************************************
# ECF DUMMIES
# ******************************************************************************
# Create explicit dummies like SPSS (1R135 is implicitly excluded as base)
ecf_codes <- unique(df$ecf)
ecf_codes <- ecf_codes[ecf_codes != "1R135"] # Drop base

for(ecf_val in ecf_codes) {
  ecf_col_name <- tolower(paste0("ecf_", ecf_val))
  df[[ecf_col_name]] <- ifelse(df$ecf == ecf_val, 1, 0)
}

# Formula Helper: Create string of all lowercase ECF variables created
ecf_formula_vars <- paste0(tolower(paste0("ecf_", ecf_codes)), collapse = " + ")

# ******************************************************************************
# REGRESSIONS
# ******************************************************************************

# --- BASE MODEL (No Outliers Removed Yet) ---
formula_mod1 <- as.formula(paste(
  "ln_price ~ ln_landratio + ln_base_bldgsize_ratio + arterial +",
  "single_family2story + half_duplex1story + half_duplex2story +",
  "smallest + small + large + largest + two_baths + three_plus_baths + powder_room +",
  "good + fair + poor + unsound + qabove_avg + qbelow_avg + qpoor +",
  "yb_pre_1910 + yb_1910_to_1929 + yb_1930_to_1945 + yb_1961_to_1990 + yb_post_1990 +",
  "crawl + slab + ln_garagespaces + fireplace +",
  "hot_water + elec_baseboard + wall + central_air +",
  "months_1to9 + months_10to18 + months_19to24 +",
  ecf_formula_vars
))

# Assuming `backward_p` is your custom function that drops variables based on P-values
mod1 <- backward_p(formula_mod1, data = df, p_out = 0.10)

# Calculate ratio stats for Model 1
df$pre_1 <- predict(mod1, newdata = df)
df$esp1 <- exp(df$pre_1)
df$ratio1 <- df$esp1 / df$saleprice


# --- OUTLIER REMOVAL (Model 2) ---
df$coo_1 <- cooks.distance(mod1)
df$sdr_1 <- rstudent(mod1)

# Create a filtered dataframe without outliers
df_clean <- df %>% filter(coo_1 <= (4/nrow(df)) & abs(sdr_1) <= 2)

mod2 <- backward_p(formula_mod1, data = df_clean, p_out = 0.10)


# --- RECALIBRATED TIME MODEL (Model 3) ---
# NOTE: Update rate2_new and end_index_new based on your model output
rate2_new <- 0.017347
end_index_new <- 1.1674

df_clean <- df_clean %>%
  mutate(
    price_index = (1+0)^months_1to9 * (1+rate2_new)^months_10to18 * (1+0)^months_19to24,
    taf         = end_index_new / price_index,
    tasp        = saleprice * taf,
    ln_tasp     = log(tasp)
  )

# Remove Time Variables from Model 3 Formula
formula_mod3 <- update(formula_mod1, ln_tasp ~ . - months_1to9 - months_10to18 - months_19to24)

mod3 <- backward_p(formula_mod3, data = df_clean, p_out = 0.10)


# --- FINAL MODEL (Model 4) ---
# Swapping ln_base_bldgsize_ratio for lnsbldsfint, and ln_garagespaces for garage
formula_mod4 <- update(formula_mod3, . ~ . - ln_base_bldgsize_ratio - ln_garagespaces + lnsbldsfint + garage)

mod4 <- backward_p(formula_mod4, data = df_clean, p_out = 0.10)

# Final outputs
df_clean$pre_4 <- predict(mod4, newdata = df_clean)
df_clean$modmv <- exp(df_clean$pre_4)
df_clean$ratio4 <- df_clean$modmv / df_clean$tasp

# Generate Final Ratio Stats using your custom function
# ratio_stats_by(df_clean, "ratio4", group_by = "ecf")