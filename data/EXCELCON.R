# This file (EXCELCON.R) will read an Excel file and convert it to csv format
# Written by Elliot Dean, 1 October 2025

library(haven)     # for reading SPSS (if necessary, uncomment)
library(writexl)   # for writing Excel
library(tidyverse) # data wrangling & ggplot2
library(readxl)    # for reading Excel
library(here)      # Find files within a project

# 1. Read the .sav file
df33 <- read_sav(here("data/city", "CCD1_2023-2025_Data.sav")) |> 
  write_csv(here("data/city", "CCD1_2023-2025_Data.csv"))

# 2. (Optional) Convert labelled columns to factors or plain values
#    e.g. df <- as_factor(df)    # turns labelled variables into R factors

# 3. Write to .xlsx or .csv (see below)
# write_xlsx(df, "../data/GrandmontECFsProjections_Data.xlsx")

# here references the folder the files are in
xlfile_2 <- read_excel(here("data/city", "SF-Sales-Apr2021-Mar2026-08182026.xlsx")) |> 
   write_csv(here("data/city", "SF-Sales-Apr2021-Mar2026-08182026.csv"))

xlfile_2 <- read_excel(here("data/spss/spatial_data", "salesSFapr2023toMarch2025_withSpatials.xlsx")) |> 
  write_csv(here("data/spss/spatial_data", "salesSFapr2023toMarch2025_withSpatials.csv"))
