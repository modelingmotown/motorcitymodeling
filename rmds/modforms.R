# This is a list of formulas used for model development.
# First set are dedicated to linear models.
# Second set are used for non-linear models.
# Third set will be used for ML approaches.

# SET ONE

formula_time <- ln_sppsf ~ arterial + single_family1story + half_duplex1story + half_duplex2story +
  smallest + small + large + largest + two_plus_baths + powder_room +
  good + fair + poor + very_poor_unsound + qabove_avg + qbelow_avg +
  yb_pre_1929 + yb_1946_to_1960 + yb_post_1961 + crawl + slab + ln_garagespaces + fireplace +
  hot_water + elec_baseboard + wall + central_air +
  months_1to9 + months_10to16 + months_17to24

formula_time_nc <- ln_sppsf ~ arterial + single_family1story + half_duplex1story + half_duplex2story +
  smallest + small + large + largest + two_plus_baths + powder_room + qabove_avg + qbelow_avg +
  yb_pre_1929 + yb_1946_to_1960 + yb_post_1961 + crawl + slab + ln_garagespaces + fireplace +
  hot_water + elec_baseboard + wall + central_air +
  months_1to9 + months_10to16 + months_17to24

formula_ln_price_no_ecf <- ln_price ~ ln_landratio + ln_base_bldgsize_ratio + arterial +
  single_family1story + half_duplex1story + half_duplex2story +
  smallest + small + large + largest + two_plus_baths + powder_room +
  good + fair + poor + very_poor_unsound + qabove_avg + qbelow_avg +
  yb_pre_1929 + yb_1946_to_1960 + yb_post_1961 + crawl + slab + ln_garagespaces + fireplace +
  hot_water + elec_baseboard + wall + central_air +
  months_1to9 + months_10to16 + months_17to24

formula_ln_price_no_ecf_nc <- ln_price ~ ln_landratio + ln_base_bldgsize_ratio + arterial +
  single_family1story + half_duplex1story + half_duplex2story +
  smallest + small + large + largest + two_plus_baths + powder_room + qabove_avg + qbelow_avg +
  yb_pre_1929 + yb_1946_to_1960 + yb_post_1961 + crawl + slab + ln_garagespaces + fireplace +
  hot_water + elec_baseboard + wall + central_air +
  months_1to9 + months_10to16 + months_17to24

baserhs <- c(
  "ln_landratio", "ln_base_bldgsize_ratio", "arterial",
  "single_family1story", "half_duplex1story", "half_duplex2story",
  "smallest", "small", "large", "largest", "two_plus_baths", "powder_room",
  "good", "fair", "poor", "very_poor_unsound", "qabove_avg", "qbelow_avg",
  "yb_pre_1929", "yb_1946_to_1960", "yb_post_1961", "crawl", "slab",
  "ln_garagespaces", "fireplace", "hot_water", "elec_baseboard", "wall", "central_air",
  ecfs_col,  # <-- splices *all* secf_ dummy columns in correctly
  "months_1to9", "months_10to16", "months_17to24"
)

formula_base <- reformulate(baserhs, response = "ln_price")

baserhsnc <- c(
  "ln_landratio", "ln_base_bldgsize_ratio", "arterial",
  "single_family1story", "half_duplex1story", "half_duplex2story",
  "smallest", "small", "large", "largest", "two_plus_baths", "powder_room",
  "qabove_avg", "qbelow_avg",
  "yb_pre_1929", "yb_1946_to_1960", "yb_post_1961", "crawl", "slab",
  "ln_garagespaces", "fireplace", "hot_water", "elec_baseboard", "wall", "central_air",
  ecfs_col,  # <-- splices *all* secf_ dummy columns in correctly
  "months_1to9", "months_10to16", "months_17to24"
)

formula_base_nc <- reformulate(baserhsnc, response = "ln_price")

tasprhs <- c(
  "ln_landratio", "ln_base_bldgsize_ratio", "arterial",
  "single_family1story", "half_duplex1story", "half_duplex2story",
  "smallest", "small", "large", "largest", "two_plus_baths", "powder_room",
  "good", "fair", "poor", "very_poor_unsound", "qabove_avg", "qbelow_avg",
  "yb_pre_1929", "yb_1946_to_1960", "yb_post_1961", "crawl", "slab",
  "ln_garagespaces", "fireplace", "hot_water", "elec_baseboard", "wall", "central_air", ecfs_col
)

formula_ln_tasp <- reformulate(tasprhs, response = "ln_tasp_re")

tasprhsnc <- c(
  "ln_landratio", "ln_base_bldgsize_ratio", "arterial",
  "single_family1story", "half_duplex1story", "half_duplex2story",
  "smallest", "small", "large", "largest", "two_plus_baths", "powder_room",
  "qabove_avg", "qbelow_avg",
  "yb_pre_1929", "yb_1946_to_1960", "yb_post_1961", "crawl", "slab",
  "ln_garagespaces", "fireplace", "hot_water", "elec_baseboard", "wall", "central_air", ecfs_col
)

formula_ln_tasp_nc <- reformulate(tasprhsnc, response = "ln_tasp_re")

altrhs <- c(
  "ln_landratio", "lnsbldsfint", "arterial",
  "single_family1story", "half_duplex1story", "half_duplex2story",
  "smallest", "small", "large", "largest", "two_plus_baths", "powder_room",
  "good", "fair", "poor", "very_poor_unsound", "qabove_avg", "qbelow_avg",
  "yb_pre_1929", "yb_1946_to_1960", "yb_post_1961", "crawl", "slab", "garage", "fireplace",
  "hot_water", "elec_baseboard", "wall", "central_air", ecfs_col
)

formula_alt <- reformulate(altrhs, response = "ln_tasp_re")

altrhsnc <- c(
  "ln_landratio", "lnsbldsfint", "arterial",
  "single_family1story", "half_duplex1story", "half_duplex2story",
  "smallest", "small", "large", "largest", "two_plus_baths", "powder_room",
  "qabove_avg", "qbelow_avg",
  "yb_pre_1929", "yb_1946_to_1960", "yb_post_1961", "crawl", "slab", "garage", "fireplace",
  "hot_water", "elec_baseboard", "wall", "central_air", ecfs_col
)

formula_alt_nc <- reformulate(altrhsnc, response = "ln_tasp_re")

# SET TWO

formula_time_nlm <- ln_sppsf ~ arterial + single_family1story + half_duplex1story + half_duplex2story +
  smallest + small + large + largest + # s(lnsbldsf, bs="cs", k=10)
  two_plus_baths + powder_room + # s(totalbath, bs="cs", k=6)
  good + fair + poor + very_poor_unsound + qabove_avg + qbelow_avg + # citycd
  s(sresb_yearbuilt, bs = "cs", k = 10) + crawl + slab + s(ln_garagespaces, bs = "cs", k = 4) +
  fireplace + hot_water + elec_baseboard + wall + central_air + s(months, bs = "cs", k = 10)

formula_ln_price_no_ecf_nlm <- ln_price ~
  s(ln_landratio, bs = "cs", k = 10) + s(ln_base_bldgsize_ratio, bs = "cs", k = 10) +
  arterial + single_family1story + half_duplex1story + half_duplex2story +
  smallest + small + large + largest + two_plus_baths + powder_room +
  good + fair + poor + very_poor_unsound + qabove_avg + qbelow_avg +
  s(sresb_yearbuilt, bs = "cs", k = 10) + crawl + slab + s(ln_garagespaces, bs = "cs", k = 4) +
  fireplace + hot_water + elec_baseboard + wall + central_air + s(months, bs = "cs", k = 10)

formula_base_nlm <- ln_price ~
  s(ln_landratio, bs = "cs", k = 10) + s(ln_base_bldgsize_ratio, bs = "cs", k = 10) + arterial +
  single_family1story + half_duplex1story + half_duplex2story + smallest + small + large + largest +
  two_plus_baths + powder_room + good + fair + poor + very_poor_unsound +
  qabove_avg + qbelow_avg + s(sresb_yearbuilt, bs = "cs", k = 10) + crawl + slab +
  s(ln_garagespaces, bs = "cs", k = 4) +
  fireplace + hot_water + elec_baseboard + wall + central_air + s(months, bs = "cs", k = 10) + ecf

formula_ln_tasp_nlm <- ln_tasp ~
  s(ln_landratio, bs = "cs", k = 10) + s(ln_base_bldgsize_ratio, bs = "cs", k = 10) + arterial +
  single_family1story + half_duplex1story + half_duplex2story +
  smallest + small + large + largest + two_plus_baths + powder_room +
  good + fair + poor + very_poor_unsound + qabove_avg + qbelow_avg +
  s(sresb_yearbuilt, bs = "cs", k = 10) + crawl + slab + s(ln_garagespaces, bs = "cs", k = 4) +
  fireplace + hot_water + elec_baseboard + wall + central_air + ecf + # s(ecf, bs = "re") +
  s(months, bs = "cs", k = 10)

# SET THREE

formula_time_g1 <- ln_sppsf ~ arterial + single_family1story + half_duplex1story + half_duplex2story +
  smallest + small + large + largest + two_plus_baths + powder_room +
  good + fair + poor + very_poor_unsound + qabove_avg + qbelow_avg +
  yb_pre_1929 + yb_1946_to_1960 + yb_post_1961 + crawl + slab + ln_garagespaces + fireplace +
  hot_water + elec_baseboard + wall + central_air +
  months_1to9 + months_10to16 + months_17to24 + near_downtown_1mi + near_downtown_5mi

formula_ln_price_no_ecf_g1 <- ln_price ~ ln_landratio + ln_base_bldgsize_ratio + arterial +
  single_family1story + half_duplex1story + half_duplex2story +
  smallest + small + large + largest + two_plus_baths + powder_room +
  good + fair + poor + very_poor_unsound + qabove_avg + qbelow_avg +
  yb_pre_1929 + yb_1946_to_1960 + yb_post_1961 + crawl + slab + ln_garagespaces + fireplace +
  hot_water + elec_baseboard + wall + central_air +
  months_1to9 + months_10to16 + months_17to24 + near_downtown_1mi + near_downtown_5mi

baserhs_g1 <- c(
  "ln_landratio", "ln_base_bldgsize_ratio", "arterial",
  "single_family1story", "half_duplex1story", "half_duplex2story",
  "smallest", "small", "large", "largest", "two_plus_baths", "powder_room",
  "good", "fair", "poor", "very_poor_unsound", "qabove_avg", "qbelow_avg",
  "yb_pre_1929", "yb_1946_to_1960", "yb_post_1961", "crawl", "slab",
  "ln_garagespaces", "fireplace", "hot_water", "elec_baseboard", "wall", "central_air",
  ecfs_col,  # <-- splices *all* secf_ dummy columns in correctly
  "months_1to9", "months_10to16", "months_17to24", "near_downtown_1mi", "near_downtown_5mi"
)

formula_base_g1 <- reformulate(baserhs_g1, response = "ln_price")

baserhsnc_g1 <- c(
  "ln_landratio", "ln_base_bldgsize_ratio", "arterial",
  "single_family1story", "half_duplex1story", "half_duplex2story",
  "smallest", "small", "large", "largest", "two_plus_baths", "powder_room",
  "qabove_avg", "qbelow_avg",
  "yb_pre_1929", "yb_1946_to_1960", "yb_post_1961", "crawl", "slab",
  "ln_garagespaces", "fireplace", "hot_water", "elec_baseboard", "wall", "central_air",
  ecfs_col,  # <-- splices *all* secf_ dummy columns in correctly
  "months_1to9", "months_10to16", "months_17to24", "near_downtown_1mi", "near_downtown_5mi"
)

formula_base_nc_g1 <- reformulate(baserhsnc_g1, response = "ln_price")

tasprhs_g1 <- c(
  "ln_landratio", "ln_base_bldgsize_ratio", "arterial",
  "single_family1story", "half_duplex1story", "half_duplex2story",
  "smallest", "small", "large", "largest", "two_plus_baths", "powder_room",
  "good", "fair", "poor", "very_poor_unsound", "qabove_avg", "qbelow_avg",
  "yb_pre_1929", "yb_1946_to_1960", "yb_post_1961", "crawl", "slab",
  "ln_garagespaces", "fireplace", "hot_water", "elec_baseboard", "wall", "central_air",
  ecfs_col, "near_downtown_1mi", "near_downtown_5mi"
)

formula_ln_tasp_g1 <- reformulate(tasprhs_g1, response = "ln_tasp_re")

tasprhsnc_g1 <- c(
  "ln_landratio", "ln_base_bldgsize_ratio", "arterial",
  "single_family1story", "half_duplex1story", "half_duplex2story",
  "smallest", "small", "large", "largest", "two_plus_baths", "powder_room",
  "qabove_avg", "qbelow_avg",
  "yb_pre_1929", "yb_1946_to_1960", "yb_post_1961", "crawl", "slab",
  "ln_garagespaces", "fireplace", "hot_water", "elec_baseboard", "wall", "central_air",
  ecfs_col, "near_downtown_1mi", "near_downtown_5mi"
)

formula_ln_tasp_nc_g1 <- reformulate(tasprhsnc_g1, response = "ln_tasp_re")

altrhs_g1 <- c(
  "ln_landratio", "lnsbldsfint", "arterial",
  "single_family1story", "half_duplex1story", "half_duplex2story",
  "smallest", "small", "large", "largest", "two_plus_baths", "powder_room",
  "good", "fair", "poor", "very_poor_unsound", "qabove_avg", "qbelow_avg",
  "yb_pre_1929", "yb_1946_to_1960", "yb_post_1961", "crawl", "slab", "garage", "fireplace",
  "hot_water", "elec_baseboard", "wall", "central_air", ecfs_col, "near_downtown_1mi", "near_downtown_5mi"
)

formula_alt_g1 <- reformulate(altrhs_g1, response = "ln_tasp_re")

altrhsnc_g1 <- c(
  "ln_landratio", "lnsbldsfint", "arterial",
  "single_family1story", "half_duplex1story", "half_duplex2story",
  "smallest", "small", "large", "largest", "two_plus_baths", "powder_room",
  "qabove_avg", "qbelow_avg",
  "yb_pre_1929", "yb_1946_to_1960", "yb_post_1961", "crawl", "slab", "garage", "fireplace",
  "hot_water", "elec_baseboard", "wall", "central_air", ecfs_col, "near_downtown_1mi", "near_downtown_5mi"
)

formula_alt_nc_g1 <- reformulate(altrhsnc_g1, response = "ln_tasp_re")

# SET FOUR

formula_time_2 <- ln_sppsf ~ arterial + single_family1story + half_duplex1story + half_duplex2story +
  smallest + small + large + largest + two_plus_baths + powder_room + totalbath +
  good + fair + poor + very_poor_unsound + cavg + qabove_avg + qbelow_avg + qavg +
  yb_pre_1929 + yb_1946_to_1960 + yb_post_1961 + crawl + slab + ln_garagespaces + fireplace +
  hot_water + elec_baseboard + wall + central_air + months_1to9 + months_10to16 + months_17to24

formula_ln_price_no_ecf_2 <- ln_price ~ ln_landratio + ln_base_bldgsize_ratio + arterial +
  single_family1story + half_duplex1story + half_duplex2story +
  smallest + small + large + largest + two_plus_baths + powder_room +
  good + fair + poor + very_poor_unsound + qabove_avg + qbelow_avg +
  yb_pre_1929 + yb_1946_to_1960 + yb_post_1961 + crawl + slab + ln_garagespaces + fireplace +
  hot_water + elec_baseboard + wall + central_air +
  months_1to9 + months_10to16 + months_17to24 + qavg + totalbath + cavg

baserhs_2 <- c(
  "ln_landratio", "ln_base_bldgsize_ratio", "arterial",
  "single_family1story", "half_duplex1story", "half_duplex2story",
  "smallest", "small", "large", "largest", "two_plus_baths", "powder_room",
  "good", "fair", "poor", "very_poor_unsound", "qabove_avg", "qbelow_avg",
  "yb_pre_1929", "yb_1946_to_1960", "yb_post_1961", "crawl", "slab",
  "ln_garagespaces", "fireplace", "hot_water", "elec_baseboard", "wall", "central_air",
  ecfs_col,  # <-- splices *all* secf_ dummy columns in correctly
  "months_1to9", "months_10to16", "months_17to24", "qavg", "totalbath", "cavg"
)

formula_base_2 <- reformulate(baserhs_2, response = "ln_price")

tasprhs_2 <- c(
  "ln_landratio", "ln_base_bldgsize_ratio", "arterial",
  "single_family1story", "half_duplex1story", "half_duplex2story",
  "smallest", "small", "large", "largest", "two_plus_baths", "powder_room",
  "good", "fair", "poor", "very_poor_unsound", "qabove_avg", "qbelow_avg",
  "yb_pre_1929", "yb_1946_to_1960", "yb_post_1961", "crawl", "slab",
  "ln_garagespaces", "fireplace", "hot_water", "elec_baseboard", "wall", "central_air", ecfs_col,
  "qavg", "totalbath", "cavg"
)

formula_ln_tasp_2 <- reformulate(tasprhs_2, response = "ln_tasp_re")

altrhs_2 <- c(
  "ln_landratio", "lnsbldsfint", "arterial",
  "single_family1story", "half_duplex1story", "half_duplex2story",
  "smallest", "small", "large", "largest", "two_plus_baths", "powder_room",
  "good", "fair", "poor", "very_poor_unsound", "qabove_avg", "qbelow_avg",
  "yb_pre_1929", "yb_1946_to_1960", "yb_post_1961", "crawl", "slab", "garage", "fireplace",
  "hot_water", "elec_baseboard", "wall", "central_air", ecfs_col, "qavg", "totalbath", "cavg"
)

formula_alt_2 <- reformulate(altrhs_2, response = "ln_tasp_re")

formula_time_g2 <- ln_sppsf ~ arterial + single_family1story + half_duplex1story + half_duplex2story +
  smallest + small + large + largest + two_plus_baths + powder_room + totalbath +
  good + fair + poor + very_poor_unsound + cavg + qabove_avg + qbelow_avg + qavg +
  yb_pre_1929 + yb_1946_to_1960 + yb_post_1961 + crawl + slab + ln_garagespaces + fireplace +
  hot_water + elec_baseboard + wall + central_air +
  months_1to9 + months_10to16 + months_17to24 + near_downtown_1mi + near_downtown_5mi +
  near_downtown_10mi

formula_ln_price_no_ecf_g2 <- ln_price ~ ln_landratio + ln_base_bldgsize_ratio + arterial +
  single_family1story + half_duplex1story + half_duplex2story +
  smallest + small + large + largest + two_plus_baths + powder_room + totalbath +
  good + fair + poor + very_poor_unsound + cavg + qabove_avg + qbelow_avg + qavg +
  yb_pre_1929 + yb_1946_to_1960 + yb_post_1961 + crawl + slab + ln_garagespaces + fireplace +
  hot_water + elec_baseboard + wall + central_air +
  months_1to9 + months_10to16 + months_17to24 + near_downtown_1mi + near_downtown_5mi +
  near_downtown_10mi

baserhs_g2 <- c(
  "ln_landratio", "ln_base_bldgsize_ratio", "arterial",
  "single_family1story", "half_duplex1story", "half_duplex2story",
  "smallest", "small", "large", "largest", "two_plus_baths", "powder_room", "totalbath",
  "good", "fair", "poor", "very_poor_unsound", "cavg", "qabove_avg", "qbelow_avg", "qavg",
  "yb_pre_1929", "yb_1946_to_1960", "yb_post_1961", "crawl", "slab",
  "ln_garagespaces", "fireplace", "hot_water", "elec_baseboard", "wall", "central_air",
  ecfs_col,  # <-- splices *all* secf_ dummy columns in correctly
  "months_1to9", "months_10to16", "months_17to24", "near_downtown_1mi", "near_downtown_5mi",
  "near_downtown_10mi"
)

formula_base_g2 <- reformulate(baserhs_g2, response = "ln_price")

tasprhs_g2 <- c(
  "ln_landratio", "ln_base_bldgsize_ratio", "arterial",
  "single_family1story", "half_duplex1story", "half_duplex2story",
  "smallest", "small", "large", "largest", "two_plus_baths", "powder_room", "totalbath",
  "good", "fair", "poor", "very_poor_unsound", "cavg", "qabove_avg", "qbelow_avg", "qavg",
  "yb_pre_1929", "yb_1946_to_1960", "yb_post_1961", "crawl", "slab",
  "ln_garagespaces", "fireplace", "hot_water", "elec_baseboard", "wall", "central_air",
  ecfs_col, "near_downtown_1mi", "near_downtown_5mi", "near_downtown_10mi"
)

formula_ln_tasp_g2 <- reformulate(tasprhs_g2, response = "ln_tasp_re")

altrhs_g2 <- c(
  "ln_landratio", "lnsbldsfint", "arterial",
  "single_family1story", "half_duplex1story", "half_duplex2story",
  "smallest", "small", "large", "largest", "two_plus_baths", "powder_room",
  "good", "fair", "poor", "very_poor_unsound", "qabove_avg", "qbelow_avg",
  "yb_pre_1929", "yb_1946_to_1960", "yb_post_1961", "crawl", "slab", "garage", "fireplace",
  "hot_water", "elec_baseboard", "wall", "central_air", ecfs_col, "near_downtown_1mi",
  "near_downtown_5mi", "near_downtown_10mi", "qavg", "totalbath", "cavg"
)

formula_alt_g2 <- reformulate(altrhs_g2, response = "ln_tasp_re")


# SET FIVE



# MAKE LIST

codform <- list(
  formula_time = formula_time,
  formula_time_nc = formula_time_nc,
  formula_ln_price_no_ecf = formula_ln_price_no_ecf,
  formula_ln_price_no_ecf_nc = formula_ln_price_no_ecf_nc,
  baserhs = baserhs,
  formula_base = formula_base,
  formula_base_nc = formula_base_nc,
  tasprhs = tasprhs,
  formula_ln_tasp = formula_ln_tasp,
  formula_ln_tasp_nc = formula_ln_tasp_nc,
  altrhs = altrhs,
  formula_alt = formula_alt,
  formula_alt_nc = formula_alt_nc,
  formula_time_nlm = formula_time_nlm,
  formula_ln_price_no_ecf_nlm = formula_ln_price_no_ecf_nlm,
  formula_base_nlm = formula_base_nlm,
  formula_ln_tasp_nlm = formula_ln_tasp_nlm,
  formula_time_g1 = formula_time_g1,
  formula_ln_price_no_ecf_g1 = formula_ln_price_no_ecf_g1,
  formula_base_g1 = formula_base_g1,
  formula_ln_tasp_g1 = formula_ln_tasp_g1,
  formula_alt_g1 = formula_alt_g1,
  formula_time_2 = formula_time_2,
  formula_time_g2 = formula_time_g2,
  formula_ln_price_no_ecf_2 = formula_ln_price_no_ecf_2,
  formula_ln_price_no_ecf_g2 = formula_ln_price_no_ecf_g2
)

# You can then save it as an RDS file just like in your example:
saveRDS(codform, file = here("rmds", "codformulas.rds"))






