# General Model Functions

backward_lm <- function(formula, data, p_out = 0.10, verbose = TRUE, keep_steps = TRUE, ...) {
  
  # Fit initial model (passes through na.action, subset, weights, etc via ...)
  mod <- lm(formula, data = data, ...)
  
  # Store a helpful provenance tag for later manifests/bundles
  attr(mod, "data_name") <- deparse(substitute(data))
  
  steps <- list()
  
  repeat {
    tt <- terms(mod)    # If only intercept remains, stop
    terms_now <- attr(tt, "term.labels")
    if (length(terms_now) == 0) break
    
    dr <- drop1(mod, test = "F") # Term-level p-values for dropping each *term*
    
    # drop1 table has rownames like "<none>", then each term  # p-value column is usually "Pr(>F)"
    pcol <- "Pr(>F)"
    if (!pcol %in% names(dr)) { # Defensive fallback (shouldn't happen for lm + test="F")
      if (verbose) message("drop1() did not return Pr(>F); stopping.")
      break
    }
    
    dr_terms <- setdiff(rownames(dr), "<none>")
    pvals <- dr[dr_terms, pcol, drop = TRUE]
    
    if (length(pvals) == 0 || all(!is.finite(pvals))) break # If all NA / non-finite, stop
    
    worst_p <- max(pvals, na.rm = TRUE)
    if (!is.finite(worst_p) || worst_p <= p_out) break
    
    worst_term <- names(which.max(pvals))
    
    if (verbose) message(sprintf("Dropping %-30s (p=%.4g)", worst_term, worst_p))
    
    if (keep_steps) {
      steps[[length(steps) + 1]] <- data.frame(
        step = length(steps) + 1, dropped = worst_term, p_value = worst_p,
        formula = paste(deparse(formula(mod)), collapse = " "), stringsAsFactors = FALSE
      )
    }
    
    # Update model: preserves response exactly (including log(), etc.)
    mod <- update(mod, paste(". ~ . -", worst_term))
    attr(mod, "data_name") <- deparse(substitute(data))
  }
  
  if (keep_steps) {
    steps_df <- if (length(steps)) do.call(rbind, steps) else
      data.frame(step = integer(0), dropped = character(0), p_value = numeric(0), formula = character(0))
    attr(mod, "backward_steps") <- steps_df
  }
  mod
}

# __________

prep_cod_sales <- function(
    df,    # ---- core column names (override if needed) ----
    saledate_col = "saledate", saleprice_col = "saleprice", parcelno_col = "parcelno",
    ecf_col      = "secf", avwhensold_col = "avwhensold",         # your file renames secf -> ecf
    # ---- key housing columns used in cdataprep1.r ----
    style_col = "sresb_style",  styhgt_col = "sresb_styhgt", yearbuilt_col = "sresb_yearbuilt",
    cond_col       = "scond",  floorarea_col  = "sresb_floorarea",  totalsqft_col  = "stotalsqft",
    fullbaths_col  = "sresb_fullbaths",  halfbaths_col  = "sresb_halfbaths",
    fireplace_col  = "sresb_fireplace",  groundarea_col   = "sresb_groundarea",
    crawspace_col    = "sresb_crawspace",  slabarea_col     = "sresb_slabarea",
    basementarea_col = "sresb_basementarea",  bldgclass_col  = "sresb_bldgclass",
    garagearea_col = "sresb_garagearea",
    gartype_col    = NULL,            # auto-detects sresb_gartype or sreb_gartype if NULL
    rdclass_col    = "rd_class",    heat_col = "sresb_heat",
    # ---- behavior options ----
    style_keep = c("SINGLE FAMILY", "1/2 DUPLEX"),  min_saleprice = 10000,
    min_totalsqft = 400,    min_floorarea = 400,
    # months origin: if NULL uses min(saledate) floored to month
    origin_month = NULL,
    # ECF dummy options (to match your script chunk)
    make_ecf_dummies = TRUE,  remove_first_dummy = TRUE,  keep_ecf_original  = TRUE,
    # if you want log(difference) terms to be 0 instead of NA when <=0
    # (NA is closer to your SPSS script; 0 is friendlier for prediction)
    logdiff_nonpos = c("na", "zero"),
    verbose = TRUE
) {
  logdiff_nonpos <- match.arg(logdiff_nonpos)
  req_pkgs <- c("dplyr", "lubridate")
  missing_pkgs <- req_pkgs[!vapply(req_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs)) stop("Missing packages: ", paste(missing_pkgs, collapse = ", "))
  has_stringr <- requireNamespace("stringr", quietly = TRUE)
  has_fastd   <- requireNamespace("fastDummies", quietly = TRUE)
  msg <- function(...) if (isTRUE(verbose)) message(...)
  
  df <- dplyr::as_tibble(df)
  names(df) <- tolower(names(df))  # lower-case incoming column names
  saledate_col   <- tolower(saledate_col)
  saleprice_col  <- tolower(saleprice_col)
  parcelno_col   <- tolower(parcelno_col)
  ecf_col        <- tolower(ecf_col)
  avwhensold_col <- tolower(avwhensold_col)
  style_col      <- tolower(style_col)
  styhgt_col     <- tolower(styhgt_col)
  yearbuilt_col  <- tolower(yearbuilt_col)
  cond_col       <- tolower(cond_col)
  floorarea_col  <- tolower(floorarea_col)
  totalsqft_col  <- tolower(totalsqft_col)
  fullbaths_col  <- tolower(fullbaths_col)
  halfbaths_col  <- tolower(halfbaths_col)
  fireplace_col  <- tolower(fireplace_col)
  groundarea_col   <- tolower(groundarea_col)
  crawspace_col    <- tolower(crawspace_col)
  slabarea_col     <- tolower(slabarea_col)
  basementarea_col <- tolower(basementarea_col)
  bldgclass_col  <- tolower(bldgclass_col)
  garagearea_col <- tolower(garagearea_col)
  rdclass_col    <- tolower(rdclass_col)
  heat_col       <- tolower(heat_col)
  
  # --- basic existence checks (soft for some blocks) ---
  must_have <- c(saledate_col, saleprice_col, parcelno_col, style_col, floorarea_col, totalsqft_col)
  missing_cols <- setdiff(must_have, names(df))
  if (length(missing_cols)) stop("Input df is missing required columns: ", paste(missing_cols, collapse = ", "))
  
  # --- parse saledate robustly (supports ISO "....Z") ---
  sd <- df[[saledate_col]]
  if (inherits(sd, "Date")) {sd_date <- sd} else if (inherits(sd, "POSIXt")) {
    sd_date <- as.Date(sd)} else {
    sd1 <- suppressWarnings(lubridate::ymd_hms(sd, tz = "UTC", quiet = TRUE))
    if (all(is.na(sd1))) {
      sd2 <- suppressWarnings(lubridate::ymd(sd, quiet = TRUE))
      sd_date <- sd2
    } else {sd_date <- as.Date(sd1)}
  }
  df <- df |>
    mutate(saledate_parsed = sd_date, sale_month = lubridate::floor_date(saledate_parsed, "month"))
  
  # --- PRICE CLASS (5 buckets) ---
  df <- df |>
    mutate(priceclass = dplyr::case_when(
        .data[[saleprice_col]] <  48000 ~ 1L,      .data[[saleprice_col]] <  67000 ~ 2L,
        .data[[saleprice_col]] <  94000 ~ 3L,      .data[[saleprice_col]] < 134000 ~ 4L,
        is.finite(.data[[saleprice_col]]) ~ 5L,    TRUE ~ NA_integer_
      ),
      priceclass = factor(priceclass, levels = 1:5,
                          labels = c("Below 48000", "48000 to 67000", "67000 to 94000",
                                     "94000 to 134000", "134000+"))
    )
  
  # --- Filter by style ---
  df <- df |>
    mutate(in_style = dplyr::if_else(.data[[style_col]] %in% style_keep, 1L, 0L)) |>
    filter(in_style == 1L)
  
  # --- Deduplicate: keep last record within parcelno + saleprice + saledate ---
  df <- df |>
    mutate(parcelno = as.character(.data[[parcelno_col]]),
      parcelno_std = {
        x <- parcelno
        x <- gsub("\\.", "P", x)
        x <- gsub("-", "D", x)
        x
      }
    ) |>
    dplyr::arrange(parcelno_std, .data[[saleprice_col]], .data[[saledate_col]]) |>
    dplyr::group_by(parcelno_std, .data[[saleprice_col]], .data[[saledate_col]]) |>
    dplyr::slice_tail(n = 1) |>  dplyr::ungroup()
  
  # --- Remove price equals assessment transactions (if columns exist) ---
  if (avwhensold_col %in% names(df)) {
    df <- df |>
      dplyr::mutate(
        priceisasmt_ratio = dplyr::if_else(.data[[saleprice_col]] > 0,
                                           .data[[avwhensold_col]] / .data[[saleprice_col]],
                                           NA_real_),
        priceisasmt_ratio2 = dplyr::if_else(priceisasmt_ratio >= 0.90 & priceisasmt_ratio <= 1.10,
                                            1, priceisasmt_ratio),
        priceisasmt_ratio = priceisasmt_ratio2,
        priceisasmt = dplyr::if_else(priceisasmt_ratio == 1, 0L, 1L)
      ) |>  dplyr::filter(priceisasmt == 1L)
  } else {msg("Note: '", avwhensold_col, "' not found; skipping price==assessment filter.")}
  
  # --- OUT filters for bad data (treat missing core values as bad) ---
  df <- df |>
    dplyr::mutate(
      out_bad = dplyr::case_when(
        is.na(.data[[totalsqft_col]]) | is.na(.data[[floorarea_col]]) | is.na(.data[[saleprice_col]]) | is.na(saledate_parsed) ~ 1L,
        .data[[totalsqft_col]] < min_totalsqft ~ 1L,  .data[[floorarea_col]] < min_floorarea ~ 1L,
        .data[[saleprice_col]] < min_saleprice ~ 1L,  TRUE ~ 0L
      )
    ) |> dplyr::filter(out_bad == 0L)
  
  # --- SPPSF + log ---
  df <- df |>
    mutate(sppsf = .data[[saleprice_col]] / .data[[floorarea_col]],
      ln_sppsf = dplyr::if_else(is.finite(sppsf) & sppsf > 0, log(sppsf), NA_real_)
    )
  
  # --- Story height bins + style/story interactions ---
  if (styhgt_col %in% names(df)) {
    df <- df |>
      dplyr::mutate(
        onestory = dplyr::if_else(.data[[styhgt_col]] <= 2, 1L, 0L, missing = 0L),
        twostory = dplyr::if_else(.data[[styhgt_col]] > 2 & .data[[styhgt_col]] <= 5, 1L, 0L, missing = 0L),
        single_family1story = dplyr::if_else(.data[[style_col]] == "SINGLE FAMILY" & onestory == 1L, 1L, 0L),
        half_duplex1story   = dplyr::if_else(.data[[style_col]] == "1/2 DUPLEX" & onestory == 1L, 1L, 0L),
        half_duplex2story   = dplyr::if_else(.data[[style_col]] == "1/2 DUPLEX" & twostory == 1L, 1L, 0L)
      )
  }
  
  # --- Era built buckets + binaries ---
  if (yearbuilt_col %in% names(df)) {
    df <- df |>
      dplyr::mutate(
        erabuilt = dplyr::case_when(
          .data[[yearbuilt_col]] < 1910 ~ 1L,          .data[[yearbuilt_col]] < 1930 ~ 2L,
          .data[[yearbuilt_col]] < 1946 ~ 3L,          .data[[yearbuilt_col]] < 1961 ~ 4L,
          .data[[yearbuilt_col]] <= 1990 ~ 5L,         .data[[yearbuilt_col]] > 1990 ~ 6L,
          TRUE ~ NA_integer_
        ),
        yb_pre_1929     = dplyr::if_else(erabuilt %in% c(1, 2), 1L, 0L, missing = 0L),
        yb_1946_to_1960 = dplyr::if_else(erabuilt == 4, 1L, 0L, missing = 0L),
        yb_post_1961    = dplyr::if_else(erabuilt %in% c(5, 6), 1L, 0L, missing = 0L),
        yb_post_1990 = dplyr::if_else(erabuilt == 6, 1L, 0L, missing = 0L)
      )
  }
  
  # --- Condition binaries (SCOND 3 base; missing treated as base) ---
  if (cond_col %in% names(df)) {
    df <- df |>
      dplyr::mutate(
        excellent = dplyr::if_else(.data[[cond_col]] == 0, 1L, 0L, missing = 0L),
        very_good = dplyr::if_else(.data[[cond_col]] == 1, 1L, 0L, missing = 0L),
        good = dplyr::if_else(.data[[cond_col]] == 2, 1L, 0L, missing = 0L),
        cavg = dplyr::if_else(.data[[cond_col]] == 3, 1L, 0L, missing = 0L),
        fair = dplyr::if_else(.data[[cond_col]] == 4, 1L, 0L, missing = 0L),
        poor = dplyr::if_else(.data[[cond_col]] == 5, 1L, 0L, missing = 0L),
        very_poor = dplyr::if_else(.data[[cond_col]] == 6, 1L, 0L, missing = 0L),
        unsound = dplyr::if_else(.data[[cond_col]] == 7, 1L, 0L, missing = 0L),
        very_poor_unsound = dplyr::if_else(.data[[cond_col]] %in% c(6, 7), 1L, 0L, missing = 0L)
      )
  }
  
  # --- Bathrooms binaries (one bath base) ---
  if (fullbaths_col %in% names(df) && halfbaths_col %in% names(df)) {
    df <- df |>
      dplyr::mutate(
        totalbath = dplyr::if_else(
          is.na(.data[[fullbaths_col]]) & is.na(.data[[halfbaths_col]]), NA_real_,
          as.numeric(.data[[fullbaths_col]]) + 0.5 * as.numeric(.data[[halfbaths_col]])
        ),
        two_plus_baths = dplyr::if_else(.data[[fullbaths_col]] %in% 2:10, 1L, 0L, missing = 0L),
        powder_room    = dplyr::if_else(.data[[halfbaths_col]] %in% 1:5, 1L, 0L, missing = 0L)
      )
  }
  
  # --- Fireplace ---
  if (fireplace_col %in% names(df)) {
    df <- df |> mutate(fireplace = dplyr::if_else(.data[[fireplace_col]] %in% 1:6, 1L, 0L, missing = 0L))}
  
  # --- Basement/crawl/slab (guard division) ---
  safe_ratio <- function(num, den) {
    out <- rep(NA_real_, length(num))
    ok <- is.finite(num) & is.finite(den) & den > 0
    out[ok] <- num[ok] / den[ok]
    out
  }
  if (all(c(groundarea_col, crawspace_col, slabarea_col, basementarea_col) %in% names(df))) {
    ga <- suppressWarnings(as.numeric(df[[groundarea_col]]))
    cs <- suppressWarnings(as.numeric(df[[crawspace_col]]))
    sa <- suppressWarnings(as.numeric(df[[slabarea_col]]))
    ba <- suppressWarnings(as.numeric(df[[basementarea_col]]))
    pctcrawl <- safe_ratio(cs, ga)
    pctslab  <- safe_ratio(sa, ga)
    pctbsmnt <- safe_ratio(ba, ga)
    
    df <- df |>
      dplyr::mutate(
        pctcrawl = pctcrawl,        pctslab  = pctslab,        pctbsmnt = pctbsmnt,
        crawl = dplyr::if_else(is.finite(pctcrawl) & is.finite(pctbsmnt) & is.finite(pctslab) &
                                 (pctcrawl > pctbsmnt) & (pctcrawl > pctslab), 1L, 0L, missing = 0L),
        slab  = dplyr::if_else(is.finite(pctslab) & is.finite(pctcrawl) & is.finite(pctbsmnt) &
                                 (pctslab > pctcrawl) & (pctslab > pctbsmnt), 1L, 0L, missing = 0L)
      )
  }
  
  # --- Quality grade binaries (2 base) ---
  if (bldgclass_col %in% names(df)) {
    df <- df |>
      dplyr::mutate(
        qabove_avg = dplyr::if_else(.data[[bldgclass_col]] %in% c(3, 4, 5), 1L, 0L, missing = 0L),
        qbelow_avg = dplyr::if_else(.data[[bldgclass_col]] %in% c(0, 1), 1L, 0L, missing = 0L),
        qbest = dplyr::if_else(.data[[bldgclass_col]] == 5, 1L, 0L, missing = 0L),
        qavg = dplyr::if_else(.data[[bldgclass_col]] == 2, 1L, 0L, missing = 0L),
        qpoor = dplyr::if_else(.data[[bldgclass_col]] == 0, 1L, 0L, missing = 0L)
      )
  }
  
  # --- Garages (presence, type, spaces, log) ---
  if (garagearea_col %in% names(df)) {
    # auto-detect gartype col if not provided
    if (is.null(gartype_col)) {
      if ("sresb_gartype" %in% names(df)) gartype_col <- "sresb_gartype"
      if ("sreb_gartype"  %in% names(df)) gartype_col <- "sreb_gartype"
    } else {gartype_col <- tolower(gartype_col)}
    
    df <- df |>
      dplyr::mutate(
        garage = dplyr::if_else(.data[[garagearea_col]] >= 200, 1L, 0L, missing = 0L),
        garagespaces = dplyr::case_when(
          .data[[garagearea_col]] > 200 & .data[[garagearea_col]] < 420 ~ 1L,
          .data[[garagearea_col]] > 419 & .data[[garagearea_col]] < 600 ~ 2L,
          .data[[garagearea_col]] > 599 & .data[[garagearea_col]] < 820 ~ 3L,
          .data[[garagearea_col]] > 819 ~ 4L,          TRUE ~ 0L
        ),
        # IMPORTANT FIX: log1p avoids NA/-Inf at 0
        ln_garagespaces = log1p(garagespaces)
      )
    
    if (!is.null(gartype_col) && gartype_col %in% names(df)) {
      df <- df |>
        dplyr::mutate(
          one_car_garage = dplyr::if_else(.data[[gartype_col]] == 1, 1L, 0L, missing = 0L),
          two_car_garage = dplyr::if_else(.data[[gartype_col]] == 2, 1L, 0L, missing = 0L)
        )
    }
  }
  # --- Street class: ARTERIAL vs others ---
  if (rdclass_col %in% names(df)) {
    df <- df |>
      dplyr::mutate(arterial = dplyr::if_else(.data[[rdclass_col]] %in% c("A21", "A31"), 1L, 0L, missing = 0L))
  }
  # --- Heat/cooling binaries (forced air base) ---
  if (heat_col %in% names(df)) {
    df <- df |>
      dplyr::mutate(
        hot_water      = dplyr::if_else(.data[[heat_col]] == 2, 1L, 0L, missing = 0L),
        elec_baseboard = dplyr::if_else(.data[[heat_col]] == 3, 1L, 0L, missing = 0L),
        wall           = dplyr::if_else(.data[[heat_col]] %in% c(6, 8), 1L, 0L, missing = 0L),
        central_air    = dplyr::if_else(.data[[heat_col]] == 9, 1L, 0L, missing = 0L)
      )
  }
  
  # --- Size categories from floorarea + binaries (3 base) ---
  df <- df |>
    dplyr::mutate(
      size = dplyr::case_when(
        .data[[floorarea_col]] <  830 ~ 1L,        .data[[floorarea_col]] < 1018 ~ 2L,
        .data[[floorarea_col]] < 1255 ~ 3L,        .data[[floorarea_col]] < 1632 ~ 4L,
        is.finite(.data[[floorarea_col]]) ~ 5L,        TRUE ~ NA_integer_
      ),
      smallest = dplyr::if_else(size == 1, 1L, 0L, missing = 0L),
      small    = dplyr::if_else(size == 2, 1L, 0L, missing = 0L),
      large    = dplyr::if_else(size == 4, 1L, 0L, missing = 0L),
      largest  = dplyr::if_else(size == 5, 1L, 0L, missing = 0L)
    )
  
  # --- Time variables: smonth, syear, months (robust origin) ---
  if (is.null(origin_month)) {origin_month <- min(df$sale_month, na.rm = TRUE)
  } else {origin_month <- lubridate::floor_date(as.Date(origin_month), "month")}
  
  df <- df |>
    dplyr::mutate(
      smonth = lubridate::month(saledate_parsed),     syear  = lubridate::year(saledate_parsed),
      months = 12L * (lubridate::year(sale_month) - lubridate::year(origin_month)) +
        (lubridate::month(sale_month) - lubridate::month(origin_month)),
      months = pmax(as.integer(months), 0L),           months_1to9   = pmin(months, 9L),
      months_10to16 = pmin(pmax(months - 9L, 0L), 7L), months_17to24 = pmax(months - 16L, 0L),
      months_10to18 = pmin(pmax(months - 9L, 0L), 9L), months_16to24 = pmax(months - 15L, 0L),
      months_18to24 = pmax(months - 17L, 0L),          months_1to8 = pmin(months, 8L),
      months_19to24 = pmax(months - 18L, 0L),          months_15to24 = pmax(months - 14L, 0L)
    )
  
  # --- BUILDING SIZE + LOT SIZE RATIO VARIABLES (medians by style) ---
  df <- df |>
    dplyr::mutate(
      base_bldgsize = dplyr::case_when(
        .data[[style_col]] == "SINGLE FAMILY" ~ 1056, .data[[style_col]] == "1/2 DUPLEX"   ~ 840,
        TRUE ~ NA_real_
      ),
      base_bldgsize_ratio = .data[[floorarea_col]] / base_bldgsize,
      ln_base_bldgsize_ratio = dplyr::if_else(is.finite(base_bldgsize_ratio) & base_bldgsize_ratio > 0,
                                              log(base_bldgsize_ratio), NA_real_),
      ln_sbldsf_bsz_single     = dplyr::if_else(.data[[style_col]] == "SINGLE FAMILY", ln_base_bldgsize_ratio, 0),
      ln_sbldsf_bsz_halfduplex = dplyr::if_else(.data[[style_col]] == "1/2 DUPLEX",   ln_base_bldgsize_ratio, 0),
      
      base_lotsize = dplyr::case_when(
        .data[[style_col]] == "SINGLE FAMILY" ~ 4599, .data[[style_col]] == "1/2 DUPLEX"   ~ 3180,
        TRUE ~ NA_real_
      ),
      base_lotsize_ratio = .data[[totalsqft_col]] / base_lotsize,
      ln_base_lotsize_ratio = dplyr::if_else(is.finite(base_lotsize_ratio) & base_lotsize_ratio > 0,
                                             log(base_lotsize_ratio), NA_real_),
      landratio  = .data[[totalsqft_col]] / base_lotsize,      landratio2 = landratio,
      landratio2 = dplyr::if_else(landratio2 >= 0.90 & landratio2 <= 1.10, 1, landratio2),
      landratio2 = dplyr::if_else(.data[[style_col]] != "SINGLE FAMILY" & landratio2 > 3, 3, landratio2),
      landratio2 = dplyr::if_else(.data[[style_col]] == "SINGLE FAMILY" & landratio2 > 4, 4, landratio2),
      ln_landratio = dplyr::if_else(is.finite(landratio2) & landratio2 > 0, log(landratio2), NA_real_)
    )
  
  # --- Alternative building size transforms (as in SPSS notes) ---
  fill_logdiff <- function(x) {
    if (logdiff_nonpos == "zero") dplyr::if_else(x > 0, log(x), 0) else dplyr::if_else(x > 0, log(x), NA_real_)
  }
  
  df <- df |>
    dplyr::mutate(
      lnsbldsf    = dplyr::if_else(is.finite(.data[[floorarea_col]]) & .data[[floorarea_col]] > 0, log(.data[[floorarea_col]]), NA_real_),
      sbldsfint   = (.data[[floorarea_col]] - 432),      lnsbldsfint = fill_logdiff(sbldsfint),
      sblds_fmed  = (.data[[floorarea_col]] - 1051),     lnsbldsfmed = fill_logdiff(sblds_fmed)
    )
  
  # --- Rename secf -> ecf and ln_price (as in your datamod1 chunk) ---
  if (ecf_col %in% names(df) && !"ecf" %in% names(df)) {df <- df |> dplyr::rename(ecf = !!ecf_col)}
  if ("ecf" %in% names(df)) {
    # light normalization (avoids whitespace-induced new levels later)
    if (has_stringr) {
      df <- df |> dplyr::mutate(ecf = stringr::str_squish(toupper(as.character(ecf))))
    } else {df <- df |> dplyr::mutate(ecf = toupper(trimws(as.character(ecf))))}
  }
  
  df <- df |>
    mutate(ln_price = dplyr::if_else(.data[[saleprice_col]] > 0, log(.data[[saleprice_col]]), NA_real_))
  
  df$citycd <- as.character(df$ccd)
  
  # --- ECF dummies (optional; matches your script pattern) ---
  ecfs_col <- character(0)
  if (isTRUE(make_ecf_dummies)) {
    if (!has_fastd) stop("make_ecf_dummies=TRUE requires fastDummies package.")
    if (!("ecf" %in% names(df))) stop("ecf column not found after rename; cannot dummy.")
    
    df <- fastDummies::dummy_cols(
      df, select_columns = "ecf", remove_first_dummy = remove_first_dummy,
      remove_selected_columns = !keep_ecf_original
    )
    ecfs_col <- grep("^ecf_", names(df), value = TRUE)
  }
  
  attr(df, "origin_month") <- origin_month
  attr(df, "ecfs_col") <- ecfs_col
  
  list(
    data = df, ecfs_col = ecfs_col, origin_month = origin_month
  )
}

### USAGE

#res <- prep_cod_sales(dfa, origin_month = as.Date("2023-04-01"))
#df_prepped <- res$data
#ecfs_col   <- res$ecfs_col


# __________

prep_cod_sales3 <- function(df,
                           saledate_col   = "saledate",
                           saleprice_col  = "saleprice",
                           parcelno_col   = "parcelno",
                           style_col      = "sresb_style",
                           floorarea_col  = "sresb_floorarea",
                           totalsqft_col  = "stotalsqft",
                           avwhensold_col = "avwhensold",
                           ecf_col        = "secf",        # will rename secf -> ecf
                           origin_month   = NULL,          # e.g. as.Date("2023-04-01")
                           style_keep     = c("SINGLE FAMILY", "1/2 DUPLEX"),
                           min_saleprice  = 10000,
                           min_totalsqft  = 400,
                           min_floorarea  = 400,
                           make_ecf_dummies = TRUE,
                           remove_first_dummy = TRUE,
                           keep_ecf_original  = TRUE,
                           verbose = TRUE) {
  
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Need package: dplyr")
  if (!requireNamespace("lubridate", quietly = TRUE)) stop("Need package: lubridate")
  
  has_fastd <- requireNamespace("fastDummies", quietly = TRUE)
  has_readr <- requireNamespace("readr", quietly = TRUE)
  
  msg <- function(...) if (isTRUE(verbose)) message(...)
  
  df <- dplyr::as_tibble(df)
  names(df) <- tolower(names(df))
  
  saledate_col   <- tolower(saledate_col)
  saleprice_col  <- tolower(saleprice_col)
  parcelno_col   <- tolower(parcelno_col)
  style_col      <- tolower(style_col)
  floorarea_col  <- tolower(floorarea_col)
  totalsqft_col  <- tolower(totalsqft_col)
  avwhensold_col <- tolower(avwhensold_col)
  ecf_col        <- tolower(ecf_col)
  
  must_have <- c(saledate_col, saleprice_col, parcelno_col, style_col, floorarea_col, totalsqft_col)
  missing_cols <- setdiff(must_have, names(df))
  if (length(missing_cols)) stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  
  # ---- robust numeric parsing (handles commas/$ if present) ----
  to_num <- function(x) {
    if (is.numeric(x)) return(x)
    if (has_readr) return(readr::parse_number(as.character(x)))
    suppressWarnings(as.numeric(gsub("[^0-9.\\-]+", "", as.character(x))))
  }
  
  df[[saleprice_col]] <- to_num(df[[saleprice_col]])
  df[[floorarea_col]] <- to_num(df[[floorarea_col]])
  df[[totalsqft_col]] <- to_num(df[[totalsqft_col]])
  if (avwhensold_col %in% names(df)) df[[avwhensold_col]] <- to_num(df[[avwhensold_col]])
  
  # ---- robust saledate parsing: ISO + ymd + US mdy (+ time variants) ----
  sd <- df[[saledate_col]]
  
  # Robust saledate parsing (returns Date vector always)
  sd <- df[[saledate_col]]
  
  if (inherits(sd, "Date")) {
    df$saledate_parsed <- sd
  } else if (inherits(sd, "POSIXt")) {
    df$saledate_parsed <- as.Date(sd)
  } else if (is.numeric(sd)) {
    # Excel-style dates (only if your file ever stores dates as numbers)
    df$saledate_parsed <- as.Date(sd, origin = "1899-12-30")
  } else {
    df$saledate_parsed <- as.Date(
      suppressWarnings(
        lubridate::parse_date_time(
          as.character(sd),
          orders = c(
            "ymd HMS", "ymd HM", "ymd",
            "mdy HMS", "mdy HM", "mdy",
            "dmy HMS", "dmy HM", "dmy"
          ),
          tz = "UTC",
          quiet = TRUE
        )
      )
    )
  }
  
  df$sale_month <- lubridate::floor_date(df$saledate_parsed, "month")
  
  na_date <- mean(is.na(df$saledate_parsed))
  if (na_date > 0) msg("Parsed saledate: NA rate = ", round(100 * na_date, 2), "% (check formats if high).")
  
  # ---- normalize style strings BEFORE filtering (fixes whitespace/case diffs) ----
  style_keep2 <- trimws(toupper(style_keep))
  df <- df |>
    dplyr::mutate(
      !!style_col := trimws(toupper(as.character(.data[[style_col]])))
    ) |>
    dplyr::filter(.data[[style_col]] %in% style_keep2)
  
  # ---- standardize parcel no (matches your old logic) ----
  df <- df |>
    dplyr::mutate(
      parcelno = as.character(.data[[parcelno_col]]),
      parcelno_std = parcelno |>
        gsub("\\.", "P", x = _) |>
        gsub("-", "D", x = _)
    )
  
  # ---- Deduplicate using parsed date (key fix) ----
  df <- df |>
    dplyr::arrange(parcelno_std, .data[[saleprice_col]], saledate_parsed) |>
    dplyr::group_by(parcelno_std, .data[[saleprice_col]], saledate_parsed) |>
    dplyr::slice_tail(n = 1) |>
    dplyr::ungroup()
  
  # ---- Remove price==assessment transactions (if present) ----
  if (avwhensold_col %in% names(df)) {
    df <- df |>
      dplyr::mutate(
        priceisasmt_ratio = dplyr::if_else(.data[[saleprice_col]] > 0,
                                           .data[[avwhensold_col]] / .data[[saleprice_col]],
                                           NA_real_),
        priceisasmt_flag = dplyr::if_else(
          is.finite(priceisasmt_ratio) & priceisasmt_ratio >= 0.90 & priceisasmt_ratio <= 1.10,
          1L, 0L, missing = 0L
        )
      ) |>
      dplyr::filter(priceisasmt_flag == 0L) |>
      dplyr::select(-priceisasmt_ratio, -priceisasmt_flag)
  }
  
  # ---- out_bad filter (now safe because saledate_parsed parses both formats) ----
  df <- df |>
    dplyr::mutate(
      out_bad = dplyr::case_when(
        is.na(saledate_parsed) | is.na(.data[[saleprice_col]]) | is.na(.data[[totalsqft_col]]) | is.na(.data[[floorarea_col]]) ~ 1L,
        .data[[totalsqft_col]] < min_totalsqft ~ 1L,
        .data[[floorarea_col]] < min_floorarea ~ 1L,
        .data[[saleprice_col]] < min_saleprice ~ 1L,
        TRUE ~ 0L
      )
    ) |>
    dplyr::filter(out_bad == 0L)
  
  # ---- time index (month number since origin) WITHOUT lubridate::months() ----
  if (is.null(origin_month)) {
    origin_month <- min(df$sale_month, na.rm = TRUE)
  } else {
    origin_month <- lubridate::floor_date(as.Date(origin_month), "month")
  }
  
  df <- df |>
    dplyr::mutate(
      months = 12L * (lubridate::year(sale_month) - lubridate::year(origin_month)) +
        (lubridate::month(sale_month) - lubridate::month(origin_month)),
      months = pmax(as.integer(months), 0L),
      months_1to9    = pmin(months, 9L),
      months_10to16  = pmin(pmax(months - 9L, 0L), 7L),
      months_17to24  = pmax(months - 16L, 0L)
    )
  
  # ---- ln_garagespaces safety (if garagespaces exists later, prefer log1p) ----
  # (This is just a placeholder here; your larger prep can add garagespaces and then:
  # df$ln_garagespaces <- log1p(df$garagespaces))
  
  # ---- ECF rename + optional dummies ----
  if (ecf_col %in% names(df) && !"ecf" %in% names(df)) {
    df <- df |>
      dplyr::rename(ecf = !!ecf_col)
  }
  
  ecfs_col <- character(0)
  if (isTRUE(make_ecf_dummies)) {
    if (!has_fastd) stop("make_ecf_dummies=TRUE requires fastDummies")
    if (!("ecf" %in% names(df))) stop("No ecf column found to dummy.")
    
    df <- df |>
      dplyr::mutate(ecf = trimws(toupper(as.character(ecf))))
    
    df <- fastDummies::dummy_cols(
      df,
      select_columns = "ecf",
      remove_first_dummy = remove_first_dummy,
      remove_selected_columns = !keep_ecf_original
    )
    
    ecfs_col <- grep("^ecf_", names(df), value = TRUE)
  }
  
  attr(df, "origin_month") <- origin_month
  attr(df, "ecfs_col") <- ecfs_col
  
  list(data = df, origin_month = origin_month, ecfs_col = ecfs_col)
}

###

prep_cod_sales4 <- function(df,
                           origin_month = NULL,          # e.g. as.Date("2022-06-01")
                           template = NULL,              # list(ecf_levels=..., schema_cols=...)
                           style_keep = c("SINGLE FAMILY", "1/2 DUPLEX"),
                           min_saleprice = 10000,
                           min_totalsqft = 400,
                           min_floorarea = 400,
                           ecf_other = "OTHER",
                           make_ecf_dummies = TRUE,
                           remove_first_dummy = TRUE,
                           keep_ecf_original = TRUE,
                           verbose = TRUE) {
  
  stopifnot(requireNamespace("dplyr", quietly = TRUE))
  stopifnot(requireNamespace("lubridate", quietly = TRUE))
  has_readr <- requireNamespace("readr", quietly = TRUE)
  
  msg <- function(...) if (isTRUE(verbose)) message(...)
  
  df <- dplyr::as_tibble(df)
  names(df) <- tolower(names(df))
  
  # --- helpers ---
  to_num <- function(x) {
    if (is.numeric(x)) return(x)
    if (has_readr) return(readr::parse_number(as.character(x)))
    suppressWarnings(as.numeric(gsub("[^0-9.\\-]+", "", as.character(x))))
  }
  get_col <- function(nm) if (nm %in% names(df)) df[[nm]] else NULL
  
  # --- required-ish columns (we’ll still create NAs if missing, but these matter) ---
  # (your files use these names consistently)
  if (!("saleid" %in% names(df))) msg("Note: saleid not found (ok unless you need per-sale lookups).")
  
  # ---- robust date parse: handles ISO and m/d/y ----
  if (!("saledate" %in% names(df))) stop("Missing saledate column.")
  
  sd <- df$saledate
  if (inherits(sd, "Date")) {
    df$saledate_parsed <- sd
  } else if (inherits(sd, "POSIXt")) {
    df$saledate_parsed <- as.Date(sd)
  } else if (is.numeric(sd)) {
    df$saledate_parsed <- as.Date(sd, origin = "1899-12-30")
  } else {
    df$saledate_parsed <- as.Date(
      suppressWarnings(
        lubridate::parse_date_time(
          as.character(sd),
          orders = c("ymd HMS","ymd HM","ymd","mdy HMS","mdy HM","mdy","dmy HMS","dmy HM","dmy"),
          tz = "UTC",
          quiet = TRUE
        )
      )
    )
  }
  df$sale_month <- lubridate::floor_date(df$saledate_parsed, "month")
  
  # ---- numeric coercions ----
  if ("saleprice" %in% names(df)) df$saleprice <- to_num(df$saleprice)
  if ("avwhensold" %in% names(df)) df$avwhensold <- to_num(df$avwhensold)
  if ("stotalsqft" %in% names(df)) df$stotalsqft <- to_num(df$stotalsqft)
  if ("sresb_floorarea" %in% names(df)) df$sresb_floorarea <- to_num(df$sresb_floorarea)
  
  # ---- normalize style + filter ----
  if ("sresb_style" %in% names(df)) {
    df$sresb_style <- trimws(toupper(as.character(df$sresb_style)))
    style_keep2 <- trimws(toupper(style_keep))
    df <- dplyr::filter(df, sresb_style %in% style_keep2)
  } else {
    # still continue; you’ll just lose style-derived features
    df$sresb_style <- NA_character_
  }
  
  # ---- dedup (use parsed date) if parcelno exists ----
  if ("parcelno" %in% names(df) && "saleprice" %in% names(df)) {
    df$parcelno <- as.character(df$parcelno)
    df$parcelno_std <- gsub("-", "D", gsub("\\.", "P", df$parcelno))
    df <- df |>
      dplyr::arrange(parcelno_std, saleprice, saledate_parsed) |>
      dplyr::group_by(parcelno_std, saleprice, saledate_parsed) |>
      dplyr::slice_tail(n = 1) |>
      dplyr::ungroup()
  } else {
    df$parcelno_std <- NA_character_
  }
  
  # ---- price == assessment filter (if present) ----
  if (all(c("avwhensold","saleprice") %in% names(df))) {
    ratio <- dplyr::if_else(df$saleprice > 0, df$avwhensold / df$saleprice, NA_real_)
    flag  <- dplyr::if_else(is.finite(ratio) & ratio >= 0.90 & ratio <= 1.10, 1L, 0L, missing = 0L)
    df <- dplyr::filter(df, flag == 0L)
  }
  
  # ---- out_bad (missing core fields count as bad) ----
  df$out_bad <- dplyr::case_when(
    is.na(df$saledate_parsed) | is.na(df$saleprice) | is.na(df$stotalsqft) | is.na(df$sresb_floorarea) ~ 1L,
    df$stotalsqft < min_totalsqft ~ 1L,
    df$sresb_floorarea < min_floorarea ~ 1L,
    df$saleprice < min_saleprice ~ 1L,
    TRUE ~ 0L
  )
  df <- dplyr::filter(df, out_bad == 0L)
  
  # ---- months index (no lubridate::months()) ----
  if (is.null(origin_month)) origin_month <- min(df$sale_month, na.rm = TRUE)
  origin_month <- lubridate::floor_date(as.Date(origin_month), "month")
  
  df$months <- 12L * (lubridate::year(df$sale_month) - lubridate::year(origin_month)) +
    (lubridate::month(df$sale_month) - lubridate::month(origin_month))
  df$months <- pmax(as.integer(df$months), 0L)
  df$months_1to9   <- pmin(df$months, 9L)
  df$months_10to16 <- pmin(pmax(df$months - 9L, 0L), 7L)
  df$months_17to24 <- pmax(df$months - 16L, 0L)
  
  # =========================
  # FULL ENGINEERING (always create columns)
  # =========================
  
  # priceclass
  df$priceclass <- dplyr::case_when(
    is.na(df$saleprice) ~ NA_integer_,
    df$saleprice <  48000 ~ 1L,
    df$saleprice <  67000 ~ 2L,
    df$saleprice <  94000 ~ 3L,
    df$saleprice < 134000 ~ 4L,
    TRUE ~ 5L
  )
  
  # sppsf + logs
  df$sppsf    <- dplyr::if_else(df$sresb_floorarea > 0, df$saleprice / df$sresb_floorarea, NA_real_)
  df$ln_sppsf <- dplyr::if_else(is.finite(df$sppsf) & df$sppsf > 0, log(df$sppsf), NA_real_)
  df$ln_price <- dplyr::if_else(is.finite(df$saleprice) & df$saleprice > 0, log(df$saleprice), NA_real_)
  
  # yearbuilt buckets
  if (!("sresb_yearbuilt" %in% names(df))) df$sresb_yearbuilt <- NA_real_
  yb <- to_num(df$sresb_yearbuilt)
  df$erabuilt <- dplyr::case_when(
    is.na(yb) ~ NA_integer_,
    yb < 1910 ~ 1L,
    yb < 1930 ~ 2L,
    yb < 1946 ~ 3L,
    yb < 1961 ~ 4L,
    yb <= 1990 ~ 5L,
    TRUE ~ 6L
  )
  df$yb_pre_1929     <- dplyr::if_else(df$erabuilt %in% c(1,2), 1L, 0L, missing = 0L)
  df$yb_1946_to_1960 <- dplyr::if_else(df$erabuilt == 4, 1L, 0L, missing = 0L)
  df$yb_post_1961    <- dplyr::if_else(df$erabuilt %in% c(5,6), 1L, 0L, missing = 0L)
  
  # condition
  if (!("scond" %in% names(df))) df$scond <- NA_real_
  sc <- to_num(df$scond)
  df$good <- dplyr::if_else(sc == 2, 1L, 0L, missing = 0L)
  df$fair <- dplyr::if_else(sc == 4, 1L, 0L, missing = 0L)
  df$poor <- dplyr::if_else(sc == 5, 1L, 0L, missing = 0L)
  df$very_poor_unsound <- dplyr::if_else(sc %in% c(6,7), 1L, 0L, missing = 0L)
  
  # fireplace
  if (!("sresb_fireplace" %in% names(df))) df$sresb_fireplace <- NA_real_
  fp <- to_num(df$sresb_fireplace)
  df$fireplace <- dplyr::if_else(fp %in% 1:6, 1L, 0L, missing = 0L)
  
  # heating/cooling
  if (!("sresb_heat" %in% names(df))) df$sresb_heat <- NA_real_
  ht <- to_num(df$sresb_heat)
  df$hot_water      <- dplyr::if_else(ht == 2, 1L, 0L, missing = 0L)
  df$elec_baseboard <- dplyr::if_else(ht == 3, 1L, 0L, missing = 0L)
  df$wall           <- dplyr::if_else(ht %in% c(6,8), 1L, 0L, missing = 0L)
  df$central_air    <- dplyr::if_else(ht == 9, 1L, 0L, missing = 0L)
  
  # garage spaces from area + ln_garagespaces (log1p = no NA/Inf at 0)
  if (!("sresb_garagearea" %in% names(df))) df$sresb_garagearea <- NA_real_
  ga <- to_num(df$sresb_garagearea)
  df$garage <- dplyr::if_else(ga >= 200, 1L, 0L, missing = 0L)
  df$garagespaces <- dplyr::case_when(
    is.na(ga) ~ NA_integer_,
    ga > 200 & ga < 420 ~ 1L,
    ga > 419 & ga < 600 ~ 2L,
    ga > 599 & ga < 820 ~ 3L,
    ga > 819 ~ 4L,
    TRUE ~ 0L
  )
  df$ln_garagespaces <- log1p(dplyr::coalesce(as.numeric(df$garagespaces), 0))
  
  # arterial
  if (!("rd_class" %in% names(df))) df$rd_class <- NA_character_
  df$arterial <- dplyr::if_else(df$rd_class %in% c("A21","A31"), 1L, 0L, missing = 0L)
  
  # size bins
  fa <- df$sresb_floorarea
  df$size <- dplyr::case_when(
    is.na(fa) ~ NA_integer_,
    fa <  830 ~ 1L,
    fa < 1018 ~ 2L,
    fa < 1255 ~ 3L,
    fa < 1632 ~ 4L,
    TRUE ~ 5L
  )
  df$smallest <- dplyr::if_else(df$size == 1, 1L, 0L, missing = 0L)
  df$small    <- dplyr::if_else(df$size == 2, 1L, 0L, missing = 0L)
  df$large    <- dplyr::if_else(df$size == 4, 1L, 0L, missing = 0L)
  df$largest  <- dplyr::if_else(df$size == 5, 1L, 0L, missing = 0L)
  
  # =========================
  # ECF handling + stable dummies
  # =========================
  # rename sECF/secf/ecf variants to ecf
  if (!("ecf" %in% names(df))) {
    if ("secf" %in% names(df)) df$ecf <- df$secf
    if (!("ecf" %in% names(df)) && "s_ecf" %in% names(df)) df$ecf <- df$s_ecf
    if (!("ecf" %in% names(df)) && "s ecf" %in% names(df)) df$ecf <- df[["s ecf"]]
  }
  if (!("ecf" %in% names(df))) df$ecf <- NA_character_
  
  df$ecf <- trimws(toupper(as.character(df$ecf)))
  df$ecf[is.na(df$ecf) | df$ecf == ""] <- ecf_other
  
  ecf_levels <- if (!is.null(template) && !is.null(template$ecf_levels)) template$ecf_levels else sort(unique(df$ecf))
  if (!(ecf_other %in% ecf_levels)) ecf_levels <- c(ecf_levels, ecf_other)
  df$ecf[!df$ecf %in% ecf_levels] <- ecf_other
  df$ecf <- factor(df$ecf, levels = ecf_levels)
  
  ecfs_col <- character(0)
  if (isTRUE(make_ecf_dummies)) {
    mm <- model.matrix(~ ecf - 1, data = df)
    colnames(mm) <- paste0("ecf_", levels(df$ecf))
    if (remove_first_dummy && ncol(mm) > 0) mm <- mm[, -1, drop = FALSE]
    ecfs_col <- colnames(mm)
    df <- dplyr::bind_cols(df, as.data.frame(mm, check.names = FALSE))
    if (!keep_ecf_original) df$ecf <- NULL
  }
  
  # =========================
  # Schema alignment to template (forces identical cols)
  # =========================
  schema_cols <- if (!is.null(template) && !is.null(template$schema_cols)) template$schema_cols else names(df)
  
  missing_cols <- setdiff(schema_cols, names(df))
  if (length(missing_cols)) {
    for (nm in missing_cols) {
      # 0 for obvious dummy-ish columns, NA otherwise
      df[[nm]] <- if (startsWith(nm, "ecf_") || grepl("^(yb_|good$|fair$|poor$|very_poor|hot_water|elec_baseboard|wall$|central_air|fireplace$|arterial$|smallest$|small$|large$|largest$)", nm)) 0 else NA
    }
  }
  df <- df[, schema_cols, drop = FALSE]
  
  out <- list(
    data = df,
    template = list(
      origin_month = origin_month,
      ecf_levels = ecf_levels,
      ecfs_col = ecfs_col,
      schema_cols = schema_cols
    )
  )
  
  out
}

###

backward_p2 <- function(formula, data, p_out = 0.10, keep = NULL,
                       verbose = TRUE, max_steps = 500,
                       na_action = stats::na.omit) {
  
  # Freeze rows once so stepwise never changes sample size
  mf0 <- model.frame(formula, data = data, na.action = na_action)
  
  current_formula <- formula
  steps <- list()
  step_i <- 0L
  
  repeat {
    mod <- lm(current_formula, data = mf0)
    
    # Term-level p-values (works with factors properly)
    d1 <- drop1(mod, test = "F")
    d1 <- d1[rownames(d1) != "<none>", , drop = FALSE]
    
    pcol <- "Pr(>F)"
    pvals <- d1[[pcol]]
    names(pvals) <- rownames(d1)
    
    # Optionally protect certain terms from being dropped
    if (!is.null(keep)) {
      pvals <- pvals[!names(pvals) %in% keep]
    }
    
    # --- guards (prevents your error)
    if (length(pvals) == 0L || all(is.na(pvals))) break
    
    worst_p <- max(pvals, na.rm = TRUE)
    if (!is.finite(worst_p) || worst_p <= p_out) break
    
    worst_term <- names(pvals)[which.max(pvals)]
    if (length(worst_term) == 0L || is.na(worst_term) || worst_term == "") break
    
    step_i <- step_i + 1L
    if (verbose) message("Dropping: ", worst_term, "  (p=", signif(worst_p, 3), ")")
    
    steps[[step_i]] <- data.frame(
      step = step_i,
      dropped = worst_term,
      p_value = worst_p,
      n = nobs(mod),
      adj_r2 = summary(mod)$adj.r.squared,
      stringsAsFactors = FALSE
    )
    
    current_formula <- update(current_formula, paste(". ~ . -", worst_term))
    
    if (step_i >= max_steps) break
  }
  
  final_mod <- lm(current_formula, data = mf0)
  steps_df <- if (length(steps)) do.call(rbind, steps) else data.frame()
  
  list(model = final_mod, steps = steps_df, data_used = mf0)
}

# Usage

# res <- backward_p2(formula_base, data = df, p_out = 0.10, verbose = TRUE)
# mod_base <- res$model
# res$steps

###

combine_sales_windows <- function(df_old, df_new, id_col = "saleid",
                                  date_col = NULL,          # e.g. "saledate_parsed" or "saledate"
                                  keep = c("new", "old", "latest_date", "most_complete"),
                                  sort_by_id = TRUE) {
  keep <- match.arg(keep)
  
  stopifnot(is.data.frame(df_old), is.data.frame(df_new))
  
  # 1) Keep only columns shared between both datasets
  common_cols <- intersect(names(df_old), names(df_new))
  if (!id_col %in% common_cols) {
    stop("`id_col` (", id_col, ") is not present in BOTH data frames after intersect().")
  }
  
  a <- df_old[, common_cols, drop = FALSE]
  b <- df_new[, common_cols, drop = FALSE]
  
  # 2) Harmonize column types so bind_rows won't choke (most conservative rule:
  #    if types differ, coerce both to character; preserve Dates/POSIXt nicely if possible)
  parse_date_any <- function(x) {
    if (inherits(x, "Date")) return(x)
    if (inherits(x, "POSIXt")) return(as.Date(x))
    # try lubridate if available for messy strings
    if (requireNamespace("lubridate", quietly = TRUE)) {
      return(as.Date(
        suppressWarnings(
          lubridate::parse_date_time(
            as.character(x),
            orders = c("ymd HMS","ymd HM","ymd","mdy HMS","mdy HM","mdy","dmy HMS","dmy HM","dmy"),
            tz = "UTC", quiet = TRUE
          )
        )
      ))
    }    # base fallback
    suppressWarnings(as.Date(as.character(x)))
  }
  
  for (nm in common_cols) {
    ca <- class(a[[nm]])[1]
    cb <- class(b[[nm]])[1]
    if (ca == cb) next
    
    # If either looks like a date/time column, try to coerce both to Date
    if (inherits(a[[nm]], c("Date","POSIXt")) || inherits(b[[nm]], c("Date","POSIXt"))) {
      a[[nm]] <- parse_date_any(a[[nm]])
      b[[nm]] <- parse_date_any(b[[nm]])
      next
    }
    
    # Otherwise, safest is to coerce both to character
    a[[nm]] <- as.character(a[[nm]])
    b[[nm]] <- as.character(b[[nm]])
  }
  
  # Ensure id is comparable
  a[[id_col]] <- as.character(a[[id_col]])
  b[[id_col]] <- as.character(b[[id_col]])
  
  # 3) Bind with a source marker so we can resolve duplicates deterministically
  a$.src__ <- "old"
  b$.src__ <- "new"
  x <- dplyr::bind_rows(a, b)
  
  # 4) Decide which row to keep for duplicate sale IDs
  if (keep == "new") {
    x <- x %>%
      dplyr::group_by(.data[[id_col]]) %>%
      dplyr::arrange(dplyr::desc(.data$.src__ == "new"), .by_group = TRUE) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup()
    
  } else if (keep == "old") {
    x <- x %>%
      dplyr::group_by(.data[[id_col]]) %>%
      dplyr::arrange(dplyr::desc(.data$.src__ == "old"), .by_group = TRUE) %>%
      dplyr::slice(1) %>% dplyr::ungroup()
    
  } else if (keep == "latest_date") {
    if (is.null(date_col) || !(date_col %in% names(x))) {
      stop("keep='latest_date' requires `date_col` to exist in BOTH datasets.")
    }
    x$.date__ <- parse_date_any(x[[date_col]])
    x <- x %>%
      dplyr::group_by(.data[[id_col]]) %>%
      dplyr::arrange(dplyr::desc(.data$.date__), dplyr::desc(.data$.src__ == "new"), .by_group = TRUE) %>%
      dplyr::slice(1) %>% dplyr::ungroup() %>% dplyr::select(-.date__)
  } else if (keep == "most_complete") {
    # Fewest NA across shared columns wins; tie-breaker prefers df_new
    x$.na_count__ <- rowSums(is.na(x[, common_cols, drop = FALSE]))
    x <- x %>%
      dplyr::group_by(.data[[id_col]]) %>%
      dplyr::arrange(.data$.na_count__, dplyr::desc(.data$.src__ == "new"), .by_group = TRUE) %>%
      dplyr::slice(1) %>% dplyr::ungroup() %>% dplyr::select(-.na_count__)
  }
  
  # 5) Drop helper columns and optionally sort
  x <- x %>% dplyr::select(-.src__)
  
  if (sort_by_id) {x <- x %>% dplyr::arrange(.data[[id_col]])}
  
  # Return merged df plus info about what happened
  list(
    data = x, common_cols = common_cols, n_old = nrow(df_old), n_new = nrow(df_new),
    n_out = nrow(x), n_dupes_removed = (nrow(dplyr::bind_rows(a, b)) - nrow(x))
  )
}

# Usage
# res <- combine_sales_windows(df_2022_2024, df_2023_2025, id_col = "saleid", keep = "new")
# merged <- res$data

###

make_taf_tbl <- function(gam_mod, data, months_col = "months", baseline = c("median", "first", "last")) {
  baseline <- match.arg(baseline)
  
  # find the smooth column name that corresponds to s(months_col)
  term_names <- colnames(predict(gam_mod, type = "terms"))
  time_term  <- grep(paste0("^s\\(", months_col, "\\)"), term_names, value = TRUE)
  if (length(time_term) != 1) {
    stop("Could not uniquely identify the months smooth term. Found: ", paste(time_term, collapse = ", "))
  }
  
  m_vec <- sort(unique(data[[months_col]]))
  if (length(m_vec) == 0) stop("No months found.")
  
  # pick baseline month
  base_m <- switch(
    baseline,    median = stats::median(m_vec, na.rm = TRUE),
    first  = min(m_vec, na.rm = TRUE),    last   = max(m_vec, na.rm = TRUE)
  )
  
  # newdata for prediction: use first row as a template so other model vars exist
  template <- data[1, , drop = FALSE]
  
  new_m <- template[rep(1, length(m_vec)), , drop = FALSE]
  new_m[[months_col]] <- m_vec
  
  base_row <- template
  base_row[[months_col]] <- base_m
  # smooth term on grid + baseline
  grid_terms <- predict(gam_mod, newdata = new_m, type = "terms", se.fit = TRUE)
  base_term  <- predict(gam_mod, newdata = base_row, type = "terms")
  
  grid_effect <- grid_terms$fit[, time_term]
  grid_se     <- grid_terms$se.fit[, time_term]
  base_effect <- as.numeric(base_term[, time_term])
  
  delta <- grid_effect - base_effect
  
  tibble(
    !!months_col := m_vec,  taf = exp(delta),         # multiplicative factor relative to baseline
    taf_lo95 = exp(delta - 1.96 * grid_se),    taf_hi95 = exp(delta + 1.96 * grid_se),
    taf_baseline_month = base_m
  )
}

# taf_tbl <- make_taf_tbl(mod_time, df, months_col = "months", baseline = "median")

###

get_b <- function(mod, term) {
  co <- coef(mod)
  if (!(term %in% names(co))) return(NA_real_)
  unname(co[[term]])
}

###

add_smooth_time_index <- function(df, gam_mod, months_col = "months",
                                  saleprice_col = "saleprice", area_col = "sresb_floorarea",
                                  baseline = c("median", "first"), end_month = NULL,
                                  handle_missing_months = c("error", "carry")) {
  
  baseline <- match.arg(baseline)
  handle_missing_months <- match.arg(handle_missing_months)
  
  term_names <- colnames(predict(gam_mod, type = "terms"))
  time_term <- grep(paste0("^s\\(", months_col, "\\)"), term_names, value = TRUE)
  if (length(time_term) != 1) stop("Could not uniquely identify smooth term for months. Found: ", paste(time_term, collapse = ", "))
  
  # month grid
  m_vec <- sort(unique(df[[months_col]]))
  if (baseline == "median") base_m <- stats::median(m_vec, na.rm = TRUE) else base_m <- min(m_vec, na.rm = TRUE)
  if (is.null(end_month)) end_month <- max(m_vec, na.rm = TRUE)
  
  # template row for newdata
  template <- df[1, , drop = FALSE]
  
  new_m <- template[rep(1, length(m_vec)), , drop = FALSE]
  new_m[[months_col]] <- m_vec
  
  base_row <- template
  base_row[[months_col]] <- base_m
  
  # smooth term predictions
  grid_terms <- predict(gam_mod, newdata = new_m, type = "terms", se.fit = FALSE)
  base_terms <- predict(gam_mod, newdata = base_row, type = "terms", se.fit = FALSE)
  
  grid_effect <- grid_terms[, time_term]
  base_effect <- as.numeric(base_terms[, time_term])
  
  # index anchored to baseline
  delta <- grid_effect - base_effect
  taf_tbl <- tibble(!!months_col := m_vec, price_index_smooth = exp(delta))
  
  # join onto df
  df2 <- df %>% dplyr::left_join(taf_tbl, by = months_col)
  
  if (anyNA(df2$price_index_smooth)) {
    if (handle_missing_months == "error") {
      stop("Some rows did not get a smooth price_index. Check months coverage / join key type.")
    } else {
      # carry nearest (simple, AVM-ish fallback)
      df2 <- df2 %>% arrange(.data[[months_col]]) %>%
        tidyr::fill(price_index_smooth, .direction = "downup")
    }
  }
  
  # end index at end_month
  end_rows <- df2 %>% dplyr::filter(.data[[months_col]] == end_month)
  if (nrow(end_rows) == 0) stop("No rows found at end_month = ", end_month, " to compute smooth end_index.")
  
  end_index <- end_rows %>% dplyr::summarise(v = mean(price_index_smooth, na.rm = TRUE)) %>% dplyr::pull(v)
  
  df2 %>%
    mutate(
      baseline_month_smooth = base_m,      end_month_smooth = end_month,
      end_index_smooth = end_index,      taf_smooth = end_index_smooth / price_index_smooth,
      tasp_smooth = .data[[saleprice_col]] * taf_smooth,
      tasppsf_smooth = tasp_smooth / .data[[area_col]],      ln_tasp_smooth = log(tasp_smooth)
    )
}

# df <- add_smooth_time_index(df, mod_time_gam)   # where mod_time_gam is your gam() fit with s(months)

###

gam_concurvity_table <- function(mod, digits = 3) {
  stopifnot(inherits(mod, "gam"))
  cc <- mgcv::concurvity(mod, full = TRUE)
  
  # cc is a list of matrices (worst/estimate/observed depending on mgcv version)
  # We'll prefer "estimate" if present.
  M <- if ("estimate" %in% names(cc)) cc$estimate else cc[[1]]
  
  diag(M) <- NA_real_
  out <- data.frame(
    term = rownames(M),
    worst_concurvity = apply(M, 1, function(x) max(x, na.rm = TRUE)),
    stringsAsFactors = FALSE
  )
  out$worst_concurvity <- round(out$worst_concurvity, digits)
  out[order(out$worst_concurvity, decreasing = TRUE), ]
}

# gam_concurvity_table(mod_gam1)

###

add_detroit_parcel_coords <- function(df, parcel_col, x_col = "x_coord", y_col = "y_coord",
                                      batch_size = 100, quiet = FALSE) {
  stopifnot(is.data.frame(df))
  
  if (!parcel_col %in% names(df)) {stop(sprintf("Column '%s' not found in dataframe.", parcel_col))}
  
  service_url <- paste0(
    "https://services2.arcgis.com/qvkbeam7Wirps6zC/",
    "ArcGIS/rest/services/Parcels_Current/FeatureServer/0/query"
  )
  
  `%||%` <- function(a, b) {if (is.null(a) || length(a) == 0) b else a}
  out <- df
  out$.parcel_clean <- as.character(out[[parcel_col]])
  out$.parcel_clean[out$.parcel_clean == ""] <- NA_character_
  unique_parcels <- unique(out$.parcel_clean[!is.na(out$.parcel_clean)])
  
  if (length(unique_parcels) == 0) {
    out[[x_col]] <- NA_real_
    out[[y_col]] <- NA_real_
    return(out %>% select(-.parcel_clean))
  }
  
  chunks <- split(unique_parcels, ceiling(seq_along(unique_parcels) / batch_size))
  
  if (!quiet) {
    message("Looking up ", length(unique_parcels), " unique parcels in ",
            length(chunks), " batch(es)...")
  }
  
  fetch_chunk <- function(parcel_vec) {
    vals <- paste0(
      "'",
      gsub("'", "''", parcel_vec, fixed = TRUE),
      "'",
      collapse = ","
    )
    
    where_clause <- sprintf("parcel_number IN (%s)", vals)
    
    resp <- httr::GET(
      url = service_url,
      query = list(
        where = where_clause, outFields = "parcel_number",
        returnGeometry = "false", returnCentroid = "true", f = "pjson"
      )
    )
    httr::stop_for_status(resp)
    
    js <- jsonlite::fromJSON(
      httr::content(resp, as = "text", encoding = "UTF-8"), simplifyVector = FALSE
    )
    
    feats <- js$features
    
    if (is.null(feats) || length(feats) == 0) {
      return(data.frame(
        .parcel_clean = parcel_vec, x = NA_real_, y = NA_real_, stringsAsFactors = FALSE
      ))
    }
    
    got <- do.call(
      rbind,
      lapply(feats, function(feat) {
        attrs <- feat$attributes
        cent  <- feat$centroid
        
        data.frame(
          .parcel_clean = attrs$parcel_number %||% NA_character_,
          x = as.numeric(cent$x %||% NA_real_), y = as.numeric(cent$y %||% NA_real_),
          stringsAsFactors = FALSE
        )
      })
    )
    
    got <- got[!duplicated(got$.parcel_clean), , drop = FALSE]
    
    missing <- setdiff(parcel_vec, got$.parcel_clean)
    if (length(missing) > 0) {
      got <- rbind(
        got,
        data.frame(.parcel_clean = missing, x = NA_real_, y = NA_real_, stringsAsFactors = FALSE)
      )
    }
    got
  }
  
  lookup_list <- lapply(chunks, fetch_chunk)
  lookup <- dplyr::bind_rows(lookup_list) %>% dplyr::rename(!!x_col := x, !!y_col := y)
  
  out <- out %>% dplyr::left_join(lookup, by = ".parcel_clean") %>%
    dplyr::select(-.parcel_clean)
  
  out
}

# sales_df <- add_detroit_parcel_coords(df, "parcelno")

###

add_parcel_coords <- function(df, parcel_col, x_col = "x_coord", y_col = "y_coord",
                                      batch_size = 100, quiet = FALSE,
                                      max_retries = 2, pause_sec = 0) {
  if (!is.data.frame(df)) {stop("df must be a data.frame")}
  if (!parcel_col %in% names(df)) {stop(sprintf("Column '%s' not found in dataframe.", parcel_col))}
  if (!is.numeric(batch_size) || length(batch_size) != 1 || batch_size < 1) {
    stop("batch_size must be a single positive number.")
  }
  
  service_url <- paste0(
    "https://services2.arcgis.com/qvkbeam7Wirps6zC/",
    "ArcGIS/rest/services/Parcels_Current/FeatureServer/0/query"
  )
  
  out <- as.data.frame(df, stringsAsFactors = FALSE)
  parcels <- as.character(out[[parcel_col]])
  parcels[parcels == ""] <- NA_character_
  
  unique_parcels <- unique(parcels[!is.na(parcels)])
  out[[x_col]] <- NA_real_
  out[[y_col]] <- NA_real_
  
  if (length(unique_parcels) == 0) {return(out)}
  if (!quiet) {
    message(
      "Looking up ", length(unique_parcels), " unique parcels in ",
      ceiling(length(unique_parcels) / batch_size), " batch(es)..."
    )
  }
  
  na_result <- function(parcel_vec) {
    data.frame(parcel_key = parcel_vec, x = NA_real_, y = NA_real_, stringsAsFactors = FALSE)
  }
  
  parse_features <- function(txt, requested_parcels) {
    js <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
    feats <- js$features
    if (is.null(feats) || length(feats) == 0) {return(na_result(requested_parcels))}
    rows <- vector("list", length(feats))
    
    for (i in seq_along(feats)) {
      feat <- feats[[i]]
      parcel_val <- NA_character_
      x_val <- NA_real_
      y_val <- NA_real_
      
      if (!is.null(feat$attributes) && !is.null(feat$attributes$parcel_number)) {
        parcel_val <- as.character(feat$attributes$parcel_number)
      }
      
      if (!is.null(feat$centroid)) {
        if (!is.null(feat$centroid$x)) x_val <- suppressWarnings(as.numeric(feat$centroid$x))
        if (!is.null(feat$centroid$y)) y_val <- suppressWarnings(as.numeric(feat$centroid$y))
      }
      
      rows[[i]] <- data.frame(
        parcel_key = parcel_val, x = x_val, y = y_val, stringsAsFactors = FALSE
      )
    }
    
    got <- do.call(rbind, rows)
    got <- got[!is.na(got$parcel_key), , drop = FALSE]
    got <- got[!duplicated(got$parcel_key), , drop = FALSE]
    
    missing <- setdiff(requested_parcels, got$parcel_key)
    if (length(missing) > 0) {
      got <- rbind(
        got,
        data.frame(parcel_key = missing, x = NA_real_, y = NA_real_, stringsAsFactors = FALSE)
      )
    }
    got
  }
  
  request_chunk_once <- function(parcel_vec) {
    vals <- paste0(
      "'", gsub("'", "''", parcel_vec, fixed = TRUE), "'", collapse = ","
    )
    
    where_clause <- sprintf("parcel_number IN (%s)", vals)
    
    resp <- httr2::request(service_url) |>
      httr2::req_url_query(
        where = where_clause, outFields = "parcel_number", returnGeometry = "false",
        returnCentroid = "true", f = "pjson"
      ) |> httr2::req_perform()
    
    txt <- httr2::resp_body_string(resp)
    parse_features(txt, parcel_vec)
  }
  
  fetch_chunk <- function(parcel_vec, depth = 0) {
    result <- NULL
    
    for (attempt in seq_len(max_retries + 1)) {
      result <- tryCatch(request_chunk_once(parcel_vec), error = function(e) e)
      
      if (!inherits(result, "error")) {return(result)}
      if (pause_sec > 0) Sys.sleep(pause_sec)
    }
    
    if (length(parcel_vec) == 1) {
      if (!quiet) {message("Parcel failed: ", parcel_vec)}
      return(na_result(parcel_vec))
    }
    
    mid <- floor(length(parcel_vec) / 2)
    left <- parcel_vec[seq_len(mid)]
    right <- parcel_vec[(mid + 1):length(parcel_vec)]
    
    if (!quiet) {
      message("Batch failed for ", length(parcel_vec), " parcels; splitting into ",
              length(left), " and ", length(right), "."
      )
    }
    
    rbind(fetch_chunk(left, depth + 1), fetch_chunk(right, depth + 1))
  }
  
  chunks <- split(unique_parcels, ceiling(seq_along(unique_parcels) / batch_size))
  results_list <- vector("list", length(chunks))
  
  for (i in seq_along(chunks)) {
    if (!quiet && (i %% 10 == 0 || i == length(chunks))) {
      message("Processing batch ", i, " / ", length(chunks))
    }
    results_list[[i]] <- fetch_chunk(chunks[[i]])
  }
  
  lookup <- do.call(rbind, results_list)
  lookup <- lookup[!duplicated(lookup$parcel_key), , drop = FALSE]
  x_map <- setNames(lookup$x, lookup$parcel_key)
  y_map <- setNames(lookup$y, lookup$parcel_key)
  out[[x_col]] <- unname(x_map[parcels])
  out[[y_col]] <- unname(y_map[parcels])
  
  out
}

###

add_dist_to_guardian_linear <- function(df, lon_col = "x_coord", lat_col = "y_coord",
                                        out_m_col = "dist_guardian_m",
                                        out_km_col = "dist_guardian_km",
                                        out_mi_col = "dist_guardian_mi",
                                        guardian_lon, guardian_lat) {
  stopifnot(is.data.frame(df))
  if (!lon_col %in% names(df)) stop(sprintf("Missing column: %s", lon_col))
  if (!lat_col %in% names(df)) stop(sprintf("Missing column: %s", lat_col))
  
  pts <- cbind(df[[lon_col]], df[[lat_col]])
  dest <- c(guardian_lon, guardian_lat)
  
  dist_m <- geosphere::distHaversine(pts, dest)
  
  df[[out_m_col]]  <- dist_m
  df[[out_km_col]] <- dist_m / 1000
  df[[out_mi_col]] <- dist_m / 1609.344
  df
}

# sales_df <- add_dist_to_guardian_linear(
#   sales_df, lon_col = "x_coord", lat_col = "y_coord", guardian_lon = -83.0466, guardian_lat = 42.3317
# )



###

add_linear_distance <- function(df, lon_col, lat_col, target_lon, target_lat,
                                out_m_col = "dist_m", out_km_col = "dist_km",
                                out_mi_col = "dist_mi") {
  stopifnot(is.data.frame(df))
  
  if (!lon_col %in% names(df)) {stop(sprintf("Missing longitude column: %s", lon_col))}
  if (!lat_col %in% names(df)) {stop(sprintf("Missing latitude column: %s", lat_col))}
  if (!is.numeric(target_lon) || length(target_lon) != 1 || is.na(target_lon)) {
    stop("target_lon must be a single non-missing numeric value.")
  }
  if (!is.numeric(target_lat) || length(target_lat) != 1 || is.na(target_lat)) {
    stop("target_lat must be a single non-missing numeric value.")
  }
  
  out <- df
  pts <- cbind(as.numeric(out[[lon_col]]), as.numeric(out[[lat_col]]))
  target <- c(target_lon, target_lat)
  dist_m <- geosphere::distHaversine(pts, target)
  
  out[[out_m_col]]  <- dist_m
  out[[out_km_col]] <- dist_m / 1000
  out[[out_mi_col]] <- dist_m / 1609.344
  
  out
}

#sales_df2 <- add_linear_distance(
#  df = sales_df2,  lon_col = "x_coord",  lat_col = "y_coord",  target_lon = -83.0777,
#  target_lat = 42.3679,  out_m_col = "dist_henryford_m",
#  out_km_col = "dist_henryford_km",  out_mi_col = "dist_henryford_mi"
#)

###

library(geosphere)

add_linear_distance_onecol <- function(df,       lon_col,          lat_col,            target_lon,
                                       target_lat,         out_col = "dist_mi",
                                       units = c("mi", "km", "m")) {
  stopifnot(is.data.frame(df))
  units <- match.arg(units)
  if (!lon_col %in% names(df)) stop(sprintf("Missing longitude column: %s", lon_col))
  if (!lat_col %in% names(df)) stop(sprintf("Missing latitude column: %s", lat_col))
  pts <- cbind(as.numeric(df[[lon_col]]), as.numeric(df[[lat_col]]))
  target <- c(target_lon, target_lat)
  dist_m <- geosphere::distHaversine(pts, target)
  
  df[[out_col]] <- switch(
    units,    m  = dist_m,    km = dist_m / 1000,    mi = dist_m / 1609.344
  )
  
  df
}

#sales_df2 <- add_linear_distance_onecol(
#  df = sales_df2,  lon_col = "x_coord",  lat_col = "y_coord",  target_lon = -83.0777,
#  target_lat = 42.3679,  out_col = "dist_henryford_mi",  units = "mi"
#)

###

add_drive_distance_ors <- function(df, lon_col, lat_col, target_lon, target_lat,
                                   profile = ors_profile("car"), dist_col = "drive_dist_km",
                                   time_col = "drive_time_min", batch_size = 200) {
  stopifnot(is.data.frame(df))
  
  if (!lon_col %in% names(df)) {stop(sprintf("Missing longitude column: %s", lon_col))}
  if (!lat_col %in% names(df)) {stop(sprintf("Missing latitude column: %s", lat_col))}
  if (!is.numeric(target_lon) || length(target_lon) != 1 || is.na(target_lon)) {
    stop("target_lon must be a single non-missing numeric value.")
  }
  if (!is.numeric(target_lat) || length(target_lat) != 1 || is.na(target_lat)) {
    stop("target_lat must be a single non-missing numeric value.")
  }
  
  out <- df %>% mutate(.row_id = seq_len(n()))
  
  valid <- out %>%
    filter(!is.na(.data[[lon_col]]), !is.na(.data[[lat_col]])) %>%
    transmute(.row_id = .row_id, lon = as.numeric(.data[[lon_col]]),
      lat = as.numeric(.data[[lat_col]])
    )
  
  out[[dist_col]] <- NA_real_
  out[[time_col]] <- NA_real_
  
  if (nrow(valid) == 0) {return(out %>% select(-.row_id))}
  
  chunks <- split(valid, ceiling(seq_len(nrow(valid)) / batch_size))
  
  res_list <- lapply(chunks, function(chunk) {
    coords <- rbind(as.matrix(chunk[, c("lon", "lat")]), c(target_lon, target_lat))
    
    src_idx <- seq_len(nrow(chunk)) - 1L
    dst_idx <- nrow(coords) - 1L
    
    mat <- openrouteservice::ors_matrix(
      locations = coords, profile = profile, metrics = c("duration", "distance"),
      units = "km", sources = src_idx, destinations = dst_idx
    )
    
    data.frame(
      .row_id = chunk$.row_id, dist = as.numeric(mat$distances[, 1]),
      time = as.numeric(mat$durations[, 1]) / 60, stringsAsFactors = FALSE
    )
  })
  
  lookup <- bind_rows(res_list)
  
  out <- out %>% left_join(lookup, by = ".row_id")
  
  out[[dist_col]] <- out$dist
  out[[time_col]] <- out$time
  
  out %>% select(-.row_id, -dist, -time)
}

#sales_df3 <- add_drive_distance_ors(
#  df = sales_df2,  lon_col = "x_coord",  lat_col = "y_coord",  target_lon = -83.0777,
#  target_lat = 42.3679,  dist_col = "drive_henryford_km",  time_col = "drive_henryford_min"
#)



###

make_time_adjusted_prices <- function(df, formula_time, p_out = 0.10, verbose = TRUE) {
  
  # 1. Fit backward-selection time model
  mod_time <- backward_p(
    formula = formula_time,   data = df,  p_out = p_out, verbose = verbose
  )
  
  # 2. Safely pull the time coefficients
  coef_names <- c("months_1to9", "months_10to16", "months_17to24")
  all_coefs   <- coef(mod_time)
  
  time_coefs <- setNames(rep(NA_real_, length(coef_names)), coef_names)
  matched    <- intersect(coef_names, names(all_coefs))
  time_coefs[matched] <- all_coefs[matched]
  
  rate1 <- unname(ifelse(is.na(time_coefs["months_1to9"]),  0, time_coefs["months_1to9"]))
  rate2 <- unname(ifelse(is.na(time_coefs["months_10to16"]), 0, time_coefs["months_10to16"]))
  rate3 <- unname(ifelse(is.na(time_coefs["months_17to24"]), 0, time_coefs["months_17to24"]))
  
  # 3. Create price index
  df_out <- df %>%
    mutate(
      price_index = (1 + rate1)^months_1to9 * (1 + rate2)^months_10to16 *
        (1 + rate3)^months_17to24
    )
  
  # 4. Get ending index from latest month
  end_month <- max(df_out$months, na.rm = TRUE)
  
  end_index <- df_out %>% filter(months == end_month) %>%
    summarise(end_index = mean(price_index, na.rm = TRUE)) %>%  pull(end_index)
  
  # 5. Create TAF-adjusted values
  df_out <- df_out %>%
    mutate(
      taf     = end_index / price_index,      tasp = saleprice * taf,
      tasppsf = tasp / sresb_floorarea,       ln_tasp = log(tasp)
    )
  
  # 6. Return everything useful
  list(
    model      = mod_time,    data  = df_out,    time_coefs = time_coefs,
    rates      = c(
      months_1to9   = rate1,      months_10to16 = rate2,      months_17to24 = rate3
    ),
    end_month  = end_month,    end_index  = end_index
  )
}

### USAGE
res <- make_time_adjusted_prices(df, formula_time)
mod_time <- res$model
df       <- res$data
res$time_coefs
res$rates
res$end_index

###

make_time_adjusted_df <- function(df, formula_time, p_out = 0.10, verbose = TRUE) {
  
  mod_time <- backward_p(formula = formula_time, data = df, p_out = p_out, verbose = verbose)
  coef_names <- c("months_1to9", "months_10to16", "months_17to24")
  all_coefs  <- coef(mod_time)
  
  time_coefs <- setNames(rep(NA_real_, length(coef_names)), coef_names)
  matched    <- intersect(coef_names, names(all_coefs))
  time_coefs[matched] <- all_coefs[matched]
  
  rate1 <- unname(ifelse(is.na(time_coefs["months_1to9"]),  0, time_coefs["months_1to9"]))
  rate2 <- unname(ifelse(is.na(time_coefs["months_10to16"]), 0, time_coefs["months_10to16"]))
  rate3 <- unname(ifelse(is.na(time_coefs["months_17to24"]), 0, time_coefs["months_17to24"]))
  
  df_out <- df %>%
    dplyr::mutate(
      rate1 = rate1, rate2 = rate2, rate3 = rate3,
      price_index = (1 + rate1)^months_1to9 * (1 + rate2)^months_10to16 *
        (1 + rate3)^months_17to24
    )
  
  end_month <- max(df_out$months, na.rm = TRUE)
  end_index <- df_out %>% dplyr::filter(months == end_month) %>%
    dplyr::summarise(end_index = mean(price_index, na.rm = TRUE)) %>% dplyr::pull(end_index)
  
  df_out <- df_out %>%
    dplyr::mutate(taf = end_index / price_index, tasp = saleprice * taf,
      tasppsf = tasp / sresb_floorarea, ln_tasp = log(tasp)
    )
  df_out
}

# df <- make_time_adjusted_df(df, formula_time)

###

plot_standard_residual_hist <- function(mod_time, df, binwidth = 0.25, ptitle = " ") {
  # row indices actually used by the model
  mf <- model.frame(mod_time)
  row_idx <- as.integer(rownames(mf))
  
  # augment fitted data
  aug_sppsf <- broom::augment(mod_time, data = df[row_idx, ]) |>
    dplyr::mutate(pred_sppsf = exp(.fitted), std_resid  = .std.resid)
  
  # residual summary stats
  stats_resid <- aug_sppsf |>
    dplyr::summarise(
      mean = mean(std_resid, na.rm = TRUE),  sd = sd(std_resid,   na.rm = TRUE),
      n    = sum(!is.na(std_resid))
    )
  
  mu  <- stats_resid$mean
  sd_ <- stats_resid$sd
  n_  <- stats_resid$n
  
  # plot
  p <- ggplot2::ggplot(aug_sppsf, ggplot2::aes(x = std_resid)) +
    ggplot2::geom_histogram(
      binwidth = binwidth,  color = "black", fill = "steelblue"
    ) +
    ggplot2::stat_function(
      fun = function(x) stats::dnorm(x, mean = mu, sd = sd_) * n_ * binwidth, linewidth = 1
    ) +
    ggplot2::labs(
      title = ptitle, x = "Regression Standardized Residual", y = "Frequency"
    ) +
    ggplot2::annotate(
      "text", x = Inf, y = Inf, hjust = 1.1, vjust = 1.5,
      label = sprintf("Mean = %.3f\nStd. Dev. = %.3f\nN = %d", mu, sd_, n_)
    ) + ggplot2::theme_minimal()
  
  return(p)
}

# plot_standard_residual_hist(mod_time, df, ptitle = "...")

###

plot_residuals_vs_months <- function(aug_sppsf, ptitle = " ") {
  ggplot2::ggplot(aug_sppsf, ggplot2::aes(x = months, y = std_resid)) +
    ggplot2::geom_point(alpha = 0.5, size = 1.2) +
    ggplot2::geom_smooth(method = "loess", se = FALSE, color = "red") +
    ggplot2::geom_vline(xintercept = c(10, 20), linetype = "dashed") +
    ggplot2::labs(title = ptitle,  x = "Months",  y = "Standardized Residual") +
    ggplot2::theme_minimal()
}

# plot_residuals_vs_months(aug_sppsf, ptitle = "...")

###

plot_predicted_sppsf_vs_months <- function(aug_sppsf, ptitle = " ") {
  ggplot2::ggplot(aug_sppsf, ggplot2::aes(x = months, y = pred_sppsf)) +
    ggplot2::geom_point(alpha = 0.5, size = 1.2) +
    ggplot2::geom_smooth(method = "loess", se = FALSE, color = "blue") +
    ggplot2::geom_vline(xintercept = c(10, 20), linetype = "dashed") +
    ggplot2::labs(title = ptitle, x = "Months", y = "SPPSF") +  ggplot2::theme_minimal()
}

# plot_predicted_sppsf_vs_months(aug_sppsf, ptitle = "...")

###

plot_price_index_vs_months <- function(df_plot) {
  fmt_x <- function(x) ifelse(x == 0, ".00", sprintf("%.2f", x))
  fmt_2 <- function(x) sprintf("%.2f", x)
  
  ggplot2::ggplot(df_plot, ggplot2::aes(x = months, y = price_index)) +
    ggplot2::geom_point(shape = 21, size = 3, stroke = 0.8,
      fill = "deepskyblue3", color = "black") +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 25, 5), labels = fmt_x, expand = ggplot2::expansion(mult = c(0.02, 0.02))
    ) +
    ggplot2::scale_y_continuous(labels = fmt_2) + ggplot2::coord_cartesian(xlim = c(0, 25)) +
    ggplot2::labs(x = "MONTHS", y = "PRICE_INDEX") + ggplot2::theme_classic(base_size = 12) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_line(color = "grey75", linewidth = 0.6),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(size = 14, face = "bold", margin = ggplot2::margin(t = 10)),
      axis.title.y = ggplot2::element_text(size = 14, face = "bold", margin = ggplot2::margin(r = 10)),
      axis.text = ggplot2::element_text(size = 10),
      axis.line = ggplot2::element_line(color = "black", linewidth = 0.8)
    )
}

# plot_price_index_vs_months(df_plot)

###

plot_taf_vs_months <- function(df_plot) {
  fmt_x <- function(x) ifelse(x == 0, ".00", sprintf("%.2f", x))
  fmt_2 <- function(x) sprintf("%.2f", x)
  
  ggplot2::ggplot(df_plot, ggplot2::aes(x = months, y = taf)) +
    ggplot2::geom_point(shape = 21, size = 3, stroke = 0.8, fill = "deepskyblue3", color = "black") +
    ggplot2::scale_x_continuous(limits = c(0, 25), breaks = seq(0, 25, 5),
      labels = fmt_x, expand = ggplot2::expansion(mult = c(0.02, 0.02))
    ) + ggplot2::scale_y_continuous(labels = fmt_2) + ggplot2::labs(x = "MONTHS", y = "TAF") +
    ggplot2::theme_classic(base_size = 12) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_line(color = "grey75", linewidth = 0.6),
      panel.grid.minor.y = ggplot2::element_blank(), panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(size = 14, face = "bold", margin = ggplot2::margin(t = 10)),
      axis.title.y = ggplot2::element_text(size = 14, face = "bold", margin = ggplot2::margin(r = 10)),
      axis.text = ggplot2::element_text(size = 10),
      axis.line = ggplot2::element_line(color = "black", linewidth = 0.8)
    )
}

# plot_taf_vs_months(df_plot)

###

plot_leverage_histogram <- function(model, df, bins_n = 30, title = "Histogram",
                               subtitle = "Dependent Variable: LN_PRICE") {
  
  rows_used <- as.integer(rownames(stats::model.frame(model)))
  lev_vals  <- stats::hatvalues(model)
  
  lev_df <- df %>% dplyr::mutate(lev = NA_real_) %>%
    dplyr::mutate(lev = replace(lev, rows_used, lev_vals))
  
  mu  <- mean(lev_df$lev, na.rm = TRUE)
  sdv <- stats::sd(lev_df$lev, na.rm = TRUE)
  n   <- sum(!is.na(lev_df$lev))
  
  binwidth <- diff(range(lev_df$lev, na.rm = TRUE)) / bins_n
  n_lab <- format(n, big.mark = ",")
  
  ggplot2::ggplot(lev_df, ggplot2::aes(x = lev)) +
    ggplot2::geom_histogram(
      bins = bins_n, boundary = 0, closed = "left", fill = "deepskyblue2",
      color = "black", linewidth = 0.4
    ) +
    ggplot2::stat_function(
      fun = function(x) stats::dnorm(x, mean = mu, sd = sdv) * n * binwidth,
      color = "black", linewidth = 1.3
    ) +
    ggplot2::labs(title = title, subtitle = subtitle, x = "Regression Leverage", y = "Frequency") +
    ggplot2::annotate("text", x = Inf, y = Inf, hjust = 1.05, vjust = 1.2,
      label = paste0(
        "Mean = ", sprintf("%.2f", mu), "\n", "Std. Dev. = ", sprintf("%.3f", sdv), "\n",
        "N = ", n_lab
      ), size = 4
    ) + ggplot2::theme_classic(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, face = "bold"),
      axis.title.x = ggplot2::element_text(size = 14, face = "bold", margin = ggplot2::margin(t = 10)),
      axis.title.y = ggplot2::element_text(size = 14, face = "bold", margin = ggplot2::margin(r = 10)),
      panel.grid.major.y = ggplot2::element_line(color = "grey70", linewidth = 0.6),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank()
    )
}

# plot_leverage_histogram(model = mod_ln_price_no_ecf, df = df, subtitle = "Dependent Variable: LN_PRICE")

###

assess_ln_price_model <- function(df, mod_base) {
  
  df <- df %>%
    dplyr::mutate(
      pre_1  = stats::predict(mod_base, newdata = df), esp1 = exp(pre_1),
      ratio1 = esp1 / saleprice
    )
  
  rrmetrics <- ratio_metrics(df, esp1, saleprice)
  
  ratio_ecf        <- ratio_by(df, esp1, saleprice, ecf)
  ratio_style      <- ratio_by(df, esp1, saleprice, sresb_style)
  ratio_cond       <- ratio_by(df, esp1, saleprice, scond)
  ratio_size       <- ratio_by(df, esp1, saleprice, size)
  ratio_era        <- ratio_by(df, esp1, saleprice, erabuilt)
  ratio_fire       <- ratio_by(df, esp1, saleprice, fireplace)
  ratio_priceclass <- ratio_by(df, esp1, saleprice, priceclass)
  
  df <- df %>%
    dplyr::mutate(
      within10_1 = dplyr::if_else(ratio1 >= 0.9 & ratio1 <= 1.1, 1L, 0L),
      within20_1 = dplyr::if_else(ratio1 >= 0.8 & ratio1 <= 1.2, 1L, 0L),
      within50_1 = dplyr::if_else(ratio1 >= 0.5 & ratio1 <= 1.5, 1L, 0L)
    )
  
  N_total <- nrow(df)
  infl    <- stats::influence.measures(mod_base)
  cook    <- stats::cooks.distance(mod_base)
  stud    <- stats::rstudent(mod_base)
  
  rows_used <- as.integer(rownames(stats::model.frame(mod_base)))
  
  df <- df %>% dplyr::mutate(cook_1 = NA_real_, stud_1 = NA_real_) %>%
    dplyr::mutate(
      cook_1    = replace(cook_1, rows_used, cook),
      stud_1    = replace(stud_1, rows_used, stud),
      in_inlier = dplyr::if_else(cook_1 <= (4 / N_total) & abs(stud_1) <= 2, 1L, 0L)
    )
  
  df_in <- df %>% dplyr::filter(in_inlier == 1)
  
  rrmetrics_kable <- knitr::kable(
    rrmetrics, format = "latex", booktabs = TRUE,
    caption  = "LN\\_PRICE model (base) assessing coefficients"
  ) %>% kableExtra::kable_styling(latex_options = c("hold_position", "striped"),font_size = 8)
  
  list(
    data = df, data_inliers = df_in, rrmetrics = rrmetrics,
    rrmetrics_kable = rrmetrics_kable, ratio_ecf = ratio_ecf, ratio_style = ratio_style,
    ratio_cond = ratio_cond, ratio_size = ratio_size, ratio_era = ratio_era,
    ratio_fire = ratio_fire, ratio_priceclass = ratio_priceclass, influence = infl,
    cooks_distance = cook, studentized_residuals = stud, rows_used = rows_used, n_total = N_total
  )
}

# USING

res <- assess_ln_price_model(df, mod_base)

df         <- res$data
df_in      <- res$data_inliers
res$rrmetrics
res$rrmetrics_kable
res$ratio_ecf
res$ratio_style
res$ratio_cond
res$ratio_size
res$ratio_era
res$ratio_fire
res$ratio_priceclass

#

###

refit_inlier_ln_price <- function(df, df_in, formula_base, mod_base, p_out = 0.10, verbose = TRUE) {
  
  mod_base_inliers <- backward_p(
    formula = formula_base, data = df_in, p_out = p_out, verbose = verbose
  )
  
  df_in <- df_in %>%
    dplyr::mutate(
      pre_2      = stats::predict(mod_base_inliers, newdata = df_in),
      esp2       = exp(pre_2), ratio2 = esp2 / saleprice,
      within10_2 = dplyr::if_else(ratio2 >= 0.9 & ratio2 <= 1.1, 1L, 0L),
      within20_2 = dplyr::if_else(ratio2 >= 0.8 & ratio2 <= 1.2, 1L, 0L),
      within50_2 = dplyr::if_else(ratio2 >= 0.5 & ratio2 <= 1.5, 1L, 0L)
    )
  
  coef_names <- c("months_1to9", "months_10to16", "months_17to24")
  all_coefs  <- stats::coef(mod_base)
  
  time_coefs2 <- setNames(rep(NA_real_, length(coef_names)), coef_names)
  matched     <- intersect(coef_names, names(all_coefs))
  time_coefs2[matched] <- all_coefs[matched]
  
  RATE1_re <- unname(ifelse(is.na(time_coefs2["months_1to9"]), 0, time_coefs2["months_1to9"]))
  RATE2_re <- unname(ifelse(is.na(time_coefs2["months_10to16"]), 0, time_coefs2["months_10to16"]))
  RATE3_re <- unname(ifelse(is.na(time_coefs2["months_17to24"]), 0, time_coefs2["months_17to24"]))
  
  df_in <- df_in %>%
    dplyr::mutate(
      rate1_re = RATE1_re, rate2_re = RATE2_re, rate3_re = RATE3_re,
      price_index_re = (1 + RATE1_re)^months_1to9 * (1 + RATE2_re)^months_10to16 *
        (1 + RATE3_re)^months_17to24
    )
  
  end_month_in <- max(df_in$months, na.rm = TRUE)
  END_INDEX_re <- df_in %>% dplyr::filter(months == end_month_in) %>%
    dplyr::summarise(end_index = mean(price_index_re, na.rm = TRUE)) %>% dplyr::pull(end_index)
  
  df_in <- df_in %>%
    dplyr::mutate(
      taf_re    = END_INDEX_re / price_index_re, tasp_re = saleprice * taf_re,
      ln_tasp_re = log(tasp_re)
    )
  
  df_in1 <- df_in %>%
    dplyr::select(
      saleid, pre_2, esp2, ratio2, within10_2, within20_2, within50_2, rate1_re, rate2_re,
      rate3_re, price_index_re, taf_re, tasp_re, ln_tasp_re
    )
  
  dfa <- df %>% dplyr::left_join(df_in1, by = "saleid")
  
  list(
    model_inliers = mod_base_inliers, data_inliers  = df_in,
    join_data     = dfa, extract_data  = df_in1, time_coefs_re = time_coefs2,
    rates_re      = c(
      months_1to9   = RATE1_re, months_10to16 = RATE2_re, months_17to24 = RATE3_re
    ), end_month_in = end_month_in, end_index_re = END_INDEX_re
  )
}

# USAGES

res2 <- refit_inlier_ln_price(
  df = df, df_in = df_in, formula_base = formula_base, mod_base = mod_base
)

mod_base_inliers <- res2$model_inliers
df_in            <- res2$data_inliers
dfa              <- res2$join_data
df_in1           <- res2$extract_data

# notes

# all_coefs <- stats::coef(mod_base)
# all_coefs <- stats::coef(mod_base_inliers)

###

add_drive_distance_graphhopper <- function(
    df, lon_col, lat_col, target_lon, target_lat, profile = "car",
    dist_col = "drive_dist_km", time_col = "drive_time_min",
    server_url = "http://localhost:8989", sleep_sec = 0
) {
  stopifnot(is.data.frame(df))
  
  if (!lon_col %in% names(df)) {stop(sprintf("Missing longitude column: %s", lon_col))}
  if (!lat_col %in% names(df)) {stop(sprintf("Missing latitude column: %s", lat_col))}
  if (!is.numeric(target_lon) || length(target_lon) != 1 || is.na(target_lon)) {
    stop("target_lon must be a single non-missing numeric value.")
  }
  if (!is.numeric(target_lat) || length(target_lat) != 1 || is.na(target_lat)) {
    stop("target_lat must be a single non-missing numeric value.")
  }
  if (!is.character(profile) || length(profile) != 1 || is.na(profile)) {
    stop("profile must be a single non-missing character value.")
  }
  if (!is.character(server_url) || length(server_url) != 1 || is.na(server_url)) {
    stop("server_url must be a single non-missing character value.")
  }
  
  server_url <- sub("/+$", "", server_url)
  
  out <- df %>% dplyr::mutate(.row_id = seq_len(dplyr::n()))
  
  valid <- out %>% dplyr::filter(!is.na(.data[[lon_col]]), !is.na(.data[[lat_col]])) %>%
    dplyr::transmute(
      .row_id = .row_id, lon = as.numeric(.data[[lon_col]]), lat = as.numeric(.data[[lat_col]])
    )
  
  out[[dist_col]] <- NA_real_
  out[[time_col]] <- NA_real_
  
  if (nrow(valid) == 0) {return(out %>% dplyr::select(-.row_id))}
  
  route_one <- function(lon, lat) {
    req <- httr2::request(paste0(server_url, "/route")) %>%
      httr2::req_url_query(
        point = c(sprintf("%.8f,%.8f", lat, lon), sprintf("%.8f,%.8f", target_lat, target_lon)),
        profile = profile, instructions = "false", calc_points = "false",
        points_encoded = "false", .multi = "explode"
      ) %>% httr2::req_error(is_error = function(resp) FALSE)
    
    resp <- httr2::req_perform(req)
    status <- httr2::resp_status(resp)
    txt <- httr2::resp_body_string(resp)
    
    if (status >= 400) {
      warning(
        sprintf(
          "GraphHopper request failed for row point (%s, %s). HTTP %s. Returning NA.",
          lon, lat, status
        ), call. = FALSE
      )
      return(c(dist = NA_real_, time = NA_real_))
    }
    
    parsed <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
    
    if (!is.null(parsed$message)) {
      warning(
        sprintf(
          "GraphHopper returned an error for row point (%s, %s): %s. Returning NA.",
          lon, lat, parsed$message
        ), call. = FALSE
      )
      return(c(dist = NA_real_, time = NA_real_))
    }
    
    if (is.null(parsed$paths) || length(parsed$paths) == 0) {
      warning(
        sprintf("No route found for row point (%s, %s). Returning NA.", lon, lat), call. = FALSE
      )
      return(c(dist = NA_real_, time = NA_real_))
    }
    
    path1 <- parsed$paths[[1]]
    c(dist = as.numeric(path1$distance) / 1000, time = as.numeric(path1$time) / 60000
    )
  }
  
  res_list <- vector("list", nrow(valid))
  
  for (i in seq_len(nrow(valid))) {
    vals <- route_one(valid$lon[i], valid$lat[i])
    
    res_list[[i]] <- data.frame(
      .row_id = valid$.row_id[i], dist = unname(vals["dist"]),
      time = unname(vals["time"]), stringsAsFactors = FALSE
    )
    
    if (sleep_sec > 0 && i < nrow(valid)) {Sys.sleep(sleep_sec)}
  }
  
  lookup <- dplyr::bind_rows(res_list)
  
  out <- out %>% dplyr::left_join(lookup, by = ".row_id")
  
  out[[dist_col]] <- out$dist
  out[[time_col]] <- out$time
  
  out %>% dplyr::select(-.row_id, -dist, -time)
}

###

backward_pv2 <- function(
    formula, data, p_out = 0.10, verbose = TRUE,
    protect_terms = c("months_1to9", "months_10to16", "months_17to24")
) {
  mod <- lm(formula, data = data)
  
  repeat {
    coefs <- summary(mod)$coefficients
    if (nrow(coefs) <= 1) break
    
    pvals <- coefs[-1, 4]  # exclude intercept
    term_names <- names(pvals)
    
    removable_idx <- !(term_names %in% protect_terms)
    pvals_removable <- pvals[removable_idx]
    
    if (length(pvals_removable) == 0) {
      if (verbose) message("No removable terms remain; stopping.")
      break
    }
    
    worst_p <- max(pvals_removable, na.rm = TRUE)
    if (!is.finite(worst_p) || worst_p <= p_out) break
    
    worst_term <- names(which.max(pvals_removable))
    
    if (verbose) {message(sprintf("Dropping %-30s (p=%.4g)", worst_term, worst_p))}
    
    tt <- terms(mod)
    terms_now <- attr(tt, "term.labels")
    terms_new <- setdiff(terms_now, worst_term)
    
    if (length(terms_new) == 0) {
      mod <- lm(update(formula, . ~ 1), data = data)
      break
    }
    
    new_formula <- as.formula(paste(all.vars(formula)[1], "~", paste(terms_new, collapse = " + ")))
    mod <- lm(new_formula, data = data)
  }
  mod
}


###

make_time_adjusted_df_gam <- function(df, formula_time,
                                  p_out = 0.10,            # kept for backwards compat; ignored
                                  verbose = TRUE, method = "REML", family = gaussian(),
                                  months_col = "months", saleprice_col = "saleprice",
                                  floorarea_col = "sresb_floorarea",
                                  # variables whose *values* should vary by row (time vars)
                                  time_var_pattern = "^months",
                                  # term columns in predict(type="terms") to treat as "time"
                                  time_term_pattern = "months", end_index_fun = c("mean", "median")) {
  
  end_index_fun <- match.arg(end_index_fun)
  
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop("Package 'mgcv' is required. Install with install.packages('mgcv').")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required. Install with install.packages('dplyr').")
  }
  if (!months_col %in% names(df)) stop("months_col '", months_col, "' not found in df.")
  if (!saleprice_col %in% names(df)) stop("saleprice_col '", saleprice_col, "' not found in df.")
  if (!floorarea_col %in% names(df)) stop("floorarea_col '", floorarea_col, "' not found in df.")
  if (verbose) {
    message("Fitting GAM time model with mgcv::gam(...). (p_out is ignored for GAM)")
  }
  
  # 1) Fit GAM on complete cases for the formula
  mod_time <- mgcv::gam(formula = formula_time, data = df,
    method  = method, family  = family, na.action = na.omit
  )
  
  # 2) Build a prediction newdata that varies ONLY the time variables by row.
  #    All other predictors are held at a reference value from the training frame.
  mf_train <- model.frame(mod_time)
  resp_name <- names(mf_train)[1]
  pred_vars <- setdiff(names(mf_train), resp_name)
  # Time variables (by name) that should vary by row (months, months_1to9, etc.)
  time_vars <- pred_vars[grepl(time_var_pattern, pred_vars, ignore.case = TRUE)]
  if (length(time_vars) == 0) {
    stop("No predictor variables matched time_var_pattern = '", time_var_pattern,
         "'. Your formula must include a months* predictor.")
  }
  # Reference values for non-time predictors
  ref_value <- function(x) {
    if (is.numeric(x) || is.integer(x)) {
      stats::median(x, na.rm = TRUE)
    } else {
      # factor/character/logical -> most frequent level/value
      x2 <- x[!is.na(x)]
      if (length(x2) == 0) return(NA)
      ux <- unique(x2)
      ux[which.max(tabulate(match(x2, ux)))]
    }
  }
  
  ref <- lapply(pred_vars, function(v) ref_value(mf_train[[v]]))
  names(ref) <- pred_vars
  newdata <- as.data.frame(lapply(pred_vars, function(v) rep(ref[[v]], nrow(df))),
                           stringsAsFactors = FALSE)
  names(newdata) <- pred_vars
  # Put the actual row-wise time values into newdata
  for (v in time_vars) {
    if (!v %in% names(df)) {
      stop("Time variable '", v, "' is used in the GAM but not present in df.")
    }
    newdata[[v]] <- df[[v]]
  }
  
  # Align factor levels to training levels (prevents predict() issues)
  if (!is.null(mod_time$xlevels)) {
    for (nm in intersect(names(mod_time$xlevels), names(newdata))) {
      newdata[[nm]] <- factor(as.character(newdata[[nm]]), levels = mod_time$xlevels[[nm]])
    }
  }
  # 3) Get term contributions and isolate the time contribution(s)
  term_mat <- stats::predict(mod_time, newdata = newdata, type = "terms")
  term_names <- colnames(term_mat)
  
  time_term_idx <- grep(time_term_pattern, term_names, ignore.case = TRUE)
  if (length(time_term_idx) == 0) {
    stop("No term columns matched time_term_pattern = '", time_term_pattern, "'.\n",
         "Available term columns are:\n  ", paste(term_names, collapse = ", "))
  }
  
  time_lp <- rowSums(term_mat[, time_term_idx, drop = FALSE])
  # 4) Convert time contribution on link scale to a multiplicative index.
  #    This assumes your GAM response is log-price (e.g., ln_price or ln_sppsf).
  price_index <- exp(time_lp)
  end_month <- max(df[[months_col]], na.rm = TRUE)
  
  end_index <- switch(
    end_index_fun,
    mean   = mean(price_index[df[[months_col]] == end_month], na.rm = TRUE),
    median = stats::median(price_index[df[[months_col]] == end_month], na.rm = TRUE)
  )
  
  if (!is.finite(end_index)) {
    stop("end_index could not be computed (no rows at end_month or all NA).")
  }
  
  # 5) Apply TAF and compute TASP outputs
  df_out <- df %>%
    dplyr::mutate(price_index = price_index, taf = end_index / price_index,
      tasp = .data[[saleprice_col]] * taf, tasppsf = tasp / .data[[floorarea_col]],
      ln_tasp = log(tasp)
    )
  
  # (Optional) attach model + meta for debugging/reuse
  attr(df_out, "time_model") <- mod_time
  attr(df_out, "time_term_cols") <- term_names[time_term_idx]
  attr(df_out, "end_month") <- end_month
  attr(df_out, "end_index") <- end_index
  
  df_out
}

###

make_time_adjusted_df_xgb <- function(
    df,
    formula_time,
    params = list(
      objective = "reg:squarederror",
      eval_metric = "rmse",
      eta = 0.05,
      max_depth = 4,
      subsample = 0.8,
      colsample_bytree = 0.8
    ),
    nrounds = 500,
    verbose = 0,
    reference = c("mean", "median_row")
) {
  reference <- match.arg(reference)
  
  needed_cols <- c("months", "months_1to9", "months_10to16", "months_17to24",
                   "saleprice", "sresb_floorarea")
  missing_needed <- setdiff(needed_cols, names(df))
  if (length(missing_needed) > 0) {
    stop("Missing required column(s): ", paste(missing_needed, collapse = ", "))
  }
  
  # Build model frame and matrix from the formula
  mf <- stats::model.frame(formula_time, data = df, na.action = stats::na.omit)
  cat("Original rows: ", nrow(df), "\n")
  cat("Rows used in model.frame: ", nrow(mf), "\n")
  cat("Rows dropped: ", nrow(df) - nrow(mf), "\n")
  y_time <- stats::model.response(mf)
  X_time <- stats::model.matrix(formula_time, data = mf)
  
  # Drop intercept for xgboost
  if ("(Intercept)" %in% colnames(X_time)) {
    X_time <- X_time[, colnames(X_time) != "(Intercept)", drop = FALSE]
  }
  
  month_terms <- c("months_1to9", "months_10to16", "months_17to24")
  missing_month_terms <- setdiff(month_terms, colnames(X_time))
  if (length(missing_month_terms) > 0) {
    stop("These month terms are not in the xgb design matrix: ",
         paste(missing_month_terms, collapse = ", "))
  }
  
  dtime <- xgboost::xgb.DMatrix(data = X_time, label = y_time)
  
  mod_time_xgb <- xgboost::xgb.train(
    params = params,
    data = dtime,
    nrounds = nrounds,
    verbose = verbose
  )
  
  # Build a reference predictor profile
  ref_x <- switch(
    reference,
    mean = colMeans(X_time, na.rm = TRUE),
    median_row = {
      med_month <- stats::median(df$months, na.rm = TRUE)
      idx <- which.min(abs(df$months - med_month))
      as.numeric(X_time[idx, ])
    }
  )
  names(ref_x) <- colnames(X_time)
  
  # Create a month grid and piecewise month variables
  month_grid <- data.frame(months = 0:max(df$months, na.rm = TRUE))
  
  month_grid$months_1to9 <- pmin(month_grid$months, 9)
  month_grid$months_10to16 <- pmin(pmax(month_grid$months - 9, 0), 7)
  month_grid$months_17to24 <- pmin(pmax(month_grid$months - 16, 0), 8)
  
  # Repeat reference profile and overwrite only the month variables
  X_grid <- matrix(
    rep(ref_x, each = nrow(month_grid)),
    nrow = nrow(month_grid),
    byrow = FALSE
  )
  colnames(X_grid) <- names(ref_x)
  
  X_grid[, "months_1to9"] <- month_grid$months_1to9
  X_grid[, "months_10to16"] <- month_grid$months_10to16
  X_grid[, "months_17to24"] <- month_grid$months_17to24
  
  dpred <- xgboost::xgb.DMatrix(data = X_grid)
  month_grid$pred_xgb <- as.numeric(stats::predict(mod_time_xgb, newdata = dpred))
  
  # Recover implied piecewise-linear month rates from xgb predictions
  aux_mod <- stats::lm(
    pred_xgb ~ months_1to9 + months_10to16 + months_17to24,
    data = month_grid
  )
  
  all_coefs <- stats::coef(aux_mod)
  
  time_coefs <- setNames(rep(NA_real_, length(month_terms)), month_terms)
  matched <- intersect(month_terms, names(all_coefs))
  time_coefs[matched] <- all_coefs[matched]
  
  rate1 <- unname(ifelse(is.na(time_coefs["months_1to9"]), 0, time_coefs["months_1to9"]))
  rate2 <- unname(ifelse(is.na(time_coefs["months_10to16"]), 0, time_coefs["months_10to16"]))
  rate3 <- unname(ifelse(is.na(time_coefs["months_17to24"]), 0, time_coefs["months_17to24"]))
  
  df_out <- df %>%
    dplyr::mutate(
      rate1 = rate1,
      rate2 = rate2,
      rate3 = rate3,
      price_index = (1 + rate1)^months_1to9 *
        (1 + rate2)^months_10to16 *
        (1 + rate3)^months_17to24
    )
  
  end_month <- max(df_out$months, na.rm = TRUE)
  
  end_index <- df_out %>%
    dplyr::filter(months == end_month) %>%
    dplyr::summarise(end_index = mean(price_index, na.rm = TRUE)) %>%
    dplyr::pull(end_index)
  
  df_out <- df_out %>%
    dplyr::mutate(
      taf = end_index / price_index,
      tasp = saleprice * taf,
      tasppsf = tasp / sresb_floorarea,
      ln_tasp = log(tasp)
    )
  
  attr(df_out, "mod_time_xgb") <- mod_time_xgb
  attr(df_out, "aux_time_lm") <- aux_mod
  attr(df_out, "time_coefs") <- time_coefs
  attr(df_out, "month_grid") <- month_grid
  
  df_out
}

###

add_spearman_kakwani <- function(df, x_col, y_col,
                                 spearman_col = "spearman_rho",
                                 kakwani_col = "kakwani_index",
                                 drop_na = TRUE) {
  stopifnot(is.data.frame(df))
  
  if (!x_col %in% names(df)) stop(sprintf("Missing x_col: %s", x_col))
  if (!y_col %in% names(df)) stop(sprintf("Missing y_col: %s", y_col))
  
  x <- df[[x_col]]
  y <- df[[y_col]]
  
  if (!is.numeric(x)) stop(sprintf("%s must be numeric.", x_col))
  if (!is.numeric(y)) stop(sprintf("%s must be numeric.", y_col))
  
  work <- data.frame(
    x = x,
    y = y,
    stringsAsFactors = FALSE
  )
  
  if (drop_na) {
    work <- work[stats::complete.cases(work), , drop = FALSE]
  }
  
  if (nrow(work) == 0) {
    stop("No complete cases available after NA handling.")
  }
  
  # Spearman correlation
  spearman_rho <- suppressWarnings(
    stats::cor(work$x, work$y, method = "spearman", use = "complete.obs")
  )
  
  # Helper: Gini coefficient
  gini_coefficient <- function(v) {
    v <- as.numeric(v)
    if (any(is.na(v))) stop("NA values are not allowed inside gini_coefficient().")
    if (any(v < 0)) stop("Gini coefficient here requires non-negative values.")
    if (sum(v) == 0) return(0)
    
    v <- sort(v)
    n <- length(v)
    idx <- seq_len(n)
    
    (2 * sum(idx * v) / (n * sum(v))) - (n + 1) / n
  }
  
  # Helper: concentration index of y ranked by x
  concentration_index <- function(rank_var, outcome_var) {
    rank_var <- as.numeric(rank_var)
    outcome_var <- as.numeric(outcome_var)
    
    if (any(is.na(rank_var)) || any(is.na(outcome_var))) {
      stop("NA values are not allowed inside concentration_index().")
    }
    if (any(rank_var < 0)) {
      stop("Rank variable must be non-negative for this implementation.")
    }
    if (any(outcome_var < 0)) {
      stop("Outcome variable must be non-negative for this implementation.")
    }
    if (sum(outcome_var) == 0) return(0)
    
    ord <- order(rank_var, outcome_var)
    y_sorted <- outcome_var[ord]
    
    n <- length(y_sorted)
    frac_rank <- (seq_len(n) - 0.5) / n
    mu_y <- mean(y_sorted)
    
    2 * stats::cov(y_sorted, frac_rank) / mu_y
  }
  
  gx <- gini_coefficient(work$x)
  cy <- concentration_index(work$x, work$y)
  kakwani_index <- cy - gx
  
  df[[spearman_col]] <- spearman_rho
  df[[kakwani_col]] <- kakwani_index
  
  attr(df, "spearman_rho") <- spearman_rho
  attr(df, "gini_x") <- gx
  attr(df, "concentration_y_ranked_by_x") <- cy
  attr(df, "kakwani_index") <- kakwani_index
  
  df
}

# USAGE
# df <- add_spearman_kakwani(df, x_col = "saleprice", y_col = "tasp")

###

spss_ratio_metrics <- function(df, estimate, saleprice) {
  estimate  <- rlang::enquo(estimate)
  saleprice <- rlang::enquo(saleprice)
  
  d <- df %>%
    dplyr::transmute(est = !!estimate, sp = !!saleprice) %>%
    dplyr::filter(is.finite(est), is.finite(sp), est > 0, sp > 0)
  
  if (nrow(d) == 0) {
    return(tibble::tibble(
      n = 0L, median_ratio = NA_real_, mean_ratio = NA_real_,
      wmean_ratio = NA_real_, PRD = NA_real_, COD = NA_real_, PRB = NA_real_,
      within_10_n = 0L, within_10_pct = NA_real_, within_20_n = 0L, within_20_pct = NA_real_,
      within_50_n = 0L, within_50_pct = NA_real_
    ))
  }
  
  ratio   <- d$est / d$sp
  med     <- stats::median(ratio, na.rm = TRUE)
  mean_r  <- mean(ratio, na.rm = TRUE)
  wmean_r <- sum(d$est, na.rm = TRUE) / sum(d$sp, na.rm = TRUE)
  COD <- (mean(abs(ratio - med), na.rm = TRUE) / med) * 100
  PRD <- mean_r / wmean_r
  # Within-band metrics (SPSS concentration index: within % of median)
  ok_ratio <- is.finite(ratio)
  r_ok     <- ratio[ok_ratio]
  n_ok     <- length(r_ok)
  
  within_med_n <- function(pct) {
    lo <- (1 - pct/100) * med
    hi <- (1 + pct/100) * med
    sum(r_ok >= lo & r_ok <= hi, na.rm = TRUE)
  }
  
  w10_n <- within_med_n(10)
  w20_n <- within_med_n(20)
  w50_n <- within_med_n(50)
  w10_pct <- if (n_ok > 0) 100 * w10_n / n_ok else NA_real_
  w20_pct <- if (n_ok > 0) 100 * w20_n / n_ok else NA_real_
  w50_pct <- if (n_ok > 0) 100 * w50_n / n_ok else NA_real_
  
  y <- ((ratio - med) / med) * 100    # PRB per SPSS definition:
  value_proxy <- 0.5 * d$sp + 0.5 * (d$est / med)
  ok <- is.finite(y) & is.finite(value_proxy) & value_proxy > 0
  
  prb_fit <- try(stats::lm(y[ok] ~ log2(value_proxy[ok])), silent = TRUE)
  PRB <- if (inherits(prb_fit, "try-error") || sum(ok) < 3) NA_real_ else unname(coef(prb_fit)[2])
  
  tibble::tibble(
    n = nrow(d), median_ratio = med, mean_ratio = mean_r, wmean_ratio = wmean_r,
    PRD = PRD, COD = COD / 100, PRB = PRB / 100, within_10_pct = w10_pct,
    within_20_pct = w20_pct, within_50_pct = w50_pct
  )
}

###

make_ecf_dummies <- function(data, base_ecf = "1r135") {
  if (!"secf" %in% names(data)) {
    stop("Column 'secf' not found in data.")
  }
  
  # force secf values to lowercase character
  data$secf <- tolower(as.character(data$secf))
  
  ecf_levels <- sprintf("1r%03d", 101:137)
  ecf_levels <- setdiff(ecf_levels, tolower(base_ecf))
  
  for (lvl in ecf_levels) {
    nm <- paste0("ecf", lvl)   # e.g. ecf1r101
    data[[nm]] <- ifelse(data$secf == lvl, 1L, 0L)
  }
  
  data
}


###



###



###



###



###




###



###



###



###






































