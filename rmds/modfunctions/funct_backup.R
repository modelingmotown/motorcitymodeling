# Backup function information







###


# Move one column to the front
df <- df %>% relocate(col_name)

# Move one column after another
df <- df %>% relocate(col_to_move, .after = other_col)

# Move one column before another
df <- df %>% relocate(col_to_move, .before = other_col)

# Swap two columns by relocating one relative to the other
df <- df %>% relocate(A, .after = B)   # puts A immediately after B (effectively swaps if adjacent)

#

make_vif_compare_kable3(mod_base, slpedf, output = "latex", join = "full", dedupe = "error",
                        caption = "R vs SPSS Collinearity Metrics (LN\\_PRICE+ECFs)")

make_vif_compare_kable4(mod_base, slpedf, output = "latex", join = "full", dedupe = "error",
                        caption = "R vs SPSS Collinearity Metrics (LN\\_PRICE+ECFs)")

#

#save(backward_p, ratio_metrics, ratio_by, get_time_params, model_metrics_A, coef_table,
#     pp_plot_resid, plot_stdresid_vs_stdpred, plot_cook_vs_rstudent, plot_lev_vs_rstudent,
#     safe_vif, safe_vif_kable, ratio_stats_single, ratio_stats_by, within_band_flags,
#     lm_spss_comparer, vif_compare_kable, file = here("rmds", "modfunctions3.RData"))

#load(file = here("rmds", "modfunctions2.RData"))

###

# __________

prep_sales_data <- function(
    df,    # --- column names (override if yours differ) ---
    saleid_col      = "saleid",
    saledate_col    = "saledate",
    saleprice_col   = "saleprice",
    ecf_col         = "ecf",
    garagespaces_col= "garagespaces",
    gartype_col     = "sresb_gartype",   # will auto-fallback to "sreb_gartype" if present
    totalsqft_col   = "stotalsqft",
    floorarea_col   = "sresb_floorarea",
    groundarea_col  = "sresb_groundarea",
    crawspace_col   = "sresb_crawspace",
    slabarea_col    = "sresb_slabarea",
    
    # --- time indexing ---
    origin_month    = NULL,              # e.g., as.Date("2023-04-01"); if NULL uses min(saledate) floored to month
    make_month_pieces = TRUE,
    month_breaks    = c(9, 16),           # creates months_1to9, months_10to16, months_17plus by default
    
    # --- filtering / flags ---
    min_saleprice = 10000, min_totalsqft = 400, min_floorarea = 400,
    mark_bad_rows = TRUE,  drop_bad_rows = FALSE,
    
    # --- ECF handling ---
    ecf_mode        = c("factor_other", "factor", "dummies"),
    ecf_levels      = NULL,              # pass training levels here when prepping prediction data
    ecf_min_n       = 0,                 # if >0, levels with count < ecf_min_n become OTHER
    ecf_other_label = "OTHER",    normalize_ecf   = TRUE,
    
    # --- saving ---
    save_rdata_path = NULL,              # e.g. "COD_sales_prepped.RData"
    save_object_name= "df_prepped",
    
    verbose         = TRUE
) {
  ecf_mode <- match.arg(ecf_mode)
  has_stringr <- requireNamespace("stringr", quietly = TRUE)
  has_fastd   <- requireNamespace("fastDummies", quietly = TRUE)
  msg <- function(...) if (isTRUE(verbose)) message(...)
  
  # ---- make a copy ----
  df <- as.data.frame(df)
  
  # ---- normalize names to lower-case if you tend to mix SALEDATE vs saledate ----
  # (This is optional but helps a ton when CSVs vary.)
  # If you *must* preserve original casing, remove the next line.
  names(df) <- tolower(names(df))
  
  # ---- helper: choose first existing column from candidates ----
  pick_col <- function(cands) {
    cands <- tolower(cands)
    hit <- cands[cands %in% names(df)]
    if (length(hit) == 0) return(NA_character_)
    hit[1]
  }
  
  # Resolve columns (allow for common alternates)
  saleid_col       <- pick_col(c(saleid_col, "sale_id", "saleid"))
  saledate_col     <- pick_col(c(saledate_col, "saledate", "sale_date", "date", "saledt"))
  saleprice_col    <- pick_col(c(saleprice_col, "saleprice", "sale_price", "price"))
  ecf_col          <- pick_col(c(ecf_col, "ecf"))
  garagespaces_col <- pick_col(c(garagespaces_col, "garagespaces", "sresb_garagespaces", "ln_garagespaces")) # if only ln exists, we'll handle later
  totalsqft_col    <- pick_col(c(totalsqft_col, "stotalsqft", "total_sqft", "totalsqft"))
  floorarea_col    <- pick_col(c(floorarea_col, "sresb_floorarea", "floorarea"))
  groundarea_col   <- pick_col(c(groundarea_col, "sresb_groundarea", "groundarea"))
  crawspace_col    <- pick_col(c(crawspace_col, "sresb_crawspace", "crawspace"))
  slabarea_col     <- pick_col(c(slabarea_col, "sresb_slabarea", "slabarea"))
  
  # garage type typo fallback
  gartype_col_try <- pick_col(c(gartype_col, "sresb_gartype", "sreb_gartype"))
  gartype_col <- gartype_col_try
  
  required <- c(saleid_col, saledate_col, saleprice_col)
  required <- required[!is.na(required)]
  missing_required <- setdiff(required, names(df))
  if (length(missing_required) > 0) {
    stop("Missing required columns: ", paste(missing_required, collapse = ", "))
  }
  
  # ---- parse saledate robustly ----
  # Handles ISO strings like "2025-03-07T00:00:00Z"
  sd <- df[[saledate_col]]
  if (inherits(sd, "Date")) {
    sd_date <- sd
  } else if (inherits(sd, "POSIXt")) {
    sd_date <- as.Date(sd)
  } else {
    # attempt ymd_hms first; fallback to ymd
    sd1 <- suppressWarnings(lubridate::ymd_hms(sd, tz = "UTC", quiet = TRUE))
    if (all(is.na(sd1))) {
      sd2 <- suppressWarnings(lubridate::ymd(sd, quiet = TRUE))
      sd_date <- sd2
    } else {sd_date <- as.Date(sd1)}
  }
  
  df$saledate_parsed <- sd_date
  
  # ---- origin month and months index ----
  df$sale_month <- lubridate::floor_date(df$saledate_parsed, unit = "month")
  
  if (is.null(origin_month)) {
    origin_month <- min(df$sale_month, na.rm = TRUE)
  } else {origin_month <- lubridate::floor_date(as.Date(origin_month), "month")}
  
  # integer months since origin
  df$months <- as.integer(lubridate::interval(origin_month, df$sale_month) %/% lubridate::months(1))
  df$months <- pmax(df$months, 0L)
  
  # optional piecewise month terms
  if (isTRUE(make_month_pieces)) {
    b1 <- month_breaks[1]
    b2 <- month_breaks[2]
    if (!is.finite(b1) || !is.finite(b2) || b2 <= b1) stop("month_breaks must be increasing, e.g. c(9,16)")
    
    df$months_1to9    <- pmin(df$months, b1)
    df$months_10to16  <- pmin(pmax(df$months - b1, 0L), b2 - b1)
    df$months_17plus  <- pmax(df$months - b2, 0L)
  }
  
  # ---- garagespaces + ln_garagespaces ----
  # Prefer raw garagespaces if present; if only ln_garagespaces exists, keep it but sanitize.
  if (!is.na(garagespaces_col) && garagespaces_col %in% names(df) && garagespaces_col != "ln_garagespaces") {
    df$garagespaces <- suppressWarnings(as.numeric(df[[garagespaces_col]]))
    df$garagespaces[!is.finite(df$garagespaces)] <- NA_real_
    df$garagespaces <- pmax(df$garagespaces, 0)
    df$ln_garagespaces <- log1p(df$garagespaces)  # key fix
  } else if ("ln_garagespaces" %in% names(df)) {
    df$ln_garagespaces <- suppressWarnings(as.numeric(df$ln_garagespaces))
    # sanitize non-finite (still better to refit if this was created with log())
    df$ln_garagespaces[!is.finite(df$ln_garagespaces)] <- NA_real_
  } else {
    df$garagespaces <- NA_real_
    df$ln_garagespaces <- NA_real_
  }
  
  # ---- garage type dummies (if available) ----
  if (!is.na(gartype_col) && gartype_col %in% names(df)) {
    gt <- suppressWarnings(as.numeric(df[[gartype_col]]))
    df$one_car_garage <- dplyr::if_else(gt == 1, 1L, 0L, missing = 0L)
    df$two_car_garage <- dplyr::if_else(gt == 2, 1L, 0L, missing = 0L)
    df$three_car_garage <- dplyr::if_else(gt == 3, 1L, 0L, missing = 0L)
  }
  
  # ---- guarded ratios (avoid Inf/NaN) ----
  safe_ratio <- function(num, den) {
    out <- rep(NA_real_, length(num))
    ok <- is.finite(num) & is.finite(den) & den > 0
    out[ok] <- num[ok] / den[ok]
    out
  }
  
  if (!is.na(groundarea_col) && groundarea_col %in% names(df)) {
    ga <- suppressWarnings(as.numeric(df[[groundarea_col]]))
    cs <- if (!is.na(crawspace_col) && crawspace_col %in% names(df)) suppressWarnings(as.numeric(df[[crawspace_col]])) else NA_real_
    sa <- if (!is.na(slabarea_col) && slabarea_col %in% names(df)) suppressWarnings(as.numeric(df[[slabarea_col]])) else NA_real_
    
    df$pctcrawl <- safe_ratio(cs, ga)
    df$pctslab  <- safe_ratio(sa, ga)
  }
  
  # ---- “bad row” flag ----
  if (isTRUE(mark_bad_rows)) {
    sp <- suppressWarnings(as.numeric(df[[saleprice_col]]))
    
    ts <- if (!is.na(totalsqft_col) && totalsqft_col %in% names(df))
      suppressWarnings(as.numeric(df[[totalsqft_col]])) else NA_real_
    
    fa <- if (!is.na(floorarea_col) && floorarea_col %in% names(df))
      suppressWarnings(as.numeric(df[[floorarea_col]])) else NA_real_
    
    df$out_bad <- dplyr::case_when(
      is.na(df$saledate_parsed) | is.na(sp) ~ 1L,      !is.na(ts) & ts < min_totalsqft ~ 1L,
      !is.na(fa) & fa < min_floorarea ~ 1L,  sp < min_saleprice ~ 1L,      TRUE ~ 0L
    )
    
    if (isTRUE(drop_bad_rows)) {
      before <- nrow(df)
      df <- df[df$out_bad == 0L, , drop = FALSE]
      msg("Dropped bad rows: ", before - nrow(df))
    }
  }
  
  # ---- ECF handling ----
  if (!is.na(ecf_col) && ecf_col %in% names(df)) {
    ecf_raw <- df[[ecf_col]]
    if (isTRUE(normalize_ecf)) {
      if (has_stringr) {
        ecf_norm <- stringr::str_squish(toupper(as.character(ecf_raw)))
      } else {ecf_norm <- toupper(trimws(as.character(ecf_raw)))}
    } else {ecf_norm <- as.character(ecf_raw)}
    
    ecf_norm[is.na(ecf_norm) | ecf_norm == ""] <- ecf_other_label
    
    # training-time: optionally collapse rare levels to OTHER
    if (is.null(ecf_levels)) {
      if (ecf_min_n > 0) {
        tab <- table(ecf_norm)
        rare <- names(tab)[tab < ecf_min_n]
        ecf_norm[ecf_norm %in% rare] <- ecf_other_label
      }
      ecf_levels <- sort(unique(c(ecf_norm, ecf_other_label)))
    } else {      # prediction-time: map unknown to OTHER, and use training levels
      ecf_levels <- unique(c(ecf_levels, ecf_other_label))
      ecf_norm[!ecf_norm %in% ecf_levels] <- ecf_other_label
    }
    
    if (ecf_mode %in% c("factor_other", "factor")) {
      df[[ecf_col]] <- factor(ecf_norm, levels = ecf_levels)
    } else if (ecf_mode == "dummies") {
      if (!has_fastd) stop("ecf_mode='dummies' requires fastDummies package.")
      df[[ecf_col]] <- factor(ecf_norm, levels = ecf_levels)
      
      # Create dummies; remove the original ecf column to prevent redundancy unless you want both
      df <- fastDummies::dummy_cols(df, select_columns = ecf_col,
                                    remove_selected_columns = TRUE, remove_first_dummy = FALSE)
    }
  } else {msg("Note: ecf column not found; skipping ECF engineering.")}
  
  # ---- small cleanup / attributes ----
  attr(df, "origin_month") <- origin_month
  attr(df, "ecf_levels") <- ecf_levels
  
  # ---- optional save ----
  if (!is.null(save_rdata_path)) {
    obj <- df
    meta <- list(origin_month = origin_month, ecf_levels = ecf_levels)
    assign(save_object_name, obj)    # save with requested object name
    save(list = c(save_object_name, "meta"), file = save_rdata_path)
    msg("Saved: ", save_rdata_path, " (objects: ", save_object_name, ", meta)")
  }
  
  list(     # return both df + metadata cleanly
    data = df,
    meta = list(
      origin_month = origin_month, ecf_levels = ecf_levels,
      ecf_mode = ecf_mode, month_breaks = month_breaks
    )
  )
}

### How to use it (training)
res <- prep_sales_data(
  df = dfa,
  origin_month = as.Date("2023-04-01"),
  ecf_mode = "factor_other",
  ecf_min_n = 20,          # optional: rare neighborhoods -> OTHER
  drop_bad_rows = TRUE
)

df_prepped <- res$data
ecf_levels <- res$meta$ecf_levels

### How to use it later (prediction / new rows)
res2 <- prep_sales_data(
  df = new_sales_df,
  origin_month = res$meta$origin_month,
  ecf_levels = ecf_levels,
  ecf_mode = "factor_other",
  drop_bad_rows = FALSE
)

new_prepped <- res2$data

# __________




