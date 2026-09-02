parse_sale_date <- function(x) {
  if (inherits(x, c("Date", "POSIXct", "POSIXt"))) return(as.POSIXct(x))
  parse_date_time(x,
                  orders = c(
                    "ymd", "mdy", "dmy", "ymd HMS", "mdy HMS", "dmy HMS",
                    "ymd HM", "mdy HM", "dmy HM", "m/d/Y", "m/d/Y H:M:S", "Y-m-d H:M:S"
                  ), quiet = TRUE
  )
}

safe_log <- function(x) {ifelse(is.na(x) | !is.finite(x) | x <= 0, NA_real_, log(x))}

detect_ecf_col <- function(data) {
  candidates <- c("ecf", "secf", "ecfcode", "ecf_code", "neighborhood_area")
  hit <- candidates[candidates %in% names(data)][1]
  if (length(hit) == 0 || is.na(hit)) {
    ecf_like <- names(data)[grepl("ecf", names(data), ignore.case = TRUE)]
    stop(
      "No ECF column found. Checked: ", paste(candidates, collapse = ", "),
      if (length(ecf_like) > 0) paste0(". Columns containing 'ecf': ", paste(ecf_like, collapse = ", ")) else ""
    )
  }
  hit
}

weighted_mean_ratio <- function(pred, actual) {sum(pred, na.rm = TRUE) / sum(actual, na.rm = TRUE)}

mean_ratio <- function(pred, actual) {mean(pred / actual, na.rm = TRUE)}

calc_prd <- function(pred, actual) {
  mr <- mean_ratio(pred, actual)
  wr <- weighted_mean_ratio(pred, actual)
  mr / wr
}

calc_cod <- function(pred, actual) {
  ratio <- pred / actual
  med <- median(ratio, na.rm = TRUE)
  100 * mean(abs(ratio - med), na.rm = TRUE) / med
}

calc_prb <- function(pred, actual) {
  ratio <- pred / actual
  valid <- is.finite(pred) & is.finite(actual) & is.finite(ratio) & pred > 0 & actual > 0
  pred <- pred[valid]
  actual <- actual[valid]
  ratio <- ratio[valid]
  if (length(ratio) < 3) return(NA_real_)
  x <- log(actual)
  fit <- lm(ratio ~ x)
  unname(coef(fit)[2])
}

ratio_stats <- function(pred, actual) {
  ratio <- pred / actual
  tibble(
    n = sum(is.finite(ratio)), median = median(ratio, na.rm = TRUE),
    mean = mean(ratio, na.rm = TRUE), wgtmean = weighted_mean_ratio(pred, actual),
    min = min(ratio, na.rm = TRUE), max = max(ratio, na.rm = TRUE),
    prd = calc_prd(pred, actual), prb = calc_prb(pred, actual), cod = calc_cod(pred, actual)
  )
}

ratio_stats_by <- function(data, pred_col, actual_col, by) {
  pred_col <- rlang::ensym(pred_col)
  actual_col <- rlang::ensym(actual_col)
  by <- rlang::ensym(by)
  data %>% group_by(!!by) %>%
    group_modify(~ ratio_stats(.x[[rlang::as_string(pred_col)]], .x[[rlang::as_string(actual_col)]])) %>% ungroup()
}

within_band_metrics <- function(ratio) {
  tibble(
    within10 = mean(ratio <= 1.1 & ratio >= 0.9, na.rm = TRUE),
    within20 = mean(ratio <= 1.2 & ratio >= 0.8, na.rm = TRUE),
    within50 = mean(ratio <= 1.5 & ratio >= 0.5, na.rm = TRUE)
  )
}

backward_pkm <- function(formula, data, p_out = 0.10, verbose = TRUE) {
  mod <- lm(formula, data = data)  # Terms to always keep so their coefficients remain visible
  keep_terms <- c("months_1to9", "months_19to24")
  repeat {
    s <- summary(mod)$coefficients
    if (nrow(s) <= 1) break    # Drop intercept and protected terms from elimination
    candidate_terms <- setdiff(rownames(s), c("(Intercept)", keep_terms))
    pvals <- s[candidate_terms, 4, drop = TRUE]
    if (length(pvals) == 0 || all(is.na(pvals))) break
    worst_term <- names(which.max(pvals))
    worst_p <- max(pvals, na.rm = TRUE)
    if (!is.finite(worst_p) || worst_p <= p_out) break
    if (verbose) {message(sprintf("Dropping term: %s (p = %.4f)", worst_term, worst_p))}
    terms_now <- attr(terms(mod), "term.labels")
    terms_new <- setdiff(terms_now, worst_term)
    if (length(terms_new) == 0) break
    mod <- lm(reformulate(terms_new, response = all.vars(formula)[1]), data = data)
  }
  mod
}

backward_p <- function(formula, data, p_out = 0.10, verbose = TRUE) {
  mod <- lm(formula, data = data)
  repeat {
    s <- summary(mod)$coefficients
    if (nrow(s) <= 1) break    # Drop intercept from elimination
    pvals <- s[setdiff(rownames(s), "(Intercept)"), 4, drop = TRUE]
    if (length(pvals) == 0 || all(is.na(pvals))) break
    worst_term <- names(which.max(pvals))
    worst_p <- max(pvals, na.rm = TRUE)
    if (!is.finite(worst_p) || worst_p <= p_out) break
    if (verbose) {message(sprintf("Dropping term: %s (p = %.4f)", worst_term, worst_p))}
    terms_now <- attr(terms(mod), "term.labels")
    terms_new <- setdiff(terms_now, worst_term)
    if (length(terms_new) == 0) break
    mod <- lm(reformulate(terms_new, response = all.vars(formula)[1]), data = data)
  }
  mod
}

backward_pss <- function(formula, data, p_out = 0.10, verbose = TRUE,
                         keep_terms = character(0)) { # c("months_1to9", "months_19to24")
  # Freeze the case set based on the FULL starting model
  mf <- model.frame(formula, data = data, na.action = na.omit)
  response <- names(mf)[1]
  terms_now <- attr(terms(formula), "term.labels")
  mod <- lm(reformulate(terms_now, response = response), data = mf)
  repeat {
    s <- summary(mod)$coefficients
    if (nrow(s) <= 1) break
    candidate_terms <- setdiff(rownames(s), c("(Intercept)", keep_terms))
    if (length(candidate_terms) == 0) break
    pvals <- s[candidate_terms, 4, drop = TRUE]
    pvals <- pvals[is.finite(pvals)]
    if (length(pvals) == 0) break
    worst_term <- names(which.max(pvals))
    worst_p <- max(pvals)
    if (worst_p <= p_out) break
    if (verbose) {message(sprintf("Dropping term: %s (p = %.4f)", worst_term, worst_p))}
    terms_now <- setdiff(terms_now, worst_term)
    if (length(terms_now) == 0) break
    mod <- lm(reformulate(terms_now, response = response), data = mf)
  }
  mod
}

model_summary_tbl <- function(mod) {
  s <- summary(mod)
  tibble(
    r_squared = s$r.squared, adj_r_squared = s$adj.r.squared, residual_se = sigma(mod),
    f_statistic = unname(s$fstatistic[1]), df1 = unname(s$fstatistic[2]),
    df2 = unname(s$fstatistic[3]),
    model_p = pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3], lower.tail = FALSE)
  )
}

add_influence_outputs <- function(data, mod, run_id = 1) {
  pred_name <- paste0("pre_", run_id)
  cook_name <- paste0("coo_", run_id)
  sdr_name  <- paste0("sdr_", run_id)
  zres_name <- paste0("zre_", run_id)
  lev_name  <- paste0("lev_", run_id)
  mah_name  <- paste0("mah_", run_id)
  data[[pred_name]] <- predict(mod, newdata = data)
  data[[cook_name]] <- cooks.distance(mod)
  data[[sdr_name]]  <- rstudent(mod)
  data[[zres_name]] <- rstandard(mod)
  data[[lev_name]]  <- hatvalues(mod)
  X <- model.matrix(mod)
  center <- colMeans(X, na.rm = TRUE)
  cov_x <- cov(X, use = "pairwise.complete.obs")
  inv_cov_x <- tryCatch(solve(cov_x), error = function(e) NULL)
  
  if (!is.null(inv_cov_x)) {data[[mah_name]] <- mahalanobis(X, center = center, cov = cov_x)
  } else {data[[mah_name]] <- NA_real_}
  data
}

add_influence_metrics <- function(data, mod, run_id = 1) {
  pred_name <- paste0("pre_", run_id)
  cook_name <- paste0("coo_", run_id)
  sdr_name  <- paste0("sdr_", run_id)
  zres_name <- paste0("zre_", run_id)
  lev_name  <- paste0("lev_", run_id)
  mah_name  <- paste0("mah_", run_id)
  mf <- stats::model.frame(mod)
  rows_used <- as.integer(rownames(mf))
  data[[pred_name]] <- NA_real_
  data[[cook_name]] <- NA_real_
  data[[sdr_name]]  <- NA_real_
  data[[zres_name]] <- NA_real_
  data[[lev_name]]  <- NA_real_
  data[[mah_name]]  <- NA_real_
  
  data[[pred_name]][rows_used] <- as.numeric(stats::predict(mod))
  data[[cook_name]][rows_used] <- as.numeric(stats::cooks.distance(mod))
  data[[sdr_name]][rows_used]  <- as.numeric(stats::rstudent(mod))
  data[[zres_name]][rows_used] <- as.numeric(stats::rstandard(mod))
  data[[lev_name]][rows_used]  <- as.numeric(stats::hatvalues(mod))
  
  X <- stats::model.matrix(mod)
  center <- colMeans(X, na.rm = TRUE)
  cov_x <- stats::cov(X, use = "pairwise.complete.obs")
  inv_cov_x <- tryCatch(solve(cov_x), error = function(e) NULL)
  
  if (!is.null(inv_cov_x)) {
    data[[mah_name]][rows_used] <- as.numeric(stats::mahalanobis(X, center = center, cov = cov_x))
  }
  data
}

make_ecf_dummies <- function(data) {
  data$ecf <- tolower(trimws(as.character(data$ecf)))
  ecf_map <- list(
    ecf6r601 = c("6r601"), ecf6r602 = c("6r602"), ecf6r603 = c("6r603"), ecf6r604 = c("6r604"),
    ecf6r605 = c("6r605"), ecf6r606 = c("6r606"), ecf6r607 = c("6r607"), ecf6r608 = c("6r608"),
    ecf6r609_614 = c("6r609", "6r614"), ecf6r611 = c("6r611"), ecf6r612 = c("6r612"),
    ecf6r613 = c("6r613"), ecf6r615 = c("6r615"), ecf6r616_617 = c("6r616", "6r617"), ecf6r618 = c("6r618")
  )
  for (nm in names(ecf_map)) {data[[nm]] <- as.integer(data$ecf %in% ecf_map[[nm]])}
  data
}

make_time_adjusted_df <- function(df, mod, formula, p_out = 0.10, verbose = TRUE) {
  coef_names <- c("months_1to9", "months_10to24")
  all_coefs  <- coef(mod)
  time_coefs <- setNames(rep(NA_real_, length(coef_names)), coef_names)
  matched    <- intersect(coef_names, names(all_coefs))
  time_coefs[matched] <- all_coefs[matched]
  rate1_initial <- unname(ifelse(is.na(time_coefs["months_1to9"]),  0, time_coefs["months_1to9"]))
  rate2_initial <- unname(ifelse(is.na(time_coefs["months_10to24"]), 0, time_coefs["months_10to24"]))
  
  df_out <- df %>%
    dplyr::mutate(
      rate1 = rate1_initial, rate2 = rate2_initial, price_index = (1 + rate1)^months_1to9 * (1 + rate2)^months_10to24
    )
  
  end_month <- max(df_out$months, na.rm = TRUE)
  end_index <- df_out %>% dplyr::filter(months == end_month) %>%
    dplyr::summarise(end_index = mean(price_index, na.rm = TRUE)) %>% dplyr::pull(end_index)
  
  df_out <- df_out %>%
    dplyr::mutate(taf = end_index / price_index, tasp = saleprice * taf,
                  tasppsf = tasp / sresb_floorarea, ln_tasp = log(tasp), end_index = end_index
    )
  df_out
}

normalize_ratio_output <- function(x, section_name) {
  if (is.data.frame(x)) {    # data.frame / tibble case
    out <- as_tibble(x)
    rn <- rownames(x)        # if rownames matter, preserve them
    if (!is.null(rn) && any(nzchar(rn))) {
      out <- tibble(row_name = rn) %>% bind_cols(out)
    }
    out <- out %>% mutate(.section = section_name, .before = 1)
    return(out)
  }
  
  if (is.atomic(x) && !is.null(names(x))) {    # named numeric / character / logical vector
    return(tibble(.section = section_name, metric = names(x), value = unname(x)))
  }
  
  if (is.list(x) && !is.data.frame(x)) {  # list case (often named list of summary stats)
    # if every element is length 1, convert to metric/value
    if (!is.null(names(x)) && all(lengths(x) == 1)) {
      return(tibble(.section = section_name, metric = names(x), value = unlist(x, use.names = FALSE)))
    }
    out <- tryCatch(as_tibble(x), error = function(e) NULL)    # otherwise try coercing to tibble
    if (!is.null(out)) {
      out <- out %>% mutate(.section = section_name, .before = 1)
      return(out)
    }
  }
  stop("Unsupported structure in section: ", section_name)
}

make_ratio_outputs_df <- function(ratio_outputs) {
  section_tables <- purrr::imap(ratio_outputs, normalize_ratio_output)
  all_cols <- unique(unlist(purrr::map(section_tables, names)))
  
  section_tables <- purrr::map(section_tables, function(tbl) {
    missing_cols <- setdiff(all_cols, names(tbl))
    if (length(missing_cols) > 0) {tbl[missing_cols] <- NA}
    tbl[, all_cols]
  })
  
  dplyr::bind_rows(section_tables) %>% rename(section = .section)
}

keep_existing_terms <- function(data, terms) terms[terms %in% names(data)]

model_metrics_c <- function(model, digits = 4) {  
  gl <- broom::glance(model)
  get_col <- function(df, nm) {if (nm %in% names(df)) df[[nm]] else NA_real_}
  df_resid = stats::df.residual(model)
  df_reg = summary(model)$fstatistic[["numdf"]]
  
  gl2 <- dplyr::tibble(
    df_total = df_resid + df_reg, df_resid, df_reg, see = get_col(gl, "sigma"),
    r2     = get_col(gl, "r.squared"),        adj_r2 = get_col(gl, "adj.r.squared"),
    f_stat = get_col(gl, "statistic")      
  ) %>% mutate(across(where(is.numeric), ~ round(.x, digits)))
  gl2
}

make_metrics_kable <- function(
    spss_df, r_df, source_col = "metricsource", spss_label = "SPSS", r_label = "R",
    pct_label = "% diff (R vs SPSS)", caption = "SPSS and R Metrics Comparison",
    latex = TRUE, digits = 4, pct_digits = 2, max_pct_for_red = 10
) {
  if (!is.data.frame(spss_df)) stop("spss_df must be a data.frame")
  if (!is.data.frame(r_df)) stop("r_df must be a data.frame")
  if (nrow(spss_df) != 1 || nrow(r_df) != 1) {
    stop("This version expects spss_df and r_df to each have exactly one row.")
  }
  if (source_col %in% names(spss_df) || source_col %in% names(r_df)) {
    stop("source_col already exists in one of the input data frames.")
  }
  if (!requireNamespace("knitr", quietly = TRUE)) {stop("Package 'knitr' is required.")}
  
  format_type <- if (latex) "latex" else "html"
  
  pdf_safe <- function(x) {
    x <- as.character(x)
    x <- gsub("\\\\", "\\\\textbackslash{}", x, perl = TRUE)
    x <- gsub("([#$%&_{}])", "\\\\\\1", x, perl = TRUE)
    x <- gsub("~", "\\\\textasciitilde{}", x, fixed = TRUE)
    x <- gsub("\\^", "\\\\textasciicircum{}", x, perl = TRUE)
    x
  }
  
  whole_numberish <- function(x, tol = 1e-9) {
    ok <- !is.na(x) & is.finite(x)
    if (!any(ok)) return(FALSE)
    all(abs(x[ok] - round(x[ok])) < tol)
  }
  
  format_metric_values <- function(vals, integer_like, digits) {
    out <- rep("", length(vals))
    ok <- !is.na(vals)
    if (!any(ok)) return(out)
    if (integer_like) {out[ok] <- formatC(as.integer(round(vals[ok])), format = "d")
    } else {out[ok] <- formatC(vals[ok], digits = digits, format = "f")
    }
    out
  }
  
  spss_out <- spss_df %>% mutate(!!source_col := spss_label) %>% relocate(dplyr::all_of(source_col))
  r_out <- r_df %>% mutate(!!source_col := r_label) %>% relocate(dplyr::all_of(source_col))
  
  all_cols <- union(names(spss_out), names(r_out))
  for (nm in setdiff(all_cols, names(spss_out))) spss_out[[nm]] <- NA
  for (nm in setdiff(all_cols, names(r_out))) r_out[[nm]] <- NA
  spss_out <- spss_out[, all_cols, drop = FALSE]
  r_out <- r_out[, all_cols, drop = FALSE]
  common_cols <- intersect(names(spss_df), names(r_df))
  numeric_compare_cols <- common_cols[vapply(
    common_cols,
    function(nm) is.numeric(spss_df[[nm]]) && is.numeric(r_df[[nm]]),
    logical(1)
  )]
  
  pct_diff_one <- function(r_val, spss_val) {
    if (is.na(r_val) || is.na(spss_val)) return(NA_real_)
    if (spss_val == 0) {
      if (r_val == 0) return(0)
      return(NA_real_)
    }
    ((r_val - spss_val) / abs(spss_val)) * 100
  }
  
  pct_row <- as.list(rep(NA, length(all_cols)))
  names(pct_row) <- all_cols
  pct_row[[source_col]] <- pct_label
  
  for (nm in numeric_compare_cols) {pct_row[[nm]] <- pct_diff_one(r_df[[nm]][1], spss_df[[nm]][1])}
  pct_row <- as.data.frame(pct_row, stringsAsFactors = FALSE)
  merged_df <- dplyr::bind_rows(spss_out, r_out, pct_row)
  
  if (!requireNamespace("kableExtra", quietly = TRUE)) {
    fallback_df <- merged_df
    if (latex) {
      names(fallback_df) <- pdf_safe(names(fallback_df))
      plain_text_cols <- setdiff(names(fallback_df), numeric_compare_cols)
      for (nm in plain_text_cols) {
        fallback_df[[nm]] <- pdf_safe(fallback_df[[nm]])
      }
    }
    kb <- knitr::kable(
      fallback_df, caption = caption, digits = digits, format = format_type,
      booktabs = latex, escape = !latex
    )
    return(list(data = merged_df, kable = kb))
  }
  
  diff_to_color <- function(x, max_pct = max_pct_for_red) {
    out <- rep(NA_character_, length(x))
    ok <- !is.na(x) & is.finite(x)
    if (!any(ok)) return(out)
    scaled <- pmin(pmax(abs(x[ok]) / max_pct, 0), 1)
    ramp_mat <- grDevices::colorRamp(c("#1a9850", "#fee08b", "#d73027"))(scaled)
    
    out[ok] <- grDevices::rgb(
      ramp_mat[, 1],
      ramp_mat[, 2],
      ramp_mat[, 3],
      maxColorValue = 255
    )
    out
  }
  display_df <- merged_df
  numeric_cols_all <- names(display_df)[vapply(display_df, is.numeric, logical(1))]
  numeric_cols_all <- setdiff(numeric_cols_all, source_col)
  
  for (nm in numeric_cols_all) {
    vals <- display_df[[nm]]
    integer_like <- whole_numberish(c(spss_out[[nm]], r_out[[nm]]))
    display_df[[nm]] <- format_metric_values(vals, integer_like = integer_like, digits = digits)
  }
  
  pct_colors <- stats::setNames(
    diff_to_color(unlist(pct_row[1, numeric_compare_cols, drop = TRUE])), numeric_compare_cols
  )
  
  r_row_index <- 2
  pct_row_index <- 3
  
  for (nm in numeric_compare_cols) {
    cell_color <- pct_colors[[nm]]
    if (is.na(cell_color) || identical(cell_color, NA_character_)) next
    display_df[r_row_index, nm] <- kableExtra::cell_spec(
      display_df[r_row_index, nm], format = format_type, background = cell_color, color = "black"
    )
    pct_val <- pct_row[[nm]][1]
    pct_text <- if (is.na(pct_val)) "" else paste0(formatC(pct_val, digits = pct_digits, format = "f"), "%")
    
    display_df[pct_row_index, nm] <- kableExtra::cell_spec(
      pct_text, format = format_type, background = cell_color, color = "black", bold = TRUE
    )
  }
  if (latex) {
    orig_names <- names(display_df)
    names(display_df) <- pdf_safe(orig_names)
    plain_text_cols <- setdiff(names(display_df), pdf_safe(numeric_compare_cols))
    for (nm in plain_text_cols) {display_df[[nm]] <- pdf_safe(display_df[[nm]])}
  }
  kb <- knitr::kable(
    display_df, caption = caption, digits = digits, format = format_type, booktabs = latex, escape = FALSE
  )
  kb <- kb %>%
    kableExtra::kable_styling(
      latex_options = if (latex) c("HOLD_position", "striped") else NULL, full_width = FALSE
    ) %>% kableExtra::row_spec(pct_row_index, bold = TRUE)
  
  list(data = merged_df, kable = kb)
}

make_coeff_compare_tabl <- function(
    model,
    spss_df,
    include_intercept = TRUE,
    intercept_first = TRUE,
    digits = 3,
    caption = "SPSS vs R Coefficient Comparison"
) {
  if (!inherits(model, "lm")) stop("`model` must be an lm object.")
  if (!is.data.frame(spss_df)) stop("`spss_df` must be a data.frame.")
  
  needed <- c("term", "b", "std_error", "beta", "t", "sig")
  missing_needed <- setdiff(needed, names(spss_df))
  if (length(missing_needed) > 0) {
    stop("`spss_df` is missing required columns: ",
         paste(missing_needed, collapse = ", "))
  }
  
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required.")
  if (!requireNamespace("knitr", quietly = TRUE)) stop("Package 'knitr' is required.")
  if (!requireNamespace("kableExtra", quietly = TRUE)) stop("Package 'kableExtra' is required.")
  
  normalize_term <- function(x) {
    x <- trimws(tolower(as.character(x)))
    x[x %in% c("(constant)", "(intercept)", "constant", "intercept")] <- "(intercept)"
    x
  }
  
  # ---- Build R coefficient dataframe directly from lm ----
  sm <- summary(model)
  coef_mat <- sm$coefficients
  
  r_df <- data.frame(
    term = rownames(coef_mat),
    b = coef_mat[, "Estimate"],
    std_error = coef_mat[, "Std. Error"],
    t = coef_mat[, "t value"],
    sig = coef_mat[, "Pr(>|t|)"],
    stringsAsFactors = FALSE
  )
  
  # Approximate standardized beta from model matrix
  mf <- stats::model.frame(model)
  y <- stats::model.response(mf)
  X <- stats::model.matrix(model)
  
  beta_vec <- rep(NA_real_, nrow(r_df))
  names(beta_vec) <- r_df$term
  
  if (ncol(X) > 1) {
    y_sd <- stats::sd(y, na.rm = TRUE)
    x_names <- colnames(X)
    
    for (i in seq_along(x_names)) {
      nm <- x_names[i]
      if (nm == "(Intercept)") next
      x_sd <- stats::sd(X[, i], na.rm = TRUE)
      b_i <- r_df$b[match(nm, r_df$term)]
      if (!is.na(x_sd) && !is.na(y_sd) && y_sd != 0) {
        beta_vec[nm] <- b_i * x_sd / y_sd
      }
    }
  }
  
  r_df$beta <- beta_vec[r_df$term]
  
  # ---- Prepare SPSS df ----
  spss_df2 <- spss_df %>%
    dplyr::transmute(
      term_raw_spss = as.character(term),
      term_key = normalize_term(term),
      b_spss = as.numeric(b),
      se_spss = as.numeric(std_error),
      beta_spss = as.numeric(beta),
      t_spss = as.numeric(t),
      sig_spss = as.numeric(sig)
    )
  
  # ---- Prepare R df ----
  r_df2 <- r_df %>%
    dplyr::transmute(
      term_raw_r = as.character(term),
      term_key = normalize_term(term),
      b_r = as.numeric(b),
      se_r = as.numeric(std_error),
      beta_r = as.numeric(beta),
      t_r = as.numeric(t),
      sig_r = as.numeric(sig)
    )
  
  # ---- Join ----
  cmp <- dplyr::full_join(spss_df2, r_df2, by = "term_key") %>%
    dplyr::mutate(
      term = dplyr::coalesce(term_raw_r, term_raw_spss),
      is_intercept = term_key == "(intercept)"
    )
  
  # ---- Keep/drop intercept ----
  if (!include_intercept) {
    cmp <- cmp %>% dplyr::filter(!is_intercept)
  }
  
  # ---- Order by R beta descending ----
  intercept_df <- cmp %>% dplyr::filter(is_intercept)
  non_intercept_df <- cmp %>%
    dplyr::filter(!is_intercept) %>%
    dplyr::arrange(dplyr::desc(beta_r), term)
  
  cmp <- if (include_intercept && intercept_first) {
    dplyr::bind_rows(intercept_df, non_intercept_df)
  } else if (include_intercept && !intercept_first) {
    dplyr::bind_rows(non_intercept_df, intercept_df)
  } else {
    non_intercept_df
  }
  
  # ---- Display df ----
  out_df <- cmp %>%
    dplyr::transmute(
      term = term,
      b_r = b_r,
      b_spss = b_spss,
      se_r = se_r,
      se_spss = se_spss,
      beta_r = beta_r,
      beta_spss = beta_spss,
      t_r = t_r,
      t_spss = t_spss,
      sig_r = sig_r,
      sig_spss = sig_spss
    )
  
  # Format numeric columns
  fmt_num <- function(x, d = digits) {
    ifelse(is.na(x), "NA", formatC(x, digits = d, format = "f"))
  }
  
  out_disp <- out_df
  for (nm in names(out_disp)[-1]) {
    out_disp[[nm]] <- fmt_num(out_disp[[nm]], digits)
  }
  
  # Make intercept label look like screenshot
  out_disp$term[out_disp$term == "(Intercept)"] <- "(constant)"
  out_disp$term[out_disp$term == "(intercept)"] <- "(constant)"
  
  # ---- Build kable ----
  kb <- knitr::kable(
    out_disp,
    format = "latex",
    booktabs = TRUE,
    escape = TRUE,
    align = c("l", rep("r", ncol(out_disp) - 1)),
    col.names = c("term", "R", "SPSS", "R", "SPSS", "R", "SPSS", "R", "SPSS", "R", "SPSS"),
    caption = caption
  ) %>%
    kableExtra::add_header_above(
      c(" " = 1, "b" = 2, "Std. Error" = 2, "Beta" = 2, "t" = 2, "Sig" = 2)
    ) %>%
    kableExtra::kable_styling(
      latex_options = c("HOLD_position"),
      full_width = FALSE
    )
  
  list(
    data = out_df,
    kable = kb
  )
}


summarize_extreme_betas_kable <- function(
    mod1, mod2, mod3, mod4, mod5, mod6,
    model_names = c("M1", "M2", "M3", "M4", "M5", "M6"),
    top_n = 3,
    bottom_n = 3,
    include_intercept = FALSE,
    digits = 3,
    caption = "Top and bottom standardized beta predictors across models",
    wrap_models_at = 18
) {
  mods <- list(mod1, mod2, mod3, mod4, mod5, mod6)
  
  if (length(model_names) != 6) {
    stop("model_names must have length 6.")
  }
  
  if (!all(vapply(mods, inherits, logical(1), what = "lm"))) {
    stop("All model inputs must be lm objects.")
  }
  
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required.")
  if (!requireNamespace("knitr", quietly = TRUE)) stop("Package 'knitr' is required.")
  if (!requireNamespace("kableExtra", quietly = TRUE)) stop("Package 'kableExtra' is required.")
  
  extract_model_metrics <- function(model, model_name, include_intercept = FALSE) {
    sm <- summary(model)
    coef_mat <- sm$coefficients
    
    out <- data.frame(
      term = rownames(coef_mat),
      b = coef_mat[, "Estimate"],
      std_error = coef_mat[, "Std. Error"],
      t = coef_mat[, "t value"],
      sig = coef_mat[, "Pr(>|t|)"],
      stringsAsFactors = FALSE
    )
    
    mf <- stats::model.frame(model)
    y <- stats::model.response(mf)
    X <- stats::model.matrix(model)
    
    beta_vec <- rep(NA_real_, nrow(out))
    names(beta_vec) <- out$term
    
    y_sd <- stats::sd(y, na.rm = TRUE)
    
    if (!is.na(y_sd) && y_sd != 0) {
      for (j in seq_len(ncol(X))) {
        nm <- colnames(X)[j]
        if (nm == "(Intercept)") next
        x_sd <- stats::sd(X[, j], na.rm = TRUE)
        b_j <- out$b[match(nm, out$term)]
        if (!is.na(x_sd) && x_sd != 0 && !is.na(b_j)) {
          beta_vec[nm] <- b_j * x_sd / y_sd
        }
      }
    }
    
    out$beta <- beta_vec[out$term]
    out$model <- model_name
    
    if (!include_intercept) {
      out <- out[out$term != "(Intercept)", , drop = FALSE]
    }
    
    out
  }
  
  all_extremes <- lapply(seq_along(mods), function(i) {
    df_i <- extract_model_metrics(mods[[i]], model_names[i], include_intercept = include_intercept)
    df_i <- df_i[!is.na(df_i$beta), , drop = FALSE]
    
    if (nrow(df_i) == 0) return(df_i)
    
    top_part <- df_i %>%
      dplyr::arrange(dplyr::desc(beta), term) %>%
      dplyr::slice_head(n = top_n)
    
    bottom_part <- df_i %>%
      dplyr::arrange(beta, term) %>%
      dplyr::slice_head(n = bottom_n)
    
    dplyr::bind_rows(top_part, bottom_part) %>%
      dplyr::distinct(term, model, .keep_all = TRUE)
  })
  
  extreme_df <- dplyr::bind_rows(all_extremes)
  
  summary_df <- extreme_df %>%
    dplyr::group_by(term) %>%
    dplyr::summarise(
      n_models = dplyr::n(),
      models = paste(model, collapse = ", "),
      avg_b = mean(b, na.rm = TRUE),
      avg_se = mean(std_error, na.rm = TRUE),
      avg_beta = mean(beta, na.rm = TRUE),
      avg_t = mean(t, na.rm = TRUE),
      avg_sig = mean(sig, na.rm = TRUE),
      min_beta = min(beta, na.rm = TRUE),
      max_beta = max(beta, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(avg_beta), term)
  
  wrap_text <- function(x, width = 18) {
    vapply(
      x,
      function(s) paste(strwrap(s, width = width), collapse = "\n"),
      character(1)
    )
  }
  
  display_df <- summary_df %>%
    dplyr::mutate(
      models = wrap_text(models, width = wrap_models_at)
    )
  
  kb <- knitr::kable(
    display_df,
    format = "latex",
    booktabs = TRUE,
    linesep = "",
    digits = digits,
    escape = TRUE,
    align = c("l", "c", "l", rep("r", 6), "r", "r"),
    caption = caption,
    col.names = c(
      "Predictor", "n",
      "Models",
      "Avg b", "Avg SE", "Avg beta", "Avg t", "Avg sig",
      "Min beta", "Max beta"
    )
  ) %>%
    kableExtra::kable_styling(
      latex_options = c("hold_position", "striped", "scale_down"),
      font_size = 12,
      full_width = FALSE
    ) %>%
    kableExtra::column_spec(1, width = "1.7in") %>%
    kableExtra::column_spec(2, width = "0.4in") %>%
    kableExtra::column_spec(3, width = "1.2in") %>%
    kableExtra::column_spec(4:ncol(display_df), width = "0.7in")
  
  list(
    extreme_rows = extreme_df,
    summary_data = summary_df,
    kable = kb
  )
}

plot_beta_lollipop <- function(
    model, include_intercept = FALSE, top_n = NULL,
    title = "Horizontal lollipop plot of standardized beta",
    x_lab = "Standardized beta", y_lab = NULL
) {
  if (!inherits(model, "lm")) {stop("`model` must be an lm object.")}
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required.")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required.")
  }
  
  sm <- summary(model)
  coef_mat <- sm$coefficients
  
  coef_df <- data.frame(
    term = rownames(coef_mat), b = coef_mat[, "Estimate"],
    std_error = coef_mat[, "Std. Error"], t = coef_mat[, "t value"],
    sig = coef_mat[, "Pr(>|t|)"], stringsAsFactors = FALSE
  )
  
  mf <- stats::model.frame(model)
  y <- stats::model.response(mf)
  X <- stats::model.matrix(model)
  
  beta_vec <- rep(NA_real_, nrow(coef_df))
  names(beta_vec) <- coef_df$term
  
  y_sd <- stats::sd(y, na.rm = TRUE)
  
  if (!is.na(y_sd) && y_sd != 0) {
    for (j in seq_len(ncol(X))) {
      nm <- colnames(X)[j]
      if (nm == "(Intercept)") next
      x_sd <- stats::sd(X[, j], na.rm = TRUE)
      b_j <- coef_df$b[match(nm, coef_df$term)]
      
      if (!is.na(x_sd) && x_sd != 0 && !is.na(b_j)) {
        beta_vec[nm] <- b_j * x_sd / y_sd
      }
    }
  }
  
  coef_df$beta <- beta_vec[coef_df$term]
  
  if (!include_intercept) {
    coef_df <- coef_df[coef_df$term != "(Intercept)", , drop = FALSE]
  }
  
  coef_df <- coef_df[!is.na(coef_df$beta), , drop = FALSE]
  
  if (!is.null(top_n)) {
    if (!is.numeric(top_n) || length(top_n) != 1 || is.na(top_n) || top_n < 1) {
      stop("`top_n` must be NULL or a single positive number.")
    }
    
    top_pos <- coef_df %>%      dplyr::arrange(dplyr::desc(beta), term) %>%
      dplyr::slice_head(n = top_n)
    
    top_neg <- coef_df %>% dplyr::arrange(beta, term) %>% dplyr::slice_head(n = top_n)
    
    coef_df <- dplyr::bind_rows(top_pos, top_neg) %>%
      dplyr::distinct(term, .keep_all = TRUE)
  }
  
  coef_df <- coef_df %>%    dplyr::arrange(beta) %>%
    dplyr::mutate(term = factor(term, levels = term))
  
  max_abs_beta <- max(abs(coef_df$beta), na.rm = TRUE)
  if (!is.finite(max_abs_beta) || max_abs_beta == 0) max_abs_beta <- 1
  
  p <- ggplot2::ggplot(coef_df, ggplot2::aes(x = beta, y = term, color = beta)) +
    ggplot2::geom_segment(
      ggplot2::aes(x = 0, xend = beta, y = term, yend = term), linewidth = 0.9
    ) + geom_point(size = 3) + geom_vline(xintercept = 0, linetype = "dashed") +
    scale_color_gradient2(
      low = "red", mid = "grey70", high = "green", midpoint = 0,
      limits = c(-max_abs_beta, max_abs_beta), name = "Beta"
    ) + labs(title = title, x = x_lab, y = y_lab) + theme_minimal(base_size = 12)
  
  list(
    data = coef_df, plot = p
  )
}

plot_avg_beta_lollipop_6models <- function(
    mod1, mod2, mod3, mod4, mod5, mod6,
    model_names = c("M1", "M2", "M3", "M4", "M5", "M6"),
    include_intercept = FALSE,    top_n = NULL,
    title = "Average standardized beta across 6 models",
    x_lab = "Average standardized beta",    y_lab = NULL
) {
  mods <- list(mod1, mod2, mod3, mod4, mod5, mod6)
  
  if (length(model_names) != 6) {stop("model_names must have length 6.")}
  if (!all(vapply(mods, inherits, logical(1), what = "lm"))) {
    stop("All model inputs must be lm objects.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required.")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required.")
  }
  
  extract_model_betas <- function(model, model_name, include_intercept = FALSE) {
    sm <- summary(model)
    coef_mat <- sm$coefficients
    
    coef_df <- data.frame(
      term = rownames(coef_mat),      b = coef_mat[, "Estimate"],
      std_error = coef_mat[, "Std. Error"],      t = coef_mat[, "t value"],
      sig = coef_mat[, "Pr(>|t|)"],      stringsAsFactors = FALSE
    )
    
    mf <- stats::model.frame(model)
    y <- stats::model.response(mf)
    X <- stats::model.matrix(model)
    
    beta_vec <- rep(NA_real_, nrow(coef_df))
    names(beta_vec) <- coef_df$term
    
    y_sd <- stats::sd(y, na.rm = TRUE)
    
    if (!is.na(y_sd) && y_sd != 0) {
      for (j in seq_len(ncol(X))) {
        nm <- colnames(X)[j]
        if (nm == "(Intercept)") next
        x_sd <- stats::sd(X[, j], na.rm = TRUE)
        b_j <- coef_df$b[match(nm, coef_df$term)]
        
        if (!is.na(x_sd) && x_sd != 0 && !is.na(b_j)) {
          beta_vec[nm] <- b_j * x_sd / y_sd
        }
      }
    }
    
    coef_df$beta <- beta_vec[coef_df$term]
    coef_df$model <- model_name
    
    if (!include_intercept) {
      coef_df <- coef_df[coef_df$term != "(Intercept)", , drop = FALSE]
    }
    
    coef_df <- coef_df[!is.na(coef_df$beta), , drop = FALSE]
    coef_df
  }
  
  all_betas <- Map(
    function(m, nm) extract_model_betas(m, nm, include_intercept = include_intercept),
    mods, model_names
  ) %>%    dplyr::bind_rows()
  
  summary_df <- all_betas %>%    dplyr::group_by(term) %>%
    dplyr::summarise(
      n_models = dplyr::n(),
      models = paste(model, collapse = ", "),
      avg_beta = mean(beta, na.rm = TRUE),
      min_beta = min(beta, na.rm = TRUE),
      max_beta = max(beta, na.rm = TRUE),
      .groups = "drop"
    )
  
  if (!is.null(top_n)) {
    if (!is.numeric(top_n) || length(top_n) != 1 || is.na(top_n) || top_n < 1) {
      stop("`top_n` must be NULL or a single positive number.")
    }
    
    top_pos <- summary_df %>%      dplyr::arrange(dplyr::desc(avg_beta), term) %>%
      dplyr::slice_head(n = top_n)
    
    top_neg <- summary_df %>%      dplyr::arrange(avg_beta, term) %>%
      dplyr::slice_head(n = top_n)
    
    summary_df <- dplyr::bind_rows(top_pos, top_neg) %>%
      dplyr::distinct(term, .keep_all = TRUE)
  }
  
  summary_df <- summary_df %>%    dplyr::arrange(avg_beta) %>%
    dplyr::mutate(term = factor(term, levels = term))
  
  max_abs_beta <- max(abs(summary_df$avg_beta), na.rm = TRUE)
  if (!is.finite(max_abs_beta) || max_abs_beta == 0) max_abs_beta <- 1
  
  p <- ggplot2::ggplot(summary_df, ggplot2::aes(x = avg_beta, y = term, color = avg_beta)) +
    ggplot2::geom_segment(
      ggplot2::aes(x = 0, xend = avg_beta, y = term, yend = term), linewidth = 0.9
    ) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
    ggplot2::scale_color_gradient2(
      low = "red",      mid = "grey70",      high = "green",      midpoint = 0,
      limits = c(-max_abs_beta, max_abs_beta),      name = "Avg beta"
    ) +
    ggplot2::labs(title = title,      x = x_lab,      y = y_lab) +
    ggplot2::theme_minimal(base_size = 12)
  
  list(model_level_data = all_betas, summary_data = summary_df, plot = p)
}

compare_metrics_kables2 <- function(mod, spss_df, 
                                    caption1 = "Model Metrics (Part 1: Impact and Coefficients)",
                                    caption2 = "Model Metrics (Part 2: Significance and Collinearity)",
                                    digits = 3) {
  
  sm <- summary(mod)                 # 1. EXTRACT R METRICS
  coefs <- sm$coefficients
  surviving_terms <- rownames(coefs)
  mf <- model.frame(mod)             # Calculate standardized coefficients (beta)
  y <- model.response(mf)
  mm <- model.matrix(mod)
  # Filter model matrix to ONLY include non-aliased terms.
  # This preventsrow mismatch when calculating beta_vals.
  mm <- mm[, surviving_terms, drop = FALSE]
  sy <- sd(y)
  sx <- apply(mm, 2, function(x) if(var(x) == 0) 0 else sd(x))
  beta_vals <- coefs[, "Estimate"] * (sx / sy)
  
  r_df <- data.frame(                   # Build the base R dataframe
    term = rownames(coefs), r_b = coefs[, "Estimate"], r_std_error = coefs[, "Std. Error"],
    r_beta = beta_vals, r_t = coefs[, "t value"], r_sig = coefs[, "Pr(>|t|)"],
    stringsAsFactors = FALSE
  )
  # Standardize R terminology to match SPSS conventions
  r_df$term[r_df$term == "(Intercept)"] <- "(constant)"
  r_df$r_beta[r_df$term == "(constant)"] <- NA
  # Extract VIF and Tolerance safely
  vif_df <- data.frame(term = character(), r_tolerance = numeric(), r_vif = numeric(), stringsAsFactors = FALSE)
  if (requireNamespace("car", quietly = TRUE)) {
    # FIX: car::vif() crashes if a model has aliased (dropped) terms.
    # We must build a temporary clean model to calculate VIFs.
    aliased_terms <- names(coef(mod))[is.na(coef(mod))]
    
    if (length(aliased_terms) > 0) {
      f_clean <- update(formula(mod), paste(". ~ . -", paste(paste0("`", aliased_terms, "`"), collapse = " - ")))
      mod_vif <- lm(f_clean, data = model.frame(mod))
    } else {mod_vif <- mod}
    
    vif_vals <- tryCatch(car::vif(mod_vif), error = function(e) NULL)
    
    if (!is.null(vif_vals)) {
      if (is.matrix(vif_vals)) {
        r_vif_vals <- (vif_vals[, "GVIF"]^(1 / (2 * vif_vals[, "Df"])))^2
        vif_terms <- rownames(vif_vals)
      } else {
        r_vif_vals <- as.numeric(vif_vals)
        vif_terms <- names(vif_vals)
      }
      # Clean names of backticks just in case, ensuring a perfect join
      vif_terms <- gsub("`", "", vif_terms)
      vif_df <- data.frame(term = vif_terms, r_vif = r_vif_vals, r_tolerance = 1 / r_vif_vals, stringsAsFactors = FALSE)
    }
  }
  # Join VIF metrics into the R dataframe
  r_df <- dplyr::left_join(r_df, vif_df, by = "term")
  # 2. PREPARE SPSS METRICS
  spss_temp <- spss_df
  names(spss_temp) <- tolower(names(spss_temp)) 
  # Ensure tolerance and vif columns exist to prevent errors if missing
  if (!"tolerance" %in% names(spss_temp)) spss_temp$tolerance <- NA_real_
  if (!"vif" %in% names(spss_temp)) spss_temp$vif <- NA_real_
  
  spss_clean <- spss_temp %>%
    dplyr::select(
      term, spss_b = b, spss_std_error = std_error, spss_beta = beta,
      spss_t = t, spss_sig = sig, spss_tolerance = tolerance, spss_vif = vif
    )
  # 3. MERGE AND RANK
  merged_df <- dplyr::full_join(r_df, spss_clean, by = "term")
  # Rank strictly by R Impact, anchoring (constant) at the top
  merged_df <- merged_df %>%
    dplyr::arrange(desc(term == "(constant)"), desc(abs(r_b)))
  # 4. SPLIT INTO TWO DATAFRAMES  # Table 1: b, Std. Error, Beta, t
  df1 <- merged_df %>%
    dplyr::select(term, r_b, spss_b, r_std_error, spss_std_error, r_beta, spss_beta, r_t, spss_t)
  # Table 2: Sig, Tolerance, VIF
  df2 <- merged_df %>%
    dplyr::select(term, r_sig, spss_sig, r_tolerance, spss_tolerance, r_vif, spss_vif)
  # 5. RENDER KABLES
  output_format <- if (knitr::is_latex_output()) "latex" else if (knitr::is_html_output()) "html" else "pipe"
  
  # ---- Setup Table 1 ----
  if (output_format %in% c("html", "latex")) {
    col_names1 <- c("term", "R", "SPSS", "R", "SPSS", "R", "SPSS", "R", "SPSS")
  } else {
    col_names1 <- c("term", "R_b", "SPSS_b", "R_SE", "SPSS_SE", "R_beta", "SPSS_beta", "R_t", "SPSS_t")
  }
  
  k1 <- knitr::kable(df1, format = output_format, booktabs = TRUE, caption = caption1, col.names = col_names1, digits = digits)
  # ---- Setup Table 2 ----
  if (output_format %in% c("html", "latex")) {
    col_names2 <- c("term", "R", "SPSS", "R", "SPSS", "R", "SPSS")
  } else {
    col_names2 <- c("term", "R_sig", "SPSS_sig", "R_tol", "SPSS_tol", "R_vif", "SPSS_vif")
  }
  
  k2 <- knitr::kable(df2, format = output_format, booktabs = TRUE, caption = caption2, col.names = col_names2, digits = digits)
  
  # ---- Apply Styling ----
  if (output_format %in% c("html", "latex")) {
    # Table 1 Styling
    k1 <- k1 %>%
      kableExtra::add_header_above(c(" " = 1, "b" = 2, "Std. Error" = 2, "Beta" = 2, "t" = 2)) %>%
      kableExtra::row_spec(0, bold = TRUE)
    # Table 2 Styling
    k2 <- k2 %>%
      kableExtra::add_header_above(c(" " = 1, "Sig" = 2, "Tolerance" = 2, "VIF" = 2)) %>%
      kableExtra::row_spec(0, bold = TRUE)
    
    if (output_format == "html") {
      k1 <- k1 %>%
        kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
        kableExtra::column_spec(1, bold = TRUE)
      
      k2 <- k2 %>%
        kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
        kableExtra::column_spec(1, bold = TRUE)
    } else {
      k1 <- k1 %>% kableExtra::kable_styling(full_width = FALSE, latex_options = "HOLD_position")
      k2 <- k2 %>% kableExtra::kable_styling(full_width = FALSE, latex_options = "HOLD_position")
    }
  }
  return(list(table1 = k1, table2 = k2)) # Return both tables as a list
}
















