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
                         keep_terms = character(0)) {
  
  # Freeze the case set once, to match SPSS listwise behavior
  mf <- model.frame(formula, data = data, na.action = na.omit)
  mod <- lm(formula, data = mf)
  
  repeat {
    d1 <- drop1(mod, test = "F")
    d1 <- tibble::rownames_to_column(as.data.frame(d1), "term")
    
    # remove the "<none>" row and any protected terms
    d1 <- d1[d1$term != "<none>", , drop = FALSE]
    d1 <- d1[!d1$term %in% keep_terms, , drop = FALSE]
    
    if (nrow(d1) == 0) break
    if (!"Pr(>F)" %in% names(d1)) break
    
    pvals <- d1[["Pr(>F)"]]
    ok <- is.finite(pvals) & !is.na(pvals)
    if (!any(ok)) break
    
    worst_idx <- which.max(pvals[ok])
    worst_term <- d1$term[ok][worst_idx]
    worst_p <- pvals[ok][worst_idx]
    
    if (worst_p <= p_out) break
    
    if (verbose) {
      message(sprintf("Dropping term: %s (p = %.4f)", worst_term, worst_p))
    }
    
    mod <- update(mod, as.formula(paste(". ~ . -", worst_term)))
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
  ecf_levels <- sprintf("7r%03d", 701:724)
  ecf_levels <- setdiff(ecf_levels, base_ecf)
  for (lvl in ecf_levels) {
    nm <- paste0("ecf", lvl)
    data[[nm]] <- as.integer(data$ecf == lvl)
  }
  data
}

make_time_adjusted_df <- function(df, mod, formula, p_out = 0.10, verbose = TRUE) {
  coef_names <- c("months_1to16", "months_17to24")
  all_coefs  <- coef(mod)
  time_coefs <- setNames(rep(NA_real_, length(coef_names)), coef_names)
  matched    <- intersect(coef_names, names(all_coefs))
  time_coefs[matched] <- all_coefs[matched]
  rate1_initial <- unname(ifelse(is.na(time_coefs["months_1to16"]),  0, time_coefs["months_1to16"]))
  rate2_initial <- unname(ifelse(is.na(time_coefs["months_17to24"]), 0, time_coefs["months_17to24"]))
  
  df_out <- df %>%
    dplyr::mutate(
      rate1 = rate1_initial, rate2 = rate2_initial, price_index = (1 + rate1)^months_1to16 * (1 + rate2)^months_17to24
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
    if (!is.null(rn) && any(nzchar(rn))) {out <- tibble(row_name = rn) %>% bind_cols(out)}
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

make_coeff_df <- function(model, model_name = "Model 1", include_intercept = TRUE,
                          standardized_beta = TRUE, digits = NULL) {
  if (!inherits(model, "lm")) {stop("`model` must be an lm object.")}
  sm <- summary(model)
  coef_mat <- as.data.frame(sm$coefficients, stringsAsFactors = FALSE)
  if (ncol(coef_mat) < 4) {stop("Unexpected coefficient matrix structure.")}
  names(coef_mat)[1:4] <- c("b", "std_error", "t", "sig")
  coef_mat$term <- rownames(coef_mat)
  rownames(coef_mat) <- NULL
  dep_var <- all.vars(formula(model))[1]
  coef_mat$beta <- NA_real_    # Standardized beta
  if (standardized_beta) {
    mf <- model.frame(model)
    y <- model.response(mf)
    X <- model.matrix(model)
    sdy <- stats::sd(y, na.rm = TRUE)
    if (is.finite(sdy) && sdy > 0) {
      sdx <- apply(X, 2, stats::sd, na.rm = TRUE)
      b_vec <- stats::coef(model)
      common_terms <- intersect(names(b_vec), colnames(X))
      beta_vals <- rep(NA_real_, length(b_vec))
      names(beta_vals) <- names(b_vec)
      beta_vals[common_terms] <- b_vec[common_terms] * sdx[common_terms] / sdy
      coef_mat$beta <- unname(beta_vals[coef_mat$term])
    }
  }
  out <- data.frame(model = model_name, dependent_variable = dep_var, term = coef_mat$term,
                    b = coef_mat$b, std_error = coef_mat$std_error, beta = coef_mat$beta,
                    t = coef_mat$t, sig = coef_mat$sig, stringsAsFactors = FALSE
  )
  if (!include_intercept) {out <- out[out$term != "(Intercept)", , drop = FALSE]}
  if (!is.null(digits)) {
    num_cols <- c("b", "std_error", "beta", "t", "sig")
    out[num_cols] <- lapply(out[num_cols], round, digits = digits)
  }
  out
}

make_coeff_kable <- function(
    model, spss_df, model_name = "Model 1", spss_label = "SPSS", r_label = "R",
    impact_by = c("beta", "t", "b"), include_intercept = FALSE,
    caption = "SPSS vs R Coefficient Comparison", latex = TRUE, digits = 4,
    pct_digits = 2, max_pct_for_red = 10, beta_if_missing = TRUE
) {
  impact_by <- match.arg(impact_by)
  
  normalize_term_key <- function(x) {
    x <- as.character(x)
    x <- trimws(x)
    x <- tolower(x)
    x[x %in% c("(intercept)", "(constant)", "constant", "intercept")] <- "intercept"
    x
  }
  
  r_df <- make_coeff_df(
    model = model,
    model_name = model_name,
    include_intercept = include_intercept,
    standardized_beta = beta_if_missing,
    digits = NULL
  )
  
  if (!include_intercept) {
    spss_df <- spss_df[spss_df$term != "(Constant)", , drop = FALSE]
    r_df    <- r_df[r_df$term != "(Intercept)", , drop = FALSE]
  }
  
  spss_df2 <- spss_df %>%
    dplyr::mutate(
      term_raw_spss = term,
      term_key = normalize_term_key(term)
    )
  
  r_df2 <- r_df %>%
    dplyr::mutate(
      term_raw_r = term,
      term_key = normalize_term_key(term)
    )
  
  cmp <- dplyr::full_join(
    spss_df2 %>% dplyr::rename_with(~ paste0(.x, "_spss"), -term_key),
    r_df2    %>% dplyr::rename_with(~ paste0(.x, "_r"), -term_key),
    by = "term_key"
  ) %>%
    dplyr::mutate(
      term = dplyr::coalesce(term_raw_r, term_raw_spss)
    )
  
  pct_diff_one <- function(r_val, spss_val) {
    if (is.na(r_val) || is.na(spss_val)) return(NA_real_)
    if (spss_val == 0) {
      if (r_val == 0) return(0)
      return(NA_real_)
    }
    ((r_val - spss_val) / abs(spss_val)) * 100
  }
  
  compare_metrics <- c("b", "std_error", "beta", "t", "sig")
  
  for (nm in compare_metrics) {
    s_col <- paste0(nm, "_spss")
    r_col <- paste0(nm, "_r")
    if (s_col %in% names(cmp) && r_col %in% names(cmp)) {
      cmp[[paste0(nm, "_pct_diff")]] <- mapply(
        pct_diff_one,
        cmp[[r_col]],
        cmp[[s_col]]
      )
    }
  }
  
  impact_candidates <- switch(
    impact_by,
    beta = c("beta_r", "beta_spss", "t_r", "t_spss", "b_r", "b_spss"),
    t    = c("t_r", "t_spss", "beta_r", "beta_spss", "b_r", "b_spss"),
    b    = c("b_r", "b_spss", "t_r", "t_spss", "beta_r", "beta_spss")
  )
  
  impact_source <- impact_candidates[impact_candidates %in% names(cmp)][1]
  
  if (is.na(impact_source) || length(impact_source) == 0) {
    cmp$impact_value <- NA_real_
    impact_source <- "none"
  } else {
    cmp$impact_value <- cmp[[impact_source]]
  }
  
  cmp <- cmp %>%
    dplyr::arrange(
      dplyr::desc(impact_value),
      term
    )
  
  out_df <- cmp %>%
    dplyr::select(
      term,
      dplyr::any_of(c(
        "b_spss", "b_r", "b_pct_diff",
        "std_error_spss", "std_error_r", "std_error_pct_diff",
        "beta_spss", "beta_r", "beta_pct_diff",
        "t_spss", "t_r", "t_pct_diff",
        "sig_spss", "sig_r", "sig_pct_diff"
      ))
    )
  
  kb <- knitr::kable(
    out_df,
    format = if (latex) "latex" else "simple",
    caption = caption,
    digits = digits,
    booktabs = latex
  )
  
  list(data = out_df, kable = kb)
}

compare_metrics_kabl3 <- function(spss_df, r_df, source_col_name = "Model_Source", caption = "Model Metrics Comparison") {
  
  # 1. Add the identifying column to both dataframes
  spss_df <- spss_df %>% 
    dplyr::mutate(!!sym(source_col_name) := "SPSS Metrics")
  
  r_df <- r_df %>% 
    dplyr::mutate(!!sym(source_col_name) := "R Metrics")
  
  # 2. Merge dataframes by stacking them
  # bind_rows keeps the original column names intact and perfectly aligns them
  merged_df <- dplyr::bind_rows(r_df, spss_df) %>%
    # Move the newly created source column to the very front
    dplyr::select(!!sym(source_col_name), dplyr::everything())
  
  # 3. Output to kable
  # Auto-detects environment (HTML for markdown/web, LaTeX for PDFs)
  output_format <- if (knitr::is_latex_output()) "latex" else if (knitr::is_html_output()) "html" else "pipe"
  
  k <- knitr::kable(
    merged_df,
    format = output_format,
    booktabs = TRUE,
    caption = caption
  )
  
  # Add styling for HTML/LaTeX outputs
  if (output_format %in% c("html", "latex")) {
    k <- k %>%
      kableExtra::row_spec(0, bold = TRUE)
    
    if (output_format == "html") {
      k <- k %>%
        kableExtra::kable_styling(
          bootstrap_options = c("striped", "hover", "condensed", "responsive"),
          full_width = FALSE
        ) %>%
        kableExtra::column_spec(1, bold = TRUE) # Bold the Source column for readability
    } else {
      k <- k %>% 
        kableExtra::kable_styling(full_width = FALSE, latex_options = "hold_position")
    }
  }
  
  return(k)
}

compare_metrics_kabl <- function(mod, spss_df, source_col_name = "Model_Source", caption = "Model Metrics Comparison") {
  
  # 1. Extract metrics from the R 'lm' object
  sm <- summary(mod)
  coefs <- sm$coefficients
  
  # Get dependent variable name
  dep_var <- all.vars(formula(mod))[1]
  
  # Calculate standardized coefficients (beta)
  mf <- model.frame(mod)
  y <- model.response(mf)
  mm <- model.matrix(mod)
  
  sy <- sd(y)
  # Safely get standard deviation of predictors (intercept will be 0)
  sx <- apply(mm, 2, function(x) if(var(x) == 0) 0 else sd(x))
  
  beta_vals <- coefs[, "Estimate"] * (sx / sy)
  
  # Build the R dataframe exactly matching the SPSS structure
  r_df <- data.frame(
    dependent_variable = dep_var,
    term = rownames(coefs),
    b = coefs[, "Estimate"],
    std_error = coefs[, "Std. Error"],
    beta = beta_vals,
    t = coefs[, "t value"],
    sig = coefs[, "Pr(>|t|)"],
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  
  # Standardize R terminology to match SPSS conventions
  r_df$term[r_df$term == "(Intercept)"] <- "(constant)"
  r_df$beta[r_df$term == "(constant)"] <- NA
  
  # 2. Add the identifying column to both dataframes
  spss_df <- spss_df %>% 
    dplyr::mutate(!!sym(source_col_name) := "SPSS Metrics")
  
  r_df <- r_df %>% 
    dplyr::mutate(!!sym(source_col_name) := "R Metrics")
  
  # 3. Merge dataframes by stacking them
  # bind_rows keeps the original column names intact and perfectly aligns them
  merged_df <- dplyr::bind_rows(r_df, spss_df) %>%
    # Move the newly created source column to the very front
    dplyr::select(!!sym(source_col_name), dplyr::everything())
  
  # 4. Output to kable
  # Auto-detects environment (HTML for markdown/web, LaTeX for PDFs)
  output_format <- if (knitr::is_latex_output()) "latex" else if (knitr::is_html_output()) "html" else "pipe"
  
  k <- knitr::kable(
    merged_df,
    format = output_format,
    booktabs = TRUE,
    caption = caption,
    digits = 3 # Round to 3 decimal places to match SPSS style output
  )
  
  # Add styling for HTML/LaTeX outputs
  if (output_format %in% c("html", "latex")) {
    k <- k %>%
      kableExtra::row_spec(0, bold = TRUE)
    
    if (output_format == "html") {
      k <- k %>%
        kableExtra::kable_styling(
          bootstrap_options = c("striped", "hover", "condensed", "responsive"),
          full_width = FALSE
        ) %>%
        kableExtra::column_spec(1, bold = TRUE) # Bold the Source column for readability
    } else {
      k <- k %>% 
        kableExtra::kable_styling(full_width = FALSE, latex_options = "hold_position")
    }
  }
  
  return(k)
}

compare_metrics_kab <- function(mod, spss_df, caption = "Model Metrics Comparison (Ranked by Impact)") {
  
  # 1. Extract metrics from the R 'lm' object
  sm <- summary(mod)
  coefs <- sm$coefficients
  
  # Calculate standardized coefficients (beta)
  mf <- model.frame(mod)
  y <- model.response(mf)
  mm <- model.matrix(mod)
  
  sy <- sd(y)
  # Safely get standard deviation of predictors (intercept will be 0)
  sx <- apply(mm, 2, function(x) if(var(x) == 0) 0 else sd(x))
  
  beta_vals <- coefs[, "Estimate"] * (sx / sy)
  
  # Build the R dataframe with specific prefixes
  r_df <- data.frame(
    term = rownames(coefs),
    r_b = coefs[, "Estimate"],
    r_std_error = coefs[, "Std. Error"],
    r_beta = beta_vals,
    r_t = coefs[, "t value"],
    r_sig = coefs[, "Pr(>|t|)"],
    stringsAsFactors = FALSE
  )
  
  # Standardize R terminology to match SPSS conventions
  r_df$term[r_df$term == "(Intercept)"] <- "(constant)"
  r_df$r_beta[r_df$term == "(constant)"] <- NA
  
  # 2. Prepare SPSS dataframe (Prefixing columns to allow side-by-side alignment)
  spss_temp <- spss_df
  # Force lowercase names to guarantee matching
  names(spss_temp) <- tolower(names(spss_temp)) 
  
  spss_clean <- spss_temp %>%
    dplyr::select(
      term,
      spss_b = b,
      spss_std_error = std_error,
      spss_beta = beta,
      spss_t = t,
      spss_sig = sig
    )
  
  # 3. Join dataframes side-by-side
  merged_df <- dplyr::full_join(r_df, spss_clean, by = "term")
  
  # 4. Rank by Impact
  # Standardized Beta is the true measure of relative impact. 
  # We anchor the (constant) at the top, then sort the rest by highest absolute beta.
  merged_df <- merged_df %>%
    dplyr::arrange(
      desc(term == "(constant)"),
      desc(abs(r_beta))
    )
  
  # 5. Order columns so R and SPSS are right next to each other per metric
  merged_df <- merged_df %>%
    dplyr::select(
      term,
      r_b, spss_b,
      r_std_error, spss_std_error,
      r_beta, spss_beta,
      r_t, spss_t,
      r_sig, spss_sig
    )
  
  # 6. Output to kable
  # Auto-detects environment (HTML for markdown/web, LaTeX for PDFs)
  output_format <- if (knitr::is_latex_output()) "latex" else if (knitr::is_html_output()) "html" else "pipe"
  
  # Grouped Headers don't exist in standard markdown pipe tables, so we change names conditionally
  if (output_format %in% c("html", "latex")) {
    col_names <- c("term", "R", "SPSS", "R", "SPSS", "R", "SPSS", "R", "SPSS", "R", "SPSS")
  } else {
    col_names <- c("term", "R_b", "SPSS_b", "R_SE", "SPSS_SE", "R_beta", "SPSS_beta", "R_t", "SPSS_t", "R_sig", "SPSS_sig")
  }
  
  k <- knitr::kable(
    merged_df,
    format = output_format,
    booktabs = TRUE,
    caption = caption,
    col.names = col_names,
    digits = 3 # Round to 3 decimal places to match SPSS style output
  )
  
  # Add grouped header styling for HTML/LaTeX outputs
  if (output_format %in% c("html", "latex")) {
    k <- k %>%
      kableExtra::add_header_above(c(" " = 1, "b" = 2, "Std. Error" = 2, "Beta" = 2, "t" = 2, "Sig" = 2)) %>%
      kableExtra::row_spec(0, bold = TRUE)
    
    if (output_format == "html") {
      k <- k %>%
        kableExtra::kable_styling(
          bootstrap_options = c("striped", "hover", "condensed", "responsive"),
          full_width = FALSE
        ) %>%
        kableExtra::column_spec(1, bold = TRUE) # Bold the term column
    } else {
      k <- k %>% 
        kableExtra::kable_styling(full_width = FALSE, latex_options = "hold_position")
    }
  }
  
  return(k)
}

compare_metrics_ka <- function(mod, spss_df, caption = "Model Metrics Comparison (Ranked by R Impact)") {
  
  # 1. Extract metrics from the R 'lm' object
  sm <- summary(mod)
  coefs <- sm$coefficients
  
  # Calculate standardized coefficients (beta)
  mf <- model.frame(mod)
  y <- model.response(mf)
  mm <- model.matrix(mod)
  
  sy <- sd(y)
  # Safely get standard deviation of predictors (intercept will be 0)
  sx <- apply(mm, 2, function(x) if(var(x) == 0) 0 else sd(x))
  
  beta_vals <- coefs[, "Estimate"] * (sx / sy)
  
  # Build the R dataframe with specific prefixes
  r_df <- data.frame(
    term = rownames(coefs),
    r_b = coefs[, "Estimate"],
    r_std_error = coefs[, "Std. Error"],
    r_beta = beta_vals,
    r_t = coefs[, "t value"],
    r_sig = coefs[, "Pr(>|t|)"],
    stringsAsFactors = FALSE
  )
  
  # Standardize R terminology to match SPSS conventions
  r_df$term[r_df$term == "(Intercept)"] <- "(constant)"
  r_df$r_beta[r_df$term == "(constant)"] <- NA
  
  # 2. Prepare SPSS dataframe (Prefixing columns to allow side-by-side alignment)
  spss_temp <- spss_df
  # Force lowercase names to guarantee matching
  names(spss_temp) <- tolower(names(spss_temp)) 
  
  spss_clean <- spss_temp %>%
    dplyr::select(
      term,
      spss_b = b,
      spss_std_error = std_error,
      spss_beta = beta,
      spss_t = t,
      spss_sig = sig
    )
  
  # 3. Join dataframes side-by-side
  merged_df <- dplyr::full_join(r_df, spss_clean, by = "term")
  
  # 4. Rank strictly by R Impact
  # We anchor the (constant) at the top.
  # We sort strictly by the absolute value of the R 'b' coefficient. 
  # Because R's dplyr::arrange() naturally pushes NA values to the very end,
  # any variables dropped by R (like ext_other) will automatically sink to the bottom.
  merged_df <- merged_df %>%
    dplyr::arrange(
      desc(term == "(constant)"),
      desc(abs(r_b))
    )
  
  # 5. Order columns so R and SPSS are right next to each other per metric
  merged_df <- merged_df %>%
    dplyr::select(
      term,
      r_b, spss_b,
      r_std_error, spss_std_error,
      r_beta, spss_beta,
      r_t, spss_t,
      r_sig, spss_sig
    )
  
  # 6. Output to kable
  # Auto-detects environment (HTML for markdown/web, LaTeX for PDFs)
  output_format <- if (knitr::is_latex_output()) "latex" else if (knitr::is_html_output()) "html" else "pipe"
  
  # Grouped Headers don't exist in standard markdown pipe tables, so we change names conditionally
  if (output_format %in% c("html", "latex")) {
    col_names <- c("term", "R", "SPSS", "R", "SPSS", "R", "SPSS", "R", "SPSS", "R", "SPSS")
  } else {
    col_names <- c("term", "R_b", "SPSS_b", "R_SE", "SPSS_SE", "R_beta", "SPSS_beta", "R_t", "SPSS_t", "R_sig", "SPSS_sig")
  }
  
  k <- knitr::kable(
    merged_df,
    format = output_format,
    booktabs = TRUE,
    caption = caption,
    col.names = col_names,
    digits = 3 # Round to 3 decimal places to match SPSS style output
  )
  
  # Add grouped header styling for HTML/LaTeX outputs
  if (output_format %in% c("html", "latex")) {
    k <- k %>%
      kableExtra::add_header_above(c(" " = 1, "b" = 2, "Std. Error" = 2, "Beta" = 2, "t" = 2, "Sig" = 2)) %>%
      kableExtra::row_spec(0, bold = TRUE)
    
    if (output_format == "html") {
      k <- k %>%
        kableExtra::kable_styling(
          bootstrap_options = c("striped", "hover", "condensed", "responsive"),
          full_width = FALSE
        ) %>%
        kableExtra::column_spec(1, bold = TRUE) # Bold the term column
    } else {
      k <- k %>% 
        kableExtra::kable_styling(full_width = FALSE, latex_options = "hold_position")
    }
  }
  
  return(k)
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
      term_raw_spss = as.character(term),      term_key = normalize_term(term),
      b_spss = as.numeric(b),      se_spss = as.numeric(std_error),
      beta_spss = as.numeric(beta),      t_spss = as.numeric(t),
      sig_spss = as.numeric(sig)
    )
  # ---- Prepare R df ----
  r_df2 <- r_df %>%
    dplyr::transmute(
      term_raw_r = as.character(term),      term_key = normalize_term(term),
      b_r = as.numeric(b),      se_r = as.numeric(std_error),
      beta_r = as.numeric(beta),      t_r = as.numeric(t),      sig_r = as.numeric(sig)
    )
  # ---- Join ----
  cmp <- dplyr::full_join(spss_df2, r_df2, by = "term_key") %>%
    dplyr::mutate(
      term = dplyr::coalesce(term_raw_r, term_raw_spss),
      is_intercept = term_key == "(intercept)"
    )
  # ---- Keep/drop intercept ----
  if (!include_intercept) {cmp <- cmp %>% dplyr::filter(!is_intercept)}
  
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
      term = term,      b_r = b_r,      b_spss = b_spss,      se_r = se_r,
      se_spss = se_spss,      beta_r = beta_r,      beta_spss = beta_spss,
      t_r = t_r,      t_spss = t_spss,      sig_r = sig_r,      sig_spss = sig_spss
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
    out_disp,    format = "latex",    booktabs = TRUE,    escape = TRUE,
    align = c("l", rep("r", ncol(out_disp) - 1)),
    col.names = c("term", "R", "SPSS", "R", "SPSS", "R", "SPSS", "R", "SPSS", "R", "SPSS"),
    caption = caption
  ) %>%
    kableExtra::add_header_above(
      c(" " = 1, "b" = 2, "Std. Error" = 2, "Beta" = 2, "t" = 2, "Sig" = 2)
    ) %>%
    kableExtra::kable_styling(latex_options = c("HOLD_position"), full_width = FALSE)
  
  list(
    data = out_df, kable = kb
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
  
  if (length(model_names) != 6) {stop("model_names must have length 6.")}
  
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
      term = rownames(coef_mat),      b = coef_mat[, "Estimate"],
      std_error = coef_mat[, "Std. Error"],      t = coef_mat[, "t value"],
      sig = coef_mat[, "Pr(>|t|)"],      stringsAsFactors = FALSE
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
    
    if (!include_intercept) {out <- out[out$term != "(Intercept)", , drop = FALSE]}
    out
  }
  
  all_extremes <- lapply(seq_along(mods), function(i) {
    df_i <- extract_model_metrics(mods[[i]], model_names[i], include_intercept = include_intercept)
    df_i <- df_i[!is.na(df_i$beta), , drop = FALSE]
    
    if (nrow(df_i) == 0) return(df_i)
    
    top_part <- df_i %>%      dplyr::arrange(dplyr::desc(beta), term) %>%
      dplyr::slice_head(n = top_n)
    
    bottom_part <- df_i %>%      dplyr::arrange(beta, term) %>%      dplyr::slice_head(n = bottom_n)
    
    dplyr::bind_rows(top_part, bottom_part) %>%
      dplyr::distinct(term, model, .keep_all = TRUE)
  })
  
  extreme_df <- dplyr::bind_rows(all_extremes)
  
  summary_df <- extreme_df %>%    dplyr::group_by(term) %>%
    dplyr::summarise(
      n_models = dplyr::n(),      models = paste(model, collapse = ", "),
      avg_b = mean(b, na.rm = TRUE),      avg_se = mean(std_error, na.rm = TRUE),
      avg_beta = mean(beta, na.rm = TRUE),      avg_t = mean(t, na.rm = TRUE),
      avg_sig = mean(sig, na.rm = TRUE),      min_beta = min(beta, na.rm = TRUE),
      max_beta = max(beta, na.rm = TRUE),      .groups = "drop"
    ) %>%    dplyr::arrange(dplyr::desc(avg_beta), term)
  
  wrap_text <- function(x, width = 18) {
    vapply(
      x,
      function(s) paste(strwrap(s, width = width), collapse = "\n"),
      character(1)
    )
  }
  
  display_df <- summary_df %>%
    dplyr::mutate(models = wrap_text(models, width = wrap_models_at))
  
  kb <- knitr::kable(
    display_df, format = "latex", booktabs = TRUE, linesep = "",
    digits = digits, escape = TRUE,    align = c("l", "c", "l", rep("r", 6), "r", "r"),
    caption = caption,    col.names = c(
      "Predictor", "n",
      "Models",
      "Avg b", "Avg SE", "Avg beta", "Avg t", "Avg sig",
      "Min beta", "Max beta"
    )
  ) %>%
    kableExtra::kable_styling(
      latex_options = c("hold_position", "striped", "scale_down"), font_size = 12,
      full_width = FALSE
    ) %>%
    kableExtra::column_spec(1, width = "1.7in") %>%
    kableExtra::column_spec(2, width = "0.4in") %>%
    kableExtra::column_spec(3, width = "1.2in") %>%
    kableExtra::column_spec(4:ncol(display_df), width = "0.7in")
  
  list(
    extreme_rows = extreme_df, summary_data = summary_df, kable = kb
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

plot_avg_beta_lollipop_6model <- function(
    mod1, mod2, mod3, mod4, mod5, mod6,
    model_names = c("M1", "M2", "M3", "M4", "M5", "M6"),
    include_intercept = FALSE,
    include_ecf = TRUE,
    ecf_pattern = "^ecf",
    top_n = NULL,
    title = "Average standardized beta across 6 models",
    x_lab = "Average standardized beta",
    y_lab = NULL
) {
  mods <- list(mod1, mod2, mod3, mod4, mod5, mod6)
  
  if (length(model_names) != 6) stop("model_names must have length 6.")
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
  ) |>
    dplyr::bind_rows()
  
  if (!include_ecf) {
    all_betas <- all_betas |>
      dplyr::filter(!grepl(ecf_pattern, term, ignore.case = TRUE))
  }
  
  summary_df <- all_betas |>
    dplyr::group_by(term) |>
    dplyr::summarise(
      n_models = dplyr::n(),
      models = paste(model, collapse = ", "),
      avg_beta = mean(beta, na.rm = TRUE),
      min_beta = min(beta, na.rm = TRUE),
      max_beta = max(beta, na.rm = TRUE),
      .groups = "drop"
    )
  
  if (nrow(summary_df) == 0) {
    stop("No predictors remain after filtering. Check `include_ecf` and `ecf_pattern`.")
  }
  
  if (!is.null(top_n)) {
    if (!is.numeric(top_n) || length(top_n) != 1 || is.na(top_n) || top_n < 1) {
      stop("`top_n` must be NULL or a single positive number.")
    }
    
    top_pos <- summary_df |>
      dplyr::arrange(dplyr::desc(avg_beta), term) |>
      dplyr::slice_head(n = top_n)
    
    top_neg <- summary_df |>
      dplyr::arrange(avg_beta, term) |>
      dplyr::slice_head(n = top_n)
    
    summary_df <- dplyr::bind_rows(top_pos, top_neg) |>
      dplyr::distinct(term, .keep_all = TRUE)
  }
  
  summary_df <- summary_df |>
    dplyr::arrange(avg_beta) |>
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
      low = "red",
      mid = "grey70",
      high = "green",
      midpoint = 0,
      limits = c(-max_abs_beta, max_abs_beta),
      name = "Avg beta"
    ) +
    ggplot2::labs(title = title, x = x_lab, y = y_lab) +
    ggplot2::theme_minimal(base_size = 12)
  
  list(model_level_data = all_betas, summary_data = summary_df, plot = p)
}

predict_property_value_by_parcel <- function(
    model,
    data,
    parcel_id,
    parcel_col = "parcelno",
    response_type = c("saleprice", "sppsf", "time_adjusted_saleprice"),
    response_is_logged = TRUE,
    floorarea_col = "sresb_floorarea",
    taf_col = "taf",
    correction_factor = 1,
    allow_multiple_matches = FALSE
) {
  response_type <- match.arg(response_type)
  
  if (!inherits(model, c("lm", "glm"))) {
    stop("`model` must be a fitted lm or glm object.")
  }
  
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.")
  }
  
  if (!parcel_col %in% names(data)) {
    stop("`parcel_col` not found in `data`: ", parcel_col)
  }
  
  if (missing(parcel_id) || length(parcel_id) != 1 || is.na(parcel_id)) {
    stop("`parcel_id` must be a single non-missing value.")
  }
  
  rows <- data[data[[parcel_col]] == parcel_id, , drop = FALSE]
  
  if (nrow(rows) == 0) {
    stop("No row found in `data` for parcel_id = ", parcel_id)
  }
  
  if (nrow(rows) > 1 && !allow_multiple_matches) {
    stop(
      "Multiple rows found for parcel_id = ", parcel_id,
      ". Set `allow_multiple_matches = TRUE` or deduplicate first."
    )
  }
  
  newdata <- rows[1, , drop = FALSE]
  
  tt <- stats::delete.response(stats::terms(model))
  needed_vars <- all.vars(tt)
  
  missing_vars <- setdiff(needed_vars, names(newdata))
  if (length(missing_vars) > 0) {
    stop(
      "The dataframe row is missing predictors required by the model: ",
      paste(missing_vars, collapse = ", ")
    )
  }
  
  pred_model_scale <- as.numeric(stats::predict(model, newdata = newdata))
  
  # Convert from model scale to natural scale
  pred_natural <- if (response_is_logged) {
    exp(pred_model_scale) * correction_factor
  } else {
    pred_model_scale * correction_factor
  }
  
  predicted_saleprice <- NA_real_
  predicted_sppsf <- NA_real_
  predicted_time_adjusted_saleprice <- NA_real_
  
  if (response_type == "saleprice") {
    predicted_saleprice <- pred_natural
  }
  
  if (response_type == "sppsf") {
    predicted_sppsf <- pred_natural
    
    if (!floorarea_col %in% names(newdata)) {
      warning("`", floorarea_col, "` not found, so sale price could not be derived from sppsf.")
    } else {
      fa <- as.numeric(newdata[[floorarea_col]])
      if (!is.na(fa) && fa > 0) {
        predicted_saleprice <- predicted_sppsf * fa
      }
    }
  }
  
  if (response_type == "time_adjusted_saleprice") {
    # Two possible scenarios:
    # 1) model predicts sale price directly, so pred_natural is already time-adjusted sale price
    # 2) model predicts sppsf, so multiply by floor area to get time-adjusted sale price
    if (floorarea_col %in% names(newdata)) {
      fa <- as.numeric(newdata[[floorarea_col]])
    } else {
      fa <- NA_real_
    }
    
    if (!is.na(fa) && fa > 0) {
      predicted_time_adjusted_saleprice <- pred_natural * fa
      predicted_sppsf <- pred_natural
    } else {
      predicted_time_adjusted_saleprice <- pred_natural
    }
    
    predicted_saleprice <- predicted_time_adjusted_saleprice
    
    # If TAF is available, also back out regular sale price estimate
    if (taf_col %in% names(newdata)) {
      taf_val <- as.numeric(newdata[[taf_col]])
      if (!is.na(taf_val) && taf_val != 0) {
        predicted_saleprice <- predicted_time_adjusted_saleprice / taf_val
      }
    }
  }
  
  list(
    parcel_id = parcel_id,
    row_used = newdata,
    formula = stats::formula(model),
    response_type = response_type,
    predicted_model_scale = pred_model_scale,
    predicted_natural_scale = pred_natural,
    predicted_saleprice = predicted_saleprice,
    predicted_sppsf = predicted_sppsf,
    predicted_time_adjusted_saleprice = predicted_time_adjusted_saleprice
  )
}

#res <- predict_property_value_by_parcel(
#  model = mod_time_adj,
#  data = df,
#  parcel_id = "12345678.001",
#  parcel_col = "parcelno",
#  response_type = "time_adjusted_saleprice",
#  response_is_logged = TRUE,
#  floorarea_col = "sresb_floorarea",
#  taf_col = "taf"
#)
#res$predicted_time_adjusted_saleprice
#res$predicted_saleprice

#res <- predict_property_value_by_parcel(
#  model = mod_sppsf, data = df, parcel_id = "12345678.001", parcel_col = "parcelno",
#  response_type = "sppsf", response_is_logged = TRUE, floorarea_col = "sresb_floorarea"
#)
#res$predicted_sppsf
#res$predicted_saleprice

predict_property_values_df <- function(
    model,
    data,
    parcel_col = "parcelno",
    actual_saleprice_col = "saleprice",
    response_type = c("saleprice", "sppsf", "time_adjusted_saleprice"),
    response_is_logged = TRUE,
    floorarea_col = "sresb_floorarea",
    taf_col = "taf",
    correction_factor = 1,
    pred_model_col = "pred_model_scale",
    pred_natural_col = "pred_natural_scale",
    pred_saleprice_col = "pred_saleprice",
    pred_sppsf_col = "pred_sppsf",
    pred_tasp_col = "pred_time_adjusted_saleprice",
    saleprice_ratio_col = "pred_saleprice_ratio",
    saleprice_diff_col = "pred_saleprice_diff",
    saleprice_pctdiff_col = "pred_saleprice_pctdiff"
) {
  response_type <- match.arg(response_type)
  
  if (!inherits(model, c("lm", "glm"))) {
    stop("`model` must be a fitted lm or glm object.")
  }
  
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.")
  }
  
  if (!parcel_col %in% names(data)) {
    warning("`parcel_col` not found in `data`: ", parcel_col)
  }
  
  if (!actual_saleprice_col %in% names(data)) {
    stop("`actual_saleprice_col` not found in `data`: ", actual_saleprice_col)
  }
  
  tt <- stats::delete.response(stats::terms(model))
  needed_vars <- all.vars(tt)
  
  missing_vars <- setdiff(needed_vars, names(data))
  if (length(missing_vars) > 0) {
    stop(
      "The dataframe is missing predictors required by the model: ",
      paste(missing_vars, collapse = ", ")
    )
  }
  
  out <- data
  
  pred_model_scale <- as.numeric(stats::predict(model, newdata = out))
  
  pred_natural <- if (response_is_logged) {
    exp(pred_model_scale) * correction_factor
  } else {
    pred_model_scale * correction_factor
  }
  
  pred_saleprice <- rep(NA_real_, nrow(out))
  pred_sppsf <- rep(NA_real_, nrow(out))
  pred_tasp <- rep(NA_real_, nrow(out))
  
  if (response_type == "saleprice") {
    pred_saleprice <- pred_natural
  }
  
  if (response_type == "sppsf") {
    pred_sppsf <- pred_natural
    
    if (!floorarea_col %in% names(out)) {
      warning("`", floorarea_col, "` not found, so sale price could not be derived from sppsf.")
    } else {
      fa <- as.numeric(out[[floorarea_col]])
      ok <- !is.na(fa) & fa > 0
      pred_saleprice[ok] <- pred_sppsf[ok] * fa[ok]
    }
  }
  
  if (response_type == "time_adjusted_saleprice") {
    if (floorarea_col %in% names(out)) {
      fa <- as.numeric(out[[floorarea_col]])
    } else {
      fa <- rep(NA_real_, nrow(out))
    }
    
    ok_fa <- !is.na(fa) & fa > 0
    
    # If floor area exists, assume model predicts time-adjusted sppsf and convert to tasp
    pred_tasp[ok_fa] <- pred_natural[ok_fa] * fa[ok_fa]
    pred_sppsf[ok_fa] <- pred_natural[ok_fa]
    
    # For rows without floor area, keep natural prediction as time-adjusted price
    pred_tasp[!ok_fa] <- pred_natural[!ok_fa]
    
    pred_saleprice <- pred_tasp
    
    if (taf_col %in% names(out)) {
      taf_val <- as.numeric(out[[taf_col]])
      ok_taf <- !is.na(taf_val) & taf_val != 0 & !is.na(pred_tasp)
      pred_saleprice[ok_taf] <- pred_tasp[ok_taf] / taf_val[ok_taf]
    }
  }
  
  actual_saleprice <- as.numeric(out[[actual_saleprice_col]])
  
  pred_ratio <- ifelse(
    !is.na(actual_saleprice) & actual_saleprice != 0,
    pred_saleprice / actual_saleprice,
    NA_real_
  )
  
  pred_diff <- pred_saleprice - actual_saleprice
  
  pred_pctdiff <- ifelse(
    !is.na(actual_saleprice) & actual_saleprice != 0,
    100 * (pred_saleprice - actual_saleprice) / actual_saleprice,
    NA_real_
  )
  
  out[[pred_model_col]] <- pred_model_scale
  out[[pred_natural_col]] <- pred_natural
  out[[pred_saleprice_col]] <- pred_saleprice
  out[[pred_sppsf_col]] <- pred_sppsf
  out[[pred_tasp_col]] <- pred_tasp
  out[[saleprice_ratio_col]] <- pred_ratio
  out[[saleprice_diff_col]] <- pred_diff
  out[[saleprice_pctdiff_col]] <- pred_pctdiff
  
  out
}

#df_pred <- predict_property_values_df(
#  model = mod_sppsf,
#  data = df,
#  response_type = "sppsf", OR "time_adjusted_saleprice
#  response_is_logged = TRUE,
#  floorarea_col = "sresb_floorarea" CAN ADD , taf_col = "taf"
#)

summarize_saleprice_deviation_kable <- function(
    model,
    data,
    title = "Sale Price Deviation Summary",
    parcel_col = "parcelno",
    actual_saleprice_col = "saleprice",
    response_type = c("saleprice", "sppsf", "time_adjusted_saleprice"),
    response_is_logged = TRUE,
    floorarea_col = "sresb_floorarea",
    taf_col = "taf",
    correction_factor = 1,
    pred_model_col = "pred_model_scale",
    pred_natural_col = "pred_natural_scale",
    pred_saleprice_col = "pred_saleprice",
    pred_sppsf_col = "pred_sppsf",
    pred_tasp_col = "pred_time_adjusted_saleprice",
    saleprice_ratio_col = "pred_saleprice_ratio",
    saleprice_diff_col = "pred_saleprice_diff",
    saleprice_pctdiff_col = "pred_saleprice_pctdiff",
    digits = 2
) {
  response_type <- match.arg(response_type)
  
  if (!exists("predict_property_values_df", mode = "function")) {
    stop("`predict_property_values_df()` was not found. Define it first.")
  }
  
  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("Package 'knitr' is required.")
  }
  if (!requireNamespace("kableExtra", quietly = TRUE)) {
    stop("Package 'kableExtra' is required.")
  }
  
  pred_df <- predict_property_values_df(
    model = model,
    data = data,
    parcel_col = parcel_col,
    actual_saleprice_col = actual_saleprice_col,
    response_type = response_type,
    response_is_logged = response_is_logged,
    floorarea_col = floorarea_col,
    taf_col = taf_col,
    correction_factor = correction_factor,
    pred_model_col = pred_model_col,
    pred_natural_col = pred_natural_col,
    pred_saleprice_col = pred_saleprice_col,
    pred_sppsf_col = pred_sppsf_col,
    pred_tasp_col = pred_tasp_col,
    saleprice_ratio_col = saleprice_ratio_col,
    saleprice_diff_col = saleprice_diff_col,
    saleprice_pctdiff_col = saleprice_pctdiff_col
  )
  
  diffs <- pred_df[[saleprice_diff_col]]
  diffs <- diffs[is.finite(diffs)]
  
  if (length(diffs) == 0) {
    stop("No finite sale price deviations were available to summarize.")
  }
  
  summary_df <- data.frame(
    metric = c(
      "Average sale price deviation",
      "Max deviation",
      "Min deviation",
      "Standard deviation"
    ),
    value = c(
      mean(diffs, na.rm = TRUE),
      max(diffs, na.rm = TRUE),
      min(diffs, na.rm = TRUE),
      stats::sd(diffs, na.rm = TRUE)
    ),
    stringsAsFactors = FALSE
  )
  
  kb <- knitr::kable(
    summary_df,
    format = "latex",
    booktabs = TRUE,
    digits = digits,
    caption = title,
    col.names = c("Metric", "Value"),
    align = c("l", "r")
  ) %>%
    kableExtra::kable_styling(
      latex_options = c("hold_position"),
      full_width = FALSE
    )
  
  list(
    prediction_data = pred_df,
    summary_data = summary_df,
    kable = kb
  )
}

summarize_saleprice_deviation_kabl3 <- function(
    model,
    data,
    title = "Sale Price Deviation Summary",
    parcel_col = "parcelno",
    actual_saleprice_col = "saleprice",
    response_type = c("saleprice", "sppsf", "time_adjusted_saleprice"),
    response_is_logged = TRUE,
    floorarea_col = "sresb_floorarea",
    taf_col = "taf",
    correction_factor = 1,
    pred_model_col = "pred_model_scale",
    pred_natural_col = "pred_natural_scale",
    pred_saleprice_col = "pred_saleprice",
    pred_sppsf_col = "pred_sppsf",
    pred_tasp_col = "pred_time_adjusted_saleprice",
    saleprice_ratio_col = "pred_saleprice_ratio",
    saleprice_diff_col = "pred_saleprice_diff",
    saleprice_pctdiff_col = "pred_saleprice_pctdiff",
    digits = 2
) {
  response_type <- match.arg(response_type)
  
  if (!exists("predict_property_values_df", mode = "function")) {
    stop("`predict_property_values_df()` was not found. Define it first.")
  }
  
  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("Package 'knitr' is required.")
  }
  if (!requireNamespace("kableExtra", quietly = TRUE)) {
    stop("Package 'kableExtra' is required.")
  }
  
  latex_escape <- function(x) {
    x <- as.character(x)
    x <- gsub("\\\\", "\\\\textbackslash{}", x)
    x <- gsub("([#$%&_{}])", "\\\\\\1", x)
    x <- gsub("~", "\\\\textasciitilde{}", x)
    x <- gsub("\\^", "\\\\textasciicircum{}", x)
    x
  }
  
  pred_df <- predict_property_values_df(
    model = model,
    data = data,
    parcel_col = parcel_col,
    actual_saleprice_col = actual_saleprice_col,
    response_type = response_type,
    response_is_logged = response_is_logged,
    floorarea_col = floorarea_col,
    taf_col = taf_col,
    correction_factor = correction_factor,
    pred_model_col = pred_model_col,
    pred_natural_col = pred_natural_col,
    pred_saleprice_col = pred_saleprice_col,
    pred_sppsf_col = pred_sppsf_col,
    pred_tasp_col = pred_tasp_col,
    saleprice_ratio_col = saleprice_ratio_col,
    saleprice_diff_col = saleprice_diff_col,
    saleprice_pctdiff_col = saleprice_pctdiff_col
  )
  
  diffs <- pred_df[[saleprice_diff_col]]
  pctdiffs <- pred_df[[saleprice_pctdiff_col]]
  
  diffs <- diffs[is.finite(diffs)]
  pctdiffs <- pctdiffs[is.finite(pctdiffs)]
  
  if (length(diffs) == 0) {
    stop("No finite sale price deviations were available to summarize.")
  }
  
  safe_median <- function(x) {
    if (length(x) == 0) return(NA_real_)
    stats::median(x, na.rm = TRUE)
  }
  
  safe_mean <- function(x) {
    if (length(x) == 0) return(NA_real_)
    mean(x, na.rm = TRUE)
  }
  
  safe_sd <- function(x) {
    if (length(x) <= 1) return(NA_real_)
    stats::sd(x, na.rm = TRUE)
  }
  
  safe_rmse <- function(x) {
    if (length(x) == 0) return(NA_real_)
    sqrt(mean(x^2, na.rm = TRUE))
  }
  
  summary_df <- data.frame(
    metric = c(
      "Number of observations",
      "Average sale price deviation",
      "Median sale price deviation",
      "Mean absolute deviation",
      "Median absolute deviation",
      "RMSE",
      "Max deviation",
      "Min deviation",
      "Standard deviation",
      "Average percentage deviation",
      "Median percentage deviation",
      "Average absolute percentage deviation",
      "Median absolute percentage deviation"
    ),
    value = c(
      length(diffs),
      safe_mean(diffs),
      safe_median(diffs),
      safe_mean(abs(diffs)),
      safe_median(abs(diffs)),
      safe_rmse(diffs),
      max(diffs, na.rm = TRUE),
      min(diffs, na.rm = TRUE),
      safe_sd(diffs),
      safe_mean(pctdiffs),
      safe_median(pctdiffs),
      safe_mean(abs(pctdiffs)),
      safe_median(abs(pctdiffs))
    ),
    stringsAsFactors = FALSE
  )
  
  kb <- knitr::kable(
    summary_df,
    format = "latex",
    booktabs = TRUE,
    digits = digits,
    caption = latex_escape(title),
    col.names = c("Metric", "Value"),
    align = c("l", "r"),
    escape = TRUE
  ) %>%
    kableExtra::kable_styling(
      latex_options = c("hold_position"),
      full_width = FALSE
    )
  
  list(
    prediction_data = pred_df,
    summary_data = summary_df,
    kable = kb
  )
}


#res_dev <- summarize_saleprice_deviation_kable(
#  model = mod_time,
#  data = df,
#  title = "Deviation Summary: LN\\_SPPSF Model",
#  response_type = "sppsf",
#  response_is_logged = TRUE,
#  floorarea_col = "sresb_floorarea"
#)
#res_dev$kable

###

make_metrics_kable_lm <- function(
    spss_mod, r_mod, source_col = "metricsource", spss_label = "SPSS",
    r_label = "R", pct_label = "% diff (R vs SPSS)",
    caption = "SPSS and R Metrics Comparison", latex = TRUE, digits = 4, pct_digits = 2,
    max_pct_for_red = 10, include_aic_bic = FALSE
) {
  if (!inherits(spss_mod, "lm")) stop("spss_mod must be an lm object.")
  if (!inherits(r_mod, "lm")) stop("r_mod must be an lm object.")
  if (!is.numeric(max_pct_for_red) || length(max_pct_for_red) != 1L ||
      !is.finite(max_pct_for_red) || max_pct_for_red <= 0) {
    stop("max_pct_for_red must be one positive finite number.")
  }
  
  if (!requireNamespace("dplyr", quietly = TRUE)) {stop("Package 'dplyr' is required.")}
  if (!requireNamespace("knitr", quietly = TRUE)) {stop("Package 'knitr' is required.")}
  if (!requireNamespace("kableExtra", quietly = TRUE)) {stop("Package 'kableExtra' is required.")}
  
  format_type <- if (latex) "latex" else "html"
  # Escape ordinary text only. Do not apply this to strings that already
  # contain LaTeX commands such as \cellcolor or \textcolor.
  pdf_safe <- function(x) {
    x <- as.character(x)
    x <- gsub("\\\\", "\\\\textbackslash{}", x, fixed = TRUE)
    x <- gsub("([#$%&_{}])", "\\\\\\1", x, perl = TRUE)
    x <- gsub("~", "\\\\textasciitilde{}", x, fixed = TRUE)
    x <- gsub("\\^", "\\\\textasciicircum{}", x, perl = TRUE)
    x
  }
  
  extract_lm_metrics <- function(mod) {
    sm <- summary(mod)
    f_val <- NA_real_
    f_num_df <- NA_real_
    f_den_df <- NA_real_
    f_sig <- NA_real_
    
    if (!is.null(sm$fstatistic)) {
      f_val <- unname(sm$fstatistic[["value"]])
      f_num_df <- unname(sm$fstatistic[["numdf"]])
      f_den_df <- unname(sm$fstatistic[["dendf"]])
      f_sig <- stats::pf(f_val, f_num_df, f_den_df, lower.tail = FALSE)
    }
    
    r_squared <- unname(sm$r.squared)
    r_value <- if (is.na(r_squared)) NA_real_ else sqrt(max(r_squared, 0))
    
    out <- data.frame(
      n = stats::nobs(mod), df_resid = stats::df.residual(mod),
      # Effective number of estimable predictors, excluding the intercept.
      df_model = unname(mod$rank - 1L), r = r_value, r_squared = r_squared,
      adj_r_squared = unname(sm$adj.r.squared), SEE = unname(sm$sigma), f_stat = f_val,
      stringsAsFactors = FALSE # sig = f_sig, f_num_df = f_num_df, f_den_df = f_den_df, 
    )
    
    if (include_aic_bic) {
      out$AIC <- as.integer(stats::AIC(mod))
      out$BIC <- as.integer(stats::BIC(mod))
    }
    out
  }
  
  whole_numberish <- function(x, tol = 1e-9) {
    ok <- !is.na(x) & is.finite(x)
    if (!any(ok)) return(FALSE)
    all(abs(x[ok] - round(x[ok])) < tol)
  }
  
  format_metric_values <- function(vals, integer_like, digits) {
    out <- rep("", length(vals))
    ok <- !is.na(vals) & is.finite(vals)
    if (!any(ok)) return(out)
    
    if (integer_like) {out[ok] <- formatC(as.integer(round(vals[ok])), format = "d")
    } else {out[ok] <- formatC(vals[ok], digits = digits, format = "f")}
    out
  }
  
  pct_diff_one <- function(r_val, spss_val) {
    if (is.na(r_val) || is.na(spss_val) ||
        !is.finite(r_val) || !is.finite(spss_val)) {
      return(NA_real_)
    }
    if (spss_val == 0) {
      if (r_val == 0) return(0)
      return(NA_real_)
    }
    ((r_val - spss_val) / abs(spss_val)) * 100
  }
  
  diff_to_color <- function(x, max_pct = max_pct_for_red) {
    out <- rep(NA_character_, length(x))
    ok <- !is.na(x) & is.finite(x)
    if (!any(ok)) return(out)
    
    scaled <- pmin(pmax(abs(x[ok]) / max_pct, 0), 1)
    ramp_mat <- grDevices::colorRamp(
      c("#fc9468", "#fcf868", "#b5fc5d") # c("#1a9850", "#fee08b", "#d73027")
    )(scaled)
    
    out[ok] <- grDevices::rgb(
      ramp_mat[, 1], ramp_mat[, 2], ramp_mat[, 3], maxColorValue = 255
    )
    out
  }
  # This deliberately avoids kableExtra::cell_spec() for PDF output.
  # cell_spec() was generating nested \cellcolor{...}{\textcolor{...}{...}}
  # markup that caused the reported "Missing } inserted" compilation error.
  latex_color_cell <- function(text, background, bold = FALSE) {
    if (is.na(background) || !nzchar(background)) {
      return(pdf_safe(text))
    }
    
    hex <- toupper(gsub("#", "", background, fixed = TRUE))
    text <- pdf_safe(text)
    if (bold) text <- paste0("\\textbf{", text, "}")
    
    paste0("\\cellcolor[HTML]{", hex, "}", "\\textcolor{black}{", text, "}")
  }
  
  spss_df <- extract_lm_metrics(spss_mod)
  r_df <- extract_lm_metrics(r_mod)
  
  spss_out <- spss_df |> dplyr::mutate(!!source_col := spss_label) |>
    dplyr::relocate(dplyr::all_of(source_col))
  
  r_out <- r_df |> dplyr::mutate(!!source_col := r_label) |>
    dplyr::relocate(dplyr::all_of(source_col))
  
  all_cols <- union(names(spss_out), names(r_out))
  
  for (nm in setdiff(all_cols, names(spss_out))) spss_out[[nm]] <- NA
  for (nm in setdiff(all_cols, names(r_out))) r_out[[nm]] <- NA
  
  spss_out <- spss_out[, all_cols, drop = FALSE]
  r_out <- r_out[, all_cols, drop = FALSE]
  
  numeric_compare_cols <- intersect(names(spss_df), names(r_df))
  numeric_compare_cols <- numeric_compare_cols[
    vapply(numeric_compare_cols, function(nm) {
      is.numeric(spss_df[[nm]]) && is.numeric(r_df[[nm]])
    }, logical(1))
  ]
  
  pct_row <- as.list(rep(NA, length(all_cols)))
  names(pct_row) <- all_cols
  pct_row[[source_col]] <- pct_label
  
  for (nm in numeric_compare_cols) {
    pct_row[[nm]] <- pct_diff_one(r_df[[nm]][1], spss_df[[nm]][1])
  }
  
  pct_row <- as.data.frame(pct_row, stringsAsFactors = FALSE)
  merged_df <- dplyr::bind_rows(spss_out, r_out, pct_row)
  display_df <- merged_df
  
  numeric_cols_all <- names(display_df)[
    vapply(display_df, is.numeric, logical(1))
  ]
  numeric_cols_all <- setdiff(numeric_cols_all, source_col)
  
  # Format the SPSS and R rows first. The percent row will be replaced below.
  for (nm in numeric_cols_all) {
    vals <- display_df[[nm]]
    integer_like <- whole_numberish(c(spss_out[[nm]], r_out[[nm]]))
    display_df[[nm]] <- format_metric_values(
      vals, integer_like = integer_like, digits = digits
    )
  }
  
  pct_colors <- stats::setNames(
    diff_to_color(
      unlist(pct_row[1, numeric_compare_cols, drop = TRUE], use.names = FALSE)
    ), numeric_compare_cols
  )
  
  # r_row_index <- 2L
  pct_row_index <- 3L
  
  # Escape only ordinary text before inserting any raw LaTeX commands.
  display_df[[source_col]] <- as.character(display_df[[source_col]])
  if (latex) {
    display_df[[source_col]] <- pdf_safe(display_df[[source_col]])
    display_df[pct_row_index, source_col] <- paste0(
      "\\textbf{", display_df[pct_row_index, source_col], "}"
    )
  }
  
  for (nm in numeric_compare_cols) {
    cell_color <- pct_colors[[nm]]
    pct_val <- pct_row[[nm]][1]
    
    pct_text <- if (is.na(pct_val) || !is.finite(pct_val)) {
      ""
    } else {paste0(formatC(pct_val, digits = pct_digits, format = "f"), "%")}
    
    if (latex) {
      if (!is.na(cell_color) && nzchar(cell_color)) {
        #display_df[r_row_index, nm] <- latex_color_cell(
        #  display_df[r_row_index, nm], background = cell_color, bold = TRUE
        #)
        display_df[pct_row_index, nm] <- latex_color_cell(
          pct_text, background = cell_color, bold = FALSE
        )
      } else {display_df[pct_row_index, nm] <- pdf_safe(pct_text)}
    } else {
      if (!is.na(cell_color) && nzchar(cell_color)) {
        #display_df[r_row_index, nm] <- kableExtra::cell_spec(
        #  display_df[r_row_index, nm], format = "html", background = cell_color, color = "black"
        #)
        display_df[pct_row_index, nm] <- kableExtra::cell_spec(
          pct_text, format = "html", background = cell_color, color = "black", bold = FALSE
        )
      } else {display_df[pct_row_index, nm] <- pct_text}
    }
  }
  
  # if (latex) {names(display_df) <- pdf_safe(names(display_df))}
  
  column_labels <- names(display_df)
  
  if (latex) {    # Escape ordinary column labels first
    column_labels <- pdf_safe(column_labels)    # Replace selected headers with raw LaTeX
    column_labels[names(display_df) == "r_squared"] <- "$R^2$"
    column_labels[names(display_df) == "adj_r_squared"] <- "Adj. $R^2$"
    column_labels[names(display_df) == source_col] <- "Metric Source"
  } else {
    column_labels[names(display_df) == "r_squared"] <- "R²"
    column_labels[names(display_df) == "adj_r_squared"] <- "Adj. R²"
  }
  
  kb <- knitr::kable(
    display_df, caption = if (latex) pdf_safe(caption) else caption, format = format_type,
    booktabs = latex, escape = FALSE, align = c("l", rep("r", ncol(display_df) - 1L)),
    col.names = column_labels
  )
  
  if (latex) {
    # Avoid striped + per-cell backgrounds and avoid row_spec() wrapping raw
    # \cellcolor markup in another formatting command.
    kb <- kableExtra::kable_styling(
      kb, latex_options = c("HOLD_position", "scale_down"), full_width = FALSE
    )
  } else {
    kb <- kableExtra::kable_styling(
      kb, full_width = FALSE
    ) |> kableExtra::row_spec(pct_row_index, bold = FALSE)
  }
  kb <- kb |> kableExtra::row_spec(0:nrow(display_df), bold = TRUE)
  list(data = merged_df, spss_metrics = spss_df, r_metrics = r_df, kable = kb)
}

make_ratio_metrics_kable_lm <- function(
    spss_mod, r_mod, source_col = "metricsource", spss_label = "SPSS", r_label = "R",
    pct_label = "% diff (R vs SPSS)", caption = "Ratio Metrics: SPSS vs R",
    latex = TRUE, digits = 4, pct_digits = 2, max_pct_for_red = 10,
    response_is_logged = TRUE, correction_factor_spss = 1,
    correction_factor_r = 1, ratio_direction = c("pred_over_actual", "actual_over_pred"),
    display_within_as_percent = TRUE
) {
  ratio_direction <- match.arg(ratio_direction)
  
  if (!inherits(spss_mod, "lm")) stop("spss_mod must be an lm object.")
  if (!inherits(r_mod, "lm")) stop("r_mod must be an lm object.")
  
  if (!is.numeric(max_pct_for_red) || length(max_pct_for_red) != 1L ||
      !is.finite(max_pct_for_red) || max_pct_for_red <= 0) {
    stop("max_pct_for_red must be one positive finite number.")
  }
  
  for (nm in c("correction_factor_spss", "correction_factor_r")) {
    val <- get(nm)
    if (!is.numeric(val) || length(val) != 1L || !is.finite(val) || val <= 0) {
      stop(nm, " must be one positive finite number.")
    }
  }
  
  if (!requireNamespace("dplyr", quietly = TRUE)) {stop("Package 'dplyr' is required.")}
  if (!requireNamespace("knitr", quietly = TRUE)) {stop("Package 'knitr' is required.")}
  if (!requireNamespace("kableExtra", quietly = TRUE)) {stop("Package 'kableExtra' is required.")}
  
  format_type <- if (latex) "latex" else "html"
  
  # Escape ordinary text only. This helper must never be applied to strings
  # after raw LaTeX commands such as \cellcolor have been inserted.
  pdf_safe <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    
    # Protect literal backslashes until all other special characters have
    # been escaped, so braces in \textbackslash{} are not escaped again.
    placeholder <- "\u0001BACKSLASH\u0001"
    x <- gsub("\\", placeholder, x, fixed = TRUE)
    x <- gsub("([#$%&_{}])", "\\\\\\1", x, perl = TRUE)
    x <- gsub("~", "\\\\textasciitilde{}", x, fixed = TRUE)
    x <- gsub("\\^", "\\\\textasciicircum{}", x, perl = TRUE)
    x <- gsub(placeholder, "\\\\textbackslash{}", x, fixed = TRUE)
    x
  }
  
  whole_numberish <- function(x, tol = 1e-9) {
    ok <- !is.na(x) & is.finite(x)
    if (!any(ok)) return(FALSE)
    all(abs(x[ok] - round(x[ok])) < tol)
  }
  
  format_metric_values <- function(vals, integer_like, digits) {
    out <- rep("", length(vals))
    ok <- !is.na(vals) & is.finite(vals)
    if (!any(ok)) return(out)
    
    if (integer_like) {out[ok] <- formatC(as.integer(round(vals[ok])), format = "d")
    } else {out[ok] <- formatC(vals[ok], digits = digits, format = "f")}
    out
  }
  
  pct_diff_one <- function(r_val, spss_val) {
    if (is.na(r_val) || is.na(spss_val) ||
        !is.finite(r_val) || !is.finite(spss_val)) {
      return(NA_real_)
    }
    
    if (spss_val == 0) {
      if (r_val == 0) return(0)
      return(NA_real_)
    }
    
    ((r_val - spss_val) / abs(spss_val)) * 100
  }
  
  diff_to_color <- function(x, max_pct = max_pct_for_red) {
    out <- rep(NA_character_, length(x))
    ok <- !is.na(x) & is.finite(x)
    if (!any(ok)) return(out)
    
    scaled <- pmin(pmax(abs(x[ok]) / max_pct, 0), 1)
    ramp_mat <- grDevices::colorRamp(
      c("#fc9468", "#fcf868", "#b5fc5d") # c("#1a9850", "#fee08b", "#d73027")
    )(scaled)
    
    out[ok] <- grDevices::rgb(
      ramp_mat[, 1], ramp_mat[, 2], ramp_mat[, 3], maxColorValue = 255
    )
    out
  }
  
  # PDF-safe colored cell. The supplied text must already be LaTeX escaped.
  # This deliberately avoids kableExtra::cell_spec() for LaTeX output because
  # nested cell_spec/row_spec/striped markup can trigger "Missing } inserted".
  latex_color_cell <- function(escaped_text, background, bold = FALSE) {
    if (is.na(background) || !nzchar(background)) {
      return(if (bold) paste0("\\textbf{", escaped_text, "}") else escaped_text)
    }
    
    hex <- toupper(gsub("#", "", background, fixed = TRUE))
    body <- if (bold) paste0("\\textbf{", escaped_text, "}") else escaped_text
    
    paste0("\\cellcolor[HTML]{", hex, "}", "\\textcolor{black}{", body, "}")
  }
  
  extract_ratio_metrics <- function(mod, correction_factor = 1) {
    mf <- stats::model.frame(mod)
    actual_model_scale <- stats::model.response(mf)
    pred_model_scale <- stats::fitted(mod)
    
    if (response_is_logged) {
      actual <- exp(actual_model_scale)
      pred <- exp(pred_model_scale) * correction_factor
    } else {
      actual <- actual_model_scale
      pred <- pred_model_scale * correction_factor
    }
    
    ok <- is.finite(actual) & is.finite(pred) & !is.na(actual) & !is.na(pred) & actual > 0 & pred > 0
    
    actual <- actual[ok]
    pred <- pred[ok]
    
    if (length(actual) == 0L) {
      stop("No valid actual/predicted pairs were available for ratio metrics.")
    }
    
    ratio <- if (ratio_direction == "pred_over_actual") {
      pred / actual} else {actual / pred}
    
    ratio_ok <- is.finite(ratio) & !is.na(ratio) & ratio > 0
    ratio <- ratio[ratio_ok]
    actual <- actual[ratio_ok]
    pred <- pred[ratio_ok]
    
    if (length(ratio) == 0L) {stop("No valid ratios were available after filtering.")}
    
    median_ratio <- stats::median(ratio, na.rm = TRUE)
    mean_ratio <- mean(ratio, na.rm = TRUE)
    
    wmean_ratio <- if (ratio_direction == "pred_over_actual") {
      sum(pred, na.rm = TRUE) / sum(actual, na.rm = TRUE)
    } else {sum(actual, na.rm = TRUE) / sum(pred, na.rm = TRUE)}
    
    prd <- mean_ratio / wmean_ratio
    cod <- 100 * mean(abs(ratio - median_ratio), na.rm = TRUE) / median_ratio
    
    value_proxy <- if (ratio_direction == "pred_over_actual") {
      0.5 * (actual + (pred / median_ratio))} else {0.5 * (pred + (actual / median_ratio))}
    
    prb_df <- data.frame(
      y = (ratio / median_ratio) - 1, x = log2(value_proxy), stringsAsFactors = FALSE
    )
    
    prb_df <- prb_df[
      is.finite(prb_df$y) & is.finite(prb_df$x) & !is.na(prb_df$y) & !is.na(prb_df$x),
      , drop = FALSE
    ]
    
    prb <- NA_real_
    if (nrow(prb_df) >= 3L && stats::sd(prb_df$x, na.rm = TRUE) > 0) {
      prb <- unname(stats::coef(stats::lm(y ~ x, data = prb_df))[["x"]])
    }
    
    within10pct <- mean(abs(ratio - 1) <= 0.10, na.rm = TRUE)
    within20pct <- mean(abs(ratio - 1) <= 0.20, na.rm = TRUE)
    within50pct <- mean(abs(ratio - 1) <= 0.50, na.rm = TRUE)
    
    data.frame(
      median_ratio = median_ratio, mean_ratio = mean_ratio, wmean_ratio = wmean_ratio,
      PRD = prd, COD = cod, PRB = prb, within10pct = within10pct,
      within20pct = within20pct, within50pct = within50pct, stringsAsFactors = FALSE
    )
  }
  
  spss_df <- extract_ratio_metrics(mod = spss_mod, correction_factor = correction_factor_spss)
  r_df <- extract_ratio_metrics(mod = r_mod, correction_factor = correction_factor_r)
  
  spss_out <- spss_df |> dplyr::mutate(!!source_col := spss_label) |>
    dplyr::relocate(dplyr::all_of(source_col))
  
  r_out <- r_df |> dplyr::mutate(!!source_col := r_label) |>
    dplyr::relocate(dplyr::all_of(source_col))
  
  all_cols <- union(names(spss_out), names(r_out))
  for (nm in setdiff(all_cols, names(spss_out))) spss_out[[nm]] <- NA
  for (nm in setdiff(all_cols, names(r_out))) r_out[[nm]] <- NA
  
  spss_out <- spss_out[, all_cols, drop = FALSE]
  r_out <- r_out[, all_cols, drop = FALSE]
  
  common_cols <- intersect(names(spss_df), names(r_df))
  numeric_compare_cols <- common_cols[
    vapply(
      common_cols,
      function(nm) is.numeric(spss_df[[nm]]) && is.numeric(r_df[[nm]]),
      logical(1)
    )
  ]
  
  pct_row <- as.list(rep(NA, length(all_cols)))
  names(pct_row) <- all_cols
  pct_row[[source_col]] <- pct_label
  
  for (nm in numeric_compare_cols) {
    pct_row[[nm]] <- pct_diff_one(r_df[[nm]][1], spss_df[[nm]][1])
  }
  pct_row <- as.data.frame(pct_row, stringsAsFactors = FALSE)
  
  merged_df <- dplyr::bind_rows(spss_out, r_out, pct_row)
  display_df <- merged_df
  numeric_cols_all <- names(display_df)[vapply(display_df, is.numeric, logical(1))]
  numeric_cols_all <- setdiff(numeric_cols_all, source_col)
  
  # Format the SPSS and R rows. The percent-difference row is overwritten below.
  for (nm in numeric_cols_all) {
    vals <- display_df[[nm]]
    
    if (display_within_as_percent &&
        nm %in% c("within10pct", "within20pct", "within50pct")) {
      display_df[[nm]] <- ifelse(
        is.na(vals) | !is.finite(vals), "",
        paste0(formatC(100 * vals, digits = 2, format = "f"), "%")
      )
    } else {
      integer_like <- whole_numberish(c(spss_out[[nm]], r_out[[nm]]))
      display_df[[nm]] <- format_metric_values(
        vals, integer_like = integer_like, digits = digits
      )
    }
  }
  
  pct_colors <- stats::setNames(
    diff_to_color(
      unlist(pct_row[1, numeric_compare_cols, drop = TRUE], use.names = FALSE)
    ), numeric_compare_cols
  )
  
  # r_row_index <- 2L
  pct_row_index <- 3L
  
  # For PDF, escape every ordinary cell before inserting any raw LaTeX. This
  # includes within-band values containing a literal percent sign.
  if (latex) {
    for (nm in names(display_df)) {display_df[[nm]] <- pdf_safe(display_df[[nm]])}
  } else {display_df[[source_col]] <- as.character(display_df[[source_col]])}
  
  for (nm in numeric_compare_cols) {
    cell_color <- pct_colors[[nm]]
    pct_val <- pct_row[[nm]][1]
    
    pct_text <- if (is.na(pct_val) || !is.finite(pct_val)) {
      ""
    } else {paste0(formatC(pct_val, digits = pct_digits, format = "f"), "%")}
    
    if (latex) {
      pct_text <- pdf_safe(pct_text)
      # display_df[r_row_index, nm] <- latex_color_cell(
      #   display_df[r_row_index, nm], background = cell_color, bold = TRUE
      # )
      display_df[pct_row_index, nm] <- latex_color_cell(
        pct_text, background = cell_color, bold = FALSE
      )
    } else {
      if (!is.na(cell_color) && nzchar(cell_color)) {
        # display_df[r_row_index, nm] <- kableExtra::cell_spec(
        #   display_df[r_row_index, nm], format = "html",
        #   background = cell_color, color = "black"
        # )
        display_df[pct_row_index, nm] <- kableExtra::cell_spec(
          pct_text, format = "html", background = cell_color,
          color = "black", bold = FALSE
        )
      } else {display_df[pct_row_index, nm] <- pct_text}
    }
  }
  
  column_labels <- names(display_df)
  
  if (latex) {
    # Bold all remaining cells in the percent row without row_spec(), which can
    # wrap raw \cellcolor markup and produce malformed LaTeX.
    for (nm in setdiff(names(display_df), numeric_compare_cols)) {
      display_df[pct_row_index, nm] <- paste0("\\textbf{", display_df[pct_row_index, nm], "}")
    }
    # names(display_df) <- pdf_safe(names(display_df))
    column_labels <- pdf_safe(column_labels)
    column_labels[names(display_df) == source_col] <- "Metric Source"
    column_labels[names(display_df) == "within10pct"] <- "Within 10\\%"
    column_labels[names(display_df) == "within20pct"] <- "Within 20\\%"
    column_labels[names(display_df) == "within50pct"] <- "Within 50\\%"
  } else {    # HTML labels
    column_labels[names(display_df) == "within10pct"] <- "Within 10%"
    column_labels[names(display_df) == "within20pct"] <- "Within 20%"
    column_labels[names(display_df) == "within50pct"] <- "Within 50%"
  }
  
  kb <- knitr::kable(
    display_df, caption = if (latex) pdf_safe(caption) else caption, format = format_type,
    booktabs = latex, escape = FALSE, align = c("l", rep("r", ncol(display_df) - 1L)),
    digits = digits, col.names = unname(column_labels)
  )
  
  if (latex) {
    # Do not combine striped rows or row_spec() with per-cell backgrounds.
    kb <- kableExtra::kable_styling(
      kb, latex_options = c("HOLD_position", "scale_down"), full_width = FALSE
    )
  } else {
    kb <- kableExtra::kable_styling(
      kb, full_width = FALSE
    ) |> kableExtra::row_spec(pct_row_index, bold = FALSE)
  }
  kb <- kb |> kableExtra::row_spec(0:nrow(display_df), bold = TRUE)
  list(data = merged_df, spss_metrics = spss_df, r_metrics = r_df, kable = kb)
}




















