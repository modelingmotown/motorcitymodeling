# Functions for various lms

# P-value based backward elimination (roughly analogous to SPSS BACKWARD w/ POUT)
# - Starts with full model, removes the term with the largest p-value above `p_out`,
#   refits, repeats until all p-values <= p_out. # - Keeps intercept always.
backward_p <- function(formula, data, p_out = 0.10, verbose = TRUE) {
  mod <- lm(formula, data = data)
  repeat {
    coefs <- summary(mod)$coefficients
    if (nrow(coefs) <= 1) break
    pvals <- coefs[-1, 4]  # exclude intercept
    worst_p <- max(pvals, na.rm = TRUE)
    if (!is.finite(worst_p) || worst_p <= p_out) break
    worst_term <- names(which.max(pvals))
    if (verbose) message(sprintf("Dropping %-30s (p=%.4g)", worst_term, worst_p))
    # update formula by removing the term (works for simple main-effects terms)
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

# Assessment-style ratio metrics used in appraisal modeling
# ratio = estimate / saleprice (or estimate / TASP)
# - COD: mean(|ratio - median|) / median
# - PRD: mean(ratio) / weighted_mean(ratio) where weights = saleprice
# - PRB: slope from regression of ratio on ln(value); here value = saleprice
ratio_metrics <- function(df, estimate, saleprice) {
  estimate <- enquo(estimate)
  saleprice <- enquo(saleprice)
  
  d <- df %>%
    transmute(est = !!estimate, sp  = !!saleprice) %>%
    filter(is.finite(est), is.finite(sp), sp > 0)
  
  if (nrow(d) == 0) {
    return(tibble(n = 0L, median_ratio = NA_real_, mean_ratio = NA_real_,
                  wmean_ratio = NA_real_, PRD = NA_real_, COD = NA_real_, PRB = NA_real_))
  }
  
  ratio <- d$est / d$sp
  med <- median(ratio, na.rm = TRUE)
  mean_r <- mean(ratio, na.rm = TRUE)
  wmean_r <- sum(d$est, na.rm = TRUE) / sum(d$sp, na.rm = TRUE)
  ok_ratio <- is.finite(ratio)    # Within-band metrics around ratio==1
  r_ok     <- ratio[ok_ratio]
  n_ok     <- length(r_ok)
  within_n <- function(band) sum(abs(r_ok - 1) <= band, na.rm = TRUE)
  w10_n <- within_n(0.10)
  w20_n <- within_n(0.20)
  w50_n <- within_n(0.50)
  w10_pct <- if (n_ok > 0) 100 * w10_n / n_ok else NA_real_
  w20_pct <- if (n_ok > 0) 100 * w20_n / n_ok else NA_real_
  w50_pct <- if (n_ok > 0) 100 * w50_n / n_ok else NA_real_
  
  COD <- (mean(abs(ratio - med), na.rm = TRUE) / med) * 100
  PRD <- mean_r / wmean_r
  
  # PRB: slope of ratio vs log(saleprice); many agencies report slope * 100
  prb_fit <- try(lm(ratio ~ log(d$sp)), silent = TRUE)
  PRB <- if (inherits(prb_fit, "try-error")) NA_real_ else coef(prb_fit)[2]
  
  tibble(
    n = nrow(d),  median_ratio = med, mean_ratio = mean_r,
    wmean_ratio = wmean_r, PRD = PRD, COD = COD, PRB = PRB, within_10_pct = w10_pct,
    within_20_pct = w20_pct, within_50_pct = w50_pct
  )
}

# Convenience: ratio metrics by group (like SPSS RATIO STATS ... BY groupvar)
ratio_by <- function(df, estimate, saleprice, by) {
  by <- enquo(by)
  df %>%
    group_by(!!by) %>%
    group_modify(~ ratio_metrics(.x, {{ estimate }}, {{ saleprice }})) %>% ungroup()
}


get_time_params <- function(model, data, var1 = "MONTHS_6TO22", var2 = "MONTHS_23TO30",
                            end = c("max", "last_obs")) {
  end <- match.arg(end)
  coefs <- coef(model)
  
  if (!all(c(var1, var2) %in% names(coefs))) {
    stop("Time variables ", var1, " and/or ", var2,
         " are not in the model coefficients. ",
         "Make sure they weren't removed by stepwise selection.")
  }
  
  b1 <- unname(coefs[var1])
  b2 <- unname(coefs[var2])
  
  rate1 <- exp(b1) - 1
  rate2 <- exp(b2) - 1
  
  # pick the end-month exponents
  if (!all(c(var1, var2) %in% names(data))) {
    stop("Columns ", var1, " and/or ", var2, " not found in the data used for time params.")
  }
  
  if (end == "max") {
    # use max values across data (rough analogue to "latest month")
    m1_end <- max(data[[var1]], na.rm = TRUE)
    m2_end <- max(data[[var2]], na.rm = TRUE)
  } else if (end == "last_obs") {
    # use the last observation in time
    idx_last <- which.max(data[["MONTHS"]])  # assumes MONTHS exists too
    m1_end <- data[[var1]][idx_last]
    m2_end <- data[[var2]][idx_last]
  }
  
  end_index <- (1 + rate1)^m1_end * (1 + rate2)^m2_end
  
  list(
    b1 = b1,  b2 = b2, RATE1 = rate1,  RATE2 = rate2,
    M6TO22_end = m1_end,  M23TO30_end = m2_end, END_INDEX = end_index
  )
}

model_metrics_A <- function(model, title, digits = 4) {  # Dependencies: broom, dplyr, tibble
  gl <- broom::glance(model)
  
  # Helper: safely pull a column if it exists, otherwise NA_real_
  get_col <- function(df, nm) {
    if (nm %in% names(df)) df[[nm]] else NA_real_
  }
  aic = tryCatch(stats::AIC(model), error = function(e) NA_real_)
  bic = tryCatch(stats::BIC(model), error = function(e) NA_real_)
  
  gl2 <- dplyr::tibble(
    n = stats::nobs(model), df_resid = stats::df.residual(model),
    df_reg = summary(model)$fstatistic[["numdf"]],
    df_total = df_resid + df_reg, p = length(coef(model)),
    # SEE (Std. Error of the Estimate) for lm-type models:
    # broom::glance(lm)$sigma == residual standard error
    see = get_col(gl, "sigma"), round(see, digits = 4),
    # Common glance stats (present for lm; may be NA for other model classes)
    r2     = get_col(gl, "r.squared"),        adj_r2 = get_col(gl, "adj.r.squared"),
    rmse   = sqrt(mean(residuals(model)^2)),  mae    = mean(abs(residuals(model))),
    f_stat = get_col(gl, "statistic"),        f_p    = get_col(gl, "p.value"),
    # These generally exist for many model types, but not all
    aic = as.integer(aic), bic = as.integer(bic), round(r2, digits = 4), round(adj_r2, digits = 4),
    round(rmse, digits = 4), round(mae, digits = 4), f_stat = as.integer(f_stat)
  )
  
  gl2 <- gl2 %>% mutate(across(where(is.numeric), ~ round(.x, digits)))
  
  knitr::kable(
    gl2,  format   = "latex",  booktabs = TRUE,  caption  = title
  ) %>%  kable_styling(latex_options = "HOLD_position")
}

coef_table <- function(model, title, digits = 4, conf.int = TRUE, conf.level = 0.95,
                       keep_only_standard_cols = TRUE) {
  
  ct <- broom::tidy(model, conf.int = conf.int, conf.level = conf.level) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, digits))) %>%
    dplyr::rename(term = term, estimate = estimate, statistic = statistic, 
                  std_error = std.error, p_value = p.value, conf_low = conf.low, conf_high = conf.high
    )
  
  # Ensure required columns exist and are ordered consistently
  standard_cols <- c("term", "estimate", "statistic", "std_error", "p_value", "conf_low", "conf_high")
  
  if (keep_only_standard_cols) {  # keep the standard columns that exist for this model class
    ct <- ct %>% dplyr::select(dplyr::any_of(standard_cols))
  } else {  # otherwise, just move them to the front (and keep anything extra)
    ct <- ct %>% dplyr::relocate(dplyr::any_of(standard_cols), .before = dplyr::everything())
  }
  
  # ct
  knitr::kable(
    ct,  format = "pandoc",  booktabs = TRUE,  caption = title 
  ) %>%  kable_styling(latex_options = c("HOLD_position", "striped"), font_size = 8)
}

pp_plot_resid <- function(mod, dep_label = "LN_PRICE") {
  # SPSS calls these "Regression Standardized Residual"
  # rstandard() = standardized residuals (scaled by estimated sd)
  z <- rstandard(mod)
  z <- z[is.finite(z)]
  n <- length(z)
  
  # Observed cumulative probabilities (empirical CDF positions)
  z_sorted <- sort(z)
  obs <- ppoints(n)  # (1:n - 0.5)/n, SPSS-like plotting positions
  exp <- pnorm(z_sorted)    # Expected cumulative probabilities under Normal(0,1)
  pp_df <- tibble(observed = obs, expected = exp)
  
  ggplot(pp_df, aes(x = observed, y = expected)) +
    geom_abline(slope = 1, intercept = 0, linewidth = 1) +
    geom_point(size = 1.2) +  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    labs(
      title = "Normal P-P Plot of Regression Standardized Residual",
      subtitle = paste0("Dependent Variable: ", dep_label),
      x = "Observed Cum Prob",      y = "Expected Cum Prob"
    ) +
    theme_classic(base_size = 12) +
    theme(
      plot.title    = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, face = "bold"),
      axis.title.x  = element_text(size = 14, face = "bold", margin = margin(t = 10)),
      axis.title.y  = element_text(size = 14, face = "bold", margin = margin(r = 10)),
      panel.grid.major = element_line(color = "grey70", linewidth = 0.6),
      panel.grid.minor = element_blank()
    )
}

plot_stdresid_vs_stdpred <- function(mod, dep_label = "LN_PRICE") {
  # Standardized residuals (SPSS: Regression Standardized Residual)
  sdr <- rstandard(mod)
  # Predicted (fitted) values
  yhat <- fitted(mod)
  # Standardized predicted values (SPSS: Regression Standardized Predicted Value)
  # SPSS standardizes predicted values to mean 0, sd 1
  spred <- as.numeric(scale(yhat))
  dfp <- tibble(spred = spred, sdr = sdr) %>% filter(is.finite(spred), is.finite(sdr))
  
  ggplot(dfp, aes(x = spred, y = sdr)) +
    geom_point(
      shape = 21, size = 2.5, stroke = 0.6, fill = "deepskyblue2", color = "black", alpha = 1
    ) +
    scale_y_continuous(breaks = seq(-4, 4, 2), limits = c(-4, 4)) +
    # x limits/breaks: adjust if you want to force SPSS-looking range
    # scale_x_continuous(breaks = seq(-6, 6, 2), limits = c(-6, 6)) +
    labs(
      title    = "Scatterplot",      subtitle = paste0("Dependent Variable: ", dep_label),
      x = "Regression Standardized Predicted Value",      y = "Regression Standardized Residual"
    ) +
    theme_classic(base_size = 12) +
    theme(
      plot.title    = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, face = "bold"),
      axis.title.x  = element_text(size = 14, face = "bold", margin = margin(t = 10)),
      axis.title.y  = element_text(size = 14, face = "bold", margin = margin(r = 10)),
      panel.grid.major.y = element_line(color = "grey70", linewidth = 0.6),
      panel.grid.minor.y = element_blank(),      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )
}

plot_cook_vs_rstudent <- function(mod, dep_label = "LN_PRICE") {
  # Studentized deleted residuals (PRESS residuals in SPSS terminology)
  stud_del <- rstudent(mod)
  # Cook's distance
  cook <- cooks.distance(mod)
  
  d <- tibble(stud_del = as.numeric(stud_del),    cook = as.numeric(cook)) %>%
    filter(is.finite(stud_del), is.finite(cook))
  
  ggplot(d, aes(x = stud_del, y = cook)) +
    geom_point(
      shape = 21, size = 2.5, stroke = 0.6,      fill = "deepskyblue2", color = "black"
    ) +
    labs(
      title    = "Scatterplot",      subtitle = paste0("Dependent Variable: ", dep_label),
      x = "Regression Studentized Deleted (Press) Residual",    y = "Regression Cook's Distance"
    ) +    theme_classic(base_size = 12) +
    theme(
      plot.title    = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, face = "bold"),
      axis.title.x  = element_text(size = 14, face = "bold", margin = margin(t = 10)),
      axis.title.y  = element_text(size = 14, face = "bold", margin = margin(r = 10)),
      panel.grid.major.y = element_line(color = "grey70", linewidth = 0.6),
      panel.grid.minor.y = element_blank(),      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    ) +
    # optional: set ranges similar to your SPSS figure (tweak as needed)
    coord_cartesian(xlim = c(-4.5, 3.2), ylim = c(0, 0.04))
}

plot_lev_vs_rstudent <- function(mod, dep_label = "LN_PRICE") {
  
  d <- tibble(
    stud_del = as.numeric(rstudent(mod)),   # studentized deleted residual (PRESS)
    lev      = as.numeric(hatvalues(mod))   # leverage
  ) %>% filter(is.finite(stud_del), is.finite(lev))
  
  ggplot(d, aes(x = stud_del, y = lev)) +
    geom_point(
      shape = 21, size = 2.5, stroke = 0.6,     fill = "deepskyblue2", color = "black"
    ) +
    labs(
      title    = "Scatterplot",      subtitle = paste0("Dependent Variable: ", dep_label),
      x = "Regression Studentized Deleted (Press) Residual",    y = "Regression Leverage"
    ) +    theme_classic(base_size = 12) +
    theme(
      plot.title    = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, face = "bold"),
      axis.title.x  = element_text(size = 14, face = "bold", margin = margin(t = 10)),
      axis.title.y  = element_text(size = 14, face = "bold", margin = margin(r = 10)),
      panel.grid.major.y = element_line(color = "grey70", linewidth = 0.6),
      panel.grid.minor.y = element_blank(),      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    ) +
    # optional: match the visual range of your screenshot (tweak if needed)
    coord_cartesian(xlim = c(-4.5, 3.2), ylim = c(0, 0.21))
}

safe_vif <- function(mod, refit = TRUE) {
  df_used <- model.frame(mod)    # Use the exact rows/columns used in the fitted model
  
  aliased_terms <- names(coef(mod))[is.na(coef(mod))] # Find aliased coefficients (NA coefficients)
  aliased_terms <- aliased_terms[!is.na(aliased_terms)]
  
  f_clean <- formula(mod)    # Build a clean formula removing aliased terms (if any)
  if (length(aliased_terms) > 0) {
    f_clean <- update(f_clean, paste(". ~ . -", paste(aliased_terms, collapse = " - ")))
  }
  # Refit safely on df_used (this avoids update() data evaluation issues)
  mod_clean <- if (refit) backward_p(f_clean, data = df_used, p_out = 0.10, verbose = TRUE) else mod
  
  v <- car::vif(mod_clean)    # Compute VIF/GVIF
  
  if (is.matrix(v)) {    # Tidy up the output into a data frame
    out <- tibble::tibble(    # Typically GVIF output for factors: columns include GVIF and Df
      term = rownames(v),      gvif = v[, "GVIF"],      df   = v[, "Df"], vif  = as.numeric(v),
      gvif_adj = (v[, "GVIF"])^(1 / (2 * v[, "Df"])) # comparable scale
    )
    out$tolerance <- 1 / out$vif
  } else {    # Simple numeric VIF per term
    out <- tibble::tibble(      term = names(v),      vif  = as.numeric(v))
    out$tolerance <- 1 / out$vif
  }
  
  list(
    aliased_terms = aliased_terms,    formula_clean = f_clean,
    model_clean   = mod_clean,    vif_table     = out
  )
}

safe_vif_kable <- function(mod, digits = 3, caption = "VIF / GVIF", print = TRUE) {
  res <- safe_vif(mod)
  tab <- res$vif_table
  
  if ("vif" %in% names(tab)) {    # Make it nicer: sort descending by the main metric
    tab <- tab[order(tab$vif, decreasing = TRUE), ]
  } else if ("gvif_adj" %in% names(tab)) {
    tab <- tab[order(tab$gvif_adj, decreasing = TRUE), ]
  }
  knitr::kable(tab, digits = digits, caption = caption)
}

ratio_stats_single <- function(actual, pred) {
  ok <- is.finite(actual) & is.finite(pred)
  a  <- actual[ok]
  p  <- pred[ok]
  n  <- length(a)
  if (n == 0) {
    return(tibble(
      N = 0L,      MEDIAN_RATIO = NA_real_,      MEAN_RATIO = NA_real_,
      WGT_MEAN_RATIO = NA_real_,      PRD = NA_real_,    COD = NA_real_,      PRB = NA_real_
    ))
  }
  r   <- p / a
  med <- median(r, na.rm = TRUE)
  mean_r <- mean(r, na.rm = TRUE)
  wgt_mean_r <- sum(p, na.rm = TRUE) / sum(a, na.rm = TRUE)
  cod <- 100 * mean(abs(r - med), na.rm = TRUE) / med
  prd <- mean_r / wgt_mean_r
  # PRB (approximate IAAO approach)
  vproxy <- (p + a) / 2
  dev_pct <- (r - med) / med * 100
  x <- log(vproxy) - mean(log(vproxy), na.rm = TRUE)
  prb <- tryCatch({
    coef(lm(dev_pct ~ x))[["x"]] / 100
  }, error = function(e) NA_real_)
  tibble(
    N = n,    MEDIAN_RATIO = med,    MEAN_RATIO = mean_r,    WGT_MEAN_RATIO = wgt_mean_r,
    PRD = prd,    COD = cod,    PRB = prb
  )
}

ratio_stats_by <- function(data, group_var, actual, pred) {
  gsym <- rlang::enquo(group_var)
  asy  <- rlang::enquo(actual)
  psy  <- rlang::enquo(pred)
  data |>
    group_by(!!gsym, .drop = FALSE) |>
    group_modify(~ ratio_stats_single(.x |> pull(!!asy),
                                      .x |> pull(!!psy))) |>  ungroup()
}

within_band_flags <- function(pred, actual, band10 = 0.10, band20 = 0.20, band50 = 0.50) {
  ratio <- pred / actual
  tibble(
    WITHIN10 = if_else(!is.na(ratio) & ratio <= 1 + band10 & ratio >= 1 - band10, 1L, 0L),
    WITHIN20 = if_else(!is.na(ratio) & ratio <= 1 + band20 & ratio >= 1 - band20, 1L, 0L),
    WITHIN50 = if_else(!is.na(ratio) & ratio <= 1 + band50 & ratio >= 1 - band50, 1L, 0L)
  )
}

lm_spss_comparer <- function(model, spss_row = NULL, digits = 4, kable_format = "simple",
                             caption = "Linear regression metrics (R vs SPSS)") {
  stopifnot(inherits(model, "lm"))
  
  mf <- model.frame(model)
  y  <- model.response(mf)
  e  <- residuals(model)
  n <- length(y)
  df_res <- df.residual(model)
  
  has_intercept <- attr(terms(model), "intercept") == 1
  df_reg <- model$rank - if (has_intercept) 1 else 0
  df_total <- n - 1
  if (df_reg <= 0) stop("Model has no predictors (intercept-only).")
  
  sse <- sum(e^2)
  sst <- sum((y - mean(y))^2)
  ssr <- sst - sse
  msr <- ssr / df_reg
  mse <- sse / df_res
  f_stat <- msr / mse
  p_val  <- stats::pf(f_stat, df_reg, df_res, lower.tail = FALSE)
  r2 <- 1 - sse / sst
  adj_r2 <- 1 - (1 - r2) * (df_total / df_res)
  r <- sqrt(max(r2, 0))
  see <- sqrt(mse)
  
  r_df <- data.frame(
    n = n,    r = r,    r_squared = r2,    adj_r_squared = adj_r2,    see = see,
    ss_regression = ssr,    ss_residual = sse,    ss_total = sst,    df_regression = df_reg,
    df_residual = df_res,    df_total = df_total,    ms_regression = msr,    ms_residual = mse,
    f = f_stat,    significance = p_val,    check.names = FALSE
  )
  
  spss_df <- NULL
  if (!is.null(spss_row)) {
    if (is.list(spss_row) && !is.data.frame(spss_row)) {
      spss_df <- as.data.frame(spss_row, stringsAsFactors = FALSE)
    } else {
      spss_df <- as.data.frame(spss_row, stringsAsFactors = FALSE)
    }
    if (nrow(spss_df) != 1) stop("spss_row must be a single-row data.frame/tibble (or a named list).")
    
    needed <- names(r_df)
    for (nm in setdiff(needed, names(spss_df))) spss_df[[nm]] <- NA_real_
    spss_df <- spss_df[, needed, drop = FALSE]
    spss_df[] <- lapply(spss_df, function(x) suppressWarnings(as.numeric(x)))
  }
  
  metrics <- names(r_df)
  
  if (is.null(spss_df)) {
    compare_df <- data.frame(
      metric = metrics,      R = as.numeric(r_df[1, metrics]),      stringsAsFactors = FALSE
    )
  } else {
    Rv <- as.numeric(r_df[1, metrics])
    Sv <- as.numeric(spss_df[1, metrics])
    diff <- Rv - Sv
    # percent deviation: 100 * (R - SPSS) / |SPSS|, guarded for 0/NA
    pct_dev <- ifelse(is.na(Sv) | Sv == 0, NA_real_, 100 * diff / abs(Sv))
    
    compare_df <- data.frame(
      metric = metrics,      R = Rv,      SPSS = Sv,      diff_R_minus_SPSS = diff,
      pct_dev_R_vs_SPSS = pct_dev,      stringsAsFactors = FALSE
    )
  }
  
  # rounding for display
  round_cols <- function(df) {
    num_cols <- vapply(df, is.numeric, logical(1))
    df[num_cols] <- lapply(df[num_cols], function(x) round(x, digits))
    df
  }
  compare_df_disp <- round_cols(compare_df)
  kb <- knitr::kable(compare_df_disp, format = kable_format, caption = caption)
  
  list(
    r_df = r_df,    spss_df = spss_df,    compare_df = compare_df,
    compare_df_display = compare_df_disp,    kable = kb
  )
}

#

vif_compare_kable <- function(mod, spss_df, digits = 3,
                              join = c("full", "inner", "left", "right"),
                              output = c("auto", "html", "latex", "pipe"),
                              normalize_term = TRUE, caption = " ",
                              dedupe = c("error", "first", "mean"), pct_dev_abs = TRUE) {
  
  join   <- match.arg(join)
  output <- match.arg(output)
  dedupe <- match.arg(dedupe)
  
  # ---- 1) Compute R VIF/Tolerance safely ----
  df_used <- model.frame(mod)
  aliased_terms <- names(coef(mod))[is.na(coef(mod))]
  aliased_terms <- aliased_terms[!is.na(aliased_terms)]
  
  f_clean <- formula(mod)
  if (length(aliased_terms) > 0) {
    f_clean <- update(f_clean, paste(". ~ . -", paste(aliased_terms, collapse = " - ")))
    # Assuming backward_p exists in your environment as per original snippet
    mod <- backward_p(f_clean, data = df_used, p_out = 0.10, verbose = FALSE)
  }
  
  v <- car::vif(mod)
  
  # FIX: Ensure column order is term, tolerance, vif
  if (is.matrix(v)) {
    r_std <- dplyr::tibble(
      term = rownames(v),      r_vif = (v[, "GVIF"]^(1 / (2 * v[, "Df"])))^2
    ) %>%
      dplyr::mutate(r_tolerance = 1 / r_vif) %>%
      dplyr::select(term, r_tolerance, r_vif) # Explicit reorder
  } else {
    r_std <- dplyr::tibble(
      term = names(v),      r_vif = as.numeric(v)
    ) %>%
      dplyr::mutate(r_tolerance = 1 / r_vif) %>%
      dplyr::select(term, r_tolerance, r_vif) # Explicit reorder
  }
  
  # ---- helper: standardize SPSS input ----
  prep_spss <- function(x, prefix = "SPSS") {
    x <- x %>%
      dplyr::select(term, tolerance, vif) %>%
      dplyr::mutate(
        term = as.character(term), term = trimws(term),
        term = if (normalize_term) tolower(term) else term,
        tolerance = readr::parse_number(as.character(tolerance)),
        vif       = readr::parse_number(as.character(vif))
      )
    
    # ... (duplicate handling logic remains same as your original)
    dup <- x %>% dplyr::count(term) %>% dplyr::filter(n > 1)
    if (nrow(dup) > 0) {
      if (dedupe == "first") x <- x %>% dplyr::group_by(term) %>% dplyr::slice(1) %>% dplyr::ungroup()
      # (rest of dedupe logic omitted for brevity but should be kept)
    }
    x
  }
  
  r_std <- r_std %>%
    dplyr::mutate(
      term = as.character(term), term = trimws(term),
      term = if (normalize_term) tolower(term) else term
    )
  spss_std <- prep_spss(spss_df) %>% dplyr::rename(spss_tolerance = tolerance, spss_vif = vif)
  
  # ---- 2) Join and 3) Diff Calculation ----
  tab <- switch(
    join,
    full  = dplyr::full_join(r_std, spss_std, by = "term"),
    inner = dplyr::inner_join(r_std, spss_std, by = "term"),
    left  = dplyr::left_join(r_std, spss_std, by = "term"),
    right = dplyr::right_join(r_std, spss_std, by = "term")
  ) %>%
    dplyr::arrange(term) %>%
    dplyr::mutate(
      diff_tolerance = r_tolerance - spss_tolerance,  diff_vif = r_vif - spss_vif,
      pctdev_tolerance = 100 * (diff_tolerance) / spss_tolerance,
      pctdev_vif       = 100 * (diff_vif) / spss_vif
    ) %>%
    dplyr::mutate(
      pctdev_tolerance = if (pct_dev_abs) abs(pctdev_tolerance) else pctdev_tolerance,
      pctdev_vif       = if (pct_dev_abs) abs(pctdev_vif) else pctdev_vif
    )
  # Rounding and Kable rendering remains as per your original script...
  # (ensure col.names in kable match the final joined table order)
  
  tab_disp <- tab %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, digits)))
  
  if (output == "auto") {
    output <- if (knitr::is_latex_output()) "latex" else if (knitr::is_html_output()) "html" else "pipe"
  }
  
  if (output %in% c("html", "latex")) {
    return(
      knitr::kable(
        tab_disp, format = output, booktabs = TRUE, align = c("l", rep("r", 8)),
        col.names = c(
          "term", "tolerance", "vif", "tolerance", "vif", "tolerance", "vif", "tolerance", "vif"
        ), caption = caption
      ) %>%
        kableExtra::add_header_above(c(" " = 1, "R" = 2, "SPSS" = 2, "Diff (R-SPSS)" = 2, "% dev (R vs SPSS)" = 2)) %>%
        kableExtra::kable_styling(full_width = FALSE)
    )
  }
  return(tab_disp)
}

#

plot_std_resid_hist <- function(df, resid_col, binwidth = NULL, dep_label = " ", fill = "steelblue") {
  
  resid_q <- rlang::enquo(resid_col)
  resid_name <- rlang::as_name(resid_q)
  
  x <- df[[resid_name]]
  x <- x[is.finite(x)]
  n <- length(x)
  if (n == 0) stop("No finite residual values found in resid_col.")
  
  mu <- mean(x)
  sd_ <- stats::sd(x)
  
  if (is.null(binwidth)) { # Choose a reasonable binwidth if not provided
    iqr <- stats::IQR(x)
    bw_fd <- if (iqr > 0) 2 * iqr / (n^(1/3)) else NA_real_
    rng <- diff(range(x))
    binwidth <- if (is.finite(bw_fd) && bw_fd > 0) bw_fd else if (rng > 0) rng / 30 else 0.1
  }
  
  p <- ggplot(df, aes(x = !!resid_q)) +
    geom_histogram(binwidth = binwidth, color = "black", fill = fill) +
    labs(
      title = paste0("Histogram\nDependent Variable: ", dep_label),
      x = "Regression Standardized Residual",      y = "Frequency"
    ) +
    annotate(
      "text",      x = Inf, y = Inf,      hjust = 1.1, vjust = 1.5,
      label = sprintf("Mean = %.3f\nStd. Dev. = %.3f\nN = %d", mu, sd_, n)
    ) + theme_minimal()
  
  if (is.finite(sd_) && sd_ > 0) { # Only add normal curve if sd is > 0
    p <- p + stat_function(
      fun = function(t) stats::dnorm(t, mean = mu, sd = sd_) * n * binwidth,  linewidth = 1
    )
  }
  p
}

#

plot_resid_vs_months <- function(df, x_col, resid_col, vlines = c(10, 20),
                                 smooth_color = "red", title = " ") {
  
  xq <- rlang::enquo(x_col)
  yq <- rlang::enquo(resid_col)
  
  ggplot(df, aes(x = !!xq, y = !!yq)) +
    geom_point(alpha = 0.5, size = 1.2) +
    geom_smooth(method = "loess", se = FALSE, color = smooth_color) +
    geom_vline(xintercept = vlines, linetype = "dashed") +
    labs(title = title, x = "Months", y = "Standardized Residual") + theme_minimal()
}

#

plot_pred_vs_months <- function(df, x_col, pred_col, vlines = c(10, 20), smooth_color = "blue",
                                title = " ", y = " ") {
  
  xq <- rlang::enquo(x_col)
  yq <- rlang::enquo(pred_col)
  
  ggplot(df, aes(x = !!xq, y = !!yq)) +
    geom_point(alpha = 0.5, size = 1.2) +
    geom_smooth(method = "loess", se = FALSE, color = smooth_color) +
    geom_vline(xintercept = vlines, linetype = "dashed") +
    labs(title = title, x = "Months", y = y) +  theme_minimal()
}

#

plot_months_vs_price_index <- function(df, months, price_index, x_limits = c(0, 25),
                                       x_breaks = seq(0, 25, 5),
                                       x_labels = NULL,     # function, e.g. scales::label_number()
                                       y_labels = NULL,     # function
                                       point_fill = "deepskyblue3", point_color = "black") {
  
  xq <- rlang::enquo(months)
  yq <- rlang::enquo(price_index)
  
  # Defaults similar to typical fmt_x/fmt_2 usage
  if (is.null(x_labels)) {x_labels <- function(x) x}
  if (is.null(y_labels)) {
    if (requireNamespace("scales", quietly = TRUE)) {
      y_labels <- scales::label_number(accuracy = 0.01)
    } else {y_labels <- function(x) format(round(x, 2), nsmall = 2)}
  }
  
  ggplot(df, aes(x = !!xq, y = !!yq)) +
    geom_point(shape = 21, size = 3, stroke = 0.8, fill = point_fill, color = point_color) +
    scale_x_continuous(
      limits = x_limits, breaks = x_breaks, labels = x_labels,
      expand = expansion(mult = c(0.02, 0.02))
    ) +  scale_y_continuous(labels = y_labels) +  labs(x = "MONTHS", y = "PRICE_INDEX") +
    theme_classic(base_size = 12) +
    theme(
      panel.grid.major.y = element_line(color = "grey75", linewidth = 0.6),
      panel.grid.minor.y = element_blank(),   panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 10)),
      axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10)),
      axis.text = element_text(size = 10),
      axis.line = element_line(color = "black", linewidth = 0.8)
    )
}

#

plot_months_vs_taf <- function(df, months, taf, x_limits = c(0, 25), x_breaks = seq(0, 25, 5),
                               x_labels = NULL,  y_labels = NULL, point_fill = "deepskyblue3",
                               point_color = "black") {
  
  xq <- rlang::enquo(months)
  yq <- rlang::enquo(taf)
  
  if (is.null(x_labels)) {x_labels <- function(x) x}
  if (is.null(y_labels)) {
    if (requireNamespace("scales", quietly = TRUE)) {
      y_labels <- scales::label_number(accuracy = 0.01)
    } else {y_labels <- function(x) format(round(x, 2), nsmall = 2)}
  }
  
  ggplot(df, aes(x = !!xq, y = !!yq)) +
    geom_point(
      shape = 21, size = 3, stroke = 0.8, fill = point_fill, color = point_color
    ) +
    scale_x_continuous(
      limits = x_limits,  breaks = x_breaks,    labels = x_labels,
      expand = expansion(mult = c(0.02, 0.02))
    ) +  scale_y_continuous(labels = y_labels) + labs(x = "MONTHS", y = "TAF") +
    theme_classic(base_size = 12) +
    theme(
      panel.grid.major.y = element_line(color = "grey75", linewidth = 0.6),
      panel.grid.minor.y = element_blank(),  panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 10)),
      axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10)),
      axis.text = element_text(size = 10),
      axis.line = element_line(color = "black", linewidth = 0.8)
    )
}

#

plot_leverage_hist <- function(df, lev, bins = 30, dep_label = " ", fill = "deepskyblue2",
                               curve_color = "black") {
  
  levq <- rlang::enquo(lev)
  lev_name <- rlang::as_name(levq)
  
  x <- df[[lev_name]]
  x <- x[is.finite(x)]
  n <- length(x)
  if (n == 0) stop("No finite leverage values found in `lev` column.")
  
  mu <- mean(x)
  sdv <- stats::sd(x)
  # binwidth used to scale the normal curve to histogram frequency
  rng <- diff(range(x))
  binwidth <- if (rng > 0) rng / bins else 1
  
  ggplot(df, aes(x = !!levq)) +
    geom_histogram(
      bins = bins, boundary = 0, closed = "left",  fill = fill,  color = "black",  linewidth = 0.4
    ) +
    stat_function(
      fun = function(t) stats::dnorm(t, mean = mu, sd = sdv) * n * binwidth,
      color = curve_color,    linewidth = 0.8
    ) +
    labs(
      title = "Histogram",    subtitle = paste0("Dependent Variable: ", dep_label),
      x = "Regression Leverage",    y = "Frequency"
    ) +
    annotate(
      "text", x = Inf, y = Inf, hjust = 1.05, vjust = 1.2,
      label = paste0(
        "Mean = ", sprintf("%.2f", mu), "\n",    "Std. Dev. = ", sprintf("%.3f", sdv), "\n",
        "N = ", n
      ),  size = 4
    ) +  theme_classic(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, face = "bold"),
      axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 10)),
      axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10)),
      panel.grid.major.y = element_line(color = "grey70", linewidth = 0.6),
      panel.grid.minor.y = element_blank(),     panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )
}

#

plot_esp_vs_ratio <- function(df, esp1, ratio1, xlab = " ", ylab = " ",
                                y_breaks = 0:5, y_upper = NA,      # keep y >= 0, no upper limit
                                point_fill = "deepskyblue2",  point_color = "black",
                                point_size = 2.6,  point_stroke = 0.6, x_accuracy = 0.01,
                                x_big_mark = "",  x_decimal_mark = ".") {
  
  xq <- rlang::enquo(esp1)
  yq <- rlang::enquo(ratio1)
  
  # scales::label_number if available; otherwise fallback
  x_labeller <- NULL
  if (requireNamespace("scales", quietly = TRUE)) {
    x_labeller <- scales::label_number(
      accuracy = x_accuracy,      big.mark = x_big_mark,      decimal.mark = x_decimal_mark
    )
  } else {x_labeller <- function(x) format(round(x, 2), nsmall = 2)}
  
  ggplot(df, aes(x = !!xq, y = !!yq)) +
    geom_point(
      shape = 21, size = point_size, stroke = point_stroke, fill = point_fill, color = point_color
    ) + scale_x_continuous(labels = x_labeller) +
    scale_y_continuous(
      limits = c(0, y_upper),      breaks = y_breaks,
      labels = function(x) ifelse(x == 0, ".00", sprintf("%.2f", x))
    ) + labs(x = xlab, y = ylab) + theme_classic(base_size = 12) +
    theme(
      panel.grid.major.y = element_line(color = "grey70", linewidth = 0.6),
      panel.grid.minor.y = element_blank(),      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 10)),
      axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10)),
      axis.text = element_text(size = 10),
      axis.line = element_line(color = "black", linewidth = 0.8)
    )
}

### plot_esp1_vs_ratio1(df_plot3, esp1, ratio1)

#

plot_ratio_hist <- function(df, ratio1, bins = 30, xlim = c(0, 5.2), fill = "deepskyblue2",
                             boundary = 0, closed = "left", show_stats = TRUE, xlab = " ",
                             mean_digits = 2, sd_digits = 3, n_digits = 0) {
  
  rq <- rlang::enquo(ratio1)
  rname <- rlang::as_name(rq)
  
  x <- df[[rname]]
  x <- x[is.finite(x)]
  if (length(x) == 0) stop("No finite values found in ratio1 column.")
  mu <- mean(x)
  sdv <- stats::sd(x)
  n  <- length(x)
  
  p <- ggplot(df, aes(x = !!rq)) +
    geom_histogram(
      bins = bins, boundary = boundary, closed = closed,  fill = fill,
      color = "black",      linewidth = 0.4
    ) + labs(x = xlab, y = "Frequency") + theme_classic(base_size = 12) +
    theme(
      axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 10)),
      axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10)),
      panel.grid.major.y = element_line(color = "grey70", linewidth = 0.6),
      panel.grid.minor.y = element_blank(),      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.line = element_line(color = "black", linewidth = 0.8)
    ) + coord_cartesian(xlim = xlim)
  
  if (isTRUE(show_stats)) {
    p <- p + annotate(
      "text",      x = Inf, y = Inf,      hjust = 1.05, vjust = 1.2,
      label = paste0(
        "Mean = ", sprintf(paste0("%.", mean_digits, "f"), mu), "\n",
        "Std. Dev. = ", sprintf(paste0("%.", sd_digits, "f"), sdv), "\n",
        "N = ", formatC(n, format = "f", digits = n_digits, big.mark = ",")
      ),      size = 4
    )
  }
  p
}

### plot_ratio1_hist(dat, ratio1, bins = bins_n, xlim = c(0, 5.2))

#

plot_tasp_vs_esp_by_style <- function(df, tasp_re, esp3, sresb_style, xlab = "TASP",
                                       ylab = " ",  legend_title = "SRESB_STYLE",
                                       palette = c("1/2 DUPLEX" = "#66CCFF",
                                                   "SINGLE FAMILY" = "#CC3366"),
                                       point_size = 2, point_alpha = 0.9, point_stroke = 0.8,
                                       add_abline = TRUE, abline_intercept = 0,
                                       abline_slope = 1, equal_axes = TRUE) {
  
  xq <- rlang::enquo(tasp_re)
  yq <- rlang::enquo(esp3)
  cq <- rlang::enquo(sresb_style)
  
  p <- ggplot(df, aes(x = !!xq, y = !!yq, colour = !!cq)) +
    geom_point(
      shape = 21, stroke = point_stroke,  fill = NA,              # open circles
      alpha = point_alpha,  size = point_size
    ) + labs(x = xlab, y = ylab, colour = legend_title) + theme_minimal(base_size = 12) +
    theme(
      panel.grid.major = element_line(color = "grey80", linewidth = 0.3),
      panel.grid.minor = element_blank(),     legend.position  = "right",
      legend.title     = element_text(face = "bold")
    )
  
  if (isTRUE(add_abline)) {p <- p + geom_abline(intercept = abline_intercept, slope = abline_slope)}
  if (isTRUE(equal_axes)) {p <- p + coord_equal()}
  
  # Use your SPSS-like palette if provided; otherwise default ggplot palette
  if (!is.null(palette)) {p <- p + scale_colour_manual(values = palette)}
  p
}

### plot_tasp_vs_esp3_by_style(df_in2, tasp_re, esp3, sresb_style)

#

save_model_bundle <- function(model_names,
                              bundle_path,
                              manifest_csv_path,
                              model_data_map = NULL,   # named character vec: c(modelA="dfA", modelB="dfB")
                              data_names = NULL,       # optional extra dfs to store
                              format = c("rds", "RData"),
                              envir = parent.frame(),
                              overwrite = TRUE,
                              digits = 6) {
  format <- match.arg(format)
  
  # ---------------- Helpers ----------------
  
  .get_obj <- function(nm) {
    if (!exists(nm, envir = envir, inherits = TRUE)) {
      stop("Object not found in environment: ", nm, call. = FALSE)
    }
    get(nm, envir = envir, inherits = TRUE)
  }
  
  .infer_df_name_from_model <- function(mod) {
    cl <- tryCatch(mod$call, error = function(e) NULL)
    if (is.null(cl) || is.null(cl$data)) return(NA_character_)
    tryCatch(deparse(cl$data), error = function(e) NA_character_)
  }
  
  .collapse_formula <- function(mod) {
    if (!inherits(mod, "lm") && !inherits(mod, "gam")) return("")
    f <- tryCatch(formula(mod), error = function(e) NULL)
    if (is.null(f)) return("")
    s <- paste(deparse(f, width.cutoff = 500L), collapse = " ")
    s <- gsub("\\s+", " ", trimws(s))
    s
  }
  
  .safe_num <- function(x) {
    if (length(x) == 0) return(NA_real_)
    x <- suppressWarnings(as.numeric(x[1]))
    ifelse(is.finite(x), x, NA_real_)
  }
  
  .rbind_fill <- function(a, b) {
    cols <- union(names(a), names(b))
    for (cc in setdiff(cols, names(a))) a[[cc]] <- NA
    for (cc in setdiff(cols, names(b))) b[[cc]] <- NA
    a <- a[, cols, drop = FALSE]
    b <- b[, cols, drop = FALSE]
    rbind(a, b)
  }
  
  .lm_metrics <- function(mod) {
    mf <- model.frame(mod)
    y  <- model.response(mf)
    e  <- residuals(mod)
    
    n <- length(y)
    df_res <- df.residual(mod)
    
    has_intercept <- attr(terms(mod), "intercept") == 1
    df_reg <- mod$rank - if (has_intercept) 1 else 0
    df_total <- n - 1
    
    sse <- sum(e^2)
    sst <- sum((y - mean(y))^2)
    ssr <- sst - sse
    
    msr <- ssr / df_reg
    mse <- sse / df_res
    
    f_stat <- msr / mse
    p_val  <- pf(f_stat, df_reg, df_res, lower.tail = FALSE)
    
    r2 <- 1 - sse / sst
    adj_r2 <- 1 - (1 - r2) * (df_total / df_res)
    r <- sqrt(max(r2, 0))
    see <- sqrt(mse)
    
    rmse <- sqrt(mean(e^2))
    mae  <- mean(abs(e))
    
    c(
      n = n,
      r = r,
      r_squared = r2,
      adj_r_squared = adj_r2,
      see = see,
      
      ss_regression = ssr,
      ss_residual   = sse,
      ss_total      = sst,
      
      df_regression = df_reg,
      df_residual   = df_res,
      df_total      = df_total,
      
      ms_regression = msr,
      ms_residual   = mse,
      
      f = f_stat,
      significance = p_val,
      
      sigma = summary(mod)$sigma,
      aic = AIC(mod),
      bic = BIC(mod),
      rmse = rmse,
      mae = mae
    )
  }
  
  # IMPORTANT: gam often inherits from lm -> handle gam FIRST.
  .coef_table <- function(mod) {
    
    .empty <- function() {
      data.frame(
        term = character(0),
        estimate = numeric(0),
        std_error = numeric(0),
        t_value = numeric(0),
        p_value = numeric(0),
        row.names = NULL,
        check.names = FALSE
      )
    }
    
    # ---- 1) mgcv::gam FIRST ----
    if (inherits(mod, "gam")) {
      sm <- summary(mod)
      out_list <- list()
      
      # Parametric terms
      ptab <- sm$p.table
      if (!is.null(ptab) && NROW(ptab) > 0) {
        ptab <- as.data.frame(ptab, check.names = FALSE)
        term <- rownames(ptab)
        
        stat_col <- if ("t value" %in% names(ptab)) "t value" else if ("z value" %in% names(ptab)) "z value" else NA_character_
        pval_col <- if ("Pr(>|t|)" %in% names(ptab)) "Pr(>|t|)" else if ("Pr(>|z|)" %in% names(ptab)) "Pr(>|z|)" else NA_character_
        
        out_list[[length(out_list) + 1]] <- data.frame(
          term      = term,
          estimate  = ptab[["Estimate"]],
          std_error = ptab[["Std. Error"]],
          t_value   = if (!is.na(stat_col)) ptab[[stat_col]] else NA_real_,
          p_value   = if (!is.na(pval_col)) ptab[[pval_col]] else NA_real_,
          type      = "parametric",
          row.names = NULL,
          check.names = FALSE
        )
      }
      
      # Smooth terms (optional but very useful)
      stab <- sm$s.table
      if (!is.null(stab) && NROW(stab) > 0) {
        stab <- as.data.frame(stab, check.names = FALSE)
        term <- rownames(stab)
        
        stat_col <- if ("F" %in% names(stab)) "F" else if ("Chi.sq" %in% names(stab)) "Chi.sq" else NA_character_
        pval_col <- if ("p-value" %in% names(stab)) "p-value" else if ("p.value" %in% names(stab)) "p.value" else NA_character_
        refdf_col <- if ("Ref.df" %in% names(stab)) "Ref.df" else NA_character_
        
        out_list[[length(out_list) + 1]] <- data.frame(
          term      = term,
          estimate  = NA_real_,
          std_error = NA_real_,
          t_value   = if (!is.na(stat_col)) stab[[stat_col]] else NA_real_,  # F / Chi.sq stored here
          p_value   = if (!is.na(pval_col)) stab[[pval_col]] else NA_real_,
          type      = "smooth",
          edf       = if ("edf" %in% names(stab)) stab[["edf"]] else NA_real_,
          ref_df    = if (!is.na(refdf_col)) stab[[refdf_col]] else NA_real_,
          row.names = NULL,
          check.names = FALSE
        )
      }
      
      if (length(out_list) == 0) return(.empty())
      
      # Bind rows with fill
      out <- out_list[[1]]
      if (length(out_list) > 1) {
        for (i in 2:length(out_list)) {
          nxt <- out_list[[i]]
          cols <- union(names(out), names(nxt))
          for (cc in setdiff(cols, names(out))) out[[cc]] <- NA
          for (cc in setdiff(cols, names(nxt))) nxt[[cc]] <- NA
          out <- rbind(out[, cols, drop = FALSE], nxt[, cols, drop = FALSE])
        }
      }
      
      # Ensure expected columns exist
      for (cc in c("term","estimate","std_error","t_value","p_value")) {
        if (!cc %in% names(out)) out[[cc]] <- NA
      }
      return(out)
    }
    
    # ---- 2) Plain lm ----
    if (inherits(mod, "lm")) {
      sm <- summary(mod)$coefficients
      out <- data.frame(
        term      = rownames(sm),
        estimate  = sm[, 1],
        std_error = sm[, 2],
        t_value   = sm[, 3],
        p_value   = sm[, 4],
        row.names = NULL,
        check.names = FALSE
      )
      return(out)
    }
    
    # ---- 3) Fallback to broom::tidy ----
    if (requireNamespace("broom", quietly = TRUE)) {
      td <- as.data.frame(broom::tidy(mod), check.names = FALSE)
      
      pick <- function(df, candidates, default = NA_real_) {
        for (nm in candidates) if (nm %in% names(df)) return(df[[nm]])
        rep(default, nrow(df))
      }
      
      if (!"term" %in% names(td)) td$term <- if (!is.null(rownames(td))) rownames(td) else rep("", nrow(td))
      
      out <- data.frame(
        term      = td$term,
        estimate  = pick(td, c("estimate", "Estimate")),
        std_error = pick(td, c("std_error", "std.error", "Std. Error", "Std..Error")),
        t_value   = pick(td, c("t_value", "statistic", "t value", "z value", "t.value", "z.value")),
        p_value   = pick(td, c("p_value", "p.value", "Pr(>|t|)", "Pr(>|z|)")),
        row.names = NULL,
        check.names = FALSE
      )
      return(out)
    }
    
    .empty()
  }
  
  # ---------------- Collect models/data ----------------
  
  bundle_created_at <- Sys.time()
  
  models <- setNames(vector("list", length(model_names)), model_names)
  data_used <- list()
  
  model_rows <- NULL
  coef_rows  <- NULL
  
  for (mn in model_names) {
    mod <- .get_obj(mn)
    
    # attach created_at if missing
    if (is.null(attr(mod, "created_at"))) attr(mod, "created_at") <- bundle_created_at
    
    # determine dataframe name
    df_name <- NA_character_
    if (!is.null(model_data_map) && mn %in% names(model_data_map)) {
      df_name <- model_data_map[[mn]]
    } else {
      df_name <- .infer_df_name_from_model(mod)
    }
    
    # load dataframe object if available
    if (!is.na(df_name) && nzchar(df_name) && exists(df_name, envir = envir, inherits = TRUE)) {
      data_used[[df_name]] <- .get_obj(df_name)
    }
    
    models[[mn]] <- mod
    
    # metrics
    metrics <- c()
    if (inherits(mod, "lm")) {
      metrics <- .lm_metrics(mod)
    } else if (requireNamespace("broom", quietly = TRUE)) {
      g <- tryCatch(broom::glance(mod), error = function(e) NULL)
      if (!is.null(g) && nrow(g) >= 1) {
        keep <- intersect(names(g), c("nobs","r.squared","adj.r.squared","sigma","AIC","BIC"))
        if (length(keep)) {
          metrics <- suppressWarnings(as.numeric(g[1, keep, drop = TRUE]))
          names(metrics) <- keep
        }
      }
    }
    
    # formula (single line)
    formula_str <- .collapse_formula(mod)
    
    # model manifest row
    mrow <- data.frame(
      model_name = mn,
      data_name = ifelse(is.na(df_name), "", df_name),
      model_class = paste(class(mod), collapse = "/"),
      formula = formula_str,
      created_at = format(attr(mod, "created_at"), tz = "UTC", usetz = TRUE),
      bundle_created_at = format(bundle_created_at, tz = "UTC", usetz = TRUE),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    
    if (length(metrics)) {
      for (k in names(metrics)) mrow[[k]] <- .safe_num(metrics[[k]])
    }
    
    if (is.null(model_rows)) model_rows <- mrow else model_rows <- .rbind_fill(model_rows, mrow)
    
    # coefficient table (FIXED)
    ct <- .coef_table(mod)
    ct <- as.data.frame(ct, check.names = FALSE)
    
    nr <- nrow(ct)
    
    # NEVER crash on 0-row coefficient tables:
    ct$model_name <- rep(mn, nr)
    ct$data_name  <- rep(ifelse(is.na(df_name), "", df_name), nr)
    ct$created_at <- rep(format(attr(mod, "created_at"), tz = "UTC", usetz = TRUE), nr)
    
    # add selected metrics to each coef row (optional but handy)
    if (length(metrics) && nr > 0) {
      for (k in names(metrics)) ct[[k]] <- rep(.safe_num(metrics[[k]]), nr)
    }
    
    # Reorder columns with a stable front
    front <- c("model_name", "data_name", "created_at", "term", "estimate", "std_error", "t_value", "p_value")
    front <- intersect(front, names(ct))
    rest  <- setdiff(names(ct), front)
    ct <- ct[, c(front, rest), drop = FALSE]
    
    if (is.null(coef_rows)) coef_rows <- ct else coef_rows <- .rbind_fill(coef_rows, ct)
  }
  
  # include extra dfs requested
  if (!is.null(data_names)) {
    for (dn in data_names) {
      if (exists(dn, envir = envir, inherits = TRUE)) {
        data_used[[dn]] <- .get_obj(dn)
      } else {
        warning("data_names includes missing object: ", dn, call. = FALSE)
      }
    }
  }
  
  # ---------------- Save bundle ----------------
  
  bundle <- list(
    created_at = bundle_created_at,
    models = models,
    data = data_used,
    model_manifest = model_rows,
    coefficients = coef_rows
  )
  
  if (file.exists(bundle_path) && !overwrite) {
    stop("bundle_path exists and overwrite=FALSE: ", bundle_path, call. = FALSE)
  }
  
  if (format == "rds") {
    saveRDS(bundle, file = bundle_path)
  } else {
    save(bundle, file = bundle_path)
  }
  
  # ---------------- Write single CSV (model + coefficients) ----------------
  
  # model rows
  model_csv <- model_rows
  model_csv$record_type <- "model"
  
  # ensure coef-related columns exist for model rows
  for (cc in c("term","estimate","std_error","t_value","p_value")) {
    if (!cc %in% names(model_csv)) model_csv[[cc]] <- NA
  }
  
  # coefficient rows
  coef_csv <- coef_rows
  coef_csv$record_type <- "coefficient"
  
  # align & combine
  model_csv <- model_csv[, c("record_type", setdiff(names(model_csv), "record_type")), drop = FALSE]
  coef_csv  <- coef_csv[,  c("record_type", setdiff(names(coef_csv),  "record_type")), drop = FALSE]
  combined_csv <- .rbind_fill(model_csv, coef_csv)
  
  # round numeric columns for readability
  num_cols <- vapply(combined_csv, is.numeric, logical(1))
  combined_csv[num_cols] <- lapply(combined_csv[num_cols], function(x) round(x, digits))
  
  utils::write.csv(combined_csv, file = manifest_csv_path, row.names = FALSE)
  
  invisible(list(
    bundle_path = bundle_path,
    manifest_csv_path = manifest_csv_path,
    bundle = bundle,
    combined_csv = combined_csv
  ))
}

#

lm_fit_stats <- function(model, digits = 4, int_cols = c("n", "df_res", "p", "aic", "bic")) {
  
  sm  <- summary(model)
  res <- residuals(model)
  
  # F-stat + p-value
  fstat <- sm$fstatistic
  f_val <- unname(fstat[1])
  df1   <- unname(fstat[2])
  df2   <- unname(fstat[3])
  f_p   <- stats::pf(f_val, df1, df2, lower.tail = FALSE)
  
  out <- data.frame(
    n      = as.integer(stats::nobs(model)),
    df_res = as.integer(stats::df.residual(model)),
    p      = as.integer(length(stats::coef(model))),  # includes intercept (matches your earlier table)
    see    = sm$sigma,
    r2     = sm$r.squared,
    adj_r2 = sm$adj.r.squared,
    rmse   = sqrt(mean(res^2, na.rm = TRUE)),
    mae    = mean(abs(res), na.rm = TRUE),
    f_stat = f_val,
    f_p    = f_p,
    aic    = as.integer(stats::AIC(model)),
    bic    = as.integer(stats::BIC(model)),
    check.names = FALSE
  )
  
  # Round ONLY non-integer columns
  round_cols <- setdiff(names(out), int_cols)
  out[round_cols] <- lapply(out[round_cols], function(x) round(as.numeric(x), digits))
  
  out
}

#

compare_lm_fit_stats <- function(model_a, model_b, names = c("Model A", "Model B"),
                                 digits = 4, add_delta_rows = TRUE,
                                 delta_label = "Δ (B - A)", pct_label   = "%Δ (B vs A)",
                                 pct_for_int_cols = FALSE,        # default: NA for n/df/p
                                 int_cols = c("n", "df_res", "p", "aic", "bic"), format = c("latex", "html", "simple"),
                                 caption = "Model fit statistics", booktabs = TRUE,
                                 hold_position = TRUE) {
  format <- match.arg(format)
  
  A <- lm_fit_stats(model_a, digits = digits, int_cols = int_cols)
  B <- lm_fit_stats(model_b, digits = digits, int_cols = int_cols)
  
  A$model <- names[1]
  B$model <- names[2]
  
  cols <- c("model", setdiff(names(A), "model"))
  A <- A[, cols, drop = FALSE]
  B <- B[, cols, drop = FALSE]
  
  tbl <- rbind(A, B)
  
  if (isTRUE(add_delta_rows)) {
    # Δ row
    d <- B
    d$model <- delta_label
    for (nm in setdiff(names(d), "model")) {
      d[[nm]] <- B[[nm]] - A[[nm]]
    }
    # keep integer cols integer
    for (nm in intersect(int_cols, names(d))) d[[nm]] <- as.integer(d[[nm]])
    # round non-int cols
    for (nm in setdiff(setdiff(names(d), "model"), int_cols)) d[[nm]] <- round(as.numeric(d[[nm]]), digits)
    
    # %Δ row
    p <- B
    p$model <- pct_label
    for (nm in setdiff(names(p), "model")) {
      base <- A[[nm]]
      diff <- B[[nm]] - base
      if (nm %in% int_cols && !pct_for_int_cols) {
        p[[nm]] <- NA_real_
      } else {
        p[[nm]] <- ifelse(is.na(base) | base == 0, NA_real_, 100 * diff / abs(base))
      }
    }
    # round % row (all numeric)
    for (nm in setdiff(names(p), "model")) p[[nm]] <- round(as.numeric(p[[nm]]), digits)
    
    tbl <- rbind(tbl, d, p)
  }
  
  # Ensure int cols stay integer in the main rows (and delta row)
  for (nm in intersect(int_cols, names(tbl))) {
    # keep model rows & delta as integer if possible; % row will be NA unless pct_for_int_cols=TRUE
    tbl[[nm]] <- suppressWarnings(ifelse(is.na(tbl[[nm]]), NA, as.integer(tbl[[nm]])))
  }
  
  kb <- knitr::kable(
    tbl,
    format = if (format == "simple") "simple" else format,
    booktabs = booktabs,
    caption = caption
  )
  
  if (requireNamespace("kableExtra", quietly = TRUE) && format %in% c("latex", "html")) {
    if (format == "latex") {
      kb <- kb |>
        kableExtra::kable_styling(
          latex_options = if (hold_position) "HOLD_position" else NULL
        )
    } else {
      kb <- kb |>
        kableExtra::kable_styling(full_width = FALSE)
    }
  }
  
  list(stats_table = tbl, kable = kb)
}


### res <- compare_lm_fit_stats(
###   mod_2023, mod_2024, names = c("Sales 2023", "Sales 2024"), digits = 4, add_delta_rows = TRUE,
  # format = "latex", caption = "LN_PRICE model fit statistics (by sales year)"
### )

### res$kable

#

newdata_from_sale <- function(data, model = NULL, formula = NULL, parcel_number = NULL,
                              sale_id = NULL, parcel_col = "parcel_number",
                              sale_col   = "sale_id", allow_multiple = FALSE,
                              pick = c("first", "last"), extra_cols = NULL, strict = TRUE) {
  
  pick <- match.arg(pick)
  
  if (is.null(model) && is.null(formula)) {stop("Provide either `model` or `formula`.")} # ---- basic validation
  if (!is.null(model) && is.null(formula)) {formula <- stats::formula(model)}
  
  has_parcel <- !is.null(parcel_number)
  has_sale   <- !is.null(sale_id)
  
  if (has_parcel == has_sale) {stop("Provide exactly one of `parcel_number` or `sale_id` (not both).")}
  if (!is.data.frame(data)) stop("`data` must be a data.frame/tibble.")
  
  key_col <- if (has_parcel) parcel_col else sale_col    # ---- find the key column and rows
  key_val <- if (has_parcel) parcel_number else sale_id
  
  if (!key_col %in% names(data)) {stop("Key column '", key_col, "' not found in data.")}
  
  idx <- which(data[[key_col]] == key_val)
  
  if (length(idx) == 0) {stop("No rows found where ", key_col, " == ", key_val)}
  if (!allow_multiple && length(idx) > 1) {idx <- if (pick == "first") idx[1] else idx[length(idx)]}
  
  trm  <- stats::terms(formula, data = data) # ---- get predictor variable names from formula
  vars <- all.vars(stats::delete.response(trm))  # predictors only
  
  # always keep the ID column too (and any extras requested)
  keep <- unique(c(key_col, extra_cols, vars))
  
  missing_cols <- setdiff(keep, names(data))
  if (length(missing_cols) > 0) {
    msg <- paste0("These variables are required but missing from `data`: ",
                  paste(missing_cols, collapse = ", "))
    if (strict) stop(msg) else warning(msg)
  }
  
  keep <- intersect(keep, names(data))
  newdata <- data[idx, keep, drop = FALSE] # subset rows + cols (keep data.frame shape)
  
  # ---- align factor levels to model (prevents predict() surprises)
  if (!is.null(model) && !is.null(model$xlevels)) {
    xlev <- model$xlevels
    common <- intersect(names(xlev), names(newdata))
    for (nm in common) {
      newdata[[nm]] <- factor(as.character(newdata[[nm]]), levels = xlev[[nm]])
    }
  }
  
  # ---- warn about NA predictors (prediction will be NA for lm/gam)
  pred_vars <- intersect(vars, names(newdata))
  na_vars <- pred_vars[colSums(is.na(newdata[pred_vars])) > 0]
  if (length(na_vars) > 0) {
    warning("Selected row has NA in predictor(s): ", paste(na_vars, collapse = ", "),
            ". Prediction may be NA.")
  }
  newdata
}

###

###nd <- newdata_from_sale(
###  data = df,
###  model = mod_ln_tasp_back,
###  parcel_number = "16001234-5",   # whatever your parcel format is
###  parcel_col = "parcel_number"
###)

###pred <- predict(mod_ln_tasp_back, newdata = nd)
###pred

###nd <- newdata_from_sale(
###  data = df,
###  formula = formula_ln_tasp,
###  sale_id = 123456,
###  sale_col = "sale_id"
###)

# then:
###predict(lm(formula_ln_tasp, data = df), newdata = nd)

#

newdata_from_sale2 <- function(data,
                               model = NULL,
                               formula = NULL,
                               prep = NULL,
                               parcel_number = NULL,
                               sale_id = NULL,
                               parcel_col = "parcel_number",
                               sale_col   = "sale_id",
                               allow_multiple = FALSE,
                               pick = c("first", "last"),
                               extra_cols = NULL,
                               strict = TRUE) {
  
  pick <- match.arg(pick)
  
  if (is.null(model) && is.null(formula)) {
    stop("Provide either `model` or `formula`.")
  }
  
  if (!is.data.frame(data)) stop("`data` must be a data.frame/tibble.")
  
  # Optional preprocessing (feature engineering) for raw df
  if (!is.null(prep)) {
    if (!is.function(prep)) stop("`prep` must be a function(data) -> data.")
    data <- prep(data)
    if (!is.data.frame(data)) stop("`prep` must return a data.frame/tibble.")
  }
  
  # Choose lookup key
  has_parcel <- !is.null(parcel_number)
  has_sale   <- !is.null(sale_id)
  
  if (has_parcel == has_sale) {
    stop("Provide exactly one of `parcel_number` or `sale_id` (not both).")
  }
  
  key_col <- if (has_parcel) parcel_col else sale_col
  key_val <- if (has_parcel) parcel_number else sale_id
  
  if (!key_col %in% names(data)) stop("Key column '", key_col, "' not found in data.")
  
  idx <- which(data[[key_col]] == key_val)
  if (length(idx) == 0) stop("No rows found where ", key_col, " == ", key_val)
  
  if (!allow_multiple && length(idx) > 1) {
    idx <- if (pick == "first") idx[1] else idx[length(idx)]
  }
  
  # --- Determine needed predictor columns
  if (!is.null(model)) {
    # Use the model’s actual training model.frame to determine required columns
    mf_train <- model.frame(model)
    
    # Response column name is the first column in model.frame()
    resp_name <- names(mf_train)[1]
    
    needed <- setdiff(names(mf_train), resp_name)
    
    # Keep key and any extras too
    keep <- unique(c(key_col, extra_cols, needed))
    
    missing_cols <- setdiff(keep, names(data))
    if (length(missing_cols) > 0) {
      msg <- paste0(
        "The `data` you passed is missing predictor columns required by the model:\n  ",
        paste(missing_cols, collapse = ", "),
        "\n\nFix: pass the same engineered dataframe used to fit the model, or provide `prep=` that creates these columns."
      )
      if (strict) stop(msg) else warning(msg)
    }
    
    keep <- intersect(keep, names(data))
    newdata <- data[idx, keep, drop = FALSE]
    
    # Align factor levels to model (critical for predict)
    if (!is.null(model$xlevels)) {
      xlev <- model$xlevels
      common <- intersect(names(xlev), names(newdata))
      for (nm in common) {
        newdata[[nm]] <- factor(as.character(newdata[[nm]]), levels = xlev[[nm]])
      }
    }
    
  } else {
    # Formula path (your existing behavior)
    trm  <- stats::terms(formula, data = data)
    vars <- all.vars(stats::delete.response(trm))
    
    keep <- unique(c(key_col, extra_cols, vars))
    missing_cols <- setdiff(keep, names(data))
    if (length(missing_cols) > 0) {
      msg <- paste0("These variables are required but missing from `data`: ",
                    paste(missing_cols, collapse = ", "))
      if (strict) stop(msg) else warning(msg)
    }
    
    keep <- intersect(keep, names(data))
    newdata <- data[idx, keep, drop = FALSE]
  }
  
  # Warn about NAs in predictors
  pred_cols <- setdiff(names(newdata), c(key_col, extra_cols))
  na_cols <- pred_cols[colSums(is.na(newdata[pred_cols])) > 0]
  if (length(na_cols) > 0) {
    warning("Selected row has NA in predictor(s): ", paste(na_cols, collapse = ", "),
            ". Prediction may be NA.")
  }
  
  newdata
}

###nd <- newdata_from_sale2(df_engineered, model = mod_ln_tasp_back, parcel_number = "123-45-6789")
###predict(mod_ln_tasp_back, newdata = nd)

#

gam_coef_table <- function(model, caption = NULL, conf.level = 0.95, digits = 4,
                           format = c("latex", "html", "simple"), include_smooth = TRUE) {
  format <- match.arg(format)
  sm <- summary(model)
  
  # --- parametric terms (CI available) ---
  ptab <- as.data.frame(sm$p.table, check.names = FALSE)
  ptab$term <- rownames(ptab)
  rownames(ptab) <- NULL
  
  # robust to t vs z
  stat_col <- if ("t value" %in% names(ptab)) "t value" else if ("z value" %in% names(ptab)) "z value" else NA
  pval_col <- if ("Pr(>|t|)" %in% names(ptab)) "Pr(>|t|)" else if ("Pr(>|z|)" %in% names(ptab)) "Pr(>|z|)" else NA
  if (is.na(stat_col) || is.na(pval_col)) stop("Could not find statistic / p-value columns in p.table.")
  
  param <- ptab |>
    dplyr::transmute(
      type = "parametric",      term,      estimate  = `Estimate`,      std_error = `Std. Error`,
      statistic = .data[[stat_col]],      p_value   = .data[[pval_col]]
    )
  
  # add parametric CI (mgcv limitation: parametric only)
  ci <- suppressMessages(tryCatch(confint(model, level = conf.level), error = function(e) NULL))
  if (!is.null(ci)) {
    ci <- as.data.frame(ci, check.names = FALSE)
    ci$term <- rownames(ci)
    rownames(ci) <- NULL
    names(ci)[1:2] <- c("conf_low", "conf_high")
    param <- dplyr::left_join(param, ci, by = "term")
  } else {
    param$conf_low <- NA_real_
    param$conf_high <- NA_real_
  }
  
  # --- smooth terms (no simple CI; show edf/ref_df/stat/p) ---
  smooth <- NULL
  if (isTRUE(include_smooth) && !is.null(sm$s.table)) {
    stab <- as.data.frame(sm$s.table, check.names = FALSE)
    stab$term <- rownames(stab)
    rownames(stab) <- NULL
    
    stat_s <- if ("F" %in% names(stab)) "F" else if ("Chi.sq" %in% names(stab)) "Chi.sq" else NA
    if (is.na(stat_s)) stab$statistic <- NA_real_ else stab$statistic <- stab[[stat_s]]
    
    pcol_s <- if ("p-value" %in% names(stab)) "p-value" else if ("p.value" %in% names(stab)) "p.value" else NA
    if (is.na(pcol_s)) stab$p_value <- NA_real_ else stab$p_value <- stab[[pcol_s]]
    
    refdf_col <- if ("Ref.df" %in% names(stab)) "Ref.df" else NA
    if (is.na(refdf_col)) stab$ref_df <- NA_real_ else stab$ref_df <- stab[[refdf_col]]
    
    smooth <- stab |>
      dplyr::transmute(
        type = "smooth",        term,        estimate = NA_real_,        std_error = NA_real_,
        conf_low = NA_real_,        conf_high = NA_real_,        statistic = statistic,
        p_value = p_value,        edf = if ("edf" %in% names(stab)) .data[["edf"]] else NA_real_,
        ref_df = ref_df
      )
  }
  
  # combine
  out <- if (!is.null(smooth)) {
    dplyr::bind_rows(param |> dplyr::mutate(edf = NA_real_, ref_df = NA_real_), smooth)
  } else {param |> dplyr::mutate(edf = NA_real_, ref_df = NA_real_)}
  
  out <- out |> dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, digits)))
  
  pretty_names <- c(
    type      = "Type",    term = "Term",  estimate = "Estimate",  std_error = "Std. Error",
    statistic = "t value", p_value = "p value",  conf_low  = "CI low",  conf_high = "CI high",
    edf       = "edf",  ref_df = "ref df"
  )
  
  kb <- knitr::kable(
    out,  format = format,  caption = caption,
    col.names = unname(pretty_names[names(out)]),  escape = TRUE
  )
  list(data = out, kable = kb)
}

#

backtransform_log_preds <- function(model, pred_ln,
                                    method = c("median", "mean_lognormal", "mean_smear"),
                                    transform = c("log", "log1p")) {
  method <- match.arg(method)
  transform <- match.arg(transform)
  
  inv <- if (transform == "log") exp else expm1
  
  if (method == "median") {return(inv(pred_ln))}
  
  if (method == "mean_lognormal") {
    sigma <- if (!is.null(summary(model)$sigma)) summary(model)$sigma else sqrt(summary(model)$scale)
    return(inv(pred_ln + 0.5 * sigma^2))
  }
  
  if (method == "mean_smear") {
    smear <- mean(exp(residuals(model)), na.rm = TRUE)
    # for log1p this is an approximation; still often useful in practice
    return(inv(pred_ln) * smear)
  }
}

###mu <- predict(mod_tasp, newdata = ndt)  # ln scale
###pred_median <- backtransform_log_preds(mod_tasp, mu, "median")
###pred_mean   <- backtransform_log_preds(mod_tasp, mu, "mean_smear")

#

plot_avg_beta_lollipop_7models <- function(
    mod1, mod2, mod3, mod4, mod5, mod6, mod7,
    model_names = c("M1", "M2", "M3", "M4", "M5", "M6", "M7"),
    include_intercept = FALSE, top_n = NULL,
    title = "Average standardized beta across 7 models",
    x_lab = "Average standardized beta", y_lab = NULL
) {
  mods <- list(mod1, mod2, mod3, mod4, mod5, mod6, mod7)
  
  if (length(model_names) != 7) {stop("model_names must have length 7.")}
  if (!all(vapply(mods, inherits, logical(1), what = "lm"))) {
    stop("All model inputs must be lm objects.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {stop("Package 'dplyr' is required.")}
  if (!requireNamespace("ggplot2", quietly = TRUE)) {stop("Package 'ggplot2' is required.")}
  
  extract_model_betas <- function(model, model_name, include_intercept = FALSE) {
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
    coef_df$model <- model_name
    
    if (!include_intercept) {coef_df <- coef_df[coef_df$term != "(Intercept)", , drop = FALSE]}
    coef_df <- coef_df[!is.na(coef_df$beta), , drop = FALSE]
    coef_df
  }
  
  all_betas <- Map(
    function(m, nm) extract_model_betas(m, nm, include_intercept = include_intercept),
    mods, model_names
  ) %>% dplyr::bind_rows()
  
  summary_df <- all_betas %>% dplyr::group_by(term) %>%
    dplyr::summarise(
      n_models = dplyr::n(), models = paste(model, collapse = ", "),
      avg_beta = mean(beta, na.rm = TRUE), min_beta = min(beta, na.rm = TRUE),
      max_beta = max(beta, na.rm = TRUE), .groups = "drop"
    )
  
  if (!is.null(top_n)) {
    if (!is.numeric(top_n) || length(top_n) != 1 || is.na(top_n) || top_n < 1) {
      stop("`top_n` must be NULL or a single positive number.")
    }
    
    top_pos <- summary_df %>% dplyr::arrange(dplyr::desc(avg_beta), term) %>%
      dplyr::slice_head(n = top_n)
    
    top_neg <- summary_df %>% dplyr::arrange(avg_beta, term) %>% dplyr::slice_head(n = top_n)
    
    summary_df <- dplyr::bind_rows(top_pos, top_neg) %>%
      dplyr::distinct(term, .keep_all = TRUE)
  }
  
  summary_df <- summary_df %>% dplyr::arrange(avg_beta) %>%
    dplyr::mutate(term = factor(term, levels = term))
  
  max_abs_beta <- max(abs(summary_df$avg_beta), na.rm = TRUE)
  if (!is.finite(max_abs_beta) || max_abs_beta == 0) max_abs_beta <- 1
  
  p <- ggplot2::ggplot(summary_df, ggplot2::aes(x = avg_beta, y = term, color = avg_beta)) +
    ggplot2::geom_segment(
      ggplot2::aes(x = 0, xend = avg_beta, y = term, yend = term), linewidth = 0.9
    ) + geom_point(size = 3) + geom_vline(xintercept = 0, linetype = "dashed") +
    ggplot2::scale_color_gradient2(
      low = "red", mid = "grey70", high = "green", midpoint = 0,
      limits = c(-max_abs_beta, max_abs_beta), name = "Avg beta"
    ) + labs(title = title, x = x_lab, y = y_lab) + theme_minimal(base_size = 12)
  
  list(model_level_data = all_betas, summary_data = summary_df, plot = p)
}

#



#

summarize_extreme_betas_kable <- function(
    models, model_names = NULL, top_n = 3, bottom_n = 3, include_intercept = FALSE,
    include_ecf = TRUE, digits = 3,
    caption = "Top and bottom standardized beta predictors across models", wrap_models_at = 18
) {
  if (!is.list(models) || length(models) == 0) {
    stop("`models` must be a non-empty list of lm objects.")
  }
  
  if (!all(vapply(models, inherits, logical(1), what = "lm"))) {
    stop("All elements of `models` must be lm objects.")
  }
  
  if (is.null(model_names)) {model_names <- paste0("M", seq_along(models))}
  
  if (length(model_names) != length(models)) {
    stop("`model_names` must have the same length as `models`.")
  }
  
  if (!is.numeric(top_n) || length(top_n) != 1 || is.na(top_n) || top_n < 1) {
    stop("`top_n` must be a single positive number.")
  }
  
  if (!is.numeric(bottom_n) || length(bottom_n) != 1 || is.na(bottom_n) || bottom_n < 1) {
    stop("`bottom_n` must be a single positive number.")
  }
  
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required.")
  if (!requireNamespace("knitr", quietly = TRUE)) stop("Package 'knitr' is required.")
  if (!requireNamespace("kableExtra", quietly = TRUE)) stop("Package 'kableExtra' is required.")
  
  is_ecf_term <- function(x) {grepl("^ecf", trimws(tolower(as.character(x))))}
  
  extract_model_metrics <- function(model, model_name, include_intercept = FALSE, include_ecf = TRUE) {
    sm <- summary(model)
    coef_mat <- sm$coefficients
    
    out <- data.frame(
      term = rownames(coef_mat), b = coef_mat[, "Estimate"],
      std_error = coef_mat[, "Std. Error"], t = coef_mat[, "t value"],
      sig = coef_mat[, "Pr(>|t|)"], stringsAsFactors = FALSE
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
    
    if (!include_ecf) {out <- out[!is_ecf_term(out$term), , drop = FALSE]}
    
    out <- out[!is.na(out$beta), , drop = FALSE]
    out
  }
  
  all_extremes <- Map(
    function(m, nm) {
      df_i <- extract_model_metrics(
        model = m,
        model_name = nm,
        include_intercept = include_intercept,
        include_ecf = include_ecf
      )
      
      if (nrow(df_i) == 0) return(df_i)
      
      top_part <- df_i %>%
        dplyr::arrange(dplyr::desc(beta), term) %>% dplyr::slice_head(n = top_n)
      
      bottom_part <- df_i %>% dplyr::arrange(beta, term) %>% dplyr::slice_head(n = bottom_n)
      
      dplyr::bind_rows(top_part, bottom_part) %>%
        dplyr::distinct(term, model, .keep_all = TRUE)
    },
    models, model_names
  ) %>% dplyr::bind_rows()
  
  if (nrow(all_extremes) == 0) {
    stop("No predictors remained after filtering and beta extraction.")
  }
  
  parse_model_tokens <- function(x) {
    if (is.na(x) || !nzchar(x)) return(character(0))
    toks <- unlist(strsplit(as.character(x), ","))
    toks <- trimws(toks)
    toks[nzchar(toks)]
  }
  
  summary_df <- all_extremes %>% dplyr::group_by(term) %>%
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
    ) %>% dplyr::arrange(dplyr::desc(avg_beta), term)
  
  wrap_text <- function(x, width = 18) {
    vapply(
      x,
      function(s) paste(strwrap(as.character(s), width = width), collapse = "\n"),
      character(1)
    )
  }
  
  display_df <- summary_df %>%
    dplyr::mutate(
      term = gsub("_", "_ ", term, fixed = TRUE), models = wrap_text(models, width = wrap_models_at)
    )
  
  kb <- knitr::kable(
    display_df, format = "latex", booktabs = TRUE, linesep = "", digits = digits,
    escape = TRUE, align = c("l", "c", "l", rep("r", 6), "r", "r"),
    caption = caption,
    col.names = c(
      "Predictor", "n",
      "Models",
      "Avg b", "Avg SE", "Avg beta", "Avg t", "Avg sig",
      "Min beta", "Max beta"
    )
  ) %>%
    kableExtra::kable_styling(
      latex_options = c("HOLD_position", "striped", "scale_down"),
      font_size = 9, full_width = FALSE
    ) %>% kableExtra::column_spec(1, width = "1.6in") %>%
    kableExtra::column_spec(2, width = "0.45in") %>%
    kableExtra::column_spec(3, width = "1.3in") %>%
    kableExtra::column_spec(4:ncol(display_df), width = "0.7in")
  
  list(extreme_rows = all_extremes, summary_data = summary_df, kable = kb)
}

#

plot_avg_beta_lollipop <- function(
    models, model_names = NULL, include_intercept = FALSE, include_ecf = TRUE, top_n = NULL,
    title = "Average standardized beta across models",
    x_lab = "Average standardized beta", y_lab = NULL
) {
  if (!is.list(models) || length(models) == 0) {
    stop("`models` must be a non-empty list of lm objects.")
  }
  
  if (!all(vapply(models, inherits, logical(1), what = "lm"))) {
    stop("All elements of `models` must be lm objects.")
  }
  
  if (is.null(model_names)) {model_names <- paste0("M", seq_along(models))}
  
  if (length(model_names) != length(models)) {
    stop("`model_names` must have the same length as `models`.")
  }
  
  if (!requireNamespace("dplyr", quietly = TRUE)) {stop("Package 'dplyr' is required.")}
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required.")
  }
  
  is_ecf_term <- function(x) {grepl("^ecf", trimws(tolower(as.character(x))))}
  
  extract_model_betas <- function(model, model_name, include_intercept = FALSE,
                                  include_ecf = TRUE) {
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
    coef_df$model <- model_name
    
    if (!include_intercept) {
      coef_df <- coef_df[coef_df$term != "(Intercept)", , drop = FALSE]
    }
    
    if (!include_ecf) {
      coef_df <- coef_df[!is_ecf_term(coef_df$term), , drop = FALSE]
    }
    
    coef_df <- coef_df[!is.na(coef_df$beta), , drop = FALSE]
    coef_df
  }
  
  all_betas <- Map(
    function(m, nm) {
      extract_model_betas(
        model = m,
        model_name = nm,
        include_intercept = include_intercept,
        include_ecf = include_ecf
      )
    },
    models, model_names
  ) %>%
    dplyr::bind_rows()
  
  if (nrow(all_betas) == 0) {
    stop("No predictors remained after filtering and beta extraction.")
  }
  
  summary_df <- all_betas %>%
    dplyr::group_by(term) %>%
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
    
    top_pos <- summary_df %>% arrange(dplyr::desc(avg_beta), term) %>% slice_head(n = top_n)
    top_neg <- summary_df %>% arrange(avg_beta, term) %>% slice_head(n = top_n)
    summary_df <- dplyr::bind_rows(top_pos, top_neg) %>%
      distinct(term, .keep_all = TRUE)
  }
  
  summary_df <- summary_df %>%
    dplyr::arrange(avg_beta) %>% dplyr::mutate(term = factor(term, levels = term))
  
  max_abs_beta <- max(abs(summary_df$avg_beta), na.rm = TRUE)
  if (!is.finite(max_abs_beta) || max_abs_beta == 0) max_abs_beta <- 1
  
  p <- ggplot(summary_df, aes(x = avg_beta, y = term, color = avg_beta)) +
    geom_segment(
      ggplot2::aes(x = 0, xend = avg_beta, y = term, yend = term), linewidth = 0.9
    ) + geom_point(size = 3) + geom_vline(xintercept = 0, linetype = "dashed") +
    scale_color_gradient2(
      low = "red", mid = "grey70", high = "green", midpoint = 0,
      limits = c(-max_abs_beta, max_abs_beta), name = "Avg beta"
    ) + labs(title = title, x = x_lab, y = y_lab) + theme_minimal(base_size = 12)
  
  list(model_level_data = all_betas, summary_data = summary_df, plot = p)
}

#

summarize_extreme_betas_kable <- function(
    models,
    model_names = NULL,
    top_n = 3,
    bottom_n = 3,
    include_intercept = FALSE,
    include_ecf = TRUE,
    digits = 3,
    caption = "Top and bottom standardized beta predictors across models",
    wrap_models_at = 18
) {
  if (!is.list(models) || length(models) == 0) {
    stop("`models` must be a non-empty list of lm objects.")
  }
  
  if (!all(vapply(models, inherits, logical(1), what = "lm"))) {
    stop("All elements of `models` must be lm objects.")
  }
  
  if (is.null(model_names)) {
    model_names <- paste0("M", seq_along(models))
  }
  
  if (length(model_names) != length(models)) {
    stop("`model_names` must have the same length as `models`.")
  }
  
  if (!is.numeric(top_n) || length(top_n) != 1 || is.na(top_n) || top_n < 1) {
    stop("`top_n` must be a single positive number.")
  }
  
  if (!is.numeric(bottom_n) || length(bottom_n) != 1 || is.na(bottom_n) || bottom_n < 1) {
    stop("`bottom_n` must be a single positive number.")
  }
  
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required.")
  if (!requireNamespace("knitr", quietly = TRUE)) stop("Package 'knitr' is required.")
  if (!requireNamespace("kableExtra", quietly = TRUE)) stop("Package 'kableExtra' is required.")
  
  is_ecf_term <- function(x) {
    grepl("^ecf", trimws(tolower(as.character(x))))
  }
  
  latex_escape <- function(x) {
    x <- as.character(x)
    x <- gsub("\\\\", "\\\\textbackslash{}", x)
    x <- gsub("([#$%&_{}])", "\\\\\\1", x)
    x <- gsub("~", "\\\\textasciitilde{}", x)
    x <- gsub("\\^", "\\\\textasciicircum{}", x)
    x
  }
  
  wrap_text <- function(x, width = 18) {
    vapply(
      x,
      function(s) paste(strwrap(as.character(s), width = width), collapse = "\n"),
      character(1)
    )
  }
  
  extract_model_metrics <- function(model, model_name, include_intercept = FALSE, include_ecf = TRUE) {
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
    
    if (!include_ecf) {
      out <- out[!is_ecf_term(out$term), , drop = FALSE]
    }
    
    out <- out[!is.na(out$beta), , drop = FALSE]
    out
  }
  
  all_extremes <- Map(
    function(m, nm) {
      df_i <- extract_model_metrics(
        model = m,
        model_name = nm,
        include_intercept = include_intercept,
        include_ecf = include_ecf
      )
      
      if (nrow(df_i) == 0) return(df_i)
      
      top_part <- df_i %>%
        dplyr::arrange(dplyr::desc(beta), term) %>%
        dplyr::slice_head(n = top_n)
      
      bottom_part <- df_i %>%
        dplyr::arrange(beta, term) %>%
        dplyr::slice_head(n = bottom_n)
      
      dplyr::bind_rows(top_part, bottom_part) %>%
        dplyr::distinct(term, model, .keep_all = TRUE)
    },
    models, model_names
  ) %>%
    dplyr::bind_rows()
  
  if (nrow(all_extremes) == 0) {
    stop("No predictors remained after filtering and beta extraction.")
  }
  
  summary_df <- all_extremes %>%
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
  
  beta_to_color <- function(x, max_abs_beta) {
    if (is.na(x) || !is.finite(x)) return(NA_character_)
    scaled <- x / max_abs_beta
    scaled <- max(min(scaled, 1), -1)
    
    if (scaled < 0) {
      t <- scaled + 1
      ramp <- grDevices::colorRamp(c("red", "grey70"))(t)
    } else {
      t <- scaled
      ramp <- grDevices::colorRamp(c("grey70", "green"))(t)
    }
    
    grDevices::rgb(ramp[1], ramp[2], ramp[3], maxColorValue = 255)
  }
  
  max_abs_beta <- max(abs(summary_df$avg_beta), na.rm = TRUE)
  if (!is.finite(max_abs_beta) || max_abs_beta == 0) {
    max_abs_beta <- 1
  }
  
  display_df <- summary_df %>%
    dplyr::mutate(
      term = latex_escape(term),
      models = latex_escape(wrap_text(models, width = wrap_models_at))
    )
  
  display_df$n_models <- as.character(summary_df$n_models)
  display_df$avg_b <- formatC(summary_df$avg_b, digits = digits, format = "f")
  display_df$avg_se <- formatC(summary_df$avg_se, digits = digits, format = "f")
  display_df$avg_t <- formatC(summary_df$avg_t, digits = digits, format = "f")
  display_df$avg_sig <- formatC(summary_df$avg_sig, digits = digits, format = "f")
  display_df$min_beta <- formatC(summary_df$min_beta, digits = digits, format = "f")
  display_df$max_beta <- formatC(summary_df$max_beta, digits = digits, format = "f")
  
  avg_beta_colors <- vapply(
    summary_df$avg_beta,
    beta_to_color,
    character(1),
    max_abs_beta = max_abs_beta
  )
  
  avg_beta_text <- formatC(summary_df$avg_beta, digits = digits, format = "f")
  display_df$avg_beta <- mapply(
    function(txt, col) {
      kableExtra::cell_spec(
        txt,
        format = "latex",
        background = col,
        color = "black"
      )
    },
    avg_beta_text,
    avg_beta_colors,
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )
  
  kb <- knitr::kable(
    display_df,
    format = "latex",
    booktabs = TRUE,
    linesep = "",
    escape = FALSE,
    align = c("l", "c", "l", rep("r", 6), "r", "r"),
    caption = latex_escape(caption),
    col.names = c(
      "Predictor", "n",
      "Models",
      "Avg b", "Avg SE", "Avg beta", "Avg t", "Avg sig",
      "Min beta", "Max beta"
    )
  ) %>%
    kableExtra::kable_styling(
      latex_options = c("hold_position", "striped", "scale_down"),
      font_size = 9,
      full_width = FALSE
    ) %>%
    kableExtra::column_spec(1, width = "1.6in") %>%
    kableExtra::column_spec(2, width = "0.45in") %>%
    kableExtra::column_spec(3, width = "1.3in") %>%
    kableExtra::column_spec(4:ncol(display_df), width = "0.7in")
  
  list(
    extreme_rows = all_extremes,
    summary_data = summary_df,
    kable = kb
  )
}

#
plot_avg_beta_lollipop_all <- function(
    models,    model_names = NULL,    include_intercept = FALSE,    include_ecf = TRUE,
    top_n = NULL,    title = "Average standardized beta across models",
    x_lab = "Average standardized beta",    y_lab = NULL
) {
  if (!is.list(models) || length(models) == 0) {
    stop("`models` must be a non-empty list of lm objects.")
  }
  
  if (!all(vapply(models, inherits, logical(1), what = "lm"))) {
    stop("All elements of `models` must be lm objects.")
  }
  
  if (is.null(model_names)) {model_names <- paste0("M", seq_along(models))}
  if (length(model_names) != length(models)) {
    stop("`model_names` must have the same length as `models`.")
  }
  
  if (!requireNamespace("dplyr", quietly = TRUE)) {stop("Package 'dplyr' is required.")}
  if (!requireNamespace("ggplot2", quietly = TRUE)) {stop("Package 'ggplot2' is required.")}
  
  is_ecf_term <- function(x) {
    grepl("^ecf", trimws(tolower(as.character(x))))
  }
  
  extract_model_betas <- function(model, model_name, include_intercept = FALSE,
                                  include_ecf = TRUE) {
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
    coef_df$model <- model_name
    
    if (!include_intercept) {coef_df <- coef_df[coef_df$term != "(Intercept)", , drop = FALSE]}
    if (!include_ecf) {
      coef_df <- coef_df[!is_ecf_term(coef_df$term), , drop = FALSE]
    }
    
    coef_df <- coef_df[!is.na(coef_df$beta), , drop = FALSE]
    coef_df
  }
  
  all_betas <- Map(
    function(m, nm) {
      extract_model_betas(
        model = m, model_name = nm, include_intercept = include_intercept,
        include_ecf = include_ecf
      )
    }, models, model_names
  ) %>% dplyr::bind_rows()
  
  if (nrow(all_betas) == 0) {stop("No predictors remained after filtering and beta extraction.")}
  
  summary_df <- all_betas %>% dplyr::group_by(term) %>%
    dplyr::summarise(
      n_models = dplyr::n(), models = paste(model, collapse = ", "),
      avg_beta = mean(beta, na.rm = TRUE), min_beta = min(beta, na.rm = TRUE),
      max_beta = max(beta, na.rm = TRUE), avg_b = mean(b, na.rm = TRUE),
      avg_se = mean(std_error, na.rm = TRUE), avg_t = mean(t, na.rm = TRUE),
      avg_sig = mean(sig, na.rm = TRUE), .groups = "drop"
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
  if (!is.finite(max_abs_beta) || max_abs_beta == 0) {max_abs_beta <- 1}
  
  p <- ggplot2::ggplot(
    summary_df, ggplot2::aes(x = avg_beta, y = term, color = avg_beta)
  ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = 0, xend = avg_beta, y = term, yend = term), linewidth = 0.9
    ) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
    ggplot2::scale_color_gradient2(
      low = "red", mid = "grey70", high = "green", midpoint = 0,
      limits = c(-max_abs_beta, max_abs_beta), name = "Avg beta"
    ) + ggplot2::labs(title = title,      x = x_lab,      y = y_lab) +
    ggplot2::theme_minimal(base_size = 12)
  
  list(model_level_data = all_betas,  summary_data = summary_df,  plot = p)
}
#

summarize_avg_betas_kable_all <- function(
    models, model_names = NULL, include_intercept = FALSE, include_ecf = TRUE,
    top_n = NULL, digits = 3,
    caption = "Average standardized beta predictors across models", wrap_models_at = 18
) {
  if (!is.list(models) || length(models) == 0) {
    stop("`models` must be a non-empty list of lm objects.")
  }
  
  if (!all(vapply(models, inherits, logical(1), what = "lm"))) {
    stop("All elements of `models` must be lm objects.")
  }
  
  if (is.null(model_names)) {model_names <- paste0("M", seq_along(models))}
  if (length(model_names) != length(models)) {
    stop("`model_names` must have the same length as `models`.")
  }
  
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required.")
  if (!requireNamespace("knitr", quietly = TRUE)) stop("Package 'knitr' is required.")
  if (!requireNamespace("kableExtra", quietly = TRUE)) stop("Package 'kableExtra' is required.")
  
  is_ecf_term <- function(x) {grepl("^ecf", trimws(tolower(as.character(x))))}
  
  latex_escape <- function(x) {
    x <- as.character(x)
    x <- gsub("\\\\", "\\\\textbackslash{}", x)
    x <- gsub("([#$%&_{}])", "\\\\\\1", x)
    x <- gsub("~", "\\\\textasciitilde{}", x)
    x <- gsub("\\^", "\\\\textasciicircum{}", x)
    x
  }
  
  wrap_text <- function(x, width = 18) {
    vapply(
      x,
      function(s) paste(strwrap(as.character(s), width = width), collapse = "\n"),
      character(1)
    )
  }
  
  extract_model_metrics <- function(model, model_name, include_intercept = FALSE, include_ecf = TRUE) {
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
    
    if (!include_intercept) {out <- out[out$term != "(Intercept)", , drop = FALSE]}
    if (!include_ecf) {out <- out[!is_ecf_term(out$term), , drop = FALSE]}
    
    out <- out[!is.na(out$beta), , drop = FALSE]
    out
  }
  
  all_metrics <- Map(
    function(m, nm) {
      extract_model_metrics(
        model = m, model_name = nm, include_intercept = include_intercept,
        include_ecf = include_ecf
      )
    },  models, model_names
  ) %>%  dplyr::bind_rows()
  
  if (nrow(all_metrics) == 0) {
    stop("No predictors remained after filtering and beta extraction.")
  }
  
  summary_df <- all_metrics %>%
    dplyr::group_by(term) %>%
    dplyr::summarise(
      n_models = dplyr::n(), models = paste(model, collapse = ", "),
      avg_b = mean(b, na.rm = TRUE), avg_se = mean(std_error, na.rm = TRUE),
      avg_beta = mean(beta, na.rm = TRUE), avg_t = mean(t, na.rm = TRUE),
      avg_sig = mean(sig, na.rm = TRUE), min_beta = min(beta, na.rm = TRUE),
      max_beta = max(beta, na.rm = TRUE), .groups = "drop"
    )
  
  if (!is.null(top_n)) {
    if (!is.numeric(top_n) || length(top_n) != 1 || is.na(top_n) || top_n < 1) {
      stop("`top_n` must be NULL or a single positive number.")
    }
    
    top_pos <- summary_df %>% arrange(dplyr::desc(avg_beta), term) %>% slice_head(n = top_n)
    top_neg <- summary_df %>% dplyr::arrange(avg_beta, term) %>% dplyr::slice_head(n = top_n)
    summary_df <- dplyr::bind_rows(top_pos, top_neg) %>%
      dplyr::distinct(term, .keep_all = TRUE)
  }
  
  summary_df <- summary_df %>% dplyr::arrange(dplyr::desc(avg_beta), term)
  
  beta_to_color <- function(x, max_abs_beta) {
    if (is.na(x) || !is.finite(x)) return(NA_character_)
    scaled <- x / max_abs_beta
    scaled <- max(min(scaled, 1), -1)
    
    if (scaled < 0) {
      t <- scaled + 1
      ramp <- grDevices::colorRamp(c("red", "grey70"))(t)
    } else {
      t <- scaled
      ramp <- grDevices::colorRamp(c("grey70", "green"))(t)
    }
    
    grDevices::rgb(ramp[1], ramp[2], ramp[3], maxColorValue = 255)
  }
  
  max_abs_beta <- max(abs(summary_df$avg_beta), na.rm = TRUE)
  if (!is.finite(max_abs_beta) || max_abs_beta == 0) {
    max_abs_beta <- 1
  }
  
  display_df <- summary_df %>%
    dplyr::mutate(
      term = latex_escape(term), models = latex_escape(wrap_text(models, width = wrap_models_at))
    )
  
  display_df$n_models <- as.character(summary_df$n_models)
  display_df$avg_b <- formatC(summary_df$avg_b, digits = digits, format = "f")
  display_df$avg_se <- formatC(summary_df$avg_se, digits = digits, format = "f")
  display_df$avg_t <- formatC(summary_df$avg_t, digits = digits, format = "f")
  display_df$avg_sig <- formatC(summary_df$avg_sig, digits = digits, format = "f")
  display_df$min_beta <- formatC(summary_df$min_beta, digits = digits, format = "f")
  display_df$max_beta <- formatC(summary_df$max_beta, digits = digits, format = "f")
  
  avg_beta_colors <- vapply(
    summary_df$avg_beta,
    beta_to_color,
    character(1),
    max_abs_beta = max_abs_beta
  )
  
  avg_beta_text <- formatC(summary_df$avg_beta, digits = digits, format = "f")
  display_df$avg_beta <- mapply(
    function(txt, col) {
      kableExtra::cell_spec(
        txt,
        format = "latex",
        background = col,
        color = "black"
      )
    },
    avg_beta_text,
    avg_beta_colors,
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )
  
  kb <- knitr::kable(
    display_df, format = "latex", booktabs = TRUE, linesep = "", escape = FALSE,
    align = c("l", "c", "l", rep("r", 6), "r", "r"),
    caption = latex_escape(caption),
    col.names = c(
      "Predictor", "n",
      "Models",
      "Avg b", "Avg SE", "Avg beta", "Avg t", "Avg sig",
      "Min beta", "Max beta"
    )
  ) %>%
    kableExtra::kable_styling(
      latex_options = c("hold_position", "striped", "scale_down"),
      font_size = 9, full_width = FALSE
    ) %>%  kableExtra::column_spec(1, width = "1.6in") %>%
    kableExtra::column_spec(2, width = "0.45in") %>%
    kableExtra::column_spec(3, width = "1.3in") %>%
    kableExtra::column_spec(4:ncol(display_df), width = "0.7in")
  
  list(model_level_data = all_metrics, summary_data = summary_df, kable = kb)
}

#

plot_avg_beta_lollipop_dfs <- function(
    df, term_col = "term", beta_col = "avg_beta", top_n = NULL,
    title = "Average standardized beta across models",
    x_lab = "Average standardized beta", y_lab = NULL
) {
  if (!is.data.frame(df)) {stop("`df` must be a data.frame.")}
  needed <- c(term_col, beta_col)
  missing_needed <- setdiff(needed, names(df))
  if (length(missing_needed) > 0) {
    stop("Missing required columns: ", paste(missing_needed, collapse = ", "))
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {stop("Package 'dplyr' is required.")}
  if (!requireNamespace("ggplot2", quietly = TRUE)) {stop("Package 'ggplot2' is required.")}
  
  plot_df <- df %>%
    dplyr::transmute(
      term = as.character(.data[[term_col]]),
      avg_beta = as.numeric(.data[[beta_col]])
    ) %>% dplyr::filter(!is.na(avg_beta), !is.na(term), nzchar(term))
  
  if (nrow(plot_df) == 0) {stop("No non-missing rows available to plot.")}
  if (!is.null(top_n)) {
    if (!is.numeric(top_n) || length(top_n) != 1 || is.na(top_n) || top_n < 1) {
      stop("`top_n` must be NULL or a single positive number.")
    }
    
    top_pos <- plot_df %>% dplyr::arrange(dplyr::desc(avg_beta), term) %>%
      dplyr::slice_head(n = top_n)
    
    top_neg <- plot_df %>% dplyr::arrange(avg_beta, term) %>% dplyr::slice_head(n = top_n)
    plot_df <- dplyr::bind_rows(top_pos, top_neg) %>%
      dplyr::distinct(term, .keep_all = TRUE)
  }
  
  plot_df <- plot_df %>%
    dplyr::arrange(avg_beta) %>%
    dplyr::mutate(term = factor(term, levels = term))
  
  max_abs_beta <- max(abs(plot_df$avg_beta), na.rm = TRUE)
  if (!is.finite(max_abs_beta) || max_abs_beta == 0) {
    max_abs_beta <- 1
  }
  
  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = avg_beta, y = term, color = avg_beta)) +
    ggplot2::geom_segment(
      ggplot2::aes(x = 0, xend = avg_beta, y = term, yend = term), linewidth = 0.9
    ) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
    ggplot2::scale_color_gradient2(
      low = "red", mid = "grey70", high = "green", midpoint = 0,
      limits = c(-max_abs_beta, max_abs_beta), name = "Avg beta"
    ) +
    ggplot2::labs(title = title, x = x_lab, y = y_lab) + ggplot2::theme_minimal(base_size = 12)
  
  list(data = plot_df, plot = p)
}

#

plot_beta_compare_lollipops <- function(
    model, df, df_term_col = "term", df_beta_col = "avg_beta", include_intercept = FALSE,
    include_ecf = TRUE, top_n = NULL, model_label = "R", df_label = "SPSS",
    title = "Standardized beta comparison", x_lab = "Standardized beta",
    y_lab = NULL, y_offset = 0.14
) {
  if (!inherits(model, "lm")) {stop("`model` must be an lm object.")}
  if (!is.data.frame(df)) {stop("`df` must be a data.frame.")}
  
  needed <- c(df_term_col, df_beta_col)
  missing_needed <- setdiff(needed, names(df))
  if (length(missing_needed) > 0) {
    stop("Missing required columns in `df`: ", paste(missing_needed, collapse = ", "))
  }
  
  if (!requireNamespace("dplyr", quietly = TRUE)) {stop("Package 'dplyr' is required.")}
  if (!requireNamespace("ggplot2", quietly = TRUE)) {stop("Package 'ggplot2' is required.")}
  
  is_ecf_term <- function(x) {grepl("^ecf", trimws(tolower(as.character(x))))}
  
  normalize_term <- function(x) {
    x <- trimws(tolower(as.character(x)))
    x[x %in% c("(constant)", "(intercept)", "constant", "intercept")] <- "(intercept)"
    x
  }
  
  # --- extract model betas ---
  sm <- summary(model)
  coef_mat <- sm$coefficients
  
  coef_df <- data.frame(
    term = rownames(coef_mat), b = coef_mat[, "Estimate"], std_error = coef_mat[, "Std. Error"],
    t = coef_mat[, "t value"], sig = coef_mat[, "Pr(>|t|)"], stringsAsFactors = FALSE
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
      
      if (!is.na(x_sd) && x_sd != 0 && !is.na(b_j)) {beta_vec[nm] <- b_j * x_sd / y_sd}
    }
  }
  coef_df$beta <- beta_vec[coef_df$term]
  
  if (!include_intercept) {coef_df <- coef_df[coef_df$term != "(Intercept)", , drop = FALSE]}
  if (!include_ecf) {coef_df <- coef_df[!is_ecf_term(coef_df$term), , drop = FALSE]}
  
  coef_df <- coef_df[!is.na(coef_df$beta), , drop = FALSE]
  
  model_df <- coef_df %>%
    dplyr::transmute(term_raw_model = term, term_key = normalize_term(term),
                     beta_model = as.numeric(beta))
  
  other_df <- df %>%
    dplyr::transmute(
      term_raw_df = as.character(.data[[df_term_col]]),
      term_key = normalize_term(.data[[df_term_col]]), beta_df = as.numeric(.data[[df_beta_col]])
    ) %>% dplyr::filter(!is.na(beta_df))
  
  if (!include_intercept) {other_df <- other_df %>% dplyr::filter(term_key != "(intercept)")}
  if (!include_ecf) {other_df <- other_df %>% dplyr::filter(!is_ecf_term(term_raw_df))}
  
  shared <- dplyr::inner_join(model_df, other_df, by = "term_key") %>%
    dplyr::mutate(term = dplyr::coalesce(term_raw_model, term_raw_df))
  
  if (nrow(shared) == 0) {stop("No shared predictors found between the model and dataframe.")}
  
  shared <- shared %>% dplyr::mutate(mean_beta = (beta_model + beta_df) / 2)
  
  if (!is.null(top_n)) {
    if (!is.numeric(top_n) || length(top_n) != 1 || is.na(top_n) || top_n < 1) {
      stop("`top_n` must be NULL or a single positive number.")
    }
    
    top_pos <- shared %>% dplyr::arrange(dplyr::desc(mean_beta), term) %>%
      dplyr::slice_head(n = top_n)
    
    top_neg <- shared %>% dplyr::arrange(mean_beta, term) %>% dplyr::slice_head(n = top_n)
    shared <- dplyr::bind_rows(top_pos, top_neg) %>% dplyr::distinct(term_key, .keep_all = TRUE)
  }
  
  shared <- shared %>% dplyr::arrange(mean_beta) %>%
    dplyr::mutate(term = factor(term, levels = term), y_base = seq_len(dplyr::n()),
                  y_model = y_base + y_offset, y_df = y_base - y_offset)
  
  max_abs_beta <- max(abs(c(shared$beta_model, shared$beta_df)), na.rm = TRUE)
  if (!is.finite(max_abs_beta) || max_abs_beta == 0) {max_abs_beta <- 1}
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = shared,
      ggplot2::aes(x = 0, xend = beta_model, y = y_model, yend = y_model, color = model_label),
      linewidth = 0.9
    ) +
    ggplot2::geom_point(
      data = shared, ggplot2::aes(x = beta_model, y = y_model, color = model_label), size = 3
    ) +
    ggplot2::geom_segment(
      data = shared, ggplot2::aes(x = 0, xend = beta_df, y = y_df, yend = y_df, color = df_label),
      linewidth = 0.9
    ) +
    ggplot2::geom_point(
      data = shared, ggplot2::aes(x = beta_df, y = y_df, color = df_label), size = 3
    ) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
    ggplot2::scale_y_continuous(breaks = shared$y_base, labels = as.character(shared$term)) +
    ggplot2::scale_color_manual(
      values = c("R" = "blue", "SPSS" = "red"), breaks = c("R", "SPSS"),
      labels = c("R", "SPSS"), name = NULL
    ) +
    ggplot2::coord_cartesian(xlim = c(-max_abs_beta * 1.1, max_abs_beta * 1.1)) +
    ggplot2::labs(title = title, x = x_lab, y = y_lab) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(hjust = 1))
  
  list(shared_data = shared, plot = p)
}

#

all_ratio_metrics <- function(df, estimate, saleprice,
                               adjust_to_spss = FALSE,
                               spss_ratio_scale = NULL) {
  estimate  <- rlang::enquo(estimate)
  saleprice <- rlang::enquo(saleprice)
  
  # If requested, rescale the estimate to match SPSS’s back-transform scaling.
  # Priority order:
  #   1) explicit spss_ratio_scale argument
  #   2) attr(df, "spss_ratio_scale")
  #   3) getOption("spss_ratio_scale")
  #   4) constant column df$spss_ratio_scale (first value)
  get_scale <- function(df, spss_ratio_scale) {
    if (!is.null(spss_ratio_scale)) {
      if (length(spss_ratio_scale) != 1 || !is.finite(spss_ratio_scale)) {
        stop("spss_ratio_scale must be a finite numeric scalar.", call. = FALSE)
      }
      return(as.numeric(spss_ratio_scale))
    }
    
    sc <- attr(df, "spss_ratio_scale", exact = TRUE)
    if (!is.null(sc) && is.finite(sc) && length(sc) == 1) return(as.numeric(sc))
    
    sc <- getOption("spss_ratio_scale", NULL)
    if (!is.null(sc) && is.finite(sc) && length(sc) == 1) return(as.numeric(sc))
    
    if ("spss_ratio_scale" %in% names(df)) {
      sc <- df[["spss_ratio_scale"]][1]
      if (!is.null(sc) && is.finite(sc) && length(sc) == 1) return(as.numeric(sc))
    }
    
    stop(
      "adjust_to_spss=TRUE but no scale factor found.\n",
      "Provide it via:\n",
      "  • spss_ratio_scale = <numeric scalar>\n",
      "  • attr(df, 'spss_ratio_scale') <- <numeric scalar>\n",
      "  • options(spss_ratio_scale = <numeric scalar>)\n",
      "  • or a constant df$spss_ratio_scale column\n",
      call. = FALSE
    )
  }
  
  scale_factor <- if (isTRUE(adjust_to_spss)) get_scale(df, spss_ratio_scale) else 1
  
  d <- df %>%
    dplyr::transmute(
      est = (!!estimate) * scale_factor,
      sp  = !!saleprice
    ) %>%
    dplyr::filter(is.finite(est), is.finite(sp), est > 0, sp > 0)
  
  if (nrow(d) == 0) {
    return(tibble::tibble(
      n = 0L, median_ratio = NA_real_, mean_ratio = NA_real_,
      wmean_ratio = NA_real_, PRD = NA_real_, COD = NA_real_, PRB = NA_real_,
      within_10_n = 0L, within_10_pct = NA_real_,
      within_20_n = 0L, within_20_pct = NA_real_,
      within_50_n = 0L, within_50_pct = NA_real_
    ))
  }
  
  ratio   <- d$est / d$sp
  med     <- stats::median(ratio, na.rm = TRUE)
  mean_r  <- mean(ratio, na.rm = TRUE)
  wmean_r <- sum(d$est, na.rm = TRUE) / sum(d$sp, na.rm = TRUE)
  
  COD <- (mean(abs(ratio - med), na.rm = TRUE) / med) * 100
  PRD <- mean_r / wmean_r
  
  # Within-band metrics (SPSS-style: within % of median)
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
  
  # PRB per SPSS definition
  y <- ((ratio - med) / med) * 100
  value_proxy <- 0.5 * d$sp + 0.5 * (d$est / med)
  ok <- is.finite(y) & is.finite(value_proxy) & value_proxy > 0
  
  prb_fit <- try(stats::lm(y[ok] ~ log2(value_proxy[ok])), silent = TRUE)
  PRB <- if (inherits(prb_fit, "try-error") || sum(ok) < 3) NA_real_ else unname(coef(prb_fit)[2])
  
  tibble::tibble(
    n = nrow(d),
    median_ratio = med,
    mean_ratio   = mean_r,
    wmean_ratio  = wmean_r,
    PRD = PRD,
    COD = COD / 100,   # decimal form to match your SPSS-style table (e.g., 0.3290)
    PRB = PRB / 100,   # decimal form likewise
    within_10_n   = w10_n,
    within_10_pct = w10_pct,
    within_20_n   = w20_n,
    within_20_pct = w20_pct,
    within_50_n   = w50_n,
    within_50_pct = w50_pct
  )
}

#

add_detroit_parcel_coords <- function(
    df, parcel_col, x_col = "x_coord", y_col = "y_coord", batch_size = 100, quiet = FALSE
) {
  
  stopifnot(is.data.frame(df))
  
  if (!parcel_col %in% names(df)) {
    stop(sprintf("Column '%s' not found in dataframe.", parcel_col))
  }
  
  service_url <- paste0(
    "https://services2.arcgis.com/qvkbeam7Wirps6zC/",
    "arcgis/rest/services/Parcels_Current/FeatureServer/0/query"
  )
  
  `%||%` <- function(a, b) {if (is.null(a) || length(a) == 0) b else a}
  
  out <- df
  out$.parcel_clean <- trimws(as.character(out[[parcel_col]]))
  
  out$.parcel_clean[
    out$.parcel_clean == ""
  ] <- NA_character_
  
  unique_parcels <- unique(out$.parcel_clean[!is.na(out$.parcel_clean)])
  
  if (length(unique_parcels) == 0) {
    out[[x_col]] <- NA_real_
    out[[y_col]] <- NA_real_
    
    return(out %>% dplyr::select(-.parcel_clean))
  }
  
  chunks <- split(unique_parcels, ceiling(seq_along(unique_parcels) / batch_size))
  
  if (!quiet) {
    message("Looking up ", length(unique_parcels), " unique parcels in ",
      length(chunks), " batch(es)..."
    )
  }
  
  fetch_chunk <- function(parcel_vec) {
    
    vals <- paste0("'", gsub("'", "''", parcel_vec, fixed = TRUE), "'", collapse = ",")
    where_clause <- sprintf("parcel_number IN (%s)", vals)
    
    resp <- httr::POST(
      url = service_url,
      body = list(
        where = where_clause, outFields = "parcel_number", returnGeometry = "false",
        returnCentroid = "true", f = "pjson"
      ), encode = "form"
    )
    
    if (httr::http_error(resp)) {
      stop(
        sprintf(
          paste0("ArcGIS request failed: HTTP %s\n", "Response: %s"),
          httr::status_code(resp), httr::content(resp, as = "text", encoding = "UTF-8")
        )
      )
    }
    
    js <- jsonlite::fromJSON(
      httr::content(resp, as = "text", encoding = "UTF-8"), simplifyVector = FALSE
    )
    
    if (!is.null(js$error)) {
      details <- paste(unlist(js$error$details %||% character(0)), collapse = "; ")
      
      stop(
        paste0("ArcGIS API error: ", js$error$message %||% "Unknown error",
          if (nzchar(details)) {paste0("\n", details)} else {""}
        )
      )
    }
    
    feats <- js$features
    
    if (is.null(feats) || length(feats) == 0) {
      return(
        data.frame(
          .parcel_clean = parcel_vec, x = NA_real_, y = NA_real_, stringsAsFactors = FALSE
        )
      )
    }
    
    got <- do.call(
      rbind,
      lapply(feats,
        function(feat) {
          attrs <- feat$attributes
          cent  <- feat$centroid
          data.frame(
            .parcel_clean = attrs$parcel_number %||% NA_character_,
            x = as.numeric(cent$x %||% NA_real_),
            y = as.numeric(cent$y %||% NA_real_), stringsAsFactors = FALSE
          )
        }
      )
    )
    
    got <- got[
      !duplicated(got$.parcel_clean),
      ,
      drop = FALSE
    ]
    
    missing <- setdiff(parcel_vec, got$.parcel_clean)
    
    if (length(missing) > 0) {
      got <- rbind(got,
        data.frame(.parcel_clean = missing, x = NA_real_, y = NA_real_,
          stringsAsFactors = FALSE
        )
      )
    }
    got
  }
  
  lookup_list <- lapply(chunks, fetch_chunk)
  lookup <- dplyr::bind_rows(lookup_list) %>% dplyr::rename(!!x_col := x, !!y_col := y)
  
  out <- out %>%
    dplyr::left_join(lookup, by = ".parcel_clean") %>% dplyr::select(-.parcel_clean)
  
  out
}

#

nearest_linear_feature <- function(
    parcels, features, parcel_id, feature_id, feature_name = NULL, metric_crs = 26917
) {
  stopifnot(inherits(parcels, "sf"))
  stopifnot(inherits(features, "sf"))
  
  if (nrow(features) == 0L) {stop("The feature layer contains no rows.")}
  
  parcels_m <- st_transform(parcels, metric_crs)
  features_m <- st_transform(features, metric_crs)
  
  nearest_index <- st_nearest_feature(parcels_m, features_m)
  
  distance_m <- st_distance(parcels_m, features_m[nearest_index, ], by_element = TRUE)
  
  result <- tibble(
    parcel_id = as.character(parcels[[parcel_id]]),
    feature_id = as.character(features[[feature_id]][nearest_index]),
    linear_distance_m = as.numeric(distance_m)
  )
  
  if (!is.null(feature_name)) {
    result$feature_name <-
      as.character(features[[feature_name]][nearest_index])
  } else {result$feature_name <- NA_character_}
  result
}

#

`%||%` <- function(x, y) {if (is.null(x) || length(x) == 0L) y else x}

graphhopper_route <- function(
    from_lon, from_lat, to_lon, to_lat, base_url = "http://localhost:8989",
    profile = "car", snap_preventions = c("ferry"), timeout_seconds = 60
) {
  route_url <- paste0(sub("/+$", "", base_url), "/route")
  
  body <- list(
    points = list(
      c(as.numeric(from_lon), as.numeric(from_lat)), c(as.numeric(to_lon), as.numeric(to_lat))
    ), profile = profile, instructions = FALSE, calc_points = FALSE, points_encoded = FALSE
  )
  
  if (length(snap_preventions) > 0L) {body$snap_preventions <- unname(snap_preventions)}
  
  response <- request(route_url) |> req_method("POST") |>
    req_body_json(body, auto_unbox = TRUE) |> req_timeout(timeout_seconds) |>
    req_retry(max_tries = 3) |> req_perform()
  
  result <- resp_body_json(response, simplifyVector = FALSE)
  
  if (is.null(result$paths) || length(result$paths) == 0L) {
    stop(result$message %||% "GraphHopper returned no route.")
  }
  
  path <- result$paths[[1]]
  snapped <- path$snapped_waypoints$coordinates
  
  tibble(
    network_distance_m = as.numeric(path$distance),
    network_time_min = as.numeric(path$time) / 60000,
    snapped_from_lon = snapped[[1]][[1]], snapped_from_lat = snapped[[1]][[2]],
    snapped_to_lon = snapped[[2]][[1]], snapped_to_lat = snapped[[2]][[2]]
  )
}

nearest_network_feature <- function(
    parcels, access_points, parcel_id, feature_id = "feature_id",
    feature_name = "feature_name", k = 10, base_url = "http://localhost:8989",
    profile = "car", metric_crs = 26917, snap_preventions = c("ferry"),
    choose_by = c("distance", "time")
) {
  choose_by <- match.arg(choose_by)
  stopifnot(inherits(parcels, "sf"))
  stopifnot(inherits(access_points, "sf"))
  if (nrow(access_points) == 0L) {stop("access_points contains no destination points.")}
  geometry_types <- unique(as.character(st_geometry_type(access_points)))
  
  if (!all(geometry_types %in% c("POINT", "MULTIPOINT"))) {
    stop("access_points must contain point geometry.")
  }
  
  parcels_m <- st_transform(parcels, metric_crs)
  access_m  <- st_transform(access_points, metric_crs)
  parcels_ll <- st_transform(parcels, 4326)
  access_ll  <- st_transform(access_points, 4326)
  parcel_xy <- st_coordinates(parcels_ll)
  access_xy <- st_coordinates(access_ll)
  
  map_dfr(seq_len(nrow(parcels_ll)), function(i) {
    candidate_linear_m <- as.numeric(st_distance(parcels_m[i, ], access_m))
    candidate_indices <- head(order(candidate_linear_m), min(k, nrow(access_m)))
    route_results <- map_dfr(candidate_indices, function(j) {
      route <- tryCatch(
        graphhopper_route(
          from_lon = parcel_xy[i, "X"], from_lat = parcel_xy[i, "Y"],
          to_lon = access_xy[j, "X"], to_lat = access_xy[j, "Y"],
          base_url = base_url, profile = profile, snap_preventions = snap_preventions
        ),
        error = function(e) {
          tibble(
            network_distance_m = NA_real_, network_time_min = NA_real_,
            snapped_from_lon = NA_real_, snapped_from_lat = NA_real_,
            snapped_to_lon = NA_real_, snapped_to_lat = NA_real_
          )
        }
      )
      
      route |>
        mutate(
          candidate_index = j, feature_id = as.character(access_points[[feature_id]][j]),
          feature_name = if (!is.null(feature_name)) {
            as.character(access_points[[feature_name]][j])
          } else {NA_character_}, candidate_linear_m = candidate_linear_m[j]
        )
    })
    
    valid_routes <- route_results |>
      filter(is.finite(network_distance_m), is.finite(network_time_min))
    
    if (nrow(valid_routes) == 0L) {
      return(
        tibble(
          parcel_id = as.character(parcels[[parcel_id]][i]), feature_id = NA_character_,
          feature_name = NA_character_, candidate_linear_m = NA_real_,
          network_distance_m = NA_real_, network_time_min = NA_real_,
          snapped_from_lon = NA_real_, snapped_from_lat = NA_real_,
          snapped_to_lon = NA_real_, snapped_to_lat = NA_real_
        )
      )
    }
    
    if (choose_by == "distance") {
      best <- valid_routes |> slice_min(network_distance_m, n = 1, with_ties = FALSE)
    } else {
      best <- valid_routes |> slice_min(network_time_min, n = 1, with_ties = FALSE)
    }
    
    best |>
      transmute(
        parcel_id = as.character(parcels[[parcel_id]][i]), feature_id, feature_name,
        candidate_linear_m, network_distance_m, network_time_min,
        snapped_from_lon, snapped_from_lat, snapped_to_lon, snapped_to_lat
      )
  })
}

#



#



#

#



#



#

#

save(backward_p, ratio_metrics, ratio_by, get_time_params, model_metrics_A, coef_table,
     pp_plot_resid, plot_stdresid_vs_stdpred, plot_cook_vs_rstudent, plot_lev_vs_rstudent,
     safe_vif, safe_vif_kable, ratio_stats_single, ratio_stats_by, within_band_flags,
     lm_spss_comparer, vif_compare_kable, plot_std_resid_hist, plot_resid_vs_months,
     plot_pred_vs_months, plot_months_vs_price_index, plot_months_vs_taf, plot_leverage_hist,
     plot_esp_vs_ratio, plot_ratio_hist, plot_tasp_vs_esp_by_style, save_model_bundle,
     lm_fit_stats, compare_lm_fit_stats, newdata_from_sale, newdata_from_sale2, gam_coef_table,
     backward_lm, prep_cod_sales, combine_sales_windows, make_taf_tbl, get_b,
     add_smooth_time_index, add_parcel_coords, add_linear_distance, add_drive_distance_ors,
     make_time_adjusted_prices, make_time_adjusted_df, plot_standard_residual_hist,
     plot_residuals_vs_months, plot_predicted_sppsf_vs_months, plot_price_index_vs_months,
     plot_taf_vs_months, plot_leverage_histogram, assess_ln_price_model,
     refit_inlier_ln_price, add_drive_distance_graphhopper, backward_pv2,
     make_time_adjusted_df_gam, spss_ratio_metrics, plot_avg_beta_lollipop_7models,
     summarize_extreme_betas_kable, plot_avg_beta_lollipop, plot_avg_beta_lollipop_all,
     summarize_avg_betas_kable_all, plot_avg_beta_lollipop_dfs, plot_beta_compare_lollipops,
     all_ratio_metrics, add_detroit_parcel_coords, nearest_linear_feature,
     graphhopper_route, nearest_network_feature,
     file = here("rmds/modfunctions", "modfunctions13.RData"))

#load(file = here("rmds/modfunctions", "modfunctions13.RData"))

my_funs <- list(
  prep_cod_sales = prep_cod_sales,
  backward_p = backward_p,
  add_inlier_flags_lm = add_inlier_flags_lm
)

saveRDS(my_funs, "my_funs.rds")

###

codf <- list(
  backward_p = backward_p,
  ratio_metrics = ratio_metrics,
  ratio_by = ratio_by,
  get_time_params = get_time_params,
  model_metrics_A = model_metrics_A,
  coef_table = coef_table,
  pp_plot_resid = pp_plot_resid,
  plot_stdresid_vs_stdpred = plot_stdresid_vs_stdpred,
  plot_cook_vs_rstudent = plot_cook_vs_rstudent,
  plot_lev_vs_rstudent = plot_lev_vs_rstudent,
  safe_vif = safe_vif,
  safe_vif_kable = safe_vif_kable,
  ratio_stats_single = ratio_stats_single,
  ratio_stats_by = ratio_stats_by,
  within_band_flags = within_band_flags,
  lm_spss_comparer = lm_spss_comparer,
  vif_compare_kable = vif_compare_kable,
  plot_std_resid_hist = plot_std_resid_hist,
  plot_resid_vs_months = plot_resid_vs_months,
  plot_pred_vs_months = plot_pred_vs_months,
  plot_months_vs_price_index = plot_months_vs_price_index,
  plot_months_vs_taf = plot_months_vs_taf,
  plot_leverage_hist = plot_leverage_hist,
  plot_esp_vs_ratio = plot_esp_vs_ratio,
  plot_ratio_hist = plot_ratio_hist,
  plot_tasp_vs_esp_by_style = plot_tasp_vs_esp_by_style,
  save_model_bundle = save_model_bundle,
  lm_fit_stats = lm_fit_stats,
  compare_lm_fit_stats = compare_lm_fit_stats,
  newdata_from_sale = newdata_from_sale,
  newdata_from_sale2 = newdata_from_sale2,
  gam_coef_table = gam_coef_table,
  backward_lm = backward_lm,
  prep_cod_sales = prep_cod_sales,
  combine_sales_windows = combine_sales_windows,
  make_taf_tbl = make_taf_tbl,
  get_b = get_b,
  add_smooth_time_index = add_smooth_time_index,
  add_parcel_coords = add_parcel_coords,
  add_linear_distance = add_linear_distance,
  add_drive_distance_ors = add_drive_distance_ors,
  make_time_adjusted_prices = make_time_adjusted_prices,
  make_time_adjusted_df = make_time_adjusted_df,
  plot_standard_residual_hist = plot_standard_residual_hist,
  plot_residuals_vs_months = plot_residuals_vs_months,
  plot_predicted_sppsf_vs_months = plot_predicted_sppsf_vs_months,
  plot_price_index_vs_months = plot_price_index_vs_months,
  plot_taf_vs_months = plot_taf_vs_months,
  plot_leverage_histogram = plot_leverage_histogram,
  assess_ln_price_model = assess_ln_price_model,
  refit_inlier_ln_price = refit_inlier_ln_price,
  add_drive_distance_graphhopper = add_drive_distance_graphhopper,
  backward_pv2 = backward_pv2,
  make_time_adjusted_df_gam = make_time_adjusted_df_gam,
  spss_ratio_metrics = spss_ratio_metrics,
  plot_avg_beta_lollipop_7models = plot_avg_beta_lollipop_7models,
  summarize_extreme_betas_kable = summarize_extreme_betas_kable,
  plot_avg_beta_lollipop = plot_avg_beta_lollipop,
  plot_avg_beta_lollipop_all = plot_avg_beta_lollipop_all,
  summarize_avg_betas_kable_all = summarize_avg_betas_kable_all,
  plot_avg_beta_lollipop_dfs = plot_avg_beta_lollipop_dfs,
  plot_beta_compare_lollipops = plot_beta_compare_lollipops,
  all_ratio_metrics = all_ratio_metrics,
  add_detroit_parcel_coords = add_detroit_parcel_coords,
  nearest_linear_feature = nearest_linear_feature,
  graphhopper_route = graphhopper_route,
  nearest_network_feature = nearest_network_feature
)

# You can then save it as an RDS file just like in your example:
saveRDS(codf, file = here("rmds/modfunctions", "codfunctions.rds"))

###

# install.packages(c("usethis", "devtools", "roxygen2"))
# usethis::create_package("~/path/to/codtools")
#usethis::create_package(here("notes/codtools"))
usethis::create_package("C:/Users/satur/GitHub/dOCFOmodeling/codtools")


###

packageVersion("rlang")
# 
# sessionInfo()
packageVersion("usethis") # usethis: Automate Package and Project Setup
packageVersion("devtools") # devtools: Tools to Make Developing R Packages Easier
packageVersion("roxygen2") # in-line documentation for r
library(rlang)
library(usethis)
library(devtools)
library(here)
library(roxygen2)

###



###



###












