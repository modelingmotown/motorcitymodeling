

# Heatmap 5

plot_strength_heatmap <- function(comparison_df) {
  
  metric_rules <- tibble::tribble(
    ~metric,           ~rule,             ~target,
    "r",               "higher_better",    NA_real_,
    "r_squared",       "higher_better",    NA_real_,
    "adj_r_squared",   "higher_better",    NA_real_,
    "see",             "lower_better",     NA_real_,
    "rmse",            "lower_better",     NA_real_,
    "mae",             "lower_better",     NA_real_,
    "f_stat",          "higher_better",    NA_real_,
    "median_ratio",    "closer_to_target", 1.00,
    "COD",             "lower_better",     NA_real_,
    "PRD",             "closer_to_target", 1.00,
    "PRB",             "closer_to_target", 0.00,
    "within10pct",     "higher_better",    NA_real_,
    "within20pct",     "higher_better",    NA_real_,
    "within50pct",     "higher_better",    NA_real_
  )
  
  metric_labels <- c(
    r              = "R",
    r_squared      = "R²",
    adj_r_squared  = "Adj. R²",
    see            = "SEE",
    rmse           = "RMSE",
    mae            = "MAE",
    f_stat         = "F",
    median_ratio   = "Median Ratio",
    COD            = "COD",
    PRD            = "PRD",
    PRB            = "PRB",
    within10pct    = "Within 10%",
    within20pct    = "Within 20%",
    within50pct    = "Within 50%"
  )
  
  metric_order <- c(
    "R", "R²", "Adj. R²", "SEE", "RMSE", "MAE", "F",
    "Median Ratio", "COD", "PRD", "PRB",
    "Within 10%", "Within 20%", "Within 50%"
  )
  
  df <- comparison_df |>
    left_join(metric_rules, by = "metric") |>
    mutate(
      raw_strength = case_when(
        rule == "higher_better" ~ r_value - spss_value,
        rule == "lower_better"  ~ spss_value - r_value,
        rule == "closer_to_target" ~
          abs(spss_value - target) - abs(r_value - target),
        TRUE ~ NA_real_
      )
    ) |>
    group_by(metric) |>
    mutate(
      max_abs_strength = max(abs(raw_strength), na.rm = TRUE),
      strength_scaled = case_when(
        is.na(raw_strength) ~ NA_real_,
        max_abs_strength == 0 ~ 0,
        TRUE ~ raw_strength / max_abs_strength
      )
    ) |>
    ungroup() |>
    mutate(
      district_num = readr::parse_number(as.character(district)),
      model_num = readr::parse_number(as.character(model)),
      row_id = paste0("District ", district_num, " – Model ", model_num),
      row_id = factor(row_id, levels = rev(unique(row_id[order(district_num, model_num)]))),
      metric_label = factor(metric_labels[metric], levels = metric_order),
      diff_label = case_when(
        metric == "COD" ~ sprintf("%+.2f", r_value - spss_value),
        metric %in% c("within10pct", "within20pct", "within50pct") ~ sprintf("%+.2f", r_value - spss_value),
        TRUE ~ sprintf("%+.4f", r_value - spss_value)
      )
    )
  
  ggplot(
    df,
    aes(x = metric_label, y = row_id, fill = strength_scaled)
  ) +
    geom_tile(color = "white", linewidth = 0.7) +
    geom_text(aes(label = diff_label), size = 2.5) +
    scale_fill_gradient2(
      low = "#D55E00",
      mid = "white",
      high = "#0072B2",
      midpoint = 0,
      limits = c(-1, 1),
      oob = scales::squish,
      name = "R relative\nto SPSS"
    ) +
    labs(
      title = "R Strength/Weakness Relative to SPSS",
      subtitle = "Blue = R stronger; orange = R weaker; labels show R − SPSS",
      x = NULL,
      y = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
      axis.text.y = element_text(size = 8),
      plot.title = element_text(face = "bold", size = 16)
    )
}

plot_strength_heatmap(comparison)


# Heatmap 2

comparison <- comparison |>
  mutate(
    difference_label = case_when(
      metric %in% c("r_squared", "adj_r_squared", "median_ratio",
                    "PRD", "PRB") ~ sprintf("%+.4f", difference),
      metric == "COD" ~ sprintf("%+.2f", difference),
      metric == "see" ~ sprintf("%+.4f", difference),
      metric == "f_stat" ~ sprintf("%+.2f", difference),
      TRUE ~ sprintf("%+.3f", difference)
    )
  )

heatmap_plot <- ggplot(
  comparison,
  aes(x = metric_label, y = model_id, fill = discrepancy_score)
) + geom_tile(color = "white", linewidth = 0.7) +
  geom_text(aes(label = difference_label), size = 2.5) +
  scale_fill_gradientn(
    colours = c("#1a9850", "#91cf60", "#fee08b", "#fc8d59", "#d73027"),
    values = scales::rescale(c(0, 0.5, 1, 2, 3)), limits = c(0, 3), oob = scales::squish,
    name = "|R − SPSS| /\nTolerance"
  ) +
  labs(
    title = "SPSS vs R Model Reproducibility",
    subtitle = "Cell values show R − SPSS; color represents discrepancy relative to tolerance",
    x = NULL, y = NULL
  ) + theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.text.y = element_text(size = 8), plot.title = element_text(face = "bold", size = 16)
  )

heatmap_plot

# Dumbbell 1

make_dumbbell <- function(
    data, metric_name, metric_title = metric_name, digits = 4, show_difference = TRUE
) {
  plot_df <- data |>    # Filter to requested metric
    filter(metric == metric_name) |>
    mutate(
      district_num = readr::parse_number(as.character(district)),
      model_num    = readr::parse_number(as.character(model)),
      district_lab = paste0("District ", district_num),
      model_lab    = paste0("Model ", model_num),
      difference   = r_value - spss_value,
      midpoint     = (r_value + spss_value) / 2
    ) |> arrange(district_num, model_num) |>
    mutate(
      district_lab = factor(
        district_lab, levels = paste0("District ", sort(unique(district_num)))
      ), model_lab = factor(model_lab, levels = rev(paste0("Model ", sort(unique(model_num)))))
    )
  
  x_center <- mean(range(c(plot_df$spss_value, plot_df$r_value), na.rm = TRUE))
  
  
  # Long version for plotting SPSS/R points
  point_df <- plot_df |> dplyr::select(district_lab, model_lab, spss_value, r_value) |>
    tidyr::pivot_longer(
      cols = c(spss_value, r_value), names_to = "software", values_to = "value"
    ) |>
    dplyr::mutate(
      software = dplyr::case_when(
        software == "spss_value" ~ "SPSS", software == "r_value" ~ "R", TRUE ~ software
      )
    )
  
  p <- ggplot() +
    geom_segment(    # Line connecting SPSS and R
      data = plot_df, aes(x = spss_value, xend = r_value, y = model_lab, yend = model_lab),
      linewidth = 1, color = "grey60"
    ) +              # SPSS and R values
    geom_point(data = point_df, aes(x = value, y = model_lab, color = software), size = 3.5) +
    facet_wrap(~ district_lab, ncol = 2) +
    scale_color_manual(values = c("SPSS" = "#D55E00", "R" = "#0072B2")) +
    geom_text(data = plot_df, aes(x = x_center, y = model_lab, label = model_lab),
              inherit.aes = FALSE, fontface = "bold", color = "grey35", size = 3.8) +
    labs(
      title = paste0(metric_title, ": SPSS vs R"),
      subtitle = "Each line connects the SPSS and R result for the same AVM",
      x = metric_title, y = NULL, color = NULL
    ) + theme_minimal(base_size = 12) +
    theme(
      panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold", size = 11),
      # axis.text.y = element_text(face = "bold"),
      axis.text.y = element_blank(), axis.ticks.y = element_blank(),
      plot.title = element_text(face = "bold", size = 16), legend.position = "top",
      panel.spacing.x = grid::unit(1.5, "cm"), panel.spacing.y = grid::unit(0.8, "cm")
    )
  
  if (show_difference) {    # Optional R - SPSS labels
    plot_df <- plot_df |>
      mutate(diff_label = paste0("\u0394 = ", sprintf(paste0("%+.", digits, "f"), difference)))
    
    p <- p +
      geom_text(
        data = plot_df, aes(x = midpoint, y = model_lab, label = diff_label),
        nudge_y = 0.22, size = 2.8, color = "grey30"
      )
  }
  p
}

p_adj_r2 <- make_dumbbell(
  comparison,
  metric_name = "adj_r_squared",
  metric_title = "Adjusted R²", show_difference = FALSE,
  digits = 4
)

p_adj_r2


# Concordance II

coef_plot_df <- coef_compare |>
  dplyr::filter(predictor != "(Intercept)")

p_coef <- ggplot(
  coef_plot_df,
  aes(
    x = spss_coef,
    y = r_coef
  )
) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    color = "grey40"
  ) +
  geom_point(
    size = 2,
    alpha = 0.8
  ) +
  facet_grid(
    district ~ model
  ) +
  coord_equal() +
  labs(
    title = "SPSS vs R Coefficient Concordance",
    subtitle = "Intercept excluded; dashed line represents perfect agreement",
    x = "SPSS Coefficient (B)",
    y = "R Coefficient"
  ) +
  theme_minimal(base_size = 10)

p_coef


## Concordance III

plot_coef_concordance <- function(
    data,
    model_number,
    exclude_intercept = TRUE,
    label_threshold = NULL
) {
  
  df <- data |>
    dplyr::filter(
      model_num == model_number
    )
  
  if (exclude_intercept) {
    df <- df |>
      dplyr::filter(
        predictor != "(Intercept)"
      )
  }
  
  if (!is.null(label_threshold)) {
    df <- df |>
      dplyr::mutate(
        label = ifelse(
          abs_diff >= label_threshold,
          predictor,
          NA_character_
        )
      )
  }
  
  p <- ggplot(
    df,
    aes(
      x = spss_coef,
      y = r_coef
    )
  ) +
    geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "dashed",
      linewidth = 0.8,
      color = "grey40"
    ) +
    geom_point(
      aes(color = abs_diff),
      size = 3,
      alpha = 0.85
    ) +
    scale_color_gradient(
      low = "#1a9850",
      high = "#d73027",
      name = "|R − SPSS|"
    ) +
    facet_wrap(
      ~ district,
      ncol = 2
    ) +
    coord_equal() +
    labs(
      title = paste0(
        "Model ",
        model_number,
        ": SPSS vs R Coefficient Concordance"
      ),
      subtitle = "Dashed 1:1 line represents identical coefficient estimates",
      x = "SPSS Coefficient (B)",
      y = "R Coefficient"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(
        face = "bold"
      ),
      plot.title = element_text(
        face = "bold",
        size = 16
      )
    )
  
  if (!is.null(label_threshold)) {
    
    p <- p +
      ggrepel::geom_text_repel(
        aes(label = label),
        size = 3,
        max.overlaps = 20,
        na.rm = TRUE
      )
  }
  
  p
}

plot_coef_concordance(
  coef_compare,
  model_number = 5,
  label_threshold = 0.01
)

# nu conchondrd

p_coef <- ggplot(
  coef_compare, aes(x = spss_coef, y = r_coef)
) +  # Perfect-agreement reference line
  geom_abline(
    intercept = 0, slope = 1, linetype = "dashed", linewidth = 0.5, color = "black"
  ) +  # Concordance points: blue fill = R, orange outline = SPSS
  geom_point(
    shape = 21, size = 2, stroke = 0.6, fill = "#0072B2",
    color = "#D55E00", alpha = 0.60) + coord_equal() +
  labs(
    title = "SPSS vs R Coefficient Concordance",
    subtitle = "Points on the dashed 1:1 line indicate equivalent coefficient estimates",
    x = "SPSS Coefficient", y = "R Coefficient"
  ) + theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16), axis.title = element_text(face = "bold")
  )
p_coef




## IAAO-focused Scorecard

iaao_df <- dplyr::bind_rows(r_metrics, spss_metrics)

iaao_df <- iaao_df |>
  dplyr::mutate(
    district_num = readr::parse_number(as.character(district)),
    model_num    = readr::parse_number(as.character(model))
  )

iaao_long <- iaao_df |>
  dplyr::select(source, district_num, model_num, median_ratio, COD, PRD, PRB) |>
  tidyr::pivot_longer(
    cols = c(median_ratio, COD, PRD, PRB), names_to = "metric", values_to = "value"
  )

iaao_limits <- tibble::tribble(
  ~metric,         ~lower, ~upper,
  "median_ratio",   0.90,   1.10,
  "COD",            5.00,  15.00,
  "PRD",            0.98,   1.03,
  "PRB",           -0.05,   0.05
)

iaao_long <- iaao_long |> dplyr::left_join(iaao_limits, by = "metric") |>
  dplyr::mutate(
    status = dplyr::case_when(
      is.na(value) ~ "Missing",
      value >= lower & value <= upper ~ "Within range", TRUE ~ "Outside range"
    )
  )

iaao_long <- iaao_long |>
  dplyr::mutate(model_id = paste0("District ", district_num, " – Model ", model_num),
                metric_label = dplyr::recode(
                  metric, median_ratio = "Median Ratio", COD = "COD", PRD = "PRD", PRB = "PRB"
                ),
                value_label = dplyr::case_when(
                  metric == "COD" ~ sprintf("%.2f", value), TRUE ~ sprintf("%.3f", value)
                )
  )

iaao_long <- iaao_long |>
  dplyr::arrange(district_num, model_num) |>
  dplyr::mutate(
    model_id = factor(model_id, levels = rev(unique(model_id))),
    metric_label = factor(metric_label, levels = c("Median Ratio", "COD", "PRD", "PRB")),
    source = factor(source, levels = c("SPSS", "R"))
  )

p_iaao <- ggplot(
  iaao_long, aes(x = metric_label, y = model_id, fill = status)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(
    aes(label = value_label), fontface = "bold", size = 3) + facet_wrap(~ source, nrow = 1) +
  scale_fill_manual(
    values = c("Within range" = "#4DAF4A", "Outside range" = "#E41A1C", "Missing" = "grey80")
  ) +
  labs(
    title = "IAAO Performance Scorecard",
    subtitle = "SPSS and R model results across seven districts and five AVM specifications",
    x = NULL, y = NULL, fill = NULL
  ) +  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(), strip.text = element_text(face = "bold",  size = 13),
    axis.text.x = element_text(face = "bold", size = 10),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(face = "bold",  size = 16),
    legend.position = "top", panel.spacing.x = grid::unit(1.5, "cm")
  )

p_iaao



























