# Industry visualizations for race/ethnicity concentration
#
# Expects (created in Industry/industry_analysis.R):
# - naics_conc_long: data.table with columns sector, prace_adj, cagr_24_34,
#   chg_24_34, diff_share, z_score
# - naics_stats: data.table/data.frame with columns sector, emp_2034, cagr_24_34,
#   chg_24_34, WAGP_median, poc_share_diff, female_share_diff
#
# Adapted from Occupation/occupation_vizes.R::make_focus_bubble_plot(), sized by
# emp_2034 instead of openings (industries have no "openings" concept).

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(ggplot2)
})

.required_naics_stats_cols <- c(
  "sector",
  "emp_2034",
  "WAGP_median",
  "poc_share_diff",
  "female_share_diff"
)

.required_equity_dot_cols <- c(
  "sector",
  "display_order",
  "female_share_diff",
  "poc_share_diff"
)

.check_naics_cols <- function(df, cols, df_name = deparse(substitute(df))) {
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0) {
    stop(sprintf(
      "%s is missing required columns: %s",
      df_name,
      paste(missing, collapse = ", ")
    ), call. = FALSE)
  }
}

.clip_industry_label <- function(x) {
  # Keep only the text before the first comma (if any)
  x <- as.character(x)
  x <- sub(",.*$", "", x)
  trimws(x)
}

#' Bubble chart: industries by signed POC and female concentration
#'
#' @param naics_stats Industry stats table (see Industry/industry_analysis.R).
#'   Required cols: sector, emp_2034, WAGP_median, poc_share_diff, female_share_diff.
#' @param emp_min Filter threshold for emp_2034.
#' @param label_width Wrap width for industry labels.
#' @param max_size Max bubble size.
#'
#' @return A ggplot object.
make_bubble_plot_naics <- function(
    naics_stats,
    emp_min = 1000,
    label_width = 25,
    max_size = 14) {

  .check_naics_cols(naics_stats, .required_naics_stats_cols, "naics_stats")

  naics_plot_df <- copy(as.data.table(naics_stats)) %>%
    filter(emp_2034 > emp_min) %>%
    mutate(
      Label_short = .clip_industry_label(sector),
      WAGP_median = as.numeric(WAGP_median),
      x_coord = as.numeric(poc_share_diff),
      y_coord = as.numeric(female_share_diff),
      Label_wrapped = ifelse(
        is.na(Label_short),
        NA_character_,
        vapply(
          as.character(Label_short),
          function(x) paste(strwrap(x, width = label_width), collapse = "\n"),
          character(1)
        )
      ),
      r_origin = sqrt(x_coord^2 + y_coord^2),
      r_outer = r_origin >= median(r_origin, na.rm = TRUE),
      quadrant = dplyr::case_when(
        is.na(x_coord) | is.na(y_coord) ~ NA_character_,
        x_coord >= 0 & y_coord >= 0 ~ "Q1",
        x_coord < 0 & y_coord >= 0 ~ "Q2",
        x_coord < 0 & y_coord < 0 ~ "Q3",
        TRUE ~ "Q4"
      )
    )

  x_lim <- suppressWarnings(max(abs(naics_plot_df$x_coord), na.rm = TRUE))
  y_lim <- suppressWarnings(max(abs(naics_plot_df$y_coord), na.rm = TRUE))

  if (!is.finite(x_lim) || x_lim == 0) {
    x_lim <- 0.01
  }
  if (!is.finite(y_lim) || y_lim == 0) {
    y_lim <- 0.01
  }

  x_lim <- x_lim * 1.08
  y_lim <- y_lim * 1.08

  # Size nudges relative to data range
  x_rng <- 2 * x_lim
  y_rng <- 2 * y_lim
  dx <- if (is.finite(x_rng) && x_rng > 0) 0.03 * x_rng else 0.02
  dy <- if (is.finite(y_rng) && y_rng > 0) 0.03 * y_rng else 0.02

  p <- ggplot2::ggplot(
    naics_plot_df,
    ggplot2::aes(
      x = x_coord,
      y = y_coord,
      size = emp_2034,
      fill = WAGP_median
    )
  ) +
    ggplot2::geom_vline(xintercept = 0, color = "grey70", linewidth = 0.4) +
    ggplot2::geom_hline(yintercept = 0, color = "grey70", linewidth = 0.4) +
    ggplot2::geom_point(
      shape = 21,
      color = "grey25",
      stroke = 0.25,
      alpha = 0.95,
      na.rm = TRUE
    ) +
    ggplot2::scale_fill_gradient(
      low = "#FFFF00",
      high = "#8E0152",
      na.value = "grey80",
      name = "Median wage"
    ) +
    ggplot2::scale_x_continuous(
      limits = c(-x_lim, x_lim),
      labels = scales::label_percent(accuracy = 1),
      expand = ggplot2::expansion(mult = 0.02)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(-y_lim, y_lim),
      labels = scales::label_percent(accuracy = 1),
      expand = ggplot2::expansion(mult = 0.02)
    ) +
    ggplot2::scale_size_area(max_size = max_size, name = "Employment (2034)") +
    ggplot2::labs(
      x = "POC share difference (industry - workforce)",
      y = "Female share difference (industry - workforce)",
      subtitle = "Positive x = POC, negative x = Non-POC; positive y = female, negative y = male"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position = "right",
      panel.grid.minor = ggplot2::element_blank()
    )

  if (requireNamespace("ggrepel", quietly = TRUE)) {
    repel_layer <- function(df, nudge_x, nudge_y) {
      ggrepel::geom_text_repel(
        data = df,
        mapping = ggplot2::aes(label = Label_wrapped),
        size = 4,
        nudge_x = nudge_x,
        nudge_y = nudge_y,
        min.segment.length = 0,
        box.padding = 0.25,
        point.padding = 0.2,
        max.overlaps = Inf,
        force = 2,
        force_pull = 0.5,
        max.iter = 5000,
        seed = 1
      )
    }

    p <- p +
      repel_layer(filter(naics_plot_df, quadrant == "Q1", !r_outer), -dx, -dy) +
      repel_layer(filter(naics_plot_df, quadrant == "Q1", r_outer),  dx,  dy) +
      repel_layer(filter(naics_plot_df, quadrant == "Q2", !r_outer),  dx, -dy) +
      repel_layer(filter(naics_plot_df, quadrant == "Q2", r_outer),  -dx,  dy) +
      repel_layer(filter(naics_plot_df, quadrant == "Q3", !r_outer),  dx,  dy) +
      repel_layer(filter(naics_plot_df, quadrant == "Q3", r_outer),  -dx, -dy) +
      repel_layer(filter(naics_plot_df, quadrant == "Q4", !r_outer), -dx,  dy) +
      repel_layer(filter(naics_plot_df, quadrant == "Q4", r_outer),   dx, -dy)
  } else {
    text_layer <- function(df, nudge_x, nudge_y) {
      ggplot2::geom_text(
        data = df,
        mapping = ggplot2::aes(label = Label_wrapped),
        size = 2.2,
        position = ggplot2::position_nudge(x = nudge_x, y = nudge_y)
      )
    }

    p <- p +
      text_layer(filter(naics_plot_df, quadrant == "Q1"),  dx,  dy) +
      text_layer(filter(naics_plot_df, quadrant == "Q2"), -dx,  dy) +
      text_layer(filter(naics_plot_df, quadrant == "Q3"), -dx, -dy) +
      text_layer(filter(naics_plot_df, quadrant == "Q4"),  dx, -dy)
  }

  p
}

#' Paired dot display: female and POC share differences by industry
#'
#' @param naics_stats Industry stats table (see Industry/industry_analysis.R).
#'   Required cols: sector, display_order, female_share_diff, poc_share_diff.
#' @param point_size Dot size.
#'
#' @return A ggplot object with one column for each equity dimension.
make_equity_dot_plot_naics <- function(naics_stats, point_size = 3) {
  .check_naics_cols(naics_stats, .required_equity_dot_cols, "naics_stats")

  plot_df <- copy(as.data.table(naics_stats))[
    order(display_order),
    .(
      sector = as.character(sector),
      display_order,
      `Female share difference` = as.numeric(female_share_diff),
      `POC share difference` = as.numeric(poc_share_diff)
    )
  ]

  industry_levels <- rev(unique(plot_df$sector))
  plot_df <- data.table::melt(
    plot_df,
    id.vars = c("sector", "display_order"),
    variable.name = "dimension",
    value.name = "share_difference",
    variable.factor = TRUE
  )
  plot_df[, sector := factor(sector, levels = industry_levels)]

  plot_limit <- suppressWarnings(max(abs(plot_df$share_difference), na.rm = TRUE))
  if (!is.finite(plot_limit) || plot_limit == 0) {
    plot_limit <- 0.01
  }
  plot_limit <- plot_limit * 1.08

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = share_difference, y = sector)
  ) +
    ggplot2::geom_hline(
      yintercept = seq_len(length(industry_levels)),
      color = "grey92",
      linewidth = 0.3
    ) +
    ggplot2::geom_vline(xintercept = 0, color = "grey55", linewidth = 0.5) +
    ggplot2::geom_point(
      color = "#2166AC",
      size = point_size,
      na.rm = TRUE
    ) +
    ggplot2::facet_grid(cols = ggplot2::vars(dimension)) +
    ggplot2::scale_x_continuous(
      limits = c(-plot_limit, plot_limit),
      labels = scales::label_percent(accuracy = 1),
      expand = ggplot2::expansion(mult = 0.03)
    ) +
    ggplot2::labs(
      x = "Share difference (industry - workforce)",
      y = NULL,
      subtitle = "Negative values indicate underrepresentation; positive values indicate overrepresentation."
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.spacing.x = grid::unit(1.25, "lines"),
      strip.text = ggplot2::element_text(face = "bold"),
      axis.text.y = ggplot2::element_text(color = "grey20")
    )
}

