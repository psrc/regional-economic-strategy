# Occupation visualizations for race/ethnicity concentration
#
# Expects (typically created in Occupation/occupation_analysis.R):
# - race_conc_long: data.table with columns soc_focus, prace_adj, diff_share, diff_moe, z_score
# - soc_stats: data.table/data.frame with columns soc_code, Label, openings_total_23_33
#
# This file defines plot-builder functions.
# For convenience, it will source Occupation/occupation_analysis.R only if the
# expected upstream objects are not already present in the environment.

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(ggplot2)
})

.required_soc_stats_cols <- c(
  "Label",
  "openings_total_23_33",
  "WAGP_median",
  "poc_share_diff",
  "female_share_diff"
)

.required_race_conc_cols <- c("soc_focus", "prace_adj", "diff_share", "z_score")

.has_required_cols <- function(df, cols) {
  is.data.frame(df) && all(cols %in% names(df))
}

.get_object_if_valid <- function(name, cols, env = parent.frame(), inherits = TRUE) {
  if (!exists(name, envir = env, inherits = inherits)) {
    return(NULL)
  }

  obj <- get(name, envir = env, inherits = inherits)
  if (!.has_required_cols(obj, cols)) {
    return(NULL)
  }

  obj
}

if (is.null(.get_object_if_valid("soc_stats", .required_soc_stats_cols)) ||
    is.null(.get_object_if_valid("race_conc_long", .required_race_conc_cols))) {
  source("Occupation/occupation_analysis.R")
}

.check_cols <- function(df, cols, df_name = deparse(substitute(df))) {
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0) {
    stop(sprintf(
      "%s is missing required columns: %s",
      df_name,
      paste(missing, collapse = ", ")
    ), call. = FALSE)
  }
}

.clip_occ_label <- function(x) {
  # Keep only the text before the first comma (if any)
  x <- as.character(x)
  x <- sub(",.*$", "", x)
  trimws(x)
}

#' Bubble chart: focus occupations by signed POC and female concentration
#'
#' This is the bubble plot previously at the end of occupation_analysis.R,
#' refactored into a plot-builder function.
#'
#' @param soc_stats Occupation stats table.
#'   Required cols: Label, openings_total_23_33, WAGP_median,
#'   poc_share_diff, female_share_diff.
#' @param openings_min Filter threshold for openings_total_23_33.
#' @param label_width Wrap width for occupation labels.
#' @param max_size Max bubble size.
#'
#' @return A ggplot object.
make_focus_bubble_plot <- function(
    soc_stats,
    openings_min = 1000,
    label_width = 25,
    max_size = 14) {

  if (!.has_required_cols(soc_stats, .required_soc_stats_cols)) {
    analysis_env <- new.env(parent = parent.frame())
    source("Occupation/occupation_analysis.R", local = analysis_env)

    refreshed_soc_stats <- .get_object_if_valid(
      "soc_stats",
      .required_soc_stats_cols,
      env = analysis_env,
      inherits = FALSE
    )

    if (!is.null(refreshed_soc_stats)) {
      soc_stats <- refreshed_soc_stats
    }
  }

  .check_cols(
    soc_stats,
    .required_soc_stats_cols,
    "soc_stats"
  )

  soc_plot_df <- copy(as.data.table(soc_stats)) %>%
    filter(openings_total_23_33 > openings_min) %>%
    mutate(
      Label_short = .clip_occ_label(Label),
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

  x_lim <- suppressWarnings(max(abs(soc_plot_df$x_coord), na.rm = TRUE))
  y_lim <- suppressWarnings(max(abs(soc_plot_df$y_coord), na.rm = TRUE))

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
    soc_plot_df,
    ggplot2::aes(
      x = x_coord,
      y = y_coord,
      size = openings_total_23_33,
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
    ggplot2::scale_size_area(max_size = max_size, name = "Openings (23–33)") +
    ggplot2::labs(
      x = "POC share difference (occupation - workforce)",
      y = "Female share difference (occupation - workforce)",
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
      repel_layer(filter(soc_plot_df, quadrant == "Q1", !r_outer), -dx, -dy) +
      repel_layer(filter(soc_plot_df, quadrant == "Q1", r_outer),  dx,  dy) +
      repel_layer(filter(soc_plot_df, quadrant == "Q2", !r_outer),  dx, -dy) +
      repel_layer(filter(soc_plot_df, quadrant == "Q2", r_outer),  -dx,  dy) +
      repel_layer(filter(soc_plot_df, quadrant == "Q3", !r_outer),  dx,  dy) +
      repel_layer(filter(soc_plot_df, quadrant == "Q3", r_outer),  -dx, -dy) +
      repel_layer(filter(soc_plot_df, quadrant == "Q4", !r_outer), -dx,  dy) +
      repel_layer(filter(soc_plot_df, quadrant == "Q4", r_outer),   dx, -dy)
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
      text_layer(filter(soc_plot_df, quadrant == "Q1"),  dx,  dy) +
      text_layer(filter(soc_plot_df, quadrant == "Q2"), -dx,  dy) +
      text_layer(filter(soc_plot_df, quadrant == "Q3"), -dx, -dy) +
      text_layer(filter(soc_plot_df, quadrant == "Q4"),  dx, -dy)
  }

  p
}

#' Heatmap of race/ethnicity concentration using Z-scores
#'
#' @param race_conc_long Long-form race concentration table.
#'   Required cols: soc_focus, prace_adj, z_score.
#'   Optional: diff_share (used only for potential labels).
#' @param soc_stats Occupation stats table (for labels and openings filter).
#'   Required cols: soc_code, Label, openings_total_23_33.
#' @param openings_min Filter threshold for openings_total_23_33.
#' @param z_limit Optional symmetric limit for the fill scale (e.g., 3).
#'   If NULL, uses the max absolute observed z (clipped to >= 1).
#'
#' @return A ggplot object.
make_race_conc_heatmap_z <- function(
    race_conc_long,
    soc_stats,
    openings_min = 5000,
    z_limit = NULL) {

  .check_cols(race_conc_long, c("soc_focus", "prace_adj", "z_score"), "race_conc_long")
  .check_cols(soc_stats, c("soc_code", "Label", "openings_total_23_33"), "soc_stats")

  rc <- as.data.table(copy(race_conc_long))
  ss <- as.data.table(copy(soc_stats))

  # Map occupation code -> label + openings
  ss_lab <- ss[, .(
    soc_focus = as.character(soc_code),
    Label = .clip_occ_label(Label),
    openings_total_23_33 = as.numeric(openings_total_23_33)
  )]

  plot_dt <- merge(rc, ss_lab, by = "soc_focus", all.x = FALSE, all.y = FALSE)
  plot_dt <- plot_dt[is.finite(openings_total_23_33) & openings_total_23_33 > openings_min]

  # Order occupations by overall magnitude of concentration (sum |z| across race groups)
  ord <- plot_dt[, .(ord = sum(abs(z_score), na.rm = TRUE)), by = .(soc_focus, Label)]
  ord <- ord[order(ord, decreasing = TRUE)]
  occ_levels <- ord$Label

  plot_dt[, Label := factor(Label, levels = occ_levels)]
  plot_dt[, prace_adj := factor(as.character(prace_adj))]

  if (is.null(z_limit)) {
    z_limit <- max(1, suppressWarnings(max(abs(plot_dt$z_score), na.rm = TRUE)))
  }

  ggplot(plot_dt, aes(x = prace_adj, y = Label, fill = z_score)) +
    geom_tile(color = NA) +
    scale_fill_gradient2(
      low = "#2C7BB6",
      mid = "white",
      high = "#D7191C",
      midpoint = 0,
      limits = c(-z_limit, z_limit),
      oob = scales::squish,
      name = "Z-score\n(diff vs workforce)"
    ) +
    labs(
      x = "Race / ethnicity",
      y = "Occupation",
      title = "Race/ethnicity concentration in focus occupations (Z-scores)",
      subtitle = paste0("Filtered to occupations with projected openings > ", openings_min, " by 2033")
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "right"
    )
}

#' Faceted Cleveland dot plot of race/ethnicity concentration (difference in share)
#'
#' @param race_conc_long Long-form race concentration table.
#'   Required cols: soc_focus, prace_adj, diff_share.
#'   Optional: diff_moe (for 90% MOE error bars).
#' @param soc_stats Occupation stats table (for labels and openings filter).
#'   Required cols: soc_code, Label, openings_total_23_33.
#' @param openings_min Filter threshold for openings_total_23_33.
#'
#' @return A ggplot object.
make_race_conc_cleveland <- function(
    race_conc_long,
    soc_stats,
    openings_min = 5000) {

  .check_cols(race_conc_long, c("soc_focus", "prace_adj", "diff_share"), "race_conc_long")
  .check_cols(soc_stats, c("soc_code", "Label", "openings_total_23_33"), "soc_stats")

  rc <- as.data.table(copy(race_conc_long))
  ss <- as.data.table(copy(soc_stats))

  ss_lab <- ss[, .(
    soc_focus = as.character(soc_code),
    Label = .clip_occ_label(Label),
    openings_total_23_33 = as.numeric(openings_total_23_33)
  )]

  plot_dt <- merge(rc, ss_lab, by = "soc_focus", all.x = FALSE, all.y = FALSE)
  plot_dt <- plot_dt[is.finite(openings_total_23_33) & openings_total_23_33 > openings_min]

  # Order occupations by overall magnitude of difference (sum |diff| across groups)
  ord <- plot_dt[, .(ord = sum(abs(diff_share), na.rm = TRUE)), by = .(soc_focus, Label)]
  ord <- ord[order(ord, decreasing = TRUE)]
  occ_levels <- ord$Label

  plot_dt[, Label := factor(Label, levels = rev(occ_levels))]
  plot_dt[, prace_adj := factor(as.character(prace_adj))]

  # Optional 90% MOE error bars if diff_moe exists
  has_moe <- "diff_moe" %in% names(plot_dt)
  if (has_moe) {
    plot_dt[, xmin := diff_share - diff_moe]
    plot_dt[, xmax := diff_share + diff_moe]
  }

  p <- ggplot(plot_dt, aes(x = diff_share, y = Label)) +
    geom_vline(xintercept = 0, color = "grey50", linewidth = 0.4) +
    {
      if (has_moe) {
        geom_segment(
          aes(x = xmin, xend = xmax, y = Label, yend = Label),
          alpha = 0.6,
          linewidth = 0.4
        )
      }
    } +
    geom_point(size = 1.7, alpha = 0.85) +
    facet_wrap(~ prace_adj, scales = "free_y", ncol = 2) +
    scale_x_continuous(labels = scales::label_percent(accuracy = 0.1)) +
    labs(
      x = "Difference in share (occupation − workforce)",
      y = NULL,
      title = "Race/ethnicity concentration by occupation (difference in share)",
      subtitle = paste0("Filtered to occupations with projected openings > ", openings_min, " by 2033")
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold"),
      legend.position = "none"
    )

  p
}

# Usage:
# If race_conc_long / soc_stats aren't already in memory, this file will source
# Occupation/occupation_analysis.R once.
#
p0 <- make_focus_bubble_plot(soc_stats, openings_min = 500)
p1 <- make_race_conc_heatmap_z(race_conc_long, soc_stats, openings_min = 0, z_limit=20)
p2 <- make_race_conc_cleveland(race_conc_long, soc_stats, openings_min = 0)

print(p0)
print(p1)
print(p2)
