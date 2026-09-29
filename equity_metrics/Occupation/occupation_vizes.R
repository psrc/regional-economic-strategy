# Occupation visualizations for race/ethnicity concentration
#
# Expects (typically created in equity_metrics/Occupation/occupation_analysis.R):
# - race_conc_long: data.table with columns soc3_focus, prace_adj, diff_share, diff_moe, z_score
# - soc_stats: data.table/data.frame with columns soc3_code, Label, openings_total_24_34
#
# This file defines plot-builder functions.
# For convenience, it will source occupation_analysis.R only if the
# expected upstream objects are not already present in the environment.

# Pin here::here() to the repository root for direct or cross-directory use.
here::i_am("equity_metrics/Occupation/occupation_vizes.R")

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(ggplot2)
})

.required_soc_stats_cols <- c(
  "soc3_code",
  "Label",
  "openings_total_24_34",
  "WAGP_median",
  "poc_share_diff",
  "female_share_diff"
)

.required_race_conc_cols <- c("soc3_focus", "prace_adj", "diff_share", "z_score")

.required_soc3_equity_dot_cols <- c(
  "soc3_focus",
  "WAGP_median",
  "female_share_diff",
  "poc_share_diff"
)

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
  source(here::here("equity_metrics", "Occupation", "occupation_analysis.R"))
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
  # Keep the full SOC3 title, including comma-separated lists.
  x <- as.character(x)

  trimws(x)
}

#' Bubble chart: focus occupations by signed POC and female concentration
#'
#' This is the bubble plot previously at the end of occupation_analysis.R,
#' refactored into a plot-builder function.
#'
#' @param soc_stats Occupation stats table.
#'   Required cols: Label, openings_total_24_34, WAGP_median,
#'   poc_share_diff, female_share_diff.
#' @param openings_min Filter threshold for openings_total_24_34.
#' @param label_width Wrap width for occupation labels.
#' @param max_size Max bubble size.
#' @param label_n Label the groups with the most openings (Inf labels all).
#'
#' @return A ggplot object.
make_focus_bubble_plot <- function(
    soc_stats,
    openings_min = 1000,
    label_width = 25,
    max_size = 14,
    label_n = 15) {

  if (!.has_required_cols(soc_stats, .required_soc_stats_cols)) {
    analysis_env <- new.env(parent = parent.frame())
    source(
      here::here("equity_metrics", "Occupation", "occupation_analysis.R"),
      local = analysis_env
    )

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
    filter(openings_total_24_34 > openings_min) %>%
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

  label_df <- soc_plot_df[order(-soc_plot_df$openings_total_24_34), ]
  if (is.finite(label_n)) label_df <- head(label_df, label_n)

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
      size = openings_total_24_34,
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
    ggplot2::scale_fill_gradientn(
      colors = c("#ffffcc", "#a1dab4", "#41b6c4", "#2c7fb8", "#253494"),
      limits = c(10000, 160000),
      na.value = NA,
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
    ggplot2::scale_size_area(
      max_size = max_size,
      name = "Openings\n(2024-2034)",
      labels = prettyunits::pretty_num
    ) +
    ggplot2::guides(
      size = ggplot2::guide_legend(reverse = TRUE, order = 1),
      fill = ggplot2::guide_colorbar(order = 2)
    ) +
    ggplot2::labs(
      x = "POC share difference (occupation - workforce)",
      y = "Female share difference (occupation - workforce)",
      caption = if (nrow(label_df) < nrow(soc_plot_df)) {
        paste("Labels show the", nrow(label_df), "groups with the most projected openings.")
      } else NULL,
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

    # Repel labels together so they also avoid labels in other quadrants.
    p <- p + repel_layer(label_df, 0, 0)
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
      text_layer(filter(label_df, quadrant == "Q1"),  dx,  dy) +
      text_layer(filter(label_df, quadrant == "Q2"), -dx,  dy) +
      text_layer(filter(label_df, quadrant == "Q3"), -dx, -dy) +
      text_layer(filter(label_df, quadrant == "Q4"),  dx, -dy)
  }

  p
}

#' Heatmap of race/ethnicity concentration using Z-scores
#'
#' @param race_conc_long Long-form race concentration table.
#'   Required cols: soc3_focus, prace_adj, z_score.
#'   Optional: diff_share (used only for potential labels).
#' @param soc_stats Occupation stats table (for labels and openings filter).
#'   Required cols: soc3_code, Label, openings_total_24_34.
#' @param openings_min Filter threshold for openings_total_24_34.
#' @param z_limit Optional symmetric limit for the fill scale (e.g., 3).
#'   If NULL, uses the max absolute observed z (clipped to >= 1).
#'
#' @return A ggplot object.
make_race_conc_heatmap_z <- function(
    race_conc_long,
    soc_stats,
    openings_min = 5000,
    z_limit = NULL) {

  .check_cols(race_conc_long, c("soc3_focus", "prace_adj", "z_score"), "race_conc_long")
  .check_cols(soc_stats, c("soc3_code", "Label", "openings_total_24_34"), "soc_stats")

  rc <- as.data.table(copy(race_conc_long))
  ss <- as.data.table(copy(soc_stats))

  # Map occupation code -> label + openings
  ss_lab <- ss[, .(
    soc3_focus = as.character(soc3_code),
    Label = .clip_occ_label(Label),
    openings_total_24_34 = as.numeric(openings_total_24_34)
  )]

  plot_dt <- merge(rc, ss_lab, by = "soc3_focus", all.x = FALSE, all.y = FALSE)
  plot_dt <- plot_dt[is.finite(openings_total_24_34) & openings_total_24_34 > openings_min]

  # Order occupations by overall magnitude of concentration (sum |z| across race groups)
  ord <- plot_dt[, .(ord = sum(abs(z_score), na.rm = TRUE)), by = .(soc3_focus, Label)]
  ord <- ord[order(ord, decreasing = TRUE)]
  occ_levels <- ord$Label

  plot_dt[, Label := factor(Label, levels = occ_levels)]
  plot_dt[, prace_adj := factor(as.character(prace_adj))]

  if (is.null(z_limit)) {
    z_limit <- max(1, suppressWarnings(max(abs(plot_dt$z_score), na.rm = TRUE)))
  }

  ggplot(plot_dt, aes(x = prace_adj, y = Label, fill = z_score)) +
    geom_tile(color = NA) +
    scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 45)) +
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
      subtitle = paste0("Filtered to occupations with projected openings > ", openings_min, " by 2034")
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
#'   Required cols: soc3_focus, prace_adj, diff_share.
#'   Optional: diff_moe (for 90% MOE error bars).
#' @param soc_stats Occupation stats table (for labels and openings filter).
#'   Required cols: soc3_code, Label, openings_total_24_34.
#' @param openings_min Filter threshold for openings_total_24_34.
#'
#' @return A ggplot object.
make_race_conc_cleveland <- function(
    race_conc_long,
    soc_stats,
    openings_min = 5000) {

  .check_cols(race_conc_long, c("soc3_focus", "prace_adj", "diff_share"), "race_conc_long")
  .check_cols(soc_stats, c("soc3_code", "Label", "openings_total_24_34"), "soc_stats")

  rc <- as.data.table(copy(race_conc_long))
  ss <- as.data.table(copy(soc_stats))

  ss_lab <- ss[, .(
    soc3_focus = as.character(soc3_code),
    Label = .clip_occ_label(Label),
    openings_total_24_34 = as.numeric(openings_total_24_34)
  )]

  plot_dt <- merge(rc, ss_lab, by = "soc3_focus", all.x = FALSE, all.y = FALSE)
  plot_dt <- plot_dt[is.finite(openings_total_24_34) & openings_total_24_34 > openings_min]

  # Order occupations by overall magnitude of difference (sum |diff| across groups)
  ord <- plot_dt[, .(ord = sum(abs(diff_share), na.rm = TRUE)), by = .(soc3_focus, Label)]
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
      subtitle = paste0("Filtered to occupations with projected openings > ", openings_min, " by 2034")
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold"),
      legend.position = "none"
    )

  p
}

#' Paired dot display: female and POC share differences by SOC3 group
#'
#' @param soc3_stats Occupation-group table with soc3_focus,
#'   WAGP_median, female_share_diff, and poc_share_diff columns.
#'   Wages are the existing annual SOC3 medians; rows are ordered by descending median wage.
#' @param point_size Dot size.
#'
#' @return A printable gtable with two equity columns and an aligned wage column.
#'   Supports ggsave() and automatic printing in Quarto/knitr.
make_equity_dot_plot_soc3 <- function(soc3_stats, point_size = 3) {
  .check_cols(soc3_stats, .required_soc3_equity_dot_cols, "soc3_stats")

  plot_df <- copy(as.data.table(soc3_stats))[, .(
    soc3_focus = as.character(soc3_focus),
    WAGP_median = as.numeric(WAGP_median),
    `Female share difference` = as.numeric(female_share_diff),
    `POC share difference` = as.numeric(poc_share_diff)
  )]

  plot_df <- plot_df[order(-WAGP_median, na.last = TRUE)]
  soc3_levels <- rev(unique(plot_df$soc3_focus))
  wage_df <- copy(plot_df)
  wage_df[, soc3_focus := factor(soc3_focus, levels = soc3_levels)]
  wage_df[, dimension := "Median annual wage"]
  wage_df[!is.finite(WAGP_median) | WAGP_median < 0, WAGP_median := NA_real_]
  wage_max <- max(c(0, wage_df$WAGP_median), na.rm = TRUE)
  if (wage_max == 0) wage_max <- 1
  wage_df[, wage_label := ifelse(
    is.na(WAGP_median), "N/A",
    scales::dollar(WAGP_median, accuracy = 1)
  )]
  axis_labels <- if ("Label" %in% names(soc3_stats)) {
    stats::setNames(stringr::str_wrap(as.character(soc3_stats$Label), width = 45),
                    as.character(soc3_stats$soc3_focus))
  } else {
    stats::setNames(soc3_levels, soc3_levels)
  }
  plot_df <- data.table::melt(
    plot_df,
    id.vars = c("soc3_focus", "WAGP_median"),
    variable.name = "dimension",
    value.name = "share_difference",
    variable.factor = TRUE
  )
  plot_df[, soc3_focus := factor(soc3_focus, levels = soc3_levels)]

  plot_limit <- suppressWarnings(max(abs(plot_df$share_difference), na.rm = TRUE))
  if (!is.finite(plot_limit) || plot_limit == 0) {
    plot_limit <- 0.01
  }
  plot_limit <- plot_limit * 1.08

  equity_plot <- ggplot2::ggplot(plot_df, ggplot2::aes(x = share_difference, y = soc3_focus)) +
    ggplot2::geom_hline(
      yintercept = seq_len(length(soc3_levels)),
      color = "grey92",
      linewidth = 0.3
    ) +
    ggplot2::geom_vline(xintercept = 0, color = "grey55", linewidth = 0.5) +
    ggplot2::geom_point(color = "#2166AC", size = point_size, na.rm = TRUE) +
    ggplot2::facet_grid(cols = ggplot2::vars(dimension)) +
    ggplot2::scale_y_discrete(labels = axis_labels, limits = soc3_levels, drop = FALSE) +
    ggplot2::scale_x_continuous(
      limits = c(-plot_limit, plot_limit),
      labels = scales::label_percent(accuracy = 1),
      expand = ggplot2::expansion(mult = 0.03)
    ) +
    ggplot2::labs(
      x = "Share difference (occupation group - workforce)",
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

  wage_plot <- ggplot2::ggplot(wage_df, ggplot2::aes(y = soc3_focus)) +
    ggplot2::geom_col(
      ggplot2::aes(x = WAGP_median, fill = WAGP_median),
      width = 0.32, orientation = "y", na.rm = TRUE
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = ifelse(is.na(WAGP_median), 0, WAGP_median),
                   label = wage_label),
      hjust = 0, nudge_x = wage_max * 0.04, size = 3, color = "grey25"
    ) +
    ggplot2::facet_grid(cols = ggplot2::vars(dimension)) +
    ggplot2::scale_y_discrete(limits = soc3_levels, drop = FALSE) +
    ggplot2::scale_x_continuous(
      limits = c(0, wage_max * 1.05), expand = ggplot2::expansion(mult = 0)
    ) +
    ggplot2::scale_fill_gradientn(
      colors = c("#ffffcc", "#a1dab4", "#41b6c4", "#2c7fb8", "#253494"),
      limits = c(10000, 160000),
      na.value = NA,
      guide = "none"
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold"),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank()
    )

  # Insert into the existing panel/strip rows: axes, subtitle and long occupation
  # labels cannot change the relative vertical alignment of the three columns.
  result <- ggplot2::ggplotGrob(equity_plot)
  wage_grob <- ggplot2::ggplotGrob(wage_plot)
  panel <- result$layout[grepl("^panel", result$layout$name), ][1, ]
  strip <- result$layout[grepl("^strip-t", result$layout$name), ][1, ]
  insert_at <- max(result$layout$r[grepl("^panel", result$layout$name)])
  # Reserve physical space for the longest formatted label, independent of the
  # wage range. The bar region is narrower than either equity panel.
  label_width <- max(grid::stringWidth(wage_df$wage_label))
  result <- gtable::gtable_add_cols(
    result,
    grid::unit.c(grid::unit(1.25, "lines"), grid::unit(0.55, "null"),
                 label_width + grid::unit(3, "mm")),
    pos = insert_at
  )
  result <- gtable::gtable_add_grob(
    result, wage_grob$grobs[[which(grepl("^panel", wage_grob$layout$name))]],
    t = panel$t, b = panel$b, l = insert_at + 2, clip = "off", name = "wage-panel"
  )
  result <- gtable::gtable_add_grob(
    result, wage_grob$grobs[[which(grepl("^strip-t", wage_grob$layout$name))]],
    t = strip$t, b = strip$b, l = insert_at + 2, r = insert_at + 3,
    clip = "off", name = "wage-heading"
  )
  class(result) <- c("soc3_equity_plot", class(result))
  result
}

print.soc3_equity_plot <- function(x, ...) {
  grid::grid.newpage()
  grid::grid.draw(x)
  invisible(x)
}
