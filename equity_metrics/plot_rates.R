library(ggplot2)

# Plot rates by one or two grouping columns.
#
# @param dt A data.frame or data.table with the group column(s) and a rate column.
# @param group_vars Character vector of one or two column names. The first is
#   the outer/color group (e.g., race). If a second is given, it is nested
#   within the first (e.g., sex), labeled above the outer group label, and
#   shaded as tones of the outer group's color.
# @param rate_var Unquoted or quoted rate column name (e.g., share).
# @param whiskers Logical; if TRUE, adds MOE whiskers using <rate_var>_moe.
#
# @return A ggplot object.
plot_rates <- function(dt, group_vars = c("PRACE", "SEX"), rate_var = "share", whiskers = FALSE) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plot_rate_by_race_and_sex().", call. = FALSE)
  }

  if (!is.character(group_vars) || !length(group_vars) %in% c(1, 2)) {
    stop("group_vars must be a character vector of length 1 or 2.", call. = FALSE)
  }
  outer_col <- group_vars[1]
  inner_col <- if (length(group_vars) == 2) group_vars[2] else NA_character_
  has_inner <- !is.na(inner_col)

  shade_hex <- function(hex, toward = c("white", "black"), amount = 0.25) {
    toward <- match.arg(toward)
    rgb_mat <- grDevices::col2rgb(hex) / 255
    target <- if (toward == "white") 1 else 0
    shaded <- rgb_mat + (target - rgb_mat) * amount
    grDevices::rgb(shaded[1], shaded[2], shaded[3])
  }

  rate_sym <- rlang::ensym(rate_var)
  rate_col <- rlang::as_string(rate_sym)

  if (!is.data.frame(dt)) {
    stop("dt must be a data.frame or data.table.", call. = FALSE)
  }
  dt <- as.data.table(dt)

  required_cols <- c(group_vars, rate_col)
  missing_cols <- setdiff(required_cols, names(dt))
  if (length(missing_cols) > 0) {
    stop(
      sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")),
      call. = FALSE
    )
  }

  moe_col <- paste0(rate_col, "_moe")
  if (isTRUE(whiskers) && !moe_col %in% names(dt)) {
    stop(
      sprintf("whiskers=TRUE requires column '%s'.", moe_col),
      call. = FALSE
    )
  }

  dt_plot <- copy(dt)[!is.na(get(outer_col))]
  if (has_inner) dt_plot <- dt_plot[!is.na(get(inner_col))] # avoid get(NA) when no inner group
  dt_plot <- dt_plot[!is.na(get(rate_col))]

  race_levels <- unique(as.character(dt_plot[[outer_col]]))
  dt_plot[, race_f := factor(as.character(get(outer_col)), levels = race_levels)]

  if (has_inner) {
    sex_levels <- c("Female","Male")
    sex_levels <- c(sex_levels[sex_levels %chin% as.character(dt_plot[[inner_col]])],
                    setdiff(unique(as.character(dt_plot[[inner_col]])), sex_levels))
    dt_plot[, sex_f := factor(as.character(get(inner_col)), levels = sex_levels)]
  } else {
    sex_levels <- race_levels
    dt_plot[, sex_f := race_f]
  }

  race_palette <- stats::setNames(
    grDevices::hcl.colors(length(race_levels), palette = "Dynamic"),
    race_levels
  )

  if (has_inner) {
    dt_plot[, fill_hex := vapply(seq_len(.N), function(i) {
      race_hex <- race_palette[as.character(race_f[i])]
      sex <- as.character(sex_f[i])

      if (sex == "Male") {
        shade_hex(race_hex, toward = "white", amount = 0.35)
      } else if (sex == "Female") {
        shade_hex(race_hex, toward = "black", amount = 0.18)
      } else {
        race_hex
      }
    }, character(1))]
  } else {
    dt_plot[, fill_hex := race_palette[as.character(race_f)]]
  }

  n_sex <- length(sex_levels)
  offsets <- seq(-(n_sex - 1) / 2, (n_sex - 1) / 2, length.out = n_sex) * 0.34
  dt_plot[, x_pos := as.numeric(race_f) + offsets[as.integer(sex_f)]]

  race_centers <- dt_plot[, .(x_center = mean(x_pos)), by = race_f]
  race_label_width <- if (length(race_levels) >= 7) 12 else 16
  race_centers[, race_label := vapply(
    as.character(race_f),
    function(x) paste(strwrap(x, width = race_label_width), collapse = "\n"),
    character(1)
  )]

  sex_labels <- if (has_inner) {
    unique(
      dt_plot[, .(x_pos, sex_label = as.character(sex_f), race_f)]
    )
  } else {
    NULL
  }

  y_min <- suppressWarnings(min(dt_plot[[rate_col]], na.rm = TRUE))
  y_max <- suppressWarnings(max(dt_plot[[rate_col]], na.rm = TRUE))

  if (!is.finite(y_min)) y_min <- 0
  if (!is.finite(y_max)) y_max <- 0

  if (isTRUE(whiskers)) {
    dt_plot[, y_low := pmax(get(rate_col) - get(moe_col), 0)]
    dt_plot[, y_high := pmin(get(rate_col) + get(moe_col), 1)]
    y_min <- min(y_min, min(dt_plot$y_low, na.rm = TRUE))
    y_max <- max(y_max, max(dt_plot$y_high, na.rm = TRUE))
  }

  label_boundary_y <- -max(0.004, 0.08 * (y_max + 1e-9))
  sex_label_y <- label_boundary_y + max(0.002, 0.002 * (y_max + 1e-9))
  race_label_y <- label_boundary_y
  bar_width <- if (n_sex <= 2) 0.26 else 0.22

  p <- ggplot2::ggplot(
    dt_plot,
    ggplot2::aes(x = x_pos, y = .data[[rate_col]], fill = fill_hex)
  ) +
    ggplot2::geom_col(width = bar_width, color = "white", linewidth = 0.25) +
    ggplot2::scale_fill_identity(guide = "none") +
    ggplot2::scale_x_continuous(
      breaks = unique(dt_plot$x_pos),
      labels = NULL,
      expand = ggplot2::expansion(mult = c(0.02, 0.02))
    ) +
    ggplot2::scale_y_continuous(
      breaks = function(x) {
        upper <- suppressWarnings(max(x, na.rm = TRUE))
        if (!is.finite(upper)) upper <- 0
        b <- pretty(c(0, upper))
        b[b >= 0]
      },
      labels = scales::label_percent(accuracy = 0.1),
      expand = ggplot2::expansion(mult = c(0.14, 0.06))
    ) +
    (if (has_inner) {
      ggplot2::geom_text(
        data = sex_labels,
        ggplot2::aes(x = x_pos, y = sex_label_y, label = sex_label),
        inherit.aes = FALSE,
        vjust = 0,
        size = 3.2,
        fontface = "bold"
      )
    }) +
    ggplot2::geom_text(
      data = race_centers,
      ggplot2::aes(x = x_center, y = race_label_y, label = race_label),
      inherit.aes = FALSE,
      vjust = 1,
      size = 3.6,
      lineheight = 0.9
    ) +
    ggplot2::labs(x = NULL, y = NULL, fill = NULL) +
    ggplot2::guides(fill = "none") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(8, 8, 12, 8),
      legend.position = "none",
      aspect.ratio = 1 / 2
    ) +
    ggplot2::coord_cartesian(ylim = c(race_label_y - 0.015, NA), clip = "off")

  if (isTRUE(whiskers)) {
    p <- p + ggplot2::geom_errorbar(
      ggplot2::aes(ymin = y_low, ymax = y_high),
      width = 0.08,
      linewidth = 0.35
    )
  }

  return(p)
}