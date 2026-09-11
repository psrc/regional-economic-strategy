library(ggplot2)

# Plot rates by one or two grouping columns.
#
# @param dt A data.frame or data.table with the group column(s) and a rate column.
# @param group_vars Character vector of one or two column names. The first is
#   the outer/color group (e.g., race). If a second is given, it is nested
#   within the first: "SEX" is dodged side-by-side and labeled above the
#   outer group label; any other inner variable is stacked instead, with a
#   swatch key below the outer group label (its values are often too long
#   to fit on the x-axis).
# @param rate_var Unquoted or quoted rate column name (e.g., share).
# @param whiskers Logical; if TRUE, adds MOE whiskers using <rate_var>_moe.
# @param xlabi List with optional parameters to control how the x-labels
#   of the inner grouping are drawn. Currently, items 'size', 'angle', 'hjust', 
#   'vjust' are used.
#
# @return A ggplot object.
plot_rates <- function(dt, group_vars = c("PRACE", "SEX"), rate_var = "share", 
                       whiskers = FALSE, xlabi = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plot_rate_by_race_and_sex().", call. = FALSE)
  }

  if (!is.character(group_vars) || !length(group_vars) %in% c(1, 2)) {
    stop("group_vars must be a character vector of length 1 or 2.", call. = FALSE)
  }
  outer_col <- group_vars[1]
  inner_col <- if (length(group_vars) == 2) group_vars[2] else NA_character_
  has_inner <- !is.na(inner_col)
  is_sex_inner <- has_inner && toupper(inner_col) == "SEX"
  # non-sex inner groups often have labels too long for the x-axis, so stack
  # the bars instead of dodging and describe the shading with a key below
  stack_inner <- has_inner && !is_sex_inner

  shade_hex <- function(hex, toward = c("white", "black"), amount = 0.25) {
    toward <- match.arg(toward)
    rgb_mat <- grDevices::col2rgb(hex) / 255
    target <- if (toward == "white") 1 else 0
    shaded <- rgb_mat + (target - rgb_mat) * amount
    grDevices::rgb(shaded[1], shaded[2], shaded[3])
  }

  tint_hex <- function(hex, amount) {
    if (amount >= 0) shade_hex(hex, toward = "white", amount = amount)
    else shade_hex(hex, toward = "black", amount = -amount)
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
    if (is_sex_inner) {
      sex_levels <- c("Female","Male")
      sex_levels <- c(sex_levels[sex_levels %chin% as.character(dt_plot[[inner_col]])],
                      setdiff(unique(as.character(dt_plot[[inner_col]])), sex_levels))
    } else {
      sex_levels <- unique(as.character(dt_plot[[inner_col]]))
    }
    dt_plot[, sex_f := factor(as.character(get(inner_col)), levels = sex_levels)]
  } else {
    sex_levels <- race_levels
    dt_plot[, sex_f := race_f]
  }
  n_sex <- if (has_inner) length(sex_levels) else 1

  race_palette <- stats::setNames(
    grDevices::hcl.colors(length(race_levels), palette = "Dynamic"),
    race_levels
  )

  # light-to-dark tint amount for each ordered inner level, shared by the
  # actual bars (tinted per race) and the neutral legend swatches
  level_amount <- if (has_inner) stats::setNames(seq(0.35, -0.30, length.out = n_sex), sex_levels) else NULL

  if (has_inner) {
    if (is_sex_inner) {
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
      dt_plot[, fill_hex := vapply(seq_len(.N), function(i) {
        race_hex <- race_palette[as.character(race_f[i])]
        tint_hex(race_hex, level_amount[[as.character(sex_f[i])]])
      }, character(1))]
    }
  } else {
    dt_plot[, fill_hex := race_palette[as.character(race_f)]]
  }

  # only the sex inner grouping dodges side-by-side; stacked inner groups and
  # the single-bar case all share one x position per race
  n_dodge <- if (is_sex_inner) n_sex else 1
  offsets <- if (n_dodge > 1) {
    seq(-(n_dodge - 1) / 2, (n_dodge - 1) / 2, length.out = n_dodge) * min(1/n_dodge, 0.34)
  } else {
    0
  }
  dt_plot[, x_pos := if (n_dodge > 1) as.numeric(race_f) + offsets[as.integer(sex_f)] else as.numeric(race_f)]

  race_centers <- dt_plot[, .(x_center = mean(x_pos)), by = race_f]
  race_label_width <- if (length(race_levels) >= 7) 12 else 16
  race_centers[, race_label := vapply(
    as.character(race_f),
    function(x) paste(strwrap(x, width = race_label_width), collapse = "\n"),
    character(1)
  )]

  sex_labels <- if (is_sex_inner) {
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

  # stacked bars can reach much higher than any single rate, so size labels
  # off the summed bar height rather than the per-row rate
  if (stack_inner) {
    stack_totals <- dt_plot[, .(total = sum(get(rate_col), na.rm = TRUE)), by = x_pos]
    y_max <- max(y_max, suppressWarnings(max(stack_totals$total, na.rm = TRUE)), na.rm = TRUE)
  }

  if (isTRUE(whiskers)) {
    dt_plot[, y_low := pmax(get(rate_col) - get(moe_col), 0)]
    dt_plot[, y_high := pmin(get(rate_col) + get(moe_col), 1)]
    y_min <- min(y_min, min(dt_plot$y_low, na.rm = TRUE))
    y_max <- max(y_max, max(dt_plot$y_high, na.rm = TRUE))
  }

  # race label sits at the floor; sex label is a fraction of the way toward
  # the x-axis so it always stays below zero regardless of y_max
  label_boundary_y <- -max(0.008, 0.12 * (y_max + 1e-9))
  race_label_y <- label_boundary_y
  sex_label_y <- label_boundary_y * 0.4
  bar_width <- min(if (n_dodge <= 2) 0.26 else 0.22, 1/n_dodge)

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
    (if (is_sex_inner) {
      ggplot2::geom_text(
        data = sex_labels,
        ggplot2::aes(x = x_pos, y = sex_label_y, label = sex_label),
        inherit.aes = FALSE,
        vjust = if(!is.null(xlabi$vjust)) xlabi$vjust else 0,
        size = if(!is.null(xlabi$size)) xlabi$size else 3.2,
        fontface = "bold", 
        angle = if(!is.null(xlabi$angle)) xlabi$angle else 0,
        hjust = if(!is.null(xlabi$hjust)) xlabi$hjust else 0.5
      )
    }) +
    ggplot2::geom_text(
      data = race_centers,
      ggplot2::aes(x = x_center, y = race_label_y, label = race_label),
      inherit.aes = FALSE,
      vjust = 1,
      size = 3.6,
      lineheight = 1
    ) +
    (if (stack_inner) {
      # invisible layer purely to populate a legend key for the tint shading;
      # the real bars use scale_fill_identity, which can't drive its own legend.
      # A standard ggplot legend (vs. in-panel text) lives outside the data
      # coordinate system, so it isn't squeezed by the fixed aspect ratio.
      ggplot2::geom_point(
        data = data.frame(level = factor(sex_levels, levels = sex_levels)),
        ggplot2::aes(x = mean(race_centers$x_center), y = 0, shape = level),
        inherit.aes = FALSE, alpha = 0, size = 0, show.legend = TRUE
      )
    }) +
    (if (stack_inner) {
      ggplot2::scale_shape_manual(
        name = NULL,
        values = stats::setNames(rep(22, n_sex), sex_levels),
        guide = ggplot2::guide_legend(
          ncol = 1,
          override.aes = list(
            alpha = 1, size = 5,
            fill = vapply(sex_levels, function(l) tint_hex("#808080", level_amount[[l]]), character(1)),
            color = "grey40"
          )
        )
      )
    }) +
    ggplot2::labs(x = NULL, y = NULL, fill = NULL) +
    ggplot2::guides(fill = "none") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(8, 8, 12, 8),
      legend.position = if (stack_inner) "bottom" else "none",
      legend.title = ggplot2::element_blank(),
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


plot_line_rates <- function(dt, xcol = "year", ycol = "share", fill = NULL, legend = "share", ...) {
    
    dt <- copy(dt)
    dt[, category := legend]
    plot_data <- as_tibble(dt)  
    
    pop_chart <- static_line_chart(
        t     = plot_data,
        x     =  xcol,       # category (x) axis
        y     =  ycol,       # numeric value to plot
        fill  =  if(is.null(fill)) "category" else fill,
        ... )
    return(pop_chart)
}


plot_stackbar_rates <- function(dt, xcol = "year", ycol = "share", fill = NULL, ...) {
    
    dt[, (xcol) := as.character(get(xcol))] # category axis needs to be character/factor
    
    plot_data <- as_tibble(dt)  
    
    pop_chart <- static_column_chart(
        t     = plot_data,
        x     =  xcol,       # category (x) axis
        y     =  ycol,       # numeric value to plot
        fill  =  fill,
        pos = "stack",
        ... )
    return(pop_chart)
}
