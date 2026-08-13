library(psrccensus)
library(magrittr)
library(data.table)


# Retrieve ACS data for PSRC region, combining separate tables for each race/ethnicity.
# Resticted to working age (16-64)
get_regional_acs_labor_force_data <- function(year, acs.type = "acs5"){
  dt_lf <- suppressMessages(
    get_acs_recs("county", 
                 table.names=paste0("C23002", toupper(letters[1:9])),
                 years = year, acs.type = acs.type)) %>% setDT() %>%
    .[GEOID == "REGION" & grepl("16 to 64 years", label)==TRUE] %>%
    .[, `:=`(race  = sub(" Alone", "", 
                         sub(".*\\((.*?)\\).*", "\\1", concept)),
             label = gsub(":", "",
                         sub("Civilian:!!", "",
                             sub("^Estimate!!Total:(!!)?", "", label))))] %>%
    .[, c("sex", "age", "labor_force_status", "emp_status") := tstrsplit(label, "!!", fixed = TRUE, fill= "Total")]
  return(dt_lf)
}

# From the labor force data.table, pull a smaller table with labor force participation rate
prep_labor_force_data <- function(dt_lf){
  dt_lf_2cats <- copy(dt_lf)[
    labor_force_status %in% c("Total", "Not in labor force") &
      emp_status == "Total",
    .(year, race, sex, labor_force_status, estimate, moe)
  ]

  dt_lf_prepped <- dt_lf_2cats[, .(
    working_age_total = sum(estimate[labor_force_status == "Total"], na.rm = TRUE),
    working_age_total_moe = tidycensus::moe_sum(
      moe[labor_force_status == "Total"],
      estimate[labor_force_status == "Total"],
      na.rm = TRUE
    ),
    not_in_labor_force = sum(estimate[labor_force_status == "Not in labor force"], na.rm = TRUE),
    not_in_labor_force_moe = tidycensus::moe_sum(
      moe[labor_force_status == "Not in labor force"],
      estimate[labor_force_status == "Not in labor force"],
      na.rm = TRUE
    )
  ), by = .(year, race, sex)]

  dt_lf_prepped[, `:=`(
    participation_rate = fifelse(
      working_age_total > 0,
      1 - (not_in_labor_force / working_age_total),
      NA_real_
    ),
    participation_rate_moe = mapply(
    function(est_not, est_total, moe_not, moe_total) {
      if (is.na(est_total) || est_total <= 0) {
        return(NA_real_)
      }
      # MOE of 1 - p is the same as MOE of p; compute p = not_in / total.
      tidycensus::moe_prop(est_not, est_total, moe_not, moe_total)
    },
    not_in_labor_force,
    working_age_total,
    not_in_labor_force_moe,
    working_age_total_moe
  ))]

  return(dt_lf_prepped[])
}

# From the labor force data.table, pull a smaller table with unemployment rate
prep_unemployment_data <- function(dt_lf){
  dt_unemp_2cats <- copy(dt_lf)[
    emp_status %in% c("Total", "Unemployed") &
      labor_force_status == "In labor force",
    .(year, race, sex, emp_status, estimate, moe)
  ]

  dt_unemp_prepped <- dt_unemp_2cats[, .(
    labor_force_total = sum(estimate[emp_status == "Total"], na.rm = TRUE),
    labor_force_total_moe = tidycensus::moe_sum(
      moe[emp_status == "Total"],
      estimate[emp_status == "Total"],
      na.rm = TRUE
    ),
    unemployed = sum(estimate[emp_status == "Unemployed"], na.rm = TRUE),
    unemployed_moe = tidycensus::moe_sum(
      moe[emp_status == "Unemployed"],
      estimate[emp_status == "Unemployed"],
      na.rm = TRUE
    )
  ), by = .(year, race, sex)]

  dt_unemp_prepped[, `:=`(
    unemployment_rate = fifelse(
      labor_force_total > 0,
      unemployed / labor_force_total,
      NA_real_
    ),
    unemployment_rate_moe = mapply(
    function(est_unemp, est_total, moe_unemp, moe_total) {
      if (is.na(est_total) || est_total <= 0) {
        return(NA_real_)
      }
      tidycensus::moe_prop(est_unemp, est_total, moe_unemp, moe_total)
    },
    unemployed,
    labor_force_total,
    unemployed_moe,
    labor_force_total_moe
  ))]

  return(dt_unemp_prepped[])
}

# Plot labor force or unemployment rates by sex within race categories.
#
# @param dt A data.frame or data.table with at least race, sex, and rate columns.
# @param rate_var Unquoted or quoted rate column name (e.g., participation_rate).
# @param whiskers Logical; if TRUE, adds MOE whiskers using <rate_var>_moe.
#
# @return A ggplot object.
plot_lf_rate <- function(dt, rate_var, whiskers = FALSE) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plot_lf_rate().", call. = FALSE)
  }

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

  required_cols <- c("race", "sex", rate_col)
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

  dt_plot <- as.data.table(copy(dt))[!is.na(race) & !is.na(sex)]
  dt_plot <- dt_plot[!is.na(get(rate_col))]

  race_levels <- unique(as.character(dt_plot$race))
  sex_levels <- c("Female","Male")
  sex_levels <- c(sex_levels[sex_levels %chin% as.character(dt_plot$sex)],
                  setdiff(unique(as.character(dt_plot$sex)), sex_levels))

  dt_plot[, race_f := factor(as.character(race), levels = race_levels)]
  dt_plot[, sex_f := factor(as.character(sex), levels = sex_levels)]

  race_palette <- stats::setNames(
    grDevices::hcl.colors(length(race_levels), palette = "Dynamic"),
    race_levels
  )

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

  sex_labels <- unique(
    dt_plot[, .(x_pos, sex_label = as.character(sex_f), race_f)]
  )

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
    ggplot2::geom_text(
      data = sex_labels,
      ggplot2::aes(x = x_pos, y = sex_label_y, label = sex_label),
      inherit.aes = FALSE,
      vjust = 0,
      size = 3.2,
      fontface = "bold"
    ) +
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
    ggplot2::coord_cartesian(ylim = c(race_label_y - 0.015, NA), clip = "off") #+ 
    #psrcplot::psrc_style()

  if (isTRUE(whiskers)) {
    p <- p + ggplot2::geom_errorbar(
      ggplot2::aes(ymin = y_low, ymax = y_high),
      width = 0.08,
      linewidth = 0.35
    )
  }

  return(p)
}

## Example -------------------------
dt_lf_2024 <- get_regional_acs_labor_force_data(2024)
dt_lf_prepped <- prep_labor_force_data(dt_lf_2024)
dt_unemp_prepped <- prep_unemployment_data(dt_lf_2024)
unemp_p <- plot_lf_rate(dt_unemp_prepped, unemployment_rate, whiskers = TRUE)
lf_p <- plot_lf_rate(dt_lf_prepped, participation_rate, whiskers = TRUE) 