library(psrccensus)
library(magrittr)
library(data.table)


# Retrieve ACS internet access data for PSRC region, combining separate tables for each race/ethnicity.
get_regional_acs_internet_data <- function(year, acs.type = "acs5"){
  dt_net <- suppressMessages(
    get_acs_recs("county", 
                 table.names=paste0("B28009", toupper(letters[1:9])),
                 years = year, acs.type = acs.type)) %>% setDT() %>% 
    .[GEOID == "REGION"] %>%
    .[, `:=`(race  = sub(" Alone", "", 
                         sub(".*\\((.*?)\\).*", "\\1", concept)),
             label = gsub(":", "",
                          sub("^Estimate!!Total:(!!)?", "", label)))] %>% 
    .[, c("computer_ownership", "internet_status") := tstrsplit(label, "!!", fixed = TRUE, fill= "Total")] %>%
    .[, internet_status:= fcase(internet_status=="Total", "Total",
                                grepl("broadband", internet_status)==TRUE, "broadband",
                                grepl("dial-up", internet_status)==TRUE, "dial-up",
                                grepl("^Without", internet_status)==TRUE, "no internet")]

  return(dt_net)
}

# From the internet data.table, pull a smaller table with shares added
prep_internet_data <- function(dt_net){
  dt_net_slim <- copy(dt_net)[
    computer_ownership == "Has a computer",
    .(year, race, internet_status, estimate, moe)
  ]
  
  dt_net_totals <- copy(dt_net_slim)[internet_status == "Total"] %>%
    .[, internet_status:=NULL] %>%
    setnames(c("estimate", "moe"), c("est_total","moe_total"))
  
  dt_net_prepped <- dt_net_slim[internet_status!="Total"][dt_net_totals, on=.(year, race)] %>%
    .[, `:=`(race = factor(race, 
                           levels = c("American Indian and Alaska Native",
                                      "Asian",
                                      "Black or African American",
                                      "Native Hawaiian and Other Pacific Islander",
                                      "White",
                                      "Some Other Race",
                                      "Two or More Races",
                                      "Hispanic or Latino",
                                      "White, Not Hispanic or Latino")),
             internet_status = factor(internet_status,
                                      levels = c("no internet", "dial-up", "broadband"))
             )]
  
  dt_net_prepped[, `:=`(internet_status_share = fifelse(is.na(est_total) | est_total <= 0, NA_real_, estimate / est_total),
             internet_status_share_moe = fifelse(is.na(est_total) | est_total <= 0, NA_real_,
                tidycensus::moe_prop(estimate, est_total, moe, moe_total)))] %>%
    .[, c("est_total", "moe_total"):= NULL]

  return(dt_net_prepped[])
}

# Stacked horizontal bar chart (ggplot2 defaults)
stacked_horizontal_bar_chart <- function(data, y, x, fill, title = NULL) {
  ggplot2::ggplot(data, ggplot2::aes(x = .data[[y]], y = .data[[x]], fill = .data[[fill]])) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(title = title, x = NULL, y = NULL, fill = NULL)
}

# Example:
# dt_net <- get_regional_acs_internet_data(2024)
# dt_net_prepped <- prep_internet_data(dt_net)
#
# net_access_p <- stacked_horizontal_bar_chart(
#   data = dt_net_prepped[internet_status != "dial-up"],
#   y = "race",
#   x = "internet_status_share",
#   fill = "internet_status",
#   title = "Internet Access by Race/Ethnicity"
# )

# net_access_p <- psrcplot::static_bar_chart(
#                     t = dt_net_prepped[internet_status!="dial-up"], 
#                     y = "race", 
#                     x = "internet_status_share", 
#                     fill = "internet_status",
#                     title = "Internet Access by Race/Ethnicity",
#                     alt = "Chart of Internet Access by Race/Ethnicity",
#                     source = paste("Source: ACS 5-Year Estimates, tables B28009A-I",
#                                  "for King, Kitsap, Pierce and Snohomish counties.",
#                                  sep = "\n"),
#                     color="pgnobgy_5", pos = "stack")
