library(psrccensus)

# Data Retrieval ----------------------
# Shared data retrieval step is more efficient than calling get_psrc_pums() for each indicator.

# pums_rds on local or shared drive for faster/more reliable access; otherwise use pums_rds = NULL
jrds = "J:/Projects/Census/AmericanCommunitySurvey/Data/PUMS/pums_rds"

pvars <- c("AGEP",
           "ESR",                 # Employment status recode
           "ED_ATTAIN",           # Educational attainment
           "HICOV",               # Health insurance coverage
           "NAICSP",              # Industry code
           "POVPIP",              # Income as a percentage of the poverty level
           "PRACE",               # PSRC non-overlapping race/ethnicity variable
           "SEX",
           "SOCP3",               # Occupation code
           "WAGP"                 # Wage or salary income past 12 months      
)

hvars <- c("ACCESSINET",          # Internet access
           "GRPIP",               # Household income as a percentage of the poverty level
           "GRNTP",               # Gross rent as a percentage of household income
           "HINCP",               # Household income past 12 months
           "HRACE",               # PSRC non-overlapping race/ethnicity variable
           "OWN_RENT"             # Tenure (own/rent)
)

get_pums_res_equity_p <- function(dyear, span = 5, pums_rds = jrds){
    pumsdata <- get_psrc_pums(span = span, dyear = dyear, level = "p", vars = pvars, dir = pums_rds)
    return(pumsdata)
}

get_pums_res_equity_h <- function(dyear, span = 5, pums_rds = jrds){
    if(dyear < 2017){
    message("Internet access data is not available for years prior to 2017.")
    }
    # Variable changed names w/ 2020 data; swap name temporarily for older data
    if(dyear %in% 2017:2019){hvars <- replace(hvars, hvars=="ACCESSINET","ACCESS")}
    pumsdata <- get_psrc_pums(span = span, dyear = dyear, level = "h", vars = hvars, dir = pums_rds)
    if("ACCESS" %in% colnames(pumsdata)){pumsdata %<>% rename("ACCESSINET"="ACCESS")}
    return(pumsdata)
}

# Add household variables ----------------

# Add internet access variable 
# - required variables: ACCESSINET
prep_internet_data <- function(raw_pumsdata_h){
  prepped_pumsdata_h <- raw_pumsdata_h %>% mutate(
    internet = factor(case_when(grepl("^Yes", ACCESSINET) ~ "With internet access",
                                grepl("^No", ACCESSINET) ~ "Without internet access")))
  return(prepped_pumsdata_h)
}

# Add rent burden variable 
# - required variables: GRPIP, GRNTP, OWN_RENT
prep_renter_burden_data <- function(raw_pumsdata_h){
  prepped_pumsdata_h <- raw_pumsdata_h %>% mutate(
    rent_burden=factor(case_when(
      GRPIP<30|(is.na(GRNTP) & OWN_RENT=="Rented") ~ "Less than 30 percent",
      dplyr::between(GRPIP,30,50) ~ "Between 30 and 50 percent",
      GRPIP>50|is.na(HINCP)       ~ "Greater than 50 percent"),
      levels=c("Greater than 50 percent",
               "Between 30 and 50 percent",
               "Less than 30 percent")))
  return(prepped_pumsdata_h)
}

# Add personal variables --------------

# Add personal health insurance variable 
# - required input variables: HICOV, ESR
prep_health_insurance_data <- function(raw_pumsdata_p){
  prepped_pumsdata_p <- raw_pumsdata_p %>% mutate(
    health_insurance = factor(case_when(
        !grepl("^(Civilian|Armed)", ESR) | AGEP < 16 ~ NA_character_,
        grepl("^With", HICOV) ~ "With health insurance",
        grepl("^No", HICOV) ~ "No health insurance")))
  return(prepped_pumsdata_p)
}

# Add labor force participation & employment status variables; 
# - limit to working age (16-64)
# - required variables: AGEP, ESR
prep_labor_force_data <- function(raw_pumsdata_p){
  prepped_pumsdata_p <- mutate(raw_pumsdata_p,
    labor_force_status = case_when(
      AGEP < 16 ~ NA_character_,
      between(AGEP, 16, 64) & grepl("^(Civilian|Armed|Unemployed)", ESR) ~ "In labor force",
      TRUE ~ ESR
    ),
    employment_status = case_when(
      AGEP < 16 | grepl("^Not", ESR) ~ NA_character_,
      between(AGEP, 16, 64) & grepl("^(Civilian|Armed)", ESR) ~ "Employed",
      TRUE ~ ESR
    ),
    PRACE_x_SEX = paste0(PRACE, ";", SEX)
  )
  return(prepped_pumsdata_p)
}

# Add personal wage/salary earnings (before taxes)
# - limit to working & over age 16
# - required variables: WAGP, ESR
prep_wage_data <- function(raw_pumsdata_p){
  prepped_pumsdata_p <- raw_pumsdata_p %>% mutate(
    wages = if_else(
        !grepl("^(Civilian|Armed)", ESR) | AGEP < 16, NA_real_,
         WAGP))
  return(prepped_pumsdata_p)
}

# Add poverty & low income status variables
# - limit to age 16+
# - required variables: POVPIP, AGEP
prep_poverty_data <- function(raw_pumsdata_p){
  prepped_pumsdata_p <- raw_pumsdata_p %>% mutate(
     low_income = case_when(is.na(POVPIP) | AGEP < 16 ~ NA_character_,
                            POVPIP < 200 ~ "Yes",
                            TRUE ~ "No"),
     poverty = case_when(is.na(POVPIP) | AGEP < 16 ~ NA_character_,
                         POVPIP < 100 ~ "Yes",
                         TRUE ~ "No")
  )
  return(prepped_pumsdata_p)
}
