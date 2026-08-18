library(psrccensus)

# Shared data retrieval step is more efficient than calling get_psrc_pums() for each indicator.

# pums_rds on local or shared drive for faster/more reliable access; otherwise use pums_rds = NULL
jrds = "J:/Projects/Census/AmericanCommunitySurvey/Data/PUMS/pums_rds"

pvars <- c("SEX",
           "AGEP", 
           "PRACE",               # PSRC non-overlapping race/ethnicity variable
           "ESR",                 # Employment status recode
           "HICOV",               # Health insurance coverage
           "SOCP3",               # Occupation code
           "NAICSP",              # Industry code
           "SCHL",                # Educational attainment
           "WAGP",                # Wage or salary income past 12 months
           "POVPIP"               # Income as a percentage of the poverty level
)

hvars <- c("HRACE",               # PSRC non-overlapping race/ethnicity variable
           "ACCESSINET",          # Internet access
           "HINCP",               # Household income past 12 months
           "GRPIP",               # Household income as a percentage of the poverty level
)

get_pums_resequity_p <- function(span = 5, dyear, pums_rds = jrds){
    pumsdata <- get_psrc_pums(span = span, dyear, "p", pvars, dir = pums_rds)
    return(pumsdata)
}

get_pums_resequity_h <- function(span = 5, dyear, pums_rds = jrds){
    if(dyear < 2017){
    stop("Internet access data is not available for years prior to 2017.")
    }
    # Variable changed names w/ 2020 data; swap name temporarily for older data
    if(dyear %in% 2017:2019){hvars <- replace(hvars, hvars=="ACCESSINET","ACCESS")}
    pumsdata <- get_psrc_pums(span = span, dyear, "h", hvars, dir = pums_rds)
    if("ACCESS" %in% colnames(pumsdata)){pumsdata %<>% rename("ACCESSINET"="ACCESS")}
    return(pumsdata)
}
