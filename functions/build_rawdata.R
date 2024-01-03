library(lubridate)
library(tidyverse)
library(zoltr)
library(covidHubUtils)
#devtools::install_github("reichlab/covidHubUtils",force=TRUE)
#devtools::install_github("reichlab/zoltr",force=TRUE)
#----------------------------------Forecast data--------------------------------------------#

# script to build a dataframe of model 
# first forcast date (a Monday)
first_monday <- as.Date("2020-04-01")
last_monday <- as.Date("2022-01-31")

# get all mondays for each saturday end dates since first saturday end date for all targets
dates <- seq(first_monday,last_monday,by = "week")
dates1 <- dates[1:floor(length(dates)/3)]  
dates2 <- dates[ceiling(length(dates)/3):(ceiling(length(dates)/3)*2)]   
dates3 <- dates[((ceiling(length(dates)/3)*2)+1):length(dates)]   

# set targets for analysis
target_list1 <- paste0(1:4," wk ahead inc death")
target_list2 <- paste0(1:28," day ahead inc hosp")
# get location fips
locs <- read.csv("./data/locations.csv") %>%
  dplyr::filter(!stringr::str_detect(location_name,"County") & location_name!="District of Columbia")
  
# Load forecasts that were submitted in a time window from zoltar
for(i in 1:3){
  assign(paste0("death",i),
  load_forecasts(
    dates = get(paste0("dates",i)),
    date_window_size = 6,
    locations = locs$location,
    types = c("point", "quantile"),
    targets = target_list1,
    source = "zoltar",
    verbose = FALSE,
    as_of = NULL,
    hub = c("US")
  )
  )
}

# write large files
write_csv(rbind(death1, death2, death3),file = "./data/pre_filtered_data/forecasts_death.csv")

# get death forecasts
for(i in 1:1){
  assign(paste0("hosp",i),
load_forecasts(
  # fix dates
  dates = get(paste0("dates",i)),
  date_window_size = 6,
  locations = locs$location,
  types = c("point", "quantile"),
  targets = target_list2,
  source = "zoltar",
  verbose = FALSE,
  as_of = NULL,
  hub = c("US")
)
  )
}
# write large files
write_csv(hosp1,file = "./data/pre_filtered_data/forecasts_hosp1.csv")

for(i in 2:2){
  assign(paste0("hosp",i),
         load_forecasts(
           # fix dates
           dates = get(paste0("dates",i)),
           date_window_size = 6,
           locations = locs$location,
           types = c("point", "quantile"),
           targets = target_list2,
           source = "zoltar",
           verbose = FALSE,
           as_of = NULL,
           hub = c("US")
         )
  )
}
# write large files
write_csv(hosp2,file = "./data/pre_filtered_data/forecasts_hosp2.csv")

for(i in 3:3){
  assign(paste0("hosp",i),
         load_forecasts(
           # fix dates
           dates = get(paste0("dates",i)),
           date_window_size = 6,
           locations = locs$location,
           types = c("point", "quantile"),
           targets = target_list2,
           source = "zoltar",
           verbose = FALSE,
           as_of = NULL,
           hub = c("US")
         )
  )
}
# write large files
write_csv(hosp3,file = "./data/pre_filtered_data/forecasts_hosp3.csv")

#----------------------------------Truth data--------------------------------------------#
truth_data <- load_truth(
  truth_source = "JHU",
  target_variable = c("inc hosp", "inc death"),
  hub = "US"
)
write_csv(truth_data,file = "./data/pre_filtered_data/truth.csv")