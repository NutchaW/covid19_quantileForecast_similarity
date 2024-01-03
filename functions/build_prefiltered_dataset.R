library(tidyverse)
library(energy)
library(knitr)
library(data.table)
library(covidHubUtils)
library(RColorBrewer)
library(lubridate)
library(zoltr)
library(igraph)
library(gtools)
library(gtable)
library(gridExtra)
library(grid)
library(ggdendro)
library(gridGraphics)
source("./functions/filtered_data.R")

## Load data
melocs <- read.csv("./data/locations.csv")
locs <- melocs$location[1:52]
truth <- read.csv("./data/pre_filtered_data/truth.csv") 
#locs <- c("06","36","48","12","US")
truth <- truth  %>%
  dplyr::filter(location %in% locs)
death_forecasts <- read.csv("./data/pre_filtered_data/forecasts_death.csv") %>%
  dplyr::filter(location %in% locs)
length(unique(death_forecasts$location))
# hosp_forecasts <- purrr::map_dfr(c("./data/pre_filtered_data/forecasts_hosp1.csv",
#                                    "./data/pre_filtered_data/forecasts_hosp2.csv",
#                                    "./data/pre_filtered_data/forecasts_hosp3.csv"),
#                                  function(path) {
#                                    read.csv(path) %>%
#                                      dplyr::filter(location %in% locs)
#                                  })
# Save data
write_csv(truth,file = "./data/truth_prefiltered.csv")
write_csv(death_forecasts,file = "./data/death_forecasts_prefiltered.csv")
#write_csv(hosp_forecasts,file = "./data/hosp_forecasts_prefiltered.csv")

# Get metadata
metapath <- "./data/pre_filtered_data/metadata/"
metadata_frame <- get_metadata(metapath)
write_csv(metadata_frame,file = "./data/metadata.csv")

