library(tidyverse)
library(tidyr)
source("./functions/decomposition.R")
# decomposition calculation
death_dat_gen <- read.csv("./data/formatted_df_gen.csv") %>%
  dplyr::mutate(target_end_date = as.Date(target_end_date),
                forecast_date = as.Date(forecast_date,"1970-01-01"),
                location = ifelse(location>=10,as.character(location),paste0("0",as.character(location)))) 
decom_models <- c("UMass.MechBayes","COVIDhub.4_week_ensemble")

decom_list_d <- death_dat_gen %>%
  dplyr::mutate(location=as.character(location)) %>%
#  dplyr::select(c(colnames(death_dat_test)[1:7],decom_models)) %>%
  decomp_wrapper(.,
                 horizon_list=c(1:4),
                 target_list="inc death")

write_csv(decom_list_d[[1]],file = "./data/analysis_data/decomp_data_left.csv") 

## filter data for use
dist_gen_norepeat_wide <- read.csv("./data/analysis_data/norepeat_distance_all_l.csv") %>%
  dplyr::mutate(target_end_date = as.Date(target_end_date),
                location = ifelse(nchar(location)==3,paste0("0",as.numeric(location)),location)) %>%
  dplyr::select(-c("n_dates","target_variable")) %>%
  dplyr::filter(horizon %in% c(1,4)) %>%
  pivot_wider(id_cols=c("model_1","model_2","location","target_end_date"),
              names_from = horizon,values_from = approx_cd) %>%
  drop_na() %>%
  dplyr::mutate(higher=ifelse(`1` >= `4`, "higher","lower")) 
common_dates <- unique(dist_gen_norepeat_wide$target_end_date)
decomp_gen <- read.csv("./data/analysis_data/decomp_data_left.csv") %>%
  na.omit(.) %>%
  dplyr::filter(model_1 != model_2) %>%
  rowwise() %>%
  mutate(key = paste(sort(c(model_1, model_2)), collapse="")) %>%
  distinct(key, approx_cd, horizon, location, target_end_date, .keep_all=T) %>%
  select(-key) %>%
  dplyr::group_by(horizon,location,target_variable,model_1,model_2) %>%
  dplyr::mutate(n_dates=n_distinct(target_end_date)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(target_end_date %in% common_dates,
                horizon %in% c(1,4))
decomp_gen <- decomp_gen %>%
  dplyr::mutate(target_end_date = as.Date(target_end_date,"1970-01-01"),
                location = ifelse(nchar(location)==3,paste0("0",as.numeric(location)),location)) 
write_csv(decomp_gen,file = "./data/analysis_data/decomp_data_filtered.csv") 
# ##
# decomp_gen_c <- decomp_gen %>%
#   filter(model_1 %in% decom_models & model_2 %in% decom_models)
# write_csv(decomp_gen_c,file = "./data/analysis_data/decomp_data_left.csv") 

