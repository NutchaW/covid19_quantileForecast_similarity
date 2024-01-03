# distance calculation
library(tidyverse)
library(covidHubUtils)
source("./functions/distance_func_script.R")
melocs <- read.csv("./data/locations.csv")
locs <- melocs$location[1:52]
target_horizon1 <- 1:4
target_var <- c("inc death")
death_dat_gen <- read.csv("./data/formatted_df_gen.csv")  %>%
  dplyr::mutate(target_end_date = as.Date(target_end_date),
                forecast_date = as.Date(forecast_date,"1970-01-01"),
                # location = ifelse(location>=10,as.character(location),paste0("0",as.character(location)))
                )
# get test data for perm test
# death_dat_test <- read.csv("./data/formatted_df_test.csv") %>%
#   dplyr::mutate(target_end_date = as.Date(target_end_date),
#                 forecast_date = as.Date(forecast_date),
#                 ) 
death_dat_test <- read.csv("./data/formatted_df_test_fin2.csv") %>%
  dplyr::mutate(target_end_date = as.Date(target_end_date),
                forecast_date = as.Date(forecast_date,"1970-01-01"),
  ) 
# death_dat_test2 <- read.csv("./data/formatted_df_test_p2.csv") %>%
#   dplyr::mutate(target_end_date = as.Date(target_end_date),
#                 forecast_date = as.Date(forecast_date),
#                 ) 
# 
# death_dat_test_post <- read.csv("./data/formatted_df_test_post.csv") %>%
#   dplyr::mutate(target_end_date = as.Date(target_end_date),
#                 forecast_date = as.Date(forecast_date),
#                 )

# death_dat_test_post2 <- read.csv("./data/formatted_df_test_post2.csv") %>%
#   dplyr::mutate(target_end_date = as.Date(target_end_date),
#                 forecast_date = as.Date(forecast_date),
#                 )

# death_dat_test_dates <- read.csv("./data/formatted_df_test_alldates.csv") %>%
#   dplyr::mutate(target_end_date = as.Date(target_end_date),
#                 forecast_date = as.Date(forecast_date),
#                 location = ifelse(location>=10,as.character(location),paste0("0",as.character(location)))) 
truth_death <- read.csv("./data/truth_prefiltered.csv") %>%
  dplyr::mutate(value=ifelse(value<0,0,value),
                target_end_date = as.Date(target_end_date),
                location = ifelse(location>=10,as.character(location),paste0("0",as.character(location)))) %>%
  dplyr::filter(geo_type=="state",
                location %in% locs) %>%
  dplyr::filter(target_variable=="inc death") 
# %>%
#   dplyr::filter(target_end_date <= max(death_dat_gen$target_end_date),
#                 target_end_date >=  min(death_dat_gen$target_end_date))

# set quantiles
q_set <- unique(death_dat_test$quantile) 
#--------------- death forecasts calculation----------------------#
approx_cd_list_d_testl <-  suppressWarnings(build_distance_frame(death_dat_test,
                                                                horizon_list=target_horizon1,
                                                                target_list=target_var[1],
                                                                approx_rule="left_sided_riemann",
                                                                tau_F=q_set,tau_G=q_set,
                                                                truth=truth_death,scale=FALSE))
# approx_cd_list_d_testl2 <-  suppressWarnings(build_distance_frame(death_dat_test_dates,
#                                                                  horizon_list=target_horizon1,
#                                                                  target_list=target_var[1],
#                                                                  approx_rule="left_sided_riemann",
#                                                                  tau_F=q_set,tau_G=q_set,
#                                                                  truth=truth_death,scale=FALSE))

# save data
write_csv(approx_cd_list_d_testl[[1]],"./data/analysis_data/distance_test_l.csv")
write_csv(approx_cd_list_d_testl[[2]],"./data/analysis_data/norepeat_distance_test_l.csv")
# write_csv(approx_cd_list_d_testl2[[1]],"./data/analysis_data/distance_test_all_l.csv")
# write_csv(approx_cd_list_d_testl2[[2]],"./data/analysis_data/norepeat_distance_test_all_l.csv")

# approx_cd_list_d_testl_p2 <-  suppressWarnings(build_distance_frame(death_dat_test2,
#                                                                  horizon_list=target_horizon1,
#                                                                  target_list=target_var[1],
#                                                                  approx_rule="left_sided_riemann",
#                                                                  tau_F=q_set,tau_G=q_set,
#                                                                  truth=truth_death,scale=FALSE))
# 
# # save data
# write_csv(approx_cd_list_d_testl_p2[[1]],"./data/analysis_data/distance_test_l2.csv")
# write_csv(approx_cd_list_d_testl_p2[[2]],"./data/analysis_data/norepeat_distance_test_l2.csv")

approx_cd_list_d_testlp <-  suppressWarnings(build_distance_frame(death_dat_test_post,
                                                                 horizon_list=target_horizon1,
                                                                 target_list=target_var[1],
                                                                 approx_rule="left_sided_riemann",
                                                                 tau_F=q_set,tau_G=q_set,
                                                                 truth=truth_death,scale=FALSE))

# save data
write_csv(approx_cd_list_d_testlp[[1]],"./data/analysis_data/distance_test_lp.csv")
write_csv(approx_cd_list_d_testlp[[2]],"./data/analysis_data/norepeat_distance_test_lp.csv")

# approx_cd_list_d_testlp2 <-  suppressWarnings(build_distance_frame(death_dat_test_post2,
#                                                                   horizon_list=target_horizon1,
#                                                                   target_list=target_var[1],
#                                                                   approx_rule="left_sided_riemann",
#                                                                   tau_F=q_set,tau_G=q_set,
#                                                                   truth=truth_death,scale=FALSE))
# 
# # save data
# write_csv(approx_cd_list_d_testlp2[[1]],"./data/analysis_data/distance_test_lp2.csv")
# write_csv(approx_cd_list_d_testlp2[[2]],"./data/analysis_data/norepeat_distance_test_lp2.csv")

#--------------- approx1----------------------#
approx_cd_list_d_test <-  suppressWarnings(build_distance_frame(death_dat_test,
                                                                horizon_list=target_horizon1,
                                                                target_list=target_var[1],
                                                                approx_rule="approximation1",
                                                                tau_F=q_set,tau_G=q_set,
                                                                truth=truth_death,scale=FALSE))
approx_cd_list_d_test2 <-  suppressWarnings(build_distance_frame(death_dat_test_dates,
                                                                 horizon_list=target_horizon1,
                                                                 target_list=target_var[1],
                                                                 approx_rule="approximation1",
                                                                 tau_F=q_set,tau_G=q_set,
                                                                 truth=truth_death,scale=FALSE))

# save data
write_csv(approx_cd_list_d_test[[1]],"./data/analysis_data/distance_test.csv")
write_csv(approx_cd_list_d_test[[2]],"./data/analysis_data/norepeat_distance_test.csv")
write_csv(approx_cd_list_d_test2[[1]],"./data/analysis_data/distance_test_all.csv")
write_csv(approx_cd_list_d_test2[[2]],"./data/analysis_data/norepeat_distance_test_all.csv")

##--------------- general set----------------------#
approx_cd_list_d <-  suppressWarnings(
  build_distance_frame(death_dat_gen,
                       horizon_list=target_horizon1,
                       target_list=target_var[1],
                       approx_rule="approximation1",
                       tau_F=q_set,tau_G=q_set,
                       truth=truth_death,scale=FALSE))
# save data
write_csv(approx_cd_list_d[[1]],"./data/analysis_data/distance_all.csv")
write_csv(approx_cd_list_d[[2]],"./data/analysis_data/norepeat_distance_all.csv")
##--------------- general set2----------------------#
approx_cd_list_dl <-  suppressWarnings(
  build_distance_frame(death_dat_gen,
                       horizon_list=target_horizon1,
                       target_list=target_var[1],
                       approx_rule="left_sided_riemann",
                       tau_F=q_set,tau_G=q_set,
                       truth=truth_death,scale=FALSE))
# save data
write_csv(approx_cd_list_dl[[1]],"./data/analysis_data/distance_all_l.csv")
write_csv(approx_cd_list_dl[[2]],"./data/analysis_data/norepeat_distance_all_l.csv")

##--------------- general set2 but with other measures----------------------#

approx_cd_list_d_test <-  suppressWarnings(build_distance_frame(death_dat_test,
                                                                horizon_list=target_horizon1,
                                                                target_list=target_var[1],
                                                                approx_rule="left_sided_riemann",
                                                                tau_F=q_set,tau_G=q_set,
                                                                truth=truth_death,scale=FALSE))
write_csv(approx_cd_list_d_test[[1]],"./data/analysis_data/distance_test.csv")
write_csv(approx_cd_list_d_test[[2]],"./data/analysis_data/norepeat_distance_test.csv")

approx_cd_list_dl_oth <-  suppressWarnings(
  build_oth_measure_frame(death_dat_gen,
                          horizon_list=target_horizon1,
                          target_list=target_var[1],
                          approx_rule="left_sided_riemann",
                          tau_F=q_set,tau_G=q_set,
                          truth=truth_death,scale=FALSE))

# save data
write_csv(approx_cd_list_dl_oth[[1]],"./data/analysis_data/oth_distance_all_l.csv")
write_csv(approx_cd_list_dl_oth[[2]],"./data/analysis_data/oth_norepeat_distance_all_l.csv")

