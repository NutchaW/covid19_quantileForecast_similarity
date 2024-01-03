## wrapper and functions for calculating pairwise CvM
## a function to format the dataframe
data_filter <- function(zoltr_frame, quantiles,loc_list, sdate, edate){
  filtered_data <- zoltr_frame %>%
    dplyr::filter(geo_type=="state",
                  target_end_date >= sdate & target_end_date <= edate,
                  !(model %in% c("CU-nochange","CU-scenario_high","CU-scenario_mid","CU-scenario_low")),
                  type=="quantile") %>%
    dplyr::mutate(forecast_date=ifelse(target_variable== "inc death", 
                                       as.Date(target_end_date-(as.numeric(horizon)*7)+2,origin = "1970-01-01"),
                                       as.Date(forecast_date, origin = "1970-01-01"))) %>%
    # check quantiles
    dplyr::group_by(model,target_end_date,location,horizon) %>%
    dplyr::mutate(quant=ifelse(all(quantiles %in% quantile), TRUE,FALSE)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(model) %>%
    dplyr::filter(all(quant == TRUE)) %>%
    dplyr::ungroup() %>%    
    # check all horizons
    dplyr::group_by(model,target_end_date,location) %>%
    dplyr::mutate(n_horiz = n_distinct(horizon)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(model,target_end_date) %>%
    dplyr::filter(ifelse(target_variable== "inc death",
                         all(n_horiz == max(n_horiz)),
                         all(n_horiz == max(n_horiz)))) %>%
    dplyr::ungroup() %>%
    # check all locations
    dplyr::group_by(model,target_end_date,horizon) %>%
    dplyr::mutate(n_locs = n_distinct(location)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(model) %>%
    dplyr::filter(all(n_locs >= 4)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c("quant","n_locs","n_horiz")) 
  return(filtered_data)
} 


data_filter_ens <- function(zoltr_frame, quantiles,loc_list, sdate, edate){
  # get dates
  set <- zoltr_frame %>%
    dplyr::filter(model=="COVIDhub-4_week_ensemble",
                  geo_type=="state",
                  target_end_date >= sdate & target_end_date <= edate,
                  type=="quantile") %>%
    dplyr::mutate(forecast_date=ifelse(target_variable== "inc death", 
                                       as.Date(target_end_date-(as.numeric(horizon)*7)+2,origin = "1970-01-01"),
                                       as.Date(forecast_date, origin = "1970-01-01"))) %>%
    # check all locations
    dplyr::group_by(target_end_date,horizon) %>%
    dplyr::mutate(n_locs = n_distinct(location)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(model) %>%
    dplyr::filter(all(n_locs >= 4)) %>%
    dplyr::ungroup() %>%
    # check all horizons
    dplyr::group_by(target_end_date,location) %>%
    dplyr::mutate(n_horiz = n_distinct(horizon)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(target_end_date) %>%
    dplyr::filter(ifelse(target_variable== "inc death",
                         all(n_horiz == max(n_horiz)),
                         all(n_horiz == max(n_horiz)))) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c("n_locs","n_horiz")) %>%
    dplyr::select(target_end_date,forecast_date,horizon)
  # get data
  filtered_data <- zoltr_frame %>%
    dplyr::filter(!(model %in% c("CU-nochange","CU-scenario_high","CU-scenario_mid","CU-scenario_low"))) %>%
    dplyr::filter(target_end_date %in% set$target_end_date,
                  forecast_date %in% set$forecast_date,
                  horizon %in% set$horizon)
  return(filtered_data)
} 


## format data
frame_format <- function(ftr_frame){
  # final clean-up
  matrix_frame <- ftr_frame %>%
    dplyr::filter(type=="quantile") %>%
    dplyr::select("location","target_variable","target_end_date",
                  "type","quantile","model","value","horizon","forecast_date") %>%
    tidyr::pivot_wider(id_cols=c("location","target_variable","target_end_date",
                                 "forecast_date","type","quantile","horizon"), 
                       names_from = model, 
                       values_from = value) %>%
    dplyr::arrange(location,horizon,target_variable,forecast_date,target_end_date,quantile) %>%
    dplyr::select_if(~ !any(is.na(.)))
  return(matrix_frame)
} 

frame_format_ens <- function(ftr_frame){
  # final clean-up
  matrix_frame <- ftr_frame %>%
    dplyr::filter(type=="quantile") %>%
    dplyr::select("location","target_variable","target_end_date",
                  "type","quantile","model","value","horizon","forecast_date") %>%
    tidyr::pivot_wider(id_cols=c("location","target_variable","target_end_date",
                                 "forecast_date","type","quantile","horizon"), 
                       names_from = model, 
                       values_from = value) %>%
    dplyr::arrange(location,horizon,target_variable,forecast_date,target_end_date,quantile) 
  return(matrix_frame)
} 

## build metadata
get_metadata <- function(metadata_path) {
  # get all paths
  path_list <- list.files(metadata_path)
  # construct path to metadata file from the root of hub repo
  model_metadata_paths <- paste0(metadata_path, path_list)
  
  model_info <- purrr::map_dfr(
    model_metadata_paths,
    function(model_metadata_path) {
    #  model_metadata_path <- paste0(hub_repo_path, "/", model_metadata_path)
        metadata_list <- yaml::read_yaml(model_metadata_path)
        metadata_list <- metadata_list[!sapply(metadata_list, is.null)]
        tmp <- as.data.frame(metadata_list, stringsAsFactors = FALSE)
        return(tmp)
      }
    )
  # %>%
  #     dplyr::rename(model = model_abbr, designation = team_model_designation)
  
  return(model_info)
}

data_filter_fdate <- function(zoltr_frame, quantiles,loc_list, sdate, edate){
  filtered_data <- zoltr_frame %>%
    dplyr::filter(geo_type=="state",
                  forecast_date >= sdate & forecast_date <= edate,
                  !(model %in% c("CU-nochange","CU-scenario_high","CU-scenario_mid","CU-scenario_low")),
                  type=="quantile") %>%
    dplyr::mutate(forecast_date=ifelse(target_variable== "inc death", 
                                       as.Date(target_end_date-(as.numeric(horizon)*7)+2,origin = "1970-01-01"),
                                       as.Date(forecast_date, origin = "1970-01-01"))) %>%
    # filter out non-monday forecast dates
    dplyr::filter(weekdays(as.Date(forecast_date, origin = "1970-01-01"))=="Monday") %>%
    # check quantiles
    dplyr::group_by(model,forecast_date,location,horizon) %>%
    dplyr::mutate(quant=ifelse(all(quantiles %in% quantile), TRUE,FALSE)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(model) %>%
    dplyr::filter(all(quant == TRUE)) %>%
    dplyr::ungroup() %>%    
    # check all horizons
    dplyr::group_by(model,forecast_date,location) %>%
    dplyr::mutate(n_horiz = n_distinct(horizon)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(model,forecast_date) %>%
    dplyr::filter(ifelse(target_variable== "inc death",
                         all(n_horiz == max(n_horiz)),
                         all(n_horiz >= 14))) %>%
    dplyr::ungroup() %>%
    # check all locations
    dplyr::group_by(model,forecast_date,horizon) %>%
    dplyr::mutate(n_locs = n_distinct(location)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(model) %>%
    dplyr::filter(all(n_locs >= 4)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c("quant","n_locs","n_horiz")) 
  return(filtered_data)
} 

data_filter_stats <- function(zoltr_frame, quantiles,loc_list, date_range){
  filtered_data <- zoltr_frame %>%
    dplyr::filter(geo_type=="state",
                  !(model %in% c("CU-nochange","CU-scenario_high","CU-scenario_mid","CU-scenario_low"))) %>%
    dplyr::mutate(forecast_date=ifelse(target_variable== "inc death", 
                                       as.Date(target_end_date-(as.numeric(horizon)*7)+2,origin = "1970-01-01"),
                                       as.Date(forecast_date, origin = "1970-01-01"))) %>%
    dplyr::filter(forecast_date %in% date_range) %>%
    # filter out non-monday forecast dates
    dplyr::filter(weekdays(as.Date(forecast_date, origin = "1970-01-01"))=="Monday") %>%
    # check quantiles
    dplyr::group_by(model,forecast_date,location,horizon) %>%
    dplyr::mutate(quant=ifelse(all(c(quantiles,"point") %in% c(quantile,"point")), TRUE,FALSE)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(model) %>%
    dplyr::filter(all(quant == TRUE)) %>%
    dplyr::ungroup() %>%
    # check all horizons
    dplyr::group_by(model,forecast_date,location) %>%
    dplyr::mutate(n_horiz = n_distinct(horizon)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(model,forecast_date) %>%
    dplyr::filter(ifelse(target_variable== "inc death",
                         all(n_horiz == max(n_horiz)),
                         all(n_horiz == max(n_horiz)))) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(model,forecast_date,horizon) %>%
    dplyr::mutate(n_locs = n_distinct(location)) %>%
    dplyr::ungroup()
  # %>%
  #   dplyr::group_by(model) %>%
  #   dplyr::filter(all(n_locs >= 4)) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::select(-c("quant","n_locs","n_horiz"))
  return(filtered_data)
} 

## a function to format the dataframe
data_filter_test <- function(zoltr_frame, quantiles,loc_list, datedat,ph,list,ens_list){
  filtered_data0 <- zoltr_frame %>%
    dplyr::mutate(forecast_date=ifelse(target_variable== "inc death", 
                                       as.Date(target_end_date-(as.numeric(horizon)*7)+2,origin = "1970-01-01"),
                                       as.Date(forecast_date, origin = "1970-01-01"))) %>%
    dplyr::filter(geo_type=="state",
                  !(model %in% c("CU-nochange","CU-scenario_high","CU-scenario_mid","CU-scenario_low",ens_list)),
                  type=="quantile",
                  location %in% loc_list) 
  filtered_data <- filtered_data0 %>%
    dplyr::mutate(forecast_date=as.Date(forecast_date, origin = "1970-01-01")) %>%
    dplyr::left_join(datedat,by=c("location","forecast_date")) %>%
    dplyr::filter(phase==ph)  %>%
    # check quantiles
  dplyr::group_by(model,forecast_date,location,horizon) %>%
  dplyr::mutate(quant=n_distinct(quantile)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(model) %>%
  dplyr::filter(all(quant == 23)) %>%
  dplyr::ungroup()  %>%
  dplyr::group_by(model,forecast_date,location) %>%
  dplyr::mutate(n_horiz = n_distinct(horizon)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(model) %>%
  dplyr::filter(all(n_horiz == 4)) %>%
  dplyr::ungroup() %>%
  group_by(model,location,target_end_date,horizon,quantile) %>%
  slice(which.max(forecast_date)) %>%
  dplyr::ungroup()
  return(filtered_data)
} 

data_filter_test2 <- function(zoltr_frame, quantiles,loc_list,attime_list){
  filtered_data <- zoltr_frame %>%
    dplyr::mutate(forecast_date=ifelse(target_variable== "inc death", 
                                       as.Date(target_end_date-(as.numeric(horizon)*7)+2,origin = "1970-01-01"),
                                       as.Date(forecast_date, origin = "1970-01-01"))) %>%
    dplyr::filter(geo_type=="state",
                  model %in% attime_list,
                  type=="quantile",
                  forecast_date > as.Date("2020-10-20"),
                  location %in% loc_list) %>%
    # check quantiles
    dplyr::group_by(model,forecast_date,location,horizon) %>%
    dplyr::mutate(quant=ifelse(all(quantiles %in% quantile), TRUE,FALSE)) %>%
    dplyr::filter(quant == TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(forecast_date=as.Date(forecast_date, origin = "1970-01-01")) %>%
    dplyr::group_by(model,forecast_date,location) %>%
    dplyr::filter(n_distinct(horizon)==4) %>%
    dplyr::ungroup() %>%
    #noooooooo
    # dplyr::group_by(forecast_date) %>%
    # dplyr::filter(all(n_locs>=20)) %>%
    # dplyr::ungroup() %>%
    # dplyr::group_by(location) %>%
    # dplyr::filter(all(n_dates>=20)) %>%
    # dplyr::ungroup() %>%
    # dplyr::group_by(model) %>%
    # dplyr::filter(all(n_locs>=35)) %>%
    # dplyr::ungroup() %>%
    # dplyr::mutate(n_dates=n_distinct(forecast_date)) %>%
    # dplyr::filter(n_dates>=15) %>%
    # dplyr::ungroup() %>%
    dplyr::mutate(forecast_date=as.Date(forecast_date, origin = "1970-01-01"))
  return(filtered_data)
} 

data_filter_gen <- function(zoltr_frame, quantiles,loc_list){
  # get dates
  filtered_data <- zoltr_frame %>%
    dplyr::filter(geo_type=="state",
                  location %in% loc_list,
                  type=="quantile",
                  !(model %in% c("CU-nochange","CU-scenario_high","CU-scenario_mid","CU-scenario_low",
                                 "COVIDhub-ensemble","COVIDhub-trained_ensemble"))) %>%
    dplyr::mutate(forecast_date=ifelse(target_variable== "inc death", 
                                       as.Date(target_end_date-(as.numeric(horizon)*7)+2,origin = "1970-01-01"),
                                       as.Date(forecast_date, origin = "1970-01-01"))) %>%
    # check quantiles
    dplyr::group_by(model,forecast_date,location,horizon) %>%
    dplyr::mutate(quant=ifelse(all(quantiles %in% quantile), TRUE,FALSE)) %>%
    dplyr::filter(all(quant == TRUE)) %>%
    dplyr::ungroup() %>%
    # check all horizons
    dplyr::group_by(model,forecast_date,location) %>%
    dplyr::filter(n_distinct(horizon)== 4) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"quant") %>%
    group_by(model,location,target_end_date,horizon,quantile) %>%
    slice(which.max(forecast_date)) %>%
    dplyr::ungroup()
  return(filtered_data)
} 

data_filter_t <- function(zoltr_frame,date_dat,loc_list){
  # get dates
  filtered_data <- zoltr_frame %>%
    dplyr::filter(geo_type=="state",
                  location %in% loc_list,
                  type=="quantile",
                  !(model %in% c("CU-nochange","CU-scenario_high","CU-scenario_mid","CU-scenario_low",
                                 "COVIDhub-ensemble","COVIDhub-trained_ensemble")),
                  horizon %in% c(2,4)) %>%
    dplyr::mutate(forecast_date=ifelse(target_variable== "inc death", 
                                       as.Date(target_end_date-(as.numeric(horizon)*7)+2,origin = "1970-01-01"),
                                       as.Date(forecast_date, origin = "1970-01-01"))) %>%
    # # check quantiles
    # dplyr::group_by(model,target_end_date,location,horizon) %>%
    # dplyr::mutate(quant=ifelse(all(quantiles %in% quantile), TRUE,FALSE)) %>%
    # dplyr::filter(all(quant == TRUE)) %>%
    # dplyr::ungroup()
    # # check dates
    # up <- filtered_data %>%
      dplyr::left_join(date_dat,by="location") %>%
      group_by(location,horizon) %>%
      dplyr::filter(target_end_date >= up_start,
                    target_end_date <= up_end) %>%
      dplyr::ungroup()
    # fdat <- up %>%
    #   group_by(location,horizon,model) %>%
    #   dplyr::filter(n_distinct(target_end_date)>=6) %>%
    #   dplyr::ungroup()  %>%
    #   #check all horizons
    #   dplyr::group_by(model,target_end_date,location) %>%
    #   dplyr::filter(n_distinct(horizon)== 2) %>%
    #   dplyr::mutate(n_hoz=n_distinct(horizon)) %>%
    #   dplyr::ungroup() %>%
    #   dplyr::select(-"quant") %>%
    #  # check all locs
    #   dplyr::group_by(model,horizon) %>%
    #   dplyr::mutate(check=n_distinct(location)) %>%
    #   dplyr::filter(n_distinct(location)==length(loc_list)) %>%
    #   dplyr::ungroup()
  return(filtered_data)
} 

# do all location with the model set and time being fixed.
data_filter_stats <- function(zoltr_frame, quantiles,date_dat,s_list){
  # get dates
  filtered_data <- zoltr_frame %>%
    dplyr::filter(geo_type=="state",
                  # location %in% loc_list,
                  type=="quantile",
                  model %in% s_list,
                  horizon %in% c(2,4)) %>%
    dplyr::mutate(forecast_date=ifelse(target_variable== "inc death", 
                                       as.Date(target_end_date-(as.numeric(horizon)*7)+2,origin = "1970-01-01"),
                                       as.Date(forecast_date, origin = "1970-01-01"))) %>%
    # check quantiles
    dplyr::group_by(model,target_end_date,location,horizon) %>%
    dplyr::mutate(quant=ifelse(all(quantiles %in% quantile), TRUE,FALSE)) %>%
    dplyr::filter(all(quant == TRUE)) %>%
    dplyr::ungroup() %>%
    # check dates
    dplyr::left_join(date_dat,by="location") %>%
    group_by(location,horizon) %>%
    dplyr::filter(target_end_date >= start,
                  target_end_date <= end) %>%
    dplyr::ungroup() %>%
    group_by(location,horizon,model) %>%
    dplyr::mutate(ndates = n_distinct(target_end_date)) %>%
    dplyr::ungroup() %>%
    group_by(location,horizon) %>%
    dplyr::filter(ndates == max(ndates)) %>%
    dplyr::mutate(n_mod=n_distinct(model)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"ndates") %>%
    # check all horizons
    dplyr::group_by(model,target_end_date,location) %>%
    dplyr::filter(n_distinct(horizon)== 2) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"quant") %>%
    # check all locs
    dplyr::group_by(model,target_end_date,horizon) %>%
    dplyr::mutate(check=n_distinct(location)) %>%
    dplyr::ungroup() %>%
    # get one forecast per target date
    group_by(model,location,target_end_date,horizon,quantile) %>%
    # slice(which.max(forecast_date)) %>%
    dplyr::filter(forecast_date==max(forecast_date)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(check>=5 & n_mod>=5)
  return(filtered_data)
} 

dis_filter <- function(formatted_frame){
  # get dates
  set <- formatted_frame %>%
    dplyr::mutate(forecast_date=ifelse(target_variable== "inc death",
                                       as.Date(target_end_date-(as.numeric(horizon)*7)+2,origin = "1970-01-01"),
                                       as.Date(forecast_date, origin = "1970-01-01"))) %>%
    # check all horizons
    dplyr::group_by(model_1,model_2,forecast_date,location) %>%
    dplyr::filter(n_distinct(horizon)==4) %>%
    dplyr::ungroup() 
  # %>%
    # # check more than 80% in each location horizon
    # dplyr::group_by(location,horizon) %>%
    # dplyr::mutate(max_dates = n_distinct(target_end_date)) %>%
    # dplyr::ungroup() %>%
    # dplyr::group_by(model_1,model_2,location,horizon) %>%
    # dplyr::mutate(n_dates = n_distinct(target_end_date)) %>%
    # dplyr::ungroup() %>%
    # dplyr::group_by(model_1,model_2) %>%
    # dplyr::filter(all(n_dates >= .8*max_dates)) %>%
    # dplyr::ungroup() %>%
    # # check all locations
    # dplyr::group_by(model_1,model_2,target_end_date) %>%
    # dplyr::mutate(n_locs = n_distinct(location)) %>%
    # dplyr::ungroup() %>%
    # dplyr::group_by(model_1,model_2) %>%
    # dplyr::mutate(check=n_locs*n_dates) %>%
    # dplyr::filter(all(check>=0.8*(n_dates*51))) %>%
    # dplyr::ungroup() %>%
    # dplyr::group_by(model_1,model_2) %>%
    # dplyr::mutate(check=n_locs*n_dates) %>%
    # dplyr::filter(all(n_locs > 25)) %>%
    # dplyr::ungroup() %>%
    # dplyr::select(-c("n_locs","n_dates","check"))
  return(set)
}


overall_filter <- function(frame){
  # get dates
  set <- frame %>%
    dplyr::filter(location !="US",
                  target_end_date>="2020-05-02",
                  target_end_date<="2021-12-18",
                  type=="quantile") %>%
    # check all horizons
    dplyr::group_by(model,target_end_date,location) %>%
    dplyr::filter(n_distinct(horizon)==4) %>%
    dplyr::ungroup() %>%
    # check quantiles for each combination
    dplyr::group_by(model,forecast_date,target_end_date,location,horizon) %>%
    dplyr::filter(n_distinct(quantile) == 23) %>%
    dplyr::ungroup()  %>%
   # check location .7*50 = 40
    dplyr::group_by(model,forecast_date,target_end_date,horizon) %>%
    dplyr::filter(n_distinct(location)>=35) %>%
    dplyr::ungroup() %>%
   # check dates .7 of all dates
    dplyr::mutate(max_dates=n_distinct(target_end_date)) %>%
    dplyr::group_by(model) %>%
    dplyr::filter(n_distinct(target_end_date)>=(.7*max_dates)) %>%
    dplyr::ungroup()
  return(set)
}