## a function that takes a data frame with single target-location and calculates CvM for pairwise combinations
## this returns a matrix of CvM for the models for a single target-location
cd_comb <- function(single_tarloc_frame,approx_rule, tau){
  # get model names with no forecasts
  missing <- single_tarloc_frame %>%
    dplyr::select_if(~ any(is.na(.)))
  missing_names <- colnames(missing)
  missing_cols <- ncol(missing)
  # start calculation
  single_tarloc<- single_tarloc_frame[ , colSums(is.na(single_tarloc_frame)) == 0]
  # pairwise column calculation
  tmp <- single_tarloc %>%
    dplyr::select(-c("location","quantile"))
  nc <- ncol(tmp)
  cnames <- colnames(tmp)
  eg <- expand.grid(1:nc, 1:nc)
  nr <- nrow(eg)
  v <- vector(length=nr)
  for (i in 1:nr) {
    cc <- calc_cramers_dist_one_model_pair(as.numeric(unlist(tmp[,eg[i,1]])),
                                           tau,
                                           as.numeric(unlist(tmp[,eg[i,2]])),
                                           tau,
                                           approx_rule)
    v[i] <- cc
  }
  single_tarloc_cvm <- data.frame(model_1=rep(cnames,nc),
                                  model_2=rep(cnames,each=nc),
                                  approx_cd=v) 
  if(nrow(missing)!=0){
    # missing table
    missing_tab <- data.frame(model_1=rep(missing_names,sum(missing_cols,nc)),
                              model_2=rep(c(missing_names,cnames),each=missing_cols),
                              approx_cd=rep(NA,sum(missing_cols,nc)*missing_cols))
    # combine
    single_tarloc_cvm <- rbind(single_tarloc_cvm,missing_tab)
  } 
  return(single_tarloc_cvm)
}



## A wrapper to create a list of data frame for each target-location combination
build_dist_frame <- function(frame,approx_rule,tau){
  library(tidyverse)
  library(tidyr)
  main_frame <- frame 
  ncols <- ncol(main_frame)
  group_v <- main_frame %>%
    dplyr::mutate(model=as.character(model)) %>%
    dplyr::select(group_vec,model) %>%
    distinct()
  ## apply distance_combination function
  locations <- unique(main_frame$location)
  dist_frame <- map_dfr(locations,
                        function(loc){
                          main_frame %>%
                            dplyr::select(-"group_vec")  %>%
                            tidyr::pivot_wider(id_cols = c("location","quantile"),names_from =  "model") %>%
                            dplyr::filter(location==loc) %>%
                            dplyr::arrange(quantile) %>%
                            dplyr::select_if(~ !any(is.na(.))) %>%
                            cd_comb(., approx_rule, tau) %>%
                            dplyr::mutate(location=loc)
                          }
                        )
  
  # non_duplicate
  non_dup <- na.omit(dist_frame) %>%
    dplyr::filter(model_1 != model_2) %>%
    rowwise() %>%
    mutate(key = paste(sort(c(model_1, model_2)), collapse="")) %>%
    distinct(key, approx_cd, location, .keep_all=T) %>%
    dplyr::select(-key) %>%
    dplyr::left_join(group_v,by=c("model_1"="model")) %>%
    dplyr::mutate(group1=group_vec) %>%
    dplyr::select(-"group_vec") %>%
    dplyr::left_join(group_v,by=c("model_2"="model")) %>%
    dplyr::mutate(group2=group_vec) %>%
    dplyr::select(-"group_vec") %>%
    dplyr::mutate(type=ifelse(group1==group2,"within","between"))
  return(non_dup)
}
perm <- function(v) {
  n <- length(v)
  if (n == 1) v
  else {
    X <- NULL
    for (i in 1:n) X <- rbind(X, cbind(v[i], perm(v[-i])))
    X
  }
}