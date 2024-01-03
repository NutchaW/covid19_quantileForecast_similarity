## a function that takes a data frame with single target-location and calculates CvM for pairwise combinations
## this returns a matrix of CvM for the models for a single target-location
cd_combination <- function(single_tarloc_frame,approx_rule, tau_F,tau_G){
  # get model names with no forecasts
  missing <- single_tarloc_frame %>%
    dplyr::select_if(~ any(is.na(.)))
    # single_tarloc_frame[ , colSums(is.na(single_tarloc_frame)) > 0] %>%
    # dplyr::select(-c("target_variable","target_end_date","location","type","quantile","horizon"))
  missing_names <- colnames(missing)
  missing_cols <- ncol(missing)
  # start calculation
  # remove any models with NA for values for this target location (assuming all models have the same quantiles)
  single_tarloc<- single_tarloc_frame[ , colSums(is.na(single_tarloc_frame)) == 0]
  # pairwise column calculation
  tmp <- single_tarloc %>%
    dplyr::select(-c("target_variable","target_end_date","location","type","quantile","horizon"))
  nc <- ncol(tmp)
  cnames <- colnames(tmp)
  eg <- expand.grid(1:nc, 1:nc)
  nr <- nrow(eg)
  v <- vector(length=nr)
  for (i in 1:nr) {
    cc <- calc_cramers_dist_one_model_pair(as.numeric(unlist(tmp[,eg[i,1]])),
                                           tau_F,
                                           as.numeric(unlist(tmp[,eg[i,2]])),
                                           tau_G,
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
### by filter on target and location
build_distance_frame <- function(model_dataframe, horizon_list,target_list, 
                                 approx_rule,tau_F,tau_G, truth, scale=FALSE){
  library(tidyverse)
  library(tidyr)
  # remove point estimates and forecast_date
  truth <- truth %>%
    dplyr::transmute(tar_date = as.Date(target_end_date),
                     location = as.character(location),
                     value = value) 
  main_frame <- model_dataframe %>% 
    dplyr::mutate(horizon = as.numeric(as.character(horizon)),
                  target_variable = as.character(target_variable),
                  location = as.character(location), 
                  target_end_date = as.Date(target_end_date)) %>%
    dplyr::filter(target_variable %in% target_list,
                  horizon %in% horizon_list) 
  
 # dist_frame <- data.frame()
  ncols <- ncol(main_frame)
  if(scale){
    main_frame <- main_frame %>% 
      dplyr::mutate(for_date = as.Date(floor_date(target_end_date-(7*horizon), "week")-1)) %>%
      dplyr::left_join(truth, 
                       by=c("location", "for_date" = "tar_date")) %>%
      dplyr::select(-for_date)
    main_frame <-cbind(main_frame[,c(1:6)],main_frame[,c(7:ncols)]/main_frame$value)
  } 
  if("forecast_date" %in% c(colnames(main_frame))){
    main_frame <- main_frame %>%
      dplyr::select(-"forecast_date")
  }
  ## apply distance_combination function
  locations <- unique(main_frame$location)
  dist_frame <- map_dfr(locations,
                        function(loc) {
                          map_dfr(target_list,
                                  function(target) {
                                    map_dfr(horizon_list,
                                            function(horiz) {
                                              tar_loc <- main_frame %>%
                                                dplyr::filter(location==loc,
                                                              target_variable==target,
                                                              horizon==horiz)
                                              end_dates <- unique(tar_loc$target_end_date)
                                              map_dfr(end_dates,
                                                      function(end_date) {
                                                        tar_loc %>%
                                                          dplyr::filter(target_end_date==end_date) %>%
                                                          dplyr::arrange(quantile) %>%
                                                          dplyr::select_if(~ !any(is.na(.))) %>%
                                                          cd_combination(., approx_rule, tau_F,tau_G) %>%
                                                          dplyr::mutate(horizon=horiz,
                                                                        location=loc,
                                                                        target_variable=target,
                                                                        target_end_date=end_date)
                                                      })
                                            })
                                  })
                        })

    # non_duplicate
    non_dup <- na.omit(dist_frame) %>%
      dplyr::filter(model_1 != model_2) %>%
      rowwise() %>%
      mutate(key = paste(sort(c(model_1, model_2)), collapse="")) %>%
      distinct(key, approx_cd, horizon, location, target_end_date, .keep_all=T) %>%
      select(-key) %>%
      dplyr::group_by(horizon,location,target_variable,model_1,model_2) %>%
      dplyr::mutate(n_dates=n_distinct(target_end_date)) %>%
      dplyr::ungroup()
    return(list(full_dataframe=dist_frame,non_dup = non_dup))
}

# # create matrix
# cd_matrix <- function(mean_cd_frame, h){
# # cd_matrix <- function(mean_cd_frame, h, target){
#   # pairwise column calculation
#   # arrange by largest average of mean dis compared to all forecasts
#   tmp <-mean_cd_frame %>% 
#     # dplyr::filter(horizon==h,target_variable==target) %>%
#     dplyr::filter(horizon==as.numeric(h)) %>%
#     # dplyr::group_by(horizon,target_variable,model_1) %>%
#     dplyr::group_by(horizon,model_1) %>%
#     # dplyr::mutate(dist_ens = mean_dis[model_2=="COVIDhub-ensemble"]) %>%
#     dplyr::mutate(dist_ens = mean_dis) %>%
#     dplyr::ungroup() %>%
#     dplyr::arrange(desc(dist_ens)) 
#   nc <- length(unique(tmp$model_1))
#   if((nc^2 != nrow(tmp))){
#     stop(paste0("cannot create a square matrix - check filtering for ",h))
#   } 
#   ord <-  tmp$model_1[(1:nrow(tmp) %% nc)==1]
#   mean_cd <- matrix(NA,nc,nc)
#   for(i in 1:nc){
#     for(j in 1:nc){
#       mean_cd[i,j] <- tmp$mean_dis[which(tmp$model_1==ord[i] & tmp$model_2==ord[j])]
#     }
#   }
#   dimnames(mean_cd) <- list(ord,ord)
#   return(mean_cd)
# }
# create matrix
cd_matrix <- function(cd_frame){
  tmp <-cd_frame %>% 
    # dplyr::filter(horizon==as.numeric(h)) %>%
    dplyr::group_by(model_1,model_2) %>%
    dplyr::mutate(dist_ens = mean(approx_cd)) %>%
    dplyr::ungroup() %>%
    dplyr::select(model_1,model_2,dist_ens) %>%
    dplyr::distinct() %>%
    dplyr::arrange(desc(dist_ens)) 
  nc <- length(unique(tmp$model_1))
  if((nc^2 != nrow(tmp))){
    stop(paste0("cannot create a square matrix - check filtering for ",h))
  } 
  # ord <-  tmp$model_1[(1:nrow(tmp) %% nc)==1]
  ord <- unique(tmp$model_1)
  mean_cd <- matrix(NA,nc,nc)
  for(i in 1:nc){
    for(j in 1:nc){
      mean_cd[i,j] <- tmp$dist_ens[which(tmp$model_1==ord[i] & tmp$model_2==ord[j])]
    }
  }
  dimnames(mean_cd) <- list(ord,ord)
  return(mean_cd)
}



sym_mat <- function(X){
  ind1 <- apply(X, 1, function(x) any(is.na(x)))
  ind2 <- apply(X, 2, function(x) any(is.na(x)))
  X_sym <- X[ !ind1, !ind2 ]
  return(as.dist(X_sym))
}

# dendro_plot <- function(horizon, frame_name,metadata,dashed_h){
#   hclust_dat <- hclust(as.dist(get(frame_name)[[horizon]]), 
#                        method = "ward.D", members = NULL)
#   ddata <- label(dendro_data(as.dendrogram(hclust_dat))) %>%
#     left_join(metadata,by=c("label"="model_abbr"))
#   color_set <- ifelse(ddata$compartmental == TRUE, "brown", "blue")
#   dendro_p <- ggdendrogram(
#            hclust(as.dist(get(frame_name)[[horizon]]), 
#                   method = "ward.D",members = NULL),size = 2, rotate=TRUE) +
#            geom_hline(yintercept = dashed_h, color = 2, linetype= 2)+
#            labs(title=paste0("Dendrogram - ",i, " wk ahead inc death"))+
#            xlab("") +
#            ylab("Mean Cramer's Distance") +
#            theme(axis.text.x = element_text(size=5),
#                  axis.text.y = element_text(size=7,colour=color_set),
#                  plot.margin=unit(c(0,0,0,0),"cm"),
#                  plot.title = element_text(size=7)) 
# 
# } 

dendro_plot <- function(cd_mat_h){
  # hclust_dat <- hclust(as.dist(get(frame_name)[[horizon]]), 
  #                      method = "ward.D", members = NULL)
  # ddata <- label(dendro_data(as.dendrogram(hclust_dat))) %>%
  #   left_join(metadata,by=c("label"="model_abbr"))
  ggdendrogram(
    hclust(as.dist(cd_mat_h), 
           method = "ward.D",members = NULL),size = 2, rotate=TRUE) +
    labs(title=paste0("Dendrogram - inc death forecasts"))+
    xlab("") +
    ylab("Mean Cramer's Distance") +
    theme(axis.text.x = element_text(size=10),
          plot.margin=unit(c(0,0,0,0),"cm"),
          plot.title = element_text(size=7)) +
    geom_hline(yintercept=46.21, color="red") 
    
  
} 

dis_ens <- function(cd_list){
  # baseline_dis <- cd_list[[2]] %>%
  #  dplyr::filter(model_1 %in% c("COVIDhub.baseline","COVIDhub.4_week_ensemble"),
  #                model_2 %in% c("COVIDhub.baseline","COVIDhub.4_week_ensemble")) %>%
  #   dplyr::group_by(horizon,location, target_end_date) %>%
  #   dplyr::transmute(base_mean=mean(approx_cd),
  #                    location=location,
  #                    horizon=horizon, 
  #                    target_end_date=target_end_date) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::distinct(.)
  processed_dat <- cd_list[[2]] %>%
  #  dplyr::mutate(model=ifelse(model_1=="COVIDhub.4_week_ensemble",model_2,model_1)) %>%
  # dplyr::filter(model_1 == "COVIDhub.4_week_ensemble" |model_2 == "COVIDhub.4_week_ensemble") %>%
    dplyr::mutate(model=ifelse(model_1=="COVIDhub.baseline",model_2,model_1)) %>%
    dplyr::filter(model_1 == "COVIDhub.baseline" |model_2 == "COVIDhub.baseline") %>%
    dplyr::transmute(mean_cd=approx_cd,
                     location=location,
                     horizon=horizon,
                     model=model,
                     target_end_date= target_end_date) %>%
    distinct() %>%
    # dplyr::left_join(baseline_dis, by=c("location","horizon","target_end_date")) %>%
    dplyr::group_by(horizon,location,target_end_date) %>%
#    dplyr::mutate(mean_scale=mean(mean_cd)/base_mean) %>%
    dplyr::mutate(mean_scale=sd(mean_cd)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(model!="COVIDhub.baseline")
  return(processed_dat)
}

scaled_dis <- function(cd_list){
  baseline_dis <- cd_list[[2]] %>%
    dplyr::filter((model_1=="COVIDhub.baseline" & model_2=="COVIDhub.4_week_ensemble") |
                    (model_1=="COVIDhub.4_week_ensemble" & model_2=="COVIDhub.baseline"))  %>%
    dplyr::group_by(horizon,location, target_end_date) %>%
    dplyr::transmute(base_mean=approx_cd,
                     location=location,
                     horizon=horizon, 
                     target_end_date=target_end_date) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(.)
  processed_dat <- cd_list[[2]] %>%
    dplyr::filter(model_1 != "COVIDhub.baseline",
                  model_2 !="COVIDhub.baseline",
                  model_2=="COVIDhub.4_week_ensemble"|model_1=="COVIDhub.4_week_ensemble") %>%
    dplyr::left_join(baseline_dis, by=c("location","horizon","target_end_date")) %>%
    dplyr::group_by(horizon,location, target_end_date) %>%
    dplyr::transmute(mean_cd=mean(approx_cd/base_mean),
                     location=location,
                     horizon=horizon,
                     target_end_date= target_end_date) %>%
    dplyr::ungroup() %>%
    distinct(.)
  return(processed_dat)
  # return(baseline_dis)
}

type_dis <- function(cd_list,stat_list){
  dat <- cd_list[[2]] %>%
    dplyr::filter(!model_1 %in% c("COVIDhub.4_week_ensemble","JHUAPL.SLPHospEns"),
                  !model_2 %in% c("COVIDhub.4_week_ensemble","JHUAPL.SLPHospEns"))
  type_dat <- data.frame(model=unique(c(dat$model_1,dat$model_2))) %>%
    dplyr::mutate(type=ifelse(model %in% stat_list,
                              "Statistical", "Epidemiological"))
  processed_dat <- dat %>%
    dplyr::left_join(type_dat,by=c("model_1"="model")) %>%
    dplyr::left_join(type_dat,by=c("model_2"="model")) %>%
    dplyr::mutate(pair_type=ifelse(type.x=="Statistical" & type.y=="Statistical", "stats-pair",
                                   ifelse(type.x=="Epidemiological" & type.y=="Epidemiological","epi-pair",
                                          "stats-epi"))) %>%
    dplyr::group_by(horizon,location,pair_type, target_end_date) %>%
    dplyr::transmute(approx_cd=approx_cd,
                     location=location,
                     horizon=horizon,
                     pair_type=pair_type, 
                     target_end_date= target_end_date) %>%
    dplyr::ungroup() %>%
    distinct() 
  return(processed_dat)
}

type_dis_phase <- function(cd_list,stat_list,epi_list){
  type_dat <- data.frame(model=unique(c(cd_list$model_1,cd_list$model_2))) %>%
    dplyr::filter(model %in% c(stat_list,epi_list)) %>%
    dplyr::mutate(type=ifelse(model %in% stat_list,
                              "Statistical", "Mechanistic"))
  processed_dat <- cd_list %>%
    dplyr::filter(model_1 %in% c(stat_list,epi_list) & model_2 %in% c(stat_list,epi_list) ) %>%
    dplyr::left_join(type_dat,by=c("model_1"="model")) %>%
    dplyr::left_join(type_dat,by=c("model_2"="model")) %>%
    dplyr::mutate(pair_type=ifelse(type.x==type.y, "Within-group","Between-group")) %>%
    dplyr::select(approx_cd, location, horizon, pair_type, target_end_date,type.x,type.y)
  return(processed_dat)
}

############----------------------------- heat_map ----------------------------- ############

# a function that takes a matrix and plot a heatmap
# distance_heatmap <- function(sum_dist,h,target,name){
distance_heatmap <- function(sum_dist,name,metadata,loc,overall=TRUE){
  metadata <- metadata %>%
    dplyr::arrange(type) %>%
    dplyr::mutate(acro0=toupper(paste0(substr(model_abbr, 1, 1),substr(model_name, 1, 1))),
                  acro=ifelse(acro0=="MG",toupper(paste0(acro0,substr(model_name, 2, 2))),acro0)) %>%
    dplyr::select(acro,type,model_abbr)
  tmp2 <- sum_dist %>%
    dplyr::filter(location %in% loc) %>%
    dplyr::left_join(metadata,by=c("model_1"="model_abbr")) %>%
    dplyr::select(-"model_1") %>%
    dplyr::mutate(model_1=acro) %>%
    dplyr::select(-"acro") %>%
    dplyr::left_join(metadata,by=c("model_2"="model_abbr")) %>%
    dplyr::select(-"model_2") %>%
    dplyr::mutate(model_2=acro) 
  order_list <- metadata$acro
    set1<-c("blue","brown")
    color_set <- ifelse(metadata$type=="SIR",set1[2],set1[1])
    type_color <-  color_set[order(match(metadata$acro,order_list))]
  if(overall){
    ggplot(tmp2, aes(factor(model_1,levels=order_list),
                     factor(model_2,levels=order_list))) +
      geom_tile(aes(fill= approx_cd1)) +
      facet_grid(location~horizon) +
      theme(axis.text.x=element_text(size=rel(0.5),color = type_color,angle = 90),
            axis.text.y=element_text(size=rel(0.4), color = type_color))+
      labs(title=name)+
      xlab("") +
      ylab("") +
      scale_x_discrete(expand = c(0, 0)) +    
      scale_y_discrete(expand = c(0, 0)) + 
      scale_fill_distiller(palette = "viridis",direction=+1,name="distance") +
      theme(plot.title = element_text(size=8),
            legend.title = element_text(size=5),
            legend.key.size = unit(0.3, 'cm'),
            legend.text = element_text(size=4),
            axis.ticks = element_blank(),     
            plot.margin=unit(c(-0.1,0,0,0),"cm"))
 
  }else{
    for(i in 1:7) {
      assign(paste0("p",i),
             tmp2 %>%
               dplyr::filter(location == loc[i]) %>%
               ggplot(aes(factor(model_1,levels=order_list),
                                      factor(model_2,levels=order_list))) +
               geom_tile(aes(fill= approx_cd1)) +
               facet_grid(location~horizon) +
               theme(axis.text.y=element_text(size=rel(0.4), color = type_color))+
              # labs(title=name)+
               xlab("") +
               ylab("") +
               scale_x_discrete(expand = c(0, 0)) +
               scale_y_discrete(expand = c(0, 0)) +
               scale_fill_distiller(palette = "viridis",direction=+1,name="distance") +
               theme(legend.title = element_text(size=5),
                     legend.key.size = unit(0.3, 'cm'),
                     axis.ticks = element_blank(),
                     strip.text.x = element_text(margin = margin(0.1,0,0.1,0, "cm")),
                     strip.text.y = element_text(margin = margin(0.1,0,0.1,0, "cm")),
                     legend.text = element_text(size=4))
             )
    }

    grid.arrange(p1+theme(plot.margin=unit(c(0,0,-0.5,0),"cm"),
                           axis.text.x=element_blank()),
                 p2+theme(plot.margin=unit(c(-0.4,0,0,0),"cm"),
                          strip.background.x =element_blank(),
                          strip.text.x =element_text(colour = "#00000000"),
                          axis.text.x=element_blank()),
                 p3+theme(plot.margin=unit(c(-1,0,0,0),"cm"),
                          strip.background.x =element_blank(),
                          strip.text.x =element_text(colour = "#00000000"),
                          axis.text.x=element_blank()),
                 p4+theme(plot.margin=unit(c(-1,0,0,0),"cm"),
                          strip.background.x =element_blank(),
                          strip.text.x =element_text(colour = "#00000000"),
                          axis.text.x=element_blank()),
                 p5+theme(plot.margin=unit(c(-1,0,0,0),"cm"),
                          strip.background.x =element_blank(),
                          strip.text.x =element_text(colour = "#00000000"),
                          axis.text.x=element_blank()),
                 p6+theme(plot.margin=unit(c(-1,0,0,0),"cm"),
                          strip.background.x =element_blank(),
                          strip.text.x =element_text(colour = "#00000000"),
                          axis.text.x=element_blank()),
                 p7+theme(plot.margin=unit(c(-1,0,0,0),"cm"),
                          strip.background.x =element_blank(),
                          strip.text.x =element_text(colour = "#00000000"),
                          axis.text.x=element_text(size=rel(0.5), colour = type_color,angle = 90)),
                 nrow=7,top=name)
  }
}

############----------------------------- calculate rank of most similar by week ----------------------------- ############
# write a function to calculate rank for weekly forecast 
rank_cd <- function(cd_frame){
  tmp <-cd_frame %>% 
    # group by first model to rank that vs the rest
    dplyr::group_by(model_1,target_end_date,horizon,location) %>%
    dplyr::mutate(dist_rank = rank(approx_cd)) %>%
    dplyr::ungroup() %>%
    # group by model to calculate mean rank
    dplyr::group_by(model_1,target_end_date,horizon,location) %>%
    dplyr::mutate(dist_rank = rank(approx_cd)) %>%
    dplyr::ungroup() %>%
    # select
    dplyr::select(model_1,model_2,dist_rank) %>%
    dplyr::distinct() %>%
    dplyr::arrange(desc(dist_ens)) 
  return(rank_frame)
}

# write a function to take the output of the first function as input and average and do dendogram?

############----------------------------- perm test ----------------------------- ############
# 
perm_test <- function(perm_ord,dis_dat){
  # locs <- unique(dis_dat$location)
  h <- c(2,4)
  # ted <- unique(dis_dat$target_end_date)
  pnum <- 3432 # number of poss group perms is 14 choose 7
  r_perm <- matrix(NA,ncol=2,nrow=pnum)
  # get group and reattach
  for(i in 1:pnum){
    mset <- perm_ord %>%
      dplyr::filter(group_p==i)
    s_list <- mset$model[mset$type=="stats"]
    for (j in 1:2){
      dist_frame <- dis_dat %>%
        dplyr::filter(horizon==h[j]) %>%
        dplyr::mutate(model1_type=ifelse(model_1 %in% s_list,"stats","mech"),
                      model2_type=ifelse(model_2 %in% s_list,"stats","mech"),
                      pair_type=ifelse(model1_type==model2_type,"within","between"))
      mean_ratio <- mean(dist_frame$approx_cd[dist_frame$pair_type=="between"])/mean(dist_frame$approx_cd[dist_frame$pair_type=="within"])
      r_perm[i,j] <- mean_ratio
    }
  }
  r_perm2 <- c()
  for (k in 1:2){
    dist_frame_t <- dis_dat %>%
      dplyr::filter(horizon==h[k]) %>%
      dplyr::mutate(pair_type=ifelse(model1_type==model2_type,"within","between"))
    mean_ratio2 <- mean(dist_frame_t$approx_cd[dist_frame_t$pair_type=="between"])/mean(dist_frame_t$approx_cd[dist_frame_t$pair_type=="within"])
    r_perm2 <- c(r_perm2,mean_ratio2)
  }
  # get ratio from permuted data
  rdat <- data.frame(r_perm)
  names(rdat) <- c("h2","h4")
  rdat$h2_real <- r_perm2[1]
  rdat$h4_real <- r_perm2[2]
  return(rdat)
}

perm_test2 <- function(perm_ord,dis_dat){
  N <- 7
  g <- 2
  # locs <- unique(dis_dat$location)
  h <- c(2,4)
  # ted <- unique(dis_dat$target_end_date)
  pnum <- 3432 # number of poss group perms is 14 choose 7
  r_perm <- matrix(NA,ncol=2,nrow=pnum)
  # get group and reattach
  for(i in 1:pnum){
    mset <- perm_ord %>%
      dplyr::filter(group_p==i)
    s_list <- mset$model[mset$type=="stats"]
    for (j in 1:2){
      dist_frame <- dis_dat %>%
        dplyr::filter(horizon==h[j]) %>%
        dplyr::mutate(model1_type=ifelse(model_1 %in% s_list,"stats","mech"),
                      model2_type=ifelse(model_2 %in% s_list,"stats","mech"),
                      pair_type=ifelse(model1_type==model2_type,"within","between"))
      mean_ratio <- sum(dist_frame$approx_cd[dist_frame$pair_type=="between"])/sum(dist_frame$approx_cd[dist_frame$pair_type=="within"])
      r_perm[i,j] <- mean_ratio
    }
  }
  r_perm2 <- c()
  for (k in 1:2){
    dist_frame_t <- dis_dat %>%
      dplyr::filter(horizon==h[k]) %>%
      dplyr::mutate(pair_type=ifelse(model1_type==model2_type,"within","between"))
    mean_ratio2 <- sum(dist_frame_t$approx_cd[dist_frame_t$pair_type=="between"])/sum(dist_frame_t$approx_cd[dist_frame_t$pair_type=="within"])
    r_perm2 <- c(r_perm2,mean_ratio2)
  }
  # get ratio from permuted data
  rdat <- data.frame(r_perm)
  names(rdat) <- c("h2","h4")
  rdat$h2_real <- r_perm2[1]
  rdat$h4_real <- r_perm2[2]
  return(rdat)
}

perm_test2l <- function(perm_ord,dis_dat){
  # do it by state
  N <- 7
  g <- 2
  h <- c(2,4)
  pnum <- 3432 
  r_perm <- matrix(NA,ncol=2,nrow=pnum)
  # get group and reattach
  for(i in 1:pnum){
    mset <- perm_ord %>%
      dplyr::filter(group_p==i)
    s_list <- mset$model[mset$type=="stats"]
    for (j in 1:2){
      dist_frame <- dis_dat %>%
        dplyr::filter(horizon==h[j]) %>%
        dplyr::mutate(model1_type=ifelse(model_1 %in% s_list,"stats","mech"),
                      model2_type=ifelse(model_2 %in% s_list,"stats","mech"),
                      pair_type=ifelse(model1_type==model2_type,"within","between"))
      between_mean <- mean(dist_frame$approx_cd[dist_frame$pair_type=="between"])
      within_mean <- mean(dist_frame$approx_cd[dist_frame$pair_type=="within"])
      cons <- 1/((N^2)*(N-1))
      mean_ratio <- (between_mean-within_mean)/(cons*sum(dist_frame$approx_cd[dist_frame$pair_type=="within"]))
      r_perm[i,j] <- mean_ratio
    }
  }
  r_perm2 <- c()
  for (k in 1:2){
    dist_frame_t <- dis_dat %>%
      dplyr::filter(horizon==h[k]) %>%
      dplyr::mutate(pair_type=ifelse(model1_type==model2_type,"within","between"))
    between_mean1 <- mean(dist_frame_t$approx_cd[dist_frame_t$pair_type=="between"])
    within_mean1 <- mean(dist_frame_t$approx_cd[dist_frame_t$pair_type=="within"])
    cons1 <- 1/((N^2)*(N-1))
    mean_ratio2 <- (between_mean1-within_mean1)/(cons1*sum(dist_frame_t$approx_cd[dist_frame_t$pair_type=="within"]))
    r_perm2 <- c(r_perm2,mean_ratio2)
  }
  # get ratio from permuted data
  rdat <- data.frame(r_perm)
  names(rdat) <- c("h2","h4")
  rdat$h2_real <- r_perm2[1]
  rdat$h4_real <- r_perm2[2]
  return(rdat)
}

perm_test2ls <- function(perm_ord,dis_dat){
  # do it by state
  N <- 7
  g <- 2
  h <- c(2,4)
  pnum <- 3432 
  r_perm <- matrix(NA,ncol=2,nrow=pnum)
  # get group and reattach
  for(i in 1:pnum){
    mset <- perm_ord %>%
      dplyr::filter(group_p==i)
    s_list <- mset$model[mset$type=="stats"]
    for (j in 1:2){
      dist_frame <- dis_dat %>%
        dplyr::filter(horizon==h[j]) %>%
        dplyr::mutate(model1_type=ifelse(model_1 %in% s_list,"stats","mech"),
                      model2_type=ifelse(model_2 %in% s_list,"stats","mech"),
                      pair_type=ifelse(model1_type==model2_type,"within","between"))
      between_mean <- mean(dist_frame$approx_cd[dist_frame$pair_type=="between"])
      within_mean <- mean(dist_frame$approx_cd[dist_frame$pair_type=="within"])
      cons <- 1/((N^2)*(N-1))
      mean_ratio <- (between_mean-within_mean)/(cons*sum(dist_frame$approx_cd[dist_frame$pair_type=="within"]))
      r_perm[i,j] <- mean_ratio
    }
  }
  r_perm2 <- c()
  for (k in 1:2){
    dist_frame_t <- dis_dat %>%
      dplyr::filter(horizon==h[k]) %>%
      dplyr::mutate(pair_type=ifelse(model1_type==model2_type,"within","between"))
    between_mean1 <- mean(dist_frame_t$approx_cd[dist_frame_t$pair_type=="between"])
    within_mean1 <- mean(dist_frame_t$approx_cd[dist_frame_t$pair_type=="within"])
    cons1 <- 1/((N^2)*(N-1))
    mean_ratio2 <- (between_mean1-within_mean1)/(cons1*sum(dist_frame_t$approx_cd[dist_frame_t$pair_type=="within"]))
    r_perm2 <- c(r_perm2,mean_ratio2)
  }
  # get ratio from permuted data
  rdat <- data.frame(r_perm)
  names(rdat) <- c("h2","h4")
  rdat$h2_real <- r_perm2[1]
  rdat$h4_real <- r_perm2[2]
  return(rdat)
}
############----------------------------- CI width and median func  ----------------------------- ############
# fix to get median diff and 50 and 80% width diff
other_diff <- function(single_tarloc_frame, tau_F,tau_G){
    # get model names with no forecasts
    missing <- single_tarloc_frame %>%
      dplyr::select_if(~ any(is.na(.)))
    # single_tarloc_frame[ , colSums(is.na(single_tarloc_frame)) > 0] %>%
    # dplyr::select(-c("target_variable","target_end_date","location","type","quantile","horizon"))
    missing_names <- colnames(missing)
    missing_cols <- ncol(missing)
    # start calculation
    # remove any models with NA for values for this target location (assuming all models have the same quantiles)
    single_tarloc<- single_tarloc_frame[ , colSums(is.na(single_tarloc_frame)) == 0]
    # pairwise column calculation
    tmp <- single_tarloc %>%
      dplyr::select(-c("target_variable","target_end_date","location","type","quantile","horizon"))
    nc <- ncol(tmp)
    cnames <- colnames(tmp)
    eg <- expand.grid(1:nc, 1:nc)
    nr <- nrow(eg)
    v1 <- v2 <- v3 <- vector(length=nr)
    for (i in 1:nr) {
      # median
      cc1 <- abs((as.numeric(unlist(tmp[,eg[i,1]]))[which(as.numeric(tau_F)==0.5)])-
                   (as.numeric(unlist(tmp[,eg[i,2]]))[which(as.numeric(tau_G)==0.5)]))
      v1[i] <- cc1
      # 50 ci width diff
      l50_f <- as.numeric(unlist(tmp[,eg[i,1]]))[which(as.numeric(tau_F)==0.25)]
      u50_f <- as.numeric(unlist(tmp[,eg[i,1]]))[which(as.numeric(tau_F)==0.75)]
      l50_g <- as.numeric(unlist(tmp[,eg[i,2]]))[which(as.numeric(tau_G)==0.25)]
      u50_g <- as.numeric(unlist(tmp[,eg[i,2]]))[which(as.numeric(tau_G)==0.75)]
      cc2 <- abs((u50_f-l50_f)-(u50_g-l50_g))
      v2[i] <- cc2
      # 80 ci width diff
      l80_f <- as.numeric(unlist(tmp[,eg[i,1]]))[which(as.numeric(tau_F)==0.1)]
      u80_f <- as.numeric(unlist(tmp[,eg[i,1]]))[which(as.numeric(tau_F)==0.9)]
      l80_g <- as.numeric(unlist(tmp[,eg[i,2]]))[which(as.numeric(tau_G)==0.1)]
      u80_g <- as.numeric(unlist(tmp[,eg[i,2]]))[which(as.numeric(tau_G)==0.9)]
      cc3 <- abs((u80_f-l80_f)-(u80_g-l80_g))
      v3[i] <- cc3
    }
    single_tarloc_cvm <- data.frame(model_1=rep(cnames,nc),
                                    model_2=rep(cnames,each=nc),
                                    med_diff=v1,
                                    ci50_diff=v2,
                                    ci80_diff=v3
                                    ) 
    if(nrow(missing)!=0){
      # missing table
      missing_tab <- data.frame(model_1=rep(missing_names,sum(missing_cols,nc)),
                                model_2=rep(c(missing_names,cnames),each=missing_cols),
                                med_diff=rep(NA,sum(missing_cols,nc)*missing_cols),
                                ci50_diff=rep(NA,sum(missing_cols,nc)*missing_cols),
                                ci80_diff=rep(NA,sum(missing_cols,nc)*missing_cols)
                                )
      # combine
      single_tarloc_cvm <- rbind(single_tarloc_cvm,missing_tab)
    } 
    return(single_tarloc_cvm)
  
}


build_oth_measure_frame <- function(model_dataframe, horizon_list,target_list, 
                                   approx_rule,tau_F,tau_G, truth, scale=FALSE){
    library(tidyverse)
    library(tidyr)
    # remove point estimates and forecast_date
    truth <- truth %>%
      dplyr::transmute(tar_date = as.Date(target_end_date),
                       location = as.character(location),
                       value = value) 
    main_frame <- model_dataframe %>% 
      dplyr::mutate(horizon = as.numeric(as.character(horizon)),
                    target_variable = as.character(target_variable),
                    location = as.character(location), 
                    target_end_date = as.Date(target_end_date)) %>%
      dplyr::filter(target_variable %in% target_list,
                    horizon %in% horizon_list) 
    
    # dist_frame <- data.frame()
    ncols <- ncol(main_frame)
    if(scale){
      main_frame <- main_frame %>% 
        dplyr::mutate(for_date = as.Date(floor_date(target_end_date-(7*horizon), "week")-1)) %>%
        dplyr::left_join(truth, 
                         by=c("location", "for_date" = "tar_date")) %>%
        dplyr::select(-for_date)
      main_frame <-cbind(main_frame[,c(1:6)],main_frame[,c(7:ncols)]/main_frame$value)
    } 
    if("forecast_date" %in% c(colnames(main_frame))){
      main_frame <- main_frame %>%
        dplyr::select(-"forecast_date")
    }
    ## apply distance_combination function
    locations <- unique(main_frame$location)
    dist_frame <- map_dfr(locations,
                          function(loc) {
                            map_dfr(target_list,
                                    function(target) {
                                      map_dfr(horizon_list,
                                              function(horiz) {
                                                tar_loc <- main_frame %>%
                                                  dplyr::filter(location==loc,
                                                                target_variable==target,
                                                                horizon==horiz)
                                                end_dates <- unique(tar_loc$target_end_date)
                                                map_dfr(end_dates,
                                                        function(end_date) {
                                                          tar_loc %>%
                                                            dplyr::filter(target_end_date==end_date) %>%
                                                            dplyr::arrange(quantile) %>%
                                                            dplyr::select_if(~ !any(is.na(.))) %>%
                                                            other_diff(., tau_F,tau_G) %>%
                                                            dplyr::mutate(horizon=horiz,
                                                                          location=loc,
                                                                          target_variable=target,
                                                                          target_end_date=end_date)
                                                        })
                                              })
                                    })
                          })
    
    # non_duplicate
    non_dup <- na.omit(dist_frame) %>%
      dplyr::filter(model_1 != model_2) %>%
      rowwise() %>%
      mutate(key = paste(sort(c(model_1, model_2)), collapse="")) %>%
      distinct(key, med_diff,ci50_diff,ci80_diff, horizon, location, target_end_date, .keep_all=T) %>%
      select(-key) %>%
      dplyr::group_by(horizon,location,target_variable,model_1,model_2) %>%
      dplyr::mutate(n_dates=n_distinct(target_end_date)) %>%
      dplyr::ungroup()
    return(list(full_dataframe=dist_frame,non_dup = non_dup))
  }