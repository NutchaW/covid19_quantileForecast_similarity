# permutation
library(gtools)
library(combinat)
library(tidyverse)
library(covidHubUtils)
library(iterpc)
source("./functions/sim_functions.R")
set.seed(1234) #for reproducability
# load data
death_dat_test_long <- read.csv("./data/long_df_test.csv") 
death_dat_test_long_p <- read.csv("./data/long_df_test_post.csv") 
# define stats models
s_list <- c("CEID-Walk","CMU-TimeSeries","COVIDhub-baseline","GT-DeepCOVID","JHU_CSSE-DECOM",
            "Microsoft-DeepSTIA","MIT_CritData-GBCF","MIT_ISOLAT-Mixtures","MUNI-ARIMA",
            "RobertWalraven-ESG","UCSB-ACTS","UMich-RidgeTfReg","GT-DeepCOVID","IHME-CurveFit",
            "DDS-NBDS","UT-Mobility","USC-SI_kJalpha")
# medata
model_names <- str_replace(unique(death_dat_test_long$model),"\\.","-")
metadat <- read.csv("./data/metadata.csv") %>%
  dplyr::filter(model_abbr %in% model_names)
meta <- metadat %>%
  dplyr::mutate(type=ifelse(model_abbr %in% s_list,"non_SIR","SIR"),
                model_abbr=str_replace(model_abbr,"\\-",".")) 
types <- meta$type
meta_p <- meta %>%
  dplyr::select(model_abbr,type) 
types_p <- paste0(types,1:length(types))
# get numbers
nG <- 2  #number of groups
nJ1 <- sum(types=="non_SIR")  #cluster size for stats/ML models (group size)
nJ2 <- sum(types=="SIR")  #cluster size (group size)
nL <- length(unique(death_dat_test_long$location)) # num location 
q_set <- unique(death_dat_test_long$quantile) 
# data
test_dat0 <- death_dat_test_long %>%
  dplyr::select(c("model","location","horizon","quantile","value")) %>%
  dplyr::mutate(model=str_replace(model,"\\-",".")) %>%
  dplyr::left_join(meta_p, by=c("model"="model_abbr")) %>%
  dplyr::mutate(group_vec=type) %>% 
  dplyr::select(-"type") %>%
  dplyr::arrange(location, horizon, group_vec, model, quantile)
test_datp0 <- death_dat_test_long_p %>%
  dplyr::select(c("model","location","horizon","quantile","value")) %>%
  dplyr::mutate(model=str_replace(model,"\\-",".")) %>%
  dplyr::left_join(meta_p, by=c("model"="model_abbr")) %>%
  dplyr::mutate(group_vec=type) %>% 
  dplyr::select(-"type") %>%
  dplyr::arrange(location, horizon, group_vec, model, quantile)
# create full permutation
# save for sim
# types_ord <- sort(types)
types_ord <- c(paste0(sort(types)[1:nJ1],1:nJ1),paste0(sort(types)[(nJ1+1):(nJ1+nJ2)],1:nJ2))
reord_types0 <- combinations((nJ1+nJ2), nJ1, v = types_ord, set = TRUE, repeats.allowed = FALSE)
getpair <- function(x){return(types_ord[which(!types_ord %in% x)])}
reord_types1 <- t(apply(reord_types0,1,getpair))
# reord_types2 <- as.matrix(cbind(reord_types0,reord_types1),ncol=12)
reord_types2 <- gsub("[0-9]", "",as.matrix(cbind(reord_types0,reord_types1),ncol=12))
B <- nrow(reord_types2)
# permutation
for(h in c(1,4)){
  test_dat <- test_dat0 %>%
    dplyr::filter(horizon==h)%>%
    dplyr::select(-"horizon")
  test_datp <- test_datp0 %>%
    dplyr::filter(horizon==h)%>%
    dplyr::select(-"horizon")
  # matrices for group ratio
  v1 <- v1p <- matrix(NA,
               nrow=((factorial(nJ1)/(factorial(2)*factorial(nJ1-2)))+(factorial(nJ2)/(factorial(2)*factorial(nJ2-2))))*(nL),
               ncol=B)
  v2 <- v2p <- matrix(NA,nrow=(nJ1*nJ2)*(nL),ncol=B)
  # matrices for group median
  g2 <- g2p <- matrix(NA,
                      nrow=(factorial(nJ1)/(factorial(2)*factorial(nJ1-2)))*(nL),
                      ncol=B)
  g1 <- g1p <- matrix(NA,
                      nrow=(factorial(nJ2)/(factorial(2)*factorial(nJ2-2)))*(nL),
                      ncol=B)
  for(b in 1:B){
    shuf_ind <- sample(types,length(types),replace = FALSE)
    test_dat <- test_dat %>%
      dplyr::group_by(location) %>%
      dplyr::mutate(group_vec=rep(rep(reord_types2[b,],each=length(q_set)))) %>%
      dplyr::ungroup()
    test_datp <- test_datp %>%
      dplyr::group_by(location) %>%
      dplyr::mutate(group_vec=rep(rep(reord_types2[b,],each=length(q_set)))) %>%
      dplyr::ungroup()
    datd <- build_dist_frame(test_dat,"left_sided_riemann",q_set)
    datdp <- build_dist_frame(test_datp,"left_sided_riemann",q_set)
    v1[,b] <- datd$approx_cd[datd$type=="within"]  
    v2[,b] <- datd$approx_cd[datd$type=="between"] 
    v1p[,b] <- datdp$approx_cd[datdp$type=="within"]  
    v2p[,b] <- datdp$approx_cd[datdp$type=="between"] 
    # group compare
    g1[,b] <- datd$approx_cd[datd$type=="within" & datd$group1=="SIR"]  
    g2[,b] <- datd$approx_cd[datd$type=="within" & datd$group1=="non_SIR"] 
    g1p[,b] <- datdp$approx_cd[datd$type=="within" & datd$group1=="SIR"]  
    g2p[,b] <- datdp$approx_cd[datd$type=="within" & datd$group1=="non_SIR"] 
  }
  assign(paste0("v_",h), 
         data.frame(cbind(
           rbind(v1,v2),c(rep("within",nrow(v1)),rep("between",nrow(v2)))
           )
                    )
         )
  assign(paste0("vp_",h), 
         data.frame(cbind(
           rbind(v1p,v2p),c(rep("within",nrow(v1p)),rep("between",nrow(v2p)))
         )
         )
  )
  assign(paste0("g_",h),
         data.frame(cbind(
           rbind(g1,g2),c(rep("SIR",nrow(g1)),rep("non_SIR",nrow(g2)))
         )
         )
  )
  assign(paste0("gp_",h),
         data.frame(cbind(
           rbind(g1p,g2p),c(rep("SIR",nrow(g1p)),rep("non_SIR",nrow(g2p)))
         )
         )
  )
}

# save observed and permutation test stats
write_csv(v_1,file="./data/analysis_data/perm_test_stats_h1.csv")
write_csv(v_4,file="./data/analysis_data/perm_test_stats_h4.csv")
write_csv(vp_1,file="./data/analysis_data/perm_test_stats_h1_p.csv")
write_csv(vp_4,file="./data/analysis_data/perm_test_stats_h4_p.csv")
# save mech stat
write_csv(g_1,file="./data/analysis_data/perm_test_stats_h1_g.csv")
write_csv(g_4,file="./data/analysis_data/perm_test_stats_h4_g.csv")
write_csv(gp_1,file="./data/analysis_data/perm_test_stats_h1_p_g.csv")
write_csv(gp_4,file="./data/analysis_data/perm_test_stats_h4_p_g.csv")