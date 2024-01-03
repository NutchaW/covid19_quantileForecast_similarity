# <!-- ```{r,include=FALSE} -->
#   <!-- d1 <- distance_heatmap(pairwise_hm2,"",meta1,locs_short[1:8],FALSE) -->
#     <!-- d2 <- distance_heatmap(pairwise_hm2,"",meta1,locs_short[9:15],FALSE) -->
#       <!-- ``` -->
#       
#       <!-- ```{r heat2,fig.align='center',out.width='95%',out.height='90%'} -->
#       <!-- grid.arrange(d1,d2,ncol=2, -->
#                           <!--              top="Approx. CD of Inc Death Forecasts by Horizon-Location - Summer/Fall 2021 Wave") -->
#       <!-- ``` -->
# The plots are by model acronyms. Blue are stats/ML and Orange are mechanistic models. Karlen-pypm seems to be dissimilar from other models for all 14 locations. 
# Looking at this by quadrants, there seems to be more dissimilarity among mechanistic models than among stats/ML models. There are some dissimilarity between groups, but not as strong as the first relationship.

# <!-- ## Cross-correlations -->
# 
#   <!-- ### Forecast inclusion criteria -->
# 
#   <!-- * Targets: 1-4 wk ahead inc death -->
#   <!-- * Target End Dates: Jan-Dec 2021 with all 4 horizons. -->
#   <!-- * Model: all models submitted on a particular date  -->
#   <!-- * Probability levels: All  -->
#   <!-- * Locations: All except US -->
# 
#   <!-- ```{r} -->
#   <!-- # data for test -->
#   <!-- scale_ov_dis3 <- dist_gen_norepeat %>% -->
#     <!--   dplyr::mutate(date = (as.Date(target_end_date)+2)-(7*horizon), -->
#                            <!--                 location=as.character(location), -->
#                            <!--                 horizon=as.character(horizon)) %>% -->
#     <!--   dplyr::filter(target_end_date >= "2021-01-01", -->
#                            <!--                 target_end_date <= "2021-12-31") %>% -->
#     <!--   dplyr::group_by(date) %>% -->
#     <!--   dplyr::filter(n_distinct(horizon)==4) %>% -->
#     <!--   dplyr::ungroup() %>% -->
#     <!--   dplyr::select(approx_cd,horizon,location,date) -->
#     <!-- ## calculation and plots -->
#     <!-- library(astsa) -->
#     <!-- dis2 <- scale_ov_dis3 %>%  -->
#       <!--   dplyr::filter(location != "US") %>% -->
#       <!--   dplyr::group_by(location, horizon, date) %>% -->
#       <!--   dplyr::mutate(med_cd=median(approx_cd)) %>% -->
#       <!--   dplyr::ungroup() %>% -->
#       <!--   dplyr::select(-"approx_cd") %>% -->
#       <!--   distinct() -->
#       <!-- ccf_list1 <- ccf_list2 <- ccf_list3 <- ccf_list4 <-  list() -->
#         <!-- for(j in 1:length(unique(dis2$location))){ -->
#             <!--   loc <- unique(dis2$location)[j] -->
#               <!--   truth_d <- truth_death_hr %>%  -->
#                 <!--     dplyr::filter(location == loc) %>% -->
#                 <!--     dplyr::arrange(date) %>% -->
#                 <!--     dplyr::select(approx_cd) %>% -->
#                 <!--     ts(.) -->
#                 <!--   for(i in 1:4){ -->
#                     <!--     assign(paste0("dis_",i), -->
#                                       <!--            dis2 %>% -->
#                                       <!--               dplyr::filter(location==loc, -->
#                                                                          <!--                             horizon==i) %>% -->
#                                       <!--               dplyr::arrange(date) %>% -->
#                                       <!--               dplyr::select(med_cd) %>% -->
#                                       <!--               ts(.) -->
#                                       <!--            ) -->
#                     <!--   } -->
#                 <!--   # only plot for some -->
#                 <!--   if(loc %in% locs_short[c(1,3,8,11)]){ -->
#                     <!--   lag2.plot(dis_1[,1], truth_d[,1], 3)[1] -->
#                     <!--   lag2.plot(dis_2[,1], truth_d[,1], 3)[1] -->
#                     <!--   lag2.plot(dis_3[,1], truth_d[,1], 3)[1] -->
#                     <!--   lag2.plot(dis_4[,1], truth_d[,1], 3)[1] -->
#                     <!--   } -->
#                 <!--   ccf_list1[[j]] <- ccf(dis_1[,1], truth_d[,1],3) -->
#                   <!--   ccf_list2[[j]] <- ccf(dis_2[,1], truth_d[,1],3) -->
#                     <!--   ccf_list3[[j]] <- ccf(dis_3[,1], truth_d[,1],3) -->
#                       <!--   ccf_list4[[j]] <- ccf(dis_4[,1], truth_d[,1],3) -->
#                         <!-- } -->
#         <!-- ``` -->

# # build growth data
# gdat <- rbind(scale_ov2_p %>%
#           dplyr::filter(horizon=="incident death") %>% 
#           dplyr::mutate(horizon="1",type="change rate \nin truth (%)"),
#           scale_ov2_p %>%
#           dplyr::filter(horizon=="incident death") %>% 
#           dplyr::mutate(horizon="2",type="change rate \nin truth (%)"),
#           scale_ov2_p %>%
#           dplyr::filter(horizon=="incident death") %>% 
#           dplyr::mutate(horizon="3",type="change rate \nin truth (%)"),
#           scale_ov2_p %>%
#           dplyr::filter(horizon=="incident death") %>% 
#           dplyr::mutate(horizon="4",type="change rate \nin truth (%)"),
#           scale_ov2_f %>%
#           dplyr::group_by(horizon,location,date) %>% 
#           dplyr::mutate(type="change rate in \nmedian CD (%)",
#                         approx_cd=median(approx_cd)) %>%
#           dplyr::ungroup() %>%
#           distinct()) %>%
#   dplyr::mutate(log_cd=ifelse(approx_cd==0,log(approx_cd+0.0001),log(approx_cd))) %>%
#   dplyr::arrange(location,horizon,date) %>%
#   dplyr::group_by(horizon,location,type) %>%
#   dplyr::mutate(
#     log_lag=ifelse(type=="change rate \nin truth (%)" & lag(log_cd,unique(as.numeric(horizon)))==0,
#                    log(0.0001),
#                    lag(log_cd,unique(as.numeric(horizon)))),
#     cd_lag=ifelse(type=="change rate \nin truth (%)" & lag(approx_cd,unique(as.numeric(horizon)))==0,
#                    0.0001,
#                    lag(approx_cd,unique(as.numeric(horizon)))),
#     growth = (approx_cd - cd_lag)/cd_lag,
#     # d/dt log[median CD(t)] - d/dt log[deaths(t)]
#     growth_log = (log_cd - log_lag)/log_lag
#     ) %>%
#     dplyr::ungroup() %>%
#   dplyr::group_by(horizon,location,date) %>%
#   dplyr::mutate(ratio=growth[type!="change rate \nin truth (%)"]/growth[type=="change rate \nin truth (%)"],
#                 diff=growth_log[type!="change rate \nin truth (%)"]-growth_log[type=="change rate \nin truth (%)"]
#                 ) %>%
#   dplyr::ungroup()
# plot
# mplot2 <- gdat %>%
#   ggplot()+
#   geom_line(aes(x=date,y=growth_log,color=horizon,linetype=type),
#             show.legend=FALSE)+
#   scale_color_brewer(palette = "Dark2") +
#   facet_grid(type~location,scales="free_y")+
#   ylab("")+
#   xlab("Forecast date")+
#   scale_x_date(date_breaks="1 months",date_labels="%b-%y")+
#   scale_color_brewer(palette = "Dark2") +
#   theme_bw()+
#   theme(axis.text.x=element_text(size=rel(0.5),angle = -40,hjust = -0.1),
#         axis.title.x = element_text(size=rel(0.8)),
#         axis.title.y = element_text(size=rel(0.8)),
#         strip.text.x =element_text(margin = margin(0.1,0,0.1,0, "cm"),colour = "#00000000"),
#         strip.background.x = element_blank(),
#         strip.text.y =element_text(size=rel(0.7)),
#         axis.text.y=element_text(size=rel(0.5)),
#         legend.text = element_text(size=rel(0.4)),
#         legend.title = element_text(size=rel(0.5)),
#         legend.position = "bottom",
#         legend.margin=unit(-0.1, "cm"),
#         plot.margin = unit(c(-0.4,0.1,0.1,-0.2), "cm")
#         )

ratiop <- ggplot()+
  geom_line(data=scale_ov2_p %>% 
              dplyr::filter(horizon=="incident death")  %>% 
              dplyr::mutate(type="incident death"),
            aes(x=date,y=approx_cd),
            show.legend=FALSE)+
  geom_line(data=scale_ov2_p %>% 
              dplyr::filter(horizon=="incident death")  %>% 
              dplyr::group_by(location) %>%
              dplyr::mutate(approx_cd=(approx_cd-min(approx_cd))/(max(approx_cd)-min(approx_cd)),
                            type="scaled death") %>%
              dplyr::ungroup(),
            aes(x=date,y=approx_cd),
            show.legend=FALSE)+
  geom_line(data=gdat %>% 
              dplyr::select(-c("type","approx_cd","growth"))  %>% 
              distinct() %>% 
              dplyr::mutate(type="ratio of change rate"),
            aes(x=date,y=ratio,color=horizon))+
  geom_line(data=gdat %>% 
              dplyr::select(-c("type","approx_cd","growth"))  %>% 
              distinct() %>% 
              dplyr::mutate(type="difference in % change \nof log of cd"),
            aes(x=date,y=diff,color=horizon))+
  scale_color_brewer(palette = "Dark2") +
  facet_grid(type~location,scales="free_y")+
  ylab("")+
  xlab("Forecast date")+
  scale_x_date(date_breaks="1 months",date_labels="%b-%y")+
  scale_color_brewer(palette = "Dark2") +
  theme_bw()+
  theme(axis.text.x=element_text(size=rel(0.5),angle = -40,hjust = -0.1),
        axis.title.x = element_text(size=rel(0.8)),
        axis.title.y = element_text(size=rel(0.8)),
        strip.text.x =element_text(margin = margin(0.1,0,0.1,0, "cm")),
        strip.text.y =element_text(size=rel(0.7)),
        axis.text.y=element_text(size=rel(0.5)),
        legend.text = element_text(size=rel(0.4)),
        legend.title = element_text(size=rel(0.5)),
        legend.position = "bottom",
        legend.margin=unit(-0.1, "cm"),
        plot.margin = unit(c(0.1,0.1,0.1,-0.2), "cm")
  )


# read permutation test statistics
pt_h1 <- read.csv("./data/analysis_data/perm_test_stats_h1.csv")
pt_h4 <- read.csv("./data/analysis_data/perm_test_stats_h4.csv")
# pt_h1p <- read.csv("./data/analysis_data/perm_test_stats_h1_p.csv")
# pt_h4p <- read.csv("./data/analysis_data/perm_test_stats_h4_p.csv")
# test group
pt_h1g <- read.csv("./data/analysis_data/perm_test_stats_h1_g.csv")
pt_h4g <- read.csv("./data/analysis_data/perm_test_stats_h4_g.csv")
# pt_h1gp <- read.csv("./data/analysis_data/perm_test_stats_h1_p_g.csv")
# pt_h4gp <- read.csv("./data/analysis_data/perm_test_stats_h4_p_g.csv")
# calculate observed test stats
obs_t <- dist_test_norepeat %>%
  dplyr::left_join(meta1, by=c("model_1"="model_abbr")) %>%
  dplyr::left_join(meta1, by=c("model_2"="model_abbr")) %>%
  dplyr::mutate(type=ifelse(type.x==type.y,"within","between")) %>%
  drop_na() %>%
  dplyr::group_by(horizon,type) %>%
  dplyr::transmute(horizon=horizon,
                   type=type,
                   med_stats=median(approx_cd)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct() %>%
  dplyr::group_by(horizon) %>%
  dplyr::transmute(horizon=horizon,
                   med_ratio1=med_stats[type=="between"]/med_stats[type=="within"]) %>%
  dplyr::ungroup() %>%
  dplyr::distinct()
obs_tg <- dist_test_norepeat %>%
  dplyr::left_join(meta1, by=c("model_1"="model_abbr")) %>%
  dplyr::left_join(meta1, by=c("model_2"="model_abbr")) %>%
  dplyr::mutate(type=ifelse(type.x==type.y,"within","between")) %>%
  dplyr::group_by(horizon,type) %>%
  dplyr::transmute(horizon=horizon,
                   type=type,
                   type1=type.x,
                   type2=type.y,
                   approx_cd=approx_cd) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(horizon,type1,type) %>%
  dplyr::mutate(med_stats2=median(approx_cd)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-"approx_cd") %>%
  dplyr::distinct() %>%
  drop_na() %>%
  dplyr::group_by(horizon) %>%
  dplyr::transmute(horizon=horizon,
                   med_ratio1g=med_stats2[type=="within" & type1=="SIR"]/med_stats2[type=="within" & type1=="non_SIR"]) %>%
  dplyr::ungroup() %>%
  dplyr::distinct()
# calculate observed test stats
# obs_tp <- dist_test_norepeat_p %>%
#   dplyr::left_join(meta_p, by=c("model_1"="model_abbr")) %>%
#   dplyr::left_join(meta_p, by=c("model_2"="model_abbr")) %>%
#   dplyr::mutate(type=ifelse(type.x==type.y,"within","between")) %>%
#   dplyr::group_by(horizon,type) %>%
#   dplyr::transmute(horizon=horizon,
#                 type=type,
#                 type1=type.x,
#                 type2=type.y,
#                 med_stats=median(approx_cd)) %>%
#   dplyr::ungroup() %>%
#   dplyr::group_by(horizon,type1,type) %>%
#   dplyr::mutate(med_stats2=median(approx_cd)) %>%
#   dplyr::ungroup() %>%
#   dplyr::distinct() %>%
#   dplyr::group_by(horizon) %>%
#   dplyr::transmute(horizon=horizon,
#                    med_ratio2=med_stats[type=="between"]/med_stats[type=="within"],
#                    med_ratio2g=med_stats2[type=="within" & type1=="SIR"]/med_stats2[type=="within" & type1=="non_SIR"]) %>%
#   dplyr::ungroup() %>%
#   dplyr::distinct()
# calculate perm t
B <- 3000
# loc_ord <- test_dat0$location
#rm(test_dat0)
# perm_t1 <- perm_t4 <-perm_t1g <- perm_t4g <- perm_t1_p <- perm_t4_p <-perm_t1g_p <- perm_t4g_p <- matrix(NA,nrow=B,ncol=1)
perm_t1 <- perm_t4 <-perm_t1g <- perm_t4g <- matrix(NA,nrow=B,ncol=1)

for (i in 1:B){
  # change to median radio?
  perm_t1[i] <- median(pt_h1[,i][pt_h1$X3001=="between"])/median(pt_h1[,i][pt_h1$X3001=="within"])
  perm_t4[i] <- median(pt_h4[,i][pt_h4$X3001=="between"])/median(pt_h4[,i][pt_h4$X3001=="within"])
  # perm_t1_p[i] <- median(pt_h1p[,i][pt_h1p$X5001=="between"])/median(pt_h1p[,i][pt_h1p$X5001=="within"])
  # perm_t4_p[i] <- median(pt_h4p[,i][pt_h4p$X5001=="between"])/median(pt_h4p[,i][pt_h4p$X5001=="within"])
  # group
  perm_t1g[i] <- median(pt_h1g[,i][pt_h1g$X3001=="SIR"])/median(pt_h1g[,i][pt_h1g$X3001=="non_SIR"])
  perm_t4g[i] <- median(pt_h4g[,i][pt_h4g$X3001=="SIR"])/median(pt_h4g[,i][pt_h4g$X3001=="non_SIR"])
  # perm_t1g_p[i] <- median(pt_h1gp[,i][pt_h1gp$X5001=="SIR"])/median(pt_h1gp[,i][pt_h1gp$X5001=="non_SIR"])
  # perm_t4g_p[i] <- median(pt_h4gp[,i][pt_h4gp$X5001=="SIR"])/median(pt_h4gp[,i][pt_h4gp$X5001=="non_SIR"])
}
# calculate p
p1<- sum(perm_t1 > obs_t$med_ratio1[obs_t$horizon==1])/B
p4<- sum(perm_t4 > obs_t$med_ratio1[obs_t$horizon==4])/B
# p1p<- sum(perm_t1_p > obs_tp$med_ratio2[obs_tp$horizon==1])/B
# p4p<- sum(perm_t4_p > obs_tp$med_ratio2[obs_tp$horizon==4])/B
# calculate p for group
p1g<- sum(perm_t1g > obs_t$med_ratio1g[obs_t$horizon==1])/B
p4g<- sum(perm_t4g > obs_t$med_ratio1g[obs_t$horizon==4])/B
# p1g_p<- sum(perm_t1g_p > obs_tp$med_ratio2g[obs_tp$horizon==1])/B
# p4g_p<- sum(perm_t4g_p > obs_tp$med_ratio2g[obs_tp$horizon==4])/B

# print out
print(paste0("P-values for within/between test - pre-wave: ",round(p1,2),",",round(p4,2)))
# print(paste0("P-values for within/between test - post-wave: ",p1p,",",p4p))
print(paste0("P-values for mech/statsML test - pre-wave: ",p1g,",",p4g))
# print(paste0("P-values for mech/statsML test - post-wave: ",p1g_p,",",p4g_p))


# plot
test_stats <- data.frame(rbind(
  cbind(perm_t1,horizon=1),
  cbind(perm_t4,horizon=4)
))
names(test_stats) <- c("value","horizon")
test_stats <- test_stats %>%
  dplyr::left_join(obs_t,by=c("horizon"))

ggplot(test_stats) +
  geom_histogram(aes(x=value),fill="lightblue",color="black") +
  geom_vline(aes(xintercept = med_ratio1),color="red")+
  facet_wrap(~horizon)+
  theme_bw()

test_stats2 <- data.frame(rbind(
  cbind(perm_t1g,horizon=1),
  cbind(perm_t4g,horizon=4)
))
names(test_stats2) <- c("value","horizon")
test_stats2 <- test_stats2 %>%
  dplyr::left_join(obs_tg,by=c("horizon"))
ggplot(test_stats2) +
  geom_histogram(aes(x=value),fill="lightblue",color="black") +
  geom_vline(aes(xintercept = med_ratio1g),color="red")+
  facet_wrap(~horizon)+
  theme_bw()
        
  