# # a function that takes a matrix and plot a heatmap
# # distance_heatmap <- function(sum_dist,h,target,name){
# distance_heatmap <- function(sum_dist,name,metadata=NULL,loc){
#   tmp <- sum_dist %>%
#     dplyr::filter(location==loc) %>%
#     dplyr::group_by(model_1,model_2,horizon) %>%
#     dplyr::mutate(ov_mean=mean(approx_cd,na.rm = TRUE)) %>%
#     dplyr::ungroup() %>%
#     dplyr::select(-c("approx_cd","target_end_date")) %>%
#     dplyr::distinct(.,.keep_all = TRUE) %>%
#     dplyr::arrange(desc(ov_mean)) 
#   # %>%
#   #   dplyr::select(-n_dates)
#   # tmp2 <- sum_dist %>%
#   #   dplyr::group_by(model_1,model_2,location) %>%
#   #   dplyr::mutate(ov_mean=mean(approx_cd)) %>%
#   #   dplyr::ungroup() %>%
#   #   dplyr::select(-c("approx_cd","target_end_date")) %>%
#   #   dplyr::distinct(.,.keep_all = TRUE) %>%
#   #   dplyr::arrange(desc(ov_mean)) %>%
#   #   dplyr::filter(location==loc)
#   # order_list <- unique(tmp2$model_1)
#   if (is.null(metadata)) {
#     ggplot(tmp, aes(model_1,model_2)) +
#       geom_tile(aes(fill= ov_mean)) +
#       facet_wrap(~horizon) +
#       theme(axis.text.x=element_text(size=rel(0.7),angle=20,hjust=1),
#             axis.text.y=element_text(size=rel(0.7)))+
#       labs(title=name)+
#       xlab("") +
#       ylab("") +
#       scale_fill_distiller(palette = "YlOrRd",direction=+1,name="distance") +
#       theme(plot.title = element_text(size=8),
#             legend.title = element_text(size=5),
#             legend.key.size = unit(0.3, 'cm'),
#             legend.text = element_text(size=4),
#             plot.margin=unit(c(0,0,0,0),"cm"))
#     
#   } else {
#     set1<-c("blue","brown")
#     color_set <- ifelse(metadata$compartmental,set1[2],set1[1])
#     type_color <-  color_set[order(match(metadata$model_abbr,order_list))]
#     ggplot(sum_dist, aes(factor(model_1,levels=order_list),
#                          factor(model_2,levels=order_list))) +
#       geom_tile(aes(fill= mean_dis)) +
#       facet_wrap(~horizon) +
#       theme(axis.text.x=element_text(size=rel(0.5),angle=20,hjust=1, colour = type_color),
#             axis.text.y=element_text(size=rel(0.5), colour = type_color))+
#       labs(title=name)+
#       xlab("") +
#       ylab("") +
#       scale_fill_distiller(palette = "YlOrRd",direction=+1,name="distance") +
#       theme(plot.title = element_text(size=8),
#             legend.title = element_text(size=5),
#             legend.key.size = unit(0.3, 'cm'),
#             legend.text = element_text(size=4),
#             plot.margin=unit(c(0,0,0,0),"cm"))
#   }
# }

# scatter <-  function(data,title_name,type,xname,yname){
#   if(type=="wis"){
#     data %>% 
#       ggplot(., aes(x=wis, y=mean,color=factor(horizon))) + 
#       geom_point()+
#       stat_smooth(method = "loess", se = FALSE)+
#       ggtitle(title_name) +
#       ylab(yname) +
#       xlab(xname) +
#       theme(legend.text = element_text(size=5),
#             legend.title = element_text(size=9),
#             legend.key.size = unit(0.5, 'cm'))
#   } else if(type=="current"){
#     data %>% 
#       ggplot(., aes(x=truth, y=sim,color=factor(horizon))) + 
#       geom_point()+
#       stat_smooth(method = "loess", se = FALSE)+
#       ggtitle(title_name) +
#       ylab(yname) +
#       xlab(xname) +
#       theme(legend.text = element_text(size=5),
#             legend.title = element_text(size=9),
#             legend.key.size = unit(0.5, 'cm'))
#   }else if(type=="future"){
#     data %>% 
#       ggplot(., aes(x=sim, y=truth,color=factor(horizon))) + 
#       geom_point()+
#       stat_smooth(method = "loess", se = FALSE)+
#       ggtitle(title_name) +
#       ylab(yname) +
#       xlab(xname) +
#       theme(legend.text = element_text(size=5),
#             legend.title = element_text(size=9),
#             legend.key.size = unit(0.5, 'cm'))
#   }
# }

# catbox_plot <- function(dat, truth_comb,name,target_var){
#    truth <- truth_comb %>%
#      dplyr::filter(target_variable==target_var)%>%
#      dplyr::group_by(target_end_date_x) %>%
#      dplyr::mutate(sum_value=sum(value)) %>%
#      dplyr::ungroup() %>%
#      dplyr::mutate(target_end_date_x=as.Date(target_end_date_x))
#    filtered_truth <- dat %>%
#      dplyr::left_join(truth, by=c("target_end_date"="target_end_date_x")) %>%
#      dplyr::select("target_end_date", "sum_value") %>%
#      dplyr::distinct()
#    # plot
#    obs <- ggplot(filtered_truth, aes(x=target_end_date,y=sum_value))+
#      geom_line()+
#      #facet_wrap(~horizon, nrow=1,scales = "free") +
#      labs(x = "", y = "Observed inc deaths")+
#      scale_x_date(date_breaks = "1 months",
#                   date_labels = "%b-%y",
#                   # to  really limit the range on the plot
#                   expand = expansion())+
#      # scale_y_continuous(limits = c(0, 8))+
#      theme_bw()+
#      theme(axis.text.x = element_text(angle=-45, hjust=-0.2,size=7),
#            axis.text.y = element_text(size=3))
#    # plot
#    box <- ggplot(dat, aes(x=target_end_date, y=mean_approx_cd, group=target_end_date)) +
#    geom_boxplot(outlier.size = 0.3) +
#    facet_wrap(~horizon,nrow=1) +
#    xlab("Date") +
#    ylab("Mean Approx. CD") +
#    guides(fill=guide_legend(title="horizon"))+
#    #ggtitle(name)+
#    scale_x_date(name=NULL, date_breaks = "1 months", date_labels = "%b-%y")+
#    theme_bw()+
#    theme(axis.text.x = element_text(angle=-45, hjust=-0.2,size=7),
#          axis.text.y = element_text(size=7),
#          plot.title = element_text(size=10),
#          axis.title=element_text(size=8),
#          plot.margin=unit(c(0,0,0,0),"cm"))
#    grid.arrange(obs,box, nrow=2,bottom=name)
# }

# catbox_plot2 <- function(dat,name){
#   ggplot(dat, aes(x=target_end_date, y=mean_approx_cd,fill=factor(horizon))) +
#     geom_boxplot(outlier.size = 0.5) +
#     xlab("Forecast Date") +
#     ylab("Mean Approx. CD") +
#     guides(fill=guide_legend(title="horizon",label.position ="bottom",direction="horizontal"))+
#     ggtitle(name)+
#     theme(axis.text.x = element_text(size=7),
#           axis.text.y = element_text(size=7),
#           plot.title = element_text(size=10),
#           axis.title=element_text(size=8))
# } 

# stack_plot <- function(table, model1, model2, by_date=TRUE){
#   if(by_date){
#     table %>%
#       dplyr::filter(model_1==model1,model_2==model2)%>%
#       dplyr::group_by(target_end_date,horizon)%>%
#       dplyr::mutate(mean_cd=mean(approx_cd),
#                     F_larger=mean(F_larger),
#                     G_larger=mean(G_larger),
#                     F_disp=mean(F_disp),
#                     G_disp=mean(G_disp)) %>%
#       dplyr::ungroup()%>%
#       dplyr::distinct(horizon,target_end_date,F_larger,G_larger,F_disp,G_disp,.keep_all = FALSE) %>%
#       pivot_longer(!c(target_end_date,horizon),names_to = "component",values_to = "value")  %>%
#       dplyr::mutate(target_end_date=as.Date(target_end_date,"1970-01-01")) %>%
#       ggplot(aes(fill=component, y=value, x=target_end_date)) + 
#       geom_bar(position="stack", stat="identity",width = 2,show.legend = FALSE) +
#       facet_wrap(~horizon)+
#       scale_x_date(date_breaks = "1 months",
#                    date_labels = "%b-%y",
#                    # to  really limit the range on the plot
#                    expand = expansion())+
#       theme_bw()+
#       theme(axis.text.x = element_text(angle=-45, hjust=-0.2,size=7))
#   } else {
#     table %>%
#       dplyr::filter(model_1==model1,model_2==model2) %>%
#       dplyr::distinct(horizon,mean_F_larger,mean_G_larger,mean_F_disp,mean_G_disp,.keep_all = FALSE) %>%
#       pivot_longer(!horizon,names_to = "component",values_to = "value")  %>%
#       ggplot(aes(fill=component, y=value, x=as.factor(horizon))) + 
#       geom_bar(position="stack", stat="identity",width = 0.4) +
#       labs(x="horizon")+
#       theme_bw()+
#       theme(axis.text.x = element_text(angle=-45, hjust=-0.2,size=7),
#             legend.key.height= unit(0.7, 'cm'),
#             legend.key.width= unit(0.5, 'cm'))
#   }
# }

# catbox_plot4 <- function(dat,name){
#   dat %>%
#     ggplot(aes(x=as.factor(horizon), y=approx_cd, color=pair)) +
#     geom_boxplot(outlier.size = 0.5) +
#     ylab("Mean Approx. CD") +
#     ggtitle(name)+
#     theme_bw()+
#     theme(axis.text.x = element_text(size=7),
#           axis.text.y = element_text(size=7),
#           plot.title = element_text(size=10),
#           axis.title=element_text(size=8),
#           legend.position="bottom",
#           legend.text = element_text(size=rel(0.5)))
# } 

# catbox_plot_inflect <- function(dat, truth,name){
#   filtered_truth <- dat %>%
#     dplyr::mutate(target_end_date=as.character(target_end_date)) %>%
#     dplyr::left_join(truth, by=c("target_end_date"="target_end_date_x")) %>%
#     dplyr::select("target_end_date", "sum_value") %>%
#     dplyr::distinct()
#   # plot
#   obs <- ggplot(filtered_truth, aes(x=target_end_date,y=sum_value))+
#     geom_line()+
#     #facet_wrap(~horizon, nrow=1,scales = "free") +
#     labs(x = "", y = "Observed inc deaths")+
#     scale_x_date(date_breaks = "1 months",
#                  date_labels = "%b-%y",
#                  # to  really limit the range on the plot
#                  expand = expansion())+
#     # scale_y_continuous(limits = c(0, 8))+
#     theme_bw()+
#     theme(axis.text.x = element_text(angle=-45, hjust=-0.2,size=7),
#           axis.text.y = element_text(size=3))
#   # plot
#   box <- ggplot(dat, aes(x=target_end_date, y=mean_approx_cd, group=target_end_date)) +
#     geom_boxplot(outlier.size = 0.3) +
#     facet_wrap(~horizon,nrow=1) +
#     xlab("Date") +
#     ylab("Mean Approx. CD") +
#     guides(fill=guide_legend(title="horizon"))+
#     #ggtitle(name)+
#     scale_x_date(name=NULL, date_breaks = "1 months", date_labels = "%b-%y")+
#     theme_bw()+
#     theme(axis.text.x = element_text(angle=-45, hjust=-0.2,size=7),
#           axis.text.y = element_text(size=7),
#           plot.title = element_text(size=10),
#           axis.title=element_text(size=8),
#           plot.margin=unit(c(0,0,0,0),"cm"))
#   grid.arrange(obs,box, nrow=2,bottom=name)
# } 
# 
# # shuffle function
# 
# shuff_layer <- function(segmented_data){
#   shuf_data <- segmented_data %>%
#     dplyr::filter(model_1!='COVIDhub-ensemble',
#                   model_2!='COVIDhub-ensemble',
#                   pair_type!='neither') %>%
#     dplyr::select('target_end_date','approx_cd','pair_type')
#   shuf_data$pair_type <-sample(shuf_data$pair_type, length(shuf_data$pair_type),FALSE)
#   return(shuf_data)
# }
# 
# perm_test <- function(dat,h,test_type=TRUE){
#   horiz_data <- dat %>%
#     dplyr::filter(horizon==h)
#   # by location, end-date
#   locs <- unique(horiz_data$location)
#   edates <- unique(horiz_data$target_end_date)
#   # shuffle
#   shuf_all <- map_dfr(locs, 
#                       function(locats) { 
#                         map_dfr(edates,
#                                 function(end_dates) {
#                                   horiz_data %>%
#                                     dplyr::filter(location==locats,
#                                                   target_end_date==end_dates) %>%
#                                     shuff_layer()}) 
#                         }
#                       ) 
#   # get ratio from permuted data
#   if(test_type){
#     r_perm <- mean(shuf_all$approx_cd[shuf_all$pair_type=='both'])/mean(shuf_all$approx_cd[shuf_all$pair_type=='mismatched'])
#   } else {
#     r_perm <- mean(shuf_all$approx_cd[shuf_all$pair_type=='mismatched'])-mean(shuf_all$approx_cd[shuf_all$pair_type=='both'])
#   }
#   return(r_perm)
# }

# # a function that takes a matrix and plot a heatmap
# # distance_heatmap <- function(sum_dist,h,target,name){
# distance_heatmap <- function(sum_dist,name,metadata=NULL,loc){
#   tmp <- sum_dist %>%
#     dplyr::filter(location==loc) %>%
#     dplyr::group_by(model_1,model_2,horizon) %>%
#     dplyr::mutate(ov_mean=mean(approx_cd,na.rm = TRUE)) %>%
#     dplyr::ungroup() %>%
#     dplyr::select(-c("approx_cd","target_end_date")) %>%
#     dplyr::distinct(.,.keep_all = TRUE) %>%
#     dplyr::arrange(desc(ov_mean)) 
#   # %>%
#   #   dplyr::select(-n_dates)
#   # tmp2 <- sum_dist %>%
#   #   dplyr::group_by(model_1,model_2,location) %>%
#   #   dplyr::mutate(ov_mean=mean(approx_cd)) %>%
#   #   dplyr::ungroup() %>%
#   #   dplyr::select(-c("approx_cd","target_end_date")) %>%
#   #   dplyr::distinct(.,.keep_all = TRUE) %>%
#   #   dplyr::arrange(desc(ov_mean)) %>%
#   #   dplyr::filter(location==loc)
#   # order_list <- unique(tmp2$model_1)
#   if (is.null(metadata)) {
#     ggplot(tmp, aes(model_1,model_2)) +
#       geom_tile(aes(fill= ov_mean)) +
#       facet_wrap(~horizon) +
#       theme(axis.text.x=element_text(size=rel(0.7),angle=20,hjust=1),
#             axis.text.y=element_text(size=rel(0.7)))+
#       labs(title=name)+
#       xlab("") +
#       ylab("") +
#       scale_fill_distiller(palette = "YlOrRd",direction=+1,name="distance") +
#       theme(plot.title = element_text(size=8),
#             legend.title = element_text(size=5),
#             legend.key.size = unit(0.3, 'cm'),
#             legend.text = element_text(size=4),
#             plot.margin=unit(c(0,0,0,0),"cm"))
#     
#   } else {
#     set1<-c("blue","brown")
#     color_set <- ifelse(metadata$compartmental,set1[2],set1[1])
#     type_color <-  color_set[order(match(metadata$model_abbr,order_list))]
#     ggplot(sum_dist, aes(factor(model_1,levels=order_list),
#                          factor(model_2,levels=order_list))) +
#       geom_tile(aes(fill= mean_dis)) +
#       facet_wrap(~horizon) +
#       theme(axis.text.x=element_text(size=rel(0.5),angle=20,hjust=1, colour = type_color),
#             axis.text.y=element_text(size=rel(0.5), colour = type_color))+
#       labs(title=name)+
#       xlab("") +
#       ylab("") +
#       scale_fill_distiller(palette = "YlOrRd",direction=+1,name="distance") +
#       theme(plot.title = element_text(size=8),
#             legend.title = element_text(size=5),
#             legend.key.size = unit(0.3, 'cm'),
#             legend.text = element_text(size=4),
#             plot.margin=unit(c(0,0,0,0),"cm"))
#   }
# }

# scatter <-  function(data,title_name,type,xname,yname){
#   if(type=="wis"){
#     data %>% 
#       ggplot(., aes(x=wis, y=mean,color=factor(horizon))) + 
#       geom_point()+
#       stat_smooth(method = "loess", se = FALSE)+
#       ggtitle(title_name) +
#       ylab(yname) +
#       xlab(xname) +
#       theme(legend.text = element_text(size=5),
#             legend.title = element_text(size=9),
#             legend.key.size = unit(0.5, 'cm'))
#   } else if(type=="current"){
#     data %>% 
#       ggplot(., aes(x=truth, y=sim,color=factor(horizon))) + 
#       geom_point()+
#       stat_smooth(method = "loess", se = FALSE)+
#       ggtitle(title_name) +
#       ylab(yname) +
#       xlab(xname) +
#       theme(legend.text = element_text(size=5),
#             legend.title = element_text(size=9),
#             legend.key.size = unit(0.5, 'cm'))
#   }else if(type=="future"){
#     data %>% 
#       ggplot(., aes(x=sim, y=truth,color=factor(horizon))) + 
#       geom_point()+
#       stat_smooth(method = "loess", se = FALSE)+
#       ggtitle(title_name) +
#       ylab(yname) +
#       xlab(xname) +
#       theme(legend.text = element_text(size=5),
#             legend.title = element_text(size=9),
#             legend.key.size = unit(0.5, 'cm'))
#   }
# }