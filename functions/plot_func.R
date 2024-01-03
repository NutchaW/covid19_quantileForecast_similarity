truth_plot <- function(truth, tar,locations,fdate_at_bot1,fdate_at_bot2){
  truth %>%
    dplyr::filter(location %in% locations,
                 target_end_date<"2021-10-25" & target_end_date>"2020-09-06",
                  target_variable==tar) %>%
    dplyr::mutate(dateg=ifelse(target_end_date<="2021-06-31","a","b")) %>%
    dplyr::group_by(location,dateg) %>%
    #[which.max(value)]
    dplyr::mutate(fdate_at_peak1=target_end_date[which.max(value)],
                  fdate_at_peak2=target_end_date[which.max(value)]) %>%
    dplyr::ungroup() %>%
    ggplot()+
    geom_line(aes(x=as.Date(target_end_date),y=value))+
    geom_vline(aes(xintercept=as.Date(fdate_at_peak1)),col="red")+
    geom_vline(aes(xintercept=as.Date(fdate_at_peak2)),col="red")+
    geom_vline(aes(xintercept=as.Date(fdate_at_bot1)),col="blue")+
    geom_vline(aes(xintercept=as.Date(fdate_at_bot2)),col="blue")+
#    geom_rect(aes(xmin=as.Date("2020-04-11"),xmax=as.Date("2020-05-02"),ymin=-Inf,ymax=Inf), fill='red', alpha= 0.3)+
#    geom_rect(aes(xmin=as.Date("2020-12-19"),xmax=as.Date("2021-01-09"),ymin=-Inf,ymax=Inf), fill='red', alpha= 0.3)+
    labs(x = "", y = tar)+
    scale_x_date(date_breaks = "1 months",
                 date_labels = "%b-%y",
                 # to  really limit the range on the plot
                 expand = expansion())+
    #facet_wrap(~abbreviation, scales = "free")+
    # scale_y_continuous(limits = c(0, 8))+
    ggtitle(locations)+
    theme_classic()+
    theme(axis.text.x = element_text(angle=90,size=7),
          axis.text.y = element_text(size=3))
}


plot_step <- function(forecasts, model1, model2,horizon,xlim_max){
  # take filter data and plot step function for a pair of forecasts (1 target)
  p_10 <- sort(unique(forecasts$quantile)) # quantile levels
  mod1 <- unlist(c(forecasts[,grep(model1, colnames(forecasts))]))
  mod2 <- unlist(forecasts[,grep(model2, colnames(forecasts))])
  rge <- c(mod1,mod2)
  cd <- covidHubUtils::calc_cramers_dist_unequal_space(q_F=mod1, tau_F=p_10, q_G=mod2, tau_G=p_10,
                                                     approx_rule="left_sided_riemann")
  
  # plot(mod1, p_10, type = "s", xlab = paste0(horizon,"-week ahead predictive quantiles"), ylab = "CDF", col = "red",xlim=c(min(rge)+100,max(rge)+100))
  # lines(mod2, p_10, type = "s", col = "blue")
  dat <- data.frame(rbind(cbind(mod1,p_10,rep(model1,length(p_10))),cbind(mod2,p_10,rep(model2,length(p_10))))) 
  names(dat)  <- c("q","p","Model")
  dat <- dat %>%
    dplyr::mutate(Model = str_replace(Model, "\\.", "-"))
  dat$q <- as.numeric(dat$q)
  dat$p <- as.numeric(dat$p)
  p <- ggplot(dat,aes(x=q,y=p,group=Model,color=Model))+
    geom_line() +
    scale_x_continuous(n.breaks = 5) +
    scale_color_manual(values = c("dodgerblue1","coral1")) 
  p +
    theme_bw() +
    theme(legend.position="bottom",
          text = element_text(size=11),
          legend.text=element_text(size=8),
          legend.title=element_text(size=8))+
    xlab(paste0(horizon,"-week Ahead Incident Deaths"))+
    ylab("Probability level") +
    # annotate(geom="text", x=6000, y=0.2, parse=TRUE, label= paste0("CD","%~~%",round(cd,2)),
    #          size = 11/.pt)
    annotate(geom="text", x=quantile(xlim_max,0.9), y=0.2, parse=TRUE, 
             label= paste0("CD","%~~%",deparse(format(round(cd,2),nsmall=2))),
             parse=TRUE,
             size = 11/.pt)
}

catbox_plot3 <- function(dat,name,h,hosp=FALSE){
  if(hosp==TRUE){
    dat <- dat %>%
      dplyr::mutate(horizon=ifelse(horizon<=7,
                                   1,
                                   ifelse(horizon<=14&horizon>7,
                                          2,
                                          ifelse(horizon>14&horizon<=21,
                                                 3,
                                                 4)
                                   )
      )) 
  } 
  dat %>%
    dplyr::filter(horizon==h) %>%  
    ggplot(aes(x=model_1, y=approx_cd, fill=model_1)) +
    geom_boxplot(outlier.size = 0.5,show.legend = FALSE) +
    stat_summary(fun.y = mean, 
                 geom = "point", 
                 shape = 3, 
                 size = 1,
                 show.legend = FALSE) +
    facet_wrap(~location,scale="free") +
    xlab("Horizon") +
    ylab("Relative Approx. CD") +
    ggtitle(name)+
    theme_bw() +
    theme(axis.text.x = element_text(size=7,angle=-20,hjust=0),
          axis.text.y = element_text(size=7),
          plot.title = element_text(size=10),
          axis.title=element_text(size=8),
          legend.position="bottom") 
} 

catbox_horizon <- function(dat,name,loc){
  dat %>%
    # define outliers as anyting past the 95% quantile to the right
    # dplyr::group_by(location,horizon) %>%
    # dplyr::mutate(outl=quantile(mean_cd,probs=0.975)) %>%
    # dplyr::ungroup() %>%
    dplyr::filter(location %in% loc,
       #           mean_cd <= outl
                  ) %>%
    ggplot(aes(x=as.factor(horizon), y=med_cd)) +
    geom_boxplot(outlier.size = 0.5,fill="#d95f02") +
    stat_summary(fun.y = mean,
                 geom = "line",group = 1, lwd = 0.3, lty = 2,
                 show.legend = FALSE) +
    stat_summary(fun.y = mean, 
                 geom = "point", 
                 shape = 3,
                 size = 1,
                 show.legend = FALSE) +
    facet_wrap(~location) +
    xlab("Horizon") +
    ylab("Median scaled approx. CD") +
    ggtitle(name)+
    theme_bw() +
    theme(axis.text.x = element_text(size=7),
          axis.text.y = element_text(size=7),
          plot.title = element_text(size=10),
          axis.title=element_text(size=8),
          legend.position="bottom") 
} 

line_cd <- function(dat,name,loc,hosp=FALSE){
  if(hosp==TRUE){
    dat <- dat %>%
      dplyr::mutate(horizon=ifelse(horizon<=7,
                                   1,
                                   ifelse(horizon<=14&horizon>7,
                                          2,
                                          ifelse(horizon>14&horizon<=21,
                                                 3,
                                                 4)
                                   )
      )) 
  } 
  dat %>%
    dplyr::filter(location==loc) %>%
    dplyr::group_by(horizon,pair_type, target_end_date) %>%
    dplyr::transmute(mean_cd=mean(approx_cd),
                     horizon=horizon,
                     pair_type=pair_type, 
                     target_end_date= target_end_date) %>%
    dplyr::ungroup() %>%
    ggplot(aes(x=target_end_date, y=mean_cd, color=pair_type)) +
    geom_point(size=0.5) +
    geom_line()+    
    facet_wrap(~horizon,scale="free") +
    xlab("Horizon") +
    ylab("Mean Approx. CD") +
    ggtitle(name)+
    scale_x_date(name=NULL, date_breaks = "1 months", date_labels = "%b-%y")+
    theme_bw() +
    theme(axis.text.x = element_text(size=7,angle=-20,hjust=0),
          axis.text.y = element_text(size=7),
          plot.title = element_text(size=10),
          axis.title=element_text(size=8),
          legend.position="bottom") 
} 

hist_cd <- function(dat,name,loc,hosp=FALSE){
  if(hosp==TRUE){
    dat <- dat %>%
      dplyr::mutate(horizon=ifelse(horizon<=7,
                                   1,
                                   ifelse(horizon<=14&horizon>7,
                                          2,
                                          ifelse(horizon>14&horizon<=21,
                                                 3,
                                                 4)
                                   )
      )) 
  } 
  dat <- dat %>%
    dplyr::filter(location==loc) 
  ggplot() +
    geom_density(data=dat,aes(y=approx_cd,color=pair_type)) +
    #  geom_density(data=subset(dat, pair_type== 'stats-pair'),
    #                 aes(y=approx_cd), color="blue",
    # #                fill = "blue", alpha = 0.5
    #                 ) +
    #   geom_density(data=subset(dat,pair_type == 'epi-pair'),
    #                  aes(y=approx_cd),color="red",
    # #                 fill = "red", alpha = 0.5
    #                  ) +
    #   geom_density(data=subset(dat,pair_type == 'stats-epi'),
    #                  aes(y=approx_cd),color="green",
    #  #                fill = "green", alpha = 0.5
  #                  )+
  coord_flip()+
    facet_wrap(~horizon,scale="free") +
    xlab("Horizon") +
    ylab("Approx. CD") +
    ggtitle(name)+
    theme_bw() +
    theme(axis.text.x = element_text(size=7),
          axis.text.y = element_text(size=7),
          plot.title = element_text(size=10),
          axis.title=element_text(size=8),
          legend.position="bottom") 
} 

stack_plot <- function(table, model1=NULL, model2=NULL, by_date=TRUE){
  if(is.null(model1) & is.null(model2)){
    table <- table 
  } else {
    table <- table %>%
      dplyr::filter(model_1 %in% c(model1,model2) & model_2 %in% c(model1,model2))
  }
  if(by_date){
    # have to run this for separate location and filter table to locaiton before calling this
    table %>%  
      dplyr::group_by(target_end_date,horizon,location)%>%
      dplyr::mutate(mean_cd=mean(approx_cd),
                    F_larger=mean(F_larger),
                    G_larger=mean(G_larger),
                    F_disp=mean(F_disp),
                    G_disp=mean(G_disp)) %>%
      dplyr::ungroup()%>%
      dplyr::distinct(horizon,target_end_date,F_larger,G_larger,F_disp,G_disp,location,.keep_all = FALSE) %>%
      pivot_longer(!c(target_end_date,horizon,location),names_to = "component",values_to = "value")  %>%
      dplyr::mutate(target_end_date=as.Date(target_end_date,"1970-01-01")) %>%
      ggplot(aes(fill=component, y=value, x=target_end_date)) +
      geom_bar(position="stack", stat="identity",width = 2,show.legend = TRUE) +
      facet_grid(location~horizon,scale="free")+
      scale_x_date(date_breaks = "1 months",
                   date_labels = "%b-%y",
                   # to  really limit the range on the plot
                   expand = expansion())+
      scale_fill_brewer(palette = "Dark2")+
      ggtitle(paste0(unique(table$model_1),"(F) - ",unique(table$model_2),"(G)"))+
      theme_bw()+
      theme(axis.text.x = element_text(angle=-45, hjust=-0.2,size=7),
            legend.text=element_text(size=8),
            legend.position = "bottom")
  } else {
    table %>%
      dplyr::distinct(location,horizon,mean_F_larger,mean_G_larger,mean_F_disp,mean_G_disp,.keep_all = FALSE) %>%
      pivot_longer(!c(horizon,location),names_to = "component",values_to = "value")  %>%
      ggplot(aes(fill=component, y=value, x=as.factor(horizon))) +
      geom_bar(position="stack", stat="identity",width = 0.4) +
      facet_wrap(~location,nrow=2) +
      labs(x="horizon")+
      scale_fill_brewer(palette = "Dark2")+
      ggtitle(paste0(unique(table$model_1),"(F) -\n",unique(table$model_2),"(G)"))+
      theme_bw()+
      theme(axis.text.x = element_text(angle=-45, hjust=-0.2,size=7),
            legend.key.height= unit(0.7, 'cm'),
            legend.key.width= unit(0.5, 'cm'),
            legend.text=element_text(size=8),
            plot.title = element_text(size = 9))
  }
}

box_type_phase <- function(dat,loc,pname,within=FALSE){
  if(within){
    dat %>%
      dplyr::filter(location %in% loc,
                    pair_type=="same-pair") %>%
      dplyr::mutate(type=ifelse(type.x=="Mech", "mechanistic","stats/ML")) %>%
      ggplot(aes(x=as.factor(horizon),y=log(approx_cd),fill=type)) +
      geom_boxplot(outlier.size = 0.1)+
      # geom_violin()+
      scale_fill_brewer(palette = "Dark2") +
      ggtitle(pname)+
      xlab("horizon")+
      facet_wrap(~location,scale="free",nrow=4) +
      theme_bw() +
      theme(legend.position = "bottom")
    
  } else {
    dat %>%
      dplyr::filter(location %in% loc) %>%
      ggplot(aes(x=as.factor(horizon),y=log(approx_cd),fill=pair_type)) +
      geom_boxplot(outlier.size = 0.1)+
      # geom_violin()+
      scale_fill_brewer(palette = "Dark2") +
      ggtitle(pname)+
      xlab("horizon")+
      facet_wrap(~location,scale="free",nrow=4) +
      theme_bw() +
      theme(legend.position = "bottom")
    
  } 
}

box_type_phase2 <- function(dat,pname){
  dat %>%
    dplyr::filter(pair_type=="Within-group") %>%
    dplyr::mutate(type=ifelse(type.x=="Mechanistic", "Mechanistic","Statistical"),
                  horizon = paste0(horizon," week \n ahead")) %>%
    # dplyr::group_by(horizon,target_end_date,type) %>%
    # dplyr::mutate(mcd =sum(approx_cd)) %>%
    # dplyr::ungroup() %>%
    # dplyr::select(mcd,horizon,pair_type,type) %>%
    # dplyr::distinct() %>%
      # ggplot(aes(x=type,y=approx_cd,fill=factor(horizon))) +
      ggplot(aes(x=horizon,y=approx_cd,fill=type)) +
      geom_boxplot(
                   outlier.size = 0.1,
                   #outlier.shape = NA
                   )+
      scale_fill_brewer(palette = "Dark2") +
     # scale_y_continuous(limits = c(0,350))+
    #  scale_y_continuous(limits = c(0,15))+
      ggtitle(pname)+
      ylab("Approximate CD")+
      xlab("")+
      # facet_wrap(~location,nrow=2,labeller=as_labeller(c('2'='2 week ahead','4'='4 week ahead'))) +
      facet_wrap(~factor(location_name,
                         levels=c("Texas","Georgia","Washington","Tennessee","Mississippi",
                                  "South Carolina","Louisiana","North Carolina","Virginia","Indiana")),
                 ncol=5, 
                 scales = "free") +
      theme_bw() +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            axis.text.x = element_text(size=rel(0.9)),
            axis.text.y = element_text(size=rel(0.9)),
            legend.margin=margin(t = -0.5, unit='cm')
            #axis.text.x = element_text(angle = 40, vjust = 1, hjust=1)
            )
}

box_type_phase3 <- function(dat,pname){
  dat %>%
    ggplot(aes(x=location,y=approx_cd,fill=pair_type)) +
    geom_boxplot(
                 # outlier.size = 0.1,
                 outlier.shape = NA)+
    scale_y_continuous(limits = c(0,275))+
    # geom_violin()+
    scale_fill_brewer(palette = "Dark2") +
    ggtitle(pname)+
    ylab("Approximated Cramer Distance")+
    xlab("")+
    # facet_wrap(~horizon,nrow=2,labeller=as_labeller(c('2'='2 week ahead','4'='4 week ahead'))) +
    facet_wrap(~location_name,nrow=2) +
    guides(fill=guide_legend(title=""))+
    theme_bw() +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 40, vjust = 1, hjust=1))
}

box_type_phase_nw <- function(dat,pname){
  dat %>%
    dplyr::mutate(type=ifelse(type.x=="Mechanistic", "Mechanistic","Statistical"),
                  horizon = paste0(horizon," week \n ahead")) %>%
    # dplyr::group_by(horizon,target_end_date,pair_type) %>%
    # dplyr::mutate(mcd =sum(approx_cd)) %>%
    # dplyr::ungroup() %>%
    # dplyr::select(mcd,horizon,pair_type) %>%
    # dplyr::distinct() %>%
    ggplot(aes(x=horizon,y=approx_cd,fill=factor(pair_type,levels=c("Within-group","Between-group")))) +
    geom_boxplot(
      outlier.size = 0.1,
      #outlier.shape = NA
      )+
    #scale_y_continuous(limits = c(0,275))+
    scale_fill_brewer(palette = "Paired") +
    ggtitle(pname)+
    ylab("Approximate CD")+
    xlab("")+
    # facet_wrap(~horizon,nrow=2,labeller=as_labeller(c('2'='2 week ahead','4'='4 week ahead'))) +
    facet_wrap(~factor(location_name,
                       levels=c("Texas","Georgia","Washington","Tennessee","Mississippi",
                                "South Carolina","Louisiana","North Carolina","Virginia","Indiana")),
               ncol=5, scales = "free") +
    guides(fill=guide_legend(title=""))+
    theme_bw() +
    theme(legend.position = "bottom",
          axis.text.x = element_text(size=rel(0.9)),
          axis.text.y = element_text(size=rel(0.9)),
          legend.margin=margin(t = -0.5, unit='cm')

    )
}

concept_plot <- function(loc, date, models,fordat){
  models_d <- str_replace(models, "\\-", ".")
  fdat <- load_forecasts(models = models,
                         dates = date,
                         source = "zoltar",
                         date_window_size = 6,
                         locations = loc,
                         types = c("quantile", "point"),
                         verbose = FALSE,
                         targets = paste(1:4, "wk ahead inc death"))
  
  pd <- plot_forecasts(fdat,
                       target_variable = "inc death",
                       truth_source = "JHU",
                       intervals = c(.95),
                       facet = model~.,
                       fill_by_model = TRUE,
                       facet_ncol=1,
                       plot=FALSE,
                       title="none",
                       subtitle="none",
                       show_caption=FALSE)
  
  p_formatted <- pd +
    scale_x_date(name=NULL, date_breaks = "1 months", date_labels = "%b-%y",
                 limits = c(as.Date("2021-03-05"), as.Date("2021-10-15")))+
    theme_bw() +
    theme(axis.text.x = element_text(angle=-45, hjust=-0.2),
          legend.position = "none") 
  w_pattern <- paste0(models_d[1],"|",models_d[2],"|quantile")
  for(i in 1:4){
    p <-  fordat %>%
      dplyr::filter(horizon==i,
                    forecast_date==as.Date(date[1]),
                    location==loc)  %>%
      .[,grep(w_pattern,colnames(fordat))]
    assign(paste0("p",i),
           plot_step(p,models_d[1],models_d[2],i)
    )
    pf <-  fordat %>%
      dplyr::filter(horizon==i,
                    forecast_date==as.Date(date[2]),
                    location==loc)  %>%
      .[,grep(w_pattern,colnames(fordat))]
    assign(paste0("pf",i),
           plot_step(pf,models_d[1],models_d[2],i)
    )
  }
  legend = gtable_filter(ggplot_gtable(ggplot_build(p1)), "guide-box")
  # plot
  pname <- paste0("Location: ",loc,", Forecast date: ",date)
  cp <- grid.arrange(p_formatted, 
               arrangeGrob(       
               arrangeGrob(textGrob(paste0("      Forecast date: ",date[1]),gp=gpar(fontsize=8)),
                           p1 + theme(legend.position="none"),
                           p2 + theme(legend.position="none"), 
                           p3 + theme(legend.position="none"),
                           p4 + theme(legend.position="none"),
                           heights=c(0.2,1,1,1,1),
                           ncol = 1),
               arrangeGrob(textGrob(paste0("      Forecast date: ",date[2]),gp=gpar(fontsize=8)),
                           pf1 + theme(legend.position="none"),
                           pf2 + theme(legend.position="none"), 
                           pf3 + theme(legend.position="none"),
                           pf4 + theme(legend.position="none"),
                           heights=c(0.2,1,1,1,1),
                           ncol = 1),
               legend,
               layout_matrix =rbind(c(1,2),
                                    c(1,2),
                                    c(1,2),
                                    c(1,2),
                                    c(1,2),
                                    c(1,2),
                                    c(1,2),
                                    c(1,2),
                                    c(1,2),
                                    c(1,2),
                                    c(3,3))),
               ncol=2
               )
}

# truth

concept_plot2 <- function(loc, date, models,fordat){
  models_d <- str_replace(models, "\\-", ".")
  fdat <- load_forecasts(models = models,
                         dates = date,
                         source = "zoltar",
                         date_window_size = 6,
                         locations = loc,
                         types = c("quantile", "point"),
                         verbose = FALSE,
                         targets = paste(1:4, "wk ahead inc death"))
  
  pd <- plot_forecasts(fdat,
                       target_variable = "inc death",
                       truth_source = "JHU",
                       intervals = c(.95),
                       facet = model~.,
                       fill_by_model = TRUE,
                       facet_ncol=1,
                       plot=FALSE,
                       title="none",
                       subtitle="none",
                       show_caption=FALSE)
  
  p_formatted <- pd +
    scale_x_date(name=NULL, date_breaks = "1 months", date_labels = "%b-%y",
                 limits = c(as.Date("2021-01-05"), as.Date("2021-12-15")))+
    theme(axis.text.x = element_text(angle=-45, hjust=-0.2),
          legend.position = "none")
  w_pattern <- paste0(models_d[1],"|",models_d[2],"|quantile")
  for(i in 1:4){
    p <-  fordat %>%
      dplyr::filter(horizon==i,
                    forecast_date==as.Date(date[1]),
                    location==loc)  %>%
      .[,grep(w_pattern,colnames(fordat))]
    assign(paste0("p",i),
           plot_step(p,models_d[1],models_d[2],i)
    )
    pf <-  fordat %>%
      dplyr::filter(horizon==i,
                    forecast_date==as.Date(date[2]),
                    location==loc)  %>%
      .[,grep(w_pattern,colnames(fordat))]
    assign(paste0("pf",i),
           plot_step(pf,models_d[1],models_d[2],i)
    )
  }
  legend = gtable_filter(ggplot_gtable(ggplot_build(p1)), "guide-box")
  # plot
  pname <- paste0("Location: ",loc,", Forecast date: ",date)
  cp <- grid.arrange(p_formatted, 
                     arrangeGrob(       
                       arrangeGrob(textGrob(paste0("      Forecast date: ",date[1]),gp=gpar(fontsize=8)),
                                   p1 + theme(legend.position="none"),
                                   p2 + theme(legend.position="none"), 
                                   p3 + theme(legend.position="none"),
                                   p4 + theme(legend.position="none"),
                                   heights=c(0.2,1,1,1,1),
                                   ncol = 1),
                       arrangeGrob(textGrob(paste0("      Forecast date: ",date[2]),gp=gpar(fontsize=8)),
                                   pf1 + theme(legend.position="none"),
                                   pf2 + theme(legend.position="none"), 
                                   pf3 + theme(legend.position="none"),
                                   pf4 + theme(legend.position="none"),
                                   heights=c(0.2,1,1,1,1),
                                   ncol = 1),
                       legend,
                       layout_matrix =rbind(c(1,2),
                                            c(1,2),
                                            c(1,2),
                                            c(1,2),
                                            c(1,2),
                                            c(1,2),
                                            c(1,2),
                                            c(1,2),
                                            c(1,2),
                                            c(1,2),
                                            c(3,3))),
                     ncol=2
  )
  #print(conceptp)
}

filter_lims <- function(x){
  l <- boxplot.stats(x)$stats[1]
  u <- boxplot.stats(x)$stats[5]
  
  for (i in 1:length(x)){
    x[i] <- ifelse(x[i]>l & x[i]<u, x[i], NA)
  }
  return(x)
}

ccf_extract <- function(hlist, horiz, location_list){
  ccf_dat <- c()
  for (x in 1:length(location_list)){
    loc_data <- hlist[[x]]
    loc <- location_list[x]
    locd <- data.frame(cbind(loc_data$acf,
                     2 * (1 - pnorm(abs(loc_data$acf), mean = 0, sd = 1/sqrt(loc_data$n.used))),
                     rep(horiz,loc_data$n.used),
                     rep(loc,loc_data$n.used),
                     loc_data$lag)) %>%
      set_names(c("corr","p_val","horizon","location","lag"))
    ccf_dat <- rbind(ccf_dat,locd)
  }
  ccf_dat <- ccf_dat %>%
    dplyr::mutate(corr=as.numeric(corr),
                  p_val=as.numeric(p_val),
                  lead = ifelse(lag < 0, "lead","0 lag/lag"))
  return(ccf_dat)
}

distance_heatmap2 <- function(distmat,name_order){
  
  dis <- distmat %>%
    dplyr::group_by(model_1,model_2) %>%
    dplyr::mutate(mean_cd=mean(approx_cd)) %>%
    dplyr::ungroup() %>%
    dplyr::select(model_1,model_2,mean_cd) %>%
    distinct()
  range_dis <- c(min(dis$mean_cd), max(dis$mean_cd))
  # set1<-c("blue","brown")
  # color_set <- ifelse(metadata$type=="SIR",set1[2],set1[1])
  # type_color <-  color_set[order(match(metadata$acro,order_list))]
  ggplot(dis, aes(factor(model_1,levels=name_order),
                  factor(model_2,levels=name_order))) +
      geom_tile(aes(fill=mean_cd)) +
      # theme(axis.text.x=element_text(size=rel(0.5),color = type_color,angle = 90),
      #       axis.text.y=element_text(size=rel(0.4), color = type_color))+
      # labs(title=name)+
      xlab("") +
      ylab("") +
      scale_x_discrete(expand = c(0, 0)) +    
      scale_y_discrete(expand = c(0, 0)) + 
      # scale_fill_distiller(palette = "viridis",direction=+1,name="distance") +
      scale_fill_distiller(palette = "viridis",direction=+1,name="Distance") +
      theme(plot.title = element_text(size=8),
            axis.text.x = element_text(size=10,angle = 90, vjust = 1, hjust=1),
            axis.text.y = element_text(size=10),
            legend.title = element_text(size=10),
            legend.key.size = unit(0.5, 'cm'),
            legend.text = element_text(size=9),
            legend.position = "bottom",
            axis.ticks = element_blank(),     
            plot.margin=unit(c(-0.1,0,0,0),"cm"))
    
}

concept_plot0 <- function(loc, date, models){
  models_d <- str_replace(models, "\\-", ".")
  fdat <- load_forecasts(models = models,
                         dates = date,
                         source = "zoltar",
                         date_window_size = 6,
                         locations = loc,
                         types = c("quantile", "point"),
                         verbose = FALSE,
                         targets = paste(1:4, "wk ahead inc death"))
  
  pd <- plot_forecasts(fdat,
                       target_variable = "inc death",
                       truth_source = "JHU",
                       intervals = c(.95),
                       facet = location~model,
                       fill_by_model = TRUE,
                       facet_ncol=1,
                       plot=FALSE,
                       title="none",
                       subtitle="none",
                       show_caption=FALSE)
  
  pd+scale_x_date(name=NULL, date_breaks = "3 months", date_labels = "%b-%y",
                 limits = c(as.Date("2020-05-15"), as.Date("2021-10-15")))+
    theme_bw()+
    theme(axis.text.x = element_text(angle=-45, hjust=-0.2),
          legend.title = element_text(size=8),
          # legend.key.size = unit(0.3, 'cm'),
          legend.text = element_text(size=7),
          # legend.position = "bottom",
          ) 
}


# concept_plot_new <- function(loc, date, models,fordat){
#   models_d <- str_replace(models, "\\-", ".")
#   fdat <- load_forecasts(models = models,
#                          dates = date,
#                          source = "zoltar",
#                          date_window_size = 6,
#                          locations = loc,
#                          types = c("quantile", "point"),
#                          verbose = FALSE,
#                          targets = paste(1:4, "wk ahead inc death"))
#   
#   pd <- plot_forecasts(fdat,
#                        target_variable = "inc death",
#                        truth_source = "JHU",
#                        intervals = c(.95),
#                        facet = model~.,
#                        fill_by_model = TRUE,
#                        facet_ncol=1,
#                        plot=FALSE,
#                        title="none",
#                        subtitle="none",
#                        show_caption=FALSE)
#   
#   p_formatted <- pd +
#     scale_x_date(name=NULL, date_breaks = "1 months", date_labels = "%b-%y",
#                  limits = c(as.Date("2021-03-05"), as.Date("2021-10-15")))+
#     theme_bw() +
#     theme(axis.text.x = element_text(angle=-45, hjust=-0.2),
#           legend.position = "none") 
#   w_pattern <- paste0(models_d[1],"|",models_d[2],"|quantile")
#   for(i in 1:4){
#     p <-  fordat %>%
#       dplyr::filter(horizon==i,
#                     forecast_date==as.Date(date[1]),
#                     location==loc)  %>%
#       .[,grep(w_pattern,colnames(fordat))]
#     assign(paste0("p",i),
#            plot_step(p,models_d[1],models_d[2],i)
#     )
#     pf <-  fordat %>%
#       dplyr::filter(horizon==i,
#                     forecast_date==as.Date(date[2]),
#                     location==loc)  %>%
#       .[,grep(w_pattern,colnames(fordat))]
#     assign(paste0("pf",i),
#            plot_step(pf,models_d[1],models_d[2],i)
#     )
#   }
#   legend = gtable_filter(ggplot_gtable(ggplot_build(p1)), "guide-box")
#   # plot
#   pname <- paste0("Location: ",loc,", Forecast date: ",date)
#   cp <- grid.arrange(p_formatted, 
#                      arrangeGrob(       
#                        arrangeGrob(textGrob(paste0("      Forecast date: ",date[1]),gp=gpar(fontsize=8)),
#                                    p1 + theme(legend.position="none"),
#                                    p2 + theme(legend.position="none"), 
#                                    p3 + theme(legend.position="none"),
#                                    p4 + theme(legend.position="none"),
#                                    heights=c(0.2,1,1,1,1),
#                                    ncol = 1),
#                        arrangeGrob(textGrob(paste0("      Forecast date: ",date[2]),gp=gpar(fontsize=8)),
#                                    pf1 + theme(legend.position="none"),
#                                    pf2 + theme(legend.position="none"), 
#                                    pf3 + theme(legend.position="none"),
#                                    pf4 + theme(legend.position="none"),
#                                    heights=c(0.2,1,1,1,1),
#                                    ncol = 1),
#                        legend,
#                        layout_matrix =rbind(c(1,2),
#                                             c(1,2),
#                                             c(1,2),
#                                             c(1,2),
#                                             c(1,2),
#                                             c(1,2),
#                                             c(1,2),
#                                             c(1,2),
#                                             c(1,2),
#                                             c(1,2),
#                                             c(3,3))),
#                      ncol=2
#   )
# }

# truth

concept_plot_new <- function(loc, date, models,fordat){
  models_d <- str_replace(models, "\\-", ".")
  fdat <- load_forecasts(models = models,
                         dates = date,
                         source = "zoltar",
                         date_window_size = 6,
                         locations = loc,
                         types = c("quantile", "point"),
                         verbose = FALSE,
                         targets = paste(1:4, "wk ahead inc death"))
  
  pd <- plot_forecasts(fdat,
                       target_variable = "inc death",
                       truth_source = "JHU",
                       intervals = c(.95),
                       facet = .~model,
                       fill_by_model = TRUE,
                       facet_ncol=1,
                       plot=FALSE,
                       title="none",
                       subtitle="none",
                       show_caption=FALSE)
  p_formatted <- pd +
    scale_x_date(name=NULL, date_breaks = "9 weeks", date_labels = "%b-%Y",
                 limits = c(as.Date("2020-04-22"), as.Date("2021-12-18")))+
    theme_bw()+
    theme(axis.text.x = element_text(size=10),
          # axis.text.x = element_text(angle=-45, hjust=-0.2),
          legend.position = "none")
  w_pattern <- paste0(models_d[1],"|",models_d[2],"|quantile")
  for(i in 1:4){
    p <-  fordat %>%
      dplyr::filter(horizon==i,
                    forecast_date==as.Date(date[1]),
                    location==loc)  %>%
      .[,grep(w_pattern,colnames(fordat))]
    assign(paste0("p",i),
           plot_step(p,models_d[1],models_d[2],i,7000)+
             xlim(250,8500)
    )
    pf <-  fordat %>%
      dplyr::filter(horizon==i,
                    forecast_date==as.Date(date[2]),
                    location==loc)  %>%
      .[,grep(w_pattern,colnames(fordat))]
    assign(paste0("pf",i),
           plot_step(pf,models_d[1],models_d[2],i,2250)+
             xlim(0,3000)
    )
  }
  legend = gtable_filter(ggplot_gtable(ggplot_build(p1)), "guide-box")
  # plot
  pname <- paste0("Location: ",loc,", Forecast date: ",date)
  cp <- grid.arrange(p_formatted, 
                     arrangeGrob(       
                       arrangeGrob(textGrob(paste0("      Forecast date: ",date[1]),gp=gpar(fontsize=11)),
                                   # p1 + theme(legend.position="none"),
                                   p2 + theme(legend.position="none"), 
                                   # p3 + theme(legend.position="none"),
                                   p4 + theme(legend.position="none"),
                                   heights=c(0.8,6,6),
                                   nrow = 3),
                       arrangeGrob(textGrob(paste0("      Forecast date: ",date[2]),gp=gpar(fontsize=11)),
                                   # pf1 + theme(legend.position="none"),
                                   pf2 + theme(legend.position="none"), 
                                   # pf3 + theme(legend.position="none"),
                                   pf4 + theme(legend.position="none"),
                                   heights=c(0.8,6,6),
                                   nrow = 3),
                       legend,
                       layout_matrix =rbind(c(1,2),
                                            c(1,2),
                                            c(1,2),
                                            c(1,2),
                                            c(1,2),
                                            c(1,2),
                                            c(1,2),
                                            c(1,2),
                                            c(1,2),
                                            c(3,3))),
                     nrow=2
  )
  #print(conceptp)
}
