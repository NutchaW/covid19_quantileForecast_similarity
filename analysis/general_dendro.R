locs_set <- locs[!(locs=="11")] # get all locations

dist_gen_filtered <- read.csv("./data/analysis_data/distance_all_l.csv") %>%
  dplyr::mutate(target_end_date = as.Date(target_end_date)) %>%
  dplyr::filter(location %in% locs_set,
                target_end_date >= "2020-05-02",
                target_end_date <= "2021-12-11") 
# calculate every over all week location make dendro plot
model_set <- c("COVIDhub.baseline","BPagano.RtDriven","GT.DeepCOVID","MOBS.GLEAM_COVID",
               "UMass.MechBayes","CU.select","UCLA.SuEIR","OliverWyman.Navigator","CEID.Walk",
               "DDS.NBDS","UMich.RidgeTfReg","RobertWalraven.ESG","CMU.TimeSeries","Karlen.pypm",
               "JHU_IDD.CovidSP","JHU_CSSE.DECOM","LANL.GrowthRate","JHUAPL.Bucky","UA.EpiCovDA",
               "Covid19Sim.Simulator","CovidAnalytics.DELPHI","epiforecasts.ensemble1","UCSD_NEU.DeepGLEAM",
               "PSI.DRAFT","SteveMcConnell.CovidComplete","COVIDhub.4_week_ensemble","IHME.CurveFit")
data_filtered <- dis_filter(dist_gen_filtered) %>%
  dplyr::filter(model_1 %in% model_set,
                model_2 %in% model_set)
write_csv(data_filtered,file = "./data/analysis_data/dist_gen_rep.csv") 