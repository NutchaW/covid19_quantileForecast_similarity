# Simple implementation without using the interval representation 
# and computing decomposition
approx_cd0 <- function(q_F, q_G){
  K <- length(q_F)
  matr_q_F <- matrix(q_F, ncol = K, nrow = K)
  matr_q_G <- matrix(q_G, ncol = K, nrow = K, byrow = TRUE)
  matr_indices <- matrix(1:K, ncol = K, nrow = K)
  mismatches <- sign(matr_q_F - matr_q_G) != sign(matr_indices - t(matr_indices))
  abs_diff <- abs(matr_q_F - matr_q_G)
  cd <- sum(2*mismatches*abs_diff)/K/(K + 1)
  cd
}
# helper function to evaluate interval divergence:
interval_comparison <- function(l_F, u_F, alpha_F, l_G, u_G, alpha_G){
  
  if((alpha_F == 0 & (l_F != u_F)) |
     (alpha_G == 0 & (l_G != u_G))){
    stop("Upper and lower bounds of 0% prediction intervals need to be identical")
  }
  # if both PIs are at level 0%
  if(alpha_F == 0 & alpha_G == 0){
    F_disp <- 0
    G_disp <- 0
    F_larger <- 4*max(l_F - l_G, 0)
    G_larger <- 4*max(l_G - l_F, 0)
  }
  
  # if both PIs have same level, but not 0%
  if(alpha_F == alpha_G & alpha_F != 0){
    F_disp <- max((u_F - l_F) - (u_G - l_G), 0)
    G_disp <- max((u_G - l_G) - (u_F - l_F), 0)
    F_larger <- max(max(u_F - u_G, 0) + max(l_F - l_G, 0) + 
                      max(l_F - u_G, 0) - F_disp - G_disp, 0)
    G_larger <- max(max(u_G - u_F, 0) + max(l_G - l_F, 0) + 
                      max(l_G - u_F, 0) - F_disp - G_disp, 0)
  }
  # if F has lower nominal coverage and "should" be nested in G:
  if(alpha_F < alpha_G){
    F_disp <- max((u_F - l_F) - (u_G - l_G), 0)
    G_disp <- 0
    F_larger <- max(max(u_F - u_G, 0) + max(l_F - u_G, 0) - F_disp, 0)
    G_larger <- max(max(l_G - l_F, 0) + max(l_G - u_F, 0) - F_disp, 0)
  }
  # if G has lower nominal coverage and "should" be nested in F:
  if(alpha_G < alpha_F){
    G_disp <- max((u_G - l_G) - (u_F - l_F), 0)
    F_disp <- 0
    G_larger <- max(max(u_G - u_F, 0) + max(l_G - u_F, 0) - G_disp, 0)
    F_larger <- max(max(l_F - l_G, 0) + max(l_F - u_G, 0) - G_disp, 0)
  }
  
  id <- F_larger + G_larger + F_disp + G_disp
  
  return(list(id = id,
              F_larger = F_larger,
              G_larger = G_larger,
              F_disp = F_disp,
              G_disp = G_disp))
}
# function to compute CD and its decomposition:
approx_cd <- function(q_F, q_G){
  # compute quantile levels from length of provided quantile vectors:
  K <- length(q_F)
  K_uneven <- K %% 2 == 1
  if(length(q_G) != K) stop("q_F and q_G need to be of the same length")
  p <- (1:K)/(K + 1) # function assumes that the quantile levels are equally spaced
  
  n_intervals <- ceiling(K/2) # to handle uneven number of quantiles
  # matrices to store interval divergences and components:
  matrix_interval_comparisons <-
    matrix_F_larger <- matrix_G_larger <-
    matrix_F_disp <- matrix_G_disp <-
    matrix(NA, ncol = n_intervals, nrow = n_intervals,
           dimnames = list(paste("F", 1:n_intervals), paste("G", 1:n_intervals)))
  
  # fill these matrices:
  for(i in 1:n_intervals){
    for(j in 1:n_intervals){
      i_comp <- interval_comparison(l_F = q_F[i], u_F = q_F[K + 1 - i], 
                                    alpha_F = p[K + 1 - i] - p[i],
                                    l_G = q_G[j], u_G = q_G[K + 1 - j], 
                                    alpha_G = p[K + 1 - j] - p[j])
      matrix_interval_comparisons[i, j] <- i_comp$id
      matrix_F_larger[i, j] <- i_comp$F_larger
      matrix_G_larger[i, j] <- i_comp$G_larger
      matrix_F_disp[i, j] <- i_comp$F_disp
      matrix_G_disp[i, j] <- i_comp$G_disp
    }
  }
  
  weights <- matrix(1, ncol = n_intervals, nrow = n_intervals)
  if(K_uneven){
    weights[n_intervals, ] <- weights[, n_intervals] <- 0.5
    weights[n_intervals, n_intervals] <- 1/4
  }
  
  normalization <- K*(K + 1)
  cd <- 2*sum(matrix_interval_comparisons*weights)/normalization
  F_larger <- 2*sum(matrix_F_larger*weights)/normalization
  G_larger <- 2*sum(matrix_G_larger*weights)/normalization
  F_disp <- 2*sum(matrix_F_disp*weights)/normalization
  G_disp <- 2*sum(matrix_G_disp*weights)/normalization
  
  return(list(cd = cd, 
              F_larger = F_larger, G_larger = G_larger,
              F_disp = F_disp, G_disp = G_disp))
}



##### make function and wrapper
comb_wrapper <- function(single_tarloc_frame){
  # remove any models with NA for values for this target location (assuming all models have the same quantiles)
  single_tarloc<- single_tarloc_frame[ , colSums(is.na(single_tarloc_frame)) == 0]
  tau <- sort(unique(single_tarloc$quantile))
  # pairwise column calculation
  tmp <- single_tarloc %>%
    dplyr::select(-c("target_variable","target_end_date","location","type","quantile","horizon"))
  nc <- ncol(tmp)
  cnames <- colnames(tmp)
  eg <- expand.grid(1:nc, 1:nc)
  nr <- nrow(eg)
  # vector to store cd, and decomp components
  v <- l1 <- l2 <- l3 <- l4 <- vector(length=nr)
  for (i in 1:nr) {
    # have  to change in the future to accommodate the unequal version
    cc <- approx_cd_gen(as.numeric(unlist(tmp[,eg[i,1]])),as.numeric(unlist(tmp[,eg[i,2]])),tau)
    v[i] <- cc[[1]]
    l1[i] <- cc[[2]] 
    l2[i] <- cc[[3]] 
    l3[i] <- cc[[4]] 
    l4[i] <- cc[[5]] 
  }
  single_tarloc_cvm <- data.frame(model_1=rep(cnames,nc),
                                  model_2=rep(cnames,each=nc),
                                  approx_cd=v,
                                  F_larger=l1,
                                  G_larger=l2,
                                  F_disp=l3,
                                  G_disp=l4)
  return(single_tarloc_cvm)
}

##---------------------------------------------- generalization start--------------------------------------------------##
approx_cd_gen <- function(q_F, q_G, tau_G) {
  library(dplyr)
  tau_F <- tau_G
    # check quantile order
    q_F_ordered <- sort(q_F)
    q_G_ordered <- sort(q_G)
    if (sum(q_F != q_F_ordered)>0) {
      warning("q_F has been re-ordered to correspond to increasing probability levels")
    }
    if (sum(q_G != q_G_ordered)>0) {
      warning("q_G has been re-ordered to correspond to increasing probability levels")
    }
    # check probability level order
    tau_F_ordered <- sort(tau_F)
    tau_G_ordered <- sort(tau_G)
    if (sum(tau_F != tau_F_ordered)>0) {
      warning("tau_F has been sorted to in an increasing order")
    }
    if (sum(tau_G != tau_G_ordered)>0) {
      warning("tau_G has been sorted to in an increasing order")
    }
    # check conditions
    if (length(q_F_ordered) != length(tau_F_ordered)) {
      stop("The lengths of q_F_ordered and tau_F_ordered need to be equal")
    }
    if (length(q_G_ordered) != length(tau_G_ordered)) {
      stop("The lengths of q_G_ordered and tau_G_ordered need to be equal")
    }
    if (sum(tau_F_ordered<=1)!=length(tau_F_ordered)|sum(tau_F_ordered>=0)!=length(tau_F_ordered)) {
      stop("The values of tau_F_ordered have to be between 0 and 1")
    }
    if (sum(tau_G_ordered<=1)!=length(tau_G_ordered)|sum(tau_G_ordered>=0)!=length(tau_G_ordered)) {
      stop("The values of tau_G_ordered have to be between 0 and 1")
    }
    if (length(q_F_ordered) != length(q_G_ordered)) {
      message("The lengths of q_F_ordered and q_G_ordered are not equal")
    }
    N <- length(q_F_ordered)
    M <- length(q_G_ordered)
    # pool quantiles:
    q0 <- c(q_F_ordered, q_G_ordered)
    # indicator whether the entry is from F or G
    q <- q0[order(q0)]
    tf <- unlist(sapply(q, function(x) ifelse(x %in% q_F_ordered,tau_F_ordered[which(x == q_F_ordered)],0)))
    tg <- unlist(sapply(q, function(x) ifelse(x %in% q_G_ordered,tau_G_ordered[which(x == q_G_ordered)],0)))
    diffs_q <- diff(q)
    # probability level vectors
    tau_F_v <- cummax(tf)
    tau_G_v <- cummax(tg)
    cvm <- sum(((tau_F_v[1:(N + M) - 1] - tau_G_v[1:(N + M) - 1]) ^ 2) * diffs_q)
    # decompose
    ## make indices
    from_F <- which(tau_F_v!=lead(tau_F_v)&(tau_F_v-tau_G_v)<=0)
    from_G <- which(tau_G_v!=lead(tau_G_v)&(tau_F_v-tau_G_v)>=0)
    disp_F <- which((tau_F_v-tau_G_v)<=0 & tau_F_v==lead(tau_F_v))
    disp_G <- which((tau_F_v-tau_G_v)>=0 & tau_G_v==lead(tau_G_v))
    F_larger <- sum(((tau_F_v[from_F] - tau_G_v[from_F]) ^ 2) * diffs_q[from_F])
    G_larger <- sum(((tau_F_v[from_G] - tau_G_v[from_G]) ^ 2) * diffs_q[from_G])
    F_disp <- sum(((tau_F_v[disp_F] - tau_G_v[disp_F]) ^ 2) * diffs_q[disp_F])
    G_disp <- sum(((tau_F_v[disp_G] - tau_G_v[disp_G]) ^ 2) * diffs_q[disp_G])
      
    # #return
    # if(round(cvm,3)!=round(sum(F_larger,G_larger,F_disp,G_disp),3)){
    #   stop("CD needs to agree or it's wrong")
    # }else{
      return(list(cd = cvm, 
                  F_larger = F_larger, G_larger = G_larger,
                  F_disp = F_disp, G_disp = G_disp))
#    }
}
# # function to compute CD and its decomposition:
# approx_cd_gen <- function(q_F, q_G, tau){
#   # compute quantile levels from length of provided quantile vectors:
#   K <- length(q_F)
#   K_uneven <- K %% 2 == 1
#   if(length(q_G) != K) stop("q_F and q_G need to be of the same length")
#   p <- tau # function assumes that the quantile levels are equally spaced
#   
#   n_intervals <- ceiling(K/2) # to handle uneven number of quantiles
#   # matrices to store interval divergences and components:
#   matrix_interval_comparisons <-
#     matrix_F_larger <- matrix_G_larger <-
#     matrix_F_disp <- matrix_G_disp <-
#     matrix(NA, ncol = n_intervals, nrow = n_intervals,
#            dimnames = list(paste("F", 1:n_intervals), paste("G", 1:n_intervals)))
#   
#   # fill these matrices:
#   for(i in 1:n_intervals){
#     for(j in 1:n_intervals){
#       aF <- p[K + 1 - i] - p[i]
#       aG <- p[K + 1 - j] - p[j]
#       i_comp <- interval_comparison2(l_F = q_F[i], u_F = q_F[K + 1 - i], 
#                                     alpha_F = aF,
#                                     l_G = q_G[j], u_G = q_G[K + 1 - j], 
#                                     alpha_G = aG,
#                                     K=K)
#       matrix_interval_comparisons[i, j] <- i_comp$id
#       matrix_F_larger[i, j] <- i_comp$F_larger
#       matrix_G_larger[i, j] <- i_comp$G_larger
#       matrix_F_disp[i, j] <- i_comp$F_disp
#       matrix_G_disp[i, j] <- i_comp$G_disp
#     }
#   }
#   
#   # uneven weighing
#   weights <- matrix(1, ncol = n_intervals, nrow = n_intervals)
#   if(K_uneven){
#     weights[n_intervals, ] <- weights[, n_intervals] <- 0.5
#     weights[n_intervals, n_intervals] <- 1/4
#   }
#   
#   #normalization <- (K + 0.5)^2
#   normalization <- (K+1)^2
#   cd <- 2*sum(matrix_interval_comparisons*weights)/normalization
#   F_larger <- 2*sum(matrix_F_larger*weights)/normalization
#   G_larger <- 2*sum(matrix_G_larger*weights)/normalization
#   F_disp <- 2*sum(matrix_F_disp*weights)/normalization
#   G_disp <- 2*sum(matrix_G_disp*weights)/normalization
#   
#   return(list(cd = cd, 
#               F_larger = F_larger, G_larger = G_larger,
#               F_disp = F_disp, G_disp = G_disp))
# 
# }
##---------------------------------------------- generalization end --------------------------------------------------##

# wrapper
decomp_wrapper <- function(model_dataframe,horizon_list,target_list){
  main_frame <- model_dataframe %>% 
    dplyr::mutate(horizon = as.numeric(as.character(horizon)),
                  target_variable = as.character(target_variable),
                  location = as.character(location), 
                  target_end_date = as.Date(target_end_date)) %>%
    dplyr::filter(target_variable %in% target_list,
                  horizon %in% horizon_list) 
  if("forecast_date" %in% c(colnames(main_frame))){
    main_frame <- main_frame %>%
      dplyr::select(-"forecast_date")
  }
  ## apply distance_combination function 
  locations <- unique(main_frame$location)
  dist_frame <- data.frame()
  for(loc in locations){
    for(target in target_list){
      for(horiz in horizon_list){
        tar_loc <- main_frame %>%
          dplyr::filter(location==loc,
                        target_variable==target,
                        horizon==horiz) 
        end_dates <- unique(tar_loc$target_end_date)
        for(end_date in end_dates){
          single_matrix <- tar_loc %>%
            dplyr::filter(target_end_date==end_date) %>%
            dplyr::arrange(quantile) %>%
            comb_wrapper(.) %>%
            dplyr::mutate(horizon=horiz,
                          location=loc,
                          target_variable=target,
                          target_end_date=end_date)
          rbind(dist_frame,single_matrix) -> dist_frame
        }
      }
    }
  }
  # calculate mean distance
  mean_frame <- dist_frame %>%
    dplyr::select(-"target_end_date") %>%
    dplyr::group_by(horizon,location,target_variable,model_1,model_2) %>%
    dplyr::mutate(mean_dis=mean(approx_cd),
                  mean_F_larger=mean(F_larger),
                  mean_G_larger=mean(G_larger),
                  mean_F_disp=mean(F_disp),
                  mean_G_disp=mean(G_disp)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c("approx_cd","F_larger","G_larger","F_disp","G_disp")) %>%
    dplyr::arrange(factor(model_2, levels = unique(model_2))) %>%
    distinct()
  mean_locframe <- dist_frame %>%
    dplyr::select(-"target_end_date") %>%
    dplyr::group_by(horizon,target_variable,model_1,model_2) %>%
    dplyr::mutate(mean_dis=mean(approx_cd),
                  mean_F_larger=mean(F_larger),
                  mean_G_larger=mean(G_larger),
                  mean_F_disp=mean(F_disp),
                  mean_G_disp=mean(G_disp)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c("approx_cd","location","F_larger","G_larger","F_disp","G_disp")) %>%
    dplyr::arrange(factor(model_2, levels = unique(model_2))) %>%
    distinct()
  return(list(full_dataframe=dist_frame,
              mean_dataframe=mean_frame,
              loc_mean = mean_locframe))
}

# # Examples to check agreement:
# # with even K:
# K <- 10
# p <- (1:K)/(K + 1) # quantile levels
# q_F <- qnorm(p, 12, 5) # quantiles of F
# q_G <- qnorm(p, 9, 4) #
# approx_cd0(q_F, q_G)
# approx_cd(q_F, q_G)
# # with uneven K:
# K <- 9
# p <- (1:K)/(K + 1) # quantile levels
# q_F <- qnorm(p, 12, 5) # quantiles of F
# q_G <- qnorm(p, 9, 4) #
# approx_cd0(q_F, q_G)
# approx_cd(q_F, q_G)
