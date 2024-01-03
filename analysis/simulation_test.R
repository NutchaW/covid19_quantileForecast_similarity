# Simulation
# 1) over all simulation
## Group difference
source("./functions/sim_functions.R")
set.seed(1234) #for reproducability
S <- 10000
nG <- 2  #number of groups
nJ <- 7  #cluster size (group size)
nL <- 5 # num location
B <- 3000
alpha_levels <- round(c(0.010,0.025,seq(0.050,0.950,0.050),0.975,0.990), digits = 3)
g1_forecasts <- g2_forecasts <- matrix(NA,nrow=nJ*nL,ncol=length(alpha_levels))
### simulate by group, allowing some within group variation
#### shared sigma
sig <- abs(rnorm(1, 0, 1))
#### group params
mu1 <- rnorm(nJ,100,10)
sd1 <- rnorm(nL,10,3)
mu2 <- rnorm(nJ,120,10)
sd2 <- rnorm(nL,25,5)
for (i in 1:nJ){
  for (j in 1:nL){
    g1_forecasts[((i-1)*nL)+j,] <- qnorm(alpha_levels, mu1[i] + sd1[j], sig)
    g2_forecasts[((i-1)*nL)+j,] <- qnorm(alpha_levels, mu2[i] + sd2[j], sig)
  }
}
group_vec <-rep(1:2,each=nJ)
forecast_mat <- data.frame(rbind(cbind(group_vec,rbind(g1_forecasts[1:5,],g2_forecasts[1:5,])),
                                   cbind(group_vec,rbind(g1_forecasts[6:10,],g2_forecasts[6:10,])),
                                   cbind(group_vec,rbind(g1_forecasts[11:15,],g2_forecasts[11:15,])),
                                   cbind(group_vec,rbind(g1_forecasts[16:20,],g2_forecasts[16:20,])),
                                   cbind(group_vec,rbind(g1_forecasts[21:25,],g2_forecasts[21:25,])),
                                   cbind(group_vec,rbind(g1_forecasts[21:25,],g2_forecasts[21:25,])),
                                   cbind(group_vec,rbind(g1_forecasts[21:25,],g2_forecasts[21:25,]))
                                   )) %>%
  dplyr::mutate(location=rep(LETTERS[1:5],each=nL*2),
                model=rep(1:10,nL))
forecast_mat <- shuff_mat <- forecast_mat %>%
  tidyr::pivot_longer(!c("group_vec","location","model"),names_to = "quantile", values_to = "value") %>%
  dplyr::mutate(quantile=rep(alpha_levels,50))
# calculate distances
v1 <- matrix(NA,nrow=20*nL,ncol=B)
v2 <- matrix(NA,nrow=25*nL,ncol=B)
for(b in 1:B){
  shuf_ind <- sample(group_vec, size = nJ*2, replace = FALSE)
  for(h in 1:4){
    shuff_mat$group_vec <- rep(rep(shuf_ind,each=length(alpha_levels)),nL)
    datd <- build_dist_frame(shuff_mat,"left_sided_riemann",alpha_levels)
    v1[,b] <- datd$approx_cd[datd$type=="within"]  
    v2[,b] <- datd$approx_cd[datd$type=="between"]  
  }
}

# calculate observed t
obs_dat <- build_dist_frame(forecast_mat,"left_sided_riemann",alpha_levels)
v1o <- obs_dat$approx_cd[obs_dat$type=="within"]  
v2o <- obs_dat$approx_cd[obs_dat$type=="between"]  
# calculate p
perm_t <- unlist(lapply(1:B, function(i) median(v1[,i])/median(v2[,i])))
obs_t <- median(v1o)/median(v2o)
p <- sum(perm_t <= obs_t)/B
type1er <-
# plot
hist(p)

## equal
#### group params
mu1 <- rnorm(nJ,100,10)
sd1 <- rnorm(nL,10,3)
mu2 <- rnorm(nJ,120,10)
sd2 <- rnorm(nL,25,5)
for (i in 1:nJ){
  for (j in 1:nL){
    g1_forecasts[((i-1)*nL)+j,] <- qnorm(alpha_levels, mu1[i] + sd1[j], sig)
    g2_forecasts[((i-1)*nL)+j,] <- qnorm(alpha_levels, mu2[i] + sd2[j], sig)
  }
}
group_vec <-rep(1:2,each=nJ)
forecast_mat <- data.frame(rbind(cbind(group_vec,rbind(g1_forecasts[1:5,],g2_forecasts[1:5,])),
                                 cbind(group_vec,rbind(g1_forecasts[6:10,],g2_forecasts[6:10,])),
                                 cbind(group_vec,rbind(g1_forecasts[11:15,],g2_forecasts[11:15,])),
                                 cbind(group_vec,rbind(g1_forecasts[16:20,],g2_forecasts[16:20,])),
                                 cbind(group_vec,rbind(g1_forecasts[21:25,],g2_forecasts[21:25,]))
)) %>%
  dplyr::mutate(location=rep(LETTERS[1:5],each=nL*2),
                model=rep(1:10,nL))
forecast_mat <- shuff_mat <- forecast_mat %>%
  tidyr::pivot_longer(!c("group_vec","location","model"),names_to = "quantile", values_to = "value") %>%
  dplyr::mutate(quantile=rep(alpha_levels,50))
# calculate distances
v1 <- matrix(NA,nrow=20*nL,ncol=B)
v2 <- matrix(NA,nrow=25*nL,ncol=B)
for(b in 1:B){
  shuf_ind <- sample(group_vec, size = nJ*2, replace = FALSE)
  shuff_mat$group_vec <- rep(rep(shuf_ind,each=length(alpha_levels)),nL)
  datd <- build_dist_frame(shuff_mat,"left_sided_riemann",alpha_levels)
  v1[,b] <- datd$approx_cd[datd$type=="within"]  
  v2[,b] <- datd$approx_cd[datd$type=="between"]  
}

# calculate observed t
obs_dat <- build_dist_frame(forecast_mat,"left_sided_riemann",alpha_levels)
v1o <- obs_dat$approx_cd[obs_dat$type=="within"]  
v2o <- obs_dat$approx_cd[obs_dat$type=="between"]  
# calculate p
perm_t <- unlist(lapply(1:B, function(i) median(v1[,i])/median(v2[,i])))
obs_t <- median(v1o)/median(v2o)
p <- sum(perm_t <= obs_t)/B

