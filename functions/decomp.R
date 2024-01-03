# Examples to check agreement:
# with even K:
K <- 10
p <- (1:K)/(K + 1) # quantile levels
q_F <- qnorm(p, 12, 5) # quantiles of F
q_G <- qnorm(p, 9, 4) #
#approx_cd0(q_F, q_G)
#calc_cramers_dist_equal_space(q_F, p, q_G, p, "approximation1")
calc_cramers_dist_unequal_space(q_F, p, q_G, p, "left_sided_riemann")
approx_cd(q_F, q_G)
approx_cd_gen(q_F, q_G,p)

# with uneven K:
K <- 9
p <- (1:K)/(K + 1) # quantile levels
q_F <- qnorm(p, 12, 5) # quantiles of F
q_G <- qnorm(p, 9, 4) #
#approx_cd0(q_F, q_G)
calc_cramers_dist_unequal_space(q_F, p, q_G, p, "left_sided_riemann")
approx_cd_gen(q_F, q_G,p)

# with uneven K:
K <- 9
p <- (1:K)/(K + 1) # quantile levels
q_G <- qnorm(p, 12, 5) # quantiles of F
q_F <- qnorm(p, 9, 4) #
#approx_cd0(q_F, q_G)
calc_cramers_dist_unequal_space(q_F, p, q_G, p, "left_sided_riemann")
approx_cd_gen(q_F, q_G,p)