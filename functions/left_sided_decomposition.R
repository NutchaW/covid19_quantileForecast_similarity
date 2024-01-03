calc_cramers_dist_unequal_space <-function(q_F, tau_F, q_G, tau_G) {
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
    # calculating distance
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
    # get decomposition
    F_larger <- c()
    G_larger <- c()
    F_disp <- c()
    G_disp <- c() 
    if () {
      value <- diffs_q
      F_larger <- c(F_larger,value)
    } else if () {
      value <- diffs_q
      G_larger <- c(G_larger,value)
    } else if () {
      value <- diffs_q
      F_disp <- c(F_disp,value)
    } else {
      value <- diffs_q
      G_disp <- c(G_disp,value)
    }
    # sum
    #id <- sum(((tau_F_v[1:(N + M) - 1] - tau_G_v[1:(N + M) - 1]) ^ 2) * diffs_q)
    id <- sum(F_larger) + sum(G_larger) + sum(F_disp) + sum(G_disp)
    return(list(id = id,
                F_larger =sum(F_larger),
                G_larger = sum(G_larger),
                F_disp = sum(F_disp),
                G_disp = sum(G_disp)))
  }