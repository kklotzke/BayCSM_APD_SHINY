# Bayesian Covariance Structure Modelling
# Gibbs-sampler for Shiny BCSM app
#
# Konrad Klotzke
# University of Twente
# January 2020

library(extraDistr)
library(invgamma)
#library(rmv)

SamplerBCSMShiny <- function(Y, u, XG, a0, b0, print = TRUE, ii.print = 100, ii.progress = 25, pbid = NULL, session = NULL) {

  N <- nrow(Y) # Number of test-takers
  p <- ncol(Y) # Number of items
  Nt <- nrow(u) # Number of covariance layers

  Z <- Z0 <- Z00 <- matrix(0, nrow = N, ncol = p)
  Samples.tcov <- matrix(NA, nrow = XG, ncol = Nt) # Covariances
  Samples.iinter <- matrix(NA, nrow = XG, ncol = p) # Item intercepts
  Samples.tcov[1, ] <- 0.0
  Samples.iinter[1, ] <- 0.0

  loopi <- 0
  ii <- 2
  while (ii <= XG)  {

    success <- FALSE
    while (!success) {
      sig2k <- rep(1, p)
      tcov <- Samples.tcov[ii-1, ]
      iinter <- Samples.iinter[ii-1, ]
      B.sum <- -iinter

      # if(print & ii > 3) {
      #   if (!is.null(ncol(Samples.tcov)))
      #     print(cat(format(ii, digits = 0), format(round(colMeans(Samples.tcov[2:(ii-1), ]), 2), digits=2)))
      #   else # Single parameter
      #     print(cat(format(ii, digits = 0), format(round(mean(Samples.tcov[2:(ii-1), ]), 2), digits=2)))
      # }

      # Latent responses
      Sigma <- diag(sig2k) # Base layer
      for (tt in 1:Nt)  # Testlet layers
        Sigma <- Sigma + u[tt, ]%*%t(u[tt, ])*tcov[tt]
      for (kk in 1:p) {
        condition.on <- (1:p)[-kk] # condition on Z[-kk]
        # A <- Sigma.inv <- diag(1/sig2k.full[condition.on])
        # for (tt in 1:Nt) {
        #   A.min1 <- A
        #   A <- A.min1 - A.min1 %*% u[tt, condition.on] %*% t(u[tt, condition.on]) %*% A.min1 / ((1/tcov[tt] + t(u[tt, condition.on]) %*% A.min1 %*% u[tt, condition.on])[1,1])
        # }
        #Sigma.inv <- A
        Sigma.inv <- solve(Sigma[condition.on,condition.on]) # faster, as it is coded in rcpp
        #print(all.equal(solve(Sigma[condition.on,condition.on]), Sigma.inv))
        B11 <- Sigma[kk, kk]
        B12 <- Sigma[kk, condition.on]
        B21 <- Sigma[condition.on, kk]
        B22.inv <- Sigma.inv
        mu <- B.sum[kk] + B12 %*% B22.inv %*% (matrix(t(Z[, condition.on]), nrow = p-1, ncol = N) - matrix(B.sum[condition.on], nrow = p-1, ncol = N, byrow = FALSE))
        sig2 <- B11 - B12 %*% B22.inv %*% B21
        Z[Y[,kk]==0, kk] <- extraDistr::rtnorm(n = length(mu[1, Y[, kk]==0]), mean = mu[1, Y[, kk]==0], sd = sqrt(sig2[1,1]), a = -Inf, b = 0)
        Z[Y[,kk]==1, kk] <- extraDistr::rtnorm(n = length(mu[1, Y[, kk]==1]), mean = mu[1, Y[, kk]==1], sd = sqrt(sig2[1,1]), a = 0, b = Inf)
      }
      Z.star <- Z - matrix(B.sum, nrow = N, ncol = p, byrow = TRUE)

      # Sum of squares for covariance parameters
      SSB <- numeric(Nt)
      for (tt in 1:(Nt)) {
        SSB[tt] <- sum((rowMeans(Z.star[, as.logical(u[tt, ])]) - mean(Z.star[, as.logical(u[tt, ])]))^2)
      }

      ## Covariance structure
      # Draw Nt covariance parameters from truncated shifted IG
      tdraw.fallback <- NULL
      A <- Sigma.inv <- diag(1/sig2k) # Base layer inverse
      layers <- 1:Nt
      layers <- sample(layers) # May speed up convergence in complex models
      for (tt in layers) {
        tmp <- 0
        for (xx in (1:Nt)[-tt]) {
          tmp <- tmp + tcov[xx]*sum(as.logical(u[xx, ]) * as.logical(u[tt, ]))^2
        }
        shift <- (sum(sig2k[as.logical(u[tt,])]) + tmp)/sum(u[tt,])^2
        tr <- -1 / (t(u[tt,]) %*% A %*% u[tt, ])[1,1]
        tdraws <- invgamma::rinvgamma(10000, shape = a0 + N/2, rate = b0 + SSB[tt]/2) - shift
        tdraws <- tdraws[is.finite(tdraws)]
        tdraws <- tdraws[tdraws > tr] # Truncate
        rr <- 0
        while(length(tdraws) == 0 & rr <= 20) { # Sample more if needed
          tdraws <- invgamma::rinvgamma(1000000, shape = a0 + N/2, rate = b0 + SSB[tt]/2) - shift
          tdraws <- tdraws[is.finite(tdraws)]
          tdraws <- tdraws[tdraws >= tr] # Truncate
          rr <- rr + 1
        }
        if(length(tdraws) == 0)
          tdraws <- tr + 0.0001 # Sampler is at the edge of parameter space
        tcov[tt] <- Samples.tcov[ii, tt] <- tdraws[1]
        A.min1 <- A
        A <- A.min1 - A.min1 %*% u[tt, ] %*% t(u[tt, ]) %*% A.min1 / ((1/tcov[tt] + t(u[tt, ]) %*% A.min1 %*% u[tt, ])[1,1])
      }
      Sigma <- diag(sig2k) # Base layer
      for (tt in 1:Nt)  # Testlet layers
        Sigma <- Sigma + u[tt, ]%*%t(u[tt, ])*tcov[tt]

      # Fallback code - R's limited numerical precision can cause rounding problems
      # Usually not necessary, just to prevent the sampler from crashing because of a single iteration
      # I'll leave it in for now
      if (all(is.finite(tcov)) && (all(eigen(Sigma)$values > 0)) && (all(tcov < 2) | ii < 100)) {
        loopi <- 0
        Z00 <- Z0
        Z0 <- Z
        success <- TRUE
      }
      else {
        loopi <- loopi + 1
        if (loopi == 5) {
          loopi <- 0
          ii <- ii - 1
          Z <- Z0
          Z0 <- Z00
        }
        if (ii < 2)
          ii <- 2
      }
    }

    if (success) {
      ## Mean structure
      # Item intercepts
      Samples.iinter[ii, ] <- -Sample.IInter(Z = Z, Sigma1 = Sigma, mu0 = 0, var0 = 10^10)

      if ((ii%%ii.print == 0) & print)
        cat("Iteration ", ii, " ", "\n")
      if (all(!is.null(c(pbid, session))) & (ii%%ii.progress == 0)) {
        updateProgressBar(session = session, id = pbid, value = ii, total = XG, title = "MCMC process: running")
      }

      ii <- ii + 1
    }

  }
  if (all(!is.null(c(pbid, session))) & ((ii-1) == XG)) {
    updateProgressBar(session = session, id = pbid, value = XG, total = XG, title = "MCMC process: finished")
  }

  colnames(Samples.tcov) <- rownames(u)
  return(list("Samples.tcov" = Samples.tcov, "Samples.iinter" = Samples.iinter, "u" = u, "XG" = XG, "a0" = a0, "b0" = b0))
}

Sample.IInter <- function (Z, Sigma1, mu0 = 0, var0 = 10^10) {
  N <- nrow(Z)
  p <- ncol(Z)
  var1 <- 1 / (N/diag(Sigma1) + 1/var0)
  mu1 <- var1 * (N*colMeans(Z) / diag(Sigma1) + mu0/var0)
  draw <- rnorm(p, mu1, sqrt(var1))
  draw <- draw - mean(draw)
  return(draw)
}