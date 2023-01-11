# Generate data for multidimensional multi-factor scale structure

library(MASS)

GenerateData <- function(N) {

  p <- 48 # Number of items  
  Nt <- 14 # Number of covariance layers
  
  # Classification matrix
  u <- matrix(0, nrow = Nt, ncol = p)
  u[1, ] <- 1 # General math ability (f1)
  u[2, 1:24] <- 1 # Informal mathematics (f2.1)
  u[3, 25:48] <- 1 # Formal mathematics (f2.2)
  u[4, 1:7] <- 1 # Numbering (f8.1)
  u[5, 8:13] <- 1 # Number comparisons (f8.2)
  u[6, 14:18] <- 1 # Calculation (f8.3)
  u[7, 19:24] <- 1 # Concepts (f8.4)
  u[8, 25:30] <- 1 # Numeral literacy (f8.5)
  u[9, 31:35] <- 1 # Number facts (f8.6)
  u[10, 36:43] <- 1 # Calculation (f8.7)
  u[11, 44:48] <- 1 # Concepts (f8.8)
  u[12, c(1:24, 25, 31, 37, 43)] <- 1 # Presentation format text (PF.1)
  u[13, c(seq(26, 48, by = 6), seq(27, 48, by = 6), seq(28, 48, by = 6))] <- 1 # Presentation format text + image (PF.2)
  u[14, c(seq(29, 48, by = 6), seq(30, 48, by = 6))] <- 1 # Presentation format image (PF.3)
  
  cov1 <- numeric(Nt) # Covariances 
  cov1[1] <- 0.04 # f1
  cov1[2] <- 0.02 # f2.1
  cov1[3] <- 0.02 # f2.2
  cov1[4] <- 0.01 # f8.1
  cov1[5] <- 0.01 # f8.2
  cov1[6] <- 0.05 # f8.3
  cov1[7] <- 0.06 # f8.4
  cov1[8] <- 0.05 # f8.5
  cov1[9] <- 0.01 # f8.6
  cov1[10] <- 0.07 # f8.7
  cov1[11] <- 0.05 # f8.8
  cov1[12] <- -0.07 # PF.1
  cov1[13] <- 0.00 # PF.2
  cov1[14] <- 0.01 # PF.3
  
  mu.items <- runif(p, 0, 1.5) # Item difficulties 
  mu.items <- mu.items - mean(mu.items)
  mu.group <- 0 # Single group; fixed to zero
  mu.testlets <- 0 # Fixed to zero for simplicity 
  mu1 <- -mu.items # Mean structure
  Sigma1 <- diag(p) # Covariance structure
  # for (tt in 1:6) {
  #   mu1 <- mu1 + u[tt, ]*mu.testlets[tt]
  # }
  for (tt in 1:Nt) {
    Sigma1 <- Sigma1 + u[tt, ]%*%t(u[tt, ])*cov1[tt]
  }
  
  Z <- MASS::mvrnorm(n = N, mu = mu1, Sigma = Sigma1, empirical = TRUE) # Generate data
  Y <- matrix(0, nrow = N, ncol = p) # Dichotomous outcome variables
  Y[which(Z > 0)] <- 1
  
  rownames(Y) <- paste0("Respondent.", 1:N)
  colnames(Y) <- paste0("Item.", 1:p)
  
  return (list("Y" = Y, "u" = u, "N" = N, "p" = p, "Nt" = Nt, "cov1" = cov1, "mu.items" = mu.items, "mu1" = mu1, "Sigma1" = Sigma1))

}