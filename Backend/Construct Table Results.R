# Construct result tables 

library(coda)

ConstructTableResults <- function ( samples, u, XG, burnin, fid, fdescr, CI.type = c("HPD"), CI.prob, digits = 3 ) {
  
  colSd <- function(x, na.rm=FALSE) apply(X = x, MARGIN = 2, FUN = sd, na.rm = na.rm)
  CIind <- function(x) {
    if (all(c(x[1], x[2]) > 0)) 
      return (1)
    else if (all(c(x[1], x[2]) < 0))
      return (-1)
    else 
      return (0)
  }

  samples.post <- as.matrix(samples[(floor(as.numeric(burnin)*as.numeric(XG)/100)+1):as.numeric(XG), ])
  HPD <- HPDinterval(mcmc(samples.post), prob = as.numeric(CI.prob)/100)
  CI.type.lower <- 
  
  tab <- data.frame("Factor.id" = fid, "Describtion" = fdescr, "Items" = rowSums(u), "Post.mean" = colMeans(samples.post) 
                    , "SD" = colSd(samples.post), "CI.lower" = HPD[ ,1], "CI.upper" = HPD[ ,2]
                    , "CI.ind" = apply(HPD, 1, CIind)) 
  tab[, 4:7] <- round(tab[, 4:7], digits = digits)
  
  return(tab)
  
}