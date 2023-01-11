# Test sampler
library(coda)
library(mcmcplots)

rm(list = ls())
source ("BCSM Sampler.R")
source ("Construct Classification.R")
source ("Generate Data.R")

set.seed(1)

XG <- 1000
Burnin <- 100
a0 <- 0.00001
b0 <- 0.00001

# Generate data under full model
dat <- GenerateData(N = 750)

# Fit full model (QF)
out <- SamplerBCSMShiny(Y = dat$Y, u = ConstructCM(f1 = TRUE, f2 = TRUE, f8 = TRUE, PF = TRUE)$u1, 
                        XG = XG, a0 = a0, b0 = b0, print = TRUE, ii.print = 100)
cov1.out <- colMeans(out$Samples.tcov[Burnin:XG,])
mu1.items.out <- colMeans(out$Samples.iinter[Burnin:XG,])

# Parameter estimates
round(dat$cov1 - cov1.out, 2)
summary(dat$mu.items - mu1.items.out)

# Diagnostics
mcmc.cov1 <- coda::mcmc(out$Samples.tcov)
mcmcplots::mcmcplot(mcmc.cov1)
coda::effectiveSize(mcmc.cov1)


## Fit models 
# UD (f1)
out <- SamplerBCSMShiny(Y = dat$Y, u = ConstructCM(f1 = TRUE, f2 = FALSE, f8 = FALSE, PF = FALSE)$u1, 
                        XG = XG, a0 = a0, b0 = b0, print = TRUE, ii.print = 100)
mean(out$Samples.tcov[Burnin:XG,])

# MD (f2)
out <- SamplerBCSMShiny(Y = dat$Y, u = ConstructCM(f1 = FALSE, f2 = TRUE, f8 = FALSE, PF = FALSE)$u1, 
                        XG = XG, a0 = a0, b0 = b0, print = TRUE, ii.print = 100)
colMeans(out$Samples.tcov[Burnin:XG,])
coda::HPDinterval(coda::mcmc(out$Samples.tcov), prob = 0.95)

# MD (f8)
out <- SamplerBCSMShiny(Y = dat$Y, u = ConstructCM(f1 = FALSE, f2 = FALSE, f8 = TRUE, PF = FALSE)$u1, 
                        XG = XG, a0 = a0, b0 = b0, print = TRUE, ii.print = 100)
colMeans(out$Samples.tcov[Burnin:XG,])
coda::HPDinterval(coda::mcmc(out$Samples.tcov), prob = 0.95)

# MD (PF)
out <- SamplerBCSMShiny(Y = dat$Y, u = ConstructCM(f1 = FALSE, f2 = FALSE, f8 = FALSE, PF = TRUE)$u1, 
                        XG = XG, a0 = a0, b0 = b0, print = TRUE, ii.print = 100)
colMeans(out$Samples.tcov[Burnin:XG,])
coda::HPDinterval(coda::mcmc(out$Samples.tcov), prob = 0.95)

# BF (f1 + f2)
out <- SamplerBCSMShiny(Y = dat$Y, u = ConstructCM(f1 = TRUE, f2 = TRUE, f8 = FALSE, PF = FALSE)$u1, 
                        XG = XG, a0 = a0, b0 = b0, print = TRUE, ii.print = 100)
colMeans(out$Samples.tcov[Burnin:XG,])
coda::HPDinterval(coda::mcmc(out$Samples.tcov), prob = 0.95)

# BF (f1 + f8)
out <- SamplerBCSMShiny(Y = dat$Y, u = ConstructCM(f1 = TRUE, f2 = FALSE, f8 = TRUE, PF = FALSE)$u1, 
                        XG = XG, a0 = a0, b0 = b0, print = TRUE, ii.print = 100)
colMeans(out$Samples.tcov[Burnin:XG,])
coda::HPDinterval(coda::mcmc(out$Samples.tcov), prob = 0.95)

# BF (f2 + f8)
out <- SamplerBCSMShiny(Y = dat$Y, u = ConstructCM(f1 = FALSE, f2 = TRUE, f8 = TRUE, PF = FALSE)$u1, 
                        XG = XG, a0 = a0, b0 = b0, print = TRUE, ii.print = 100)
colMeans(out$Samples.tcov[Burnin:XG,])
coda::HPDinterval(coda::mcmc(out$Samples.tcov), prob = 0.95)

# TF (f1 + f2 + PF)
out <- SamplerBCSMShiny(Y = dat$Y, u = ConstructCM(f1 = TRUE, f2 = TRUE, f8 = FALSE, PF = TRUE)$u1, 
                        XG = XG, a0 = a0, b0 = b0, print = TRUE, ii.print = 100)
colMeans(out$Samples.tcov[Burnin:XG,])
coda::HPDinterval(coda::mcmc(out$Samples.tcov), prob = 0.95)

# TF (f1 + f8 + PF)
out <- SamplerBCSMShiny(Y = dat$Y, u = ConstructCM(f1 = TRUE, f2 = FALSE, f8 = TRUE, PF = TRUE)$u1, 
                        XG = XG, a0 = a0, b0 = b0, print = TRUE, ii.print = 100)
colMeans(out$Samples.tcov[Burnin:XG,])
coda::HPDinterval(coda::mcmc(out$Samples.tcov), prob = 0.95)

# TF (f2 + f8 + PF)
out <- SamplerBCSMShiny(Y = dat$Y, u = ConstructCM(f1 = FALSE, f2 = TRUE, f8 = TRUE, PF = TRUE)$u1, 
                        XG = XG, a0 = a0, b0 = b0, print = TRUE, ii.print = 100)
colMeans(out$Samples.tcov[Burnin:XG,])
coda::HPDinterval(coda::mcmc(out$Samples.tcov), prob = 0.95)

# TF (f1 + f2 + f8)
out <- SamplerBCSMShiny(Y = dat$Y, u = ConstructCM(f1 = TRUE, f2 = TRUE, f8 = TRUE, PF = FALSE)$u1, 
                        XG = XG, a0 = a0, b0 = b0, print = TRUE, ii.print = 100)
colMeans(out$Samples.tcov[Burnin:XG,])
coda::HPDinterval(coda::mcmc(out$Samples.tcov), prob = 0.95)

# QF (f1 + f2 + f8 + PF)
out <- SamplerBCSMShiny(Y = dat$Y, u = ConstructCM(f1 = TRUE, f2 = TRUE, f8 = TRUE, PF = TRUE)$u1, 
                        XG = XG, a0 = a0, b0 = b0, print = TRUE, ii.print = 100)
colMeans(out$Samples.tcov[Burnin:XG,])
coda::HPDinterval(coda::mcmc(out$Samples.tcov), prob = 0.95)


## Test classification structure construction
CM <- ConstructCM(f1 = TRUE, f2 = TRUE, f8 = TRUE, PF = TRUE)
View(CM$u1)
CM$names
CM$modelid

# UD
ConstructCM(f1 = TRUE, f2 = FALSE, f8 = FALSE, PF = FALSE)$modelid

# MD
ConstructCM(f1 = FALSE, f2 = TRUE, f8 = FALSE, PF = FALSE)$modelid
ConstructCM(f1 = FALSE, f2 = FALSE, f8 = TRUE, PF = FALSE)$modelid
ConstructCM(f1 = FALSE, f2 = FALSE, f8 = FALSE, PF = TRUE)$modelid

# BF
ConstructCM(f1 = TRUE, f2 = TRUE, f8 = FALSE, PF = FALSE)$modelid
ConstructCM(f1 = TRUE, f2 = FALSE, f8 = TRUE, PF = FALSE)$modelid
ConstructCM(f1 = TRUE, f2 = FALSE, f8 = FALSE, PF = TRUE)$modelid
ConstructCM(f1 = FALSE, f2 = TRUE, f8 = TRUE, PF = FALSE)$modelid
ConstructCM(f1 = FALSE, f2 = TRUE, f8 = FALSE, PF = TRUE)$modelid
ConstructCM(f1 = FALSE, f2 = FALSE, f8 = TRUE, PF = TRUE)$modelid

# TF
ConstructCM(f1 = FALSE, f2 = TRUE, f8 = TRUE, PF = TRUE)$modelid
ConstructCM(f1 = TRUE, f2 = FALSE, f8 = TRUE, PF = TRUE)$modelid
ConstructCM(f1 = TRUE, f2 = TRUE, f8 = FALSE, PF = TRUE)$modelid
ConstructCM(f1 = TRUE, f2 = TRUE, f8 = TRUE, PF = FALSE)$modelid

# QF
ConstructCM(f1 = TRUE, f2 = TRUE, f8 = TRUE, PF = TRUE)$modelid

# Empty 
ConstructCM(f1 = FALSE, f2 = FALSE, f8 = FALSE, PF = FALSE)$modelid
