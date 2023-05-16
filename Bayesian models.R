#Test to run the Bayesian bivariate normal models on their own first
options(scipen = 5)

#Load data in
df <- read.csv(file = "eq5dsrilanka.csv", sep = ",")
df$EQ5D_VAS <- as.numeric(df$EQ5D_VAS)
df <- na.omit(df)
df$EQVAS <- df$EQ5D_VAS/100

#Set up
library(nimble)

MCMC = 5000
thin=3
n.chains=2

N = nrow(df)
Y = as.matrix(dplyr::select(df, "EQ5D", "EQVAS"))
R = matrix(data = c(1,0,0,1), nrow = 2)

bvnorm <- list(N = N, Y = Y, R = R)

#Sampler
eq5dcode <- nimbleCode({
  #Model
  for (i in 1:N) {
    Y[i,1:2] ~ dmnorm(mean = mu[i,1:2], prec = Omega[1:2,1:2])
    mu[i,1] <- beta[1]
    mu[i,2] <- beta[1]
  }
  beta[1] ~ dbeta(1,1)
  Omega[1:2,1:2] ~ dwish(R[1:2,1:2], 2)
  sigma[1:2, 1:2] <- inverse(Omega[1:2, 1:2])
})

inits <- list(beta = c(mean(df$EQ5D), mean(df$EQVAS)), Omega = R)
inits = rep(list(inits), n.chains) # repeat initial values per chain

nimbleMCMC_samples <- nimbleMCMC(code = eq5dcode,
                                 data = bvnorm,
                                 inits = inits, 
                                 nchains = n.chains, 
                                 niter = MCMC*n.chains*thin,
                                 setSeed = 88888)
