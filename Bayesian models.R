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


###Intercept only model
bvnorm1 <- list(N = N, Y = Y, R = R)

#Sampler
eq5dnox <- nimbleCode({
  #Model
  for (i in 1:N) {
    Y[i,1:2] ~ dmnorm(mean = mu[i,1:2], prec = Omega[1:2,1:2])
    mu[i,1] <- beta[1]
    mu[i,2] <- beta[1] + beta[2]
  }
  beta[1] ~ dbeta(1,1)
  beta[2] ~ dbeta(1,1)
  Omega[1:2,1:2] ~ dwish(R[1:2,1:2], 2)
  sigma[1:2, 1:2] <- inverse(Omega[1:2, 1:2])
})

inits <- list(beta = c(mean(df$EQ5D), mean(df$EQVAS)), Omega = R)
inits = rep(list(inits), n.chains) # repeat initial values per chain

nimble1 <- nimbleMCMC(code = eq5dnox, 
                      data = bvnorm1,
                      inits = inits, 
                      nchains = n.chains, 
                      niter = MCMC*n.chains*thin,
                      setSeed = 88888)



###With X variable 'sex'
bvnorm2 <- list(N = N, Y = Y, R = R, sex = df$sex)

#Sampler
eq5dyesx <- nimbleCode({
  #Model
  for (i in 1:N) {
    Y[i,1:2] ~ dmnorm(mean = mu[i,1:2], prec = Omega[1:2,1:2])
    mu[i,1] <- beta[1] + beta[3]*sex[i]
    mu[i,2] <- beta[1] + beta[2] + beta[3]*sex[i]
  }
  beta[1] ~ dbeta(1,1)
  beta[2] ~ dbeta(1,1)
  beta[3] ~ dnorm(0,10^5)
  Omega[1:2,1:2] ~ dwish(R[1:2,1:2], 2)
  sigma[1:2, 1:2] <- inverse(Omega[1:2, 1:2])
})

inits2 <- list(beta = c(mean(df$EQ5D), mean(df$EQVAS), 0), Omega = R)
inits2 = rep(list(inits2), n.chains) # repeat initial values per chain

nimble2 <- nimbleMCMC(code = eq5dyesx, 
                      data = bvnorm2,
                      inits = inits2, 
                      nchains = n.chains, 
                      niter = MCMC*n.chains*thin,
                      setSeed = 88888)
