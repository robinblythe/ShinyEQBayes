#run_nimble_with_sex.R
#Takes inputs from the UI:
#CSV(utility,VAS,sex)
#MCMC iterations, chains, thinning, burn in

run_nimble_with_sex = function(data, vareq5d, vareqvas, sex, MCMC, n.chains, burnin, thin, seed){
  
  #Define inputs
  utility <- as.numeric(data[[vareq5d]])
  vas <- as.numeric(data[[vareqvas]])/100
  sex <- as.numeric(data[[sex]])
  
  Y <- na.omit(as.matrix(cbind(utility, vas)))
  R <- matrix(data = c(1,0,0,1), nrow = 2)
  N <- nrow(Y)
  
  #Data
  normqs <- list(N = N, Y = utility, sex = sex)
  normvas <- list(N = N, Y = vas, sex = sex)
  bvnorm <- list(N = N, Y = Y, R = R, sex = sex)
  
  eq5d <- nimbleCode({
    #Model 
    for (i in 1:N) {
      Y[i] ~ dnorm(mean = mu[i], tau)
      mu[i] <- beta[1] + beta[2]*sex[i]
      }
    beta[1] ~ dbeta(1,1)
    beta[2] ~ dnorm(0,10000)
    tau ~ dgamma(1,1)
    })
  
  eq5dboth <- nimbleCode({
    #Model 
    for (i in 1:N) {
      Y[i,1:2] ~ dmnorm(mean = mu[i,1:2], prec = Omega[1:2,1:2])
      mu[i,1] <- beta[1] + beta[3]*sex[i]
      mu[i,2] <- beta[1] + beta[2] + beta[3]*sex[i]
    }
    beta[1] ~ dbeta(1,1)
    beta[2] ~ dbeta(1,1)
    beta[3] ~ dnorm(0,10000)
    Omega[1:2,1:2] ~ dwish(R[1:2,1:2], 2)
    sigma[1:2, 1:2] <- inverse(Omega[1:2, 1:2])
  })
  
  constants <- list(N = N)
  inits1 <- list(beta = c(mean(utility, na.rm = T), 0), tau = 1)
  inits1 <- rep(list(inits1), n.chains) # repeat initial values per chain
  inits2 <- list(beta = c(mean(Y[,1]), mean(Y[,2]), 0), Omega = R)
  inits2 = rep(list(inits2), n.chains) # repeat initial values per chain
  
  # nimblereg2 <- c(
  #   qs = nimbleMCMC(code = eq5d,
  #                   data = normqs,
  #                   inits = inits1, 
  #                   nchains = n.chains,
  #                   nburnin = burnin,
  #                   niter = ifelse(MCMC*n.chains*thin == 0,
  #                                  MCMC*n.chains + burnin,
  #                                  MCMC*n.chains*thin + burnin),
  #                   setSeed = seed,
  #                   constants = constants),
  #   
  #   vas = nimbleMCMC(code = eq5d,
  #                    data = normvas,
  #                    inits = inits1,
  #                    nchains = n.chains,
  #                    nburnin = burnin,
  #                    niter = ifelse(MCMC*n.chains*thin == 0,
  #                                   MCMC*n.chains + burnin,
  #                                   MCMC*n.chains*thin + burnin),
  #                    setSeed = seed,
  #                    constants = constants),
  #   
  #   both = nimbleMCMC(code = eq5dboth,
  #                     data = bvnorm,
  #                     inits = inits2, 
  #                     nchains = n.chains,
  #                     nburnin = burnin,
  #                     niter = ifelse(MCMC*n.chains*thin == 0,
  #                                    MCMC*n.chains + burnin,
  #                                    MCMC*n.chains*thin + burnin),
  #                     setSeed = seed,
  #                     constants = constants))

  nimblereg2 <- readRDS("nimble_test_with_sex.rds")
  nimblereg2

}
