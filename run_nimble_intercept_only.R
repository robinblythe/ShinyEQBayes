#run_nimble_intercept_only.R
#Takes inputs from the UI:
#CSV(utility,VAS)
#MCMC iterations, chains, thinning, burn in

run_nimble_intercept_only = function(data, vareq5d, vareqvas, MCMC, n.chains, burnin, thin, seed){

  #Define inputs
  utility <- as.numeric(data[[vareq5d]])
  vas <- as.numeric(data[[vareqvas]])/100
  Y <- na.omit(as.matrix(cbind(utility, vas)))
  R <- matrix(data = c(1,0,0,1), nrow = 2)
  N <- nrow(Y)
  
  #Data - 1 notation is intercept only model
  bvnorm1 <- list(N = N, Y = Y, R = R)
  
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

  constants1 <- list(N = N)
  inits1 <- list(beta = c(mean(Y[,1]), mean(Y[,2])), Omega = R)
  inits1 = rep(list(inits1), n.chains) # repeat initial values per chain
  
  # nimblereg1 <- nimbleMCMC(code = eq5dnox, 
  #                          data = bvnorm1,
  #                          inits = inits1, 
  #                          nchains = n.chains,
  #                          nburnin = burnin,
  #                          niter = ifelse(MCMC*n.chains*thin == 0,
  #                                         MCMC*n.chains + burnin,
  #                                         MCMC*n.chains*thin + burnin),
  #                          setSeed = seed,
  #                          constants = constants1)
  # browser()
  nimblereg1 <- readRDS("nimble_test_intercept_only.rds")
  nimblereg1
}
