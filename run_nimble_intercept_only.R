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
  
  #Data
  normqs <- list(N = N, Y = Y[,1])
  normvas <- list(N = N, Y = Y[,2])
  bvnorm <- list(N = N, Y = Y, R = R)
  
  eq5d <- nimbleCode({
    #Model 
    for (i in 1:N) {
      Y[i] ~ dnorm(mean = mu[i], tau)
      mu[i] <- beta
    }
    beta ~ dbeta(1,1)
    tau ~ dgamma(1,1)
  })
  
  eq5dboth <- nimbleCode({
    #Model 
    for (i in 1:N) {
      Y[i,1:2] ~ dmnorm(mean = mu[i,1:2], prec = Omega[1:2,1:2])
      mu[i,1] <- beta[1]
      mu[i,2] <- beta[2]
    }
    beta[1] ~ dbeta(1,1)
    beta[2] ~ dbeta(1,1)
    Omega[1:2,1:2] ~ dwish(R[1:2,1:2], 2)
    sigma[1:2, 1:2] <- inverse(Omega[1:2, 1:2])
  })
  
  constants <- list(N = N)
  inits1 <- list(beta = mean(Y), tau = 1) #For univariate just start at mean (both)
  inits1 <- rep(list(inits1), n.chains) # repeat initial values per chain
  inits2 <- list(beta = c(mean(Y[,1]), mean(Y[,2])), Omega = R) #For bivaraiate start at each mean
  inits2 = rep(list(inits2), n.chains) # repeat initial values per chain
  
  nimblereg1 <- c(
    qs = nimbleMCMC(code = eq5d,
                    data = normqs,
                    inits = inits1,
                    nchains = n.chains,
                    nburnin = burnin,
                    niter = ifelse(MCMC*n.chains*thin == 0,
                                   MCMC*n.chains + burnin,
                                   MCMC*n.chains*thin + burnin),
                    setSeed = seed,
                    constants = constants),

    vas = nimbleMCMC(code = eq5d,
                     data = normvas,
                     inits = inits1,
                     nchains = n.chains,
                     nburnin = burnin,
                     niter = ifelse(MCMC*n.chains*thin == 0,
                                    MCMC*n.chains + burnin,
                                    MCMC*n.chains*thin + burnin),
                     setSeed = seed,
                     constants = constants),

    both = nimbleMCMC(code = eq5dboth,
                      data = bvnorm,
                      inits = inits2,
                      nchains = n.chains,
                      nburnin = burnin,
                      niter = ifelse(MCMC*n.chains*thin == 0,
                                     MCMC*n.chains + burnin,
                                     MCMC*n.chains*thin + burnin),
                      setSeed = seed,
                      constants = constants))
  
  #nimblereg1 <- readRDS("nimble_test_intercept_only.rds")
  
  qs <- as.data.table(cbind(do.call(rbind, nimblereg1[1:n.chains])[,"beta"], "Questions only"))
  colnames(qs)[c(1,2)] <- c("Estimate", "Method")
  vas <- as.data.table(cbind(do.call(rbind, nimblereg1[(n.chains+1):(2*n.chains)])[,"beta"], "VAS only"))
  colnames(vas)[c(1,2)] <- c("Estimate", "Method")
  both <- as.data.table(cbind(do.call(rbind, nimblereg1[(2*n.chains+1):(3*n.chains)])[,c("beta[1]", "beta[2]")], "Questions + VAS"))
  qs_vas <- as.data.table(cbind("Estimate" = (as.numeric(both$`beta[1]`)+as.numeric(both$`beta[2]`))/2, "Method" = both$V3))

  model1 <- rbind(qs, vas, qs_vas, fill = T)
  model1 <- model1[, lapply(.SD, as.numeric), by = Method]

  model1

}