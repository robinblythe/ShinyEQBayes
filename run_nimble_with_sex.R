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

  qs <- as.data.table(cbind(do.call(rbind, nimblereg2[1:n.chains])[,c("beta[1]", "beta[2]")], "Questions only"))
  qs0 <- as.data.table(cbind("Estimate" = as.numeric(qs$`beta[1]`), "Method" = qs$V3, "Sex" = 0))
  qs1 <- as.data.table(cbind("Estimate" = as.numeric(qs$`beta[1]`)+as.numeric(qs$`beta[2]`), "Method" = qs$V3, "Sex" = 1))
  qs <- rbind(qs0, qs1)
  
  vas <- as.data.table(cbind(do.call(rbind, nimblereg2[(n.chains+1):(2*n.chains)])[,c("beta[1]", "beta[2]")], "VAS only"))
  vas0 <- as.data.table(cbind("Estimate" = as.numeric(vas$`beta[1]`), "Method" = vas$V3, "Sex" = 0))
  vas1 <- as.data.table(cbind("Estimate" = as.numeric(vas$`beta[1]`)+as.numeric(vas$`beta[2]`), "Method" = vas$V3, "Sex" = 1))
  vas <- rbind(vas0, vas1)
  
  qs_vas <- as.data.table(cbind(do.call(rbind, nimblereg2[(2*n.chains+1):(3*n.chains)])[,c("beta[1]", "beta[2]", "beta[3]")], "Questions + VAS"))
  qs_vas_0 <- as.data.table(cbind("Estimate" = as.numeric(qs_vas$`beta[1]`)+as.numeric(qs_vas$`beta[2]`), "Method" = qs_vas$V4, "Sex" = 0))
  qs_vas_1 <- as.data.table(cbind("Estimate" = as.numeric(qs_vas$`beta[1]`)+as.numeric(qs_vas$`beta[2]`)+as.numeric(qs_vas$`beta[3]`), "Method" = qs_vas$V4, "Sex" = 1))
  qs_vas <- rbind(qs_vas_0, qs_vas_1)
  
  model2 <- rbind(qs, vas, qs_vas, fill = T)
  model2 <- model2[, lapply(.SD, as.numeric), by = Method]

  model2

}
