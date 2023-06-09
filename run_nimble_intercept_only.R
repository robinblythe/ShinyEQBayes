# run_nimble_intercept_only.R
# Takes inputs from the UI:
# CSV(utility,VAS)
# MCMC iterations, chains, thinning, burn in

run_nimble_intercept_only <- function(data, vareq5d, vareqvas, MCMC, n.chains, burnin, thin, seed) {
  # Define inputs
  utility <- as.numeric(data[[vareq5d]])
  vas <- as.numeric(data[[vareqvas]]) / 100

  Y <- na.omit(as.matrix(cbind(utility, vas)))
  R <- matrix(data = c(1, 0, 0, 1), nrow = 2)
  N <- nrow(Y)

  # Data
  normqs <- list(N = N, Y = Y[, 1])
  normvas <- list(N = N, Y = Y[, 2])
  bvnorm <- list(N = N, Y = Y, R = R)

  eq5d <- nimbleCode({
    # Model
    for (i in 1:N) {
      Y[i] ~ dnorm(mean = mu[i], tau)
      mu[i] <- beta
    }
    beta ~ dbeta(1, 1)
    tau ~ dgamma(1, 1)
  })

  eq5dboth <- nimbleCode({
    # Model
    for (i in 1:N) {
      Y[i, 1:2] ~ dmnorm(mean = mu[i, 1:2], prec = Omega[1:2, 1:2])
      mu[i, 1] <- beta[1]
      mu[i, 2] <- beta[2]
    }
    beta[1] ~ dbeta(1, 1)
    beta[2] ~ dbeta(1, 1)
    Omega[1:2, 1:2] ~ dwish(R[1:2, 1:2], 2)
    sigma[1:2, 1:2] <- inverse(Omega[1:2, 1:2])
  })

  constants <- list(N = N)
  inits1 <- list(beta = mean(Y), tau = 1) # For univariate just start at mean (both)
  inits1 <- rep(list(inits1), n.chains) # repeat initial values per chain
  inits2 <- list(beta = c(mean(Y[, 1]), mean(Y[, 2])), Omega = R) # For bivaraiate start at each mean
  inits2 <- rep(list(inits2), n.chains) # repeat initial values per chain

  withProgress(
    message = "Running MCMC models - please be patient!",
    detail = "Compiling EQ5D...",
    value = 0.2,
    {
      qs <- nimbleMCMC(
        code = eq5d,
        data = normqs,
        inits = inits1,
        nchains = n.chains,
        nburnin = burnin,
        niter = ifelse(MCMC * n.chains * thin == 0,
          MCMC * n.chains + burnin,
          MCMC * n.chains * thin + burnin
        ),
        setSeed = seed,
        constants = constants
      )

      incProgress(0.2, detail = "Compiling EQVAS...")

      vas <- nimbleMCMC(
        code = eq5d,
        data = normvas,
        inits = inits1,
        nchains = n.chains,
        nburnin = burnin,
        niter = ifelse(MCMC * n.chains * thin == 0,
          MCMC * n.chains + burnin,
          MCMC * n.chains * thin + burnin
        ),
        setSeed = seed,
        constants = constants
      )

      incProgress(0.2, detail = "Compiling joint model...")

      both <- nimbleMCMC(
        code = eq5dboth,
        data = bvnorm,
        inits = inits2,
        nchains = n.chains,
        nburnin = burnin,
        niter = ifelse(MCMC * n.chains * thin == 0,
          MCMC * n.chains + burnin,
          MCMC * n.chains * thin + burnin
        ),
        setSeed = seed,
        constants = constants
      )

      incProgress(0.9, detail = "Rendering...")
    }
  )

  # nimblereg1 <- readRDS("nimble_test_intercept_only.rds")

  for (i in 1:n.chains) {
    qs[[i]] <- cbind.data.frame(qs[[i]], Chain = names(qs)[[i]])
    vas[[i]] <- cbind.data.frame(vas[[i]], Chain = names(vas)[[i]])
    both[[i]] <- cbind.data.frame(both[[i]], Chain = names(both)[[i]])
  }

  qs <- cbind.data.frame(do.call(rbind, qs), "Method" = "Questions only")
  qs$tau <- NULL
  qs$Estimate <- qs$beta
  
  vas <- cbind.data.frame(do.call(rbind, vas), "Method" = "VAS only")
  vas$tau <- NULL
  vas$Estimate <- vas$beta
  
  both <- cbind.data.frame(do.call(rbind, both), "Method" = "Questions + VAS")
  both <- both[, -c(1:4)]
  both$Estimate <- (both$`beta[1]` + both$`beta[2]`) / 2
  
  model1 <- dplyr::bind_rows(qs, vas, both)
  model1$`beta[1]`[is.na(model1$`beta[1]`)] <- model1$beta[!is.na(model1$beta)]
  model1$beta <- NULL

  model1
}
