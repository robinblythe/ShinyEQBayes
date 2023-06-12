# run_nimble_with_x.R
# Takes inputs from the UI:
# CSV(utility,VAS,xvar)
# MCMC iterations, chains, thinning, burn in

run_nimble_with_x <- function(data, vareq5d, vareqvas, xvar, MCMC, n.chains, burnin, thin, seed) {
  # Define inputs
  utility <- as.numeric(data[[vareq5d]])
  vas <- as.numeric(data[[vareqvas]]) / 100
  xvar <- as.numeric(data[[xvar]])

  Y <- na.omit(as.matrix(cbind(utility, vas)))
  R <- matrix(data = c(1, 0, 0, 1), nrow = 2)
  N <- nrow(Y)

  # Data
  normqs <- list(N = N, Y = utility, xvar = xvar)
  normvas <- list(N = N, Y = vas, xvar = xvar)
  bvnorm <- list(N = N, Y = Y, R = R, xvar = xvar)

  eq5d <- nimbleCode({
    # Model
    for (i in 1:N) {
      Y[i] ~ dnorm(mean = mu[i], tau)
      mu[i] <- beta[1] + beta[2] * xvar[i]
    }
    beta[1] ~ dbeta(1, 1)
    beta[2] ~ dnorm(0, 10000)
    tau ~ dgamma(1, 1)
  })

  eq5dboth <- nimbleCode({
    # Model
    for (i in 1:N) {
      Y[i, 1:2] ~ dmnorm(mean = mu[i, 1:2], prec = Omega[1:2, 1:2])
      mu[i, 1] <- beta[1] + beta[2] * xvar[i]
      mu[i, 2] <- beta[3] + beta[2] * xvar[i]
    }
    beta[1] ~ dbeta(1, 1)
    beta[2] ~ dnorm(0, 10000)
    beta[3] ~ dbeta(1, 1)
    Omega[1:2, 1:2] ~ dwish(R[1:2, 1:2], 2)
    sigma[1:2, 1:2] <- inverse(Omega[1:2, 1:2])
  })

  constants <- list(N = N)
  inits1 <- list(beta = c(mean(utility, na.rm = T), 0), tau = 1)
  inits1 <- rep(list(inits1), n.chains) # repeat initial values per chain
  inits2 <- list(beta = c(mean(Y[, 1]), mean(Y[, 2]), 0), Omega = R)
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

      incProgress(0.3, detail = "Rendering...")
    }
  )

  for (i in 1:n.chains) {
    qs[[i]] <- cbind.data.frame(qs[[i]], Chain = names(qs)[[i]])
    vas[[i]] <- cbind.data.frame(vas[[i]], Chain = names(vas)[[i]])
    both[[i]] <- cbind.data.frame(both[[i]], Chain = names(both)[[i]])
  }

  # nimblereg2 <- readRDS("nimble_test_with_sex.rds")

  qs <- cbind.data.frame(do.call(rbind, qs), "Method" = "Questions only")
  qs0 <- qs[, c(1, 4, 5)]
  qs0$xvar <- 0
  qs0$Estimate <- qs0$`beta[1]`
  qs1 <- qs[, c(1, 2, 4, 5)]
  qs1$Estimate <- qs1$`beta[1]` + qs1$`beta[2]`
  qs1$xvar <- 1
  qs <- dplyr::bind_rows(qs0, qs1)

  vas <- cbind.data.frame(do.call(rbind, vas), "Method" = "VAS only")
  vas0 <- vas[, c(1, 4, 5)]
  vas0$xvar <- 0
  vas0$Estimate <- vas0$`beta[1]`
  vas1 <- vas[, c(1, 2, 4, 5)]
  vas1$Estimate <- vas1$`beta[1]` + vas1$`beta[2]`
  vas1$xvar <- 1
  vas <- dplyr::bind_rows(vas0, vas1)
  
  both <- cbind.data.frame(do.call(rbind, both), "Method" = "Questions + VAS")
  both0 <- both[, c(5, 7, 8, 9)]
  both0$xvar <- 0
  both0$Estimate <- (both0$`beta[1]` + both0$`beta[3]`) / 2
  both1 <- both[, c(5:9)]
  both1$xvar <- 1
  both1$Estimate <- (both1$`beta[1]` + both1$`beta[3]`) / 2 + both1$`beta[2]`
  both <- dplyr::bind_rows(both0, both1)

  model2 <- dplyr::bind_rows(qs, vas, both)

  model2
}
