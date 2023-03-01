library(network)
library(ergm)



set.seed(100)

# gwdegree: geometrically weighted degree statistic
# gwesp: geometrically weighted edgewise shared partner statistic, equals alternating k-triangle statistic (cf. Hunter 2007, p. 224 f.)
sims <- simulate(network(50, directed = FALSE) ~ edges + gwdegree(log(2), fixed = TRUE) + gwesp(decay = log(2), fixed = TRUE),
                 nsim = 100, coef = c(-4, 1.2, 0.7))

# plots of two graphs (numbers are random)
plot(sims[[1]])
plot(sims[[81]])


# dataframe with estimate results (for now empty)
res <- as.data.frame(matrix(nrow = 100, ncol = 6))
colnames(res) <- c("mcmc_edges", "mcmc_gwdeg", "mcmc_gwesp", "mple_edges", "mple_gwdeg", "mple_gwesp")

# ERGMs: MCMC MLE
start <- Sys.time()
for (i in seq_len(100)){
  set.seed(101)
  ergm_mcmc <- ergm(sims[[i]] ~ edges + gwdegree(log(2), fixed = TRUE) + gwesp(decay = log(2), fixed = TRUE))
  res[i, 1:3] <- c(summary(ergm_mcmc)$coefficients[[1]], summary(ergm_mcmc)$coefficients[[2]], summary(ergm_mcmc)$coefficients[[3]])
}
end <- Sys.time()
time_mcmc <- end - start
time_mcmc

# ERGMs: MPLE
start <- Sys.time()
for (i in seq_len(100)) {
  ergm_mcmc <- ergm(sims[[i]] ~ edges + gwdegree(log(2), fixed = TRUE) + gwesp(decay = log(2), fixed = TRUE), estimate = "MPLE")
  res[i, 4:6] <- c(summary(ergm_mcmc)$coefficients[[1]], summary(ergm_mcmc)$coefficients[[2]], summary(ergm_mcmc)$coefficients[[3]])
}
end <- Sys.time()
time_mple <- end - start
time_mple


# mean
round(mean(res$mcmc_edges), 3)
round(mean(res$mple_edges), 3)
round(mean(res$mcmc_gwdeg), 3)
round(mean(res$mple_gwdeg), 3)
round(mean(res$mcmc_gwesp), 3)
round(mean(res$mple_gwesp), 3)

# MAE
round(mean(abs(res$mcmc_edges - (-4))), 3)
round(mean(abs(res$mple_edges - (-4))), 3)
round(mean(abs(res$mcmc_gwdeg - (1.2))), 3)
round(mean(abs(res$mple_gwdeg - (1.2))), 3)
round(mean(abs(res$mcmc_gwesp - (0.7))), 3)
round(mean(abs(res$mple_gwesp - (0.7))), 3)

# MSE
round(mean((res$mcmc_edges - (-4))^2), 3)
round(mean((res$mple_edges - (-4))^2), 3)
round(mean((res$mcmc_gwdeg - (1.2))^2), 3)
round(mean((res$mple_gwdeg - (1.2))^2), 3)
round(mean((res$mcmc_gwesp - (0.7))^2), 3)
round(mean((res$mple_gwesp - (0.7))^2), 3)