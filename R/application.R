library(igraph)
library(intergraph)
library(ergm)



karate <- read.table(here::here("data/karate.txt"))

karate_data <- graph.data.frame(karate, directed=FALSE)
plot(karate_data)

karate_network <- asNetwork(karate_data)
karate_network
plot(karate_network)

summary(karate_network ~ edges)
summary(karate_network ~ density)
summary(karate_network ~ gwdegree)


set.seed(124)

# ERGM via MCMC MLE
karate_model <- ergm(karate_network ~ edges + gwdegree(0.25, fixed = TRUE) + gwesp(0.25, fixed = TRUE))
summary(karate_model)
mcmc.diagnostics(karate_model)

# Goodness-of-fit
gof <- gof(karate_model)
gof
plot(gof)

# Simulation
sims <- simulate(karate_model, nsim = 1)
summary(sims)
plot(sims)

summary(sims ~ gwdegree)