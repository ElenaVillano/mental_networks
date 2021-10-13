rm(list=ls())
library(tidyverse)

library("psychTools")
data(bfi)
bfiSub <- bfi[1:250,1:25]



Network <- estimateNetwork(bfiSub, default = "ggmModSelect", 
                           corMethod = "cor",stepwise = FALSE)

boots <- bootnet(Network, nBoots = 10)

Network_inclusion <- bootInclude(boots)

layout(1)
plot(Network_inclusion)


# BFI Extraversion data from psychTools package:
library("psychTools")
data(bfi)
bfiSub <- bfi[,1:25]
# Estimate network:
Network <- estimateNetwork(bfiSub, default = "EBICglasso",
                           threshold=TRUE)
# Centrality indices:
library("qgraph")
centralityPlot(Network)
## Not run:
# Estimated network:
plot(Network, layout = 'spring')
### Non-parametric bootstrap ###
# Bootstrap 1000 values, using 8 cores:
Results1 <- bootnet(Network, nBoots = 100, nCores = 1)
# Plot bootstrapped edge CIs:
plot(Results1, labels = FALSE, order = "sample")
# Plot significant differences (alpha = 0.05) of edges:
plot(Results1, "edge", plot = "difference",onlyNonZero = TRUE,
     order = "sample")
# Plot significant differences (alpha = 0.05) of node strength:
plot(Results1, "strength", plot = "difference")
# Test for difference in strength between node "A1" and "C2":
differenceTest(Results1, "A1", "C2", "strength")
### Case-drop bootstrap ###
# Bootstrap 1000 values, using 8 cores:
Results2 <- bootnet(Network, nBoots = 100, nCores = 1,
                    type = "case")
# Plot centrality stability:
plot(Results2)
# Compute CS-coefficients:
corStability(Results2)
## End(Not run)


# BFI Extraversion data from psychTools package:
library("psychTools")
data(bfi)
bfiSub <- bfi[,1:25]
# Estimate network:
Network <- estimateNetwork(bfiSub, default = "EBICglasso")
## Not run:
# Some pointers:
print(Network)
# Estimated network:
plot(Network, layout = 'spring')
# Centrality indices:
library("qgraph")
centralityPlot(Network)
# BIC model selection:
Network_BIC <- estimateNetwork(bfiSub, default = "EBICglasso", tuning = 0)
# Ising model:
Network_BIC <- estimateNetwork(bfiSub, default = "IsingFit")
## End(Not run)


# 5-node GGM chain graph:
trueNetwork <- genGGM(5)
# Simulate:
Res <- netSimulator(trueNetwork, nReps = 10)
# Results:
Res
# Plot:
plot(Res)
## Not run:
library("bootnet")
# BFI example:
# Load data:
library("psychTools")
data(bfi)
bfiData <- bfi[,1:25]
# Estimate a network structure, with parameters refitted without LASSO regularization:
library("qgraph")
Network <- EBICglasso(cor_auto(bfiData), nrow(bfiData), refit = TRUE)
# Simulate 100 repititions in 8 cores under different sampling levels:
Sim1 <- netSimulator(Network,
                     default = "EBICglasso",
                     nCases = c(100,250,500),
                     nReps = 100,
                     nCores = 1)
# Table of results:
Sim1
# Plot results:
plot(Sim1)
# Compare different default set at two sampling levels:
Sim2 <- netSimulator(Network,
                     default = c("EBICglasso","pcor","huge"),
                     nCases = c(100,250,500),
                     nReps = 100,
                     nCores = 1)
# Print results:
Sim2
# Plot results:
plot(Sim2, xfacet = "default", yvar = "correlation")
# Difference using polychoric or pearson correlations in ordinal data:
Sim3 <- netSimulator(Network,
                     dataGenerator = ggmGenerator(ordinal = TRUE, nLevels = 4),
                     default = "EBICglasso",
                     corMethod = c("cor","cor_auto"),
                     nCases = c(100,250, 500),
                     nReps = 100,
                     nCores = 1)
# Print results:
Sim3
# Plot results:
plot(Sim3, color = "corMethod")
# Ising model:
trueNetwork <- read.csv('http://sachaepskamp.com/files/weiadj.csv')[,-1]
trueNetwork <- as.matrix(trueNetwork)
Symptoms <- rownames(trueNetwork) <- colnames(trueNetwork)
Thresholds <- read.csv('http://sachaepskamp.com/files/thr.csv')[,-1]
# Create an input list (intercepts now needed)
input <- list(graph=trueNetwork,intercepts=Thresholds)
# Simulate under different sampling levels:
Sim4 <- netSimulator(
  input = input,
  default = "IsingFit",
  nCases = c(250,500,1000),
  nReps = 100,
  nCores = 1)
# Results:
Sim4
# Plot:
plot(Sim4)
# Compare AND and OR rule:
Sim5 <- netSimulator(
  input = input,
  default = "IsingFit",
  nCases = c(250,500,1000),
  rule = c("AND","OR"),
  nReps = 100,
  nCores = 1)
# Print:
Sim5
# Plot:
plot(Sim5, yfacet = "rule")
## End(Not run)








