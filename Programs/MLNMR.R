library(PCNMA)
library(dplyr)
library(multinma)

net <- readRDS("Data/network.RDs")

test <- fit_model(net, effects = "fixed", llhood = "weibull")
