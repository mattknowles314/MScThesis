library(dplyr)
library(multinma)
library(openxlsx)
library(ggraph)
library(tidyr)
library(bayesplot)
devtools::load_all()

net <- readRDS("Data/network.RDs")

test <- nma(
  network = net,
  trt_effects = "fixed",
  likelihood = "lognormal",
  prior_intercept = normal(0, 100),
  prior_trt = normal(0, 10),
  QR = TRUE,
  iter = 400,
  aux_regression = ~.trt + Male
)
saveRDS(test, file = "Data/test_regression.RDs")
test <- readRDS("Data/test_regression.RDs")
mcmc_trace(test, pars = parsForStan)

plot(predict(test, type = "hazard")) +
  labs(x = "Time (Months)") +
  theme_bw() 
