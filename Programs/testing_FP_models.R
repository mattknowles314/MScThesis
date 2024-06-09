library(flexsurv)
library(dplyr)
library(mfp)

Colucci_GEM <- read.csv("Data/IPD/IPD_Colucci_OS_GEM.csv")
Colucci_GEMCIS <- read.csv("Data/IPD/IPD_Colucci_OS_GEM-CIS.csv")

Colucci <- bind_rows(Colucci_GEM, Colucci_GEMCIS) |> 
  mutate(status = ifelse(censored == FALSE, 1, 0))

dfp1 <- function(x, eta0, eta1, eta2, log = FALSE) {
  f_x <- eta0 + eta1/(x^2) + (eta2)/(x^3)
  return(f_x)
}
