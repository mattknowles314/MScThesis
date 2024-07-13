library(ggplot2)
library(dplyr)
library(purrr)

k = seq(2, 100, 1)
n = 500
L = 1500

aic <- function(k, L) {
  return(2*(k - log(L)))
}

bic <- function(k, L, n) {
  return(k*log(n) - 2*log(L))
}

df <- tibble(k1 = k, k2 = k) |> 
  mutate(Model1_AIC = map_dbl(k1, aic, L = L)) |> 
  mutate(Model2_AIC = map_dbl(k2, aic, L = L)) |> 
  mutate(Model1_BIC = map_dbl(k1, bic, n = n, L = L)) |> 
  mutate(Model2_BIC = map_dbl(k2, bic, n = n, L = L))

colours <- c(
  "AIC" = "#7EBE91",
  "BIC" = "darkred"
)

p <- ggplot(df, aes(x = k, y = k)) +
  
  
ggsave(p, filename = "~/Documents/MScThesis/figures/AICBIC.png", width = 12, height = 10, units = "in")
