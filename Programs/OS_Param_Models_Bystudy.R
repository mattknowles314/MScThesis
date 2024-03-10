library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(openxlsx)
library(PancSurv)
library(multinma)

Colucci_GEM <- read.csv("Data/IPD/IPD_Colucci_OS_GEM.csv")
Colucci_GEMCIS <- read.csv("Data/IPD/IPD_Colucci_OS_GEM-CIS.csv")

Cunningham_GEM <- read.csv("Data/IPD/IPD_Cunningham_OS_GEM.csv")
Cunningham_GEMCAP <- read.csv("Data/IPD/IPD_Cunningham_OS_GEM-CAP.csv")

Kindler_GEM <- read.csv("Data/IPD/IPD_Kindler_OS_GEM.csv")
Kindler_GEMAXI <- read.csv("Data/IPD/IPD_Kindler_OS_GEM-AXI.csv")

Oettle_GEM <- read.csv("Data/IPD/IPD_Oettle_OS_GEM.csv")
Oettle_GEMPEM <- read.csv("Data/IPD/IPD_Oettle_OS_GEM-PEM.csv")

RochaLima_GEM <- read.csv("Data/IPD/IPD_RochaLima_OS_GEM.csv")
RochaLima_GEMIRI <- read.csv("Data/IPD/IPD_RochaLima_OS_GEM-IRI.csv")

df <- bind_rows(Colucci_GEM, Colucci_GEMCIS, Cunningham_GEM, Cunningham_GEMCAP,
                Kindler_GEM, Kindler_GEMAXI, Oettle_GEM, Oettle_GEMPEM, RochaLima_GEM, 
                RochaLima_GEMIRI) |> 
  mutate(status = ifelse(censored == FALSE, 1, 0))

indep_model <- function(TTE, study) {
  d <- TTE |> filter(Study == study)
  out1 <- fit_distribution(
    distributions = nice_parametric_dists,
    data = d |> filter(Treatment == "GEM"),
    maxT = 60,
    strata = "1") |> 
    mutate(Treatment = "GEM")
  out2 <- fit_distribution(
    distributions = nice_parametric_dists,
    data = d |> filter(Treatment != "GEM"),
    maxT = 60,
    strata = "1") |> 
    mutate(Treatment = levels(as.factor(d$Treatment))[2])
  out <- bind_rows(out1, out2)
  class(out) <- c("fitted_distribution", class(out))
  out
}

### Medians

# Colucci 
colucci_models <- indep_model(df, "Colucci")
plot(colucci_models, km = TRUE, alpha = 1, linewidth = 0.5) + labs(x = "Time (Months)")
ggsave(filename = "~/Documents/MScThesis/figures/Models/OS/Colucci.png",
       height = 5, width = 7.5, units = "in")
Colucci_medians <- summary(colucci_models, median = TRUE) |> 
  mutate(Study = "Colucci") |> 
  mutate(n = 400)

# Cunningham
cunningham_models <- indep_model(df, "Cunningham")
plot(cunningham_models, km = TRUE, alpha = 1, linewidth = 0.5) + labs(x = "Time (Months)")
ggsave(filename = "~/Documents/MScThesis/figures/Models/OS/Cunningham.png",
       height = 5, width = 7.5, units = "in")
Cunningham_medians <- summary(cunningham_models, median = TRUE)|> 
  mutate(Study = "Cunningham") |> 
  mutate(n = 533)

# Kindler
kindler_models <- indep_model(df, "Kindler")
plot(kindler_models, km = TRUE, alpha = 1, linewidth = 0.5) + labs(x = "Time (Months)")
ggsave(filename = "~/Documents/MScThesis/figures/Models/OS/Kindler.png",
       height = 5, width = 7.5, units = "in")
Kindler_medians <- summary(kindler_models, median = TRUE) |> 
  mutate(Study = "Kindler") |> 
  mutate(n = 632)

# Oettle
oettle_models <- indep_model(df, "Oettle")
plot(oettle_models, km = TRUE, alpha = 1, linewidth = 0.5) + labs(x = "Time (Months)")
ggsave(filename = "~/Documents/MScThesis/figures/Models/OS/Oettle.png",
       height = 5, width = 7.5, units = "in")
Oettle_medians <- summary(oettle_models, median = TRUE) |> 
  mutate(Study = "Oettle") |> 
  mutate(n = 565)

# RochaLima
rochalima_models <- indep_model(df, "RochaLima")
plot(rochalima_models, km = TRUE, alpha = 1, linewidth = 0.5) + labs(x = "Time (Months)")
ggsave(filename = "~/Documents/MScThesis/figures/Models/OS/RochaLima.png",
       height = 5, width = 7.5, units = "in")
RochaLima_medians <- summary(rochalima_models, median = TRUE) |> 
  mutate(Study = "Rocha Lima") |> 
  mutate(n = 360)

Medians <- bind_rows(
  Colucci_medians,
  Cunningham_medians,
  Kindler_medians,
  Oettle_medians,
  RochaLima_medians
) 

write.xlsx(Medians, file = "~/Documents/MScThesis/Results/Models/OS/Medians.xlsx", overwrite = TRUE)

### NMA

nma_input <- bind_rows(
  Colucci_medians |> filter(Treatment == "GEM" & Distribution == "Gompertz"),
  Colucci_medians |> filter(Treatment == "GEM-CIS" & Distribution == "Generalised Gamma"),
  Cunningham_medians |> filter(Treatment == "GEM" & Distribution == "Log-Logistic"),
  Cunningham_medians |> filter(Treatment == "GEM-CAP" & Distribution == "Generalised Gamma"),
  Kindler_medians |> filter(Distribution == "Exponential"),
  Oettle_medians |> filter(Treatment == "GEM" & Distribution == "Generalised Gamma"),
  Oettle_medians |> filter(Treatment == "GEM-PEM" & Distribution == "Gamma"),
  RochaLima_medians |> filter(Treatment == "GEM" & Distribution == "Gamma"),
  RochaLima_medians |> filter(Treatment == "GEM-IRI" & Distribution == "Weibull")
) |> 
  mutate(SE = (0.5*(U95 - L95))/qnorm(0.975)*sqrt(n))

write.xlsx(nma_input, file = "~/Documents/MScThesis/Results/NMA/input_data_param.xlsx", overwrite = TRUE)

network <- gen_network(nma_input, "GEM")
p <- plot(network, circular = TRUE)
ggsave(p, filename = "OS_Network.png", path = "~/Documents/MScThesis/figures/",
       height = 5, width = 8, units = "in")

param_FE <- fit_model(network, "fixed", 30000)
param_RE <- fit_model(network, "random", 30000)


p <- plot(param_FE, type = "forest", xLims = c(-1, 1)) + labs(x = "Treatment Effects", y = "Treatment")
ggsave(p, filename = "FE-Zoomed.png", path = "~/Documents/MScThesis/figures/",
       height = 6, width = 6, units = "in")
p <- plot(param_FE, type = "forest") + labs(x = "Treatment Effects", y = "Treatment")
ggsave(p, filename = "FE-Forest.png", path = "~/Documents/MScThesis/figures/",
       height = 6, width = 6, units = "in")
p <- plot(param_FE, type = "trace")
ggsave(p, filename = "FE-Trace-Param.png", path = "~/Documents/MScThesis/figures/",
       height = 6, width = 8, units = "in")

summary(param_FE, dic = TRUE)


p <- plot(param_RE, type = "forest", xLims = c(-1, 1)) + labs(x = "Treatment Effects", y = "Treatment")
ggsave(p, filename = "RE-Zoomed.png", path = "~/Documents/MScThesis/figures/",
       height = 6, width = 6, units = "in")
p <- plot(param_RE, type = "forest") + labs(x = "Treatment Effects", y = "Treatment")
ggsave(p, filename = "RE-Forest.png", path = "~/Documents/MScThesis/figures/",
       height = 6, width = 6, units = "in")
p <- plot(param_RE, type = "trace")
ggsave(p, filename = "RE-Trace-Param.png", path = "~/Documents/MScThesis/figures/",
       height = 6, width = 8, units = "in")

summary(param_RE, dic = TRUE)
