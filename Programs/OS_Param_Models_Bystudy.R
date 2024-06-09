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

### Models

colucci_models <- indep_model(df, "Colucci")
cunningham_models <- indep_model(df, "Cunningham")
kindler_models <- indep_model(df, "Kindler")
oettle_models <- indep_model(df, "Oettle")
rochalima_models <- indep_model(df, "RochaLima")

### Medians

# Colucci 
p <- plot(colucci_models, km = TRUE, alpha = 1, linewidth = 0.5) + labs(x = "Time (Months)")
ggsave(p, filename = "~/Documents/MScThesis/figures/Models/OS/Colucci.png",
       height = 5, width = 7.5, units = "in")
Colucci_medians <- summary(colucci_models, median = TRUE) |> 
  mutate(Study = "Colucci") |> 
  mutate(n = 400)

# Cunningham
p <- plot(cunningham_models, km = TRUE, alpha = 1, linewidth = 0.5) + labs(x = "Time (Months)")
ggsave(p, filename = "~/Documents/MScThesis/figures/Models/OS/Cunningham.png",
       height = 5, width = 7.5, units = "in")
Cunningham_medians <- summary(cunningham_models, median = TRUE)|> 
  mutate(Study = "Cunningham") |> 
  mutate(n = 533)

# Kindler
p <- plot(kindler_models, km = TRUE, alpha = 1, linewidth = 0.5) + labs(x = "Time (Months)")
ggsave(p, filename = "~/Documents/MScThesis/figures/Models/OS/Kindler.png",
       height = 5, width = 7.5, units = "in")
Kindler_medians <- summary(kindler_models, median = TRUE) |> 
  mutate(Study = "Kindler") |> 
  mutate(n = 632)

# Oettle
p <- plot(oettle_models, km = TRUE, alpha = 1, linewidth = 0.5) + labs(x = "Time (Months)")
ggsave(p, filename = "~/Documents/MScThesis/figures/Models/OS/Oettle.png",
       height = 5, width = 7.5, units = "in")
Oettle_medians <- summary(oettle_models, median = TRUE) |> 
  mutate(Study = "Oettle") |> 
  mutate(n = 565)

# RochaLima
P <- plot(rochalima_models, km = TRUE, alpha = 1, linewidth = 0.5) + labs(x = "Time (Months)")
ggsave(p, filename = "~/Documents/MScThesis/figures/Models/OS/RochaLima.png",
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

# RMST

Colucci_GEM_RMST <- bind_rows(rmst((colucci_models |> filter(Treatment == "GEM", Distribution == "Gompertz")), 12),
                              rmst((colucci_models |> filter(Treatment == "GEM", Distribution == "Gompertz")), 18),
                              rmst((colucci_models |> filter(Treatment == "GEM", Distribution == "Gompertz")), 24))
                              
Colucci_GEMCIS_RMST <- bind_rows(rmst((colucci_models |> filter(Treatment == "GEM-CIS", Distribution == "Generalised Gamma")), 12),
                                 rmst((colucci_models |> filter(Treatment == "GEM-CIS", Distribution == "Generalised Gamma")), 18),
                                 rmst((colucci_models |> filter(Treatment == "GEM-CIS", Distribution == "Generalised Gamma")), 24))

Cunningham_GEM_RMST <- bind_rows(rmst((cunningham_models |> filter(Treatment == "GEM", Distribution == "Log-Logistic")), 12),
                                 rmst((cunningham_models |> filter(Treatment == "GEM", Distribution == "Log-Logistic")), 18),
                                 rmst((cunningham_models |> filter(Treatment == "GEM", Distribution == "Log-Logistic")), 24))

Cunningham_GEMCAP_RMST <- bind_rows(rmst((cunningham_models |> filter(Treatment == "GEM-CAP", Distribution == "Generalised Gamma")), 12),
                                    rmst((cunningham_models |> filter(Treatment == "GEM-CAP", Distribution == "Generalised Gamma")), 18),
                                    rmst((cunningham_models |> filter(Treatment == "GEM-CAP", Distribution == "Generalised Gamma")), 24))

Kindler_GEM_RMST <- bind_rows(rmst((kindler_models |> filter(Treatment == "GEM", Distribution == "Exponential")), 12),
                              rmst((kindler_models |> filter(Treatment == "GEM", Distribution == "Exponential")), 18),
                              rmst((kindler_models |> filter(Treatment == "GEM", Distribution == "Exponential")), 24))

Kindler_GEMAXI_RMST <- bind_rows(rmst((kindler_models |> filter(Treatment == "GEM-AXI", Distribution == "Exponential")), 12),
                                 rmst((kindler_models |> filter(Treatment == "GEM-AXI", Distribution == "Exponential")), 18),
                                 rmst((kindler_models |> filter(Treatment == "GEM-AXI", Distribution == "Exponential")), 24))

Oettle_GEM_RMST <- bind_rows(rmst((oettle_models |> filter(Treatment == "GEM", Distribution == "Generalised Gamma")), 12),
                             rmst((oettle_models |> filter(Treatment == "GEM", Distribution == "Generalised Gamma")), 18),
                             rmst((oettle_models |> filter(Treatment == "GEM", Distribution == "Generalised Gamma")), 24))

Oettle_GEMPEM_RMST <- bind_rows(rmst((oettle_models |> filter(Treatment == "GEM-PEM", Distribution == "Gamma")), 12),
                                rmst((oettle_models |> filter(Treatment == "GEM-PEM", Distribution == "Gamma")), 18),
                                rmst((oettle_models |> filter(Treatment == "GEM-PEM", Distribution == "Gamma")), 24))

RochaLima_GEM_RMST <- bind_rows(rmst((rochalima_models |> filter(Treatment == "GEM", Distribution == "Generalised Gamma")), 12),
                                rmst((rochalima_models |> filter(Treatment == "GEM", Distribution == "Generalised Gamma")), 18),
                                rmst((rochalima_models |> filter(Treatment == "GEM", Distribution == "Generalised Gamma")), 24))

RochaLima_GEMIRI_RMST <- bind_rows(rmst((rochalima_models |> filter(Treatment == "GEM-IRI", Distribution == "Gamma")), 12),
                                   rmst((rochalima_models |> filter(Treatment == "GEM-IRI", Distribution == "Gamma")), 18),
                                   rmst((rochalima_models |> filter(Treatment == "GEM-IRI", Distribution == "Gamma")), 24))

### NMA

nma_input_medians <- bind_rows(
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

nma_input_RMST <- bind_rows(
  Colucci_GEM_RMST |> mutate(Treatment = "GEM") |> mutate(n = 400) |> mutate(Study = "Colucci"),
  Colucci_GEMCIS_RMST |> mutate(Treatment = "GEM-CIS") |> mutate(n = 400) |> mutate(Study = "Colucci"),
  Cunningham_GEM_RMST |> mutate(Treatment = "GEM") |> mutate(n = 533) |> mutate(Study = "Cunningham"),
  Cunningham_GEMCAP_RMST |> mutate(Treatment = "GEM-CAP") |> mutate(n = 533) |> mutate(Study = "Cunningham"),
  Kindler_GEM_RMST |> mutate(Treatment = "GEM") |> mutate(n = 632) |> mutate(Study = "Kindler"),
  Kindler_GEMAXI_RMST |> mutate(Treatment = "GEM-AXI") |> mutate(n = 632) |> mutate(Study = "Kindler"),
  Oettle_GEM_RMST |> mutate(Treatment = "GEM") |> mutate(n = 565) |> mutate(Study = "Oettle"),
  Oettle_GEMPEM_RMST |> mutate(Treatment = "GEM-PEM") |> mutate(n = 565) |> mutate(Study = "Oettle"),
  RochaLima_GEM_RMST |> mutate(Treatment = "GEM") |> mutate(n = 360) |> mutate(Study = "Rocha Lima"),
  RochaLima_GEMIRI_RMST |> mutate(Treatment = "GEM-IRI") |> mutate(n = 360) |> mutate(Study = "Rocha Lima")
) |> 
  mutate(SE = (0.5*(UCL - LCL))/qnorm(0.975)*sqrt(n))

write.xlsx(nma_input_medians, file = "~/Documents/MScThesis/Results/NMA/input_data_param_Medians.xlsx", overwrite = TRUE)
write.xlsx(nma_input_RMST, file = "~/Documents/MScThesis/Results/NMA/input_data_param_RMST.xlsx", overwrite = TRUE)

network_12 <- gen_network(nma_input_RMST |> filter(Timepoint == 12), ref = "GEM")
network_18 <- gen_network(nma_input_RMST |> filter(Timepoint == 18), ref = "GEM")
network_24 <- gen_network(nma_input_RMST |> filter(Timepoint == 24), ref = "GEM")

p <- plot(network_12, circular = TRUE)
ggsave(p, filename = "OS_Network.png", path = "~/Documents/MScThesis/figures/",
       height = 5, width = 8, units = "in")

param_12_FE <- fit_model(network_12, "fixed", 30000)
param_18_FE <- fit_model(network_18, "fixed", 30000)
param_24_FE <- fit_model(network_24, "fixed", 30000)

param_12_RE <- fit_model(network_12, "random", 30000)
param_18_RE <- fit_model(network_18, "random", 30000)
param_24_RE <- fit_model(network_24, "random", 30000)

# Summaries

dic(param_12_FE)
dic(param_18_FE)
dic(param_24_FE)

dic(param_12_RE)
dic(param_18_RE)
dic(param_24_RE)

# Fixed Effects plots

p <- plot(param_12_FE, type = "forest", xLims = c(-1, 1)) + labs(x = "Treatment Effects", y = "Treatment")
ggsave(p, filename = "FE12-Zoomed.png", path = "~/Documents/MScThesis/figures/",
       height = 6, width = 6, units = "in")
p <- plot(param_12_FE, type = "forest") + labs(x = "Treatment Effects", y = "Treatment")
ggsave(p, filename = "FE12-Forest.png", path = "~/Documents/MScThesis/figures/",
       height = 6, width = 6, units = "in")
p <- plot(param_12_FE, type = "trace")
ggsave(p, filename = "FE12-Trace-Param.png", path = "~/Documents/MScThesis/figures/",
       height = 6, width = 8, units = "in")

p <- plot(param_18_FE, type = "forest", xLims = c(-1, 1)) + labs(x = "Treatment Effects", y = "Treatment")
ggsave(p, filename = "FE18-Zoomed.png", path = "~/Documents/MScThesis/figures/",
       height = 6, width = 6, units = "in")
p <- plot(param_18_FE, type = "forest") + labs(x = "Treatment Effects", y = "Treatment")
ggsave(p, filename = "FE18-Forest.png", path = "~/Documents/MScThesis/figures/",
       height = 6, width = 6, units = "in")
p <- plot(param_18_FE, type = "trace")
ggsave(p, filename = "FE18-Trace-Param.png", path = "~/Documents/MScThesis/figures/",
       height = 6, width = 8, units = "in")

p <- plot(param_24_FE, type = "forest", xLims = c(-1, 1)) + labs(x = "Treatment Effects", y = "Treatment")
ggsave(p, filename = "FE24-Zoomed.png", path = "~/Documents/MScThesis/figures/",
       height = 6, width = 6, units = "in")
p <- plot(param_24_FE, type = "forest") + labs(x = "Treatment Effects", y = "Treatment")
ggsave(p, filename = "FE24-Forest.png", path = "~/Documents/MScThesis/figures/",
       height = 6, width = 6, units = "in")
p <- plot(param_24_FE, type = "trace")
ggsave(p, filename = "FE24-Trace-Param.png", path = "~/Documents/MScThesis/figures/",
       height = 6, width = 8, units = "in")

# Random Effects plots

p <- plot(param_12_RE, type = "forest", xLims = c(-1, 1)) + labs(x = "Treatment Effects", y = "Treatment")
ggsave(p, filename = "RE12-Zoomed.png", path = "~/Documents/MScThesis/figures/",
       height = 6, width = 6, units = "in")
p <- plot(param_12_RE, type = "forest") + labs(x = "Treatment Effects", y = "Treatment")
ggsave(p, filename = "RE12-Forest.png", path = "~/Documents/MScThesis/figures/",
       height = 6, width = 6, units = "in")
p <- plot(param_12_RE, type = "trace")
ggsave(p, filename = "RE12-Trace-Param.png", path = "~/Documents/MScThesis/figures/",
       height = 6, width = 8, units = "in")

p <- plot(param_18_RE, type = "forest", xLims = c(-1, 1)) + labs(x = "Treatment Effects", y = "Treatment")
ggsave(p, filename = "RE18-Zoomed.png", path = "~/Documents/MScThesis/figures/",
       height = 6, width = 6, units = "in")
p <- plot(param_18_RE, type = "forest") + labs(x = "Treatment Effects", y = "Treatment")
ggsave(p, filename = "RE18-Forest.png", path = "~/Documents/MScThesis/figures/",
       height = 6, width = 6, units = "in")
p <- plot(param_18_RE, type = "trace")
ggsave(p, filename = "RE18-Trace-Param.png", path = "~/Documents/MScThesis/figures/",
       height = 6, width = 8, units = "in")

p <- plot(param_24_RE, type = "forest", xLims = c(-1, 1)) + labs(x = "Treatment Effects", y = "Treatment")
ggsave(p, filename = "RE24-Zoomed.png", path = "~/Documents/MScThesis/figures/",
       height = 6, width = 6, units = "in")
p <- plot(param_24_RE, type = "forest") + labs(x = "Treatment Effects", y = "Treatment")
ggsave(p, filename = "RE24-Forest.png", path = "~/Documents/MScThesis/figures/",
       height = 6, width = 6, units = "in")
p <- plot(param_24_RE, type = "trace")
ggsave(p, filename = "RE24-Trace-Param.png", path = "~/Documents/MScThesis/figures/",
       height = 6, width = 8, units = "in")
