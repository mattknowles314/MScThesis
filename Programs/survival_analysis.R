library(dplyr)
library(openxlsx)
devtools::load_all()

Cunningham_GEM <- read.csv("Data/IPD/IPD_Cunningham_OS_GEM.csv")
Cunningham_GEMCAP <- read.csv("Data/IPD/IPD_Cunningham_OS_GEM-CAP.csv")

Kindler_GEM <- read.csv("Data/IPD/IPD_Kindler_OS_GEM.csv")
Kindler_GEMAXI <- read.csv("Data/IPD/IPD_Kindler_OS_GEM-AXI.csv")

Oettle_GEM <- read.csv("Data/IPD/IPD_Oettle_OS_GEM.csv")
Oettle_GEMPEM <- read.csv("Data/IPD/IPD_Oettle_OS_GEM-PEM.csv")

RochaLima_GEM <- read.csv("Data/IPD/IPD_RochaLima_OS_GEM.csv")
RochaLima_GEMIRI <- read.csv("Data/IPD/IPD_RochaLima_OS_GEM-IRI.csv")

Goldstein_GEM <- read.csv("Data/IPD/IPD_Goldstein_OS_GEM.csv") |> mutate(Study = "Goldstein") |> mutate(Treatment = "GEM")
Goldstein_NAB <- read.csv("Data/IPD/IPD_Goldstein_OS_NAB.csv") |> mutate(Study = "Goldstein") |> mutate(Treatment = "GEM-NAB")

Spano_GEMAXI <- read.csv("Data/IPD/IPD_Spano_OS_AXI.csv") |> mutate(Study = "Spano") |> mutate(Treatment = "GEM-AXI")
Spano_GEM <- read.csv("Data/IPD/IPD_Spano_OS_GEM.csv") |> mutate(Study = "Spano") |> mutate(Treatment = "GEM")

Goncalves_GEM <- read.csv("Data/IPD/IPD_Goncalves_OS_GEM.csv") |> mutate(Study = "Goncalves") |> mutate(Treatment = "GEM")
Goncalves_GEMSOR <- read.csv("Data/IPD/IPD_Goncalves_OS_SOR.csv") |> mutate(Study = "Goncalves") |> mutate(Treatment = "GEM-SOR")

Conroy_FOL <- read.csv("Data/IPD/IPD_Conroy_OS_FOL.csv")
Conroy_GEM <- read.csv("Data/IPD/IPD_Conroy_OS_GEM.csv") |> mutate(Study = "Conroy") |> mutate(Treatment = "GEM")

Net_Data <- bind_rows(
  Conroy_GEM,
  Conroy_FOL,
  Cunningham_GEM,
  Cunningham_GEMCAP,
  Kindler_GEM,
  Kindler_GEMAXI,
  Oettle_GEM,
  Oettle_GEMPEM,
  RochaLima_GEM,
  RochaLima_GEMIRI,
  Spano_GEM,
  Spano_GEMAXI,
  Goncalves_GEM,
  Goldstein_GEM,
  Goldstein_NAB,
  Goncalves_GEMSOR
) |> 
  rename(status = censored) |> 
  mutate(status = ifelse(status == FALSE, 1, 0)) |> 
  select(-c(PARAMCD)) |> 
  mutate(across(Treatment, as.factor))

# Parametric

Cunningham_Models <- fit_distribution(data = Net_Data |> filter(Study == "Cunningham"))
Kindler_Models <- fit_distribution(data = Net_Data |> filter(Study == "Kindler"))
Oettle_Models <- fit_distribution(data = Net_Data |> filter(Study == "Oettle"))
RochaLima_Models <- fit_distribution(data = Net_Data |> filter(Study == "RochaLima"))
Goldstein_Models <- fit_distribution(data = Net_Data |> filter(Study == "Goldstein"))
Spano_Models <- fit_distribution(data = Net_Data |> filter(Study == "Spano"))
Goncalves_Models <- fit_distribution(data = Net_Data |> filter(Study == "Goncalves"))
Conroy_Models <- fit_distribution(data = Net_Data |> filter(Study == "Conroy"))

Cunningham_Models_Plot <- plot(Cunningham_Models, km = TRUE)
Kindler_Models_Plot <- plot(Kindler_Models, km = TRUE)
Oettle_Models_Plot <- plot(Oettle_Models, km = TRUE)
RochaLima_Models_Plot <- plot(RochaLima_Models, km = TRUE)
Goldstein_Models_Plot <- plot(Goldstein_Models, km = TRUE)
Spano_Models_Plot <- plot(Spano_Models, km = TRUE)
Goncalves_Models_Plot <- plot(Goncalves_Models, km = TRUE)
Conroy_Models_Plot <- plot(Conroy_Models, km = TRUE)

ggsave(plot = Cunningham_Models_Plot, filename = "~/Documents/MScThesis/Results/Survival/Cunningham.png", width = 8, height = 6, units = "in")
ggsave(plot = Kindler_Models_Plot, filename = "~/Documents/MScThesis/Results/Survival/Kindler.png", width = 8, height = 6, units = "in")
ggsave(plot = Oettle_Models_Plot, filename = "~/Documents/MScThesis/Results/Survival/Oettle.png", width = 8, height = 6, units = "in")
ggsave(plot = RochaLima_Models_Plot, filename = "~/Documents/MScThesis/Results/Survival/RochaLima.png", width = 8, height = 6, units = "in")
ggsave(plot = Goldstein_Models_Plot, filename = "~/Documents/MScThesis/Results/Survival/Goldstein.png", width = 8, height = 6, units = "in")
ggsave(plot = Spano_Models_Plot, filename = "~/Documents/MScThesis/Results/Survival/Spano.png", width = 8, height = 6, units = "in")
ggsave(plot = Goncalves_Models_Plot, filename = "~/Documents/MScThesis/Results/Survival/Goncalves.png", width = 8, height = 6, units = "in")
ggsave(plot = Conroy_Models_Plot, filename = "~/Documents/MScThesis/Results/Survival/Conroy.png", width = 8, height = 6, units = "in")

param_model_table <- data.frame(Model = c("Exponential", "Gamma", "Generalised Gamma", "Gompertz", "Log-Logistic", "Log-Normal", "Weibull"))

Cunningham_Medians <- summary(Cunningham_Models, median = TRUE)
Kindler_Medians <- summary(Kindler_Models, median = TRUE)
Oettle_Medians <- summary(Oettle_Models, median = TRUE)
RochaLima_Medians <- summary(RochaLima_Models, median = TRUE)
Goldstein_Medians <- summary(Goldstein_Models, median = TRUE)
Spano_Medians <- summary(Spano_Models, median = TRUE)
Goncalves_Medians <- summary(Goncalves_Models, median = TRUE)

Param_Medians <- bind_rows(
  Cunningham_Medians |> mutate(Study = "Cunningham"),
  Kindler_Medians |> mutate(Study = "Kindler"),
  Oettle_Medians |> mutate(Study = "Oettle"),
  RochaLima_Medians |> mutate(Study = "Rocha Lima"),
  Goldstein_Medians |> mutate(Study = "Goldstein"),
  Spano_Medians |> mutate(Study = "Spano"),
  Goncalves_Medians |> mutate(Study = "Goncalves"),
)

write.xlsx(Param_Medians, "~/Documents/MScThesis/Results/Survival/Medians.xlsx", overwrite = TRUE)
