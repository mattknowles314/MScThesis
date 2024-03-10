library(dplyr)
library(ggplot2)
library(tidyr)

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

DATA_GEM <- bind_rows(
  Colucci_GEM,
  Cunningham_GEM,
  Kindler_GEM,
  Oettle_GEM,
  RochaLima_GEM
) |> 
  rename(status = censored) |> 
  mutate(status = ifelse(status == FALSE, 1, 0))

DATA_COMPARATORS <- bind_rows(
  Colucci_GEMCIS,
  Cunningham_GEMCAP,
  Kindler_GEMAXI,
  Oettle_GEMPEM,
  RochaLima_GEMIRI
) |> 
  rename(status = censored) |> 
  mutate(status = ifelse(status == FALSE, 1, 0))

# KM DATA

GEM_KM <- km_estimates(TTE = DATA_GEM, strata = "Study") 
p <- plot(GEM_KM, break.x.by = 3)
ggsave(p, filename = "OS_KM_GEM.png", path = "../MScThesis/figures/KMs/", 
       height = 8, width = 8, units = "in")
GEM_Summary <- summary(GEM_KM) |> mutate(Treatment = "GEM")

COMP_KM <- km_estimates(TTE = DATA_COMPARATORS, strata = c("Study"))
p <- plot(COMP_KM, break.x.by = 3)
ggsave(p, filename = "OS_KM_COMP.png", path = "../MScThesis/figures/KMs/",
       height = 8, width = 8, units = "in")
COMP_Summary <- summary(COMP_KM) |> mutate(Treatment = c("GEMCIS", "GEMCAP", "GEMAXI", "GEMPEM", "GEMIRI"))

overall_summary <- bind_rows(GEM_Summary, COMP_Summary)
openxlsx::write.xlsx(overall_summary, file = "../MScThesis/Results/KM/OS_Summary.xlsx")

# Parametric Models

GEM_Models <- fit_distribution(distributions = nice_parametric_dists, 
                               data = DATA_GEM,
                               strata = "Study")
p <- plot(GEM_Models, km = TRUE, linewidth = 0.5) +
  facet_wrap(~Study) +
  theme_bw()
ggsave(p, filename = "OS_GEM_Models.png", path = "../MScThesis/figures/Models/OS/",
       width = 7, height = 6, units = "in")

GEM_Models_Medians <- summary(GEM_Models, median = TRUE)
openxlsx::write.xlsx(GEM_Models_Medians, file = "../MScThesis/Results/Models/OS/GEM_Medians.xlsx")

GEM_Models_AIC <- summary(GEM_Models, AIC = TRUE)

COMP_Models <- fit_distribution(distributions = nice_parametric_dists,
                                data = DATA_COMPARATORS,
                                strata = "Study")
p <- plot(COMP_Models, km = TRUE, linewidth = 0.5) +
  facet_wrap(~Study) +
  theme_bw()
ggsave(p, filename = "OS_COMP_Models.png", path = "../MScThesis/figures/Models/OS/",
       width = 7, height = 6, units = "in")

COMP_Models_Medians <- summary(COMP_Models, median = TRUE)
openxlsx::write.xlsx(COMP_Models_Medians, file = "../MScThesis/Results/Models/OS/COMP_Medians.xlsx")

COMP_Models_AIC <- summary(COMP_Models, AIC = TRUE)

# NMA
inputData <- readxl::read_xlsx(path = "Data/MediansNMAData.xlsx") |> 
  mutate(across(Treatment, as.factor)) |> 
  mutate(across(Study, as.factor)) |> 
  rename(r = Events) |> 
  rename(treatment = Treatment)

net <- gen_network(inputData, "GEM")
