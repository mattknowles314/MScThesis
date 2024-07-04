library(dplyr)
library(multinma)
library(openxlsx)
library(ggraph)
library(tidyr)
library(bayesplot)
devtools::load_all()

net <- readRDS("Data/network.RDs")

# FE_Gamma <- readRDS("Data/Models/FE_Gamma.RDs")
# RE_Gamma <- readRDS("Data/Models/RE_Gamma.RDs")
# FE_GGam <- readRDS("Data/Models/FE_GGam.RDs")
# RE_GGam <- readRDS("Data/Models/RE_GGam.RDs")
# FE_Gomp <- readRDS("Data/Models/FE_GGam.RDs")
# RE_Gomp <- readRDS("Data/Models/RE_GGam.RDs")
FE_LLogis <- readRDS("Data/Models/FE_Llogis.RDs")
RE_Llogis <- readRDS("Data/Models/RE_Llogis.RDs")
FE_Lnorm <- readRDS("Data/Models/FE_Lnorm.RDs")
RE_Lnorm <- readRDS("Data/Models/RE_Lnorm.RDs")
FE_LLogis <- readRDS("Data/Models/FE_Weib.RDs")
RE_Weib <- readRDS("Data/Models/RE_Weib.RDs")

# FE_Gamma_Summary <- summary(FE_Gamma, "Gamma", "Fixed")
# RE_Gamma_Summary <- summary(RE_Gamma, "Gamma", "Random")
# FE_GGam_Summary <- summary(FE_GGam, "Gen Gamma", "Fixed")
# RE_GGam_Summary <- summary(RE_GGam, "Gen Gamma", "Random")
# FE_Gomp_Summary <- summary(FE_Gomp, "Gompertz", "Fixed")
# RE_Gomp_Summary <- summary(RE_Gomp, "Gompertz", "Random")
FE_LLogis_Summary <- summary(FE_Llogis, "Log-logistic", "Fixed")
RE_Llogis_Summary <- summary(RE_Llogis, "Log-logistic", "Random")
FE_Lnorm_Summary <- summary(FE_Lnorm, "Log-normal", "Fixed")
RE_Lnorm_Summary <- summary(RE_Lnorm, "Log-normal", "Random")
FE_Weib_Summary <- summary(FE_Weib, "Weibull", "Fixed")
RE_Weib_Summary <- summary(RE_Weib, "Weibull", "Random")

model_selection <- bind_rows(
  # FE_Gamma_Summary,
  # RE_Gamma_Summary,
  # FE_GGam_Summary,
  # RE_GGam_Summary,
  # FE_Gomp_Summary,
  # RE_Gomp_Summary,
  FE_LLogis_Summary,
  RE_Llogis_Summary,
  FE_Lnorm_Summary,
  RE_Lnorm_Summary,
  FE_Weib_Summary,
  RE_Weib_Summary
)

write.xlsx(model_selection, "~/Documents/MScThesis/Results/NMA/Selection.xlsx")
write.csv(model_selection, "~/Documents/MScThesis/Results/NMA/Selection.csv")

p <- plot(predict(FE_Lnorm, type = "hazard"), width = c(0, 0)) +
  labs(x = "Time (Months)") +
  theme_bw() 
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/Hazard_Plot.png",
       height = 10, width = 12, units = "in")
p <- plot(predict(FE_Lnorm, type = "survival", width = c(0, 0)) +
  geom_km(net) +
  labs(x = "Time (Months)",
       y = "Overall Survival") +
  theme_bw() 
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/Survival_Plot.png",
       height = 10, width = 12, units = "in")
p <- plot(predict(FE_Lnorm, type = "rmst")) +
  labs(x = "RMST (Months)") +
  theme_bw()
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/RMST_Plot.png",
       height = 10, width = 12, units = "in")
p <- plot(predict(FE_Lnorm, type = "median"))
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/Median_Plot.png",
       height = 10, width = 12, units = "in") +
  labs(x = "Median Overall Survival (Months)") +
  theme_bw()

p <- mcmc_trace(FE_Lnorm, pars = parsForStan) +
  theme_bw()
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/Trace.png",
       height = 10, width = 12, units = "in")
