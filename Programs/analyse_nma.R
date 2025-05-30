library(dplyr)
library(multinma)
library(openxlsx)
library(ggraph)
library(tidyr)
library(bayesplot)
devtools::load_all()

net <- readRDS("Data/network.RDs")

FE_Llogis <- readRDS("Data/Models/FE_Llogis.RDs")
RE_Llogis <- readRDS("Data/Models/RE_Llogis.RDs")
FE_Lnorm <- readRDS("Data/Models/FE_Lnorm.RDs")
RE_Lnorm <- readRDS("Data/Models/RE_Lnorm.RDs")
FE_Weib <- readRDS("Data/Models/FE_Weib.RDs")
RE_Weib <- readRDS("Data/Models/RE_Weib.RDs")

FE_LLogis_Summary <- summary(FE_Llogis, "Log-logistic", "Fixed")
RE_Llogis_Summary <- summary(RE_Llogis, "Log-logistic", "Random")
FE_Lnorm_Summary <- summary(FE_Lnorm, "Log-normal", "Fixed")
RE_Lnorm_Summary <- summary(RE_Lnorm, "Log-normal", "Random")
FE_Weib_Summary <- summary(FE_Weib, "Weibull", "Fixed")
RE_Weib_Summary <- summary(RE_Weib, "Weibull", "Random")

model_selection <- bind_rows(
  FE_LLogis_Summary,
  RE_Llogis_Summary,
  FE_Lnorm_Summary,
  RE_Lnorm_Summary,
  FE_Weib_Summary,
  RE_Weib_Summary
)

write.xlsx(model_selection, "~/Documents/MScThesis/Results/NMA/Selection.xlsx")
write.csv(model_selection, "~/Documents/MScThesis/Results/NMA/Selection.csv")

p <- plot(predict(FE_Llogis, type = "hazard")) +
  labs(x = "Time (Months)") +
  theme_bw() 
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/Hazard_Plot.png",
       height = 10, width = 12, units = "in")

p <- plot(predict(FE_Llogis, type = "survival", width = c(0, 0))) +
  labs(x = "Time (Months)") +
  theme_bw() 
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/Survival_Plot.png",
       height = 10, width = 12, units = "in")
p <- plot(FE_Llogis, type = "survival", study = "Goldstein")
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/Goldstein_Survival_Plot.png",
       height = 10, width = 12, units = "in")
p <- plot(FE_Llogis, type = "survival", study = "Conroy")
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/Conroy_Survival_Plot.png",
       height = 10, width = 12, units = "in")

p <- plot(predict(FE_Llogis, type = "rmst")) +
  labs(x = "RMST (Months)") +
  theme_bw()
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/RMST_Plot.png",
       height = 10, width = 12, units = "in")
p <- plot(predict(FE_Llogis, type = "median")) +
  labs(x = "Median Overall Survival (Months)") +
  theme_bw()
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/Median_Plot.png",
       height = 10, width = 12, units = "in")

p <- mcmc_trace(FE_Llogis, pars = parsForStan) +
  theme_bw()
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/Trace.png",
       height = 10, width = 12, units = "in")
p <- mcmc_trace(RE_Llogis, pars = parsForStan) +
  theme_bw()
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/RE_Trace.png",
       height = 10, width = 12, units = "in")

p <- plot(relative_effects(FE_Llogis), ref_line = 0,
          stat = "halfeye") +
  ggplot2::aes(slab_fill = ggplot2::after_stat(ifelse(x < 0, "darkred", "#7EBE91"))) +
  theme_bw()
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/Releff.png")

p <- plot(relative_effects(FE_Llogis, all_contrasts = TRUE), ref_line = 0,
          stat = "halfeye") +
  ggplot2::aes(slab_fill = ggplot2::after_stat(ifelse(x < 0, "darkred", "#7EBE91"))) +
  theme_bw()
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/Pair_Releff.png",
       height = 14, width = 10, units = "in")

color_scheme_set("green")
p <- mcmc_parcoord(FE_Llogis, pars = parsForStan, np = nuts_params(FE_Llogis$stanfit), transformations = function(x) {(x - mean(x))/sd(x)}) +
  labs(x = "Parameter") +
  theme_bw()
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/Parcoord.png", 
       height = 10, width = 12, units = "in")
p <- mcmc_parcoord(RE_Llogis, pars = parsForStan, np = nuts_params(RE_Llogis$stanfit), transformations = function(x) {(x - mean(x))/sd(x)}) +
  labs(x = "Parameter") +
  theme_bw()
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/RE_Parcoord.png", 
       height = 10, width = 12, units = "in")

p <- mcmc_pairs(FE_Llogis, np = nuts_params(FE_Llogis$stanfit), pars = parsForStan, off_diag_args = list(size = 0.75)) 
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/FE_Pairs.png", width = 12, height = 10, units = "in")
p <- mcmc_pairs(RE_Llogis, np = nuts_params(RE_Llogis$stanfit), pars = parsForStan, off_diag_args = list(size = 0.75))
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/RE_Pairs.png", width = 12, height = 10, units = "in")

p <- plot_prior_posterior(FE_Llogis, prior = "trt", overlay = "posterior",
                     post_args = list(fill = "#7EBE91"),
                     prior_args = list(colour = "darkred", size = 1)) +
  labs(x = "Value",
       y = "Density") +
  theme_bw()
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/prior_post.png", width = 12, height = 10, units = "in")

write.csv(posterior_rank_probs(FE_Llogis, sucra = TRUE, cumulative = TRUE), "~/Documents/MScThesis/Results/NMA/SUCRA.csv")
