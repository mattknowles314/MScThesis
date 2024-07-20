library(dplyr)
library(multinma)
library(openxlsx)
library(ggraph)
library(tidyr)
library(bayesplot)
devtools::load_all()

Cunningham_GEM <- read.csv("Data/IPD/IPD_Cunningham_OS_GEM.csv")
Cunningham_GEMCAP <- read.csv("Data/IPD/IPD_Cunningham_OS_GEM-CAP.csv")

Conroy_FOL <- read.csv("Data/IPD/IPD_Conroy_OS_FOL.csv")
Conroy_GEM <- read.csv("Data/IPD/IPD_Conroy_OS_GEM.csv") |> mutate(Study = "Conroy") |> mutate(Treatment = "GEM")

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
  Goncalves_GEMSOR
) |> 
  rename(status = censored) |> 
  mutate(status = ifelse(status == FALSE, 1, 0)) |> 
  select(-c(PARAMCD)) |> 
  mutate(across(Treatment, as.factor))

Net_Data$trtclass <- ifelse(Net_Data$Treatment == "GEM", "Placebo", "CombChemo")

#net <- gen_network(Net_Data, "GEM", covs = read.xlsx("Data/DEF.xlsx") |> select(Study, Treatment = Trt, Male) |> filter(Study != "Ueno" | Study != "Heinemann"))

covs = read.xlsx("Data/DEF.xlsx") |> 
  select(Study, Treatment = Trt, Male) |> 
  filter(Study != "Ueno" | Study != "Heinemann" | Study != "Goldstein")

# Males in Goldsteing

n_GEM_patients <- 430
prop_GEM_male <- 0.6
n_GEM_males <- rbinom(1, n_GEM_patients, prop_GEM_male)
n_GEM_females <- n_GEM_patients - n_GEM_males
genders_GEM <- c(rep("Male", n_GEM_males), rep("Female", n_GEM_females))
genders <- sample(genders_GEM)

Goldstein_GEM <- Goldstein_GEM |> 
  mutate(Male = ifelse(genders == "Male", 1, 0))

n_NAB_patients <- 431
prop_NAB_male <- 0.57
n_NAB_males <- rbinom(1, n_NAB_patients, prop_NAB_male)
n_NAB_females <- n_NAB_patients - n_NAB_males
genders_NAB <- c(rep("Male", n_NAB_males), rep("Female", n_NAB_females))
genders <- sample(genders_NAB)

Goldstein_NAB <- Goldstein_NAB |> 
  mutate(Male = ifelse(genders == "Male", 1, 0))

Goldstein <- bind_rows(
  Goldstein_GEM,
  Goldstein_NAB) |> 
  rename(status = censored) |> 
  mutate(status = ifelse(status == FALSE, 1, 0)) |> 
  mutate(across(Treatment, as.factor))

Goldstein$trtclass <- ifelse(Goldstein$Treatment == "GEM", "Placebo", "Active")

net_SA1 <- combine_network(
  set_ipd(
    Goldstein,
    trt = Treatment,
    study = Study,
    Surv = Surv(time, status),
    trt_class = trtclass),
  set_agd_surv(
    Net_Data |> filter(Study == "Cunningham"),
    trt = Treatment,
    study = Study,
    covariates = covs,
    Surv = Surv(time, status),
    trt_class = trtclass),
  set_agd_surv(
    Net_Data |> filter(Study == "Kindler"),
    trt = Treatment,
    study = Study,
    covariates = covs,
    Surv = Surv(time, status),
    trt_class = trtclass),
  set_agd_surv(
    Net_Data |> filter(Study == "Oettle"),
    trt = Treatment,
    study = Study,
    covariates = covs,
    Surv = Surv(time, status),
    trt_class = trtclass),
  set_agd_surv(
    Net_Data |> filter(Study == "RochaLima"),
    trt = Treatment,
    study = Study,
    covariates = covs,
    Surv = Surv(time, status),
    trt_class = trtclass),
  set_agd_surv(
    Net_Data |> filter(Study == "Spano"),
    trt = Treatment,
    study = Study,
    covariates = covs,
    Surv = Surv(time, status),
    trt_class = trtclass),
  set_agd_surv(
    Net_Data |> filter(Study == "Goncalves"),
    trt = Treatment,
    study = Study,
    covariates = covs,
    Surv = Surv(time, status),
    trt_class = trtclass),
  trt_ref = "GEM"
)

net_SA2 <- combine_network(
  set_ipd(
    Goldstein,
    trt = Treatment,
    study = Study,
    Surv = Surv(time, status),
    trt_class = trtclass),
  set_agd_surv(
    Net_Data |> filter(Study == "Cunningham"),
    trt = Treatment,
    study = Study,
    covariates = covs,
    Surv = Surv(time, status),
    trt_class = trtclass),
  set_agd_surv(
    Net_Data |> filter(Study == "Oettle"),
    trt = Treatment,
    study = Study,
    covariates = covs,
    Surv = Surv(time, status),
    trt_class = trtclass),
  set_agd_surv(
    Net_Data |> filter(Study == "RochaLima"),
    trt = Treatment,
    study = Study,
    covariates = covs,
    Surv = Surv(time, status),
    trt_class = trtclass),
  set_agd_surv(
    Net_Data |> filter(Study == "Spano"),
    trt = Treatment,
    study = Study,
    covariates = covs,
    Surv = Surv(time, status),
    trt_class = trtclass),
  set_agd_surv(
    Net_Data |> filter(Study == "Conroy"),
    trt = Treatment,
    study = Study,
    covariates = covs,
    Surv = Surv(time, status),
    trt_class = trtclass
  ),
  trt_ref = "GEM"
)

saveRDS(net_SA1, file = "Data/network_SA1.RDs")
saveRDS(net_SA2, file = "Data/network_SA2.RDs")

p <- plot(net_SA1,
          nudge = 0.1,
          weight_nodes = TRUE
) + 
  guides(edge_colour = guide_legend(override.aes = list(edge_width = 2))) +
  theme(legend.position = "bottom", legend.direction = "vertical")
ggsave(p, file = "~/Documents/MScThesis/figures/SA1_Network.png", width = 12, height = 8, units = "in")

p <- plot(net_SA2,
          nudge = 0.1,
          weight_nodes = TRUE
) + 
  guides(edge_colour = guide_legend(override.aes = list(edge_width = 2))) +
  theme(legend.position = "bottom", legend.direction = "vertical")
ggsave(p, file = "~/Documents/MScThesis/figures/SA2_Network.png", width = 12, height = 8, units = "in")

# SA1

FESA1_Llogis <- fit_model(net_SA1, "fixed", llhood = "Loglogistic")
saveRDS(FESA1_Llogis, "Data/Models/SA1/FE_Llogis.RDs")
gc()
RESA1_Llogis <- fit_model(net_SA1, "random", llhood = "Loglogistic")
saveRDS(RESA1_Llogis, "Data/Models/SA1/RE_Llogis.RDs")
gc()
FESA1_Lnorm <- fit_model(net_SA1, "fixed", llhood = "Lognormal")
saveRDS(FESA1_Lnorm, "Data/Models/SA1/FE_Lnorm.RDs")
gc()
RESA1_Lnorm <- fit_model(net_SA1, "random", llhood = "Lognormal")
saveRDS(RESA1_Lnorm, "Data/Models/SA1/RE_Lnorm.RDs")
gc()
FESA1_Weib <- fit_model(net_SA1, "fixed", llhood = "weibull")
saveRDS(FESA1_Weib, "Data/Models/SA1/FE_Weib.RDs")
gc()
RESA1_Weib <- fit_model(net_SA1, "random", llhood = "weibull")
saveRDS(RESA1_Weib, "Data/Models/SA1/RE_Weib.RDs")

FESA1_Llogis <- readRDS("Data/Models/SA1/FE_Llogis.RDs")
RESA1_Llogis <- readRDS("Data/Models/SA1/RE_Llogis.RDs")
FESA1_Lnorm <- readRDS("Data/Models/SA1/FE_Lnorm.RDs")
RESA1_Lnorm <- readRDS("Data/Models/SA1/RE_Lnorm.RDs")
FESA1_Weib <- readRDS("Data/Models/SA1/FE_Weib.RDs")
RESA1_Weib <- readRDS("Data/Models/SA1/RE_Weib.RDs")

FESA1_LLogis_Summary <- summary(FESA1_Llogis, "Log-logistic", "Fixed")
RESA1_Llogis_Summary <- summary(RESA1_Llogis, "Log-logistic", "Random")
FESA1_Lnorm_Summary <- summary(FESA1_Lnorm, "Log-normal", "Fixed")
RESA1_Lnorm_Summary <- summary(RESA1_Lnorm, "Log-normal", "Random")
FESA1_Weib_Summary <- summary(FESA1_Weib, "Weibull", "Fixed")
RESA1_Weib_Summary <- summary(RESA1_Weib, "Weibull", "Random")

model_selection_SA1 <- bind_rows(
  FESA1_LLogis_Summary,
  RESA1_Llogis_Summary,
  FESA1_Lnorm_Summary,
  RESA1_Lnorm_Summary,
  FESA1_Weib_Summary,
  RESA1_Weib_Summary
)

write.csv(model_selection_SA1, "~/Documents/MScThesis/Results/NMA/Selection_SA1.csv")

# SA2

FESA2_Llogis <- fit_model(net_SA2, "fixed", llhood = "Loglogistic")
saveRDS(FESA2_Llogis, "Data/Models/SA2/FE_Llogis.RDs")
gc()
RESA2_Llogis <- fit_model(net_SA2, "random", llhood = "Loglogistic")
saveRDS(RESA2_Llogis, "Data/Models/SA2/RE_Llogis.RDs")
gc()
FESA2_Lnorm <- fit_model(net_SA2, "fixed", llhood = "Lognormal")
saveRDS(FESA2_Lnorm, "Data/Models/SA2/FE_Lnorm.RDs")
gc()
RESA2_Lnorm <- fit_model(net_SA2, "random", llhood = "Lognormal")
saveRDS(RESA2_Lnorm, "Data/Models/SA2/RE_Lnorm.RDs")
gc()
FESA2_Weib <- fit_model(net_SA2, "fixed", llhood = "weibull")
saveRDS(FESA2_Weib, "Data/Models/SA2/FE_Weib.RDs")
gc()
RESA2_Weib <- fit_model(net_SA2, "random", llhood = "weibull")
saveRDS(RESA2_Weib, "Data/Models/SA2/RE_Weib.RDs")
FESA1_Llogis <- fit_model(net_SA1, "fixed", llhood = "Loglogistic")
saveRDS(FESA1_Llogis, "Data/Models/SA1/FE_Llogis.RDs")
gc()
RESA1_Llogis <- fit_model(net_SA1, "random", llhood = "Loglogistic")
saveRDS(RESA1_Llogis, "Data/Models/SA1/RE_Llogis.RDs")
gc()
FESA1_Lnorm <- fit_model(net_SA1, "fixed", llhood = "Lognormal")
saveRDS(FESA1_Lnorm, "Data/Models/SA1/FE_Lnorm.RDs")
gc()
RESA1_Lnorm <- fit_model(net_SA1, "random", llhood = "Lognormal")
saveRDS(RESA1_Lnorm, "Data/Models/SA1/RE_Lnorm.RDs")
gc()
FESA1_Weib <- fit_model(net_SA1, "fixed", llhood = "weibull")
saveRDS(FESA1_Weib, "Data/Models/SA1/FE_Weib.RDs")
gc()
RESA1_Weib <- fit_model(net_SA1, "random", llhood = "weibull")
saveRDS(RESA1_Weib, "Data/Models/SA1/RE_Weib.RDs")

FESA1_Llogis <- readRDS("Data/Models/SA1/FE_Llogis.RDs")
RESA1_Llogis <- readRDS("Data/Models/SA1/RE_Llogis.RDs")
FESA1_Lnorm <- readRDS("Data/Models/SA1/FE_Lnorm.RDs")
RESA1_Lnorm <- readRDS("Data/Models/SA1/RE_Lnorm.RDs")
FESA1_Weib <- readRDS("Data/Models/SA1/FE_Weib.RDs")
RESA1_Weib <- readRDS("Data/Models/SA1/RE_Weib.RDs")

FESA1_LLogis_Summary <- summary(FESA1_Llogis, "Log-logistic", "Fixed")
RESA1_Llogis_Summary <- summary(RESA1_Llogis, "Log-logistic", "Random")
FESA1_Lnorm_Summary <- summary(FESA1_Lnorm, "Log-normal", "Fixed")
RESA1_Lnorm_Summary <- summary(RESA1_Lnorm, "Log-normal", "Random")
FESA1_Weib_Summary <- summary(FESA1_Weib, "Weibull", "Fixed")
RESA1_Weib_Summary <- summary(RESA1_Weib, "Weibull", "Random")

model_selection_SA1 <- bind_rows(
  FESA1_LLogis_Summary,
  RESA1_Llogis_Summary,
  FESA1_Lnorm_Summary,
  RESA1_Lnorm_Summary,
  FESA1_Weib_Summary,
  RESA1_Weib_Summary
)

write.csv(model_selection_SA1, "~/Documents/MScThesis/Results/NMA/Selection_SA1.csv")

# SA2

FESA2_Llogis <- fit_model(net_SA2, "fixed", llhood = "Loglogistic")
saveRDS(FESA2_Llogis, "Data/Models/SA2/FE_Llogis.RDs")
gc()
RESA2_Llogis <- fit_model(net_SA2, "random", llhood = "Loglogistic")
saveRDS(RESA2_Llogis, "Data/Models/SA2/RE_Llogis.RDs")
gc()
FESA2_Lnorm <- fit_model(net_SA2, "fixed", llhood = "Lognormal")
saveRDS(FESA2_Lnorm, "Data/Models/SA2/FE_Lnorm.RDs")
gc()
RESA2_Lnorm <- fit_model(net_SA2, "random", llhood = "Lognormal")
saveRDS(RESA2_Lnorm, "Data/Models/SA2/RE_Lnorm.RDs")
gc()
FESA2_Weib <- fit_model(net_SA2, "fixed", llhood = "weibull")
saveRDS(FESA2_Weib, "Data/Models/SA2/FE_Weib.RDs")
gc()
RESA2_Weib <- fit_model(net_SA2, "random", llhood = "weibull")
saveRDS(RESA2_Weib, "Data/Models/SA2/RE_Weib.RDs")

FESA2_Llogis <- readRDS("Data/Models/SA2/FE_Llogis.RDs")
RESA2_Llogis <- readRDS("Data/Models/SA2/RE_Llogis.RDs")
FESA2_Lnorm <- readRDS("Data/Models/SA2/FE_Lnorm.RDs")
RESA2_Lnorm <- readRDS("Data/Models/SA2/RE_Lnorm.RDs")
FESA2_Weib <- readRDS("Data/Models/SA2/FE_Weib.RDs")
RESA2_Weib <- readRDS("Data/Models/SA2/RE_Weib.RDs")

FESA2_LLogis_Summary <- summary(FESA2_Llogis, "Log-logistic", "Fixed")
RESA2_Llogis_Summary <- summary(RESA2_Llogis, "Log-logistic", "Random")
FESA2_Lnorm_Summary <- summary(FESA2_Lnorm, "Log-normal", "Fixed")
RESA2_Lnorm_Summary <- summary(RESA2_Lnorm, "Log-normal", "Random")
FESA2_Weib_Summary <- summary(FESA2_Weib, "Weibull", "Fixed")
RESA2_Weib_Summary <- summary(RESA2_Weib, "Weibull", "Random")

model_selection_SA2 <- bind_rows(
  FESA2_LLogis_Summary,
  RESA2_Llogis_Summary,
  FESA2_Lnorm_Summary,
  RESA2_Lnorm_Summary,
  FESA2_Weib_Summary,
  RESA2_Weib_Summary
)

write.csv(model_selection_SA2, "~/Documents/MScThesis/Results/NMA/Selection_SA2.csv")

# SA1

p <- plot(predict(FESA1_Lnorm, type = "hazard")) +
  labs(x = "Time (Months)") +
  theme_bw() 
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/SA1/Hazard_Plot.png",
       height = 10, width = 12, units = "in")

p <- plot(predict(FESA1_Lnorm, type = "survival", width = c(0, 0))) +
  labs(x = "Time (Months)") +
  theme_bw() 
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/SA1/Survival_Plot.png",
       height = 10, width = 12, units = "in")
p <- plot(FESA1_Lnorm, type = "survival", study = "Goldstein")
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/SA1/Goldstein_Survival_Plot.png",
       height = 10, width = 12, units = "in")
p <- plot(FESA1_Lnorm, type = "survival", study = "Conroy")
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/SA1/Conroy_Survival_Plot.png",
       height = 10, width = 12, units = "in")

p <- plot(predict(FESA1_Lnorm, type = "rmst")) +
  labs(x = "RMST (Months)") +
  theme_bw()
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/SA1/RMST_Plot.png",
       height = 10, width = 12, units = "in")
p <- plot(predict(FESA1_Lnorm, type = "median")) +
  labs(x = "Median Overall Survival (Months)") +
  theme_bw()
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/SA1/Median_Plot.png",
       height = 10, width = 12, units = "in")

p <- mcmc_trace(FESA1_Lnorm, pars = parsForStanSA1) +
  theme_bw()
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/SA1/Trace.png",
       height = 10, width = 12, units = "in")
p <- mcmc_trace(RESA1_Lnorm, pars = parsForStanSA1) +
  theme_bw()
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/SA1/RE_Trace.png",
       height = 10, width = 12, units = "in")

p <- plot(relative_effects(FESA1_Lnorm), ref_line = 0,
          stat = "halfeye") +
  ggplot2::aes(slab_fill = ggplot2::after_stat(ifelse(x < 0, "darkred", "#7EBE91"))) +
  theme_bw()
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/SA1/Releff.png")

p <- plot(relative_effects(FESA1_Lnorm, all_contrasts = TRUE), ref_line = 0,
          stat = "halfeye") +
  ggplot2::aes(slab_fill = ggplot2::after_stat(ifelse(x < 0, "darkred", "#7EBE91"))) +
  theme_bw()
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/SA1/Pair_Releff.png",
       height = 14, width = 10, units = "in")

color_scheme_set("green")
p <- mcmc_parcoord(FESA1_Lnorm, pars = parsForStanSA1, np = nuts_params(FESA1_Lnorm$stanfit), transformations = function(x) {(x - mean(x))/sd(x)}) +
  labs(x = "Parameter") +
  theme_bw()
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/SA1/Parcoord.png", 
       height = 10, width = 12, units = "in")
p <- mcmc_parcoord(RESA1_Lnorm, pars = parsForStanSA1, np = nuts_params(RESA1_Lnorm$stanfit), transformations = function(x) {(x - mean(x))/sd(x)}) +
  labs(x = "Parameter") +
  theme_bw()
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/SA1/RE_Parcoord.png", 
       height = 10, width = 12, units = "in")

p <- mcmc_pairs(FESA1_Lnorm, np = nuts_params(FESA1_Lnorm$stanfit), pars = parsForStanSA1, off_diag_args = list(size = 0.75)) 
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/SA1/FE_Pairs.png", width = 12, height = 10, units = "in")
p <- mcmc_pairs(RESA1_Lnorm, np = nuts_params(RESA1_Lnorm$stanfit), pars = parsForStanSA1, off_diag_args = list(size = 0.75))
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/SA1/RE_Pairs.png", width = 12, height = 10, units = "in")

p <- plot_prior_posterior(FESA1_Lnorm, prior = "trt", overlay = "posterior",
                          post_args = list(fill = "#7EBE91"),
                          prior_args = list(colour = "darkred", size = 1)) +
  labs(x = "Value",
       y = "Density") +
  theme_bw()
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/SA1/prior_post.png", width = 12, height = 10, units = "in")

write.csv(posterior_rank_probs(FESA1_Lnorm, sucra = TRUE, cumulative = TRUE), "~/Documents/MScThesis/Results/NMA/SA1/SUCRA.csv")


FESA2_Llogis <- readRDS("Data/Models/SA2/FE_Llogis.RDs")
RESA2_Llogis <- readRDS("Data/Models/SA2/RE_Llogis.RDs")
FESA2_Lnorm <- readRDS("Data/Models/SA2/FE_Lnorm.RDs")
RESA2_Lnorm <- readRDS("Data/Models/SA2/RE_Lnorm.RDs")
FESA2_Weib <- readRDS("Data/Models/SA2/FE_Weib.RDs")
RESA2_Weib <- readRDS("Data/Models/SA2/RE_Weib.RDs")

FESA2_LLogis_Summary <- summary(FESA2_Llogis, "Log-logistic", "Fixed")
RESA2_Llogis_Summary <- summary(RESA2_Llogis, "Log-logistic", "Random")
FESA2_Lnorm_Summary <- summary(FESA2_Lnorm, "Log-normal", "Fixed")
RESA2_Lnorm_Summary <- summary(RESA2_Lnorm, "Log-normal", "Random")
FESA2_Weib_Summary <- summary(FESA2_Weib, "Weibull", "Fixed")
RESA2_Weib_Summary <- summary(RESA2_Weib, "Weibull", "Random")

model_selection_SA2 <- bind_rows(
  FESA2_LLogis_Summary,
  RESA2_Llogis_Summary,
  FESA2_Lnorm_Summary,
  RESA2_Lnorm_Summary,
  FESA2_Weib_Summary,
  RESA2_Weib_Summary
)

write.csv(model_selection_SA2, "~/Documents/MScThesis/Results/NMA/Selection_SA2.csv")

# SA1

p <- plot(predict(FESA1_Lnorm, type = "hazard")) +
  labs(x = "Time (Months)") +
  theme_bw() 
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/SA1/Hazard_Plot.png",
       height = 10, width = 12, units = "in")

p <- plot(predict(FESA1_Lnorm, type = "survival", width = c(0, 0))) +
  labs(x = "Time (Months)") +
  theme_bw() 
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/SA1/Survival_Plot.png",
       height = 10, width = 12, units = "in")
p <- plot(FESA1_Lnorm, type = "survival", study = "Goldstein")
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/SA1/Goldstein_Survival_Plot.png",
       height = 10, width = 12, units = "in")
p <- plot(FESA1_Lnorm, type = "survival", study = "Conroy")
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/SA1/Conroy_Survival_Plot.png",
       height = 10, width = 12, units = "in")

p <- plot(predict(FESA1_Lnorm, type = "rmst")) +
  labs(x = "RMST (Months)") +
  theme_bw()
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/SA1/RMST_Plot.png",
       height = 10, width = 12, units = "in")
p <- plot(predict(FESA1_Lnorm, type = "median")) +
  labs(x = "Median Overall Survival (Months)") +
  theme_bw()
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/SA1/Median_Plot.png",
       height = 10, width = 12, units = "in")


p <- mcmc_trace(FESA1_Lnorm, pars = parsForStanSA1) +
  theme_bw()
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/SA1/Trace.png",
       height = 10, width = 12, units = "in")
p <- mcmc_trace(RESA1_Lnorm, pars = parsForStanSA1) +
  theme_bw()
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/SA1/RE_Trace.png",
       height = 10, width = 12, units = "in")

p <- plot(relative_effects(FESA1_Lnorm), ref_line = 0,
          stat = "halfeye") +
  ggplot2::aes(slab_fill = ggplot2::after_stat(ifelse(x < 0, "darkred", "#7EBE91"))) +
  theme_bw()
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/SA1/Releff.png")

p <- plot(relative_effects(FESA1_Lnorm, all_contrasts = TRUE), ref_line = 0,
          stat = "halfeye") +
  ggplot2::aes(slab_fill = ggplot2::after_stat(ifelse(x < 0, "darkred", "#7EBE91"))) +
  theme_bw()
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/SA1/Pair_Releff.png",
       height = 14, width = 10, units = "in")

color_scheme_set("green")
p <- mcmc_parcoord(FESA1_Lnorm, pars = parsForStanSA1, np = nuts_params(FESA1_Lnorm$stanfit), transformations = function(x) {(x - mean(x))/sd(x)}) +
  labs(x = "Parameter") +
  theme_bw()
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/SA1/Parcoord.png", 
       height = 10, width = 12, units = "in")
p <- mcmc_parcoord(RESA1_Lnorm, pars = parsForStanSA1, np = nuts_params(RESA1_Lnorm$stanfit), transformations = function(x) {(x - mean(x))/sd(x)}) +
  labs(x = "Parameter") +
  theme_bw()
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/SA1/RE_Parcoord.png", 
       height = 10, width = 12, units = "in")

p <- mcmc_pairs(FESA1_Lnorm, np = nuts_params(FESA1_Lnorm$stanfit), pars = parsForStanSA1, off_diag_args = list(size = 0.75)) 
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/SA1/FE_Pairs.png", width = 12, height = 10, units = "in")
p <- mcmc_pairs(RESA1_Lnorm, np = nuts_params(RESA1_Lnorm$stanfit), pars = parsForStanSA1, off_diag_args = list(size = 0.75))
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/SA1/RE_Pairs.png", width = 12, height = 10, units = "in")

p <- plot_prior_posterior(FESA1_Lnorm, prior = "trt", overlay = "posterior",
                          post_args = list(fill = "#7EBE91"),
                          prior_args = list(colour = "darkred", size = 1)) +
  labs(x = "Value",
       y = "Density") +
  theme_bw()
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/SA1/prior_post.png", width = 12, height = 10, units = "in")

write.csv(posterior_rank_probs(FESA1_Lnorm, sucra = TRUE, cumulative = TRUE, lower_better = FALSE), "~/Documents/MScThesis/Results/NMA/SA1/SUCRA.csv")

p <- plot(posterior_rank_probs(FESA1_Lnorm, sucra = TRUE, cumulative = TRUE, lower_better = FALSE)) + ggplot2::facet_null() +
  ggplot2::aes(colour = Treatment)
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/SA1/SUCRA.png", width = 12, height = 10, units = "in")

# SA2

p <- plot(predict(FESA2_Llogis, type = "hazard")) +
  labs(x = "Time (Months)") +
  theme_bw() 
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/SA2/Hazard_Plot.png",
       height = 10, width = 12, units = "in")

p <- plot(predict(FESA2_Llogis, type = "survival", width = c(0, 0))) +
  labs(x = "Time (Months)") +
  theme_bw() 
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/SA2/Survival_Plot.png",
       height = 10, width = 12, units = "in")
p <- plot(FESA2_Llogis, type = "survival", study = "Goldstein")
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/SA2/Goldstein_Survival_Plot.png",
       height = 10, width = 12, units = "in")
p <- plot(FESA2_Llogis, type = "survival", study = "Conroy")
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/SA2/Conroy_Survival_Plot.png",
       height = 10, width = 12, units = "in")

p <- plot(predict(FESA2_Llogis, type = "rmst")) +
  labs(x = "RMST (Months)") +
  theme_bw()
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/SA2/RMST_Plot.png",
       height = 10, width = 12, units = "in")
p <- plot(predict(FESA2_Llogis, type = "median")) +
  labs(x = "Median Overall Survival (Months)") +
  theme_bw()
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/SA2/Median_Plot.png",
       height = 10, width = 12, units = "in")


p <- mcmc_trace(FESA2_Llogis, pars = parsForStanSA2) +
  theme_bw()
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/SA2/Trace.png",
       height = 10, width = 12, units = "in")
p <- mcmc_trace(RESA2_Llogis, pars = parsForStanSA2) +
  theme_bw()
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/SA2/RE_Trace.png",
       height = 10, width = 12, units = "in")

p <- plot(relative_effects(FESA2_Llogis), ref_line = 0,
          stat = "halfeye") +
  ggplot2::aes(slab_fill = ggplot2::after_stat(ifelse(x < 0, "darkred", "#7EBE91"))) +
  theme_bw()
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/SA2/Releff.png")

p <- plot(relative_effects(FESA2_Llogis, all_contrasts = TRUE), ref_line = 0,
          stat = "halfeye") +
  ggplot2::aes(slab_fill = ggplot2::after_stat(ifelse(x < 0, "darkred", "#7EBE91"))) +
  theme_bw()
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/SA2/Pair_Releff.png",
       height = 14, width = 10, units = "in")

color_scheme_set("green")
p <- mcmc_parcoord(FESA2_Llogis, pars = parsForStanSA2, np = nuts_params(FESA2_Llogis$stanfit), transformations = function(x) {(x - mean(x))/sd(x)}) +
  labs(x = "Parameter") +
  theme_bw()
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/SA2/Parcoord.png", 
       height = 10, width = 12, units = "in")
p <- mcmc_parcoord(RESA2_Llogis, pars = parsForStanSA2, np = nuts_params(RESA2_Llogis$stanfit), transformations = function(x) {(x - mean(x))/sd(x)}) +
  labs(x = "Parameter") +
  theme_bw()
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/SA2/RE_Parcoord.png", 
       height = 10, width = 12, units = "in")

p <- mcmc_pairs(FESA2_Llogis, np = nuts_params(FESA2_Llogis$stanfit), pars = parsForStanSA2, off_diag_args = list(size = 0.75)) 
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/SA2/FE_Pairs.png", width = 12, height = 10, units = "in")
p <- mcmc_pairs(RESA2_Llogis, np = nuts_params(RESA2_Llogis$stanfit), pars = parsForStanSA2, off_diag_args = list(size = 0.75))
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/SA2/RE_Pairs.png", width = 12, height = 10, units = "in")

p <- plot_prior_posterior(FESA2_Llogis, prior = "trt", overlay = "posterior",
                          post_args = list(fill = "#7EBE91"),
                          prior_args = list(colour = "darkred", size = 1)) +
  labs(x = "Value",
       y = "Density") +
  theme_bw()
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/SA2/prior_post.png", width = 12, height = 10, units = "in")

p <- plot(posterior_rank_probs(FESA2_Llogis, sucra = TRUE, cumulative = TRUE, lower_better = FALSE)) + ggplot2::facet_null() +
  ggplot2::aes(colour = Treatment)
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/SA2/SUCRA.png", width = 12, height = 10, units = "in")

write.csv(posterior_rank_probs(FESA2_Llogis, sucra = TRUE, cumulative = TRUE, lower_better = FALSE), "~/Documents/MScThesis/Results/NMA/SA2/SUCRA.csv")
