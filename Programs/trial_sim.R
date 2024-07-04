library(simsurv)
library(dplyr)
library(ggsurvfit)
devtools::load_all()

net <- readRDS("Data/network.RDs")

set.seed(12)

N <- 350
cens_prob <- 0.35
cov <- data.frame(id = 1:N,
                  trt = rbinom(N, 1, 0.5),
                  Male = rbinom(N, 1, 0.57275)) |> 
  mutate(trt = ifelse(trt == 1, "GEM", "GEM-SIM")) |> 
  mutate(trtclass = ifelse(trt == "GEM", "Placebo", "Active"))

dat <- simsurv(
  dist = "weibull",
  lambdas = 0.3,
  gammas = 1.3,
  x = cov,
  maxt = 40
)

sim_ipd <- merge(cov, dat) |> 
  rename(time = eventtime) |> 
  rename(Treatment = trt) |> 
  mutate(Study = "Simulated")

cens <- sim_ipd |> 
  slice_sample(n = floor(N*cens_prob), replace = FALSE)

sim_ipd <- sim_ipd |> 
  mutate(status = ifelse(id %in% cens$id, 0, 1))

p <- survfit2(Surv(time, status) ~ Treatment, data = sim_ipd) |> 
  ggsurvfit(linewidth = 1) +
  add_risktable() +
  scale_ggsurvfit() +
  theme_bw()
ggsave(p, filename = "~/Documents/MScThesis/Results/Survival/Simulated_IPD.png", width  = 12, height = 10, units = "in")

p <- survfit2(Surv(time, status) ~ Treatment, data = Net_Data |> filter(Study == "Goncalves")) |> 
  ggsurvfit(linewidth = 1) +
  add_risktable() +
  scale_ggsurvfit() +
  theme_bw()
ggsave(p, filename = "~/Documents/MScThesis/Results/Survival/Goldstein_KM.png", width = 12, height = 10, units = "in")


netSA <- combine_network(
  set_agd_surv(
    Net_Data |> mutate(trtclass = ifelse(Treatment == "GEM", "Placebo", "Active")),
    study = Study,
    trt = Treatment,
    Surv = Surv(time, status),
    covariates = covs,
    trt_class = trtclass
  ),
  set_ipd(
    sim_ipd,
    study = Study,
    trt = Treatment,
    Surv = Surv(time, status),
    trt_class = trtclass
  )
)

p <- plot(netSA,
          nudge = 0.1,
          weight_nodes = TRUE
) + 
  guides(edge_colour = guide_legend(override.aes = list(edge_width = 2))) +
  theme(legend.position = "bottom", legend.direction = "vertical")
ggsave(p, file = "~/Documents/MScThesis/figures/OS_Network_SA1.png", width = 12, height = 8, units = "in")

p <- ggplot() +
  geom_km(netSA) +
  facet_wrap(~.study) +
  labs(y = "Overall Survival",
       x = "Time (Months)") +
  theme(legend.position = "top", legend.box.spacing = unit(0, "lines")) +
  theme_bw()
ggsave(p, file = "~/Documents/MScThesis/figures/OS_KMs_SA1.png", width = 12, height = 12, units = "in")

netSA <- add_integration(netSA,
                       Male = distr(qbern, Male))

FE_Gamma <- fit_model(netSA, "fixed", llhood = "gamma")
saveRDS(FE_Gamma, "Data/Models/FE_Gamma_Sim1.RDs")
RE_Gamma <- fit_model(netSA, "random", llhood = "gamma")
saveRDS(RE_Gamma, "Data/Models/RE_Gamma_Sim1.RDs")
FE_Gomp <- fit_model(netSA, "fixed", llhood = "gompertz")
saveRDS(FE_Gomp, "Data/Models/FE_Gomp_Sim1.RDs")
RE_Gomp <- fit_model(netSA, "random", llhood = "gompertz")
saveRDS(RE_Gomp, "Data/Models/RE_Gomp_Sim1.RDs")
FE_GGam <- fit_model(netSA, "fixed", llhood = "gengamma")
saveRDS(FE_GGam, "Data/Models/FE_GGam_Sim1.RDs")
RE_GGam <- fit_model(netSA, "random", llhood = "gengamma")
saveRDS(RE_GGam, "Data/Models/RE_GGam_Sim1.RDs")
FE_Llogis <- fit_model(netSA, "fixed", llhood = "Loglogistic")
saveRDS(FE_Llogis, "Data/Models/FE_Llogis_Sim1.RDs")
RE_Llogis <- fit_model(netSA, "random", llhood = "Loglogistic")
saveRDS(RE_Gamma, "Data/Models/RE_Llogis_Sim1.RDs")
FE_Lnorm <- fit_model(netSA, "fixed", llhood = "Lognormal")
saveRDS(FE_Lnorm, "Data/Models/FE_Lnorm_Sim1.RDs")
RE_Lnorm <- fit_model(netSA, "random", llhood = "Lognormal")
saveRDS(RE_Lnorm, "Data/Models/RE_Lnorm_Sim1.RDs")
FE_Weib <- fit_model(netSA, "fixed", llhood = "weibull")
saveRDS(FE_Weib, "Data/Models/FE_Weib_Sim1.RDs")
RE_Weib <- fit_model(netSA, "random", llhood = "weibull")
saveRDS(RE_Weib, "Data/Models/RE_Weib_Sim1.RDs")

FE_Gamma_Summary <- summary(FE_Gamma, "Gamma", "Fixed")
RE_Gamma_Summary <- summary(RE_Gamma, "Gamma", "Random")
FE_GGam_Summary <- summary(FE_GGam, "Gen Gamma", "Fixed")
RE_GGam_Summary <- summary(RE_GGam, "Gen Gamma", "Random")
FE_Gomp_Summary <- summary(FE_Gomp, "Gompertz", "Fixed")
RE_Gomp_Summary <- summary(RE_Gomp, "Gompertz", "Random")
FE_LLogis_Summary <- summary(FE_Llogis, "Log-logistic", "Fixed")
RE_Llogis_Summary <- summary(RE_Llogis, "Log-logistic", "Random")
FE_Lnorm_Summary <- summary(FE_Lnorm, "Log-normal", "Fixed")
RE_Lnorm_Summary <- summary(RE_Lnorm, "Log-normal", "Random")
FE_Weib_Summary <- summary(FE_Weib, "Weibull", "Fixed")
RE_Weib_Summary <- summary(RE_Weib, "Weibull", "Random")

model_selection <- bind_rows(
  FE_Gamma_Summary,
  RE_Gamma_Summary,
  FE_GGam_Summary,
  RE_GGam_Summary,
  FE_Gomp_Summary,
  RE_Gomp_Summary,
  FE_LLogis_Summary,
  RE_Llogis_Summary,
  FE_Lnorm_Summary,
  RE_Lnorm_Summary,
  FE_Weib_Summary,
  RE_Weib_Summary
)

write.xlsx(model_selection, "~/Documents/MScThesis/Results/NMA/SelectionSA1.xlsx")
write.csv(model_selection, "~/Documents/MScThesis/Results/NMA/SelectionSA1.csv")

FE_Llogis <- readRDS("Data/Models/FE_Llogis_Sim1.RDs")

p <- plot(predict(FE_Llogis, type = "hazard")) +
  labs(x = "Time (Months)") +
  theme_bw() 
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/Hazard_Plot_SA1.png",
       height = 10, width = 12, units = "in")
p <- plot(predict(FE_Llogis, type = "survival")) +
  geom_km(netSA) +
  labs(x = "Time (Months)",
       y = "Overall Survival") +
  theme_bw() 
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/Survival_Plot_SA1.png",
       height = 10, width = 12, units = "in")
p <- plot(predict(FE_Llogis, type = "rmst")) +
  labs(x = "RMST (Months)") +
  theme_bw()
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/RMST_Plot_SA1.png",
       height = 10, width = 12, units = "in")
p <- plot(predict(FE_Llogis, type = "median"))
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/Median_Plot_SA1.png",
       height = 10, width = 12, units = "in") +
  labs(x = "Median Overall Survival (Months)") +
  theme_bw()

p <- mcmc_trace(FE_Llogis, pars = IPDParsforstan) +
  theme_bw()
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/Trace_SA1_FE.png",
       height = 10, width = 12, units = "in")

p <- mcmc_trace(RE_Llogis, pars = IPDParsforstan) +
  theme_bw()
ggsave(p, filename = "~/Documents/MScThesis/Results/NMA/Trace_SA1_RE.png",
       height = 10, width = 12, units = "in")