library(multinma)
library(dplyr)
library(openxlsx)
library(ggraph)
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

net <- combine_network(
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

p <- plot(net,
          nudge = 0.1,
          weight_nodes = TRUE
          ) + 
  guides(edge_colour = guide_legend(override.aes = list(edge_width = 2))) +
  theme(legend.position = "bottom", legend.direction = "vertical")
ggsave(p, file = "~/Documents/MScThesis/figures/OS_Network.png", width = 12, height = 8, units = "in")

p <- ggplot() +
  geom_km(net) +
  facet_wrap(~.study) +
  labs(y = "Overall Survival",
       x = "Time (Months)") +
  theme(legend.position = "top", legend.box.spacing = unit(0, "lines")) +
  theme_bw()
ggsave(p, file = "~/Documents/MScThesis/figures/OS_KMs.png", width = 12, height = 12, units = "in")

p <- ggplot() +
  geom_km(net, transform = "cloglog") +
  facet_wrap(~.study) +
  labs(y = "Overall Survival",
       x = "Time (Months)") +
  theme(legend.position = "top", legend.box.spacing = unit(0, "lines")) +
  theme_bw()
ggsave(p, file = "~/Documents/MScThesis/figures/OS_LogCumulatives.png", width = 12, height = 12, units = "in")

net <- add_integration(net,
                       Male = distr(qbern, Male))
saveRDS(net, "Data/network.RDs")
