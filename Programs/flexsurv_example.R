Colucci_GEM <- read.csv("Data/IPD/IPD_Colucci_OS_GEM.csv")
Colucci_GEMCIS <- read.csv("Data/IPD/IPD_Colucci_OS_GEM-CIS.csv")

Colucci <- bind_rows(Colucci_GEM, Colucci_GEMCIS) |> 
  rename(status = censored) |> 
  select(-c(Study, PARAMCD)) |> 
  mutate(status = ifelse(status == FALSE, 1, 0))

Colucci_KM <- km_estimates(Colucci, "Treatment")

p <- plot_km(Colucci_KM)
p + labs(
  title = "Colucci 2010"
)
ggsave(p, filename = "Colucci2010OS.png", path = "../MScThesis/figures/KMs/",
       width = 7, height = 4, units = "in")

models <- fit_distribution(data = Colucci,
                 distributions = list(
                   "Exponential" = "exp",
                   "Gamma" = "gamma",
                   "Generalised Gamma" = "gengamma",
                   "Gompertz" = "gompertz",
                   "Log-logistic" = "llogis",
                   "Log-normal" = "lnorm",
                   "Weibull" = "weibull"
                 ), strata = "Treatment")

p <- plot(models,
     km = TRUE) +
  theme_bw()
ggsave(p, filename = "Colucci2010OS.png", path = "../MScThesis/figures/Models/OS/",
       width = 7, height = 4, units = "in")
