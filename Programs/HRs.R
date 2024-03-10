library(dplyr)
library(ggplot2)
library(tidyr)

plot_study <- function(data, study, xMax, breaks = 6, type = "survival") {
  data |> 
    filter(Study == study) |> 
    rename(status = censored) |> 
    mutate(status = ifelse(status == FALSE, 1, 0)) |> 
    km_estimates(strata = "Treatment") |> 
    plot(break.x.by = breaks, xMax = xMax, type = type)
}

get_hr <- function(data, study) {
  hr(data |> filter(Study == study), strata = "Treatment")
}

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
            RochaLima_GEMIRI)

p1 <- plot_study(df, "Colucci", xMax = 40)
p2 <- plot_study(df, "Cunningham", xMax = 30)
p3 <- plot_study(df, "Kindler", xMax = 15, breaks = 3)
p4 <- plot_study(df, "Oettle", xMax = 24)
p5 <- plot_study(df, "RochaLima", xMax = 30)

p6 <- plot_study(df, "Colucci", xMax = 40, type = "cumhaz")
p7 <- plot_study(df, "Cunningham", xMax = 40, type = "cumhaz")
p8 <- plot_study(df, "Kindler", xMax = 14, type = "cumhaz")
p9 <- plot_study(df, "Oettle", xMax = 25, type = "cumhaz")
p10 <- plot_study(df, "RochaLima", xMax = 30, type = "cumhaz")


ggsave(p1, filename = "Colucci.png", path = "~/Documents/MScThesis/figures/KMs/", 
       height = 8, width = 7.5, units = "in")
ggsave(p2, filename = "Cunningham.png", path = "~/Documents/MScThesis/figures/KMs/", 
       height = 8, width = 7.5, units = "in")
ggsave(p3, filename = "Kindler.png", path = "~/Documents/MScThesis/figures/KMs/", 
       height = 8, width = 7.5, units = "in")
ggsave(p4, filename = "Oettle.png", path = "~/Documents/MScThesis/figures/KMs/", 
       height = 8, width = 7.5, units = "in")
ggsave(p5, filename = "RochaLima.png", path = "~/Documents/MScThesis/figures/KMs/", 
       height = 8, width = 7.5, units = "in")

ggsave(p6, filename = "Colucci_CumHaz.png", path = "~/Documents/MScThesis/figures/KMs/", 
       height = 8, width = 7.5, units = "in")
ggsave(p7, filename = "Cunningham_CumHaz.png", path = "~/Documents/MScThesis/figures/KMs/", 
       height = 8, width = 7.5, units = "in")
ggsave(p8, filename = "Kindler_CumHaz.png", path = "~/Documents/MScThesis/figures/KMs/", 
       height = 8, width = 7.5, units = "in")
ggsave(p9, filename = "Oettle_CumHaz.png", path = "~/Documents/MScThesis/figures/KMs/", 
       height = 8, width = 7.5, units = "in")
ggsave(p10, filename = "RochaLima_CumHaz.png", path = "~/Documents/MScThesis/figures/KMs/", 
       height = 8, width = 7.5, units = "in")


colucciHR <- get_hr(df, "Colucci")
cunninghamHR <- get_hr(df, "Cunningham")
kindlerHR <- get_hr(df, "Kindler")
oettleHR <- get_hr(df, "Oettle")
rochalimaHR <- get_hr(df, "RochaLima")

HRTable <- data.frame(Study = c("Colucci", "Cunningham", "Kindler", "Oettle", "RochaLima"),
                      HR = c(
                        summary(colucciHR)$coefficients[[2]],
                        summary(cunninghamHR)$coefficients[[2]],
                        summary(kindlerHR)$coefficients[[2]],
                        summary(oettleHR)$coefficients[[2]],
                        summary(rochalimaHR)$coefficients[[2]]
                      ),
                      L95 = c(
                        summary(colucciHR)$conf.int[[3]],
                        summary(cunninghamHR)$conf.int[[3]],
                        summary(kindlerHR)$conf.int[[3]],
                        summary(oettleHR)$conf.int[[3]],
                        summary(rochalimaHR)$conf.int[[3]]
                      ),
                      U95 = c(
                        summary(colucciHR)$conf.int[[4]],
                        summary(cunninghamHR)$conf.int[[4]],
                        summary(kindlerHR)$conf.int[[4]],
                        summary(oettleHR)$conf.int[[4]],
                        summary(rochalimaHR)$conf.int[[4]]
                      )) 
openxlsx::write.xlsx(x = HRTable, file = "~/Documents/MScThesis/Results/KM/HRTable.xlsx", overwrite = TRUE)
