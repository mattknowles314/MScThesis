library(ggplot2)
library(tidyverse)

study_table <- read.csv("Data/study_table.csv") |> 
  select(Study, Treatment, Median_Raw = `Median.OS..Months.`) |> 
  extract(Median_Raw, into = c("Median", "L95", "U95"), regex = "([0-9\\.]+) \\(([0-9\\.]+), ([0-9\\.]+)\\)") %>%
  mutate(
    Median = as.numeric(Median),
    L95 = as.numeric(L95),
    U95 = as.numeric(U95)
  ) |> 
  mutate(across(Treatment, as.factor)) |> 
  arrange(Median)

GEM_Studies <- study_table |> filter(Treatment == "GEM") |> mutate(index = seq(1, 7, 1))
Comp_Studies <- study_table |> filter(Treatment != "GEM") |> mutate(index = seq(1, 7, 1))


p <- ggplot(GEM_Studies, aes(y = index, x = Median)) +
  geom_point(shape = 18, size = 5) +
  geom_errorbarh(aes(xmin = L95, xmax = U95), height = 0.2) +
  scale_y_continuous(name = "", breaks = 1:7, labels = GEM_Studies$Study, trans = "reverse") +
  theme_bw() +
  scale_x_continuous(breaks = seq(4, 12, 1)) +
  labs(
    x = "Median (OS) [Months]",
    y = "Study"
  )
ggsave(filename = "~/Documents/MScThesis/Results/Survival/GEM_Trial_Medians.png",
       width = 8, height = 6, units = "in")

p <- ggplot(Comp_Studies, aes(y = index, x = Median)) +
  geom_point(shape = 18, size = 5) +
  geom_errorbarh(aes(xmin = L95, xmax = U95), height = 0.2) +
  scale_y_continuous(name = "", breaks = 1:7, labels = paste0(Comp_Studies$Study, " (", Comp_Studies$Treatment, ")"), trans = "reverse") +
  scale_x_continuous(breaks = seq(4, 12, 1)) +
  theme_bw() +
  labs(
    x = "Median (OS) [Months]",
    y = "Study"
  )
ggsave(filename = "~/Documents/MScThesis/Results/Survival/Comp_Trial_Medians.png",
       width = 8, height = 6, units = "in")
