library(ggplot2)
library(tidyr)
library(dplyr)
library(sf)
library(rnaturalearth)

df <- readxl::read_xlsx("Data/inc_asr_mfp_pancreatic_i18.xlsx", range = "A5:Y8") |> 
  pivot_longer(names_to = "Year", cols = -Sex) |> 
  rename(Incidence = value) |> 
  mutate(Sex = ifelse(Sex == "Persons", "All", Sex)) |> 
  mutate(Year = )

p <- ggplot(df, aes(x = Year, y = Incidence, group = Sex)) +
  geom_line(aes(colour = Sex)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Year",
       y = "Incidence Rate (Per 100,000 people)") +
  scale_y_continuous(breaks = seq(0, 20, 1), limits = c(0, 20))
ggsave(p, filename = "~/Documents/MScThesis/figures/IncStat.png", width = 6, height = 5, units = "in")

location_df <- readxl::read_xlsx("Data/cases_rates_mfp_pancreatic_i19.xlsx", range = "A5:G20") |> 
  filter(Metric == "Cases") |> 
  select(-Metric) |> 
  pivot_longer(names_to = "Country", cols = -Sex) |> 
  rename(Geounit = Country)

uk <- ne_countries(type = "map_units", country = "United Kingdom", scale = "medium") |> 
  mutate(Incidence = c(538, 856, 267, 9125)) |> 
  mutate(Cancer = c(19847, 33863, 10075, 321693)) |> 
  mutate(Perc = 100*Incidence/Cancer)

p1 <- ggplot(data = uk) +
  geom_sf(fill = "#7EBE91") +
  geom_text(aes(label = paste(Incidence, " (", round(Perc, 2),"%)", sep = ""), x = -10, y = label_y), 
            size = 4, 
            color = "black") +
  geom_segment(y = 56.79237, x = -8.3, xend = -4) +
  geom_segment(y = 54.65551, x = -8.3, xend = -7) +
  geom_segment(y = 52.60981, x = -8.2, xend = -2) +
  geom_segment(y = 52.13586, x = -8.3, xend = -4) +
  scale_x_continuous(limits = c(-12, 2)) +
  scale_y_continuous(limits = c(50,59)) +
  labs(x = "Longitude",
       y = "Latitude") +
  theme_bw()
ggsave(p1, filename = "~/Documents/MScThesis/figures/incidence_map.png", width = 5, height = 5, units = "in")
