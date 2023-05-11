# read-data.R
# Dylan Schwilk 2023

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

drydown <- read_csv("../data/shrub_drydown_test.csv")

drydown_long <- drydown %>% select(-note) %>%
  pivot_longer(cols=starts_with("hr_"), names_to="time",
               values_to="water_potential",
               names_pattern="hr_([0-9\\.]+)") %>%
  mutate(time = as.numeric(time), water_potential = -1*water_potential)


drydown_long %>%
  ggplot(aes(time, water_potential, color=plantID)) +
  facet_grid(. ~ species) +
  geom_line() +
  xlab("Bench drying time (hr)") + ylab("Water potential (MPa)")
