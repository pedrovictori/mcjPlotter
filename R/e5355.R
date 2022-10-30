library(mcjPlotter)
library(tidyverse)
conflict_prefer("filter", "dplyr")
library(plotrix)
library(ggpubr)
library(paletteer)
library(magrittr)
library(rstatix)

data = loadAndBind("e53/raw/", not.numeric = NULL) %>% 
  select(-c(cell.count, filename))  %>% 
  rename(timepoint = "time.point", `EGFR+` = "EGFR.") %>% 
  pivot_longer(!timepoint, names_to = "pop", 
               values_to = "count", values_drop_na = T) %>% 
  group_by(timepoint, pop) %>% 
  summarise(across(.cols = everything(),
                   list(mean = mean, sterr = std.error))) %>% 
  rename(mean = "count_mean", sterr = "count_sterr") %>%
  ungroup() %T>% 
  write_csv("e44/growth_data.csv")

p=plotCounts(data)
p


