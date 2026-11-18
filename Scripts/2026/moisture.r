# packages ####
library(tidyverse)
library(here)

# data ####

mois_dir <- here("Data/2026", "2026-5-26_Cp_soil_moisture.csv")
moisture <- as_tibble(read.csv(mois_dir))


# cleaning ####

moisture %>% 
  select(1:12) %>% 
  rename(TL_p = TL_.,
    BR_p = BR_.) %>% 
  mutate(avg_m = (TL_p + BR_p)/2,
    avg_temp = (TL_Temp+BR_Temp)/2,
    avg_perm = (TL_Perm+BR_Perm)/ 2,
    avg_cond = (TL_Cond+BR_Cond)/2) %>% 
  relocate(Date, Qdrat.Tag, avg_m, avg_temp, avg_perm, avg_cond) %>% 
  select(1:6) %>% 
  ggplot(aes(x = Date, y = avg_m, group = Qdrat.Tag))+
  geom_point()+
  geom_line()

# Write a function to assign locations and elevations 