# packages ####
library(here)
library(tidyverse)


# data ####
cp.dir <- here('Data/2026', '2026-5-26_Cp_obs.csv')

cp = as_tibble(read.csv(cp.dir))

# cleaning ####

cp %>% 
  select(-21, -22, -23, -24, -25, -26)
