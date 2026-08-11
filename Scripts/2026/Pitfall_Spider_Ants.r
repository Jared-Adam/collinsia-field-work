# packages ####
library(tidyverse)

# data ####

pf <- X2026_7_23_Pitfall_SA_length
q <- X2026_7_22_QuadratIDs


# cleaning and such ####

q

pf <- pf %>% 
  rename(qtag = Quadrat)

inner_join(q, pf, by = "qtag") %>% 
