# packages ####
library(tidyverse)

# DFs ####
labeled_df

# EFN vis ####
labeled_df

labeled_df %>% 
  mutate(EFN.ct = as.numeric(EFN.ct)) %>% 
  ggplot(aes(el, EFN.ct))+
  geom_smooth(method = 'gam',
formula = y~s (x, k = 4))+
  geom_point()
