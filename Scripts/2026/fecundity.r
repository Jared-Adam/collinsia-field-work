# packages ####
library(here)
library(tidyverse)
library(ggridges)

# DFs ####
labeled_df

# fruit and flower ####

fec_df <- labeled_df %>% 
  select(Date, loc, band, el, qid, rep, Tag, PH, PD, FlC, FrC)

fec_df %>%
  select(-rep) %>% 
  filter(if_any(everything(), is.na)) %>% 
  print(n = Inf)
# when back in bozeman : There are several instances here where the q id is not a plot
# need to determine where these belong and then add them back to the other dfs/ wranglings too

# 457 = MC



  ggplot(aes(x = el, FrC))+
  geom_point()
