# packages ####
library(here)
library(tidyverse)
library(ggridges)

# call in data via fxn ####

source(here("Scripts/2026/fxns", "cp_cleaning_fxn.R"))
dfs <- cp_clean_function()

tags_complete <- dfs$tags_complete 
max_value_df <- dfs$max_value_df
e_df <- dfs$e_df
final_cp <- dfs$final_cp


# old plot I saved here ####
# plot of the max means... Why?
e_df %>% 
  ggplot(aes(x = el, y = mean))+
  geom_point(size = 2)+
  facet_grid(cols = vars(dmg_type))+
  xlim(1500,3000)+
  ylim(0,15)+
  geom_smooth(method = 'gam',
formula = y~s(x, k=4))+
  labs(title = "Mean damage percent x damage type",
y = "Mean percent damage",
x = "Elevation (m)")+
  theme_bw()