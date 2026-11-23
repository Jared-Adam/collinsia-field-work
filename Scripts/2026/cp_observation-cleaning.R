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


