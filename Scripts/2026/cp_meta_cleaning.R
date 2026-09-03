# packages ####
library(here)
library(tidyverse)


# data ####
meta.dir <- here('Data/2026', '2026-5-26_Cp_meta.csv')

meta = as_tibble(read.csv(meta.dir))

# cleaning densiometer readings: yearly avg####

# get the mean densiometer
df_1 <- meta %>%
  # slice(1:20) %>%
  filter(Band.Site.Qdrat != 'na' & L_n != "na") %>% 
  mutate_at(8:11, as.numeric) %>% 
  mutate_at(c('L_n', 'L_e', 'L_s', 'L_w'), ~.  * 1.04) %>% 
  select(1:11) %>% 
  group_by(b,s,q) %>% 
  mutate(den_mean = rowMeans(across(c('L_n', 'L_e', 'L_s', 'L_w')))) %>% 
  select(-8, -9, -10, -11) %>% 
  rename(local = Band.Site.Qdrat) %>% 
  ungroup() 
  # summarise(across(everything(), ~sum(is.na(.))))



# combine the coluns to get a factor-level BSQ of all in one column 
df_2 <- meta %>% 
  filter(Band.Site.Qdrat == 'na' & L_n != 'na') %>% 
  mutate_at(8:11, as.numeric) %>% 
  mutate_at(c('L_n', 'L_e', 'L_s', 'L_w'), ~.  * 1.04) %>% 
  group_by(b,s,q) %>% 
  mutate(den_mean = rowMeans(across(c('L_n', 'L_e', 'L_s', 'L_w')))) %>% 
  # slice(1:10) %>% 
  mutate(local = 
           paste0("B", b, "S", s, "Q", q, "R", rep)) %>% 
  relocate(Date, local) %>% 
  select(c(1,2,4:8, 20)) %>% 
  ungroup() %>% 
  # summarise(across(everything(), ~sum(is.na(.)))) %>% 
  mutate(q = replace_na(q, 1)) 


# joining

bind_rows(df_1, df_2) %>% 
  print(n = Inf)
