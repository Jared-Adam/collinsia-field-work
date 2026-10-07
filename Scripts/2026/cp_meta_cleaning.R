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



# combine the columns without local to get a factor-level BSQ of all in one column 
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

den_df <- bind_rows(df_1, df_2) %>% 
  print(n = Inf)


# plots ####

den_df %>%   
  mutate(el = case_when(b == '1' ~ '5400',
                        b == '2'  ~'6150',
                        b == '3'  ~ '6320',
                        b == '4'  ~ '6760', 
                        b == '5'  ~ '7200',
                        b == '6'  ~ '7900', 
                        b == '7'  ~ '8200',
                        b == '8'  ~ '8370',
                        b == '9'  ~ '8800',
                        b == '10'  ~ '9150', 
                        b == '1'   ~ '5500', 
                        b == '2'   ~ '5650', 
                        b == '3'   ~ '6300',
                        b == '4'   ~ '6650', 
                        b == '5' ~ '6950', 
                        b == '6'   ~ '7500', 
                        b == '7'   ~ '8120',
                        b == '8'  & rep == '1' ~ '8120',
                        b == '8'  & rep == '2' ~ '8300',
                        b == '9'  & rep == '1' ~ '8600',
                        b == '9'  & rep == '2' ~ '9000', 
                        b == '10'  ~ '9500',
                        b == '1'   ~ '5600',
                        b == '2'   ~ '5900',
                        b == '3'   ~ '6300',
                        b == '4'   ~ '6750', 
                        b == '5'   ~ '7100', 
                        b == '6'   ~ '7600',
                        b == '7'   ~ '8000')) %>%
  mutate(el = as.numeric(el)) %>% 
  mutate(el = el*0.3) %>% 
  ggplot(aes(x = el, y = den_mean))+
  geom_point()
