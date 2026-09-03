# packages ####
library(here)
library(tidyverse)


# data ####
cp.dir <- here('Data/2026', '2026-5-26_Cp_obs.csv')

cp = as_tibble(read.csv(cp.dir))

# cleaning ####

cp

cp %>% 
  select(-21, -22, -23, -24, -25, -26) %>% 
   slice(1:100) %>% 
  mutate(test =
           case_when(
             EFN.score == "1" ~ paste0("0", ",", EFN.ct),
             EFN.score == "2" ~ paste0(EFN.ct, ",", "0"),
             EFN.ct == "0" ~ paste0("0", ",", "0"),
             .default = EFN.ct
           )
  ) %>% 
  separate_wider_delim(
    cols = test,
    delim = ",",
    names = c("EFN_sc_2", "EFN_sc_1")) %>% 
  select(-c(18,19)) %>% 
  mutate(across(
    .cols = c(5:16, 18:20),
    .fns = ~case_when(
      D1_p == 'dead' ~ NA,
      TRUE ~ .x
    )
  )) %>%
  mutate(D1_p = case_when(
    D1_p == "Sen" | D1_p == 'sen' ~ '0', 
    .default = D1_p
  )) 


# pick up here, this is not working. Why?
# debug fail 
debug <- cp %>% 
  select(-21, -22, -23, -24, -25, -26) %>% 
  mutate(test =
           case_when(
             EFN.score == "1" ~ paste0("0", ",", EFN.ct),
             EFN.score == "2" ~ paste0(EFN.ct, ",", "0"),
             EFN.ct == "0" ~ paste0("0", ",", "0"),
             .default = EFN.ct
           )
  ) %>% 
  separate_wider_delim(
    test, 
    delim = ",", 
    names = c("EFN_sc_2", "EFN_sc_1"),
    too_few = "debug"
  ) 

debug %>% filter(!test_ok) %>% 
  relocate(EFN.ct, EFN.score, test)




