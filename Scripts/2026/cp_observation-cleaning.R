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
# there are several rows with nonsense. some dots, some unsre
# starting with removal of dots

cp %>% 
  filter(str_detect(EFN.ct, "\\.")) %>% # dots are wildcards, so must be preceded with \\
  mutate(EFN.ct = str_replace_all(EFN.ct, "\\.", ","))



debug <- cp %>% 
  select(-21, -22, -23, -24, -25, -26) %>%
  mutate(EFN.ct = str_replace_all(EFN.ct, "\\.", ",")) %>% 
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
  ) %>% 
  filter(test_ok == TRUE) # removing the confusing rows for now

# 10/6/2026:
  # these do not work becuase the score column is erroneous 
  # ct has a value >0 but the score is incorrect. These must be found in the data sheets

look <- debug %>% filter(!test_ok) %>% 
  relocate(EFN.ct, EFN.score, test) %>% 
  print(n = Inf)

# EFN vis ####

next_df <- debug

# DROPPING NA FOR NOW (10/6/2026)
again_df <- next_df %>% 
  mutate(across(where(is.character), ~na_if(., "na"))) %>% 
  drop_na()

