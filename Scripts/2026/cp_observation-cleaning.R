# packages ####
library(here)
library(tidyverse)
library(ggridges)


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
  ) 

# extract the TRUE
test_ok_df <- debug %>% 
  filter(test_ok == TRUE) # removing the confusing rows for now

# extract the FALSE
test_false_df <- debug %>% filter(!test_ok) %>% 
  relocate(EFN.ct, EFN.score, test) %>% 
  print(n = Inf)

# SOLUTION: I have a test_ok column that I will use to parce the case_when
  # I still need to subset and then merge after the corrections have been made

colnames(test_false_df)
false_clean_df <- test_false_df %>% 
  mutate(EFN.score = case_when(test_ok == FALSE & Qdrat.ID == 683 & Tag == 28 ~ 1,
                              test_ok == FALSE & Qdrat.ID == 76 & Tag == 132 ~ 1,
                              test_ok == FALSE & Qdrat.ID == 76 & Tag == 135 ~ 1,
                              test_ok == FALSE & Qdrat.ID == 76 & Tag == 123 ~ 1,
                              test_ok == FALSE & Qdrat.ID == 691 & Tag == 11 ~ 1,
                              test_ok == FALSE & Qdrat.ID == 691 & Tag == 2 ~ 1,
                              test_ok == FALSE & Qdrat.ID == 619 & Tag == 9 ~ 2,
                              test_ok == FALSE & Qdrat.ID == 619 & Tag == 1 ~ 2,
                              test_ok == FALSE & Qdrat.ID == 650 & Tag == 10 ~ 1,
                              test_ok == FALSE & Qdrat.ID == 883 & Tag == 234 ~ 1,
  .default = as.numeric(EFN.score))) %>%
  mutate(EFN.ct = case_when(test_ok == FALSE & Qdrat.ID == 76 & Tag == 132 ~ 2,
                            test_ok == FALSE & Qdrat.ID == 76 & Tag == 135 ~ 2,
                            test_ok == FALSE & Qdrat.ID == 76 & Tag == 123 ~ 2,
                            test_ok == FALSE & Qdrat.ID == 691 & Tag == 11 ~ 12,
                          .default = as.numeric(EFN.ct))
                        ) %>% 
  select(-c(3,22,23,24,25,26)) %>% # need to remove the old columns from when I ran delim earlier
  mutate(test =
           case_when(
             EFN.score == "1" ~ paste0("0", ",", EFN.ct),
             EFN.score == "2" ~ paste0(EFN.ct, ",", "0"),
             EFN.ct == "0" ~ paste0("0", ",", "0")
           )
  ) %>% 
  separate_wider_delim(
    test, 
    delim = ",", 
    names = c("EFN_sc_2", "EFN_sc_1"),
    too_few = "debug"
  ) %>% 
  print(n = Inf)


# joining the old and new dfs

false_clean_df
ready <- test_ok_df %>% 
  mutate_at(18:19, as.numeric)

joined_df <- full_join(false_clean_df, ready)



# NOTE: 10/21/2026 
  # I am NOT removing NAs anymore. I cannot recall why I removed them originally 
# OLD: now getting rid of the NAs as they are not pertinent to EFNs
next_df <- joined_df

###
##
#
# again_df <- next_df %>% 
#   mutate(across(where(is.character), ~na_if(., "na"))) %>% 
#   drop_na()
#
##
###


# add elevation ####
# going to add a band column and assign that by qid and then add elev ~ band

labeled_df <- next_df %>% 
  rename(id = Qdrat.ID) %>% 
  mutate(band = case_when(
    id == "813" | id == '838' | id == "794" | id == "71" | id == "893" | id == "891" | id == "914" | id == "945" | id == "898" ~ "1",
    id == "883" | id == "34" | id == "15" | id == "76" | id == "80" | id == "6" | id == " 457" | id == "416" | id == "458" ~ "2",
    id == "3" | id == "29" | id == "433"| id == "96" | id == "38"| id == "94"| id == "667"| id == "815" | id == "610" ~ "3",
    id == "320"| id == "387" | id == "302" | id == "623" | id == "639" | id == "687" | id == "259" | id == "258" | id == "285" ~ "4",
    id == "311" | id == "319" | id == "347" | id == "574" | id == "695" | id == "636" | id == "218" | id == "240" | id == "227" ~ "5",
    id == "652" | id == "989" | id == "626" | id == "674" | id == "660" | id == "683" | id == "634" | id == "611" | id == "609" ~ "6",
    id == "696" | id == "670" | id == "899" | id == "624" | id == "672" | id == "632" | id == "614" | id == "629" | id == "642" ~ "7",
    id == "699" | id == "98" | id == "640" | id == "698" | id == "697" | id == "604" | id == "619" | id == "691" | id == "650" ~ "8",
    id == "655" | id == "700" | id == "41" | id == "620" | id == "78" | id == "36" | id == "263" | id == "17" | id == "730" ~ "9",
    id == "63" | id == "905" | id == "105" | id == "88" | id == "49" | id == "637" | id == "27" | id == "13" | id == "81" ~ "10",
    .default = as.character(id)
  )) %>% 
  mutate(loc = case_when(
    id == "813" | id == '838' | id == "794" | id == "883" | id == "34" | id == "15" | id == "3" | id == "29" | id == "433" | id == "320"| id == "387" | id == "302" |
      id == "311" | id == "319" | id == "347" | id == "652" | id == "989" | id == "626" | id == "696" | id == "670" | id == "899" | id == "699" | id == "98" | id == "640" | 
      id == "655" | id == "700" | id == "41" | id == "63" | id == "905" | id == "105" | id == "698" | id == "697" | id == "604" | id == "620" | id == "78" | id == "36" ~ "IR",
    id == "71" | id == "893" | id == "891" | id == "76" | id == "80" | id == "6" | id == "96" | id == "38"| id == "94"| id == "623" | id == "639" | id == "687" |
      id == "574" | id == "695" | id == "636" | id == "674" | id == "660" | id == "683" | id == "624" | id == "672" | id == "632" ~ "HR",
    id == "914" | id == "945" | id == "898" | id == " 457" | id == "416" | id == "458" | id == "667"| id == "815" | id == "610" | id == "259" | id == "258" | id == "285" |
      id == "218" | id == "240" | id == "227" | id == "634" | id == "611" | id == "609" | id == "614" | id == "629" | id == "642" | id == "619" | id == "691" | id == "650"|
       id == "263" | id == "17" | id == "730" | id == "88" | id == "49" | id == "637" | id == "27" | id == "13" | id == "81" ~ "MC",
    .default = as.character(id)
  )) %>% 
  mutate(rep = case_when(
    id == "699" | id == " 98" | id == "640" | id == "655" | id == "700" | id == "41" | id == "88" | id == "49" | id == "637" ~ "1",
    id == "27" | id == "13" | id == "81" | id == "687" | id == "697" | id == "604" | id == "620" | id == "78" | id == "36" ~ "2",
    .default = NULL
  )) %>% 
  mutate(el = case_when(band == '1' & loc == 'MC' ~ '5400',
                        band == '2'& loc == 'MC' ~'6150',
                        band == '3' & loc == 'MC'~ '6320',
                        band == '4' & loc == 'MC'~ '6760', 
                        band == '5' & loc == 'MC'~ '7200',
                        band == '6' & loc == 'MC'~ '7900', 
                        band == '7' & loc == 'MC'~ '8200',
                        band == '8'& loc == 'MC' ~ '8370',
                        band == '9' & loc == 'MC'~ '8800',
                        band == '10'& loc == 'MC' ~ '9150', 
                        band == '1' & loc == 'IR' ~ '5500', 
                        band == '2' & loc == 'IR' ~ '5650', 
                        band == '3' & loc == 'IR' ~ '6300',
                        band == '4' & loc == 'IR' ~ '6650', 
                        band == '5' & loc == "IR" ~ '6950', 
                        band == '6' & loc == 'IR' ~ '7500', 
                        band == '7' & loc == 'IR' ~ '8120',
                        band == '8' & loc == 'IR' & rep == '1' ~ '8120',
                        band == '8' & loc == 'IR' & rep == '2' ~ '8300',
                        band == '9' & loc == 'IR' & rep == '1' ~ '8600',
                        band == '9' & loc == 'IR' & rep == '2' ~ '9000', 
                        band == '10' & loc == 'IR' ~ '9500',
                        band == '1' & loc == 'HR' ~ '5600',
                        band == '2' & loc == 'HR' ~ '5900',
                        band == '3' & loc == 'HR' ~ '6300',
                        band == '4' & loc == 'HR' ~ '6750', 
                        band == '5' & loc == 'HR' ~ '7100', 
                        band == '6' & loc == 'HR' ~ '7600',
                        band == '7' & loc == 'HR' ~ '8000')) %>%
  mutate(el = as.numeric(el)) %>% 
  mutate(el = el*0.3) %>% 
  mutate(qid = as.factor(id)) %>% 
  relocate(Date, loc, band, el, qid, rep) %>% 
  select(-7)

# df for the correct elevation
label_no_na <- labeled_df %>% 
  filter(!is.na(el))

# df for errors in elevation 
missing_loc <- labeled_df %>% 
  filter(is.na(el)) 

missing_loc %>% 
  distinct(Date, loc, band, qid)

# repairing the errors in elevation. Most of this is due to incorrect quadrat labels 
repaired_loc <- missing_loc %>% 
  mutate(loc = case_when(loc == '69' ~ "IR",
loc == '457' ~ "MC",
loc == '62' ~ "IR",
loc == '910' ~ "MC", 
loc == '668' ~ "IR",
loc == '654' ~ "IR",
.default = loc)) %>% 
  mutate(qid = case_when(loc == 'IR' & qid == '69' ~ '697',
loc == 'IR' & qid == '654' ~ '652',
loc == 'IR' & qid == '62' ~ '29',
loc == 'MC' & qid == '910' ~ '914',
loc == 'IR' & qid == '668' ~ '899',
.default = qid)) %>% 
  mutate(band = case_when(loc == 'IR' & qid == '697' ~ '8',
loc == 'MC' & qid == '457' ~ '2',
loc == 'IR' & qid == '652' ~ '6',
loc == 'IR' & qid == '29' ~ '3',
loc == 'MC' & qid == '914' ~ '1',
loc == 'IR' & qid == '899' ~ '7',
.default = band)) %>% 
  mutate(rep = case_when(loc == 'IR' & qid == '697' ~ '2',
loc == 'IR' & qid == '98' ~ '1',
loc == 'IR' & qid == '698' ~ '2',
.default = rep)) %>%
  mutate(el = case_when(band == '1' & loc == 'MC' ~ 5400,
                        band == '2'& loc == 'MC' ~6150,
                        band == '3' & loc == 'IR' ~ 6300,
                        band == '6' & loc == 'IR' ~ 7500, 
                        band == '7' & loc == 'IR' ~ 8120,
                        band == '8' & loc == 'IR' & rep == '1' ~ 8120,
                        band == '8' & loc == 'IR' & rep == '2' ~ 8300,
                      .default = el)) %>% 
  distinct(Date, loc, band, qid, rep,el)

# check for nas before saving object
full_join(repaired_loc, label_no_na) %>% 
  filter(is.na(el))

# repaired df
location_joined_df <- full_join(repaired_loc, label_no_na)


# splitting the mines
cleaned_df <- location_joined_df %>%
  mutate(d6_splitter = case_when(
    D6_c.p == "0" ~ paste0("0",",",D6_c.p),
    .default = D6_c.p
  )) %>% 
  separate_wider_delim(
    cols = d6_splitter, 
    delim = ",", 
    names = c("D6_ct", "D6_p"),
    too_few = "debug"
  ) %>% 
  mutate(d7_splitter = case_when(
    D7_c.p == "0" ~ paste0("0", ",", D7_c.p),
    .default = D7_c.p
  )) %>% 
  separate_wider_delim(
    d7_splitter, 
    delim = ",", 
    names = c("D7_ct", "D7_p"),
    too_few = "debug"
  ) %>% 
  relocate(Date, loc, band, el, qid, rep, Tag, D1_p, D2_p, D3_p, D6_p, D7_p) %>% 
  mutate_at(8:12, as.numeric)
  
# extract the max damage from each plant 
cleaned_df %>% # quick vis with the wide df, switching to long
  group_by(Tag, el, loc) %>% 
  summarise(D1_value = max(D1_p, na.rm = TRUE),
D2_value = max(D2_p, na.rm = TRUE),
D3_value = max(D3_p, na.rm = TRUE),
D6_value = max(D6_p, na.rm = TRUE),
D7_value = max(D7_p, na.rm = TRUE)) %>%
  ungroup() %>% 
  na.omit() %>% 
  ggplot(aes(x = el, y = D1_value)) +
  geom_point()

cleaned_df %>% # just looking at the spread
  group_by(el) %>% 
  summarise(max = max(el, na.rm = TRUE)) %>% 
  print(n = Inf)

max_value_df <- cleaned_df %>% # new df for the max values 
  group_by(Tag, el, loc, band) %>% 
  summarise(D1_value = max(D1_p, na.rm = TRUE),
D2_value = max(D2_p, na.rm = TRUE),
D3_value = max(D3_p, na.rm = TRUE),
D6_value = max(D6_p, na.rm = TRUE),
D7_value = max(D7_p, na.rm = TRUE)) %>%
  ungroup() %>% 
  na.omit() %>% 
  pivot_longer(
    c('D1_value', 'D2_value', 'D3_value', 'D6_value', 'D7_value'),
    names_to = 'dmg_type',
    values_to = 'max_p'
  ) %>% 
  mutate(Tag = case_when(
    Tag == " " ~ 'X', .default = Tag
  )) %>% 
  mutate(Tag = as.factor(Tag))  

# in the above math there is an issue somewhere that is causing some -Inf values. IDK why
max_value_df %>% 
  filter(max_p == -Inf)

e_df <- max_value_df %>% 
  group_by(band, el, dmg_type) %>% 
  filter(max_p != -Inf) %>% 
  summarise(mean = mean(max_p, na.rm = TRUE)) 

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

# Mortality cleaning ####

cleaned_df %>% 
  arrange(Date, loc, band, el) %>% 
  print(n = 100)
















df_test <- tibble(
  plant_id = c(1, 1, 1, 1, 2, 3, 3, 3, 3),
  week     = c(1, 2, 3, 4, 1, 1, 2, 3, 4),
  status   = c(1, 1, 1, 1, 0, 1, 1, 1, 1)
) %>% 
  print(n = Inf)

df_test %>%
  complete(plant_id, week = full_seq(week, 1)) %>%
  arrange(plant_id, week) %>%
  group_by(plant_id) %>%
  fill(status, .direction = "down") %>%
  ungroup()

df_complete



  
#   select(1:8) %>% 
#   mutate(binary = case_when(D1_p == "dead" ~ "0",
# .default = "1",
# is.na(D1_p) ~ "X"
# )) %>% 
#   filter(binary != "X") %>% 
#   print(n = 100)

# fill in the missing rows so we have 15 plants per row still
  # eg proportion alive at each date 
  # can also plot this by phenological stage 
  # veg (1) flower (2) fruit (3) dead (0)