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
  mutate(el = as.numeric(el)) %>% 
  mutate(el = el*0.3) 

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

# See note on 11/4 for rationale
  # Repairing tags that are broken one location at a time 

step1_tagRepair <- cleaned_df %>% 
  mutate(Date = case_when(Date == "4/20/26" & qid == 71 ~ "4/10/26", 
.default = Date)) %>% 
  mutate(Date = str_replace(Date, "/26$", "/2026")) %>% 
  # filter(qid == 71) %>% 
  # arrange(Date) %>% 
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>% 
  arrange(Date) %>% 
  select(1:7)


# ###
##
#
#. IR

# Isolate the unique tags based on qid
IR_tagRepair <- step1_tagRepair %>% 
  filter(loc == "IR") 

IR_List <- unique(IR_tagRepair$qid)


# PLAN: 

IR_tagRepair %>% 
  filter(qid %in% IR_List) %>% 
  group_by(qid) %>% 
  summarise(tags = list(unique(Tag))) %>% 
  filter(lengths(tags) > 15) %>% 
  unnest(tags) %>%
  #filter(qid == "105") %>% 
  arrange(qid) %>% 
  write.csv("IG_broken_tags.csv", row.names = FALSE)

# REPAIRS
IR_tagRepair %>% 
  mutate(Tag = case_when(loc == "IR" & qid == "105" & Tag == "71" ~ "79",# IR
    loc == "IR" & qid == "105" & Tag == "689" ~ "698",
    loc == "IR" & qid == "29" & Tag == "51" ~ "15",
    loc == "IR" & qid == "3" & Tag == "35" ~ "37",
    loc == "IR" & qid == "302" & Tag == "273" ~ "278",
    loc == "IR" & qid == "320" & Tag == "284" ~ "384",
    loc == "IR" & qid == "36" & Tag == "807" ~ "897",
    loc == "IR" & qid == "36" & Tag == "68" ~ "89",
    loc == "IR" & qid == "433" & Tag == "29" ~ "62",
    loc == "IR" & qid == "433" & Tag == "253" ~ "252",
    loc == "IR" & qid == "433" & Tag == " " ~ "658",
    loc == "IR" & qid == "620" & Tag == "892" ~ "268",
    loc == "IR" & qid == "626" & Tag == "1" ~ "9",
    loc == "IR" & qid == "652" & Tag == "19" ~ "49",
    loc == "IR" & qid == "838" & Tag == "270" ~ "278",
    loc == "IR" & qid == "899" & Tag == "14" ~ "24",
    loc == "IR" & qid == "98" & Tag == "61" ~ "6",
    loc == "IR" & qid == "989" & Tag == "71" ~ "7",
    .default = Tag)) %>% 
  filter(Tag != "na") %>% # for  IR quadrat 78
  filter(qid %in% IR_List) %>% 
  group_by(qid) %>% 
  summarise(tags = list(unique(Tag))) %>% 
  filter(lengths(tags) > 15) 








stop
###
##
#
#. HR

HR_tagRepair <- step1_tagRepair %>% 
  filter(loc == "HR")

HR_List <- unique(HR_tagRepair$qid)


HR_tagRepair %>% 
  mutate(Tag = case_when(loc == "HR" & qid == "6" & Tag == "6" ~ "9",  # HR
    loc == "HR" & qid == "6" & Tag == "154" ~ "159",
    loc == "HR" & qid == "639" & Tag == "565" ~ "556",
    loc == "HR" & qid == "660" & Tag == "17" ~ "7",
    loc == "HR" & qid == "660" & Tag == "24" ~ "27",
    # loc == "HR" & qid == "660" & Date == "2026-6-24" & Tag == "41" ~ "11",
    loc == "HR" & qid == "672" & Tag == "79" ~ "29",
    loc == "HR" & qid == "672" & Tag == "33" ~ "23",
    loc == "HR" & qid == "672" & Tag == "30" ~ "20",
    loc == "HR" & qid == "683" & Tag == "29" ~ "24",
    loc == "HR" & qid == "687" & Tag == "594" ~ "595",
    loc == "HR" & qid == "71" & Tag == "809" ~ "869",
    loc == "HR" & qid == "80" & Tag == "120" ~ "126",
    loc == "HR" & qid == "80" & Tag == "104" ~ "164",
    loc == "HR" & qid == "80" & Tag == "765" ~ "763",
    loc == "HR" & qid == "891" & Tag == "777" ~ "977",
    loc == "HR" & qid == "94" & Tag == "107" ~ "167",
    loc == "HR" & qid == "96" & Tag == "108" ~ "168",
    .default = Tag)) %>% 
  filter(Tag != "574") %>% # 623 resolution
  filter(Tag != "999") %>% # 660 resolution
  mutate(filter_c = case_when(qid == "624" & Date == "2026-06-23" ~ "take", # fixing 624 issue
    .default = "leave")) %>% 
  filter(filter_c == "leave") %>% 
  select(-8) %>% 
  filter(qid %in% HR_List) %>% 
  group_by(qid) %>% 
  summarise(tags = list(unique(Tag))) %>% 
  filter(lengths(tags) > 15) 
#   unnest(tags) %>%
# #   filter(qid == "105") %>% 
#   arrange(qid) %>% 
#   write.csv("HR_broken_tags.csv", row.names = FALSE)

stop
###
##
#
#.  MC

# extract 624 bc some are incorrect qid


MC_broke_629 <- HR_tagRepair %>% 
  filter(qid == "624") %>% 
  mutate(filter_c = case_when(Date == "2026-06-23" ~ "take")) %>% 
  filter(filter_c == "take") %>% 
  mutate(qid = "629",
    loc = "MC") %>% 
  select(-8) %>% 
  print()

MC_tagRepair <- step1_tagRepair %>% 
  filter(loc == "MC")

MC_List <- unique(MC_tagRepair$qid)

MC_tagRep_bind <- 
  rbind(MC_tagRepair, MC_broke_629)

MC_tagRep_bind %>%  
  mutate(Tag = case_when(loc == "MC" & qid == "13" & Tag == "220" ~ "230",
    loc == "MC" & qid == "13" & Tag == "477" ~ "877",
    loc == "MC" & qid == "17" & Tag == "692" ~ "290",
    loc == "MC" & qid == "17" & Tag == "256" ~ "656",
    loc == "MC" & qid == "218" & Tag == "210" ~ "201",
    loc == "MC" & qid == "218" & Tag == "102" ~ "201",
    loc == "MC" & qid == "258" & Tag == "258" ~ "251",
    loc == "MC" & qid == "259" & Tag == "226" ~ "266",
    loc == "MC" & qid == "259" & Tag == "203" ~ "293",
    loc == "MC" & qid == "263" & Tag == "353" ~ "853",
    loc == "MC" & qid == "285" & Tag == "557" ~ "257",
    loc == "MC" & qid == "285" & Tag == "279" ~ "271",
    loc == "MC" & qid == "267" & Tag == "476" ~ "953",
    loc == "MC" & qid == "458" & Tag == "667" ~ "67",
    loc == "MC" & qid == "458" & Tag == "460" ~ "400",
    loc == "MC" & qid == "610" & Tag == "707" ~ "767",
    loc == "MC" & qid == "611" & Tag == "638" ~ "689",
    loc == "MC" & qid == "629" & Tag == "851" ~ "651",
    loc == "MC" & qid == "629" & Tag == "299" ~ "662",
    loc == "MC" & qid == "634" & Tag == "662" ~ "602",
    loc == "MC" & qid == "730" & Tag == "356" ~ "357",
    loc == "MC" & qid == "81" & Tag == "43" ~ "143",
    loc == "MC" & qid == "81" & Tag == "106" ~ "166",
    loc == "MC" & qid == "81" & Tag == "112" ~ "211",
    loc == "MC" & qid == "81" & Tag == "233" ~ "235",
    loc == "MC" & qid == "416" & Tag == "184" ~ "484",
    .default = Tag)) %>% 
  filter(!(Date == "2026-07-17" & Tag == "55")) %>% 
  filter(!(Date == "2026-06-30" & Tag == "247")) %>% 
  mutate(qid = case_when(
    qid == "457" & Tag == "186" ~ "416",
    qid == "457" & Tag == "416" ~ "416",
    qid == "457" & Tag == "451" ~ "416",
    qid == "457" & Tag == "455" ~ "416",
    qid == "457" & Tag == "463" ~ "416",
    qid == "457" & Tag == "469" ~ "416",
    qid == "457" & Tag == "471" ~ "416",
    qid == "457" & Tag == "478" ~ "416",
    qid == "457" & Tag == "484" ~ "416",
    qid == "457" & Tag == "600" ~ "416",
    qid == "457" & Tag == "634" ~ "416",
    qid == "457" & Tag == "661" ~ "416",
    .default = qid
  )) %>% 
  filter(qid %in% MC_List) %>% 
  group_by(qid) %>% 
  summarise(tags = list(unique(Tag))) %>% 
  filter(lengths(tags) > 15) 
#   unnest(tags) %>%
# #  filter(qid == "105") %>% 
#   arrange(qid) %>% 
# write.csv("MC_broken_tags.csv", row.names = FALSE)

# attempting the changes at the whole-df level now .

cleaned_df  %>% 
  mutate(Tag = case_when(loc == "IR" & qid == "105" & Tag == "71" ~ "79",# IR
    loc == "IR" & qid == "105" & Tag == "689" ~ "698",
    loc == "IR" & qid == "29" & Tag == "51" ~ "15",
    loc == "IR" & qid == "3" & Tag == "35" ~ "37",
    loc == "IR" & qid == "302" & Tag == "273" ~ "278",
    loc == "IR" & qid == "320" & Tag == "284" ~ "384",
    loc == "IR" & qid == "36" & Tag == "807" ~ "897",
    loc == "IR" & qid == "36" & Tag == "68" ~ "89",
    loc == "IR" & qid == "433" & Tag == "29" ~ "62",
    loc == "IR" & qid == "433" & Tag == "253" ~ "252",
    loc == "IR" & qid == "433" & Tag == " " ~ "658",
    loc == "IR" & qid == "620" & Tag == "892" ~ "268",
    loc == "IR" & qid == "626" & Tag == "1" ~ "9",
    loc == "IR" & qid == "652" & Tag == "19" ~ "49",
    loc == "IR" & qid == "838" & Tag == "270" ~ "278",
    loc == "IR" & qid == "899" & Tag == "14" ~ "24",
    loc == "IR" & qid == "98" & Tag == "61" ~ "6",
    loc == "IR" & qid == "989" & Tag == "71" ~ "7",
    .default = Tag)) %>% 
  filter(Tag != "na") %>% 
  mutate(Tag = case_when(loc == "HR" & qid == "6" & Tag == "6" ~ "9",  # HR
    loc == "HR" & qid == "6" & Tag == "154" ~ "159",
    loc == "HR" & qid == "639" & Tag == "565" ~ "556",
    loc == "HR" & qid == "660" & Tag == "17" ~ "7",
    loc == "HR" & qid == "660" & Tag == "24" ~ "27",
    loc == "HR" & qid == "660" & Date == "2026-6-24" & PH == '23' & Tag == "41" ~ "11",
    loc == "HR" & qid == "672" & Tag == "79" ~ "29",
    loc == "HR" & qid == "672" & Tag == "33" ~ "23",
    loc == "HR" & qid == "672" & Tag == "30" ~ "20",
    loc == "HR" & qid == "683" & Tag == "29" ~ "24",
    loc == "HR" & qid == "687" & Tag == "594" ~ "595",
    loc == "HR" & qid == "71" & Tag == "809" ~ "869",
    loc == "HR" & qid == "80" & Tag == "120" ~ "126",
    loc == "HR" & qid == "80" & Tag == "104" ~ "164",
    loc == "HR" & qid == "80" & Tag == "765" ~ "763",
    loc == "HR" & qid == "891" & Tag == "777" ~ "977",
    loc == "HR" & qid == "94" & Tag == "107" ~ "167",
    loc == "HR" & qid == "96" & Tag == "108" ~ "168",
    .default = Tag)) %>% 
  filter(Tag != "574") %>% # 623 resolution
  filter(Tag != "999") %>% # 660 resolution
  mutate(qid = case_when(qid == "624" & Date == "2026-06-23" ~ "629", # fixing 624 issue
    .default = qid)) %>% 
  mutate(loc = case_when(qid == '629' & Date == "2026-06-23" ~ "MC",
    .default = loc)) %>% # fixing 624 issue
mutate(Tag = case_when(loc == "MC" & qid == "13" & Tag == "220" ~ "230", # MC 
    loc == "MC" & qid == "13" & Tag == "477" ~ "877",
    loc == "MC" & qid == "17" & Tag == "692" ~ "290",
    loc == "MC" & qid == "17" & Tag == "256" ~ "656",
    loc == "MC" & qid == "218" & Tag == "210" ~ "201",
    loc == "MC" & qid == "218" & Tag == "102" ~ "201",
    loc == "MC" & qid == "258" & Tag == "258" ~ "251",
    loc == "MC" & qid == "259" & Tag == "226" ~ "266",
    loc == "MC" & qid == "259" & Tag == "203" ~ "293",
    loc == "MC" & qid == "263" & Tag == "353" ~ "853",
    loc == "MC" & qid == "285" & Tag == "557" ~ "257",
    loc == "MC" & qid == "285" & Tag == "279" ~ "271",
    loc == "MC" & qid == "267" & Tag == "476" ~ "953",
    loc == "MC" & qid == "458" & Tag == "667" ~ "67",
    loc == "MC" & qid == "458" & Tag == "460" ~ "400",
    loc == "MC" & qid == "610" & Tag == "707" ~ "767",
    loc == "MC" & qid == "611" & Tag == "638" ~ "689",
    loc == "MC" & qid == "629" & Tag == "851" ~ "651",
    loc == "MC" & qid == "629" & Tag == "299" ~ "662",
    loc == "MC" & qid == "634" & Tag == "662" ~ "602",
    loc == "MC" & qid == "730" & Tag == "356" ~ "357",
    loc == "MC" & qid == "81" & Tag == "43" ~ "143",
    loc == "MC" & qid == "81" & Tag == "106" ~ "166",
    loc == "MC" & qid == "81" & Tag == "112" ~ "211",
    loc == "MC" & qid == "81" & Tag == "233" ~ "235",
    loc == "MC" & qid == "416" & Tag == "184" ~ "484",
    .default = Tag)) %>% 
  filter(!(Date == "2026-07-17" & Tag == "55")) %>% 
  filter(!(Date == "2026-06-30" & Tag == "247")) %>% 
  mutate(qid = case_when(
    qid == "457" & Tag == "186" ~ "416",
    qid == "457" & Tag == "416" ~ "416",
    qid == "457" & Tag == "451" ~ "416",
    qid == "457" & Tag == "455" ~ "416",
    qid == "457" & Tag == "463" ~ "416",
    qid == "457" & Tag == "469" ~ "416",
    qid == "457" & Tag == "471" ~ "416",
    qid == "457" & Tag == "478" ~ "416",
    qid == "457" & Tag == "484" ~ "416",
    qid == "457" & Tag == "600" ~ "416",
    qid == "457" & Tag == "634" ~ "416",
    qid == "457" & Tag == "661" ~ "416",
    .default = qid
  )) %>% 
  filter(!(loc == "MC" & Date == "2026-4-11" & qid == "457" & Tag == "467" & PH == "8")) %>% 
    filter(!(loc == "MC" & qid == "13" & Tag == "798" & PH == "62"))












stop


# 11/4/2026: All of this code was in an effort to coalece the missing tag numbers in 
  # However, there are potentially greater issues with the tag ids than realized
  # For example: Mislabled tags are persistent 
  # In the ABOVE code: 
    # I am starting fresh with first looking to correct any incorrect tags
      # Methodology, unclear 

# Through some cleaning, I discovered some date issues
  # I cannot identify where the error occured, but I know the issue
  # HR q 71 has 5 tags with a 4/20 date. This should be 4/10
  # Will repair that 
as_date_df <- cleaned_df %>% 
  mutate(Date = case_when(Date == "4/20/26" & qid == 71 ~ "4/10/26", 
.default = Date)) %>% 
  mutate(Date = str_replace(Date, "/26$", "/2026")) %>% 
  # filter(qid == 71) %>% 
  # arrange(Date) %>% 
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>% 
  arrange(Date) %>% 
  group_by(loc, qid, band) %>% 
  mutate(timing = dense_rank(Date)) %>% 
  relocate(timing) 

as_date_df

# filled df of all locs and such 
filled_df <- as_date_df %>% 
  ungroup() %>% 
  arrange(loc, band) %>% 
  select(1:9) %>% 
  mutate(row_id = row_number(), .by = c(Date, qid)) %>% 
  relocate(row_id) %>% 
  group_by(Date, qid, timing) %>% 
  complete(row_id = 1:15) %>% 
  ungroup() %>%  
  group_by(Date, qid) %>% 
  fill(loc, band, el, rep,timing, .direction = 'down') %>% 
  ungroup() 

write.csv(filled_df, "filled_df_ref.csv", row.names = FALSE)


# master df with the first timing of each location and band
  # these should be the most accurate tag ids
  # complete the whole df based on these values 
t1_master_df <- filled_df %>% 
  group_by(qid) %>% 
  filter(timing == min(timing)) 

  
write.csv(t1_master_df, "master_list_df.csv", row.names = FALSE)

# master list test
HR_master_test <- t1_master_df %>% 
  filter(loc == "HR")
HR_filled_test <- filled_df %>% 
  filter(loc == "HR")

# HR_filled_test %>% 
#   mutate(Tag = coalesce())

look <- HR_filled_test %>% 
  complete(Tag = HR_master_test$Tag) %>% 
  arrange(qid, timing, band, row_id)
# %>%  
#   count(row_id)

target <- unique(look$qid)

look %>% 
  filter(qid %in% target) %>%
  group_by(qid) %>%
  summarise(tags = list(unique(Tag))) %>% 
  filter(lengths(tags) > 15) %>% 
  unnest(tags) %>%
  filter(qid == "624") %>% 
  arrange(tags) %>% 
  print(n = Inf)



stop





# want create a master df with the correct qid and tag id so I can ref in code later for repairs

# inspect_df <- cleaned_df %>% 
#   mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>% 
#   group_by(band, qid, Tag) %>% 
#   filter(Date == min(Date)) %>% 
#   slice(1) %>% 
#   ungroup() %>% 
#   arrange(loc, band) %>% 
#   select(1:8) %>% 
#   mutate(row_id = row_number(), .by = c(Date, qid)) %>% 
#   relocate(row_id) %>% 
#   group_by(Date, qid) %>% 
#   complete(row_id = 1:15) %>% 
#   ungroup() 


# write.csv(inspect_df, "master_list_df.csv", row.names = FALSE)

# inspect_df %>% 
#   filter(band == 1) %>%  
#   arrange(qid) %>% 
#   print(n = Inf)

# cleaned_df %>% 
#   filter(qid == 71 | qid == 76) %>% 
#   arrange(qid) %>% 
#   print(n = Inf)

# cleaned_df %>% 
#   filter(qid == 71) %>% 
#   filter(Date == "4/10/26" | Date == "4/20/26") %>% 
#   print(n = Inf)








# 10/23/2026: 
  # This is not done 
  # my og code does create for complete and fill.
  # TODO: create a df with mc and 416
    # Then, attempt the valid qids and plant map ideas with the if statement. 

mc_test <- cleaned_df %>% 
  arrange(Date, loc, band, el) %>% 
  filter(loc == "MC" & qid == '416') 

# expand trial 10/26: did not work 

# mc_test %>% 
#   group_by(Date, loc, band, el, qid, rep) %>% 
#   expand(Tag) %>%
#   ungroup() %>% 
#   mutate(row_id = row_number(), .by = c(Date, qid)) %>% 
#   relocate(row_id) %>% 
#   group_by(Date, qid) %>% 
#   complete(row_id = 1:15) %>% 
#   ungroup() %>%  
#   print(n = Inf)


# complete and fill 10/26: Worked 
one <- mc_test %>% 
  mutate(row_id = row_number(), .by = c(Date, qid)) %>% 
  relocate(row_id) %>% 
  group_by(Date, qid) %>% 
  complete(row_id = 1:15) %>% 
  ungroup() 

# on 6/30, 184 exists, but I think it should be 484. Going to change that
two <- one %>%  
  group_by(Date, qid) %>% 
  fill(loc, band, el, rep, .direction = 'down') %>% 
  ungroup()
two <- two %>% 
  mutate(Tag = case_when(Date == "6/30/26" & Tag == '184' ~ "484",
.default = Tag)) 
two %>% 
  print(n = Inf)

# completing all rows
test_on_two <- two %>% 
  select(-row_id) %>% 
  #select(1:7) %>% 
  # group_by(Date,qid, el, band, rep, loc) %>% 
  complete(nesting(Date, loc, band, rep, el, qid),Tag) %>% 
  distinct() %>% 
  print(n = Inf)

# completing a binary 
two %>% 
  select(-row_id) %>% 
  select(1:8) %>% 
  mutate(binary = case_when(D1_p == "dead" ~ "0",
.default = "1",
is.na(D1_p) ~ NA
)) %>%
  select(-D1_p) %>% 
  complete(nesting(Date, loc, band, rep, el, qid),Tag) %>% 
  distinct() %>% 
  print(n = Inf)
  
# all of mica creek at once
# need to locate which stage makes repair easiest
# I might have to do this one loc and band at a time 

mc_band_one <- cleaned_df %>% 
  filter(loc == 'MC'& band == '1')

mc_b1_row <- mc_band_one %>% 
  mutate(row_id = row_number(), .by = c(Date, qid)) %>% 
  relocate(row_id) %>% 
  group_by(Date, qid) %>% 
  complete(row_id = 1:15) %>% 
  print(n = Inf)

mc_b1_down <- mc_b1_row%>% 
  fill(loc, band, el, rep, .direction = 'down') %>% 
  ungroup() %>%
  print(n = Inf)

  
mc_b1_down %>% 
  select(-row_id) %>% 
  group_by(Date,qid) %>%
  distinct(Tag) %>% 
  print(n = Inf)
  

  


  # complete(nesting(Date, loc, band, rep, el, qid), Tag) %>% 
  # distinct() %>% 
  # print(n = Inf)












hold


















# 10/26/2026: Nothing below is in the final, keeping for now 
# NO
two %>% 
  mutate(Tag = as.numeric(Tag)) %>% 
  # complete(Tag) %>% 
  group_by(Date, qid) %>% 
  mutate(has_match = any(!is.na(Tag))) %>% 
  fill(Tag, .direction = "downup") %>% 
  mutate(Tag = if_else(has_match, Tag, NA_real_)) %>% 
  ungroup() %>% 
  relocate(Date, qid, row_id, loc, band, el, rep, Tag) %>% 
  arrange(Date, row_id) %>% 
  print(n = Inf)

valid_qids <- mc_test %>%
  #slice(1:126) %>% 
  group_by(loc, qid) %>%
  summarise(max_plants = n_distinct(Tag), .groups = "drop") %>%
  filter(max_plants >= 15) %>%
  pull(qid)

# Step 2: Create your master plant map for mapping row_id to plant_id
plant_map <- mc_test %>% 
  mutate(row_id = row_number(), .by = c(Date, qid)) %>% 
  relocate(row_id) %>% 
  group_by(Date, qid) %>% 
  complete(row_id = 1:15) %>% 
  ungroup() %>% 
  distinct(row_id, Tag)


# broken ... need to revisit
mc_test %>% 
  group_by(loc, qid) %>% 
  group_modify(function(sub_df, key){
    # Check if this group's qid belongs to the 15-plant set
    if (unique(key$qid) %in% valid_qids) {
      # For 15-plant groups, safely complete using your plant template map
      complete(sub_df, plant_map)
    } else {
      # For smaller groups, skip completion entirely to retain natural NAs
      sub_df 
    }
  }) %>% 
  fill(loc, band, el, rep,  .direction = "down") %>% 
  ungroup() %>% 
  print( n = Inf)

d













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