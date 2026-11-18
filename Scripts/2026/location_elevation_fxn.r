# Function to add location, band, elevation to quadrats 
# NOT done, just copied in a bunch of code to recycle 
elevation_band_fxn <- function(x){
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
  mutate(el = el*0.3) 
  
  
  
  
  
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
.default = rep)) %>% # 11/18/26: Adding elevation at the quadrat level below
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

###
##
#
# 11/1/2026
# FIX EL X Q

location_joined_df %>% 
  distinct(loc, band, qid, rep) %>% 
  write.csv("distinct_qid_loc.csv", row.names = FALSE)

# Reference df with elevation at the quadrat level
q_el
q_el_join <- q_el %>% 
  rename(el = elevation,
    qid = qnum) %>% 
  mutate(qid = as.character(qid))

appended_el <- location_joined_df %>% 
  left_join(
    q_el_join %>% 
      select(el, qid),
    by = "qid"
  ) %>% 
  relocate(Date, loc, band, el.x, el.y) %>% 
  rename(el_band = el.x,
    el_quadrat = el.y) %>% 
  mutate(el_quadrat = round(el_quadrat * 0.3))











}



