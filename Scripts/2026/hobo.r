# packages ####
library(tidyverse)
library(readr)

# data ####

# step 1: change 1:1 to "q" on all .csv files
# step 2: merge

path <- list.files(path = "Data/2026/1csv", pattern = "\\.csv$", full.names = TRUE)

tester <- map(path, read_csv) %>% 
  bind_rows()



test <-  path %>% 
  map(~ read_csv(.x) %>% 
  rename(Q = 1)) %>% 
  reduce(full_join, by = "Q") %>% 
  print(n = 10)

cols <- test %>% 
  select(1:4) %>% 
  rename(qtag = Q,
  number = "#.x",
  temp_f = "Temp, °F (LGR S/N: 10567939, SEN S/N: 10567939, LBL: 1B3G)"
  ) %>% 
  separate_wider_delim(
    cols = 'Date Time, GMT-06:00.x',
    delim = " ",
    names = c("date", "time")
  )

# step to test how to clean the ids and assign labels ####

cols %>% 
  distinct(qtag, .keep_all = TRUE) %>% # identify and extract unique values
  print(n = Inf) %>% 
  separate_wider_delim(cols = 'qtag', # split the row values based on a deliminator 
  delim = ":",
  names = c("misc", "ID"),
  too_few = "align_end") %>% # when deliminator not there, but in ID column 
  select(-1) %>% 
  mutate(
    qid = case_when(ID == " 1B3G" ~ "q898",
                  ID == " 1S2C" ~ "q623",
                  ID == " 2S1C" ~ "696",
                  ID == " 3S2G" ~ "q674",
                  ID == " 3S3G" ~ "q698",
                  ID == " 5B1C" ~ "q655",
                  ID == " 5B1G" ~ "q320",
                  ID == " 5B2G" ~ "q96",
                  ID == " 5B2C" ~ "q620",
                  ID == " 5C1C" ~ "q634", 
                  ID == " 5C2C" ~ "q71",
                  ID == " 5C2G" ~ "q36",
                  ID == " 5C3C" ~ "q3",
                  ID == " 5C3G" ~ "q311",
                  ID == " 5S1C" ~ "q259",
                  ID == " 5S2G" ~ "q76", 
                  ID == " 5S3C" ~ "q574",
                  ID == " 5S4C" ~ "q63",
                  ID == " 5S4G" ~ "q624",
                  ID == " T03" ~ "q672",
                  ID == " T04" ~ "q652",
                  ID == " WL006" ~ "q698",
                  ID == " WL011" ~ "q88",
                  , .default = ID)
  ) %>% 
  print(n = Inf)

# applying test step to whole df ####

pentDf <- cols %>% 
  separate_wider_delim(cols = 'qtag',
  delim = ":",
  names = c("misc", "ID"),
  too_few = "align_end") %>% 
  select(-1) %>% 
  mutate(
    qid = case_when(ID == " 1B3G" ~ "q898",
                  ID == " 1S2C" ~ "q623",
                  ID == " 2S1C" ~ "696",
                  ID == " 3S2G" ~ "q674",
                  ID == " 3S3G" ~ "q698",
                  ID == " 5B1C" ~ "q655",
                  ID == " 5B1G" ~ "q320",
                  ID == " 5B2G" ~ "q96",
                  ID == " 5B2C" ~ "q620",
                  ID == " 5C1C" ~ "q634", 
                  ID == " 5C2C" ~ "q71",
                  ID == " 5C2G" ~ "q36",
                  ID == " 5C3C" ~ "q3",
                  ID == " 5C3G" ~ "q311",
                  ID == " 5S1C" ~ "q259",
                  ID == " 5S2G" ~ "q76", 
                  ID == " 5S3C" ~ "q574",
                  ID == " 5S4C" ~ "q63",
                  ID == " 5S4G" ~ "q624",
                  ID == " T03" ~ "q672",
                  ID == " T04" ~ "q652",
                  ID == " WL006" ~ "q698",
                  ID == " WL011" ~ "q88",
                  , .default = ID)) %>% 
  relocate(qid) %>% 
  select(-2) %>% 
  print(n = 10)

# Assign an elevation to each quadrat id ####

unique(pentDf$qid)

clean <- pentDf %>% 
  mutate(elevation = case_when(
    qid == "q898" ~ '5403',
    qid == "q623" ~ '6741',
    qid == "696" ~ '7781',
    qid == "q674" ~ '7623',
    qid == "q698" ~ '8124',
    qid == "q655" ~ '8296',
    qid == "q320" ~ '6628',
    qid == "q620" ~ '8605', 
    qid == "q96" ~ '6298',
    qid == "q634" ~ '7907',
    qid == "q71" ~ '5555',
    qid == "q36" ~ '7112',
    qid == "q3" ~ '6241',
    qid == "q311" ~ '6953',
    qid == "q259" ~ '6773',
    qid == "q76" ~ '5890',
    qid == "q574" ~ '7112',
    qid == "q63" ~ '9508',
    qid == "q624" ~ '7984',
    qid == "q218" ~ '7228',
    qid == "q667" ~ '6296',
    qid == "q672" ~ '8041', 
    qid == "q652" ~ '7446',
    qid == "q88" ~ '9131')
  ) %>% 
  mutate(elevation = as.numeric(elevation)) %>% 
  mutate(temp = (temp_f-32)*(5/9)) %>% 
  mutate(elevation = elevation*0.3048) %>% 
  relocate(qid, elevation) %>% 
  select(-6) %>% 
  print(n = 10)


# plotting function ####

# first plot 
clean %>% 
  subset(qid == 'q898') %>%
  ggplot(aes(x = date, y = temp_f))+
  geom_point() 

ggplot(clean, aes(x = date, y = temp))+
  geom_point()+
  facet_wrap(~qid, scales = 'fixed')


