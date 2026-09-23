# packages ####
library(tidyverse)
library(readr)

# data ####

# step 1: change 1:1 to "q" on all .csv files
# step 2: merge

path <- list.files(path = "Data/2026/1csv", pattern = "*.csv", full.names = TRUE)

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

test <- cols %>% 
  slice(1:10000)

unique(test$qtag)

cols %>% 
  distinct(qtag, .keep_all = TRUE) %>% 
  print(n = Inf) %>% 
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
                  , .default = ID)
  ) %>% 
  print(n = Inf)


