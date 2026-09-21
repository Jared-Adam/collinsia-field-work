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
  slice(1:1000)

