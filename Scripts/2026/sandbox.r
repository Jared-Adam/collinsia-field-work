# Load packages ####
library(tidyverse)

# Will example ####
# Set up data ###
# Data parameters
nquad = 5 # number of quadrats
nplant = 5 # number of plants per quadrat
ntime = 3 # number of time points

# Set up data frame
d = data.frame(
  quadID = rep(1:nquad, each = nplant * ntime),
  plantID = rep(rep(1:nplant, times=ntime), times=nquad),
  time = rep(rep(1:ntime, each=nplant), times = nquad)
)


# Remove some rows, simulating dead or missing plants ###
dmissing = d |>
  # Drop plant 1 in quad 1 at time 3
  filter(!(quadID == 1 & plantID == 1 & time == 3)) |>
  # Drop plant 5 in quad 13 at times 2 and 3
  filter(!(quadID == 2 & plantID == 5 & time >1))


# Add a height col, which will be NA for missing plants ###
dmissing$height = runif(nrow(dmissing), 0, 1)


# Do the expansion ###
# I'm using nesting(quadID, plantID),
# but it would also work with group_by(quadID)
dfull = dmissing |>
  expand(nesting(quadID, plantID), time) |>
  left_join(dmissing, by = c('quadID', 'plantID', 'time'))

# Can use complete if you don't want to join manually after
dfull2 = dmissing |>
  complete(nesting(quadID, plantID), time)


# ME ####
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

# Does not change anything 
tag_revised_df %>% 
  expand(nesting(qid, Tag, Date, loc)) %>% 
  left_join(tag_revised_df, by = c('qid', 'Tag', 'Date', 'loc'))




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