# broken code from the mortality cleaning 


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


