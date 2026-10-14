# packages ####
library(tidyverse)
library(ggridges)

# DFs ####
den_df
clean_1 # hobo


# joining ####
den_to_merge <- den_df %>% 
  mutate(q_t = paste0("q", q_t)) %>% 
  rename(qid = q_t) %>% 
  relocate(qid, den_mean) %>% 
  select(1:2) %>% 
  print(n = 10)


# test

test_hobo <- clean1 %>% 
  slice_sample(n = 1000) %>% 
  print(n = 10)
unique(test_hobo$qid)

test_hobo %>% 
  left_join(den_to_merge, by = 'qid', relationship = "many-to-many") %>% 
  ggplot(aes(x = elevation, y = temp))+
  geom_point(aes(color = den_mean))+
  scale_color_viridis_c()

# for real now

merged_df <- clean1 %>% 
  left_join(den_to_merge, by = 'qid', relationship = "many-to-many") 

merged_df %>% 
  ggplot(aes(x = elevation, y = temp)) +
  geom_point(aes(color = den_mean))+
  scale_color_viridis_b()

merged_df %>% 
  ggplot(aes(x = den_mean, y = temp))+
  geom_point(aes(color = elevation)) +
  scale_color_viridis_c()+
  theme_bw()+
  labs(title = "Soil temp x Average densiometer",
x = "Average densiometer",
y = "Soil temperature (C)")

merged_df %>% 
  ggplot(aes(x = elevation, y = den_mean)) +
  geom_point()+
  geom_smooth(method = 'gam',
formula = y~s(x, k=4))

merged_df %>%
  group_by(qid, elevation,date) %>% 
  summarise(max = max(temp, na.rm = TRUE),
min = min(temp, na.rm = TRUE),
median = median(temp, na.rm = TRUE)) %>% 
  pivot_longer(cols = -c(qid, elevation,date),
names_to = "stat", 
values_to = "value") %>%
  ggplot(aes(x = elevation, y = value))+
  geom_point()+
  geom_smooth(method = 'gam',
formula = y~s(x, k=4))+
  facet_wrap(~stat)+
  labs(title = "Max, Min, Med soil temperature x elevation",
x = "Elevation (m)",
y = "Soil temperature (C)")

merged_df %>% 
  group_by(qid, elevation) %>% 
  summarise(mean = mean(temp)) %>% 
  ggplot(aes(x = elevation, y = mean))+
  geom_point()+
  geom_smooth(method = 'gam',
formula = y~s(x, k=4))+
  labs(
    title = "Average soil temperature x elevation",
    x = "Elevation (m)",
    y = "Average soil temperature (C)"
  )
