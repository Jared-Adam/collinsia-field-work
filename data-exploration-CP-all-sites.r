# Packages ####
library(tidyverse)
library()

# data ####
mica_creek_df <- X2025_7_15_MC_field_data_online_entry

# wrangling ####
colnames(mica_creek_df)
mica_creek_df <- mica_creek_df %>% 
  rename(suck_mite_p = 'suck-mite_p',
         suck_mite_n = 'suck-mite_n3',
         suck_thrips_p = 'suck-thrips_p',
         suck_thrips_n = 'suck-thrips_n')

# test to see the best way to split the node columns, BUT this does not work if the values match 
mc_test_df <- mica_creek_df[1:100,]
mc_test_df %>% 
  separate_rows(c(Dn1, Dn2, suck_mite_n, suck_thrips_n, Dn4, Dn5, Dn6), sep = ",")

# adding this to a new df 
mica_creek_df %>% 
  separate_rows(c(Dn1, Dn2, suck_mite_n, suck_thrips_n, Dn4, Dn5, Dn6), sep = ",")

mc_error <- mica_creek_df[188,]
mc_error %>% 
  separate_rows(c(Dn1, Dn2), sep = ",")

# exploration ####
no_node_mc <- mica_creek_df %>% 
  select(!c(Dn1, Dn2, suck_mite_n, suck_thrips_n, Dn5, Dn4, Dn6)) %>% 
  mutate_at(vars(6:20), as.numeric) %>% 
  mutate_at(vars(1,3,4,5), as.factor)

ggplot(no_node_df, aes(y = D2_p, x = Site))+
  geom_point()+
  geom_abline()

