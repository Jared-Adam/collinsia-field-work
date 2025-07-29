# Packages ####
library(tidyverse)
library(ggpubr)

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

# how about totaling all damage?
# or, create a proportion of damage type based on site?

mc_test_df[mc_test_df =='na'] <- NA
mc_test_df %>% 
  select(Site, D1_p) %>% 
  mutate(D1_p = as.numeric(D1_p)) %>% 
  drop_na() %>% 
  group_by(Site) %>% 
  summarise(
    mean = mean(D1_p)
  ) %>% 
  ggplot(aes(x = Site, y = mean))+
  geom_point()+
  labs(title = "Damage type 1")


mc_prop_df <- mica_creek_df
mc_prop_df[mc_prop_df =='na'] <- NA
mc_prop_df %>% 
  select(Site, D1_p) %>% 
  mutate(D1_p = as.numeric(D1_p),
         Site = as.character(Site)) %>% 
  drop_na() %>% 
  group_by(Site) %>% 
  summarise(
    mean = mean(D1_p)
  ) %>% 
  ggplot(aes(x = Site, y = mean))+
  geom_point()+
  labs(title = "Damage type 1")


# fxn for the different damage types 
# sort the df to get the damage types in order

prelim_fxn_df <- mc_prop_df %>% 
  select(Site, D1_p, D2_p, suck_mite_p, suck_thrips_p, D5_p, D4_p, D6_p) %>% 
  mutate_at(vars(2:8), as.numeric) %>% 
  mutate(Site = as.character(Site)) %>% 
  drop_na() %>% 
  group_by(Site) %>% 
  summarise(
    d1m = mean(D1_p), 
    d2m = mean(D2_p), 
    suckm = mean(suck_mite_p),
    thripm = mean(suck_thrips_p),
    d5m = mean(D5_p), 
    d4m = mean(D4_p), 
    d6m = mean(D6_p)
  )

exp_var <- names(prelim_fxn_df[1])
exp_var <- set_names(exp_var)

resp_var <- names(prelim_fxn_df[2:8])
resp_var <-set_names(resp_var)

dot_plots <- function(x,y){
  ggplot(prelim_fxn_df, aes(x = .data[[x]], y = data[[y]]))+
    geom_point()+
    theme_bw()
}

prelim_plots <- map(resp_var, ~dot_plots(.y, 'Site'))
ggarrange(plotlist = prelim_plots)



# exploration ####
no_node_mc <- mica_creek_df %>% 
  select(!c(Dn1, Dn2, suck_mite_n, suck_thrips_n, Dn5, Dn4, Dn6)) %>% 
  mutate_at(vars(6:20), as.numeric) %>% 
  mutate_at(vars(1,3,4,5), as.factor)

ggplot(no_node_df, aes(y = D2_p, x = Site))+
  geom_point()+
  geom_abline()

