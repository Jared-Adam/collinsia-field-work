# Packages ####
library(tidyverse)
library(ggpubr)

# data ####
mica_creek_df <- X2025_7_15_MC_field_data_online_entry
rw_df <- X2025_7_22_RW_field_data_online_entry
truman_df <- X2025_7_22_TG_field_data_online_entry

# MC wrangling ####
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


# MC exploration ####
no_node_mc <- mica_creek_df %>% 
  select(!c(Dn1, Dn2, suck_mite_n, suck_thrips_n, Dn5, Dn4, Dn6)) %>% 
  mutate_at(vars(6:20), as.numeric) %>% 
  mutate_at(vars(1,3,4,5), as.factor)

ggplot(no_node_df, aes(y = D2_p, x = Site))+
  geom_point()+
  geom_abline()
# Mica Creek early plots #### 

# how about totaling all damage?
# or, create a proportion of damage type based on site?

# test
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

# real
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
 # drop_na() %>% 
  filter(D1_p != 'NA') %>% 
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
  ggplot(prelim_fxn_df, aes(x = .data[[x]], y = .data[[y]]))+
    geom_point()+
    theme_bw()+
    labs(title = 'Mica Creek')
}

# to accomplish looping across the response variable, you have to nest the exp within. double map! 

plo1 <- map(resp_var,
            ~map(exp_var, dot_plots, y = .x))
# turn the plots into a list 
plo1_list <- map(plo1 ,~cowplot::plot_grid(plotlist = .x))
# combine the list 
mc_plot_pdf <- ggarrange(plotlist = plo1_list)
mc_plot_pdf
pdf('mc_plot_pdf.pdf')
dev.off()

# Retirement wall early wrangle and plots ####
rw_df
rw_mean_df <- rw_df
rw_mean_df[rw_mean_df == 'na'] <- NA

# removed damage 6 bc the NAs were causing a fuss. Will plot that damage indep of the loop

rw_prelim_fxn_df <- rw_mean_df %>% 
  rename(suck_mite_p = 'suck-mite_p') %>% 
  filter(D1_p != 'NA') %>% 
  select(Site, D1_p, D2_p, suck_mite_p, D5_p, D4_p) %>% 
  mutate_at(vars(2:6), as.numeric) %>% 
  mutate(Site = as.character(Site)) %>% 
  group_by(Site) %>% 
  summarise(
    d1m = mean(D1_p), 
    d2m = mean(D2_p), 
    suckm = mean(suck_mite_p),
    d5m = mean(D5_p), 
    d4m = mean(D4_p)
  )
rw_prelim_fxn_df

rw_exp_var <- names(rw_prelim_fxn_df[1])
rw_exp_var <- set_names(rw_exp_var)

rw_resp_var <- names(rw_prelim_fxn_df[2:6])
rw_resp_var <-set_names(rw_resp_var)

rw_dot_plots <- function(x,y){
  ggplot(rw_prelim_fxn_df, aes(x = .data[[x]], y = .data[[y]]))+
    geom_point()+
    theme_bw()+
    labs(title = 'Retirement Wall')
}


plo2 <- map(rw_resp_var,
            ~map(rw_exp_var, rw_dot_plots, y = .x))
plo2_list <- map(plo2 ,~cowplot::plot_grid(plotlist = .x))
# combine the list 
rw_plot_pdf <- ggarrange(plotlist = plo2_list)
rw_plot_pdf


# Damage 6
rw_d6_plot_df <- rw_mean_df %>% 
  select(Site, D6_p) %>% 
  drop_na() %>% 
  mutate(D6_p = as.numeric(D6_p),
         Site = as.character(Site)) %>% 
  group_by(Site) %>% 
  summarise(
    d6_m = mean(D6_p) 
  ) %>% 
  ggplot(aes(x = Site, y = d6_m))+
  geom_point()+
  theme_bw()+
  labs(title = 'Retriement wall D6')


# TG early wrangle and plots ####

# again, pulling d6 out

truman_df
tg_mean_df <- truman_df
tg_mean_df[tg_mean_df == 'na'] <- NA

tg_prelim_fx_df <- tg_mean_df %>% 
  filter(D1_p != 'NA') %>% 
  rename(suck_mite_p = 'suck-mite_p') %>% 
  select(Site, D1_p, D2_p, suck_mite_p, D5_p, D4_p) %>%
  mutate_at(vars(2:6), as.numeric) %>% 
  mutate(Site = as.character(Site)) %>% 
  group_by(Site) %>% 
  summarise(
    d1_m = mean(D1_p),
    d2_m = mean(D2_p),
    suckm = mean(suck_mite_p),
    d5_m = mean(D5_p),
    d4_m = mean(D4_p)
  )
  

tg_exp_var <- names(tg_prelim_fx_df[1])
tg_exp_var <- set_names(tg_exp_var)

tg_resp_var <- names(tg_prelim_fx_df[2:6])
tg_resp_var <-set_names(tg_resp_var)

tg_dot_plots <- function(x,y){
  ggplot(tg_prelim_fx_df, aes(x = .data[[x]], y = .data[[y]]))+
    geom_point()+
    theme_bw()+
    labs(title = 'Truman Gulch')
}


plo3 <- map(tg_resp_var,
            ~map(tg_exp_var, tg_dot_plots, y = .x))
plo3_list <- map(plo3 ,~cowplot::plot_grid(plotlist = .x))
# combine the list 
tg_plot_pdf <- ggarrange(plotlist = plo3_list)
tg_plot_pdf


tg_d6_plot_df <- tg_mean_df %>% 
  select(Site, D6_p) %>% 
  drop_na() %>% 
  mutate(D6_p = as.numeric(D6_p),
         Site = as.character(Site)) %>% 
  group_by(Site) %>% 
  summarise(
    d6_m = mean(D6_p) 
  ) %>% 
  ggplot(aes(x = Site, y = d6_m))+
  geom_point()+
  theme_bw()+
  labs(title = 'TG D6')

