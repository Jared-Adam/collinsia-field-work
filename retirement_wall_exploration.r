# Packages ####

# data ####
rw_df <- X2025_7_22_RW_field_data_online_entry

# early wrangle  ####
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
# plotting function ####

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
