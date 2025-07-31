# packages ####


# data ####

truman_df <- X2025_7_22_TG_field_data_online_entry

# early wrangle ####

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

# plotting function ####

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

