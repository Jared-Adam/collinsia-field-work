# Packages ####

# data ####
rw_df <- X2025_7_22_RW_field_data_online_entry

# early wrangle  ####
rw_df
rw_mean_df <- rw_df
rw_mean_df[rw_mean_df == 'na'] <- NA

# removed damage 6 bc the NAs were causing a fuss. Will plot that damage indep of the loop
colnames(rw_mean_df)
rw_prelim_fxn_df <- rw_mean_df %>% 
  rename(suck_mite_p = 'suck-mite_p',
         q = Quadrat) %>% 
  filter(D1_p != 'NA') %>% 
  mutate(elevation = case_when(Site == 1 ~ '5600',
                               Site == 2 ~ '6050',
                               Site == 3 ~ '6700',
                               Site == 4 ~ '6900',
                               Site == 5 ~ '7300')) %>% 
  select(elevation, q, D1_p, D2_p, suck_mite_p, D5_p, D4_p, PH, PD, FlC, FrC) %>% 
  mutate_at(vars(3:11), as.numeric) %>% 
  mutate_at(vars(1:2), as.factor) %>% 
  mutate(elevation = as.character(elevation)) %>% 
  group_by(elevation, q) %>% 
  summarise(
    edge_no_mv = mean(D1_p), 
    edge_mv = mean(D2_p), 
    suckm = mean(suck_mite_p),
    scape = mean(D5_p), 
    hole = mean(D4_p),
    height_m = mean(PH), 
    diam_m = mean(PD),
    flower_ct = mean(FlC),
    fruit_ct = mean(FrC)
  ) %>% 
  print(n = Inf)
rw_prelim_fxn_df
# plotting function ####

rw_exp_var <- names(rw_prelim_fxn_df[1])
rw_exp_var <- set_names(rw_exp_var)

rw_resp_var <- names(rw_prelim_fxn_df[3:11])
rw_resp_var <-set_names(rw_resp_var)


q.labs <- c("Sun", "Shade")
names(q.labs) <- c('1', '2')


rw_dot_plots <- function(x,y){
  ggplot(rw_prelim_fxn_df, aes(x = .data[[x]], y = .data[[y]]))+
    geom_point(aes(size = 3, color = q))+
    facet_wrap(~q, 
               labeller = labeller(q = q.labs))+
    ylim(0,NA)+
    theme_bw()+
    theme(
      legend.position = 'none',
      axis.text = element_text(size = 10)
    )+
    labs(title = 'Retirement Wall')
}


plo2 <- map(rw_resp_var,
            ~map(rw_exp_var, rw_dot_plots, y = .x))
plo2_list <- map(plo2 ,~cowplot::plot_grid(plotlist = .x))
ggarrange(plotlist = plo2_list)


# Damage 6
rw_d6_plot_df <- rw_mean_df %>% 
  mutate(elevation = case_when(Site == 1 ~ '5600',
                               Site == 2 ~ '6050',
                               Site == 3 ~ '6700',
                               Site == 4 ~ '6900',
                               Site == 5 ~ '7300')) %>% 
  rename(q = Quadrat) %>% 
  select(elevation, q, D6_p) %>% 
  drop_na() %>% 
  mutate(D6_p = as.numeric(D6_p),
         elevation = as.factor(elevation),
         q = as.factor(q)) %>% 
  group_by(elevation,q ) %>% 
  summarise(
    mine = mean(D6_p) 
  ) %>% 
  ggplot(aes(x = elevation, y = mine))+
  facet_wrap(~q,
             labeller = labeller(q = q.labs))+
  geom_point(aes(size = 3, color = q))+
  theme_bw()+
  theme(
    legend.position = 'none',
    axis.text = element_text(size = 10)
  )+
  labs(title = 'Retriement wall D6')

rw_d6_plot_df
