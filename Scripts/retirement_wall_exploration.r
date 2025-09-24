# Packages ####
library(tidyverse)
library(ggpubr)


# plotting all but 6 ####

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

# plotting raw and jitter ####

rw_raw_exp_var <- names(rw_raw_df[1])
rw_raw_exp_var <- set_names(rw_raw_exp_var)

rw_raw_resp_var <- names(rw_raw_df[5:11])
rw_raw_resp_var <- set_names(rw_raw_resp_var)

q.labs <- c("Sun", "Shade")
names(q.labs) <- c('1', '2')

rw_raw_plot <- function(x,y){
  ggplot(rw_raw_df, aes(x = .data[[x]], y = .data[[y]]))+
    geom_point(aes(color = q))+
    facet_wrap(~q, 
               labeller = labeller(q = q.labs))+
    stat_summary(fun = 'mean', color = 'red', size = 5, geom = 'point')+
    ylim(0,NA)+
    theme_bw()+
    theme(
      legend.position = 'none',
      axis.text = element_text(size = 8)
    )+
    labs(title = 'Retirement Wall')
  
}


rw_raw_plo <- map(rw_raw_resp_var,
                  ~map(rw_raw_exp_var, rw_raw_plot, y = .x))
rw_raw_plo_list <- map(rw_raw_plo ,~cowplot::plot_grid(plotlist = .x))
ggarrange(plotlist = rw_raw_plo_list)



# Damage 6 ####
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

##

# raw 6 with jitter 

rw_mean_df %>% 
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
  rename(mine = D6_p) %>% 
  ggplot(aes(x = elevation, y = mine))+
  facet_wrap(~q,
             labeller = labeller(q = q.labs))+
  geom_point(aes(size = 3, color = q))+
  stat_summary(fun = 'mean', color = 'red', size = 10, geom = 'point')+
  theme_bw()+
  theme(
    legend.position = 'none',
    axis.text = element_text(size = 10)
  )+
  labs(title = 'Reitrement Wall mine mean dmg')


