# packages ####
library(tidyverse)
library(ggpubr)


# plotting mean values ####

tg_exp_var <- names(tg_prelim_fx_df[1])
tg_exp_var <- set_names(tg_exp_var)

tg_resp_var <- names(tg_prelim_fx_df[3:11])
tg_resp_var <-set_names(tg_resp_var)

q.labs <- c("Sun", "Shade")
names(q.labs) <- c('1', '2')

tg_dot_plots <- function(x,y){
  ggplot(tg_prelim_fx_df, aes(x = .data[[x]], y = .data[[y]]))+
    geom_point(aes(size = 3, color = q))+
    facet_wrap(~q, 
               labeller = labeller(q = q.labs))+
    ylim(0,NA)+
    theme_bw()+
    theme(
      legend.position = 'none',
      axis.text = element_text(size = 8)
    )+
    labs(title = 'Truman Gulch')
}


plo3 <- map(tg_resp_var,
            ~map(tg_exp_var, tg_dot_plots, y = .x))
plo3_list <- map(plo3 ,~cowplot::plot_grid(plotlist = .x))
ggarrange(plotlist = plo3_list)


# plotting mean of d6 ####

tg_mean_df %>% 
  mutate(elevation = case_when(Site == 1 ~ '5400',
                             Site == 2 ~ '6100',
                             Site == 3 ~ '6900',
                           Site == 4 ~ '7600',
                             Site == 5 ~ '8360')) %>% 
  rename(q = Quadrat) %>% 
  select(elevation, q, D6_p) %>% 
  drop_na() %>% 
  mutate(D6_p = as.numeric(D6_p),
         elevation = as.factor(elevation),
         q = as.factor(q)) %>% 
  group_by(elevation, q) %>% 
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
  labs(title = 'Truman Gulch mine mean dmg')

# plotting raw over mean ####

tg_raw_df


tg_raw_exp_var <- names(tg_raw_df[1])
tg_raw_exp_var <- set_names(tg_raw_exp_var)

tg_raw_resp_var <- names(tg_raw_df[5:13])
tg_raw_resp_var <- set_names(tg_raw_resp_var)

q.labs <- c("Sun", "Shade")
names(q.labs) <- c('1', '2')

tg_raw_plot <- function(x,y){
  ggplot(tg_raw_df, aes(x = .data[[x]], y = .data[[y]]))+
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
    labs(title = 'Truman Gulch')
  
}


tg_raw_plo <- map(tg_raw_resp_var,
               ~map(tg_raw_exp_var, tg_raw_plot, y = .x))
tg_raw_plo_list <- map(tg_raw_plo ,~cowplot::plot_grid(plotlist = .x))
ggarrange(plotlist = tg_raw_plo_list)


# d6 raw with jitter ####

tg_mean_df %>% 
  mutate(elevation = case_when(Site == 1 ~ '5400',
                               Site == 2 ~ '6100',
                               Site == 3 ~ '6900',
                               Site == 4 ~ '7600',
                               Site == 5 ~ '8360')) %>% 
  rename(q = Quadrat) %>% 
  select(elevation, q, D6_p) %>% 
  rename(mine = D6_p) %>% 
  drop_na() %>% 
  mutate(mine = as.numeric(mine),
         elevation = as.factor(elevation),
         q = as.factor(q)) %>%
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
  labs(title = 'Truman Gulch mine mean dmg')
