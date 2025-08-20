# packages ####
library(tidyverse)
library(ggpubr)
# data ####

truman_df <- X2025_7_22_TG_field_data_online_entry

# early wrangle ####

# again, pulling d6 out

truman_df
tg_mean_df <- truman_df
tg_mean_df[tg_mean_df == 'na'] <- NA
colnames(tg_mean_df)
tg_prelim_fx_df <- tg_mean_df %>% 
  filter(D1_p != 'NA') %>% 
  rename(suck_mite_p = 'suck-mite_p',
         q = Quadrat) %>% 
  mutate(elevation = case_when(Site == 1 ~ '5400',
                               Site == 2 ~ '6100',
                               Site == 3 ~ '6900',
                               Site == 4 ~ '7600',
                               Site == 5 ~ '8360')) %>% 
  select(q, elevation, D1_p, D2_p, suck_mite_p, D5_p, D4_p, PH, PD, FlC, FrC) %>%
  mutate_at(vars(2:11), as.numeric) %>% 
  mutate(elevation = as.factor(elevation),
         q = as.factor(q)) %>% 
  group_by(elevation,q) %>% 
  summarise(
    edge_no_mv = mean(D1_p),
    edge_mv = mean(D2_p),
    suckm = mean(suck_mite_p),
    scrape = mean(D5_p),
    hole = mean(D4_p),
    phm = mean(PH, na.rm = TRUE),
    pdm = mean(PD, na.rm = TRUE),
    flower.ctm = mean(FlC),
    fruit.ctm = mean(FrC)
  )

# plotting function ####

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

