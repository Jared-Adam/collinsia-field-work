# Packages ####
library(tidyverse)
library(ggpubr)


# plotting all variables ####

# changed df to mc_prelim_fxn_df on 9/24/2025

dmg_exp_var <- names(prelim_fxn_df[1])
dmg_exp_var <- set_names(dmg_exp_var)

dmg_resp_var <- names(prelim_fxn_df[3:14])
dmg_resp_var <-set_names(dmg_resp_var)

q.labs <- c("Sun", "Shade")
names(q.labs) <- c('1', '2')

dot_plots <- function(x,y){
  ggplot(prelim_fxn_df, aes(x = .data[[x]], y = .data[[y]]))+
    geom_point(aes(size = 3, color = q))+
    facet_wrap(~q, 
               labeller = labeller(q = q.labs))+
    ylim(0,NA)+
    theme_bw()+
    theme(
      legend.position = 'none',
      axis.text = element_text(size = 8)
    )+
    labs(title = 'Mica Creek')
}

plo1 <- map(dmg_resp_var,
            ~map(dmg_exp_var, dot_plots, y = .x))
plo1_list <- map(plo1 ,~cowplot::plot_grid(plotlist = .x))
ggarrange(plotlist = plo1_list)


##

## plots for the raw data with mean and jitter ####

raw_exp_var <- names(raw_df[1])
raw_exp_var <- set_names(raw_exp_var)

raw_resp_var <- names(raw_df[3:11])
raw_resp_var <- set_names(raw_resp_var)

q.labs <- c("Sun", "Shade")
names(q.labs) <- c('1', '2')

raw_plot <- function(x,y){
  ggplot(raw_df, aes(x = .data[[x]], y = .data[[y]]))+
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
    labs(title = 'Mica Creek')
    
}


raw_plo <- map(raw_resp_var,
            ~map(raw_exp_var, raw_plot, y = .x))
raw_plo_list <- map(raw_plo ,~cowplot::plot_grid(plotlist = .x))
ggarrange(plotlist = raw_plo_list)

##


# misc ####

# fitness_exp_var <- names(prelim_fxn_df[1])
# fitness_exp_var <- set_names(fitness_exp_var)
# 
# fitness_resp_var <- names(prelim_fxn_df[3:7])
# fitness_resp_var <-set_names(fitness_resp_var)

# exploring the variables that are not damage ####

colnames(mc_prop_df)
fe_df <- mc_prop_df %>% 
  select(Site, Date, Quadrat, D6_ct, PH, PD, FlC, FrC) %>% 
  mutate_at(vars(3:8), as.numeric) %>% 
  rename(date = Date,
         site = Site,
         q = Quadrat) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"),
         site = as.factor(site),
         q = as.factor(q)) %>% 
  filter(D6_ct != 'NA',) %>% 
  group_by(site, q) %>% 
  summarise(
    mine.ctm = mean(D6_ct),
    phm = mean(PH, na.rm = TRUE),
    pdm = mean(PD, na.rm = TRUE),
    flower.ctm = mean(FlC),
    fruit.ctm = mean(FrC)) %>% 
  print(N = inf)


q.labs <- c("Sun", "Shade")
names(q.labs) <- c('1', '2')

ggplot(fe_df, aes(x = site, y = pdm))+
  geom_point()+
  ylim(0, NA)+
  facet_wrap(~q, 
             labeller = labeller(q = q.labs))
  

exp_var <- names(fe_df[1])
exp_var <- set_names(exp_var)

resp_var <- names(fe_df[3:7])
resp_var <-set_names(resp_var)

dot_plots <- function(x,y){
  ggplot(fe_df, aes(x = .data[[x]], y = .data[[y]]))+
    geom_point(aes(size = 3, color = q))+
    facet_wrap(~q, 
               labeller = labeller(q = q.labs))+
    ylim(0,NA)+
    theme_bw()+
    theme(
      legend.position = 'none',
      axis.text = element_text(size = 14)
    )+
    labs(title = 'Mica Creek')
}


mc_xtra_plots <- map(resp_var,
            ~map(exp_var, dot_plots, y = .x))
mc_xtra_plots_list <- map(mc_xtra_plots ,~cowplot::plot_grid(plotlist = .x))
ggarrange(plotlist = mc_xtra_plots_list)



?ylim







mc_xtra_plots <- map(fitness_resp_var,
            ~map(fitness_exp_var, dot_plots, y = .x))
mc_xtra_plots_list <- map(mc_xtra_plots ,~cowplot::plot_grid(plotlist = .x))
ggarrange(plotlist = mc_xtra_plots_list)
