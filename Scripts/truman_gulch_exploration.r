# packages ####
library(tidyverse)
library(ggpubr)


# plotting mean values ####

tg_exp_var <- names(tg_prelim_fx_df[1])
tg_exp_var <- set_names(tg_exp_var)

tg_resp_var <- names(tg_prelim_fx_df[3:13])
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
