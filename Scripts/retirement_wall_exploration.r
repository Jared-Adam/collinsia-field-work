# Packages ####
library(tidyverse)
library(ggpubr)


# plotting all ####

rw_exp_var <- names(rw_prelim_fxn_df[1])
rw_exp_var <- set_names(rw_exp_var)

rw_resp_var <- names(rw_prelim_fxn_df[3:13])
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

rw_raw_resp_var <- names(rw_raw_df[5:13])
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
