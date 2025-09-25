# script for grouped damage types and locations

# packages ####
library(tidyverse)
library(ggpubr)

# data
final_merge

# by raw values ####

ggplot(final_merge, aes(x = elev_new, y = sum))+
  geom_point()+
  stat_summary(fun = 'mean', color = 'red', size = 5, geom = 'point')

exp <- names(final_merge[2])
exp <- set_names(exp)

resp <- names(final_merge[10:14])
resp <- set_names(resp)

q.labs <- c("Sun", "Shade")
names(q.labs) <- c('1', '2')

dot_plots <- function(x,y){
  ggplot(final_merge, aes(x = .data[[x]], y = .data[[y]]))+
    geom_point(aes(size = 3, color = q))+
    facet_wrap(~q, 
               labeller = labeller(q = q.labs))+
    stat_summary(fun = 'mean', color = 'red', size = 5, geom = 'point')+
    ylim(0,NA)+
    theme_bw()+
    theme(
      legend.position = 'none',
      axis.text = element_text(size = 8)
    )
}

plots_raw <- map(resp,
            ~map(exp, dot_plots, y = .x))
raw_list <- map(plots_raw ,~cowplot::plot_grid(plotlist = .x))
ggarrange(plotlist = raw_list)

final_merge %>% 
  mutate(elevation = as.numeric(levels(elevation))[elevation]) %>%
ggplot(aes(x = elevation, y = sum, color = q))+
  geom_smooth(method = 'gam',
              formula = y ~ s(x, k =4))
  




# by mean values ####

mean_exp <- names(final_merge[2])
mean_exp <- set_names(mean_exp)

mean_resp <- names(final_merge[10:14])
mean_resp <- set_names(mean_resp)

final_merge_df <- as.data.frame(final_merge)

# broken function 9/25/2025
mean_plots <- function(x,y){
  final_merge_df %>% 
    group_by(elev_new , q) %>%
    filter(sum != 'NA') %>% 
  ggplot(final_merge_df, aes(x = .data[[x]], y = .data[[y]]))+
    geom_point(aes(size = 3, color = q))+
    stat_summary(fun = 'mean', geom = 'line')+
    ylim(0,NA)+
    theme_bw()+
    theme(
      legend.position = 'none',
      axis.text = element_text(size = 8)
    )
}

plots_mean <- map(mean_resp,
                 ~map(mean_exp, dot_plots, y = .x))
raw_list <- map(plots_mean ,~cowplot::plot_grid(plotlist = .x))
ggarrange(plotlist = plots_mean)



final_merge %>%
  group_by(elev_new , q) %>%
  filter(sum != 'NA') %>% 
  summarise(mean = mean(sum)) %>% 
  ggplot(aes(x = elev_new, y = mean, color = q))+
  geom_line(aes(group = q), size = 2)+
  labs(title = 'Mean damage')

final_merge %>%
  group_by(elev_new , q) %>% 
  filter(PD != 'NA') %>% 
  summarise(mean = mean(PD)) %>% 
  ggplot(aes(x = elev_new, y = mean, color = q))+
  geom_line(aes(group = q), size = 2)+
  labs(title = 'Mean diameter')

final_merge %>%
  group_by(elev_new , q) %>% 
  filter(PH != 'NA') %>% 
  summarise(mean = mean(PH)) %>% 
  ggplot(aes(x = elev_new, y = mean, color = q))+
  geom_line(aes(group = q), size = 2)+
  labs(title = 'Mean height')

final_merge %>%
  group_by(elev_new , q) %>% 
  filter(FrC != 'NA') %>% 
  summarise(mean = mean(FrC)) %>% 
  ggplot(aes(x = elev_new, y = mean, color = q))+
  geom_line(aes(group = q), size = 2)+
  labs(title = 'Mean fruit')

final_merge %>%
  group_by(elev_new , q) %>% 
  filter(FlC != 'NA') %>% 
  summarise(mean = mean(FlC)) %>% 
  ggplot(aes(x = elev_new, y = mean, color = q))+
  geom_line(aes(group = q), size = 2)+
  labs(title = 'Mean flower')
