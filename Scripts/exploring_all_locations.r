# script for grouped damage types and locations

# packages ####
library(tidyverse)
library(ggpubr)
library(RColorBrewer)

# data ####
final_merge
long_damage
meta
fitness_final

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




# by mean values ####
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

# GAMs of sum damage and unique damage type ####

#  y = predicted mean from a models

final_merge %>% 
  mutate(elevation = as.numeric(levels(elevation))[elevation]) %>%
  mutate(q = case_when(q == '1' ~ 'Sun',
                       q == '2' ~ 'Shade')) %>% 
ggplot(aes(x = elevation, y = sum, color = q))+
  geom_smooth(method = 'gam',
              formula = y ~ s(x, k =4))+
  theme_bw() +
  theme(axis.title = element_text(size=24),
        panel.grid = element_blank(),
        plot.subtitle = element_text(size=20, hjust = 0.5),
        axis.text = element_text(size = 24),
        legend.text = element_text(size = 18),
        axis.ticks.length = unit(.25, 'cm'),
        legend.title = element_blank())+
  scale_color_brewer(palette = "Dark2")+
  labs(x = "Elevation",
       y = "Damage")


long_damage %>% 
  mutate(damagae = as.factor(damage)) %>% 
  mutate(damage = case_when( damage == 'D1_p' ~ 'Edge No Midvein',
                             damage == 'D2_p' ~ 'Edge with Midvein',
                             damage == 'D4_p' ~ 'Hole', 
                             damage == 'D6_p' ~ 'Mine')) %>% 
  ggplot(aes(x = elevation, y = amount, color = damage))+
  geom_smooth(method = 'gam',
              formula = y ~ s(x, k =4))+
  theme_bw() +
  theme(axis.title = element_text(size=24),
        panel.grid = element_blank(),
        plot.subtitle = element_text(size=20, hjust = 0.5),
        axis.text = element_text(size = 24),
        legend.text = element_text(size = 18),
        axis.ticks.length = unit(.25, 'cm'),
        legend.title = element_text(size = 20))+
  guides(color=guide_legend(title="Damage Type"))+
  scale_color_brewer(palette = "Dark2")+
  labs(x = "Elevation",
       y = "Damage")

# GAMs of fitness x elevation ####

# Density x elevation 

densityXelevation_plot <- meta_clean %>% 
ggplot(aes(x = elevation, y = density, color = q))+
  geom_smooth(method = 'gam',
              formula = y ~ s(x, k =4))+
  theme_bw() +
  theme(axis.title = element_text(size=24),
        panel.grid = element_blank(),
        plot.subtitle = element_text(size=20, hjust = 0.5),
        axis.text = element_text(size = 24),
        legend.text = element_text(size = 18),
        axis.ticks.length = unit(.25, 'cm'),
        legend.title = element_blank())+
  scale_color_brewer(palette = "Dark2")+
  labs(x = "Elevation",
       y = "Density")

# 6x6 seems to be the best size for the elevation axis
ggsave('Collinsia_densityXelev.pdf',
       densityXelevation_plot,
       width = 6,
       height = 6,
       units='in',
       dpi=600)



# Size x elevation 

fitness_final

f_exp <- names(fitness_final[1])
f_exp <- set_names(f_exp)

f_resp <- names(fitness_final[3:6])
f_resp <- set_names(f_resp)

q.labs <- c("Sun", "Shade")
names(q.labs) <- c('1', '2')

f_plots <- function(x,y){
  ggplot(fitness_final, aes(x = .data[[x]], y = .data[[y]], color = q))+
    geom_smooth(method = 'gam',
                formula = y ~ s(x, k =4))+
    theme_bw() +
    theme(axis.title = element_text(size=24),
          panel.grid = element_blank(),
          plot.subtitle = element_text(size=20, hjust = 0.5),
          axis.text = element_text(size = 24),
          legend.text = element_text(size = 18),
          axis.ticks.length = unit(.25, 'cm'),
          legend.title = element_blank())+
    scale_color_brewer(palette = "Dark2")+
    labs(x = "Elevation")
}

f_plots_obj <- map(f_resp,
                 ~map(f_exp, f_plots, y = .x))
f_list <- map(f_plots_obj ,~cowplot::plot_grid(plotlist = .x))
fitness_plot_save <- ggarrange(plotlist = f_list)

# for multi-panel, save bigger 
ggsave('Collinsia_Fitness_plots.pdf',
       fitness_plot_save,
       width = 14,
       height = 14,
       units='in',
       dpi=600)


