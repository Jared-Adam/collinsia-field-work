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

ggsave('Collinsia_densityXelev.pdf',
       densityXelevation_plot,
       width = 6,
       height = 6,
       units='in',
       dpi=600)


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

ggsave('Collinsia_densityXelev.pdf',
       densityXelevation_plot,
       width = 6,
       height = 6,
       units='in',
       dpi=600)



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



# Plots for Will ####

#1: density ~ sun/shade (categorical)
#2: damage ~ elev, can you just do flea beetle and edge no mv, and can you add the points with + geom_point()
#3: Cp fecundity or size ~ Cp density
#4: FlC ~ elev and FrC ~ elev, but (1) remove sun/shade and (2) use method='lm' for an overall linear estimate of the relationships

# ALL with virdis option D


#1: density ~ sun/shade (categorical) ####

CP.Will.DenxSS.plot <- meta_clean %>% 
ggplot(aes(x = q, y = density, color = q))+
  stat_summary(fun = 'mean', size = 6, geom = 'point')+
    stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.3, size = 1)+
  ylim(0, NA)+
  theme_bw() +
  theme(axis.title.y = element_text(size=12),
        axis.title.x = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_text(size = 12),
        legend.position = 'none',
        axis.ticks.length = unit(.25, 'cm'))+
  scale_color_viridis_d(option = "D", end = 0.7)+
  labs(y = "Density")

ggsave('CP.Will.DenxSS.plot.pdf',
       CP.Will.DenxSS.plot,
       width = 3.5,
       height = 3.25,
       units='in',
       dpi=600)


#2: damage ~ elev, can you just do flea beetle and edge no mv, and can you add the points with + geom_point() ####

long_damage %>% 
  mutate(damage = as.factor(damage)) %>% 
  filter(damage == c('D1_p', 'D2_p', 'D6_p')) %>% 
  mutate(damage = case_when(damage == 'D1_p' ~ 'Edge No Midvein',
                             damage == 'D2_p' ~ 'Edge with Midvein',
                             damage == 'D6_p' ~ 'Mine')) %>% 
  ggplot(aes(x = meters, y = amount, color = damage, fill = damage))+
  geom_smooth(method = 'gam',
              formula = y ~ s(x, k = 4),
              size = 2)+
  geom_point()+
  theme_bw() +
  theme(axis.title = element_text(size=24),
        panel.grid = element_blank(),
        plot.subtitle = element_text(size=20, hjust = 0.5),
        axis.text = element_text(size = 24),
        legend.text = element_text(size = 18),
        axis.ticks.length = unit(.25, 'cm'),
        legend.title = element_text(size = 20))+
  guides(color=guide_legend(title="Damage Type"), fill = FALSE)+
  scale_color_viridis_d(option = "D", end = 0.85, direction = -1)+
  scale_fill_viridis_d(option = "D", end = 0.85, direction = -1)+
  labs(x = "Elevation",
       y = "Damage")


# WITH geom_point

CP.Will.DamxElev_GeomPoint <- final_merge %>% 
  mutate(chew = D1_p + D2_p) %>%
  select(c(elevation, chew, D6_p)) %>% 
  pivot_longer(!elevation, names_to = 'damage', values_to = 'amount') %>% 
  mutate(elevation = as.numeric(levels(elevation))[elevation])%>% 
  mutate(damage = case_when(damage == 'chew' ~ 'Grasshopper',
                            damage == 'D6_p' ~ 'Flea Beetle')) %>% 
  mutate(meters = (elevation * 0.3048)) %>% 
  ggplot(aes(x = meters, y = amount, color = damage, fill = damage))+
  geom_smooth(method = 'gam',
              formula = y ~ s(x, k = 4))+
  geom_point()+
  theme_bw() +
  theme(axis.title = element_text(size=12),
        panel.grid = element_blank(),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.ticks.length = unit(.25, 'cm'),
        legend.title = element_text(size = 12))+
  guides(color=guide_legend(title="Damage Type"), fill = FALSE)+
  scale_color_viridis_d(option = "D", end = 0.85, direction = -1)+
  scale_fill_viridis_d(option = "D", end = 0.85, direction = -1)+
  labs(x = "Elevation (m)",
       y = "Damage")

ggsave('CP.Will.DamxElev_GeomPoint.pdf',
       CP.Will.DamxElev_GeomPoint,
       width = 6,
       height = 3.25,
       units='in',
       dpi=600)



# WITHOUT geom_point
CP.Will.DamxElev <- final_merge %>% 
  mutate(chew = D1_p + D2_p) %>%
  select(c(elevation, chew, D6_p)) %>% 
  pivot_longer(!elevation, names_to = 'damage', values_to = 'amount') %>% 
  mutate(elevation = as.numeric(levels(elevation))[elevation])%>% 
  mutate(damage = case_when(damage == 'chew' ~ 'Grasshopper',
                            damage == 'D6_p' ~ 'Flea Beetle')) %>% 
  mutate(meters = (elevation * 0.3048)) %>% 
  ggplot(aes(x = meters, y = amount, color = damage, fill = damage))+
  geom_smooth(method = 'gam',
              formula = y ~ s(x, k = 4),
              size = 2)+
  theme_bw() +
  theme(axis.title = element_text(size=12),
        panel.grid = element_blank(),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.ticks.length = unit(.25, 'cm'),
        legend.title = element_text(size = 12))+
  guides(color=guide_legend(title="Damage Type"), fill = FALSE)+
  scale_color_viridis_d(option = "D", end = 0.85, direction = -1)+
  scale_fill_viridis_d(option = "D", end = 0.85, direction = -1)+
  labs(x = "Elevation (m)",
       y = "Damage")

ggsave('CP.Will.DamxElev.pdf',
       CP.Will.DamxElev,
       width = 6,
       height = 3.25,
       units='in',
       dpi=600)



#3: Cp fecundity or size ~ Cp density ####

left_meta <- meta_clean %>% 
  mutate(plant = 1:n()) %>% 
  mutate(plant = as.factor(plant))

final_left <- final_merge %>% 
  mutate(elevation = as.numeric(levels(elevation))[elevation]) %>% 
  select(elevation, loc, PH, PD, FlC, FrC) %>% 
  rename(Location = loc) %>% 
  mutate(Location = as.character(Location)) %>% 
  mutate(plant = 1:n()) %>% 
  mutate(plant = as.factor(plant)) %>% 
  relocate(plant) %>% 
  left_join(left_meta, join_by(elevation, Location), relationship = 'many-to-many') 


length_metric_left <- final_left %>% 
  select(density, PH, PD) %>% 
  pivot_longer(!density, names_to = 'fitness', values_to = 'unit')


CP.Will.SizeXDensity <- length_metric_left %>% 
  mutate(Size = case_when(fitness == 'PH' ~ 'Height',
                             fitness == 'PD' ~ 'Diameter')) %>% 
  ggplot(aes(x = density, y = unit, color = Size, fill = Size))+
  geom_smooth(method = 'gam',
              formula = y ~ s(x, k =4))+
  theme_bw() +
  theme(axis.title = element_text(size=12),
        panel.grid = element_blank(),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.ticks.length = unit(.25, 'cm'),
        legend.title = element_text(size = 12))+
  scale_color_viridis_d(option = "D", end = 0.85, direction = -1)+
  scale_fill_viridis_d(option = "D", end = 0.85, direction = -1)+
  labs(x = "Plant Density / Quadrat",
       y = "mm")+
  ylim(0,NA)

ggsave('CP.Will.SizeXDensity.pdf',
       CP.Will.SizeXDensity,
       width = 3.5,
       height = 3.25,
       units='in',
       dpi=600)


count_metric_left <- final_left %>% 
  select(density, FlC, FrC) %>% 
  pivot_longer(!density, names_to = 'fitness', values_to = 'count')

CP.Will.FecundXDensity <- count_metric_left %>% 
  mutate(Fecundity = case_when(fitness == 'FlC' ~ 'Flower Count',
                             fitness == 'FrC' ~ 'Fruit Count')) %>% 
  ggplot(aes(x = density, y = count, color = Fecundity, fill = Fecundity))+
  geom_smooth(method = 'gam',
              formula = y ~ s(x, k =4))+
  theme_bw() +
  theme(axis.title = element_text(size=12),
        panel.grid = element_blank(),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.ticks.length = unit(.25, 'cm'),
        legend.title = element_text(size = 12))+
  scale_color_viridis_d(option = "D", end = 0.85, direction = -1)+
  scale_fill_viridis_d(option = "D", end = 0.85, direction = -1)+
  labs(x = "Plant Density / Quadrat",
       y = "Count")+
  ylim(0,NA)

ggsave('CP.Will.FecundXDensity.pdf',
       CP.Will.FecundXDensity,
       width = 3.5,
       height = 3.25,
       units='in',
       dpi=600)


#4: FlC ~ elev and FrC ~ elev, but (1) remove sun/shade and (2) use method='lm' for an overall linear estimate of the relationships ####

fitness_final

CP.Will.FruitxElev <- ggplot(fitness_final, aes(x = meters, y = FrC))+
  geom_smooth(method = 'lm', size = 2)+
  theme_bw() +
  theme(axis.title = element_text(size=12),
        panel.grid = element_blank(),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.ticks.length = unit(.25, 'cm'),
        legend.title = element_blank())+
  labs(x = "Elevation (m)",
       y = "Fruit Count")+
  ylim(0,NA)

ggsave('CP.Will.FruitxElev.pdf',
       CP.Will.FruitxElev,
       width = 6,
       height = 3.25,
       units='in',
       dpi=600)


CP.Will.FlowerxElev <- ggplot(fitness_final, aes(x = meters, y = FlC))+
  geom_smooth(method = 'lm', size = 2)+
  theme_bw() +
  theme(axis.title = element_text(size=12),
        panel.grid = element_blank(),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.ticks.length = unit(.25, 'cm'),
        legend.title = element_blank())+
  labs(x = "Elevation (m)",
       y = "Flower Count")+
  ylim(0,NA)

ggsave('CP.Will.FlowerxElev.pdf',
       CP.Will.FlowerxElev,
       width = 6,
       height = 3.25,
       units='in',
       dpi=600)



