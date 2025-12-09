# exploration of pf data ####

# packages ####
library(tidyverse)

# data ####

pf_count
pf_one
pf


# predators ####

pred_elevation <- pf_count %>% 
  filter(fxn == 'p') %>% 
  mutate(elevation = as.numeric(levels(elevation))[elevation]) %>%
  mutate(meters = (elevation * 0.3038)) %>% 
  mutate(q = case_when(q == '1' ~ 'Sun',
                       q == '2' ~ 'Shade')) %>% 
  ggplot(aes(x = meters, y = n, color = q))+
  geom_smooth(method = 'gam',
              formula = y ~ s(x, k =4))+
  geom_point(size = 4)+
  labs(y = 'Count', x = 'Elevation (m)', title = 'Truman Gulch: Predators x Elevation')+
  theme_bw() +
  theme(title = element_text(size = 24),
        axis.title = element_text(size=24),
        panel.grid = element_blank(),
        plot.subtitle = element_text(size=20, hjust = 0.5),
        axis.text = element_text(size = 24),
        legend.text = element_text(size = 18),
        axis.ticks.length = unit(.25, 'cm'),
        legend.title = element_blank())+
  scale_color_brewer(palette = "Dark2")+
  scale_x_continuous(breaks = seq(1850,2700, by = 200))

ggsave('predatorsXelev.pdf',
       pred_elevation,
       width = 10,
       height = 6,
       units='in',
       dpi=600)



# herbivores ####

herb_elevation <- pf_count %>% 
  filter(fxn == 'h') %>% 
  mutate(elevation = as.numeric(levels(elevation))[elevation]) %>%
  mutate(meters = (elevation * 0.3038)) %>% 
  mutate(q = case_when(q == '1' ~ 'Sun',
                       q == '2' ~ 'Shade')) %>% 
  ggplot(aes(x = meters, y = n, color = q))+
  geom_smooth(method = 'gam',
              formula = y ~ s(x, k =4))+
  geom_point(size = 4)+
  labs(y = 'Count', x = 'Elevation (m)', title = 'Truman Gulch: Herbivores x Elevation')+
  theme_bw() +
  theme(title = element_text(size = 24),
        axis.title = element_text(size=24),
        panel.grid = element_blank(),
        plot.subtitle = element_text(size=20, hjust = 0.5),
        axis.text = element_text(size = 24),
        legend.text = element_text(size = 18),
        axis.ticks.length = unit(.25, 'cm'),
        legend.title = element_blank())+
  scale_color_brewer(palette = "Dark2")+
  scale_x_continuous(breaks = seq(1850,2700, by = 200))

ggsave('herbssXelev.pdf',
       herb_elevation,
       width = 10,
       height = 6,
       units='in',
       dpi=600)


# omnivores ####

omnivore_elevation <- pf_count %>% 
  filter(fxn == 'o') %>% 
  mutate(elevation = as.numeric(levels(elevation))[elevation]) %>%
  mutate(meters = (elevation * 0.3038)) %>% 
  mutate(q = case_when(q == '1' ~ 'Sun',
                       q == '2' ~ 'Shade')) %>% 
  ggplot(aes(x = meters, y = n, color = q))+
  geom_smooth(method = 'gam',
              formula = y ~ s(x, k =4))+
  geom_point(size = 4)+
  labs(y = 'Count', x = 'Elevation (m)', title = 'Truman Gulch: Omnivores x Elevation')+
  theme_bw() +
  theme(title = element_text(size = 24),
        axis.title = element_text(size=24),
        panel.grid = element_blank(),
        plot.subtitle = element_text(size=20, hjust = 0.5),
        axis.text = element_text(size = 24),
        legend.text = element_text(size = 18),
        axis.ticks.length = unit(.25, 'cm'),
        legend.title = element_blank())+
  scale_color_brewer(palette = "Dark2")+
  scale_x_continuous(breaks = seq(1850,2700, by = 200))

ggsave('omnivoressXelev.pdf',
       omnivore_elevation,
       width = 10,
       height = 6,
       units='in',
       dpi=600)



