# packages ####
library(tidyverse)


# data ####
seeds <- X2026_8_10_SeedPredation_JA

# clean ####

seeds %>% 
  select(-1,-2) %>% 
  mutate(el = case_when(band == '1' ~ '5400',
                        band == '2' ~'6150',
                        band == '3' ~ '6320',
                        band == '4' ~ '6760', 
                        band == '5' ~ '7200',
                        band == '6' ~ '7900', 
                        band == '7' ~ '8200',
                        band == '8' ~ '8370',
                        band == '9' ~ '8800',
                        band == '10' ~ '9150')) %>% 
  relocate(band, el) %>% 
  mutate(el = as.numeric(el)) %>% 
  ggplot(aes(x = el, y = present_d_ct))+
  geom_smooth(method = 'gam',
       formula = y ~s(x, k=4))+
  theme_bw() +
  xlab("Elevation (feet)")+
  ylab("Seeds present on dish (10 max)")+
  labs(title = "Mica Creek Seed Predation - Petri Dish")+
  theme(axis.title = element_text(size=24),
        panel.grid = element_blank(),
        plot.title = element_text(size=24),
        axis.text = element_text(size = 24),
        legend.text = element_text(size = 18),
        axis.ticks.length = unit(.25, 'cm'),
        legend.title = element_text(size = 20),
        strip.text.x = element_text(size = 20))+
  scale_x_continuous(breaks = seq(5400, 9150, by = 300))+
  ylim(0,10)
           
