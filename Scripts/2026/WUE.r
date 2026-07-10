# packages ####

library(tidyverse)


# data ####

wue <- X2026_7_8_WUE_pilot


# view ####

wue %>% 
  mutate(trt = as.factor(trt),
         plant = as.factor(plant), 
         bar = as.numeric(bar), 
         time = as.numeric(time),
         date =  as.Date(date, "%m/%d/%Y")) %>% 
  mutate(day = as.factor(case_when(date == "2026-06-30" ~ 1,
                         date == '2026-07-01' ~ 2,
                         date == "2026-07-02" ~ 3))) %>% 
  na.omit() %>% 
  ggplot(aes(x = time, y = bar, color = trt)) +
    geom_smooth(method = 'gam',
                formula = y ~ s(x, k=4))+
    facet_grid(vars(date))+
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

