# packages ####

library(tidyverse)


# data ####

wue <- X2026_7_8_WUE_pilot
mos <- X2026_7_8_WUE_pilot_moisture

# wue ####


# bar to psi is 1 bar = 14.504 psi
p1 <- wue %>% 
  mutate(trt = as.factor(trt),
         plant = as.factor(plant), 
         bar = as.numeric(bar), 
         time = as.numeric(time),
         date =  as.Date(date, "%m/%d/%Y")) %>% 
  mutate(day = as.factor(case_when(date == "2026-06-30" ~ 1,
                         date == '2026-07-01' ~ 2,
                         date == "2026-07-02" ~ 3))) %>% 
mutate(trt = as.factor(case_when(trt == "w" ~ "Control",
                         trt == "b" ~ "Hole punch",
                         trt == "r" ~ "Leaf tip"))) %>% 
  mutate(psi = bar*14.504) %>% 
  na.omit() %>% 
  print(n = Inf) %>% 
  ggplot(aes(x = time, y = psi, color = trt)) +
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
  labs(x = "Time of day (24hr)",
       y = "PSI")+
  xlim(250,450)


ggsave('WUE_Pilot.pdf',
       p1,
       width = 6,
       height = 6,
       units='in',
       dpi=600)

# moisture ####

mos %>% 
  select(1:8) %>% 
  mutate(trt = as.factor(case_when(trt == 'w' ~ 'Control',
                         trt == 'b' ~ 'Hole punch',
                         trt == 'r' ~ 'Leaf tip'))) %>%
  ggplot(aes(x = tag, y = water, color = trt))+
  geom_smooth(method = 'gam', 
              formula = y ~ s(x, k=4))
  


