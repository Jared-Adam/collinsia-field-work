# packages ####
library(tidyverse)

# data ####

pf <- X2026_7_23_Pitfall_SA_length
q <- X2026_7_22_QuadratIDs


# cleaning and such ####

q

pf <- pf %>% # match q name
  rename(qtag = Quadrat)

df <- inner_join(q, pf, by = "qtag") 
unique(df$loc)

df_c <- df %>% 
  mutate(el = case_when(band == '1' & loc == 'MC' ~ '5400',
                        band == '2'& loc == 'MC' ~'6150',
                        band == '3' & loc == 'MC'~ '6320',
                        band == '4' & loc == 'MC'~ '6760', 
                        band == '5' & loc == 'MC'~ '7200',
                        band == '6' & loc == 'MC'~ '7900', 
                        band == '7' & loc == 'MC'~ '8200',
                        band == '8'& loc == 'MC' ~ '8370',
                        band == '9' & loc == 'MC'~ '8800',
                        band == '10'& loc == 'MC' ~ '9150', 
                        band == '1' & loc == 'IR' ~ '5500', 
                        band == '2' & loc == 'IR' ~ '5650', 
                        band == '3' & loc == 'IR' ~ '6300',
                        band == '4' & loc == 'IR' ~ '6650', 
                        band == '5' & loc == "IR" ~ '6950', 
                        band == '6' & loc == 'IR' ~ '7500', 
                        band == '7' & loc == 'IR' ~ '8120',
                        band == '8' & loc == 'IR' & rep == '1' ~ '8120',
                        band == '8' & loc == 'IR' & rep == '2' ~ '8300',
                        band == '9' & loc == 'IR' & rep == '1' ~ '8600',
                        band == '9' & loc == 'IR' & rep == '2' ~ '9000', 
                        band == '10' & loc == 'IR' ~ '9500',
                        band == '1' & loc == 'HR' ~ '5600',
                        band == '2' & loc == 'HR' ~ '5900',
                        band == '3' & loc == 'HR' ~ '6300',
                        band == '4' & loc == 'HR' ~ '6750', 
                        band == '5' & loc == 'HR' ~ '7100', 
                        band == '6' & loc == 'HR' ~ '7600',
                        band == '7' & loc == 'HR' ~ '8000')) %>%
  mutate(el = as.numeric(el)) %>% 
  mutate(el = el*0.3) %>% 
  mutate(qnum = as.factor(qnum)) %>% 
  relocate(Date, el)

spider <- ggplot(df_c, aes(x = el, y = Spiders))+
  geom_smooth(method = 'gam',
              formula = y ~s (x, k =4 ))+
  #geom_point(aes(shape = qnum, size = 3))+
  theme_bw() +
  xlab("Elevation (m)")+
  ylab("Spiders")+
  labs(title = "Spiders x Elevation (all Loc)")+
  theme(axis.title = element_text(size=24),
        panel.grid = element_blank(),
        plot.title = element_text(size=24),
        axis.text = element_text(size = 24),
        legend.text = element_text(size = 18),
        axis.ticks.length = unit(.25, 'cm'),
        legend.title = element_text(size = 20),
        strip.text.x = element_text(size = 20))+
  scale_x_continuous(breaks = seq(1620, 3000, by = 160))+
  ylim(0,6)

ggsave("Spider_PF.pdf", 
       spider, 
       width = 16, 
       height = 6, 
       units = 'in', 
       dpi = 600)
  
ant <- ggplot(df_c, aes(x = el, y = Ants))+
  geom_smooth(method = 'gam',
              formula = y ~s (x, k =4 ))+
  #geom_point(aes(shape = qnum, size = 3))+
  theme_bw() +
  xlab("Elevation (m)")+
  ylab("Ants")+
  labs(title = "Ants x Elevation (all Loc)")+
  theme(axis.title = element_text(size=24),
        panel.grid = element_blank(),
        plot.title = element_text(size=24),
        axis.text = element_text(size = 24),
        legend.text = element_text(size = 18),
        axis.ticks.length = unit(.25, 'cm'),
        legend.title = element_text(size = 20),
        strip.text.x = element_text(size = 20))+
  scale_x_continuous(breaks = seq(1620, 3000, by = 160))+
  ylim(0,NA) 

ggsave("Ant_PF.pdf", 
       ant, 
       width = 16, 
       height = 6, 
       units = 'in',
       dpi = 600) 

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
