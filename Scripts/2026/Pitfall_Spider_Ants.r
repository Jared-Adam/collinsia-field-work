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

df %>% 
  mutate(el = case_when(band == '1' & loc == 'MC' ~ '5400',
                        band == '2'& loc == 'MC' ~'6150',
                        band == '3' & loc == 'MC'~ '6320',
                        band == '4' & loc == 'MC'~ '6760', 
                        band == '5' & loc == 'MC'~ '7200',
                        band == '6' & loc == 'MC'~ '7900', 
                        band == '7' & loc == 'MC'~ '8200',
                        band == '8'& loc == 'MC' ~ '8370',
                        band == '9' & loc == 'MC'~ '8800',
                        band == '10'& loc == 'MC' ~ '9150')) %>%
  mutate(el = as.numeric(el)) %>% 
  mutate(el = el*0.3) %>% 
  relocate(Date, el)
  ggplot(aes(x = band, y = Spiders))+
  geom_smooth(method = 'gam',
              formula = )
