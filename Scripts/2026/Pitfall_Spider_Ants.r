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
  relocate(Date, el) %>% 
  ggplot(aes(x = band, y = Spiders))+
  geom_smooth(method = 'gam',
              formula = )
  
  
  
  
# notes for the elevations 
  # IR , 1 5500, 2 5650, 3 6300, 4 6650, 5 6950, 6 7500, 81 8120, 82 8300, 91 8600, 92 9000, 10 9500 
  # HR , 1 5600, 2 5900, 3 6300, 4 6750, 5 7100, 6 7600, 7 8000
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
