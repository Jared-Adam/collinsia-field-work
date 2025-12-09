# This script is for wrangling the pf sample data

# packages ####

library(tidyverse)


# data ####
pf <- X2025_11_4_Pitfalls


# exploration ~ prelim ####

unique(pf$Family)

pf %>% 
  mutate(elevation = case_when(Site == 1 ~ '6100',
                               Site == 2 ~ '6700',
                               Site == 3 ~ '7300',
                               Site == 4 ~ '8200',
                               Site == 5 ~ '8800',
                               Site == 6 ~ '9100')) %>% 
  select(-Date, -Location, -Method, -Collector, -Species, -notes) %>% 
  relocate(elevation) %>% 
  mutate_at(vars(1:4), as.factor) %>%
  mutate(fxn = case_when(Family %in% c('Formicidae', 'Lycosidae', "Caelifera", "Philodromidae", "Gryllidae", "Reduviidae", 
                                       "Staphylinidae" ,  "Ghaphosidae", "Cybaeidae", "Linyphiidae" ,"Mutillidae" ,
                                       "Zoropsidae",  "Miturgidae" , "Histeridae"  , "Dapriidae") ~ 'p'),
         fxn = case_when(Family %in% c("Rhaphidophoridae", "Cicadelidae", "Berytidae", "Fulgroid" , "Curculionidae",
                                       "Scarabiidae" , "Tenebrionidae" ,"Chrysomeldiae?" , "Scarabaeidae" , "Tipulidae",
                                       "Aphidae") ~ 'h'),
         fxn = case_when(Family %in% c("Berytidae", "Elateridae", "Carabidae", "Coccinellidae") ~ "o")) %>% 
  print(n = 150)
         
         
         
         
         
         
