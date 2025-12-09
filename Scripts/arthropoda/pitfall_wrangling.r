# This script is for wrangling the pf sample data

# packages ####

library(tidyverse)


# data ####
pf <- X2025_11_4_Pitfalls


# exploration ~ prelim ####

unique(pf$Suborder)

pf_one <- pf %>% 
  mutate(elevation = case_when(Site == 1 ~ '6100',
                               Site == 2 ~ '6700',
                               Site == 3 ~ '7300',
                               Site == 4 ~ '8200',
                               Site == 5 ~ '8800',
                               Site == 6 ~ '9100')) %>% 
  select(-Date, -Location, -Method, -Collector, -Species, -notes, -Genus) %>% 
  relocate(elevation) %>% 
  mutate(fxn = case_when(Family %in% c('Formicidae', 'Lycosidae', "Caelifera", "Philodromidae", "Gryllidae", "Reduviidae", 
                                       "Staphylinidae" ,  "Ghaphosidae", "Cybaeidae", "Linyphiidae" ,"Mutillidae" ,
                                       "Zoropsidae",  "Miturgidae" , "Histeridae"  , "Dapriidae") ~ 'p',
                         Family %in% c("Rhaphidophoridae", "Cicadelidae", "Berytidae", "Fulgroid" , "Curculionidae",
                                       "Scarabiidae" , "Tenebrionidae" ,"Chrysomeldiae?" , "Scarabaeidae" , "Tipulidae",
                                       "Aphidae") ~ 'h',
                         Family %in% c("Berytidae", "Elateridae", "Carabidae", "Coccinellidae") ~ "o",
                         Order %in% c("Lepidoptera", "Acari", "Collembola", "Diplopoda", "Archaeognatha", "Diplododa") ~ 'h',
                         Order %in% c("Chilopoda") ~ 'p',
                         Order %in% c("Diptera", "Dermaptera", "Opiliones") ~ 'o',
                         Suborder %in% c("Sternorrhyncha","Auchenorrhyncha", "Caelifera") ~ 'h',
                         Suborder %in% c("Aprocita") ~ 'p')) %>% 
  mutate_at(vars(1:4), as.factor) 
unique(pf_one$fxn)


# obtain counts ####

pf_count <- pf_one %>% 
  group_by(elevation, sun_shade) %>% 
  count(fxn) %>% 
  rename(q = sun_shade) %>% 
  na.omit(TRUE) %>% 
  print(n = Inf)



         
         
         
         
