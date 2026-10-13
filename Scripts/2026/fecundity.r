# packages ####
library(here)
library(tidyverse)
library(ggridges)

# DFs ####
labeled_df

# fruit and flower ####

fec_df <- labeled_df %>% 
  select(Date, loc, band, el, qid, rep, Tag, PH, PD, FlC, FrC)

fec_df %>%
  select(-rep) %>% 
  filter(if_any(everything(), is.na)) %>% 
  print(n = Inf)
# when back in bozeman : There are several instances here where the q id is not a plot
# need to determine where these belong and then add them back to the other dfs/ wranglings too

# 457 = MC

fec_plot <- fec_df %>% 
  select(-rep) %>% 
  na.omit() %>% 
  group_by(loc, band, el, Tag, FrC) %>% 
  mutate(FrC = as.numeric(FrC)) %>%
  mutate(zone = case_when(
    el <= 914 ~ "Plains",
    el <1524 & el >914 ~ "Foothills",
    el <1829 & el >1524 ~ "Intermountain",
    el <2286 & el >1829 ~ "Subalpine",
    el >= 2286 ~ "Alpine",
    .default = as.character(el)
  )) 


fec_plot %>% 
  ggplot(aes(x = el, FrC))+
  geom_point()


fec_plot %>% 
  ggplot(aes(x = FrC, y = zone, fill = zone))+
  geom_density_ridges(alpha = 0.6)+
  theme_ridges( )

fec_plot %>% 
  ggplot(aes(x = el, y = FrC))+
  geom_smooth(method = 'gam', 
formula = y~s(x, k=4))+
  #geom_point()+
  coord_cartesian(xlim = NULL)+
  scale_x_continuous(breaks = c(1600, 2000, 2500, 2900))+
  theme_bw()+
  labs(
    title= "Fruit X Elevation",
    x = "Elevation (m)",
    y = "Fruit Count"
  )
