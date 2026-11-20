# packages ####
library(tidyverse)
library(here)

# data ####

mois_dir <- here("Data/2026", "2026-5-26_Cp_soil_moisture.csv")
moisture <- as_tibble(read.csv(mois_dir))


source(here("Scripts/2026/fxns", "location_elevation_fxn.r"))
moisture_elev <- elevation_band_fxn()




# additional cleaning ####

moisture_elev %>% 
  filter(loc == "IR") %>% 
  rename(TL_p = TL_.,
    BR_p = BR_.) %>% 
  mutate(avg_m = (TL_p + BR_p)/2,
    avg_temp = (TL_Temp+BR_Temp)/2,
    avg_perm = (TL_Perm+BR_Perm)/ 2,
    avg_cond = (TL_Cond+BR_Cond)/2) %>% 
  relocate(Date, qid, avg_m, avg_temp, avg_perm, avg_cond) %>% 
  ggplot(aes(x = Date, y = avg_m, group = qid, color = el_quadrat))+
  geom_point()+
  geom_line()+
  scale_color_viridis_c()



dev.off()
unique(moisture_elev$loc)
# a function for sequential graphing of data by region
plot.moisture.elevation <- function(x = moisture_elev) {
  
  # a vector of names of regions to loop over
  locs <- unique(x$loc)
  
  # a loop to produce ggplot2 graphs 
  for (i in seq_along(locs)) {
    
    # make plots; note data = args in each geom
    plot_data <- x %>% 
      filter(loc == locs[i]) %>% 
      rename(TL_p = TL_.,
    BR_p = BR_.) %>% 
      mutate(avg_m = (TL_p + BR_p)/2,
    avg_temp = (TL_Temp+BR_Temp)/2,
    avg_perm = (TL_Perm+BR_Perm)/ 2,
    avg_cond = (TL_Cond+BR_Cond)/2) %>%
      relocate(Date, qid, avg_m, avg_temp, avg_perm, avg_cond)

  my_plot <- ggplot(plot_data, aes(x = Date, y = avg_m, group = qid, color = el_quadrat))+
  geom_point()+
  geom_line()+
  scale_color_viridis_c()+
      labs(title = paste("Location:", locs[i]))

    
    # print each plot to screen
    #print(my_plot)


  }
}     

all_my_plots <- plot.moisture.elevation(moisture_elev)

