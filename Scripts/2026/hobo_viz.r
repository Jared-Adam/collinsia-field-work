library(tidyverse)
library(readr)
library(ggridges)

# DFs ####
clean
clean1

# first plots #### 

clean %>% 
  subset(qid == 'q898') %>%
  ggplot(aes(x = date, y = temp_f))+
  geom_point() 

# all together 
ggplot(clean, aes(x = date, y = temp))+
  geom_point()+
  facet_wrap(~qid, scales = 'fixed')


# writing a function to automate some plots 

my_plot_func <- function(qid, clean) { # variable, data set
  subset_data <- filter(clean, qid == qid) # what I want the name of the column to be for the loop 
  
  p <- ggplot(subset_data, aes(x = date, y = temp)) +
    geom_point() +
    labs(title = paste("Plot for:", qid)) + # automate a title change 
    theme_minimal()
  
  # Save each plot automatically
  ggsave(filename = paste0("plot_", qid, ".png"), plot = p, # save and automate the naming 
  width = 14,
  height = 10, 
  unit = 'in')
  
  return(p)
}

unique_groups <- unique(clean$qid)
all_plots <- map(unique_groups, ~ my_plot_func(.x, clean))





# average plot ####
avg_plot <- clean1 %>%
  group_by(qid, elevation) %>% 
  summarise(
    mean = mean(temp)
  ) %>% 
  ggplot(aes(x = elevation, y = mean)) + 
  geom_point()+
  theme_bw()+
  labs(
    title = "Average soil temperature",
    x = "Elevation (m)",
    y = "Average temperature (C)"
  )

ggsave("avg_hobo_tremp.jpg", avg_plot,
height = 8,
width = 8, 
unit = 'in')
?ggsave

# GAM plot ####

hobo_gam <- clean1 %>% 
  ggplot(aes(x = elevation, y = temp))+
   geom_smooth(method = 'gam',
              formula = y ~s (x, k =4 ))+
  theme_bw()+
  labs(
    title = "Average soil temperature",
    x = "Elevation (m)",
    y = "Average temperature (C)"
  )
ggsave("hobo_gam.jpg", hobo_gam,
height = 8,
width = 8,
unit = 'in')
