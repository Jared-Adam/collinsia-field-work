# packages ####
library(tidyverse)
library(ggridges)
library(ggh4x)
library(DescTools)

# DFs ####
final_cp
max_value_df
e_df
tags_complete


# damage vis ####


dmg_plot_df <- final_cp %>% 
  rename("Chewing damage (%)" = D1_p,
    "Piercing/sucking damage (%)" = D2_p,
    "Adult beetle damage (%)" = D3_p,
    "Beetle leaf miner (%)" = D6_p,
    "Fly leaf miner (%)" = D7_p)

dmg_plots_raw <- function(data, x_col, y_col){

ggplot(dmg_plot_df, aes(x = el, y = .data[[y_col]]))+
  geom_smooth(method = 'gam', formula = y ~ s (x, k = 4))+
  geom_point(size = 2)+
  theme_bw()+
  labs(title = paste(y_col),
    x = "Elevation (m)")+
  theme(title = element_text(size = 18),
    axis.text = element_text(size = 16))
}

y_variables = c('Chewing damage (%)', 'Piercing/sucking damage (%)', 'Adult beetle damage (%)', 'Beetle leaf miner (%)', 'Fly leaf miner (%)')


raw_damage_plots <- map(y_variables, ~ dmg_plots_raw(data = dmg_plot_df, x_col = el, y_col = .x))
raw_damage_plots[[1]]

walk2(y_variables, raw_damage_plots, 
  ~ggsave(filename = paste0("raw_plot_", .x, ".png"), plot = .y,
width = 8,
height = 8,
unit = 'in'))




dmg_plots_gam <- function(data, x_col, y_col){

  ggplot(final_cp, aes(x = el, y = .data[[y_col]]))+
    geom_smooth(method = 'gam', formula = y ~ s (x, k=4))+
    theme_bw()
    #labs(title = paste("Relationship between", x_col, "and", y_col))
}
y_variables = c('D1_p', 'D2_p', 'D3_p', 'D6_p', 'D7_p')


gam_damage_plots <- map(y_variables, ~ dmg_plots_gam(data = final_cp, x_col = el, y_col = .x))
gam_damage_plots

walk2(y_variables, gam_damage_plots, 
  ~ggsave(filename = paste0("gam_plot_", .x, ".png"), plot = .y,
width = 8,
height = 8,
unit = 'in'))

# plot of average damage type 

wide_sum <- final_cp %>% 
  group_by(loc, band, el) %>% 
  summarise(across(c('D1_p', 'D2_p', 'D3_p', 'D6_p', 'D7_p'), mean, na.rm = TRUE)) %>% 
  drop_na() %>% 
  print(n = Inf)

wide_dmg_plots_raw <- function(data, x_col, y_col){
ggplot(data = wide_sum, aes(x = el, y = .data[[y_col]]))+
  geom_smooth(method = 'gam',
  formula= y ~ s(x, k=4))+
  geom_point()+
  theme_bw()

}
y_variables = c('D1_p', 'D2_p', 'D3_p', 'D6_p', 'D7_p')

wide_dmg_mean_raw <- map(y_variables, ~wide_dmg_plots_raw(data = wide_sum, x_col = el, y_col = .x))
walk2(y_variables, wide_dmg_mean_raw, 
  ~ggsave(filename = paste0("mean_raw_plot_", .x, ".png"), plot = .y,
width = 8,
height = 8,
unit = 'in'))

# long wise for total damage 
## FOR AYA 10-8-2026
df_for_avg_plot <- final_cp %>% 
  group_by(loc, band, el) %>% 
  # select(1:12) %>% 
  pivot_longer(c('D1_p', 'D2_p', 'D3_p', 'D6_p', 'D7_p'),
names_to = "dmg_type",
values_to = "dmg_p") %>% 
  ungroup() %>% 
  group_by(el) %>% 
  summarise(mean = mean(dmg_p, na.rm = TRUE))

df_for_avg_plot %>% 
  ggplot(aes(x = el, y = mean))+
  geom_smooth(method = 'gam',
formula = y~s(x, k=4))+
  geom_point(size = 2)+
  coord_cartesian()+
  labs(title = "Average total damage by elevation",
y = "Average percent damage",
x = "Elevation (m)")+
  theme_bw(base_size = 24)+
  theme(
    axis.title = element_text(size = 24),
    panel.grid = element_blank(),
    axis.text = element_text(size = 20))+
  scale_x_continuous(breaks = c(1600, 1800, 2000, 2200, 2400, 2600, 2800, 3000))

ggsave("FinalAvgDamage.pdf", plot = get_last_plot(), height = 10, width = 12, units = 'in', dpi = 300)
ggsave("FinalAvgDamage.jpg", plot = get_last_plot(), height = 10, width = 12, units = 'in', dpi = 300)


# new wrangling? #### 

final_cp
wide_sum

long_df <- final_cp %>% 
  group_by(loc, band, el) %>% 
  select(1:13) %>% 
  pivot_longer(c('D1_p', 'D2_p', 'D3_p', 'D6_p', 'D7_p'),
names_to = "dmg_type",
values_to = "dmg_p") %>% 
  ungroup() 

long_df %>% 
  filter(dmg_p == -Inf)


dmage_labels <- c("D1_p" = "Caterpillar",
"D2_p" = "Hemipteran/Mite",
"D3_p" = "Adult Beetle", 
"D6_p" = "Beetle leaf miner",
"D7_p" = "Fly leaf miner")

# The x axis is cluttered with all the elevations, going to remove from other facets w ggh4x

# plot of mean damage grouped by el and band
long_df %>% 
  group_by(band, el, dmg_type) %>% 
  summarise(mean = mean(dmg_p, na.rm = TRUE)) %>% 
  ggplot(aes(x = el, y = mean))+
  geom_point(size = 2)+
  facet_grid(~dmg_type, labeller = labeller(dmg_type = dmage_labels), scales = "free_x")+ 
  xlim(1500,3000)+
  ylim(0,15)+
  geom_smooth(method = 'gam',
formula = y~s(x, k=4))+
  labs(title = "Average damage percent x damage type",
y = "Average percent damage",
x = "Elevation (m)")+
  theme_bw(base_size = 24)+
  theme(
    axis.title = element_text(size = 24),
    panel.grid = element_blank(),
    axis.text = element_text(size = 20)
  )+
  facetted_pos_scales(
    x = list(
     scale_x_continuous(limits = c(1500,3000),
     breaks = c(1500,2000,2500,3000),
     labels = NULL),
     scale_x_continuous(limits = c(1500,3000),
     breaks = c(1500,2000,2500,3000),labels = NULL),
     scale_x_continuous(limits = c(1500,3000)),
     scale_x_continuous(limits = c(1500,3000),
     breaks = c(1500,2000,2500,3000),labels = NULL),
     scale_x_continuous(limits = c(1500,3000),
     breaks = c(1500,2000,2500,3000),labels = NULL)
    )
  )

ggsave("FinalBYDamageType.pdf", plot = get_last_plot(), height = 10, width = 14, units = 'in', dpi = 300)
ggsave("FinalBYDamageType.jpg", plot = get_last_plot(), height = 10, width = 14, units = 'in', dpi = 300)





# grouped by elevation df

long_mean_df <- final_cp %>% 
  group_by(loc, band, el) %>% 
  select(1:13) %>% 
  pivot_longer(c('D1_p', 'D2_p', 'D3_p', 'D6_p', 'D7_p'),
names_to = "dmg_type",
values_to = "dmg_p") %>% 
  ungroup() %>% 
  group_by(el) %>% 
  summarise(mean = mean(dmg_p, na.rm = TRUE)) 

# I want to split this into dmage by insect by elevational "group"
# what are those groups?
# potential groups: Plains: 1800-3000
# Foothills: 3000-5000
# Lower slopes/ intermountain: 3000-6000
# Subalpine: 6000-7500
# Alpine: 7500+

# get these in meters 
tibble(name = c('plains', 'foothills','intermountain','subalpine','alpine'),
value = c(3000, 5000, 6000, 7500, 7501))%>% 
  mutate(m = value*0.3048)


zoned_df <- long_df %>% 
  select(-6) %>% 
  group_by(loc, band, el, dmg_type, dmg_p) %>% 
  mutate(zone = case_when(
    el <= 914 ~ "Plains",
    el <1524 & el >914 ~ "Foothills",
    el <1829 & el >1524 ~ "Intermountain",
    el <2286 & el >1829 ~ "Subalpine",
    el >= 2286 ~ "Alpine",
    .default = as.character(el)
  )) %>% 
  group_by(band, el, loc, zone, dmg_p) %>% 
  summarise(standard = mean(dmg_p)*100) %>% 
  na.omit() 

# factor reorder of the zone names for ggridges plotting
zoned_df$zone <- factor(zoned_df$zone, levels = c("Intermountain", "Subalpine", "Alpine"))


zoned_df %>% 
  ggplot(aes(x = standard))+
  geom_histogram()+
  facet_grid(. ~ factor(zone, levels = c("Intermountain", "Subalpine", "Alpine")))+
  labs(title= "Relative frequency of damage by elevation zone",
x = "Relative frequency",
y = "Count")+
  theme_bw()

zoned_df %>% 
  ggplot(aes(x = standard, y = zone, fill = zone))+
  geom_density_ridges(alpha = 0.6)+
  theme_ridges()+
  labs(title = "Frequnecy of damage by elevation zone",
x = "Frequnecy on standardozed percent scale (MIGHT be wrong)",
y = "",
fill = "Elevation zone")+
  theme(
    axis.title.x = element_text(hjust = .5, size = 14),
    axis.text = element_text(size = 14)
  )



# max damage values ####
  
# extract the max damage from each plant 
final_cp %>% # quick vis with the wide df, switching to long
  group_by(Tag, el, loc) %>% 
  summarise(D1_value = max(D1_p, na.rm = TRUE),
D2_value = max(D2_p, na.rm = TRUE),
D3_value = max(D3_p, na.rm = TRUE),
D6_value = max(D6_p, na.rm = TRUE),
D7_value = max(D7_p, na.rm = TRUE)) %>%
  ungroup() %>% 
  na.omit() %>% 
  ggplot(aes(x = el, y = D1_value)) +
  geom_point()

final_cp %>% # just looking at the spread
  group_by(el) %>% 
  summarise(max = max(el, na.rm = TRUE)) %>% 
  print(n = Inf)



# plot of the max means... Why?
e_df %>% 
  ggplot(aes(x = el, y = mean))+
  geom_point(size = 2)+
  facet_grid(cols = vars(dmg_type))+
  xlim(1500,3000)+
  ylim(0,15)+
  geom_smooth(method = 'gam',
formula = y~s(x, k=4))+
  labs(title = "Mean damage percent x damage type",
y = "Mean percent damage",
x = "Elevation (m)")+
  theme_bw()



# GINI ####
 ?Gini

gini_df <- final_cp %>% 
  filter(!is.na(D1_p))

Gini(gini_df$D2_p)

# gini by elevation 

# average damage 
gini_df %>% 
  group_by(el) %>%
  mutate(avg_p = (D1_p + D2_p + D3_p + D6_p + D7_p)/5) %>% 
  relocate(el, avg_p) %>% 
  summarise(Gini = Gini(avg_p)) %>% 
ggplot(aes(el, Gini))+
  geom_point()+
  geom_smooth(method = 'lm')+
  theme_bw()+
  labs(title = "Gini of average damage", 
    x = "Elevation (m)", 
    y = "Gini")+
  theme(title = element_text(size = 20),
    axis.text = element_text(size = 16))


# pivot long to do all dmg 
dmg_gini <- gini_df %>% 
  pivot_longer(c("D1_p", "D2_p", "D3_p", "D6_p", "D7_p"),
    names_to = "dmg_type",
    values_to = "per") %>% 
  relocate(Date, loc, band, qid, rep, el, dmg_type, per) %>% 
  select(1:8) %>% 
  group_by(el, qid, dmg_type) %>% 
  summarise(gini = Gini(per, na.rm = TRUE))

ggplot(dmg_gini, aes(x = el, y = gini))+
  geom_point()+
  facet_wrap(~dmg_type) +
  geom_smooth(method = 'lm')+
  theme_bw()+
  labs(title = "Gini by dammage type",
    x = "Elevation (m)",
    y = "Gini")+
  theme(title = element_text(size = 20), 
    axis.text = element_text(size = 16))



# CODE WORKS, BUT moving on from this for now. Need to assess time and report back to this code 

# gini_test <- gini_df %>% 
#   group_by(qid) %>% 
#   summarise("Gini-chewing damage" = Gini(D1_p),
#     "Gini-piercing/sucking" = Gini(D2_p),
#     "Gini-adult beetle" = Gini(D3_p),
#     "Gini-beetle mines" = Gini(D6_p),
#     "Gini-fly mines" = Gini(D7_p))


# gini_fxn <- function(y_col, x_col, data) {

# ggplot(gini_test, aes(x = el, y = .data[[y_col]]))+
#   geom_point(size = 2)+
#   geom_smooth(method = 'gam', formula = y ~ s (x, k =4))+
#   theme_bw()+
#   labs(title = paste(y_col),
#     x = "Elevation (m)")+
#   theme(title = element_text(size = 18),
#    axis.text = element_text(size = 16) 
    
#     )
# }
  
# y_variables = c('Gini-chewing damage','Gini-piercing/sucking', 'Gini-adult beetle', 'Gini-beetle mines', 'Gini-fly mines')
# gini_dmg_type_plots <- map(y_variables, ~ gini_fxn(data = gini_test, x_col = el, y_col = .x))
# gini_dmg_type_plots[[1]]

# # Add in the raw dmg plots from the top
# raw_damage_plots


# raw_damage_plots[[1]]+gini_dmg_type_plots[[1]]+
#   plot_annotation(title = "Chewing damage percent and Gini index by elevation",
#     theme = theme(plot.title = element_text(size = 24)))

# raw_damage_plots[[2]]+gini_dmg_type_plots[[2]]+
#   plot_annotation(title = "Piercing/ sucking damage percent and Gini index by elevation",
#     theme = theme(plot.title = element_text(size = 24)))

# raw_damage_plots[[3]]+gini_dmg_type_plots[[3]]+
#   plot_annotation(title = "Adult beetle damage percent and Gini index by elevation",
#     theme = theme(plot.title = element_text(size = 24)))

# raw_damage_plots[[4]]+gini_dmg_type_plots[[4]]+
#   plot_annotation(title = "Beelte leaf miner damage percent and Gini index by elevation",
#     theme = theme(plot.title = element_text(size = 24)))

# raw_damage_plots[[5]]+gini_dmg_type_plots[[5]]+
#   plot_annotation(title = "Fly leaf miner damage percent and Gini index by elevation",
#     theme = theme(plot.title = element_text(size = 24)))

# Bin by time ####

dates_df <- as_tibble(unique(gini_df$Date))
dates_df %>% 
  arrange(value) %>% 
  print(n = Inf)
# 4,5,6,7,8

date_bin_df <- gini_df %>% 
  mutate(month_num = month(Date),
  date_bin = case_when(
    month_num %in% c(4) ~ "Early",
    month_num %in% c(5,6) ~ "Mid", 
    month_num %in% c(7,8) ~ "Late"
  )) %>% 
  group_by(qid, el, date_bin) %>% 
  summarise("Gini-chewing damage" = Gini(D1_p),
    "Gini-piercing/sucking" = Gini(D2_p),
    "Gini-adult beetle" = Gini(D3_p),
    "Gini-beetle mines" = Gini(D6_p),
    "Gini-fly mines" = Gini(D7_p)) %>% 
  mutate(date_bin = as.factor(date_bin),
    date_bin = fct_relevel(date_bin, "Early", "Mid", "Late")
)

# ggplot(date_bin_df, aes( x = el, y = "Gini-chewing damage"))+
#   facet_wrap(~date_bin)+
#   geom_point(size = 2)+
#   geom_smooth(method = 'gam', formula = y ~ s (x, k=4))


gini_date_fxn <- function(y_col, x_col, data) {

ggplot(date_bin_df, aes(x = el, y = .data[[y_col]]))+
  facet_wrap(~date_bin)+
  geom_point(size = 2)+
  geom_smooth(method = 'gam', formula = y ~ s (x, k =4))+
  theme_bw()+
  labs(title = paste(y_col),
    x = "Elevation (m)")+
  theme(title = element_text(size = 18),
   axis.text = element_text(size = 16),
   strip.text = element_text(size = 16)
    
    )+
  ylim(0,1)
}
  
y_variables = c('Gini-chewing damage','Gini-piercing/sucking', 'Gini-adult beetle', 'Gini-beetle mines', 'Gini-fly mines')
gini_date_plots <- map(y_variables, ~ gini_date_fxn(data = date_bin_df, x_col = el, y_col = .x))
gini_date_plots[[3]]




# sum by max dmg 

gini_df %>% 
  group_by(Tag) %>% 
  summarise(max_1 = max(D1_p),
    max_2 = max(D2_p),
    max_3 = max(D3_p),
    max_6 = max(D6_p), 
    max_7 = max(D7_p)) %>% 
  summarise(g1 = Gini(max_1, na.rm = TRUE),
    g2 = Gini(max_2, na.rm = TRUE),
    g3 = Gini(max_3, na.rm = TRUE),
    g6 = Gini(max_6, na.rm = TRUE),
    g7 = Gini(max_7, na.rm = TRUE))

