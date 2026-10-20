# packages ####
library(tidyverse)
library(ggridges)

# DFs ####
labeled_df
cleaned_df

# damage vis ####

colnames(labeled_df)
cleaned_df <- labeled_df %>%
  mutate(d6_splitter = case_when(
    D6_c.p == "0" ~ paste0("0",",",D6_c.p),
    .default = D6_c.p
  )) %>% 
  separate_wider_delim(
    cols = d6_splitter, 
    delim = ",", 
    names = c("D6_ct", "D6_p"),
    too_few = "debug"
  ) %>% 
  mutate(d7_splitter = case_when(
    D7_c.p == "0" ~ paste0("0", ",", D7_c.p),
    .default = D7_c.p
  )) %>% 
  separate_wider_delim(
    d7_splitter, 
    delim = ",", 
    names = c("D7_ct", "D7_p"),
    too_few = "debug"
  ) %>% 
  relocate(Date, loc, band, el, qid, rep, Tag, D1_p, D2_p, D3_p, D6_p, D7_p) %>% 
  mutate_at(8:12, as.numeric)

dmg_plots_raw <- function(data, x_col, y_col){

ggplot(cleaned_df, aes(x = el, y = .data[[y_col]]))+
  geom_smooth(method = 'gam', formula = y ~ s (x, k = 4))+
  geom_point()+
  theme_bw()+
  ylim(0,100)
}

y_variables = c('D1_p', 'D2_p', 'D3_p', 'D6_p', 'D7_p')


raw_damage_plots <- map(y_variables, ~ dmg_plots_raw(data = cleaned_df, x_col = el, y_col = .x))
raw_damage_plots[[1]]

walk2(y_variables, raw_damage_plots, 
  ~ggsave(filename = paste0("raw_plot_", .x, ".png"), plot = .y,
width = 8,
height = 8,
unit = 'in'))


dmg_plots_gam <- function(data, x_col, y_col){

  ggplot(cleaned_df, aes(x = el, y = .data[[y_col]]))+
    geom_smooth(method = 'gam', formula = y ~ s (x, k=4))+
    theme_bw()
    #labs(title = paste("Relationship between", x_col, "and", y_col))
}
y_variables = c('D1_p', 'D2_p', 'D3_p', 'D6_p', 'D7_p')


gam_damage_plots <- map(y_variables, ~ dmg_plots_gam(data = cleaned_df, x_col = el, y_col = .x))
gam_damage_plots

walk2(y_variables, gam_damage_plots, 
  ~ggsave(filename = paste0("gam_plot_", .x, ".png"), plot = .y,
width = 8,
height = 8,
unit = 'in'))

# plot of average damage type 

wide_sum <- cleaned_df %>% 
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
df_for_avg_plot <- cleaned_df %>% 
  group_by(loc, band, el) %>% 
  select(1:12) %>% 
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

cleaned_df
wide_sum

long_df <- cleaned_df %>% 
  group_by(loc, band, el) %>% 
  select(1:12) %>% 
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

long_mean_df <- cleaned_df %>% 
  group_by(loc, band, el) %>% 
  select(1:12) %>% 
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
cleaned_df %>% # quick vis with the wide df, switching to long
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

cleaned_df %>% # just looking at the spread
  group_by(el) %>% 
  summarise(max = max(el, na.rm = TRUE)) %>% 
  print(n = Inf)

max_value_df <- cleaned_df %>% # new df for the max values 
  group_by(Tag, el, loc, band) %>% 
  summarise(D1_value = max(D1_p, na.rm = TRUE),
D2_value = max(D2_p, na.rm = TRUE),
D3_value = max(D3_p, na.rm = TRUE),
D6_value = max(D6_p, na.rm = TRUE),
D7_value = max(D7_p, na.rm = TRUE)) %>%
  ungroup() %>% 
  na.omit() %>% 
  pivot_longer(
    c('D1_value', 'D2_value', 'D3_value', 'D6_value', 'D7_value'),
    names_to = 'dmg_type',
    values_to = 'max_p'
  ) %>% 
  mutate(Tag = case_when(
    Tag == " " ~ 'X', .default = Tag
  )) %>% 
  mutate(Tag = as.factor(Tag))  

# in the above math there is an issue somewhere that is causing some -Inf values. IDK why
max_value_df %>% 
  filter(max_p == -Inf)

e_df <- max_value_df %>% 
  group_by(band, el, dmg_type) %>% 
  filter(max_p != -Inf) %>% 
  summarise(mean = mean(max_p, na.rm = TRUE)) 

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

