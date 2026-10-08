# packages ####
library(here)
library(tidyverse)
library(ggridges)


# data ####
cp.dir <- here('Data/2026', '2026-5-26_Cp_obs.csv')

cp = as_tibble(read.csv(cp.dir))

# cleaning ####

cp

cp %>% 
  select(-21, -22, -23, -24, -25, -26) %>% 
   slice(1:100) %>% 
  mutate(test =
           case_when(
             EFN.score == "1" ~ paste0("0", ",", EFN.ct),
             EFN.score == "2" ~ paste0(EFN.ct, ",", "0"),
             EFN.ct == "0" ~ paste0("0", ",", "0"),
             .default = EFN.ct
           )
  ) %>% 
  separate_wider_delim(
    cols = test,
    delim = ",",
    names = c("EFN_sc_2", "EFN_sc_1")) %>% 
  select(-c(18,19)) %>% 
  mutate(across(
    .cols = c(5:16, 18:20),
    .fns = ~case_when(
      D1_p == 'dead' ~ NA,
      TRUE ~ .x
    )
  )) %>%
  mutate(D1_p = case_when(
    D1_p == "Sen" | D1_p == 'sen' ~ '0', 
    .default = D1_p
  )) 


# pick up here, this is not working. Why?
# there are several rows with nonsense. some dots, some unsre
# starting with removal of dots

cp %>% 
  filter(str_detect(EFN.ct, "\\.")) %>% # dots are wildcards, so must be preceded with \\
  mutate(EFN.ct = str_replace_all(EFN.ct, "\\.", ","))



debug <- cp %>% 
  select(-21, -22, -23, -24, -25, -26) %>%
  mutate(EFN.ct = str_replace_all(EFN.ct, "\\.", ",")) %>% 
  mutate(test =
           case_when(
             EFN.score == "1" ~ paste0("0", ",", EFN.ct),
             EFN.score == "2" ~ paste0(EFN.ct, ",", "0"),
             EFN.ct == "0" ~ paste0("0", ",", "0"),
             .default = EFN.ct
           )
  ) %>% 
  separate_wider_delim(
    test, 
    delim = ",", 
    names = c("EFN_sc_2", "EFN_sc_1"),
    too_few = "debug"
  ) %>% 
  filter(test_ok == TRUE) # removing the confusing rows for now

# 10/6/2026:
  # these do not work becuase the score column is erroneous 
  # ct has a value >0 but the score is incorrect. These must be found in the data sheets

look <- debug %>% filter(!test_ok) %>% 
  relocate(EFN.ct, EFN.score, test) %>% 
  print(n = Inf)


# now getting rid of the NAs as they are not pertinent to EFNs
next_df <- debug

###
##
#
# DROPPING NA FOR NOW (10/6/2026)
again_df <- next_df %>% 
  mutate(across(where(is.character), ~na_if(., "na"))) %>% 
  drop_na()
#
##
###


# add elevation ####
# going to add a band column and assign that by qid and then add elev ~ band

labeled_df <- again_df %>% 
  rename(id = Qdrat.ID) %>% 
  mutate(band = case_when(
    id == "813" | id == '838' | id == "794" | id == "71" | id == "893" | id == "891" | id == "914" | id == "945" | id == "898" ~ "1",
    id == "883" | id == "34" | id == "15" | id == "76" | id == "80" | id == "6" | id == " 457" | id == "416" | id == "458" ~ "2",
    id == "3" | id == "29" | id == "433"| id == "96" | id == "38"| id == "94"| id == "667"| id == "815" | id == "610" ~ "3",
    id == "320"| id == "387" | id == "302" | id == "623" | id == "639" | id == "687" | id == "259" | id == "258" | id == "285" ~ "4",
    id == "311" | id == "319" | id == "347" | id == "574" | id == "695" | id == "636" | id == "218" | id == "240" | id == "227" ~ "5",
    id == "652" | id == "989" | id == "626" | id == "674" | id == "660" | id == "683" | id == "634" | id == "611" | id == "609" ~ "6",
    id == "696" | id == "670" | id == "899" | id == "624" | id == "672" | id == "632" | id == "614" | id == "629" | id == "642" ~ "7",
    id == "699" | id == "98" | id == "640" | id == "698" | id == "697" | id == "604" | id == "619" | id == "691" | id == "650" ~ "8",
    id == "655" | id == "700" | id == "41" | id == "620" | id == "78" | id == "36" | id == "263" | id == "17" | id == "730" ~ "9",
    id == "63" | id == "905" | id == "105" | id == "88" | id == "49" | id == "637" | id == "27" | id == "13" | id == "81" ~ "10",
    .default = as.character(id)
  )) %>% 
  mutate(loc = case_when(
    id == "813" | id == '838' | id == "794" | id == "883" | id == "34" | id == "15" | id == "3" | id == "29" | id == "433" | id == "320"| id == "387" | id == "302" |
      id == "311" | id == "319" | id == "347" | id == "652" | id == "989" | id == "626" | id == "696" | id == "670" | id == "899" | id == "699" | id == "98" | id == "640" | 
      id == "655" | id == "700" | id == "41" | id == "63" | id == "905" | id == "105" | id == "698" | id == "697" | id == "604" | id == "620" | id == "78" | id == "36" ~ "IR",
    id == "71" | id == "893" | id == "891" | id == "76" | id == "80" | id == "6" | id == "96" | id == "38"| id == "94"| id == "623" | id == "639" | id == "687" |
      id == "574" | id == "695" | id == "636" | id == "674" | id == "660" | id == "683" | id == "624" | id == "672" | id == "632" ~ "HR",
    id == "914" | id == "945" | id == "898" | id == " 457" | id == "416" | id == "458" | id == "667"| id == "815" | id == "610" | id == "259" | id == "258" | id == "285" |
      id == "218" | id == "240" | id == "227" | id == "634" | id == "611" | id == "609" | id == "614" | id == "629" | id == "642" | id == "619" | id == "691" | id == "650"|
       id == "263" | id == "17" | id == "730" | id == "88" | id == "49" | id == "637" | id == "27" | id == "13" | id == "81" ~ "MC",
    .default = as.character(id)
  )) %>% 
  mutate(rep = case_when(
    id == "699" | id == " 98" | id == "640" | id == "655" | id == "700" | id == "41" | id == "88" | id == "49" | id == "637" ~ "1",
    id == "27" | id == "13" | id == "81" | id == "687" | id == "697" | id == "604" | id == "620" | id == "78" | id == "36" ~ "2",
    .default = NULL
  )) %>% 
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
  mutate(qid = as.factor(id)) %>% 
  relocate(Date, loc, band, el, qid, rep) %>% 
  select(-7)

###
##
# 
# This is not done! There are 6 instances of missing plots, come back to this 
# ALSO opne instance of a messed up IR
# 
##
###

# A tibble: 36 × 8
# Groups:   loc, band [33]
#    loc   band     el   D1_p   D2_p  D3_p    D6_p    D7_p
#    <chr> <chr> <dbl>  <dbl>  <dbl> <dbl>   <dbl>   <dbl>
#  1 457   457      NA 17.8   0.0600 0.371 0       0.135  
#  2 62    62       NA  3.80  3.49   4.01  0.455   0      
#  3 654   654      NA  0     0      0.633 0       0      
#  4 668   668      NA  5.63  0      1.78  0       0      
#  5 69    69       NA  0     0      0     0       0      
#  6 910   910      NA  0     0      3.05  0       0 

# EFN vis ####
labeled_df

labeled_df %>% 
  mutate(EFN.ct = as.numeric(EFN.ct)) %>% 
  ggplot(aes(el, EFN.ct))+
  geom_smooth(method = 'gam',
formula = y~s (x, k = 4))+
  geom_point()


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

# The x axis is cluttered with all the elevations, going to write a function to remove from other facets

# show_one_facet <- function(x){
#   if(max(x, na.rm = TRUE)>3000){
#     return(scales::label_extended()(x))
#   }else{
#     return(rep("", length(x)))
#   }
# }

# show_one_facet <- function(x){
#   facet_counter <- facet_counter+1

#   if(facet_counter ==2) {
#     return(scales::label_extended()(x))
#   } else{
#     return(rep("", length(x)))
#   }
#   }

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
