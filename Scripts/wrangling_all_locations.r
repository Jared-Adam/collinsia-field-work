# script for all data wrangling and some brief exploration 

# packages ####
library(tidyverse)

# data ####
mica_creek_df <- X2025_7_15_MC_field_data_online_entry
truman_df <- X2025_7_22_TG_field_data_online_entry
rw_df <- X2025_7_22_RW_field_data_online_entry

# Mica Creek ####

# wrangling 

mc_prop_df <- mica_creek_df
mc_prop_df[mc_prop_df =='na'] <- NA

# collect the means 
mc_prelim_fxn_df <- mc_prop_df %>% 
  rename(suck_mite_p = `suck-mite_p`,
         suck_thrips_p = `suck-thrips_p`) %>% 
  select(Site, Quadrat, Date, D1_p, D2_p, suck_mite_p, suck_thrips_p, D5_p, D4_p, D6_p,
         D6_ct, PH, PD, FlC, FrC) %>% 
  mutate_at(vars(4:15), as.numeric) %>% 
  rename(q = Quadrat,
         date = Date) %>% 
  mutate(elevation = case_when(Site == 1 ~ '6100',
                               Site == 2 ~ '6700',
                               Site == 3 ~ '7300',
                               Site == 4 ~ '8200',
                               Site == 5 ~ '8800',
                               Site == 6 ~ '9100')) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y")) %>% 
  relocate(elevation) %>% 
  mutate_at(vars(1:3), as.factor) %>% 
  filter(D1_p != 'NA') %>% 
  group_by(elevation, q) %>% 
  summarise(
    edge_nmv_m = mean(D1_p), 
    edge_mv_m = mean(D2_p), 
    suckm = mean(suck_mite_p),
    thripm = mean(suck_thrips_p),
    scape_m = mean(D5_p), 
    hole_m = mean(D4_p), 
    mine_m = mean(D6_p),
    mine.ctm = mean(D6_ct),
    phm = mean(PH, na.rm = TRUE),
    pdm = mean(PD, na.rm = TRUE),
    flower.ctm = mean(FlC),
    fruit.ctm = mean(FrC)) %>% 
  print(N = inf)


# raw data cleaning for jitter plots
# now for the whole df and I will plot the means. 
# I think I will need to group by elevation and q still, if possible

mc_raw_df <- mc_prop_df %>% 
  rename(suck_mite_p = `suck-mite_p`,
         suck_thrips_p = `suck-thrips_p`,
         q = Quadrat) %>% 
  select(Site, q, Date, D1_p, D2_p, suck_mite_p, suck_thrips_p, D5_p, D4_p, D6_p,
         D6_ct, PH, PD, FlC, FrC) %>% 
  mutate_at(vars(4:15), as.numeric) %>% 
  mutate(elevation = case_when(Site == 1 ~ '6100',
                               Site == 2 ~ '6700',
                               Site == 3 ~ '7300',
                               Site == 4 ~ '8200',
                               Site == 5 ~ '8800',
                               Site == 6 ~ '9100')) %>% 
  relocate(elevation) %>% 
  select(!c(Site, Date,suck_mite_p, suck_thrips_p, D5_p)) %>% 
  mutate_at(vars(1:2), as.factor)%>% 
  filter(D1_p != 'NA') 

ggplot(mc_raw_df, aes(x = elevation, y = D1_p))+
  geom_point()+
  stat_summary(fun = 'mean', color = 'red', geom = 'point', size = 5)+
  facet_wrap(~q)

# looking to pivot long, ignore for now

# test <- prelim_fxn_df %>% 
#   pivot_longer(
#     cols = edge_nmv_m:fruit.ctm,
#     names_to = 'variables',
#     values_to = 'mean'
#   ) %>% 
#   print(N = inf)
# 
# test %>% 
#   filter(variables != 'suckm' & variables != 'thripm' & variables != 'scape_m') %>% 
#   ggplot(aes(x = site, y = mean, color = q, shape = q))+
#   geom_point(size = 4)+
#   facet_grid(~ variables, labeller = labeller(q = q.labs))



# Truman Gulch ####



# early wrangle 

# pulling d6 out

truman_df
tg_mean_df <- truman_df
tg_mean_df[tg_mean_df == 'na'] <- NA
colnames(tg_mean_df)
tg_prelim_fx_df <- tg_mean_df %>% 
  filter(D1_p != 'NA') %>% 
  rename(suck_mite_p = 'suck-mite_p',
         q = Quadrat) %>% 
  mutate(elevation = case_when(Site == 1 ~ '5400',
                               Site == 2 ~ '6100',
                               Site == 3 ~ '6900',
                               Site == 4 ~ '7600',
                               Site == 5 ~ '8360')) %>% 
  select(q, elevation, D1_p, D2_p, suck_mite_p, D5_p, D4_p, D6_p,
         D6_ct, PH, PD, FlC, FrC) %>%
  mutate_at(vars(2:13), as.numeric) %>% 
  mutate(elevation = as.factor(elevation),
         q = as.factor(q)) %>% 
  group_by(elevation,q) %>% 
  summarise(
    edge_no_mv = mean(D1_p),
    edge_mv = mean(D2_p),
    suckm = mean(suck_mite_p),
    scrape = mean(D5_p),
    hole = mean(D4_p),
    mine = mean(D6_p),
    mine_ct = mean(D6_ct),
    phm = mean(PH, na.rm = TRUE),
    pdm = mean(PD, na.rm = TRUE),
    flower.ctm = mean(FlC),
    fruit.ctm = mean(FrC)
  )

## 

# raw data for mean over jitter
colnames(tg_mean_df)

tg_raw_df <- tg_mean_df %>% 
  rename(suck_mite_p = 'suck-mite_p',
         EFN_ct = 'EFN ct',
         q = Quadrat
  ) %>% 
  filter(D1_p != 'NA') %>% 
  mutate(elevation = case_when(Site == 1 ~ '5400',
                               Site == 2 ~ '6100',
                               Site == 3 ~ '6900',
                               Site == 4 ~ '7600',
                               Site == 5 ~ '8360')) %>% 
  select(Site, q, Date, D1_p, D2_p,D4_p, D6_p,
         D6_ct, PH, PD, FlC, FrC, elevation) %>% 
  relocate(elevation) %>% 
  mutate_at(vars(1:4), as.factor) %>% 
  mutate_at(vars(5:13), as.numeric)


# Retirement Wall ####


# wrangle  
rw_df
rw_mean_df <- rw_df
rw_mean_df[rw_mean_df == 'na'] <- NA

# removed damage 6 bc the NAs were causing a fuss. Will plot that damage indep of the loop
colnames(rw_mean_df)
rw_prelim_fxn_df <- rw_mean_df %>% 
  rename(suck_mite_p = 'suck-mite_p',
         q = Quadrat) %>% 
  filter(D1_p != 'NA') %>% 
  mutate(elevation = case_when(Site == 1 ~ '5600',
                               Site == 2 ~ '6050',
                               Site == 3 ~ '6700',
                               Site == 4 ~ '6900',
                               Site == 5 ~ '7300')) %>% 
  select(elevation, q, D1_p, D2_p, suck_mite_p, D5_p, D4_p, D6_p, D6_ct, PH, PD, FlC, FrC) %>% 
  mutate_at(vars(3:13), as.numeric) %>% 
  mutate_at(vars(1:2), as.factor) %>% 
  mutate(elevation = as.character(elevation)) %>% 
  group_by(elevation, q) %>% 
  summarise(
    edge_no_mv = mean(D1_p), 
    edge_mv = mean(D2_p), 
    suckm = mean(suck_mite_p),
    scape = mean(D5_p), 
    hole = mean(D4_p),
    mine = mean(D6_p),
    mine_ct = mean(D6_ct),
    height_m = mean(PH), 
    diam_m = mean(PD),
    flower_ct = mean(FlC),
    fruit_ct = mean(FrC)
  ) %>% 
  print(n = Inf)
rw_prelim_fxn_df

##

# raw values for jitter 

colnames(rw_mean_df)

rw_raw_df <- rw_mean_df %>% 
  rename(suck_mite_p = 'suck-mite_p',
         q = Quadrat
  ) %>% 
  filter(D1_p != 'NA') %>% 
  mutate(elevation = case_when(Site == 1 ~ '5600',
                               Site == 2 ~ '6050',
                               Site == 3 ~ '6700',
                               Site == 4 ~ '6900',
                               Site == 5 ~ '7300')) %>% 
  select(Site, q, Date, D1_p, D2_p,D4_p,D6_p, D6_ct, 
         PH, PD, FlC, FrC, elevation) %>% 
  relocate(elevation) %>% 
  mutate_at(vars(1:4), as.factor) %>% 
  mutate_at(vars(5:13), as.numeric)


# Merging all RAW dfs ####

rw_raw_df
tg_raw_df
mc_raw_df

tg_merge <- tg_raw_df %>% 
  select(!c(Site, Date)) %>% 
  mutate(loc = 'TG')

rw_merge <- rw_raw_df %>% 
  select(!c(Site, Date)) %>% 
  mutate(loc = 'RW')

mc_merge <- mc_raw_df %>% 
  mutate(loc = 'MC')


merged_df <- rbind(rw_merge, tg_merge, mc_merge) %>% 
  relocate(loc) %>% 
  mutate(loc = as.factor(loc)) %>% 
  print(n = 10)


