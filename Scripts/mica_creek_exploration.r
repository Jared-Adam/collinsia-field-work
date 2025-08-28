# Packages ####
library(tidyverse)
library(ggpubr)

# data ####
mica_creek_df <- X2025_7_15_MC_field_data_online_entry

# comma separated attempt at wrangling ####

colnames(mica_creek_df)
mica_creek_df <- mica_creek_df %>% 
  rename(suck_mite_p = 'suck-mite_p',
         suck_mite_n = 'suck-mite_n3',
         suck_thrips_p = 'suck-thrips_p',
         suck_thrips_n = 'suck-thrips_n')

# the below code is unresolved 7/31/2025
# test to see the best way to split the node columns, BUT this does not work if the values match 
mc_test_df <- mica_creek_df[1:100,]
mc_test_df %>% 
  separate_rows(c(Dn1, Dn2, suck_mite_n, suck_thrips_n, Dn4, Dn5, Dn6), sep = ",")

# adding this to a new df 
mica_creek_df %>% 
  separate_rows(c(Dn1, Dn2, suck_mite_n, suck_thrips_n, Dn4, Dn5, Dn6), sep = ",")

mc_error <- mica_creek_df[188,]
mc_error %>% 
  separate_rows(c(Dn1, Dn2), sep = ",")


# exploration ####

no_node_mc <- mica_creek_df %>% 
  select(!c(Dn1, Dn2, suck_mite_n, suck_thrips_n, Dn5, Dn4, Dn6)) %>% 
  mutate_at(vars(6:20), as.numeric) %>% 
  mutate_at(vars(1,3,4,5), as.factor)

ggplot(no_node_mc, aes(y = D2_p, x = Site))+
  geom_point()+
  geom_abline()



# wrangling #### 


mc_prop_df <- mica_creek_df
mc_prop_df[mc_prop_df =='na'] <- NA

# collect the means 
prelim_fxn_df <- mc_prop_df %>% 
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

raw_df <- mc_prop_df %>% 
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

ggplot(raw_df, aes(x = elevation, y = D1_p))+
  geom_point()+
  stat_summary(fun = 'mean', color = 'red', geom = 'point', size = 5)+
  facet_wrap(~q)
  
  

# looking to pivot long, ignore for now

test <- prelim_fxn_df %>% 
  pivot_longer(
    cols = edge_nmv_m:fruit.ctm,
    names_to = 'variables',
    values_to = 'mean'
  ) %>% 
  print(N = inf)

test %>% 
  filter(variables != 'suckm' & variables != 'thripm' & variables != 'scape_m') %>% 
ggplot(aes(x = site, y = mean, color = q, shape = q))+
  geom_point(size = 4)+
  facet_grid(~ variables, labeller = labeller(q = q.labs))

?select

# plotting all variables ####

dmg_exp_var <- names(prelim_fxn_df[1])
dmg_exp_var <- set_names(dmg_exp_var)

dmg_resp_var <- names(prelim_fxn_df[3:14])
dmg_resp_var <-set_names(dmg_resp_var)

q.labs <- c("Sun", "Shade")
names(q.labs) <- c('1', '2')

dot_plots <- function(x,y){
  ggplot(prelim_fxn_df, aes(x = .data[[x]], y = .data[[y]]))+
    geom_point(aes(size = 3, color = q))+
    facet_wrap(~q, 
               labeller = labeller(q = q.labs))+
    ylim(0,NA)+
    theme_bw()+
    theme(
      legend.position = 'none',
      axis.text = element_text(size = 8)
    )+
    labs(title = 'Mica Creek')
}

plo1 <- map(dmg_resp_var,
            ~map(dmg_exp_var, dot_plots, y = .x))
plo1_list <- map(plo1 ,~cowplot::plot_grid(plotlist = .x))
ggarrange(plotlist = plo1_list)


##

## plots for the raw data with mean and jitter ####

raw_exp_var <- names(raw_df[1])
raw_exp_var <- set_names(raw_exp_var)

raw_resp_var <- names(raw_df[3:11])
raw_resp_var <- set_names(raw_resp_var)

q.labs <- c("Sun", "Shade")
names(q.labs) <- c('1', '2')

raw_plot <- function(x,y){
  ggplot(raw_df, aes(x = .data[[x]], y = .data[[y]]))+
    geom_point(aes(color = q))+
    facet_wrap(~q, 
               labeller = labeller(q = q.labs))+
    stat_summary(fun = 'mean', color = 'red', size = 5, geom = 'point')+
    ylim(0,NA)+
    theme_bw()+
    theme(
      legend.position = 'none',
      axis.text = element_text(size = 8)
    )+
    labs(title = 'Mica Creek')
    
}


raw_plo <- map(raw_resp_var,
            ~map(raw_exp_var, raw_plot, y = .x))
raw_plo_list <- map(raw_plo ,~cowplot::plot_grid(plotlist = .x))
ggarrange(plotlist = raw_plo_list)

##


# misc ####

# fitness_exp_var <- names(prelim_fxn_df[1])
# fitness_exp_var <- set_names(fitness_exp_var)
# 
# fitness_resp_var <- names(prelim_fxn_df[3:7])
# fitness_resp_var <-set_names(fitness_resp_var)

# exploring the variables that are not damage ####

colnames(mc_prop_df)
fe_df <- mc_prop_df %>% 
  select(Site, Date, Quadrat, D6_ct, PH, PD, FlC, FrC) %>% 
  mutate_at(vars(3:8), as.numeric) %>% 
  rename(date = Date,
         site = Site,
         q = Quadrat) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"),
         site = as.factor(site),
         q = as.factor(q)) %>% 
  filter(D6_ct != 'NA',) %>% 
  group_by(site, q) %>% 
  summarise(
    mine.ctm = mean(D6_ct),
    phm = mean(PH, na.rm = TRUE),
    pdm = mean(PD, na.rm = TRUE),
    flower.ctm = mean(FlC),
    fruit.ctm = mean(FrC)) %>% 
  print(N = inf)


q.labs <- c("Sun", "Shade")
names(q.labs) <- c('1', '2')

ggplot(fe_df, aes(x = site, y = pdm))+
  geom_point()+
  ylim(0, NA)+
  facet_wrap(~q, 
             labeller = labeller(q = q.labs))
  

exp_var <- names(fe_df[1])
exp_var <- set_names(exp_var)

resp_var <- names(fe_df[3:7])
resp_var <-set_names(resp_var)

dot_plots <- function(x,y){
  ggplot(fe_df, aes(x = .data[[x]], y = .data[[y]]))+
    geom_point(aes(size = 3, color = q))+
    facet_wrap(~q, 
               labeller = labeller(q = q.labs))+
    ylim(0,NA)+
    theme_bw()+
    theme(
      legend.position = 'none',
      axis.text = element_text(size = 14)
    )+
    labs(title = 'Mica Creek')
}


mc_xtra_plots <- map(resp_var,
            ~map(exp_var, dot_plots, y = .x))
mc_xtra_plots_list <- map(mc_xtra_plots ,~cowplot::plot_grid(plotlist = .x))
ggarrange(plotlist = mc_xtra_plots_list)



?ylim







mc_xtra_plots <- map(fitness_resp_var,
            ~map(fitness_exp_var, dot_plots, y = .x))
mc_xtra_plots_list <- map(mc_xtra_plots ,~cowplot::plot_grid(plotlist = .x))
ggarrange(plotlist = mc_xtra_plots_list)
