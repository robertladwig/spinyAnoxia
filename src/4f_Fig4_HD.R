# Package ID: knb-lter-ntl.88.30 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: Phytoplankton - Madison Lakes Area 1995 - current.
# Data set creator:  John Magnuson - University of Wisconsin 
# Data set creator:  Stephen Carpenter - University of Wisconsin 
# Data set creator:  Emily Stanley - University of Wisconsin 
# Contact:  NTL Information Manager -  University of Wisconsin  - ntl.infomgr@gmail.com
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

library(lubridate)
library(tidyverse)

# download ice data
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/33/35/f5bc02452cafcd461c49bd7429d8b40c" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
ice.df <-read_csv(infile1)

ice.df2 = ice.df |> filter(lakeid == 'ME') |> 
  mutate(ice_on = ymd(ice_on), ice_off = ymd(ice_off)) |> 
  select(year4, ice_on, ice_off) |> 
  filter(year4 >= 1994) |> 
  mutate(year4 = year(ice_off))

# download phyto data
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/88/30/f2de15b2fff6ae962a04c150c0a1c510" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))

phyto.df <- read_csv(infile1) |> 
  filter(lakeid == 'ME') |> 
  filter(cells_per_ml > 0) |> 
  select(year4, sampledate, depth_range, division, biomass_conc) |> 
  mutate(division = if_else(division %in% c('Euglenophyta','Haptophyta','Miscellaneous','Xanthophyta'), 'other', division))

# load stratification data
straton = read_csv('data_processed/stratification_start.csv') |> 
  mutate(straton = `year<-`(linear, year)) |> 
  select(year4 = year, straton)
stratoff = read_csv('data_processed/stratification_end.csv') |> 
  mutate(stratoff = `year<-`(linear, year)) |> 
  select(year4 = year, stratoff)

# Join data sets
phyto.join = phyto.df |> 
  group_by(sampledate, year4, depth_range, division) |> 
  summarise(biomass_conc = sum(biomass_conc, na.rm = T)) |> 
  left_join(ice.df2) |> 
  left_join(straton) |> 
  left_join(stratoff) |> 
  mutate(season = case_when(sampledate < ice_off ~ 'ice',
                            sampledate > ice_off & sampledate < straton ~ 'spring',
                            sampledate > straton & sampledate < stratoff ~ 'stratified',
                            sampledate > stratoff ~ 'fall')) |> 
  mutate(season = factor(season, levels = c('ice', 'spring', 'stratified', 'fall'))) |> 
  mutate(division = factor(division, levels = c('Bacillariophyta','Cryptophyta','Cyanophyta','Chlorophyta',
                                                'Chrysophyta','Pyrrhophyta','other'))) |> 
  group_by(season, year4, depth_range, division) |> 
  summarise(biomass_conc = mean(biomass_conc, na.rm = T)) |>
  # mutate(division = fct_relevel(division, "other", after = Inf)) |> 
  filter(!is.na(season))
  
season.names = c('ice' = 'Ice', 'spring' = 'Spring Mixed','stratified' = 'Stratified','fall' = 'Fall Mixed')

ggplot(phyto.join |> filter (depth_range == '0-8m', year4 > 1994)) +
  geom_col(aes(x = year4, y = biomass_conc, fill = division), width = 0.8) +
  geom_vline(aes(xintercept = 2009.5), linetype = 2) +
  scale_fill_manual(values = c('#9a713e', '#f1635c', '#4ea5d7', '#76b88a', '#ffa98f', '#c6eb55', 'grey50'), name = 'Division') +
  facet_wrap(~season, ncol = 1, labeller = as_labeller(season.names) ) +
  ylab("Mean Phytoplankton Biomass"~(mg~L^-1)) +
  theme_bw(base_size = 9) +
  theme(axis.title.x = element_blank(), 
        strip.background = element_blank(),
        strip.text = element_text(size = 9, face = "bold", hjust = 0, margin = margin(0, 0, 0, 0, "pt")))
  
ggsave('figs_publication/Fig4_HD.png', dpi = 500, units = 'in', width = 6.5, height = 5)
