library(tidyverse)
library(ggpubr)
library(rstatix)
library(lubridate)

# Alternative Figure 3 boxplot with points 
anoxia <- read_csv(file = "data_processed/timelag.csv") |> 
  mutate(group = if_else(year < 2010, 'Pre','Post')) |> 
  mutate(group = factor(group, levels = c('Pre','Post')))

stat.test <- anoxia %>%
  wilcox_test(timelag ~ group, paired = FALSE) %>%
  add_significance("p")
anoxia.stat = stat.test |> add_y_position() |> mutate(y.position = y.position + 3)

phyto <- readRDS("data_processed/3g_biomass_metrics_by_season-measured_values.rds") |> 
  pluck("mean") |> 
  bind_rows(.id = "season") |> 
  mutate(season = factor(season, levels = c('ice', 'spring', 'stratified', 'fall'))) |> 
  mutate(group = if_else(Year < 2010, 'Pre','Post')) |> 
  mutate(group = factor(group, levels = c('Pre','Post')))

stat.test <- phyto %>%
  group_by(season) %>%
  wilcox_test(Ave.Daily.mg.L ~ group, paired = FALSE) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")
  # add_significance("p")
  
phyto.stat = stat.test |> add_xy_position(x = "season", dodge = 0.8)

p1 = ggplot(phyto) + 
  geom_boxplot(aes(x = season, y = Ave.Daily.mg.L, fill = group), size = 0.2) +
  geom_point(aes(x = season, y = Ave.Daily.mg.L, fill = group), shape = 21, size = 2, position=position_dodge(width=0.75)) +
  scale_fill_manual(values = c('steelblue','orange3')) +
  scale_x_discrete(labels = c('Ice', 'Spring\nMixed','Stratified','Fall\nMixed')) +
  stat_pvalue_manual(phyto.stat, label = "{p.adj}{p.adj.signif}", hide.ns = TRUE, 
                     tip.length = 0.01, remove.bracket = FALSE, size = 2) +
  scale_y_continuous(expand = expansion(mult = c(0.03, 0.1))) +
  ylab("Phytoplankton Biomass"~(mg~L^-1)) +
  theme_bw(base_size = 9) +
  theme(axis.title.x = element_blank(), 
        legend.position = c(0.1,0.8),
        legend.key.size = unit(0.2, "cm"),
        legend.title = element_blank()); p1
  
p2 = ggplot(anoxia) + 
  geom_boxplot(aes(x = group, y = timelag, fill = group), size = 0.2) +
  geom_point(aes(x = group, y = timelag, fill = group), shape = 21, size = 2, position=position_dodge(width=0.75)) +
  scale_fill_manual(values = c('steelblue','orange3')) +
  stat_pvalue_manual(anoxia.stat, label = "{p}{p.signif}", tip.length = 0.01, remove.bracket = FALSE, size = 2) +
  scale_y_continuous(expand = expansion(mult = c(0.03, 0.1))) +
  ylab("Lag between stratfication \nand anoxia (days)") +
  theme_bw(base_size = 9) +
  theme(axis.title.x = element_blank(), 
        legend.position = "none",
        legend.title = element_blank()); p2

# Final figure
p1 + p2 + plot_layout(widths = c(3,1)) +
  plot_annotation(tag_levels = 'A', tag_suffix = ')') & 
  theme(plot.tag = element_text(size  = 8))
# ggsave('figs_publication/Fig3_HD.png', dpi = 500, units = 'in', width = 6.5, height = 3)

### Phytoplankton Figure #####
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

p3 = ggplot(phyto.join |> filter (depth_range == '0-8m', year4 > 1994)) +
  geom_col(aes(x = year4, y = biomass_conc, fill = division), width = 0.8) +
  geom_vline(aes(xintercept = 2009.5), linetype = 2) +
  scale_fill_manual(values = c('#9a713e', '#f1635c', '#4ea5d7', '#76b88a', '#ffa98f', '#c6eb55', 'grey50'), name = 'Division') +
  facet_wrap(~season, ncol = 2, labeller = as_labeller(season.names) ) +
  ylab("Mean Phytoplankton Biomass"~(mg~L^-1)) +
  theme_bw(base_size = 9) +
  theme(axis.title.x = element_blank(),
        legend.position = 'bottom',
        strip.background = element_blank(),
        legend.key.size = unit(0.2, "cm"),
        legend.title = element_blank(),
        strip.text = element_text(size = 9, face = "bold", hjust = 0, margin = margin(0, 0, 0, 0, "pt"))) +
  guides(fill = guide_legend(nrow = 1))

layout <- "
AAAB
AAAB
CCCC
CCCC
CCCC
"

p1 + p2 + p3 + plot_layout(design = layout) +
  plot_annotation(tag_levels = 'A', tag_suffix = ')') &
  theme(plot.tag = element_text(size  = 8))
ggsave('figs_publication/Fig3_Combo_HD.png', dpi = 500, units = 'in', width = 6.5, height = 5.5)

