library(tidyverse)
library(ggpubr)
library(rstatix)
library(patchwork)

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
                     tip.length = 0.01, remove.bracket = FALSE, size = 2.3) +
  scale_y_continuous(expand = expansion(mult = c(0.03, 0.1))) +
  ylab("Phytoplankton Biomass"~(mg~L^-1)) +
  theme_bw(base_size = 9) +
  theme(axis.title.x = element_blank(),
        legend.background=element_blank(),
        legend.position = c(0.1,0.8),
        legend.title = element_blank()); p1
  
p2 = ggplot(anoxia) + 
  geom_boxplot(aes(x = group, y = timelag, fill = group), size = 0.2) +
  geom_point(aes(x = group, y = timelag, fill = group), shape = 21, size = 2, position=position_dodge(width=0.75)) +
  scale_fill_manual(values = c('steelblue','orange3')) +
  stat_pvalue_manual(anoxia.stat, label = "{p}{p.signif}", tip.length = 0.01, remove.bracket = FALSE, size = 2.3) +
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
ggsave('figs_publication/Fig3_HD.png', dpi = 500, units = 'in', width = 6, height = 2.5)


