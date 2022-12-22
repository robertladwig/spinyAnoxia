library(tidyverse)
library(ggpubr)
library(rstatix)
library(patchwork)

col.pre <- adjustcolor("steelblue",.9)
col.post <- "orange3"

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


# ---- make plot ----

p1 = ggplot(phyto) + 
  geom_boxplot(aes(x = season, y = Ave.Daily.mg.L, fill = group), size = 0.2, show.legend = F, outlier.shape = NA, color = "black", linewidth = .5) +
  geom_point(aes(x = season, y = Ave.Daily.mg.L, fill = group), shape = 21, size = 1, position=position_jitterdodge(jitter.width = .5, jitter.height = 0, dodge.width = .75), show.legend = T) +
  scale_fill_manual(values = c(col.pre,col.post)) +
  scale_x_discrete(labels = c('Ice', 'Spring\nMixed','Stratified','Fall\nMixed')) +
  stat_pvalue_manual(phyto.stat, label = "{p.adj.signif}", hide.ns = TRUE, 
                     tip.length = 0.01, remove.bracket = FALSE, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0.03, 0.1))) +
  ylab("Phytoplankton Biomass"~(mg~L^-1)) +
  theme_bw(base_size = 9) +
  theme(axis.title.x = element_blank(),
        legend.background = element_blank(),
        legend.position = c(0.15,0.825),
        legend.title = element_blank(),
        panel.grid = element_blank(), 
        panel.border = element_rect(linewidth = 1))+
  guides(fill = guide_legend(override.aes = list(shape = 22, size = 5))); p1
  
p2 = ggplot(anoxia) + 
  geom_boxplot(aes(x = group, y = timelag, fill = group), size = 0.2, outlier.shape = NA, color = "black", linewidth = .5) +
  geom_point(aes(x = group, y = timelag, fill = group), shape = 21, size = 1, position=position_jitterdodge(jitter.width = .5, jitter.height = 0, dodge.width = .75)) +
  scale_fill_manual(values = c(col.pre,col.post)) +
  stat_pvalue_manual(anoxia.stat, label = "{p.signif}", tip.length = 0.01, remove.bracket = FALSE, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0.03, 0.1))) +
  ylab("Lag between stratfication \nand anoxia (days)") +
  theme_bw(base_size = 9) +
  theme(axis.title.x = element_blank(), 
        legend.position = "none",
        legend.title = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(linewidth = 1)); p2

# Final figure
p1 + p2 + plot_layout(widths = c(4,1)) +
  plot_annotation(tag_levels = 'A', tag_suffix = ')') & 
  theme(plot.tag = element_text(size  = 8))
ggsave('figs_publication/Fig3_HD.png', dpi = 500, units = 'in', width = 6, height = 2.5)


