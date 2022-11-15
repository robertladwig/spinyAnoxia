# RRR
# this is how I looked up the stats mentioned in the results text
options(scipen = 999)


# After the spiny water flea invasion, spring average biomass increased significantly from 1 ± 1 to 3 ± 2 (p = 0.004)
# During ice-on, average biomass also increased significantly (p = 0.002), however the total biomass is much lower during this season.
x <- read.csv(file = "figs_publication/Fig3_stats.csv")
x

# diatoms were predominantly responsible for the increase in spring biomass, comprising 69 ± 18 % of the spring biomass before and 67 ± 25 % after the spiny water flea invasion. 
x <- readRDS(file = "data_processed/4g_taxon_invasion_group_averages_perc-other_category.rds")
x$spring

# Diatom biomass in the spring increased from 1 ± 2 to 2 ± 2 (1 --> 2 is 2 fold), but the proportion of phytoplankton biomass comprised of diatoms remained relatively constant.
# chlorophyta increased 0.04 ± .03 to 0.2 ± 0.2 ( p = 0.02) 
# cyanophyta more variable, but increased 6 fold from 0.0228 ± 0.022 to 0.1270 ±  0.1973 (p = 0.1)
x <- readRDS(file = "data_processed/4g_taxon_invasion_group_averages_mg_L-other_category.rds")
x$spring
0.186783080 / 0.03779779417 # chlorophyta up 5 fold
x$spring$fold <- x$spring$post / x$spring$pre
x$spring
