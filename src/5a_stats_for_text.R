# RRR
options(scipen = 999)


# After the spiny water flea invasion, spring average biomass increased significantly from 1 ± 2 to 3 ± 2 (p = 0.05)
# During ice-on, average biomass also increased significantly (p = 0.02), however the total biomass is much lower during this season.
x <- read.csv(file = "plots/2022-08-07_spring_importance_plot/mean_annual_biomass_by_season-stats.csv")
x

# After the spiny water flea invasion, spring average biomass increased significantly from 1 ± 2 to 3 ± 2 (p = 0.05)
# During ice-on, average biomass also increased significantly (p = 0.02), however the total biomass is much lower during this season.
y <- read.csv(file = "plots/2022-09-30_spring_importance_plot/mean_annual_biomass_by_season-stats.csv")
y


# diatoms were predominantly responsible for the increase in spring biomass, comprising 69 ± 18 % of the spring biomass before and 67 ± 25 % after the spiny water flea invasion. 
x <- readRDS(file = "robin-data/2022-09-28_phyto_stats_by_taxon/taxon_invasion_group_averages_perc.rds")
x$spring

# Diatom biomass in the spring increased from 1 ± 2 to 2 ± 2 (1 --> 2 is 2 fold), but the proportion of phytoplankton biomass comprised of diatoms remained relatively constant.
# chlorophyta increased 0.04 ± .03 to 0.2 ± 0.2 ( p = 0.02) 
# cyanophyta more variable, but increased 6 fold from 0.0228 ± 0.022 to 0.1270 ±  0.1973 (p = 0.1)
x <- readRDS(file = "robin-data/2022-09-28_phyto_stats_by_taxon/taxon_invasion_group_averages_mg_L.rds")
x$spring
0.186783080 / 0.03779779417 # chlorophyta up 5 fold
x$spring$fold <- x$spring$post / x$spring$pre
x$spring
