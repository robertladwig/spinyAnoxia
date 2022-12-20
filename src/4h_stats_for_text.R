# RRR
# this is how I looked up the stats mentioned in the results text
options(scipen = 999)


x <- read.csv(file = "figs_publication/Fig3_stats.csv")
x
# Mean spring  phytoplankton biomass increased to concentrations typical of the stratified summer season, 
# from 1 ± 1 (pre-invasion) to 3 ± 2 mg L\textsuperscript{-1} (post invasion) (p < 0.005)
x[ ,c("Metric","Spring")]
# Similarly, biomass under lake-ice increased to concentrations previously typical of spring, 
# from 0.3 ± 0.3 to 2 ± 2 mg L\textsuperscript{-1} (p < 0.005)
x[ ,c("Metric","Ice")]
# We found that after the spiny water flea invasion, the lag between stratification and anoxia onset decreased by nearly 2 weeks, 
# from 51 ± 9 days to 39 ± 15 days (p < 0.05)
x[ ,c("Metric","Days.till.anoxia")]


x <- readRDS(file = "data_processed/4g_taxon_invasion_group_averages_perc-other_category.rds")
x$spring
y <- readRDS(file = "data_processed/4g_taxon_invasion_group_averages_mg_L-other_category.rds")
y$spring
# diatoms were predominantly responsible for the increase in spring biomass, comprising the majority of the phytoplankton community 
# before and after the spiny water flea invasion (67 ± 20\% and 65 ± 25\% respectively). 
x$spring[x$spring$taxon == "Bacillariophyta", ]
# Diatom biomass in the spring increased 2-fold, from 0.9 ± 0.9 to 2 ± 2 mg L\textsuperscript{-1} (p = 0.08), 
y$spring[x$spring$taxon == "Bacillariophyta", ]
y$spring[x$spring$taxon == "Bacillariophyta","post"] / y$spring[x$spring$taxon == "Bacillariophyta","pre"] # fold change = post / pre

# \textit{Chlorophyta} (green algae) remained at 5-9\% of the community 
x$spring[x$spring$taxon == "Chlorophyta", ]
# but increased 4-fold, from 0.04 ± .02 to 0.1 ± 0.1 mg/L ( p < 0.005)
y$spring[x$spring$taxon == "Chlorophyta", ]
y$spring[x$spring$taxon == "Chlorophyta","post"] / y$spring[x$spring$taxon == "Chlorophyta","pre"] # fold change = post / pre


# Cyanobacteria remained at 5-9\% of the community, 
x$spring[x$spring$taxon == "Cyanophyta", ]
# but increased by 6-fold, from 0.03 ± 0.03 to 0.2 ±  0.2 (p = 0.002)
y$spring[x$spring$taxon == "Cyanophyta", ]
y$spring[x$spring$taxon == "Cyanophyta","post"] / y$spring[x$spring$taxon == "Cyanophyta","pre"] # fold change = post / pre


# \textit{Pyrrhophyta} (dinoflagellates) remained at 1-3\% of the community 
x$spring[x$spring$taxon == "Pyrrhophyta", ]
#but increased 3-fold, from 0.02 ± 0.03 to 0.05 ± 0.03 (p = 0.01).
y$spring[x$spring$taxon == "Pyrrhophyta", ]
y$spring[x$spring$taxon == "Pyrrhophyta","post"] / y$spring[x$spring$taxon == "Pyrrhophyta","pre"] # fold change = post / pre


# \textit{Cryptophyta} (cryptophytes) decreased from 17 ± 12 to 9 ± 6\% of the community (p = 0.07)
x$spring[x$spring$taxon == "Cryptophyta", ]
#\textit{Chrysophyta} (golden algae) decreased from 3 ± 2 to 1 ± 1\% of the community (p = 0.05), 
x$spring[x$spring$taxon == "Chrysophyta", ]
# although the absolute biomass of both taxa remained constant.
y$spring[y$spring$taxon == "Cryptophyta", ]
y$spring[y$spring$taxon == "Chrysophyta", ]


