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
# No statistically significant change in total biomass was observed during the stratified season (p > 0.1),
x[ ,c("Metric","Stratified")]
# and more modest increases were observed during the fall mixed season (p < 0.05).
x[ ,c("Metric","Fall")]
# We found that after the spiny water flea invasion, the lag between stratification and anoxia onset decreased by nearly 2 weeks, 
# from 51 ± 9 days to 39 ± 15 days (p < 0.05)
x[ ,c("Metric","Days.till.anoxia")]


x <- readRDS(file = "data_processed/4g_taxon_invasion_group_averages_perc-other_category.rds")
x$spring
y <- readRDS(file = "data_processed/4g_taxon_invasion_group_averages_mg_L-other_category.rds")
y$spring
# diatoms were predominantly responsible for the increase in spring biomass, comprising the majority of the phytoplankton community 
# before and after the spiny water flea invasion (67 ± 20\% and 65 ± 25\% respectively). 
x$spring[x$spring$taxon == "Diatoms", ]
# Diatom biomass in the spring increased 2-fold, from 0.9 ± 0.9 to 2 ± 2 mg L\textsuperscript{-1} (p = 0.08), 
y$spring[x$spring$taxon == "Diatoms", ]
y$spring[x$spring$taxon == "Diatoms","post"] / y$spring[x$spring$taxon == "Diatoms","pre"] # fold change = post / pre

# \textit{Chlorophyta} (green algae) remained at 5-9\% of the community 
x$spring[x$spring$taxon == "Green algae", ]
# but increased 4-fold, from 0.04 ± .02 to 0.1 ± 0.1 mg/L ( p < 0.005)
y$spring[x$spring$taxon == "Green algae", ]
y$spring[x$spring$taxon == "Green algae","post"] / y$spring[x$spring$taxon == "Green algae","pre"] # fold change = post / pre


# Cyanobacteria remained at 5-9\% of the community, 
x$spring[x$spring$taxon == "Cyanobacteria", ]
# but increased by 6-fold, from 0.03 ± 0.03 to 0.2 ±  0.2 (p = 0.002)
y$spring[x$spring$taxon == "Cyanobacteria", ]
y$spring[x$spring$taxon == "Cyanobacteria","post"] / y$spring[x$spring$taxon == "Cyanobacteria","pre"] # fold change = post / pre


# \textit{Pyrrhophyta} (dinoflagellates) remained at 1-3\% of the community 
x$spring[x$spring$taxon == "Dinoflagellates", ]
#but increased 3-fold, from 0.02 ± 0.03 to 0.05 ± 0.03 (p = 0.01).
y$spring[x$spring$taxon == "Dinoflagellates", ]
y$spring[x$spring$taxon == "Dinoflagellates","post"] / y$spring[x$spring$taxon == "Dinoflagellates","pre"] # fold change = post / pre


# \textit{Cryptophyta} (cryptophytes) decreased from 17 ± 12 to 9 ± 6\% of the community (p = 0.07)
x$spring[x$spring$taxon == "Cryptophytes", ]
#\textit{Chrysophyta} (golden algae) decreased from 3 ± 2 to 1 ± 1\% of the community (p = 0.05), 
x$spring[x$spring$taxon == "Golden algae", ]
# although the absolute biomass of both taxa remained constant.
y$spring[y$spring$taxon == "Cryptophytes", ]
y$spring[y$spring$taxon == "Golden algae", ]

# ---- exclude zebra years for reviewer ----

x <- readRDS(file = "data_processed/4g_taxon_invasion_group_averages_perc-other_category-no_zebra.rds")
x$spring
y <- readRDS(file = "data_processed/4g_taxon_invasion_group_averages_mg_L-other_category-no_zebra.rds")
y$spring
# diatoms were predominantly responsible for the increase in spring biomass, comprising the majority of the phytoplankton community 
# before and after the spiny water flea invasion (67 ± 20\% and 65 ± 25\% respectively). 
# NOW                                            (67 ± 20       71 ± 16   (p = .65))
x$spring[x$spring$taxon == "Diatoms", ]
# Diatom biomass in the spring increased 2-fold, from 0.9 ± 0.9 to 2 ± 2 mg L\textsuperscript{-1} (p = 0.08), 
# NOW                                    2-fold,      0.9 ± 0.9.   2 ± 1                          (p = 0.08)
y$spring[x$spring$taxon == "Diatoms", ]
y$spring[x$spring$taxon == "Diatoms","post"] / y$spring[x$spring$taxon == "Diatoms","pre"] # fold change = post / pre

# \textit{Chlorophyta} (green algae) remained at 5-9\% of the community 
# NOW                                            5-9
x$spring[x$spring$taxon == "Green algae", ]
# but increased 4-fold, from 0.04 ± .02 to 0.1 ± 0.1 mg/L ( p < 0.005)
# NOW           4-fold,      0.04 ± .02.   0.1 ± 0.1        p < 0.005
y$spring[x$spring$taxon == "Green algae", ]
y$spring[x$spring$taxon == "Green algae","post"] / y$spring[x$spring$taxon == "Green algae","pre"] # fold change = post / pre


# Cyanobacteria remained at 5-9\% of the community, 
# NOW                       5-5
x$spring[x$spring$taxon == "Cyanobacteria", ]
# but increased by 6-fold, from 0.03 ± 0.03 to 0.2 ±  0.2 (p = 0.002)
# NOW              3-fold,      0.03 ± 0.03.   0.09 ± 0.07 (p = 0.02)
y$spring[x$spring$taxon == "Cyanobacteria", ]
y$spring[x$spring$taxon == "Cyanobacteria","post"] / y$spring[x$spring$taxon == "Cyanobacteria","pre"] # fold change = post / pre


# \textit{Pyrrhophyta} (dinoflagellates) remained at 1-3\% of the community 
# NOW                                                1-2
x$spring[x$spring$taxon == "Dinoflagellates", ]
#but increased 3-fold, from 0.02 ± 0.03 to 0.05 ± 0.03 (p = 0.01).
# NOW          3-fold.      0.02 ± 0.03    0.05 ± 0.02 (p = 0.02)
y$spring[x$spring$taxon == "Dinoflagellates", ]
y$spring[x$spring$taxon == "Dinoflagellates","post"] / y$spring[x$spring$taxon == "Dinoflagellates","pre"] # fold change = post / pre


# \textit{Cryptophyta} (cryptophytes) decreased from 17 ± 12 to 9 ± 6\% of the community (p = 0.07)
# NOW                                                17 ± 12    8 ± 5                    (p = 0.09)
x$spring[x$spring$taxon == "Cryptophytes", ]
#\textit{Chrysophyta} (golden algae) decreased from 3 ± 2 to 1 ± 1\% of the community (p = 0.05), 
# NOW                                               3 ± 2.   1 ± 2                    (p = 0.15) 
x$spring[x$spring$taxon == "Golden algae", ]
# although the absolute biomass of both taxa remained constant.
y$spring[y$spring$taxon == "Cryptophytes", ]
y$spring[y$spring$taxon == "Golden algae", ]

