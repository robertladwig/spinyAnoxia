# RRR

phyto.abund <- readRDS("data_processed/4f_taxon_year_averages_mg_L-other_category.rds")
phyto.perc <- readRDS("data_processed/4f_taxon_year_averages_perc-other_category.rds")

file.inv.mgL <-  "data_processed/4g_taxon_invasion_group_averages_mg_L-other_category.rds"
file.inv.perc <-  "data_processed/4g_taxon_invasion_group_averages_perc-other_category.rds"


# ---- get stats ----

stats.table <- data.frame("taxon" = row.names(phyto.abund$spring), "pre" = NA, "sd.pre" = NA, "post" = NA, "sd.post" = NA, "p.value" = NA)

i.pre <- 1:14 # change if add back in 1995
i.post <- 15:25

# ---- get version of stats without zebra years ----
# file.inv.mgL <-  "data_processed/4g_taxon_invasion_group_averages_mg_L-other_category-no_zebra.rds"
# file.inv.perc <-  "data_processed/4g_taxon_invasion_group_averages_perc-other_category-no_zebra.rds"
# i.pre <- 1:14 
# i.post <- 15:20
# ----

for (s in names(phyto.abund)){
  my.stats <- stats.table
  for (r in 1:nrow(my.stats)){
    my.stats$pre[r] <- mean(phyto.abund[[s]][r,i.pre], na.rm = T)
    my.stats$post[r] <- mean(phyto.abund[[s]][r,i.post], na.rm = T)
    my.stats$sd.pre[r] <- sd(phyto.abund[[s]][r,i.pre], na.rm = T)
    my.stats$sd.post[r] <- sd(phyto.abund[[s]][r,i.post], na.rm = T)
    
    # my.t <- t.test(x = phyto.abund[[s]][r,i.pre], y = phyto.abund[[s]][r,i.post])
    # my.t <- kruskal.test(x = list("pre" = phyto.abund[[s]][r,i.pre], "post" = phyto.abund[[s]][r,i.post]))
    my.t <- wilcox.test(x = phyto.abund[[s]][r,i.pre], y = phyto.abund[[s]][r,i.post])
    my.stats$p.value[r] <- my.t$p.value
  }
  phyto.abund[[s]] <- my.stats
}
phyto.abund


for (s in names(phyto.perc)){
  my.stats <- stats.table
  for (r in 1:nrow(my.stats)){
    my.stats$pre[r] <- mean(phyto.perc[[s]][r,i.pre], na.rm = T)
    my.stats$post[r] <- mean(phyto.perc[[s]][r,i.post], na.rm = T)
    my.stats$sd.pre[r] <- sd(phyto.perc[[s]][r,i.pre], na.rm = T)
    my.stats$sd.post[r] <- sd(phyto.perc[[s]][r,i.post], na.rm = T)
    # my.t <- t.test(x = phyto.perc[[s]][r,i.pre], y = phyto.perc[[s]][r,i.post])
    # my.t <- kruskal.test(x = list("pre" = phyto[[s]][r,i.pre], "post" = phyto[[s]][r,i.post]))
    my.t <- wilcox.test(x = phyto.perc[[s]][r,i.pre], y = phyto.perc[[s]][r,i.post])
    my.stats$p.value[r] <- my.t$p.value
  }
  phyto.perc[[s]] <- my.stats
}
phyto.perc


saveRDS(object = phyto.abund, file = file.inv.mgL)
saveRDS(object = phyto.perc, file = file.inv.perc)
