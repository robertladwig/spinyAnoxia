# RRR

phyto <- readRDS("data_processed/4f_taxon_year_averages_mg_L-other_category.rds")

file.perc.phyto <- "data_processed/4g_taxon_year_averages_perc-other_category.rds"

file.inv.mgL <-  "data_processed/4g_taxon_invasion_group_averages_mg_L-other_category.rds"
file.inv.perc <-  "data_processed/4g_taxon_invasion_group_averages_perc-other_category.rds"

# ---- make version of table as percent abundance ----
phyto.perc <- phyto
for (s in 1:length(phyto.perc)){
  as.perc <- t(phyto.perc[[s]]) / colSums(phyto.perc[[s]]) * 100
  as.perc <- t(as.perc)
  colSums(as.perc, na.rm = T)
  phyto.perc[[s]] <- as.perc
}

saveRDS(object = phyto.perc, file = file.perc.phyto)

# ---- get stats ----

stats.table <- data.frame("taxon" = row.names(phyto$spring), "pre" = NA, "sd.pre" = NA, "post" = NA, "sd.post" = NA, "p.value" = NA)

i.pre <- 1:14 # change if add back in 1995
i.post <- 15:25

phyto.abund <- phyto
for (s in names(phyto)){
  my.stats <- stats.table
  for (r in 1:nrow(my.stats)){
    my.stats$pre[r] <- mean(phyto[[s]][r,i.pre], na.rm = T)
    my.stats$post[r] <- mean(phyto[[s]][r,i.post], na.rm = T)
    my.stats$sd.pre[r] <- sd(phyto[[s]][r,i.pre], na.rm = T)
    my.stats$sd.post[r] <- sd(phyto[[s]][r,i.post], na.rm = T)
    
    # my.t <- t.test(x = phyto[[s]][r,i.pre], y = phyto[[s]][r,i.post])
    # my.t <- kruskal.test(x = list("pre" = phyto[[s]][r,i.pre], "post" = phyto[[s]][r,i.post]))
    my.t <- wilcox.test(x = phyto[[s]][r,i.pre], y = phyto[[s]][r,i.post])
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
