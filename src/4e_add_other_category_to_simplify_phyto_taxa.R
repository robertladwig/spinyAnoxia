# RRR
# want to simplify figure 4 to only show discussed taxa and collapse rest (all minor contributors) as "other"
# keep: Bacillariophyta, Cyanophyta, Chlorophyta, Pyrrhophyta, Cryptophyta, Chrysophyta
# other: Miscellaneous, Xanthophyta, Euglenophyta, Haptophyta

phyto.list <- readRDS("data_processed/3a_phyto_list.rds")

created.file <- "data_processed/4e_phyto_division_table.rds"

# ----

phyto <- phyto.list$div

phyto[1:5,1:5]

phyto.names <- row.names(phyto)
phyto.names[phyto.names == "Miscellaneous"] <- "other"
phyto.names[phyto.names == "Xanthophyta"] <- "other"
phyto.names[phyto.names == "Euglenophyta"] <- "other"
phyto.names[phyto.names == "Haptophyta"] <- "other"

phyto <- aggregate(x = phyto, by = list("Division" = phyto.names), FUN = sum, na.rm = T)

row.names(phyto) <- phyto$Division

phyto <- as.matrix(phyto[ ,-1])

saveRDS(object = phyto, file = created.file)
