# RRR
# want to simplify figure 4 to only show discussed taxa and collapse rest (all minor contributors) as "other"
# keep: Bacillariophyta, Cyanophyta, Chlorophyta, Pyrrhophyta, Cryptophyta, Chrysophyta
# other: Miscellaneous, Xanthophyta, Euglenophyta, Haptophyta

phyto.list <- readRDS("robin-data/2022-06-08_phyto_list.rds")

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

saveRDS(object = phyto, file = "robin-data/2022-10-25_phyto_division_table.rds")
