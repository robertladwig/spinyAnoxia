# RRR
# Do a simple average of all measurements
# working with counts not biomass for now

library(data.table)

zoops <- readRDS(file = "data_processed/0q_zoop_abundances.rds")
zoops

# get count table ----

zoops.av <- dcast(data = zoops, formula = year4 ~ species_name, value.var = "density.num.m3", fun.aggregate = mean, na.rm = T)

# it's NaN when no observations in that year, make that zero
zoops.av <- as.matrix(zoops.av)
zoops.av[is.na(zoops.av)] <- 0
zoops.av <- as.data.table(zoops.av)

zoops.av$tot.zoops <- rowSums(x = zoops.av[ ,-c("year4")], na.rm = T)
zoops.av$tot.zoops.no.SWF <- rowSums(x = zoops.av[ ,-c("year4","Bythotrephes Longimanus","tot.zoops")], na.rm = T)

write.csv(x = zoops.av, file = "data_processed/0r_annual_zoop_counts.csv", row.names = F)

# get biomass table ----

zoops.av <- dcast(data = zoops, formula = year4 ~ species_name, value.var = "Biomass.mg.L", fun.aggregate = mean, na.rm = T)

# it's NaN when no observations in that year, make that zero

# it's NaN when no observations in that year, make that zero
zoops.av <- as.matrix(zoops.av)
zoops.av[is.na(zoops.av)] <- 0
zoops.av <- as.data.table(zoops.av)

zoops.av$tot.zoops <- rowSums(x = zoops.av[ ,-c("year4")], na.rm = T)
zoops.av$tot.zoops.no.SWF <- rowSums(x = zoops.av[ ,-c("year4","Bythotrephes Longimanus","tot.zoops")], na.rm = T)

write.csv(x = zoops.av, file = "data_processed/0r_annual_zoop_biomass.csv", row.names = F)

write.csv(x = zoops.av, file = "data_processed/0r_zoop_abundances.csv", row.names = F)
