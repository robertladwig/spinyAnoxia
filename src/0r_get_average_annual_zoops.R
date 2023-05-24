# RRR
# Do a simple average of all measurements
# working with counts not biomass for now

library(data.table)

zoops <- readRDS(file = "data_input/0o_zoop_knb-lter-ntl.90.33.rds")
zoops

zoops.av <- dcast(data = zoops, formula = year4 ~ species_name, value.var = "density.num.m3", fun.aggregate = mean, na.rm = T)

# it's NaN when no observations in that year, make that zero

zoops.av <- as.matrix(zoops.av)
zoops.av[is.na(zoops.av)] <- 0
zoops.av <- as.data.table(zoops.av)

zoops.av$tot.zoops <- rowSums(x = zoops.av[ ,-1], na.rm = T)
colnames(zoops.av)
zoops.av$tot.zoops.no.SWF <- rowSums(x = zoops.av[ ,-c("year4","Bythotrephes Longimanus")], na.rm = T)

write.csv(x = zoops.av, file = "data_processed/0r_zoop_abundances.csv", row.names = F)

