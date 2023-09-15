# RRR
# Do a simple average of all measurements
# working with counts not biomass for now

library(data.table)

zoops <- readRDS(file = "data_processed/0q_zoop_abundances.rds")
zoops <- as.data.table(zoops)

seasons <- readRDS(file = "data_processed/4a_seasons_by_year.rds")
seasons <- as.data.table(seasons)

# subset to only spring ----

zoops$season <- "ice" # note this ignores any late-freeze dates but only looking at spring for now

for (yr in unique(zoops$year4)){
  seas <- seasons[Year == yr]
  zoops[year4 == yr & yday >= seas$Ice.Off, season := "spring"]
  zoops[year4 == yr & yday >= seas$Strat.Start, season := "stratified"]
  zoops[year4 == yr & yday >= seas$Strat.End, season := "fall"]
  zoops[year4 == yr & yday >= seas$Ice.On, season := "ice"]
}

zoops <- zoops[season == "spring"]
# unique(zoops$species_name) # huh, there are no spinies in the spring

# get count table ----

zoops.av <- dcast(data = zoops, formula = year4 ~ species_name, value.var = "density.num.m3", fun.aggregate = mean, na.rm = T)

# it's NaN when no observations in that year, make that zero
zoops.av <- as.matrix(zoops.av)
zoops.av[is.na(zoops.av)] <- 0
zoops.av <- as.data.table(zoops.av)

zoops.av$tot.zoops <- rowSums(x = zoops.av[ ,-c("year4")], na.rm = T)
# zoops.av$tot.zoops.no.SWF <- rowSums(x = zoops.av[ ,-c("year4","Bythotrephes Longimanus","tot.zoops")], na.rm = T) # no bytho in spring

write.csv(x = zoops.av, file = "data_processed/0s_spring_zoop_counts.csv", row.names = F)

# get biomass table ----

zoops.av <- dcast(data = zoops, formula = year4 ~ species_name, value.var = "Biomass.mg.L", fun.aggregate = mean, na.rm = T)

# it's NaN when no observations in that year, make that zero
zoops.av <- as.matrix(zoops.av)
zoops.av[is.na(zoops.av)] <- 0
zoops.av <- as.data.table(zoops.av)

zoops.av$tot.zoops <- rowSums(x = zoops.av[ ,-c("year4")], na.rm = T)
# zoops.av$tot.zoops.no.SWF <- rowSums(x = zoops.av[ ,-c("year4","Bythotrephes Longimanus","tot.zoops")], na.rm = T) # no bytho in spring

write.csv(x = zoops.av, file = "data_processed/0s_spring_zoop_biomass.csv", row.names = F)

