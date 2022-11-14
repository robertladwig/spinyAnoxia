# RRR
# pull out just the info of interest from LTER data, and make it a wide table.
# not sure if biovolume or biomass is a better metric. 
# maybe biomass, people seem to talk about that more? simple units?

library(tidyr)
library(lubridate)

phyto <- readRDS(file = "data_input/0a_phyto_knb-lter-ntl.88.30.rds")

created.file <- "data_processed/3a_phyto_list.rds"

# ----

# There's a mistake in the data on 2011-07-25, total biomass is 104
# shows up in biomass, not biovolume. so likely a conversion error, no?

problem.day <- which(phyto$Date == parse_date_time("2011-07-25","ymd", "Etc/GMT-5"))

View(phyto[problem.day, ])

problem.row <- 5423

phyto[problem.row, ] 

coelastrum.reticulatum <- which(phyto$lter.taxa_name == "Coelastrum reticulatum")

plot(x = phyto$Date[coelastrum.reticulatum], y = phyto$Biovolume.um3.mL[coelastrum.reticulatum])
plot(x = phyto$Date[coelastrum.reticulatum], y = phyto$Count.cells.mL[coelastrum.reticulatum])

# It's not like this is a common organism. Just remove the problem row, leave the rest of the date.
phyto <- phyto[-problem.row, ]

# ----

colnames(phyto)

phyto.division <- pivot_wider(data = phyto, id_cols = "lter.division", names_from = "Date", values_from = "Biomass.mg.L", values_fn = sum)
phyto.genus <- pivot_wider(data = phyto, id_cols = "lter.genus", names_from = "Date", values_from = "Biomass.mg.L", values_fn = sum)
phyto.name <-pivot_wider(data = phyto, id_cols = "lter.taxa_name", names_from = "Date", values_from = "Biomass.mg.L", values_fn = sum)

# phyto.division <- pivot_wider(data = phyto, id_cols = "lter.division", names_from = "Date", values_from = "Biovolume.um3.mL", values_fn = sum)
# phyto.genus <- pivot_wider(data = phyto, id_cols = "lter.genus", names_from = "Date", values_from = "Biovolume.um3.mL", values_fn = sum)
# phyto.name <-pivot_wider(data = phyto, id_cols = "lter.taxa_name", names_from = "Date", values_from = "Biovolume.um3.mL", values_fn = sum)


# stupid tibbles

un.tibble.it <- function(stupid.phyto){
  stupid.phyto <- as.data.frame(stupid.phyto)
  row.names(stupid.phyto) <- stupid.phyto[ ,1]
  stupid.phyto <- stupid.phyto[ ,-1]
  stupid.phyto <- as.matrix(stupid.phyto)
  return(stupid.phyto)
}

phyto.division <- un.tibble.it(stupid.phyto = phyto.division)
phyto.genus <- un.tibble.it(stupid.phyto = phyto.genus)
phyto.name <- un.tibble.it(stupid.phyto = phyto.name)

phyto.total <- data.frame("date" = colnames(phyto.division), "biomass" = colSums(phyto.division, na.rm = TRUE))
phyto.total$date <- parse_date_time(x = phyto.total$date, orders = "ymd")

my.phyto <- list("tot" = phyto.total, "div" = phyto.division, "gen" = phyto.genus, "tax" = phyto.name)

# ----
cat(created.file)
saveRDS(object = my.phyto, file = created.file)
