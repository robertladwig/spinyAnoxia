# RRR
# pull out just the info of interest from LTER data, and make it a wide table.
# not sure if biovolume or biomass is a better metric. 
# maybe biomass, people seem to talk about that more? simple units?
# also get annual average values for Robert/reviewers

library(tidyr)
library(lubridate)
library(data.table)

phyto <- readRDS(file = "data_input/0a_phyto_knb-lter-ntl.88.30.rds")

seasons <- readRDS(file = "data_processed/4a_seasons_by_year.rds")

created.file <- "data_processed/3a_phyto_list.rds"

created.file.annual.avs <- "data_processed/3a_phyto_annual_average_biomass.csv"

created.file.spring.avs <- "data_processed/3a_phyto_spring_average_biomass.csv"

# ----

# There's a mistake in the data on 2011-07-25, total biomass is 104
# shows up in biomass, not biovolume. so likely a conversion error, no?

problem.day <- which(phyto$Date == parse_date_time("2011-07-25","ymd", "Etc/GMT-5"))

# View(phyto[problem.day, ])

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

# ---- annual averages for models ----

annual.aves <- my.phyto$div |>
  t()

annual.aves <- data.table("Year" = year(parse_date_time(row.names(annual.aves), orders = "ymd")), annual.aves)

annual.aves <- melt(data = annual.aves, id.vars = "Year", variable.name = "Division", value.name = "biomass")
annual.aves <- dcast(data = annual.aves, formula = Year ~ Division, value.var = "biomass", fun.aggregate = mean, na.rm = T)

annual.aves$Total.Mean.Biomass.mg.L <- rowSums(annual.aves[ ,-1], na.rm = T)

annual.aves <- as.matrix(annual.aves)
annual.aves[is.na(annual.aves)] <- 0

# ---- spring averages for models ----

spring.aves <- my.phyto$div |>
  t()

spring.aves <- data.table("Date" = parse_date_time(row.names(spring.aves), orders = "ymd"), spring.aves)
spring.aves$is.spring <- FALSE
spring.aves <- spring.aves[year(Date) > 1995, ]

for (yr in unique(year(spring.aves$Date))){
  seas <- seasons[seasons$Year == yr, ]
  spring.aves[year(Date) == yr & yday(Date) >= seas$Ice.Off & yday(Date) < seas$Strat.Start, is.spring := TRUE]
}

spring.aves <- spring.aves[is.spring == TRUE]
spring.aves[ ,`:=`(Year = year(Date),
                   Date = NULL,
                   is.spring = NULL)]

spring.aves <- melt(data = spring.aves, id.vars = "Year", variable.name = "Division", value.name = "biomass")
spring.aves <- dcast(data = spring.aves, formula = Year ~ Division, value.var = "biomass", fun.aggregate = mean, na.rm = T)

spring.aves$Total.Mean.Biomass.mg.L <- rowSums(spring.aves[ ,-1], na.rm = T)

spring.aves <- as.matrix(spring.aves)
spring.aves[is.na(spring.aves)] <- 0

# ----
cat(created.file)
saveRDS(object = my.phyto, file = created.file)

cat(created.file.annual.avs)
write.csv(x = annual.aves, file = created.file.annual.avs, row.names = F)

cat(created.file.spring.avs)
write.csv(x = spring.aves, file = created.file.spring.avs, row.names = F)