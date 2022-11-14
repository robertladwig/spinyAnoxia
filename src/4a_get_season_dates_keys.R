# RRR
# get season key for sample dates
# 

# ---- Import data and remind self of format ----

library(lubridate)

# strat.dates <- readRDS("robin-data/2022-06-23_stratification_dates.rds")
strat.dates <- readRDS("robin-data/2022-10-02_stratification_dates.rds")
head(strat.dates$mean)
strat.dates$mean$start # yday (and fractional)

ice.dates <- readRDS("data/2-ice_seasons_split_by_year.rds")
head(ice.dates)
ice.dates$Spring.Ice.On # GMT-5

phyto.list <- readRDS("robin-data/2022-06-08_phyto_list.rds")
names(phyto.list)
phyto.list$tot$date # UTC

# ---- combine ice and stratification info ----

season.dates <- strat.dates$mean
colnames(season.dates) <- c("Year","Strat.Start","Strat.End")

ice.off <- data.frame("Year" = year(ice.dates$Spring.Ice.Off), "Ice.Off" = yday(ice.dates$Spring.Ice.Off))
ice.on <-  data.frame("Year" = year(ice.dates$Spring.Ice.Off), "Ice.On" = yday(ice.dates$Fall.Ice.On))
index.na <- which(is.na(ice.on$Ice.On))
ice.on$Ice.On[index.na] <- yday(ice.dates$Spring.Ice.On[index.na + 1]) + 365

season.dates <- merge(x = season.dates, y = ice.off, by = "Year", all.x = TRUE, all.y = FALSE)
season.dates <- merge(x = season.dates, y = ice.on, by = "Year", all.x = TRUE, all.y = FALSE)

# pull recent year data from here: https://www.aos.wisc.edu/~sco/lakes/msnicesum.html
season.dates$Ice.On[season.dates$Year == 2020] <- 365 + 3
season.dates$Ice.Off[season.dates$Year == 2021] <- yday(parse_date_time("3-20-21","mdy"))
season.dates$Ice.On[season.dates$Year == 2021] <- 365 + 7

season.dates <- season.dates[ ,c(1,4,2,3,5)]
head(season.dates)

# ---- assign season to each biomass sample date ----

sample.dates <- data.frame("Year" = year(phyto.list$tot$date), "Yday" = yday(phyto.list$tot$date), "Date" = phyto.list$tot$date)
missing.dates <- sample.dates[sample.dates$Year == 1995, ] # 1995 is missing from stratification info
sample.dates <- sample.dates[sample.dates$Year != 1995, ] 

sample.dates$Season <- "stratified"

for (yr in unique(sample.dates$Year)){
  my.dates <- sample.dates[sample.dates$Year == yr, ,drop= F]
  my.seas <- season.dates[season.dates$Year == yr, ,drop = F]
  
  index.ice <- my.dates$Yday < my.seas$Ice.Off
  my.dates$Season[index.ice] <- "ice-on"
  
  index.spring <- my.dates$Yday < ceiling(my.seas$Strat.Start) & !index.ice # ex. strat start is 131.5, 131 is spring, 132 is strat. strat start is 131, 130 is spring 131 is strat
  my.dates$Season[index.spring] <- "spring"
  
  index.fall <- my.dates$Yday > floor(my.seas$Strat.End)
  my.dates$Season[index.fall] <- "fall"
  
  index.ice <- my.dates$Yday >= my.seas$Ice.On
  my.dates$Season[index.ice] <- "ice-on"
  
  sample.dates[sample.dates$Year == yr, ] <- my.dates
}

# -- add back the unknown dates so key matches phyto.list rows ----

missing.dates$Season <- "unknown"
sample.dates <- rbind(missing.dates, sample.dates)



# ---- save data ----

# saveRDS(object = sample.dates, file = "robin-data/2022-07-25_season_dates/seasons_by_sample.rds")
# saveRDS(object = season.dates, file = "robin-data/2022-07-25_season_dates/seasons_by_year.rds")

saveRDS(object = sample.dates, file = "robin-data/2022-10-02_season_dates/seasons_by_sample.rds")
saveRDS(object = season.dates, file = "robin-data/2022-10-02_season_dates/seasons_by_year.rds")
