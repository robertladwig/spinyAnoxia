# RRR
# look at NMDS plots as a way to summarize the community change seen in the bar plots

phyto.list <- readRDS("robin-data/2022-06-08_phyto_list.rds")
# key <- readRDS("robin-data/2022-07-25_season_dates/seasons_by_sample.rds")
key <- readRDS("robin-data/2022-10-02_season_dates/seasons_by_sample.rds")

library(vegan)

plot.folder <- "plots/2022-10-02_NMDS_plots/"

# ---- functions ----

get.yearly.av <- function(my.phy){
  my.phy <- t(my.phy)
  my.phy <- as.data.frame(my.phy)
  agg.by.year <- as.numeric(substr(row.names(my.phy),start = 1, stop = 4))
  my.phy.av <- aggregate(x = my.phy, by = list(agg.by.year), FUN = mean)
  my.phy.av[1:5,1:5]
  row.names(my.phy.av) <- my.phy.av[ ,1]
  my.phy.av <- my.phy.av[ ,-1]
  my.phy.av <- as.matrix(t(my.phy.av))
  return(my.phy.av)
}

make.nmds.plot <- function(phyto, key, season, use.yearly.av = FALSE){
  my.key <- key[key$Season == season, ]
  my.phyto <- phyto[ ,key$Season == season]
  if (use.yearly.av){
    my.phyto <- get.yearly.av(my.phy = my.phyto)
    my.key <- my.key[!duplicated(my.key$Year), ]
  }
  my.nmds <- metaMDS(comm = t(my.phyto), distance = "bray", trymax = 100)
  my.anosim <- anosim(x = t(my.phyto), distance = "bray", grouping = my.key$Color.Invasion)
  plot(my.nmds, type = "n", ann = F, axes = F)
  box()
  if (use.yearly.av){
    cat(all.equal(row.names(my.nmds$points), as.character(my.key$Year)))
  }else{
    cat(all.equal(row.names(my.nmds$points), as.character(my.key$Date)))
  }
  points(my.nmds, col = my.key$Color.Invasion, pch = 19, cex = 2.5)
  text(my.nmds, labels = my.key$Year, cex = .5)
  mtext(text = season, side = 3, line = .5)
  return(my.anosim)
}

# ---- make color key ----

key$Color.Season <- "hotpink2"
key$Color.Season[key$Season == "ice-on"] <- "snow3"
key$Color.Season[key$Season == "spring"] <- "tan4"
key$Color.Season[key$Season == "stratified"] <- "chartreuse4"

key$Color.Invasion <- "steelblue"
key$Color.Invasion[key$Year >= 2010] <- "orange2"

phyto <- phyto.list$div

phyto[1:5,1:5]
phyto[is.na(phyto)] <- 0

# all
phyto.nmds <- metaMDS(comm = t(phyto), distance = "bray", trymax = 100)
plot(phyto.nmds, type = "n")
all.equal(row.names(phyto.nmds$points), as.character(key$Date))
points(phyto.nmds, col = key$Color.Season, pch = 19, cex = .9)

# invasion group
par(mfrow = c(1,2))
pre <- key$Color.Invasion == "steelblue"
phyto.nmds <- metaMDS(comm = t(phyto[ ,pre]), distance = "bray", trymax = 100)
plot(phyto.nmds, type = "n")
all.equal(row.names(phyto.nmds$points), as.character(key$Date[pre]))
points(phyto.nmds, col = key$Color.Season[pre], pch = 19, cex = .9)

post <- key$Color.Invasion == "orange2"
phyto.nmds <- metaMDS(comm = t(phyto[ ,post]), distance = "bray", trymax = 100)
plot(phyto.nmds, type = "n")
all.equal(row.names(phyto.nmds$points), as.character(key$Date[pre]))
points(phyto.nmds, col = key$Color.Season[post], pch = 19, cex = .9)

# year group
par(mfrow = c(5,6))
for (yr in unique(key$Year)){
  index <- key$Year == yr
  phyto.nmds <- metaMDS(comm = t(phyto[ ,index]), distance = "bray", trymax = 100)
  plot(phyto.nmds, type = "n")
  all.equal(row.names(phyto.nmds$points), as.character(key$Date[index]))
  points(phyto.nmds, col = key$Color.Season[index], pch = 19, cex = .9)
}

# ---- all data for each year ----
anosim.all.datapoints <- data.frame("Season" = unique(key$Season)[-1], "anosim.statistic.R" = NA, "anosim.significance" = NA)

pdf(file = file.path(plot.folder,"NMDS_all_datapoints.pdf"), width = 5, height = 5)

par(mfrow = c(2,2), mar = c(1,1,2,1))

my.ano <- make.nmds.plot(phyto = phyto, key = key, season = "ice-on")
anosim.all.datapoints[anosim.all.datapoints$Season == "ice-on", "anosim.statistic.R"] <- my.ano$statistic
anosim.all.datapoints[anosim.all.datapoints$Season == "ice-on", "anosim.significance"] <- my.ano$signif

my.ano <- make.nmds.plot(phyto = phyto, key = key, season = "spring")
anosim.all.datapoints[anosim.all.datapoints$Season == "spring", "anosim.statistic.R"] <- my.ano$statistic
anosim.all.datapoints[anosim.all.datapoints$Season == "spring", "anosim.significance"] <- my.ano$signif

my.ano <- make.nmds.plot(phyto = phyto, key = key, season = "stratified")
anosim.all.datapoints[anosim.all.datapoints$Season == "stratified", "anosim.statistic.R"] <- my.ano$statistic
anosim.all.datapoints[anosim.all.datapoints$Season == "stratified", "anosim.significance"] <- my.ano$signif

my.ano <- make.nmds.plot(phyto = phyto, key = key, season = "fall")
anosim.all.datapoints[anosim.all.datapoints$Season == "fall", "anosim.statistic.R"] <- my.ano$statistic
anosim.all.datapoints[anosim.all.datapoints$Season == "fall", "anosim.significance"] <- my.ano$signif

dev.off()

write.csv(x = anosim.all.datapoints, file = file.path(plot.folder, "anosim_all_datapoints.csv"), row.names = F)

# ---- averages for each year ----

anosim.year.averages <- data.frame("Season" = unique(key$Season)[-1], "anosim.statistic.R" = NA, "anosim.significance" = NA)

pdf(file = file.path(plot.folder,"NMDS_year_averages.pdf"), width = 5, height = 5)

par(mfrow = c(2,2), mar = c(1,1,2,1))

my.ano <- make.nmds.plot(phyto = phyto, key = key, season = "ice-on", use.yearly.av = TRUE)
anosim.year.averages[anosim.year.averages$Season == "ice-on", "anosim.statistic.R"] <- my.ano$statistic
anosim.year.averages[anosim.year.averages$Season == "ice-on", "anosim.significance"] <- my.ano$signif

my.ano <- make.nmds.plot(phyto = phyto, key = key, season = "spring", use.yearly.av = TRUE)
anosim.year.averages[anosim.year.averages$Season == "spring", "anosim.statistic.R"] <- my.ano$statistic
anosim.year.averages[anosim.year.averages$Season == "spring", "anosim.significance"] <- my.ano$signif

my.ano <- make.nmds.plot(phyto = phyto, key = key, season = "stratified", use.yearly.av = TRUE)
anosim.year.averages[anosim.year.averages$Season == "stratified", "anosim.statistic.R"] <- my.ano$statistic
anosim.year.averages[anosim.year.averages$Season == "stratified", "anosim.significance"] <- my.ano$signif

my.ano <- make.nmds.plot(phyto = phyto, key = key, season = "fall", use.yearly.av = TRUE)
anosim.year.averages[anosim.year.averages$Season == "fall", "anosim.statistic.R"] <- my.ano$statistic
anosim.year.averages[anosim.year.averages$Season == "fall", "anosim.significance"] <- my.ano$signif

dev.off()

write.csv(x = anosim.year.averages, file = file.path(plot.folder, "anosim_year_averages.csv"), row.names = F)
