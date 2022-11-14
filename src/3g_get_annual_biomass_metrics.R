# RRR

# get an annual "metric" of biomass that Robert can model with anoxic factor
# Try sum total biomass instead of just high biomass days like I did before
# try this during the stratified period and whole year, or starting at ice-off

library(lubridate)

phyto <- readRDS("data_processed/3b_phyto-interp_split_by_year.rds")
measured <- readRDS("data_processed/3b_phyto_split_by_year.rds")
strat <- read.csv(file = "data_processed/stratification_start.csv")
ice <- readRDS("data_processed/0c_ice_seasons_split_by_year.rds")

save.formatted.strat.dates <- "data_processed/3g_stratification_dates.rds"
save.season.assigned.interp.days <- "data_processed/3g_phyto_interp_split-by-year_with_seasons.rds"
save.season.assigned.measured.days <- "data_processed/3g_phyto_measured_split-by-year_with_seasons.rds"
save.season.biomass.metrics.interp <- "data_processed/3g_biomass_metrics_by_season-interp_values.rds"
save.season.biomass.metrics.measured <- "data_processed/3g_biomass_metrics_by_season-measured_values.rds"
save.season.biomass.individual.measurements <- "data_processed/3g_biomass_metrics_by_season-individual_measurements"
save.quick.plots.folder <- "figs/3g_biomass_metrics_by_season/"

# ---- format strat dates ----
make.empty.list.structure <- function(ListNames){
  # the ListNames can be something like c("OTU", "kingdom","phylum","class","order","family/lineage","genus/clade","species/tribe")
  empty.list <- list(NULL)
  for (e in 1:length(ListNames)){
    empty.list[[e]] <- 0
    names(empty.list)[e] <- ListNames[e]
  }
  return(empty.list)
}


strat$linear <- yday(parse_date_time(x = strat$linear, orders = "ymd"))
strat$constant.high <- yday(parse_date_time(x = strat$constant.high, orders = "ymd")) # note 2 of these are 1, so probably should be NA- 2008, 2016
strat$constant.low <- yday(parse_date_time(x = strat$constant.low, orders = "ymd"))
strat$spline <- yday(parse_date_time(x = strat$spline, orders = "ymd"))
strat$mean <- rowMeans(strat[ ,-1])
strat.onset <- strat

strat <- read.csv(file = "data_processed/stratification_end.csv")
strat$linear <- yday(parse_date_time(x = strat$linear, orders = "ymd"))
strat$constant.high <- yday(parse_date_time(x = strat$constant.high, orders = "ymd")) # note 2 of these are 1, so probably should be NA- 2008, 2016
strat$constant.low <- yday(parse_date_time(x = strat$constant.low, orders = "ymd"))
strat$spline <- yday(parse_date_time(x = strat$spline, orders = "ymd"))
strat$mean <- rowMeans(strat[ ,-1])
strat.end <- strat

strat <- make.empty.list.structure(ListNames = c(colnames(strat.onset)[-1]))
for (n in names(strat)){
  strat[[n]] <- data.frame("year" = strat.onset$year, "start" = strat.onset[ ,n], "end" = strat.end[ ,n])
}

# ---- export strat ----

cat(save.formatted.strat.dates)
saveRDS(object = strat, file = save.formatted.strat.dates)
rm(strat.end, strat.onset)

# ---- split into seasons- interpolated ----

phyto <- phyto[-1]
names(phyto) # no 1995 in the strat info

for (yr in names(phyto)){
  my.phy <- phyto[[yr]]
  my.ice <- ice[ice$Summer.Year == as.numeric(yr), ]
  for (m in names(strat)){
    my.seas <- strat[[m]][strat[[m]]$year == as.numeric(yr), ]
    my.phy$season <- "stratified"
    my.phy$season[my.phy$day < my.seas$start] <- "spring"
    my.phy$season[my.phy$day >= my.seas$end] <- "fall"
    my.phy$season[my.phy$day < yday(my.ice$Spring.Ice.Off)] <- "ice"
    my.phy$season[my.phy$day >= yday(my.ice$Fall.Ice.On)] <- "ice"
    colnames(my.phy)[ncol(my.phy)] <- m
  }
  phyto[[yr]] <- my.phy
}
head(phyto$`2020`)

# ---- split into seasons- measured ----

measured <- measured[-1]
names(measured) # no 1995 in the strat info

for (yr in names(measured)){
  my.phy <- measured[[yr]]
  my.ice <- ice[ice$Summer.Year == as.numeric(yr), ]
  for (m in names(strat)){
    my.seas <- strat[[m]][strat[[m]]$year == as.numeric(yr), ]
    my.phy$season <- "stratified"
    my.phy$season[my.phy$yday < my.seas$start] <- "spring"
    my.phy$season[my.phy$yday >= my.seas$end] <- "fall"
    my.phy$season[my.phy$yday < yday(my.ice$Spring.Ice.Off)] <- "ice"
    my.phy$season[my.phy$yday >= yday(my.ice$Fall.Ice.On)] <- "ice"
    colnames(my.phy)[ncol(my.phy)] <- m
  }
  measured[[yr]] <- my.phy
}
head(measured$`2020`)


# ---- export seasons ----
cat(save.season.assigned.interp.days)
saveRDS(object = phyto, file = save.season.assigned.interp.days)

cat(save.season.assigned.measured.days)
saveRDS(object = measured, file = save.season.assigned.measured.days)

# ----- get metrics ----

# interp data ----

metrics <- data.frame("Year" = as.numeric(names(phyto)), "Cumulative.mg.L" = NA, "Ave.Daily.mg.L" = NA, "Total.Days" = NA)
all.models <- make.empty.list.structure(ListNames = c("linear","constant.high","constant.low","spline", "mean"))
all.seasons <- make.empty.list.structure(ListNames = c("ice","spring","stratified","fall"))

for (m in names(all.models)){
  all.models[[m]] <- all.seasons
  for (s in names(all.seasons)){
    all.models[[m]][[s]] <- metrics
  }
}
names(all.models)
names(all.models$linear)
head(all.models$mean$stratified)

for (m in names(all.models)){
  for (s in names(all.seasons)){
    for (yr in names(phyto)){
      y <- which(metrics$Year == as.numeric(yr))
      my.phy <- phyto[[yr]]
      my.col <- which(colnames(my.phy) == m)
      index.season <- my.phy[ ,m] == s
      my.n <- sum(index.season)
      tot.biomass <- sum(my.phy[index.season,2])
      if (my.n == 0){
        tot.biomass <- NA
      }
      all.models[[m]][[s]][y,"Cumulative.mg.L"] <- tot.biomass
      all.models[[m]][[s]][y,"Total.Days"] <- my.n
      all.models[[m]][[s]][y,"Ave.Daily.mg.L"] <- tot.biomass / my.n
    }
  }
}

all.models$linear$ice
all.models$constant.high$spring
all.models$constant.low$stratified
all.models$spline$fall
all.models$mean$spring

all.models.interp <- all.models

# measured data ----

metrics <- data.frame("Year" = as.numeric(names(measured)), "Cumulative.mg.L" = NA, "Ave.Daily.mg.L" = NA, "Total.Days" = NA)
all.models <- make.empty.list.structure(ListNames = c("linear","constant.high","constant.low","spline","mean"))
all.seasons <- make.empty.list.structure(ListNames = c("ice","spring","stratified","fall"))

for (m in names(all.models)){
  all.models[[m]] <- all.seasons
  for (s in names(all.seasons)){
    all.models[[m]][[s]] <- metrics
  }
}
names(all.models)
names(all.models$linear)
head(all.models$linear$ice)

for (m in names(all.models)){
  for (s in names(all.seasons)){
    for (yr in names(measured)){
      y <- which(metrics$Year == as.numeric(yr))
      my.phy <- measured[[yr]]
      my.col <- which(colnames(my.phy) == m)
      index.season <- my.phy[ ,m] == s
      my.n <- sum(index.season)
      tot.biomass <- sum(my.phy[index.season,2])
      if (my.n == 0){
        tot.biomass <- NA
      }
      all.models[[m]][[s]][y,"Cumulative.mg.L"] <- tot.biomass
      all.models[[m]][[s]][y,"Total.Days"] <- my.n
      all.models[[m]][[s]][y,"Ave.Daily.mg.L"] <- tot.biomass / my.n
    }
  }
}

all.models$linear$ice
all.models$constant.high$spring
all.models$constant.low$stratified
all.models$spline$fall
all.models$mean$spring

all.models.measured <- all.models

# measured data - Keep exact measurements, just reformat no stats ----

metrics <- data.frame("year" = 2050, "date" = parse_date_time("2050-1-1","ymd"),"biomass" = 0)
all.models <- make.empty.list.structure(ListNames = c("linear","constant.high","constant.low","spline","mean"))
all.seasons <- make.empty.list.structure(ListNames = c("ice","spring","stratified","fall"))

for (m in names(all.models)){
  all.models[[m]] <- all.seasons
  for (s in names(all.seasons)){
    all.models[[m]][[s]] <- metrics
  }
}
names(all.models)
names(all.models$linear)
head(all.models$linear$ice)

for (m in names(all.models)){
  for (s in names(all.seasons)){
    for (yr in names(measured)){
      my.phy <- measured[[yr]]
      my.col <- which(colnames(my.phy) == m)
      index.season <- my.phy[ ,m] == s
      my.seas <- my.phy[index.season, c("year","date","biomass")]
      all.models[[m]][[s]] <- rbind(all.models[[m]][[s]], my.seas) 
    }
    index.placeholder <- which(all.models[[m]][[s]]$year == 2050)
    all.models[[m]][[s]] <- all.models[[m]][[s]][-index.placeholder, ]
  }
}

all.models$linear$ice
all.models$constant.high$spring
all.models$constant.low$stratified
all.models$spline$fall
all.models$mean$spring

all.models.individ.measurements <- all.models

# ---- add true measured days to interp metrics ----

for (m in names(all.models)){
  for (s in names(all.seasons)){
    my.interp <- all.models.interp[[m]][[s]]
    
    my.measured <- all.models.measured[[m]][[s]]
    my.interp$Measured.Days <- my.measured$Total.Days
    
    all.models.interp[[m]][[s]] <- my.interp
  }
}

# ---- export season biomass metrics ----

cat(save.season.biomass.metrics.interp)
saveRDS(object = all.models.interp, file = save.season.biomass.metrics.interp)

cat(save.season.biomass.metrics.measured)
saveRDS(object = all.models.measured, file = save.season.biomass.metrics.measured)

cat(save.season.biomass.individual.measurements)
saveRDS(object = all.models.individ.measurements, file = save.season.biomass.individual.measurements)

# ---- quick looks ----

all.models <- all.models.interp

model.choice <- "linear"
season.choice <- "stratified"
# ----
for (model.choice in names(all.models)){
  for (season.choice in names(all.models[[1]])){
    pdf(file = paste0(save.quick.plots.folder,"/cumulative_biomass_",season.choice,"_",model.choice,".pdf"), width = 6.5, height = 3)
    par(mar = c(2,2,1,.5))
    plot(x = all.models[[model.choice]][[season.choice]]$Year, y = all.models[[model.choice]][[season.choice]]$Cumulative.mg.L, type = "n")
    points(x = all.models[[model.choice]][[season.choice]]$Year, y = all.models[[model.choice]][[season.choice]]$Cumulative.mg.L)
    lines(x = all.models[[model.choice]][[season.choice]]$Year, y = all.models[[model.choice]][[season.choice]]$Cumulative.mg.L)
    mtext(paste(season.choice, model.choice), side = 3)
    dev.off()
  }
}
# ----
# ----
for (model.choice in names(all.models)){
  for (season.choice in names(all.models[[1]])){
    pdf(file = paste0(save.quick.plots.folder,"/average_biomass_",season.choice,"_",model.choice,".pdf"), width = 6.5, height = 3)
    par(mar = c(2,2,1,.5))
    plot(x = all.models[[model.choice]][[season.choice]]$Year, y = all.models[[model.choice]][[season.choice]]$Ave.Daily.mg.L, type = "n")
    points(x = all.models[[model.choice]][[season.choice]]$Year, y = all.models[[model.choice]][[season.choice]]$Ave.Daily.mg.L)
    lines(x = all.models[[model.choice]][[season.choice]]$Year, y = all.models[[model.choice]][[season.choice]]$Ave.Daily.mg.L)
    mtext(paste(season.choice, model.choice), side = 3)
    dev.off()
  }
}
# ----

for (model.choice in names(all.models)){
  for (season.choice in names(all.models[[1]])){
    pdf(file = paste0(save.quick.plots.folder,"/boxplots_",season.choice,"_",model.choice,".pdf"), width = 6.5, height = 3)
    par(mar = c(3,3,2,.5), oma = c(.1,.1,.1,.1),mfrow = c(1,2))
    before <- all.models[[model.choice]][[season.choice]]$Cumulative.mg.L[all.models[[model.choice]][[season.choice]]$Year <= 2009]
    after <- all.models[[model.choice]][[season.choice]]$Cumulative.mg.L[all.models[[model.choice]][[season.choice]]$Year > 2009]
    boxplot(list("Before\n(1995-2009)" = before, "After\n(2010-2020)" = after), notch = F, range = 0, lty = 1, axes = F)
    axis(side = 2, las = 2)
    box()
    mtext(text = c("Before\n(1995-2009)","After\n(2010-2020)"), side = 1, line = 1.5, at = c(1,2), xpd = T)
    points(x = jitter(rep(1,length(before)), factor = 15), y = before, pch = 21, bg = adjustcolor("black",.3))
    points(x = 1 + jitter(rep(1,length(after)), factor = 15), y = after, pch = 21, bg = adjustcolor("black",.3))
    mtext(text = paste(season.choice, model.choice), side = 3, outer = T, line = -1)
    mtext(text = "Cumulative", side = 3, outer = F, line = 0)
    before <- all.models[[model.choice]][[season.choice]]$Ave.Daily.mg.L[all.models[[model.choice]][[season.choice]]$Year <= 2009]
    after <- all.models[[model.choice]][[season.choice]]$Ave.Daily.mg.L[all.models[[model.choice]][[season.choice]]$Year > 2009]
    boxplot(list("Before\n(1995-2009)" = before, "After\n(2010-2020)" = after), notch = F, range = 0, lty = 1, axes = F)
    axis(side = 2, las = 2)
    box()
    mtext(text = c("Before\n(1995-2009)","After\n(2010-2020)"), side = 1, line = 1.5, at = c(1,2), xpd = T)
    points(x = jitter(rep(1,length(before)), factor = 15), y = before, pch = 21, bg = adjustcolor("black",.3))
    points(x = 1 + jitter(rep(1,length(after)), factor = 15), y = after, pch = 21, bg = adjustcolor("black",.3))
    mtext(text = "Average", side = 3, outer = F, line = 0)
    dev.off()
  }
}
