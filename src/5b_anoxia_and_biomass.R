# RRR
# this is kind of ugly because I'm not good a ggplot, but to get the idea:

# ---- set up ----

library(lubridate)
library(tidyverse)
library(gridExtra)

phyto <- readRDS("data_processed/3a_phyto_list.rds")
anox <- readRDS("data_input/14-DO_profiles_with_more_oxycline_stats-colnames_updated.rds") # pull from data I curated elsewhere- just quick for now
strat <- readRDS("data_processed/3g_stratification_dates.rds")

# ---- format data ----

strat <- strat$mean

phyto <- phyto$tot
phyto$yday <- yday(phyto$date)
phyto$year <- year(phyto$date)
phyto <- phyto[phyto$year %in% strat$year, ]
phyto$days.since.strat <- NA
for (yr in strat$year){
  phyto$days.since.strat[phyto$year == yr] <- phyto$yday[phyto$year == yr] - strat$start[strat$year == yr]
}
phyto$invasion <- "pre"
phyto$invasion[phyto$year >= 2010] <- "post"
head(phyto)

anox <- anox[ ,c("Sample.DateTime","Top.Anoxic")]
head(anox)
anox <- aggregate(x = anox$Top.Anoxic, by = list(anox$Sample.DateTime), FUN = mean, na.rm = T)
head(anox)
anox <- anox[!is.na(anox$x), ]
anox$x[abs(anox$x) > 20] <- 20 #  make max depth 20 to match MO profiles
colnames(anox) <- c("date","top.anoxic")
anox$yday <- yday(anox$date)
anox$year <- year(anox$date)
anox <- anox[anox$year %in% strat$year, ]
anox$days.since.strat <- NA
for (yr in strat$year){
  anox$days.since.strat[anox$year == yr] <- anox$yday[anox$year == yr] - strat$start[strat$year == yr]
}
anox$invasion <- "pre"
anox$invasion[anox$year >= 2010] <- "post"
head(anox)
summary(anox$top.anoxic)

# ---- plot, attempt ggplot ----

p.anox <- ggplot(data = anox, aes(x = days.since.strat, y = top.anoxic, color = invasion))+
  scale_fill_manual(values = c("orange3", "steelblue"))+
  scale_color_manual(values = c("orange3", "steelblue"))+
  geom_point(aes(fill = invasion), shape = 21, color = "black", size = 2, alpha = .5)+
  geom_path(aes(group = year), alpha = .5)+
  geom_smooth(aes(group = invasion, fill = invasion), method = "loess", alpha = .6)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  coord_cartesian(xlim = c(-10,200), ylim = c(20,6))+
  scale_y_reverse()
p.anox

p.phyto <- ggplot(data = phyto, aes(x = days.since.strat, y = biomass, color = invasion))+
  scale_fill_manual(values = c("orange3", "steelblue"))+
  scale_color_manual(values = c("orange3", "steelblue"))+
  geom_point(aes(fill = invasion), shape = 21, color = "black", size = 2, alpha = .5)+
  geom_path(aes(group = year),alpha = .5)+
  geom_smooth(aes(group = invasion, fill = invasion), method = "loess", alpha = .6)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  coord_cartesian(xlim = c(-100,200), ylim = c(0,25))
p.phyto

grid.arrange(p.phyto,p.anox, nrow = 1)
