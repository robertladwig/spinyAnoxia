# RRR
# this is kind of ugly because I'm not good a ggplot, but to get the idea:

# ---- set up ----

library(lubridate)
library(tidyverse)
library(patchwork)

phyto <- readRDS("data_processed/3a_phyto_list.rds")
anox <- readRDS("data_processed/0n_combined_DO-full_profiles.rds") # pull from data I curated elsewhere- just quick for now
strat <- readRDS("data_processed/3g_stratification_dates.rds")
strat <- strat$mean

plot.file <- "figs_publication/Fig5-robin.pdf"

# ---- format phyto ----

phyto <- phyto$tot
phyto$yday <- yday(phyto$date)
phyto$year <- year(phyto$date)
phyto$days.since.strat <- NA
for (yr in strat$year){
  phyto$days.since.strat[phyto$year == yr] <- phyto$yday[phyto$year == yr] - strat$start[strat$year == yr]
}
phyto = phyto |> mutate(invasion = if_else(year < 2010, 'Pre','Post')) |> 
  mutate(invasion = factor(invasion, levels = c('Pre','Post')))


head(phyto)

# ---- remove spring anoxia sketchy ones ----

# 2007-5-14
# 2015-5-24
# 2015-5-28

index <- anox$Year == 2007 & anox$Month == 5 & anox$Day == 14
plot(x = anox$DO.mg.L[index], y = -anox$Depth.m[index])
# profile looks fine, but clearly stratified to ~ 10 m when the lake supposedly just stratified 8.75 days earlier
# so removing because I think there is no explanation for how this cannot be wrong?
anox <- anox[!index, ]

index <- anox$Year == 2015 & anox$Month == 5 & anox$Day == 24
plot(x = anox$DO.mg.L[index], y = -anox$Depth.m[index])
# looks like shit, how did that make it past my screening
anox <- anox[!index, ]

index <- anox$Year == 2015 & anox$Month == 5 & anox$Day == 28
plot(x = anox$DO.mg.L[index], y = -anox$Depth.m[index])
# obviously something wrong with the probe
anox <- anox[!index, ]

# ---- remove summer sketchy depth = 0 ones ----

# 2013-07-29
# 2016-07-06
# 2016-07-08

index <- anox$Year == 2013 & anox$Month == 7 & anox$Day == 29
plot(x = anox$DO.mg.L[index], y = -anox$Depth.m[index])
# super wack, how did this make it past screening I freaking already checked them all
anox <- anox[!index, ]

index <- anox$Year == 2016 & anox$Month == 7 & anox$Day == 06
plot(x = anox$DO.mg.L[index], y = -anox$Depth.m[index])
# super wack, how did this make it past screening I freaking already checked them all
anox <- anox[!index, ]

index <- anox$Year == 2016 & anox$Month == 7 & anox$Day == 08
plot(x = anox$DO.mg.L[index], y = -anox$Depth.m[index])
# super wack, how did this make it past screening I freaking already checked them all
anox <- anox[!index, ]

# ---- format anox ----

anox$yday <- yday(anox$Date)
anox <- anox[anox$Depth.m <= 20, ] # trim profiles to extend to 20 m only
anox$days.since.strat <- NA
for (yr in strat$year){
  anox$days.since.strat[anox$Year == yr] <- anox$yday[anox$Year == yr] - strat$start[strat$year == yr]
}
anox <- anox[order(anox$Date), ]

anox <- anox |> 
  group_by(Date) |> 
  dplyr::filter(DO.mg.L < 1.5) |> 
  dplyr::filter(Month >= 5) |> 
  dplyr::filter(Depth.m == min(Depth.m))  |> 
  mutate(invasion = if_else(Year < 2010, 'Pre','Post')) |> 
  mutate(invasion = factor(invasion, levels = c('Pre','Post')))
head(anox)

# ---- plot, attempt ggplot - align by strat ----

p.anox <- ggplot(data = anox[!is.na(anox$days.since.strat), ], aes(x = days.since.strat, y = Depth.m, color = invasion))+
  scale_fill_manual(values = c(col.post, col.pre))+
  scale_color_manual(values = c(col.post, col.pre))+
  geom_point(aes(fill = invasion), shape = 21, color = "black", size = 1, alpha = .5)+
  geom_path(aes(group = Year), alpha = .5)+
  geom_smooth(aes(group = invasion, fill = invasion), method = "loess", alpha = .6)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  coord_cartesian(xlim = c(-10,200), ylim = c(20,6))+
  scale_y_reverse()

p.phyto <- ggplot(data = phyto, aes(x = days.since.strat, y = biomass, color = invasion))+
  scale_fill_manual(values = c(col.post, col.pre))+
  scale_color_manual(values = c(col.post, col.pre))+
  geom_point(aes(fill = invasion), shape = 21, color = "black", size = 1, alpha = .5)+
  geom_path(aes(group = year),alpha = .5)+
  geom_smooth(aes(group = invasion, fill = invasion), method = "loess", alpha = .6)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  coord_cartesian(xlim = c(-100,200), ylim = c(.001,25))+
  scale_y_continuous(trans = "log10", name = "log biomass (mg/L)")

p.anox / p.phyto

# ---- plot, attempt ggplot - align by date ----

x.lab.yday <- yday(parse_date_time(x = paste(2:11,1), orders = "md"))
x.lab <- month(parse_date_time(x = paste(2:11,1), orders = "md"), label = T)

p.anox <- ggplot(data = anox, aes(x = yday, y = Depth.m, fill = invasion))+
  geom_point(shape = 21, size = 0.5, alpha = .5, stroke = 0.1)+
  geom_path(aes(group = Year, color = invasion), alpha = .5, linewidth = .3) +
  geom_smooth(aes(group = invasion, color = invasion), method = "gam", alpha = .7) +
  theme_bw()+
  theme(panel.grid = element_blank(), panel.border = element_rect(linewidth = 1))+
  coord_cartesian(xlim = c(50,300), ylim = c(20,5), expand = F)+
  scale_fill_manual(values = c('steelblue','orange3')) +
  scale_color_manual(values = c('steelblue','orange3')) +
  scale_x_continuous(name = element_blank(), breaks = x.lab.yday, labels = x.lab)+
  scale_y_reverse(name = "Depth Anoxic Layer (m)")+
  theme(axis.title.x = element_blank(), 
        legend.key.width = unit(0.5, 'cm'),
        legend.key.height = unit(0.5, 'cm'),
        legend.position = c(0.15,0.8),
        legend.background = element_blank(),
        legend.title = element_blank()); p.anox

p.phyto <- ggplot(data = phyto, aes(x = yday, y = biomass, fill = invasion))+
  geom_point( shape = 21, size = 0.5, alpha = .5, stroke = 0.1)+
  geom_path(aes(group = year, color = invasion),alpha = .5, linewidth = .3, show.legend = F)+
  geom_smooth(aes(group = invasion, color = invasion), method = "gam", alpha = .7, show.legend = T)+
  theme_bw()+
  theme(panel.grid = element_blank(), panel.border = element_rect(linewidth = 1))+
  coord_cartesian(xlim = c(50,300), ylim = c(.01,25))+
  scale_fill_manual(values = c('steelblue','orange3')) +
  scale_color_manual(values = c('steelblue','orange3')) +
  scale_x_continuous(name = element_blank(), breaks = x.lab.yday, labels = x.lab)+
  scale_y_continuous(trans = "log10", name = "Biomass"~(mg~L^-1))+
  theme(axis.title.x = element_blank(), 
        legend.key.width = unit(0.5, 'cm'),
        legend.key.height = unit(0.5, 'cm'),
        legend.position = c(0.85,0.23),
        legend.title = element_blank())
# save plot 

p.phyto / p.anox + 
  plot_annotation(tag_levels = 'A', tag_suffix = ')') &
  theme(plot.tag = element_text(size  = 8)) 

ggsave('figs_publication/Fig5_HD.pdf', width = 3.4, height = 4)

