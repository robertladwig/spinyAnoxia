# RRR
# this is kind of ugly because I'm not good a ggplot, but to get the idea:

# ---- set up ----

library(lubridate)
library(tidyverse)
library(patchwork)
library(gridGraphics)

phyto <- readRDS("data_processed/3a_phyto_list.rds")
anox <- readRDS("data_processed/0n_combined_DO-full_profiles.rds") # pull from data I curated elsewhere- just quick for now
strat <- readRDS("data_processed/3g_stratification_dates.rds")
strat <- strat$mean
ice <- readRDS("data_processed/0c_ice_seasons_split_by_year.rds")

plot.file <- "figs_publication/Fig5-robin.pdf"
plot.file.vertical <- "figs_publication/Fig5-robin-vertical.pdf"
plot.file.vertical.anoxia.top <- "figs_publication/Fig5-robin-vertical-flipped.pdf"

col.pre <- adjustcolor("steelblue",.9)
col.post <- "orange3"

# ---- format phyto ----

phyto <- phyto$tot
phyto$yday <- yday(phyto$date)
phyto$year <- year(phyto$date)
phyto$days.since.strat <- NA
for (yr in strat$year){
  phyto$days.since.strat[phyto$year == yr] <- phyto$yday[phyto$year == yr] - strat$start[strat$year == yr]
}
phyto$invasion <- "pre"
phyto$invasion[phyto$year >= 2010] <- "post"
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
  filter(DO.mg.L < 1.5) |> 
  filter(Depth.m == min(Depth.m))  |> 
  mutate(invasion = if_else(Year < 2010, 'Pre','Post'))
head(anox)

# ---- plot - align by strat ----

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

# ---- plot - align by date ----

x.lab.yday <- yday(parse_date_time(x = paste(2:11,1), orders = "md"))
x.lab <- month(parse_date_time(x = paste(2:11,1), orders = "md"), label = T)

p.anox <- ggplot(data = anox, aes(x = yday, y = Depth.m, color = invasion))+
  scale_fill_manual(values = c(col.post, col.pre))+
  scale_color_manual(values = c(col.post, col.pre))+
  # geom_point(aes(fill = invasion), shape = 21, color = "black", size = 1, alpha = .5)+
  geom_path(aes(group = Year), alpha = .5, linewidth = .3, show.legend = F)+
  geom_smooth(aes(group = invasion, fill = invasion), method = "loess", alpha = .7, show.legend = F)+
  theme_bw()+
  theme(panel.grid = element_blank(), panel.border = element_rect(linewidth = 1))+
  coord_cartesian(xlim = c(150,300), ylim = c(20,5), expand = F)+
  scale_x_continuous(name = element_blank(), breaks = x.lab.yday, labels = x.lab)+
  scale_y_reverse()+
  scale_y_continuous(name = "Depth Anoxic Layer (m)")+
  guides(fill = guide_legend(override.aes = list(alpha = .9, linetype = "blank"), byrow = TRUE, reverse = T))+ # need to specify byrow = T for it to respect the legend spacing
  guides(color = guide_legend(reverse = T))+ # need both fill and color to be reversed
  theme(legend.title = element_blank(), legend.spacing.y = unit(.5,"cm"))+
  theme(axis.title.y = element_text(margin = margin(0,5,0,0)), axis.text.x = element_text(margin = margin(4,0,0,0), angle = 90, hjust = 1, vjust = .5))

p.phyto <- ggplot(data = phyto, aes(x = yday, y = biomass, color = invasion))+
  scale_fill_manual(values = c(col.post, col.pre))+
  scale_color_manual(values = c(col.post, col.pre))+
  # geom_point(aes(fill = invasion), shape = 21, color = "black", size = 1, alpha = .5)+
  geom_path(aes(group = year),alpha = .5, linewidth = .3, show.legend = F)+
  geom_smooth(aes(group = invasion, fill = invasion), method = "loess", alpha = .7, show.legend = T)+
  theme_bw()+
  theme(panel.grid = element_blank(), panel.border = element_rect(linewidth = 1))+
  coord_cartesian(xlim = c(50,300), ylim = c(.01,25))+
  scale_x_continuous(name = element_blank(), breaks = x.lab.yday, labels = x.lab)+
  scale_y_continuous(trans = "log10", name = "Biomass"~(mg~L^-1))+
  guides(fill = guide_legend(override.aes = list(alpha = .9, linetype = "blank"), byrow = TRUE, reverse = T))+ # need to specify byrow = T for it to respect the legend spacing
  guides(color = guide_legend(reverse = T))+ # need both fill and color to be reversed
  theme(legend.title = element_blank(), legend.spacing.y = unit(.25,"cm"), legend.margin = margin(c(0,0,2,0), unit = "cm"))+
  theme(axis.title.y = element_text(margin = margin(0,3,0,0)), axis.text.x = element_text(margin = margin(4,0,0,0), angle = 90, hjust = 1, vjust = .5))

# save plot 

pdf(file = plot.file, width = 6, height = 2.5)

p.phyto + plot_spacer() + p.anox + plot_layout(ncol = 3, widths = c(1,0,1), guides = "collect") +
  plot_annotation(tag_levels = 'A', tag_suffix = ')') & 
  theme(plot.margin = margin(.1,.05,0,.1, unit = "cm")) 

dev.off()

# ---- plot - vertical by date ----

x.lab.yday <- yday(parse_date_time(x = paste(2:11,1), orders = "md"))
x.lab <- month(parse_date_time(x = paste(2:11,1), orders = "md"), label = T)
anox.x.lim <- c(153,300)
phyto.x.lim <- c(61,300)
ave.strat.on <- mean(strat$start)
ave.ice.on <- mean(yday(ice$Spring.Ice.Off[ice$Summer.Year > 1994]))
y.log.ticks <- c(.01,.1,1,10)
y.log.labels <- c("0.01","0.1","1","10")

theme_robin <- function(){
    theme_bw()+
      theme(panel.grid = element_blank(), 
            panel.border = element_rect(linewidth = 1),
            legend.title = element_blank(), 
            legend.spacing.y = unit(.25,"cm"), legend.margin = margin(0,0,0,0),
            axis.text.x = element_text(margin = margin(2,0,0,0), angle = 0, hjust = .5, vjust = .5))
}

p.anox <- ggplot(data = anox, aes(x = yday, y = Depth.m, color = invasion))+
  geom_point(shape = 19, size = 0.5, alpha = .5, stroke = 0, show.legend = F)+
  geom_path(aes(group = Year), alpha = .5, linewidth = .3, show.legend = F)+
  geom_smooth(aes(group = invasion, fill = invasion), method = "loess", alpha = .7, show.legend = T)+

  coord_cartesian(xlim = anox.x.lim, ylim = c(20,5), expand = F)+
  scale_x_continuous(name = element_blank(), breaks = x.lab.yday, labels = x.lab)+
  scale_y_reverse()+
  scale_y_continuous(name = "Depth Anoxic\nLayer (m)")+
  
  theme_robin()+ 
  theme(axis.title.y = element_text(margin = margin(0,5,0,0)),
        legend.key.size = unit(.4,"cm"))+
  guides(fill = guide_legend(override.aes = list(alpha = .9, linetype = "blank"), byrow = TRUE, reverse = T, label.position = "right"))+# need to specify byrow = T for it to respect the legend spacing 
  guides(color = guide_legend(reverse = T, label.position = "right"))+ # need both fill and color to be reversed
  scale_fill_manual(values = c(col.post, col.pre))+
  scale_color_manual(values = c(col.post, col.pre))
  

  
p.phyto <- ggplot(data = phyto, aes(x = yday, y = biomass, color = invasion))+
  geom_vline(xintercept = ave.strat.on, linewidth =  1, color = "grey80")+
  geom_vline(xintercept = ave.ice.on, linewidth =  1, color = "grey80")+
  annotate(geom = "text", x = 73, y = .006, label = "Ice", size = 3, color = "grey80", hjust = .5)+
  annotate(geom = "text", x = 105, y = .006, label = "Mixed", size = 3, color = "grey80", hjust = .5)+
  annotate(geom = "text", x = 149, y = .006, label = "Stratified", size = 3, color = "grey80", hjust = .5)+
  
  geom_point(shape = 19, size = 0.5, alpha = .5, stroke = 0, show.legend = F)+
  geom_path(aes(group = year), alpha = .5, linewidth = .3, show.legend = F)+
  geom_smooth(aes(group = invasion, fill = invasion), method = "loess", alpha = .7, show.legend = F)+
  
  coord_cartesian(xlim = phyto.x.lim, ylim = c(.003,40), expand = F)+
  scale_x_continuous(name = element_blank(), breaks = x.lab.yday, labels = x.lab)+
  scale_y_continuous(trans = "log10", name = "Biomass"~(mg~L^-1), breaks = y.log.ticks, labels = y.log.labels)+
  theme_robin()+
  theme(axis.title.y = element_text(margin = margin(0,0,0,0)))+
  scale_fill_manual(values = c(col.post, col.pre))+
  scale_color_manual(values = c(col.post, col.pre))
  
  


# save plot 

pdf(file = plot.file.vertical, width = 3.6, height = 3)
m <- c(
  area(t = 1, b = 1, l = 1, r = 2),
  area(t = 2, b = 2, l = 1, r = 1),
  area(t = 2, b = 2, l = 2, r = 2)
)
p.phyto + guide_area() + p.anox + plot_layout(guides = "collect", design = m, widths = c(1.3,5)) +
  plot_annotation(tag_levels = 'A', tag_suffix = ') ') & 
  theme(plot.margin = margin(.1,.05,0,0, unit = "cm"),
        plot.tag = element_text(size  = 8),
        legend.position = "left", 
        axis.title.y = element_text(size = 8),
        axis.text = element_text(size = 8),
        legend.text = element_text(size = 8)) 

dev.off()

# It's impossible to have the legend not end up aligned to a "plot area" 
# meaning that it can't just sit under the top axis space. 
# To make June and June still line up, I widened the plot. 
# ggplot and it's stupid rules

# save plot with reversed A and B (anoxia on top)

pdf(file = plot.file.vertical.anoxia.top, width = 3.6, height = 3)
m <- c(
  area(t = 1, b = 1, l = 1, r = 1),
  area(t = 1, b = 1, l = 2, r = 2),
  area(t = 2, b = 2, l = 1, r = 2)
)
guide_area() + p.anox + p.phyto + plot_layout(guides = "collect", design = m, widths = c(1.3,5)) +
  plot_annotation(tag_levels = 'A', tag_suffix = ') ') & 
  theme(plot.margin = margin(.1,.05,0,0, unit = "cm"),
        plot.tag = element_text(size  = 8),
        legend.position = "left", 
        axis.title.y = element_text(size = 8),
        axis.text = element_text(size = 8),
        legend.text = element_text(size = 8)) 

dev.off()
