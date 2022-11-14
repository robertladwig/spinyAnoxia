# RRR
# look at biomass taxa individually over time

library(lubridate)

phyto.list <- readRDS("robin-data/2022-06-08_phyto_list.rds")
names(phyto.list)

key <- readRDS("robin-data/2022-07-25_season_dates/seasons_by_sample.rds")

my.dates <- phyto.list$tot$date
index.pre <- year(my.dates) < 2010
index.post <- year(my.dates) >= 2010

i.ice <- key$Season == "ice-on"
i.spring <- key$Season == "spring"
i.strat <- key$Season == "stratified"
i.fall <- key$Season == "fall"

x.ax <- unique(year(my.dates))
x.ax.ticks <- parse_date_time(paste0("1-1-",x.ax), "mdy")
index <- seq.int(from = 1, by = 5, length.out = 6)
x.ax.lab.loc <- x.ax.ticks[index]
x.ax.lab <- x.ax[index]

t = 2
r = 1

# manually add total plot - diff structure
t = 1
folder.name <- paste0("plots/2022-07-25_taxa_over_time_plots/",t,"-",names(phyto.list)[t])
my.tax <- phyto.list[[t]]$biomass
my.name <- "Total"
# and run the figure part of the loop only


for (t in 2:length(phyto.list)){
  folder.name <- paste0("plots/2022-07-25_taxa_over_time_plots/",t,"-",names(phyto.list)[t])
  for (r in 1:nrow(phyto.list[[t]])){
    my.tax <- phyto.list[[t]][r, ]
    my.name <- row.names(phyto.list[[t]])[r]
    my.tax[is.na(my.tax)] <- 0
    
    
    # ----
    pdf(file = file.path(folder.name,paste0(my.name,".pdf")), width = 7.5, height = 10)
    
    # all dates
    par(fig = c(0,.8,.8,1)) 
    par(mar = c(1.25,2.5,1,1))
    plot(x = my.dates, y = my.tax, type = "n", ann = F, axes = F)
    box()
    axis(side = 1, at = x.ax.ticks, lwd = 0, lwd.ticks = 1, labels = F, line = 0, tck = -.025)
    axis(side = 1, at = x.ax.lab.loc, lwd = 0, lwd.ticks = 1, labels = F, line = 0, tck = -.05)
    axis(side = 2, lwd = 0, lwd.ticks = 1, tck = -.03, labels = F, line = 0)
    axis(side = 2, lwd = 0, labels = T, line = -.5, las = 2)
    mtext(text = "Biomass (mg/L)", side = 2, line = 1.5)
    lines(x = my.dates, y = my.tax)
    points(x = my.dates, y = my.tax, pch = 19, cex = .3)
    
    mtext(text = my.name, side = 3)
    
    par(fig = c(.8,1,.8,1), mar = c(1.25,1,1,1), new = T)
    boxplot(x = list("pre" = my.tax[index.pre], "post" = my.tax[index.post]), ann = F, axes = F)
    box()
    axis(side = 1, at = c(1,2), lwd = 0, lwd.ticks = 1, tck = -.05, labels = F)
    axis(side = 2, lwd = 0, lwd.ticks = 1, tck = -.03, labels = F)
    axis(side = 2, lwd = 0, labels = T, line = -.5, las = 2)
    
    # ice
    par(fig = c(0,.8,.6,.8), new = T) 
    par(mar = c(1.25,2.5,1,1))
    plot(x = my.dates[i.ice], y = my.tax[i.ice], type = "n", ann = F, axes = F)
    box()
    axis(side = 1, at = x.ax.ticks, lwd = 0, lwd.ticks = 1, labels = F, line = 0, tck = -.025)
    axis(side = 1, at = x.ax.lab.loc, lwd = 0, lwd.ticks = 1, labels = F, line = 0, tck = -.05)
    axis(side = 2, lwd = 0, lwd.ticks = 1, tck = -.03, labels = F, line = 0)
    axis(side = 2, lwd = 0, labels = T, line = -.5, las = 2)
    mtext(text = "Ice-On (mg/L)", side = 2, line = 1.5)
    lines(x = my.dates[i.ice], y = my.tax[i.ice])
    points(x = my.dates[i.ice], y = my.tax[i.ice], pch = 19, cex = .3)
   
    par(fig = c(.8,1,.6,.8), mar = c(1.25,1,1,1), new = T)
    boxplot(x = list("pre" = my.tax[index.pre & i.ice], "post" = my.tax[index.post & i.ice]), ann = F, axes = F)
    box()
    axis(side = 1, at = c(1,2), lwd = 0, lwd.ticks = 1, tck = -.05, labels = F)
    axis(side = 2, lwd = 0, lwd.ticks = 1, tck = -.03, labels = F)
    axis(side = 2, lwd = 0, labels = T, line = -.5, las = 2)
    
    # spring
    par(fig = c(0,.8,.4,.6), new = T) 
    par(mar = c(1.25,2.5,1,1))
    plot(x = my.dates[i.spring], y = my.tax[i.spring], type = "n", ann = F, axes = F)
    box()
    axis(side = 1, at = x.ax.ticks, lwd = 0, lwd.ticks = 1, labels = F, line = 0, tck = -.025)
    axis(side = 1, at = x.ax.lab.loc, lwd = 0, lwd.ticks = 1, labels = F, line = 0, tck = -.05)
    axis(side = 2, lwd = 0, lwd.ticks = 1, tck = -.03, labels = F, line = 0)
    axis(side = 2, lwd = 0, labels = T, line = -.5, las = 2)
    mtext(text = "Spring (mg/L)", side = 2, line = 1.5)
    lines(x = my.dates[i.spring], y = my.tax[i.spring])
    points(x = my.dates[i.spring], y = my.tax[i.spring], pch = 19, cex = .3)
    
    par(fig = c(.8,1,.4,.6), mar = c(1.25,1,1,1), new = T)
    boxplot(x = list("pre" = my.tax[index.pre & i.spring], "post" = my.tax[index.post & i.spring]), ann = F, axes = F)
    box()
    axis(side = 1, at = c(1,2), lwd = 0, lwd.ticks = 1, tck = -.05, labels = F)
    axis(side = 2, lwd = 0, lwd.ticks = 1, tck = -.03, labels = F)
    axis(side = 2, lwd = 0, labels = T, line = -.5, las = 2)
    
    # strat
    par(fig = c(0,.8,.2,.4), new = T) 
    par(mar = c(1.25,2.5,1,1))
    plot(x = my.dates[i.strat], y = my.tax[i.strat], type = "n", ann = F, axes = F)
    box()
    axis(side = 1, at = x.ax.ticks, lwd = 0, lwd.ticks = 1, labels = F, line = 0, tck = -.025)
    axis(side = 1, at = x.ax.lab.loc, lwd = 0, lwd.ticks = 1, labels = F, line = 0, tck = -.05)
    axis(side = 2, lwd = 0, lwd.ticks = 1, tck = -.03, labels = F, line = 0)
    axis(side = 2, lwd = 0, labels = T, line = -.5, las = 2)
    mtext(text = "Stratified (mg/L)", side = 2, line = 1.5)
    lines(x = my.dates[i.strat], y = my.tax[i.strat])
    points(x = my.dates[i.strat], y = my.tax[i.strat], pch = 19, cex = .3)
    
    par(fig = c(.8,1,.2,.4), mar = c(1.25,1,1,1), new = T)
    boxplot(x = list("pre" = my.tax[index.pre & i.strat], "post" = my.tax[index.post & i.strat]), ann = F, axes = F)
    box()
    axis(side = 1, at = c(1,2), lwd = 0, lwd.ticks = 1, tck = -.05, labels = F)
    axis(side = 2, lwd = 0, lwd.ticks = 1, tck = -.03, labels = F)
    axis(side = 2, lwd = 0, labels = T, line = -.5, las = 2)
    
    # fall
    par(fig = c(0,.8,0,.2), new = T) 
    par(mar = c(1.25,2.5,1,1))
    plot(x = my.dates[i.fall], y = my.tax[i.fall], type = "n", ann = F, axes = F)
    box()
    axis(side = 1, at = x.ax.ticks, lwd = 0, lwd.ticks = 1, labels = F, line = 0, tck = -.025)
    axis(side = 1, at = x.ax.lab.loc, lwd = 0, lwd.ticks = 1, labels = F, line = 0, tck = -.05)
    axis(side = 1, lwd = 0, at = x.ax.lab.loc, labels = x.ax.lab, line = -.75, xpd = NA)
    axis(side = 2, lwd = 0, lwd.ticks = 1, tck = -.03, labels = F, line = 0)
    axis(side = 2, lwd = 0, labels = T, line = -.5, las = 2)
    mtext(text = "Fall (mg/L)", side = 2, line = 1.5)
    lines(x = my.dates[i.fall], y = my.tax[i.fall])
    points(x = my.dates[i.fall], y = my.tax[i.fall], pch = 19, cex = .3)
    
    par(fig = c(.8,1,0,.2), mar = c(1.25,1,1,1), new = T)
    boxplot(x = list("pre" = my.tax[index.pre & i.fall], "post" = my.tax[index.post & i.fall]), ann = F, axes = F)
    box()
    axis(side = 1, at = c(1,2), lwd = 0, lwd.ticks = 1, tck = -.05, labels = F)
    axis(side = 1, at = c(1,2), lwd = 0, labels = c("pre","post"), line = -.75)
    axis(side = 2, lwd = 0, lwd.ticks = 1, tck = -.03, labels = F)
    axis(side = 2, lwd = 0, labels = T, line = -.5, las = 2)
    
    
    dev.off()
    
    # ----
    
    
  }
}



