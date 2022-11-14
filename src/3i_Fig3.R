# RRR

save.plot <- "figs_publication/Fig3.pdf" # season start == strat start, season start is 0.1 kg/m3, add panel labels
save.data <- "figs_publication/Fig3_data.csv"
save.stats <- "figs_publication/Fig3_stats.csv"

phyto <- readRDS("data_processed/3g_biomass_metrics_by_season-measured_values.rds")

anoxia <- read.csv(file = "data_processed/timelag.csv")

# ---- format biomass data ----

phyto <- phyto$mean

n.phyto <- data.frame("year" = phyto$ice$Year,
                      "spring" = phyto$spring$Total.Days,
                      "stratified" = phyto$stratified$Total.Days,
                      "fall" = phyto$fall$Total.Days,
                      "ice" = phyto$ice$Total.Days)

phyto <- data.frame("year" = phyto$ice$Year,
                    "spring" = phyto$spring$Ave.Daily.mg.L,
                    "stratified" = phyto$stratified$Ave.Daily.mg.L,
                    "fall" = phyto$fall$Ave.Daily.mg.L,
                    "ice" = phyto$ice$Ave.Daily.mg.L)

i.pre <- phyto$year < 2010
i.post <- phyto$year >= 2010

phyto.groups <- list("ice.pre" = phyto$ice[i.pre],
                     "ice.post" = phyto$ice[i.post],
                     "spring.pre" = phyto$spring[i.pre],
                     "spring.post" = phyto$spring[i.post],
                     "stratified.pre" = phyto$stratified[i.pre],
                     "stratified.post" = phyto$stratified[i.post],
                     "fall.pre" = phyto$fall[i.pre],
                     "fall.post" = phyto$fall[i.post])

# ---- format anoxia data ----

anoxia.groups <- list("pre" = anoxia$timelag[anoxia$year < 2010],
                      "post" = anoxia$timelag[anoxia$year >= 2010])
# ---- summary stats ----

phyto.summary <- data.frame("Metric" = c("mean.pre","mean.post","sd.pre","sd.post","fold.change","sd.fold.change","p.value"),
                            "Ice" = NA,
                            "Spring" = NA,
                            "Stratified" = NA,
                            "Fall" = NA,
                            "Days.till.anoxia" = NA)

# my.t <- t.test(x = phyto.groups$spring.pre, y = phyto.groups$spring.post)
# my.t <- kruskal.test(x = list(x = phyto.groups$spring.pre, y = phyto.groups$spring.post))
my.t <- wilcox.test(x = phyto.groups$spring.pre, y = phyto.groups$spring.post)
phyto.summary$Spring <- c(mean(phyto.groups$spring.pre, na.rm = T),
                          mean(phyto.groups$spring.post, na.rm = T),
                          sd(phyto.groups$spring.pre, na.rm = T),
                          sd(phyto.groups$spring.post, na.rm = T),
                          NA, NA,
                          my.t$p.value)

# my.t <- t.test(x = phyto.groups$stratified.pre, y = phyto.groups$stratified.post)
# my.t <- kruskal.test(x = list(x = phyto.groups$stratified.pre, y = phyto.groups$stratified.post))
my.t <- wilcox.test(x = phyto.groups$stratified.pre, y = phyto.groups$stratified.post)
phyto.summary$Stratified <- c(mean(phyto.groups$stratified.pre, na.rm = T),
                              mean(phyto.groups$stratified.post, na.rm = T),
                              sd(phyto.groups$stratified.pre, na.rm = T),
                              sd(phyto.groups$stratified.post, na.rm = T),
                              NA, NA,
                              my.t$p.value)

# my.t <- t.test(x = phyto.groups$fall.pre, y = phyto.groups$fall.post)
# my.t <- kruskal.test(x = list(x = phyto.groups$fall.pre, y = phyto.groups$fall.post))
my.t <- wilcox.test(x = phyto.groups$fall.pre, y = phyto.groups$fall.post)
phyto.summary$Fall <- c(mean(phyto.groups$fall.pre, na.rm = T),
                        mean(phyto.groups$fall.post, na.rm = T),
                        sd(phyto.groups$fall.pre, na.rm = T),
                        sd(phyto.groups$fall.post, na.rm = T),
                        NA, NA,
                        my.t$p.value)

# my.t <- t.test(x = phyto.groups$ice.pre, y = phyto.groups$ice.post)
# my.t <- kruskal.test(x = list(x = phyto.groups$ice.pre, y = phyto.groups$ice.post))
my.t <- wilcox.test(x = phyto.groups$ice.pre, y = phyto.groups$ice.post)
phyto.summary$Ice <- c(mean(phyto.groups$ice.pre, na.rm = T),
                       mean(phyto.groups$ice.post, na.rm = T),
                       sd(phyto.groups$ice.pre, na.rm = T),
                       sd(phyto.groups$ice.post, na.rm = T),
                       NA, NA,
                       my.t$p.value)

# my.t <- t.test(x = anoxia.groups$pre, y = anoxia.groups$post) 
# my.t <- kruskal.test(x = list(x = anoxia.groups$pre, y = anoxia.groups$post))
my.t <- wilcox.test(x = anoxia.groups$pre, y = anoxia.groups$post)
phyto.summary$Days.till.anoxia <- c(mean(anoxia.groups$pre, na.rm = T),
                                    mean(anoxia.groups$post, na.rm = T),
                                    sd(anoxia.groups$pre, na.rm = T),
                                    sd(anoxia.groups$post, na.rm = T),
                                    NA, NA,
                                    my.t$p.value)

phyto.summary[5,-1] <- phyto.summary[2,-1] / phyto.summary[1,-1]
phyto.summary[6,-1] <- phyto.summary[5,-1] * sqrt((phyto.summary[3,-1]/phyto.summary[1,-1])^2 + (phyto.summary[4,-1]/phyto.summary[2,-1])) 

# ---- quick look ----

par(mfrow = c(1,2))
boxplot(phyto.groups, at = c(1,2,4,5,7,8,10,11), range = 0, lty = 1)
stripchart(x = phyto.groups, at = c(1,2,4,5,7,8,10,11), vertical = T, pch = 19, method = "jitter", cex = .5, jitter = .15, add = T)
boxplot(anoxia.groups, horizontal = F)
stripchart(x = anoxia.groups, at = c(1,2), vertical = T, add = T, pch = 19, method = "jitter", cex = .7, jitter = .25)



# ---- save tables ----

write.csv(x = phyto, file = save.data)
write.csv(x = phyto.summary, file = save.stats)


# ---- save plot ----

col.pre <- "steelblue"
col.post <- "orange3"

pdf(file = save.plot, width = 6.5, height = 3)

# par(fig = c(0,.6,0,1), mar = c(2,2.75,.5,.5))
par(fig = c(0,.6,0,1), mar = c(2.75,2.75,.5,.5)) # accommodate new season names
boxplot(x = phyto.groups, at = c(1,2,4,5,7,8,10,11), range = 1.5, ylim = c(0,8),
        lty = 1, axes = F, ann = F, col = c(col.pre,col.post), outpch = 21, outbg = c(col.pre,col.post), outcex = .7)
box()
axis(side = 1, at = c(1.5,4.5,7.5,10.5), lwd = 0, lwd.ticks = 1, labels = F)
# axis(side = 1, at = c(1.5,4.5,7.5,10.5), lwd = 0, labels = c("Ice-on","Spring","Stratified","Fall"), line = -.25)
mtext(text = c("Ice","Spring\nmixed","Stratified","Fall\nmixed"), side = 1, at = c(1.5,4.5,7.5,10.5), line = c(.75, 1.6))
axis(side = 2, at = c(0,2,4,6,8), lwd = 0, lwd.ticks = 1, labels = F)
axis(side = 2, at = c(0,2,4,6,8), lwd = 0, labels = T, las = 2, line = -.25)
# mtext(text = "Phytoplankton Biomass (mg/L)", side = 2, line = 1.75)
mtext(text = "Phytoplankton Biomass (mg/L)", side = 2, line = 1.75, at = 3.5) # move out of way of "A" label
# Must note in caption: 1 outlier point is outside the plot area (Fall 2011 = 14 mg/L)

# par(fig = c(.6,.9,0,1), new = T, mar = c(2,5,.5,1.5))
par(fig = c(.6,.9,0,1), new = T,  mar = c(2.75,5,.5,1.5)) # accommodate new season names
boxplot(anoxia.groups, horizontal = F, col = c(col.pre, col.post), range = 1.5, axes = F, ann = F, lty = 1, outpch = 21, outbg = c(col.pre,col.post), outcex = .7)
box()
axis(side = 1, at = 1.5, labels = F, lwd = 0, lwd.ticks = 1)
# axis(side = 1, at = 1.5, lwd = 0, labels = "Anoxia lag", line = -.25)
mtext(text = c("Anoxia lag"), side = 1, at = c(1.5), line = c(.75))
# axis(side = 2, lwd = 0, lwd.ticks = 1, at = c(0,15,30,45), labels = F)
axis(side = 2, lwd = 0, lwd.ticks = 1, at = c(15,30,45,60), labels = F)
# axis(side = 2, lwd = 0, at = c(0,15,30,45), labels = T, las = 2, line = -.25)
axis(side = 2, lwd = 0, at = c(15,30,45,60), labels = T, las = 2, line = -.25)
mtext(text = "Lag between stratification\nand anoxia (days)", side = 2, line = 2.25)


par(fig = c(.9,1,0,1), new = TRUE, mar = c(.1,.1,.1,.1))
plot(1:10,1:10, type = "n", axes = F, ann = F)
lab.1.ht <- 8
lab.2.ht <- 7
rect.size <- .25

rect(xleft = 0, xright = 3, ybottom = lab.1.ht - rect.size, ytop = lab.1.ht + rect.size, col = col.pre, xpd = NA)
rect(xleft = 0, xright = 3, ybottom = lab.2.ht - rect.size, ytop = lab.2.ht + rect.size, col = col.post, xpd = NA)
text(x = 4, y = lab.1.ht, label = "Pre", adj = 0)
text(x = 4, y = lab.2.ht, label = "Post", adj = 0)

mtext(text = "A", side = 3, outer = T, line = -1.25, at = .015, cex = 1.5)
mtext(text = "B", side = 3, outer = T, line = -1.25, at = .64, cex = 1.5)

dev.off()
