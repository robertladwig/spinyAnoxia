# RRR
# re-make figure 3a with and without using data from the linear interpolation- Paul was concerned about using modeled data
# Goal is to visualize before and after biomass by season
# about the data being used:
# it is the LTER biomass (mg/L) data totaled across all phytoplankton counted
# then seasons were assigned to each observation day:
#   - ice-on based on LTER data
#   - stratified based on Roberts calculated intervals
#   - spring is between ice-off and stratification
#   - fall is between stratification and ice on
# then for each year, the average of each season's biomass observations was calculated

# Note, for L&O letters, small figs are 80 mm (~3 in) wide, and large figures are 180 mm (~7 in) wide

interp <- readRDS("data_processed/3g_biomass_metrics_by_season-interp_values.rds")
names(interp)
names(interp$linear)
head(interp$linear$spring)

biomass <- readRDS("data_processed/3g_biomass_metrics_by_season-measured_values.rds")
names(biomass)
names(biomass$mean)
head(biomass$mean$spring)

measured <- readRDS("data_processed/3g_biomass_metrics_by_season-individual_measurements")
names(measured)
names(measured$mean)
head(measured$mean$spring)

plot.1 <- "figs/3h_Seasonal_Biomass_Change.pdf"
plot.1.jitter <- "figs/3h_Seasonal_Biomass_Change_jitter.pdf"

# ---- prep data- interpolated ----

m <- "mean"

ice <- interp[[m]]$ice
spring <- interp[[m]]$spring
strat <- interp[[m]]$stratified
fall <- interp[[m]]$fall

ice.cutoff <- 2
spring.cutoff <- 2
strat.cutoff <- 2
fall.cutoff <- 2
ice <- ice[!ice$Measured.Days < ice.cutoff, ]
spring <- spring[!spring$Measured.Days < spring.cutoff, ]
strat <- strat[!strat$Measured.Days < strat.cutoff, ]
fall <- fall[!fall$Measured.Days < fall.cutoff, ]

spring.pre <- spring[spring$Year <= 2009, ]
spring.post <- spring[spring$Year >= 2010, ]
strat.pre <- strat[strat$Year <= 2009, ]
strat.post <- strat[strat$Year >= 2010, ]
fall.pre <- fall[fall$Year <= 2009, ]
fall.post <- fall[fall$Year >= 2010, ]

av.bio.interp <- list(spring.pre$Ave.Daily.mg.L, spring.post$Ave.Daily.mg.L, 
               strat.pre$Ave.Daily.mg.L, strat.post$Ave.Daily.mg.L, 
               fall.pre$Ave.Daily.mg.L, fall.post$Ave.Daily.mg.L)
names(av.bio.interp) <- c("spring.pre","spring.post","strat.pre","strat.post","fall.pre","fall.post")


# ---- prep data - plot YEAR AVERAGES (of only actual measured values) ----

m <- "mean"

ice <- biomass[[m]]$ice
spring <- biomass[[m]]$spring
strat <- biomass[[m]]$stratified
fall <- biomass[[m]]$fall

ice.cutoff <- 2
spring.cutoff <- 2
strat.cutoff <- 2
fall.cutoff <- 2
ice <- ice[!ice$Total.Days < ice.cutoff, ]
spring <- spring[!spring$Total.Days < spring.cutoff, ]
strat <- strat[!strat$Total.Days < strat.cutoff, ]
fall <- fall[!fall$Total.Days < fall.cutoff, ]

spring.pre <- spring[spring$Year <= 2009, ]
spring.post <- spring[spring$Year >= 2010, ]
strat.pre <- strat[strat$Year <= 2009, ]
strat.post <- strat[strat$Year >= 2010, ]
fall.pre <- fall[fall$Year <= 2009, ]
fall.post <- fall[fall$Year >= 2010, ]

av.bio.measured <- list(spring.pre$Ave.Daily.mg.L, spring.post$Ave.Daily.mg.L, 
                           strat.pre$Ave.Daily.mg.L, strat.post$Ave.Daily.mg.L, 
                           fall.pre$Ave.Daily.mg.L, fall.post$Ave.Daily.mg.L)
names(av.bio.measured) <- c("spring.pre","spring.post","strat.pre","strat.post","fall.pre","fall.post")


# ---- prep data - plot ALL MEASURED VALUES ----

m <- "mean"

ice <- measured[[m]]$ice
spring <- measured[[m]]$spring
strat <- measured[[m]]$stratified
fall <- measured[[m]]$fall

spring.pre <- spring[spring$year <= 2009, ]
spring.post <- spring[spring$year >= 2010, ]
strat.pre <- strat[strat$year <= 2009, ]
strat.post <- strat[strat$year >= 2010, ]
fall.pre <- fall[fall$year <= 2009, ]
fall.post <- fall[fall$year >= 2010, ]

av.bio.all.samples <- list(spring.pre$biomass, spring.post$biomass, 
               strat.pre$biomass, strat.post$biomass, 
               fall.pre$biomass, fall.post$biomass)
names(av.bio.all.samples) <- c("spring.pre","spring.post","strat.pre","strat.post","fall.pre","fall.post")


col.pre <- "steelblue"
col.post <- "orange3"

# ---- make plot ----

pdf(file = plot.1, width = 7, height = 3)

par(mar = c(2.25,1.5,3.25,1), oma = c(.1,2.5,.1,.1), mfrow = c(1,3))

boxplot(x = av.bio.interp, notch = F, range = 0, lty = 1, axes = F, at = c(1,2,4,5,7,8), col = rep(c(col.pre,col.post), 3))
axis(side = 2, las = 2)
axis(side = 1, at = c(1.5, 4.5, 7.5), labels = F)
box()
mtext(text = c("Spring","Stratified","Fall"), side = 1, line = 1, at = c(1.2, 4.5, 7.5), xpd = T)
mtext(text = "Average Biomass (mg/L)", side = 2, outer = F, line = 2.5)
mtext(text = "Interpolated\nYearly Averages", line = .25)

boxplot(x = av.bio.measured, notch = F, range = 0, lty = 1, axes = F, at = c(1,2,4,5,7,8), col = rep(c(col.pre,col.post), 3))
axis(side = 2, las = 2)
axis(side = 1, at = c(1.5, 4.5, 7.5), labels = F)
box()
mtext(text = c("Spring","Stratified","Fall"), side = 1, line = 1, at = c(1.2, 4.5, 7.5), xpd = T)
mtext(text = "Measurement\nYearly Averages", line = .25)

boxplot(x = av.bio.all.samples, notch = F, range = 0, lty = 1, axes = F, at = c(1,2,4,5,7,8), col = rep(c(col.pre,col.post), 3))
axis(side = 2, las = 2)
axis(side = 1, at = c(1.5, 4.5, 7.5), labels = F)
box()
mtext(text = c("Spring","Stratified","Fall"), side = 1, line = 1, at = c(1.2, 4.5, 7.5), xpd = T)
mtext(text = "All Measurements\nFrom All Years", line = .25)

mtext(text = "Pre-SWF", side = 3, line = -5, outer = T, col = col.pre, at = .035, adj = 0)
mtext(text = "Post-SWF", side = 3, line = -6.5, outer = T, col = col.post, at = .035, adj = 0)

dev.off()
# ----

# ---- make plot with jitter ----

pdf(file = plot.1.jitter, width = 7, height = 3)

par(mar = c(2.25,1.5,3.25,1), oma = c(.1,2.5,.1,.1), mfrow = c(1,3))

boxplot(x = av.bio.interp, notch = F, range = 0, lty = 1, axes = F, at = c(1,2,4,5,7,8), col = rep(c(col.pre,col.post), 3))
points(x = jitter(rep(1, length(av.bio.interp$spring.pre)), factor = 15), y = av.bio.interp$spring.pre, pch = 21, bg = adjustcolor("black",.3), cex = .7)
points(x = 1 + jitter(rep(1, length(av.bio.interp$spring.post)), factor = 15), y = av.bio.interp$spring.post, pch = 21, bg = adjustcolor("black",.3), cex = .7)
points(x = 3 + jitter(rep(1, length(av.bio.interp$strat.pre)), factor = 15), y = av.bio.interp$strat.pre, pch = 21, bg = adjustcolor("black",.3), cex = .7)
points(x = 4 + jitter(rep(1, length(av.bio.interp$strat.post)), factor = 15), y = av.bio.interp$strat.post, pch = 21, bg = adjustcolor("black",.3), cex = .7)
points(x = 6 + jitter(rep(1, length(av.bio.interp$fall.pre)), factor = 15), y = av.bio.interp$fall.pre, pch = 21, bg = adjustcolor("black",.3), cex = .7)
points(x = 7 + jitter(rep(1, length(av.bio.interp$fall.post)), factor = 15), y = av.bio.interp$fall.post, pch = 21, bg = adjustcolor("black",.3), cex = .7)
axis(side = 2, las = 2)
axis(side = 1, at = c(1.5, 4.5, 7.5), labels = F)
box()
mtext(text = c("Spring","Stratified","Fall"), side = 1, line = 1, at = c(1.2, 4.5, 7.5), xpd = T)
mtext(text = "Average Biomass (mg/L)", side = 2, outer = F, line = 2.5)
mtext(text = "Interpolated\nYearly Averages", line = .25)

boxplot(x = av.bio.measured, notch = F, range = 0, lty = 1, axes = F, at = c(1,2,4,5,7,8), col = rep(c(col.pre,col.post), 3))
points(x = jitter(rep(1, length(av.bio.measured$spring.pre)), factor = 15), y = av.bio.measured$spring.pre, pch = 21, bg = adjustcolor("black",.3), cex = .7)
points(x = 1 + jitter(rep(1, length(av.bio.measured$spring.post)), factor = 15), y = av.bio.measured$spring.post, pch = 21, bg = adjustcolor("black",.3), cex = .7)
points(x = 3 + jitter(rep(1, length(av.bio.measured$strat.pre)), factor = 15), y = av.bio.measured$strat.pre, pch = 21, bg = adjustcolor("black",.3), cex = .7)
points(x = 4 + jitter(rep(1, length(av.bio.measured$strat.post)), factor = 15), y = av.bio.measured$strat.post, pch = 21, bg = adjustcolor("black",.3), cex = .7)
points(x = 6 + jitter(rep(1, length(av.bio.measured$fall.pre)), factor = 15), y = av.bio.measured$fall.pre, pch = 21, bg = adjustcolor("black",.3), cex = .7)
points(x = 7 + jitter(rep(1, length(av.bio.measured$fall.post)), factor = 15), y = av.bio.measured$fall.post, pch = 21, bg = adjustcolor("black",.3), cex = .7)
axis(side = 2, las = 2)
axis(side = 1, at = c(1.5, 4.5, 7.5), labels = F)
box()
mtext(text = c("Spring","Stratified","Fall"), side = 1, line = 1, at = c(1.2, 4.5, 7.5), xpd = T)
mtext(text = "Measurement\nYearly Averages", line = .25)

boxplot(x = av.bio.all.samples, notch = F, range = 0, lty = 1, axes = F, at = c(1,2,4,5,7,8), col = rep(c(col.pre,col.post), 3))
points(x = jitter(rep(1, length(av.bio.all.samples$spring.pre)), factor = 15), y = av.bio.all.samples$spring.pre, pch = 21, bg = adjustcolor("black",.3), cex = .7)
points(x = 1 + jitter(rep(1, length(av.bio.all.samples$spring.post)), factor = 15), y = av.bio.all.samples$spring.post, pch = 21, bg = adjustcolor("black",.3), cex = .7)
points(x = 3 + jitter(rep(1, length(av.bio.all.samples$strat.pre)), factor = 15), y = av.bio.all.samples$strat.pre, pch = 21, bg = adjustcolor("black",.3), cex = .7)
points(x = 4 + jitter(rep(1, length(av.bio.all.samples$strat.post)), factor = 15), y = av.bio.all.samples$strat.post, pch = 21, bg = adjustcolor("black",.3), cex = .7)
points(x = 6 + jitter(rep(1, length(av.bio.all.samples$fall.pre)), factor = 15), y = av.bio.all.samples$fall.pre, pch = 21, bg = adjustcolor("black",.3), cex = .7)
points(x = 7 + jitter(rep(1, length(av.bio.all.samples$fall.post)), factor = 15), y = av.bio.all.samples$fall.post, pch = 21, bg = adjustcolor("black",.3), cex = .7)
axis(side = 2, las = 2)
axis(side = 1, at = c(1.5, 4.5, 7.5), labels = F)
box()
mtext(text = c("Spring","Stratified","Fall"), side = 1, line = 1, at = c(1.2, 4.5, 7.5), xpd = T)
mtext(text = "All Measurements\nFrom All Years", line = .25)

mtext(text = "Pre-SWF", side = 3, line = -5, outer = T, col = col.pre, at = .035, adj = 0)
mtext(text = "Post-SWF", side = 3, line = -6.5, outer = T, col = col.post, at = .035, adj = 0)

dev.off()
# ----

