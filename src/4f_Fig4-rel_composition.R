# RRR
# Figure 4
# **** NO LONGER RUN THIS VERSION ****

# ---- set-up ----

phyto <- readRDS("data_processed/4e_phyto_division_table.rds")
key <- readRDS("data_processed/4a_seasons_by_sample.rds")

output.fig <- "figs_publication/Fig4.pdf"

output.ice.stats <- "figs_publication/Fig4_ice.csv"
output.spring.stats <- "figs_publication/Fig4_spring.csv"
output.strat.stats <- "figs_publication/Fig4_stratified.csv"
output.fall.stats <- "figs_publication/Fig4_fall.csv"

output.taxon.stats <- "data_processed/4f_taxon_year_averages_mg_L-other_category.rds"

# ----

# Make division color key ----
col.key <- data.frame("taxa" = row.names(phyto), "color" = rainbow(n = nrow(phyto)))

# colors from photos... too similar I think
col.key$color[col.key$taxa == "Bacillariophyta"] <- adjustcolor("#544614", .8) # diatoms
col.key$color[col.key$taxa == "Chrysophyta"] <- adjustcolor("#e3cd5b",.8)
col.key$color[col.key$taxa == "Chlorophyta"] <- adjustcolor("#275510",.8)
col.key$color[col.key$taxa == "Cryptophyta"] <- adjustcolor("#842b2c",.8)
col.key$color[col.key$taxa == "Cyanophyta"] <- adjustcolor("#9fb9d3",.8)
col.key$color[col.key$taxa == "Pyrrhophyta"] <- adjustcolor("#a3ba2e",.8)
col.key$color[col.key$taxa == "other"] <- adjustcolor("grey",.8)
# col.key$color[col.key$taxa == "Xanthophyta"] <- adjustcolor("#718f20",.8)
# col.key$color[col.key$taxa == "Euglenophyta"] <- adjustcolor("#3a4d1d",.8)
# col.key$color[col.key$taxa == "Haptophyta"] <- adjustcolor("#b67024",.8)

# more different from each other
col.key$color[col.key$taxa == "Bacillariophyta"] <- adjustcolor(rainbow(n = 20, v = .5)[3], .8) # diatoms
col.key$color[col.key$taxa == "Chrysophyta"] <- adjustcolor(rainbow(n = 20, v = 1)[2],.5)
col.key$color[col.key$taxa == "Chlorophyta"] <- adjustcolor(rainbow(n = 20, v = .9)[8],.8)
col.key$color[col.key$taxa == "Cryptophyta"] <- adjustcolor(rainbow(n = 20, v = .9)[1],.7)
col.key$color[col.key$taxa == "Cyanophyta"] <- adjustcolor(rainbow(n = 20, v = .8)[12],.8)
col.key$color[col.key$taxa == "Pyrrhophyta"] <- adjustcolor(rainbow(n = 20, v = .9)[5],.8)
col.key$color[col.key$taxa == "other"] <- adjustcolor("grey40",.8)
# col.key$color[col.key$taxa == "Xanthophyta"] <- adjustcolor(rainbow(n = 20, v = .5)[9],.8)
# col.key$color[col.key$taxa == "Euglenophyta"] <- adjustcolor(rainbow(n = 20, v = .2)[6],.5)
# col.key$color[col.key$taxa == "Haptophyta"] <- adjustcolor(rainbow(n = 20, v = .9)[20],.4)


# ----

spring <- phyto[ ,key$Season == "spring"]
spring[is.na(spring)] <- 0

index <- order(rowSums(spring), decreasing = T)
spring <- spring[index, ]
phyto <- phyto[index, ]
col.key <- col.key[index, ]
index <- c(1:5,7,6) # put other last
spring <- spring[index, ]
phyto <- phyto[index, ]
col.key <- col.key[index, ]
all.equal(row.names(spring), col.key$taxa)

fall <- phyto[ ,key$Season == "fall"]
fall[is.na(fall)] <- 0
all.equal(row.names(fall), col.key$taxa)

stratified <- phyto[ ,key$Season == "stratified"]
stratified[is.na(stratified)] <- 0
all.equal(row.names(stratified), col.key$taxa)

ice <- phyto[ ,key$Season == "ice-on"]
ice[is.na(ice)] <- 0
all.equal(row.names(ice), col.key$taxa)

# ---- average by year ----

spring[1:5,1:5]

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


spring.av <- get.yearly.av(my.phy = spring)
all.equal(row.names(spring.av), col.key$taxa)
setdiff(x = 1996:2020, y = colnames(spring.av)) # missing 2017 before, no years now

stratified.av <- get.yearly.av(my.phy = stratified)
all.equal(row.names(stratified.av), col.key$taxa)
setdiff(x = 1996:2020, y = colnames(stratified.av)) # missing no years

fall.av <- get.yearly.av(my.phy = fall)
all.equal(row.names(fall.av), col.key$taxa)
setdiff(x = 1996:2020, y = colnames(fall.av)) # before, missing 2000, 2001, 2014; now missing 2000 only

ice.av <- get.yearly.av(my.phy = ice)
all.equal(row.names(ice.av), col.key$taxa)
setdiff(x = 1996:2020, y = colnames(ice.av)) # missing 2002

# ---- add missing year placeholders ----

# strat start is 0.05
# spring.av <- cbind(spring.av[ ,1:21], 
#                    "2017" = NA, 
#                    spring.av[ ,22:ncol(spring.av)])
# 
# fall.av <- cbind(fall.av[ ,1:4], 
#                  "2000" = NA, "2001" = NA, 
#                  fall.av[ ,5:16],
#                  "2014" = NA,
#                  fall.av[ ,17:ncol(fall.av)])
# 
# ice.av <- cbind(ice.av[ ,1:6],
#                 "2002" = NA,
#                 ice.av[ ,7:ncol(ice.av)])

# strat start is 0.1

fall.av <- cbind(fall.av[ ,1:4],
                 "2000" = NA, 
                 fall.av[ ,5:ncol(fall.av)])

ice.av <- cbind(ice.av[ ,1:6],
                "2002" = NA,
                ice.av[ ,7:ncol(ice.av)])

# ---- make relative abundance to focus on composition ----

get.rel.abund <- function(my.av){
  my.av <- t(my.av)
  my.av <- my.av / rowSums(my.av, na.rm = T) * 100
  my.av <- t(my.av)
  colSums(my.av, na.rm = T)
  return(my.av)
}

ice.av <- get.rel.abund(my.av = ice.av)
spring.av <- get.rel.abund(my.av = spring.av)
stratified.av <- get.rel.abund(my.av = stratified.av)
fall.av <- get.rel.abund(my.av = fall.av)

# ---- save data behind plot ----

write.csv(x = ice.av, file = output.ice.stats, quote = F, row.names = T)
write.csv(x = spring.av, file = output.spring.stats, quote = F, row.names = T)
write.csv(x = stratified.av, file = output.strat.stats, quote = F, row.names = T)
write.csv(x = fall.av, file = output.fall.stats, quote = F, row.names = T)

saveRDS(object = list("ice" = ice.av, "spring" = spring.av, "stratified" = stratified.av, "fall" = fall.av), file = output.taxon.stats)

# ---- add a gap to draw the 2010 demarcation line ----
ice.av <- cbind(ice.av[ ,1:14],"gap" = 0, ice.av[ ,15:ncol(ice.av)])
spring.av <- cbind(spring.av[ ,1:14],"gap" = 0, spring.av[ ,15:ncol(spring.av)])
stratified.av <- cbind(stratified.av[ ,1:14],"gap" = 0, stratified.av[ ,15:ncol(stratified.av)])
fall.av <- cbind(fall.av[ ,1:14],"gap" = 0, fall.av[ ,15:ncol(fall.av)])

# ---- make a prettier plot ----

pdf(file = output.fig, width = 6.5, height = 3)

par(mar = c(.5,1.75,1,0), oma = c(1.15,.75,0,0))

1-.05

.95 / 4
0.2375 * 1 + .05
0.2375 * 2 + .05
0.2375 * 3 + .05
0.2375 * 4 + .05

par(fig = c(0,.42,0.5,1), xaxs = "i")
bar.spots <- barplot(ice.av, col = col.key$color, border = NA, las = 2, names.arg = rep("",ncol(ice.av)), axes = F, xlim = c(-.3,31.7), ylim = c(0,100))
box()
axis(side = 2, line = 0, las = 2, cex.axis = .7,tck = -.05, labels = F, at = seq.int(from = 0, to = 100, by = 25), lwd = 0, lwd.ticks = 1)
axis(side = 2, lwd = 0, line = -.6, las = 2, cex.axis = .7, at = seq.int(from = 0, to = 100, by = 25))
mtext(text = "Ice", side = 3, line = .05, at = 1, adj = 0)
# text(x = bar.spots[7], y = max(ice.av / 20, na.rm = T), labels = "X", xpd = NA, cex = .7, col = "grey3")
segments(x0 = bar.spots[15], x1 = bar.spots[15], y0 = -5, y1 = 105, xpd = NA, lwd = 3, col = "black", lty = c("41"))

par(fig = c(0,.42,0,0.5), new = T)

bar.spots <- barplot(stratified.av, col = col.key$color, border = NA, las = 2, names.arg = rep("",ncol(stratified.av)), axes = F, xlim = c(-.3,31.7), ylim = c(0,100))
box()
axis(side = 2, line = 0, las = 2, cex.axis = .7,tck = -.05, labels = F, at = seq.int(from = 0, to = 100, by = 25), lwd = 0, lwd.ticks = 1)
axis(side = 2, lwd = 0, line = -.6, las = 2, cex.axis = .7, at = seq.int(from = 0, to = 100, by = 25))
# mtext(text = "Biomass (mg/L)", side = 2, line = 1)
mtext(text = "Stratified", side = 3, line = .05, at = 1, adj = 0)
index.labels <- c(seq.int(from = 2, to = 14, by = 2), seq.int(from = 16, to = ncol(fall.av), by = 2))
text(x = bar.spots[index.labels], y = -4, labels = colnames(fall.av)[index.labels], xpd = NA, srt = 90, adj = 1, cex = .7)
segments(x0 = bar.spots[15], x1 = bar.spots[15], y0 = -5, y1 = 105, xpd = NA, lwd = 3, col = "black", lty = c("41"))

par(fig = c(.42,.84,0.5,1), new = T)
bar.spots <- barplot(spring.av, col = col.key$color, border = NA, las = 2, names.arg = rep("",ncol(spring.av)), axes = F, xlim = c(-.3,31.7), ylim = c(0,100))
box()
axis(side = 2, line = 0, las = 2, cex.axis = .7,tck = -.05, labels = F, at = seq.int(from = 0, to = 100, by = 25), lwd = 0, lwd.ticks = 1)
axis(side = 2, lwd = 0, line = -.6, las = 2, cex.axis = .7, at = seq.int(from = 0, to = 100, by = 25))
mtext(text = "Spring mixed", side = 3, line = .05, at = 1, adj = 0)
# text(x = bar.spots[22], y = max(spring.av / 20, na.rm = T), labels = "X", xpd = NA, cex = .7, col = "grey3")
segments(x0 = bar.spots[15], x1 = bar.spots[15], y0 = -5, y1 = 105, xpd = NA, lwd = 3, col = "black", lty = c("41"))

par(fig = c(.42,.84,0,0.5), new = T)
bar.spots <- barplot(fall.av, col = col.key$color, border = NA, las = 2, names.arg = rep("",ncol(fall.av)), axes = F, xlim = c(-.3,31.7), ylim = c(0,100))
box()
axis(side = 2, line = 0, las = 2, cex.axis = .7,tck = -.05, labels = F, at = seq.int(from = 0, to = 100, by = 25), lwd = 0, lwd.ticks = 1)
axis(side = 2, lwd = 0, line = -.6, las = 2, cex.axis = .7, at = seq.int(from = 0, to = 100, by = 25))
mtext(text = "Fall mixed", side = 3, line = .05, at = 1, adj = 0)
# text(x = bar.spots[c(5,6,19)], y = max(fall.av / 20, na.rm = T), labels = "X", xpd = NA, cex = .7, col = "grey3")
# text(x = bar.spots[5], y = max(fall.av / 20, na.rm = T), labels = "X", xpd = NA, cex = .7, col = "grey3")
text(x = bar.spots[index.labels], y = -4, labels = colnames(fall.av)[index.labels], xpd = NA, srt = 90, adj = 1, cex = .7)
segments(x0 = bar.spots[15], x1 = bar.spots[15], y0 = -5, y1 = 105, xpd = NA, lwd = 3, col = "black", lty = c("41"))

# legend
# par(fig = c(.84,1,.3,.7), new = T, mar = c(0,0,0,0))
par(fig = c(.84,1,.4,.72), new = T, mar = c(0,0,0,0)) # make tighter with fewer taxa listed
plot(1:10,1:10, type = "n", ann = F, axes = F)
lab.locs <- seq(from = 10, to = 0, along.with = rownames(spring.av))
text(x = 3.3, y = lab.locs, labels = row.names(spring.av), cex = .7, xpd = NA, adj = 0)
# rect(xleft = 1.25, xright = 2.5, ybottom = lab.locs - .3, ytop = lab.locs +.3, 
#      col = col.key$color, xpd = NA, border = NA)
rect(xleft = 1.55, xright = 2.8, ybottom = lab.locs - .4, ytop = lab.locs +.4, 
     col = col.key$color, xpd = NA, border = NA)

# label
par(fig = c(0,1,0,1), new = T, mar = c(0,0,0,0))
plot(1:10,1:10, type = "n", ann = F, axes = F)
mtext(text = "Biomass (%)", side = 2, line = -.25, outer = T)

dev.off()
