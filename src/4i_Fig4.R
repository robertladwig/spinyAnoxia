# RRR
# Figure 5

# output.folder <- "plots/2022-08-11_taxa_composition_plot/"
# output.folder <- "plots/2022-10-02_taxa_composition_plot/"
output.folder <- "plots/2022-10-25_taxa_composition_plot"

# phyto.list <- readRDS("robin-data/2022-06-08_phyto_list.rds")
phyto.table <- readRDS("robin-data/2022-10-25_phyto_division_table.rds")
# key <- readRDS("robin-data/2022-07-25_season_dates/seasons_by_sample.rds")
key <- readRDS("robin-data/2022-10-02_season_dates/seasons_by_sample.rds")

phyto <- phyto.table

phyto[1:5,1:5]

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
col.key$color[col.key$taxa == "other"] <- adjustcolor("grey",.8)
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

# ---- save data behind plot ----

write.csv(x = ice.av, file = file.path(output.folder,"ice_yearly_av_taxa.csv"), quote = F, row.names = T)
write.csv(x = spring.av, file = file.path(output.folder,"spring_yearly_av_taxa.csv"), quote = F, row.names = T)
write.csv(x = stratified.av, file = file.path(output.folder,"stratified_yearly_av_taxa.csv"), quote = F, row.names = T)
write.csv(x = fall.av, file = file.path(output.folder,"fall_yearly_av_taxa.csv"), quote = F, row.names = T)

# saveRDS(object = list("ice" = ice.av, "spring" = spring.av, "stratified" = stratified.av, "fall" = fall.av), file = "robin-data/2022-09-28_phyto_stats_by_taxon/taxon_year_averages_mg_L.rds")
# saveRDS(object = list("ice" = ice.av, "spring" = spring.av, "stratified" = stratified.av, "fall" = fall.av), file = "robin-data/2022-10-02_phyto_stats_by_taxon/taxon_year_averages_mg_L.rds")
saveRDS(object = list("ice" = ice.av, "spring" = spring.av, "stratified" = stratified.av, "fall" = fall.av), file = "robin-data/2022-10-02_phyto_stats_by_taxon/taxon_year_averages_mg_L-other_category.rds")


# ---- make a prettier plot ----

pdf(file = file.path(output.folder,"season_barplot.pdf"), width = 6.5, height = 5)

par(mar = c(.75,2,.25,0), oma = c(0,0,0,0))

1-.05

.95 / 4
0.2375 * 1 + .05
0.2375 * 2 + .05
0.2375 * 3 + .05
0.2375 * 4 + .05

par(fig = c(0,.84,0.7625,1))
bar.spots <- barplot(ice.av, col = col.key$color, border = NA, las = 2, names.arg = rep("",ncol(ice.av)), axes = F)
axis(side = 2, line = -.5, las = 2, cex.axis = .7,tck = -.05, labels = F, at = c(0:5))
axis(side = 2, lwd = 0, line = -1, las = 2, cex.axis = .7, at = 0:5)
axis(side = 1, labels = F, at = c(bar.spots[1] - .5 ,bar.spots[length(bar.spots)] + .5), lwd.ticks = 0, line = 0)
axis(side = 1, labels = F, at = bar.spots, tck = -.05, line = 0, lwd = 0, lwd.ticks = 1)
mtext(text = "Ice", side = 3, line = -1, at = 1, adj = 0)
text(x = bar.spots[7], y = max(ice.av / 20, na.rm = T), labels = "X", xpd = NA, cex = .7, col = "grey3")

par(fig = c(0,.84,0.525,0.7625), new = T)
bar.spots <- barplot(spring.av, col = col.key$color, border = NA, las = 2, names.arg = rep("",ncol(spring.av)), axes = F)
axis(side = 2, line = -.5, las = 2, cex.axis = .7,tck = -.05, labels = F, at = 0:6)
axis(side = 2, lwd = 0, line = -1, las = 2, cex.axis = .7, at = 0:6)
axis(side = 1, labels = F, at = c(bar.spots[1] - .5 ,bar.spots[length(bar.spots)] + .5), lwd.ticks = 0, line = 0)
axis(side = 1, labels = F, at = bar.spots, tck = -.05, line = 0, lwd = 0, lwd.ticks = 1)
mtext(text = "Spring mixed", side = 3, line = -1, at = 1, adj = 0)
# text(x = bar.spots[22], y = max(spring.av / 20, na.rm = T), labels = "X", xpd = NA, cex = .7, col = "grey3")

par(fig = c(0,.84,0.2875,0.525), new = T)
bar.spots <- barplot(stratified.av, col = col.key$color, border = NA, las = 2, names.arg = rep("",ncol(stratified.av)), axes = F)
axis(side = 2, line = -.5, las = 2, cex.axis = .7,tck = -.05, labels = F, at = 0:7)
axis(side = 2, lwd = 0, line = -1, las = 2, cex.axis = .7, at = 0:7)
axis(side = 1, labels = F, at = c(bar.spots[1] - .5 ,bar.spots[length(bar.spots)] + .5), lwd.ticks = 0, line = 0)
axis(side = 1, labels = F, at = bar.spots, tck = -.05, line = 0, lwd = 0, lwd.ticks = 1)
# mtext(text = "Biomass (mg/L)", side = 2, line = 1)
mtext(text = "Stratified", side = 3, line = -1, at = 1, adj = 0)

par(fig = c(0,.84,.05,0.2875), new = T)
bar.spots <- barplot(fall.av, col = col.key$color, border = NA, las = 2, names.arg = rep("",ncol(fall.av)), axes = F)
axis(side = 2, line = -.5, las = 2, cex.axis = .7,tck = -.05, labels = F, at = seq.int(0,14,2))
axis(side = 2, lwd = 0, line = -1, las = 2, cex.axis = .7)
axis(side = 1, labels = F, at = c(bar.spots[1] - .5 ,bar.spots[length(bar.spots)] + .5), lwd.ticks = 0, line = 0)
axis(side = 1, labels = F, at = bar.spots, tck = -.05, line = 0, lwd = 0, lwd.ticks = 1)
mtext(text = "Fall mixed", side = 3, line = -1, at = 1, adj = 0)
# text(x = bar.spots[c(5,6,19)], y = max(fall.av / 20, na.rm = T), labels = "X", xpd = NA, cex = .7, col = "grey3")
text(x = bar.spots[5], y = max(fall.av / 20, na.rm = T), labels = "X", xpd = NA, cex = .7, col = "grey3")

text(x = bar.spots, y = -1.25, labels = colnames(fall.av), xpd = NA, srt = 90, adj = 1, cex = .7)

# legend
# par(fig = c(.84,1,.3,.7), new = T, mar = c(0,0,0,0))
par(fig = c(.84,1,.4,.72), new = T, mar = c(0,0,0,0)) # make tighter with fewer taxa listed
plot(1:10,1:10, type = "n", ann = F, axes = F)
lab.locs <- seq(from = 10, to = 0, along.with = rownames(spring.av))
text(x = 3, y = lab.locs, labels = row.names(spring.av), cex = .7, xpd = NA, adj = 0)
# rect(xleft = 1.25, xright = 2.5, ybottom = lab.locs - .3, ytop = lab.locs +.3, 
#      col = col.key$color, xpd = NA, border = NA)
rect(xleft = 1.25, xright = 2.5, ybottom = lab.locs - .4, ytop = lab.locs +.4, 
     col = col.key$color, xpd = NA, border = NA)

# label
par(fig = c(0,1,0,1), new = T, mar = c(0,0,0,0))
plot(1:10,1:10, type = "n", ann = F, axes = F)
mtext(text = "Biomass (mg/L)", side = 2, line = -1)

dev.off()
