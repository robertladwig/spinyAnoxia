# RRR
# look at community composition via barplots
# meh, first look at least just do a barplot for every day

phyto.list <- readRDS("robin-data/2022-06-08_phyto_list.rds")
key <- readRDS("robin-data/2022-07-25_season_dates/seasons_by_sample.rds")

phyto <- phyto.list$div

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
col.key$color[col.key$taxa == "Miscellaneous"] <- adjustcolor("grey",.8)
col.key$color[col.key$taxa == "Xanthophyta"] <- adjustcolor("#718f20",.8)
col.key$color[col.key$taxa == "Euglenophyta"] <- adjustcolor("#3a4d1d",.8)
col.key$color[col.key$taxa == "Haptophyta"] <- adjustcolor("#b67024",.8)

# more different from each other
col.key$color[col.key$taxa == "Bacillariophyta"] <- adjustcolor(rainbow(n = 20, v = .5)[3], .8) # diatoms
col.key$color[col.key$taxa == "Chrysophyta"] <- adjustcolor(rainbow(n = 20, v = 1)[2],.5)
col.key$color[col.key$taxa == "Chlorophyta"] <- adjustcolor(rainbow(n = 20, v = .9)[8],.8)
col.key$color[col.key$taxa == "Cryptophyta"] <- adjustcolor(rainbow(n = 20, v = .9)[1],.7)
col.key$color[col.key$taxa == "Cyanophyta"] <- adjustcolor(rainbow(n = 20, v = .8)[12],.8)
col.key$color[col.key$taxa == "Pyrrhophyta"] <- adjustcolor(rainbow(n = 20, v = .9)[5],.8)
col.key$color[col.key$taxa == "Miscellaneous"] <- adjustcolor("grey",.8)
col.key$color[col.key$taxa == "Xanthophyta"] <- adjustcolor(rainbow(n = 20, v = .5)[9],.8)
col.key$color[col.key$taxa == "Euglenophyta"] <- adjustcolor(rainbow(n = 20, v = .2)[6],.5)
col.key$color[col.key$taxa == "Haptophyta"] <- adjustcolor(rainbow(n = 20, v = .9)[20],.4)


# ----

barplot(phyto, border = NA, col = col.key$color, las = 2, cex.names = .5)
all.equal(row.names(phyto), col.key$taxa)

spring <- phyto[ ,key$Season == "spring"]
spring[is.na(spring)] <- 0

index <- order(rowSums(spring), decreasing = T)
spring <- spring[index, ]
phyto <- phyto[index, ]
col.key <- col.key[index, ]
all.equal(row.names(spring), col.key$taxa)

barplot(spring, las  = 2, cex.names = .5, legend = F, col = col.key$color, border = NA)

fall <- phyto[ ,key$Season == "fall"]
fall[is.na(fall)] <- 0

all.equal(row.names(fall), col.key$taxa)

barplot(fall, las  = 2, cex.names = .5, legend = F, col = col.key$color, border = NA)


stratified <- phyto[ ,key$Season == "stratified"]
stratified[is.na(stratified)] <- 0

all.equal(row.names(stratified), col.key$taxa)

barplot(stratified, las  = 2, cex.names = .5, legend = F, col = col.key$color, border = NA)

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
setdiff(x = 1996:2020, y = colnames(spring.av)) # missing 2017
barplot(spring.av, col = col.key$color, border = NA, las = 2)

stratified.av <- get.yearly.av(my.phy = stratified)
all.equal(row.names(stratified.av), col.key$taxa)
barplot(stratified.av, col = col.key$color, border = NA, las = 2)
setdiff(x = 1996:2020, y = colnames(stratified.av))

fall.av <- get.yearly.av(my.phy = fall)
all.equal(row.names(fall.av), col.key$taxa)
barplot(fall.av, col = col.key$color, border = NA, las = 2)
setdiff(x = 1996:2020, y = colnames(fall.av)) # missing 2000, 2001, 2014

# ---- add missing year placeholders ----

spring.av <- cbind(spring.av[ ,1:21], 
                   "2017" = NA, 
                   spring.av[ ,22:ncol(spring.av)])

fall.av <- cbind(fall.av[ ,1:4], 
                 "2000" = NA, "2001" = NA, 
                 fall.av[ ,5:16],
                 "2014" = NA,
                 fall.av[ ,17:ncol(fall.av)])

# ---- make a prettier plot ----

# pdf(file = "plots/2022-07-28_taxa_composition_plots/season_barplots.pdf", width = 6.5, height = 5)
png(filename = "figs/season_barplots.png", width = 6.5, height = 5, units = "in", res = 300)

par(mar = c(1,2,.1,0), oma = c(0,0,0,0))

par(fig = c(0,.8,.65,.95))
barplot(spring.av, col = col.key$color, border = NA, las = 2, names.arg = rep("",ncol(spring.av)), axes = F)
axis(side = 2, line = -.5, las = 2, cex.axis = .7,tck = -.05, labels = F)
axis(side = 2, lwd = 0, line = -1, las = 2, cex.axis = .7)
mtext(text = "Spring", side = 3, line = -1, at = 3)

par(fig = c(0,.8,.35,.65), new = T)
barplot(stratified.av, col = col.key$color, border = NA, las = 2, names.arg = rep("",ncol(stratified.av)), axes = F)
axis(side = 2, line = -.5, las = 2, cex.axis = .7,tck = -.05, labels = F)
axis(side = 2, lwd = 0, line = -1, las = 2, cex.axis = .7)
mtext(text = "Biomass (mg/L)", side = 2, line = 1)
mtext(text = "Stratified", side = 3, line = -1, at = 3)

par(fig = c(0,.8,.05,.35), new = T)
bar.spots <- barplot(fall.av, col = col.key$color, border = NA, las = 2, names.arg = rep("",ncol(fall.av)), axes = F)
axis(side = 2, line = -.5, las = 2, cex.axis = .7,tck = -.05, labels = F)
axis(side = 2, lwd = 0, line = -1, las = 2, cex.axis = .7)
mtext(text = "Fall", side = 3, line = -1, at = 3)
text(x = bar.spots, y = -1.25, labels = colnames(fall.av), xpd = NA, srt = 90, adj = 1, cex = .7)

par(fig = c(.8,1,.3,.7), new = T, mar = c(0,0,0,0))
plot(1:10,1:10, type = "n", ann = F, axes = F)
lab.locs <- seq(from = 0, to = 10, along.with = rownames(spring.av))
text(x = 3, y = lab.locs, labels = row.names(spring.av), cex = .7, xpd = NA, adj = 0)
rect(xleft = 1.25, xright = 2.5, ybottom = lab.locs - .3, ytop = lab.locs +.3, 
     col = col.key$color, xpd = NA, border = NA)

dev.off()