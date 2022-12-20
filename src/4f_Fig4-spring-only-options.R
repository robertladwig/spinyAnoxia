# RRR
# Figure 4

# ---- set-up ----

phyto <- readRDS("data_processed/4e_phyto_division_table.rds")
key <- readRDS("data_processed/4a_seasons_by_sample.rds")

output.fig <- "figs_publication/Fig4-spring_only.pdf"

output.spring.stats.abs <- "figs_publication/Fig4_spring_abund.csv"
output.spring.stats.perc <- "figs_publication/Fig4_spring_perc.csv"

# output.taxon.stats <- "data_processed/4f_taxon_year_averages_mg_L-other_category.rds"

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


# ---- make relative abundance to focus on composition ----

get.rel.abund <- function(my.av){
  my.av <- t(my.av)
  my.av <- my.av / rowSums(my.av, na.rm = T) * 100
  my.av <- t(my.av)
  colSums(my.av, na.rm = T)
  return(my.av)
}

spring.av.perc <- get.rel.abund(my.av = spring.av)

# ---- save data behind plot ----

write.csv(x = spring.av, file = output.spring.stats.abs, quote = F, row.names = T)
write.csv(x = spring.av.perc, file = output.spring.stats.perc, quote = F, row.names = T)

# saveRDS(object = list("ice" = ice.av, "spring" = spring.av, "stratified" = stratified.av, "fall" = fall.av), file = output.taxon.stats)

# ---- add a gap to draw the 2010 demarcation line ----

spring.av <- cbind(spring.av[ ,1:14],"gap" = 0, spring.av[ ,15:ncol(spring.av)])
spring.av.perc <- cbind(spring.av.perc[ ,1:14],"gap" = 0, spring.av.perc[ ,15:ncol(spring.av.perc)])

# ---- make a prettier plot ----

pdf(file = output.fig, width = 6.5, height = 3)

par(mar = c(.5,2.5,1,.5), oma = c(1.15,0,0,0))

par(fig = c(0,.42,0,1))
y.max <- 5.5
bar.spots <- barplot(spring.av, col = col.key$color, border = NA, las = 2, names.arg = rep("",ncol(spring.av)), axes = F, xlim = c(-.3,31.7), ylim = c(0,y.max))
box()
axis(side = 2, line = 0, las = 2, cex.axis = .7,tck = -.025, labels = F, at = seq.int(from = 0, to = 5, by = 1), lwd = 0, lwd.ticks = 1)
axis(side = 2, lwd = 0, line = -.6, las = 2, cex.axis = .7, at = seq.int(from = 0, to = 5, by = 1))
index.labels <- c(seq.int(from = 2, to = 14, by = 2), seq.int(from = 16, to = ncol(spring.av), by = 2))
text(x = bar.spots[index.labels], y = -1 * y.max / 80, labels = colnames(spring.av)[index.labels], xpd = NA, srt = 90, adj = 1, cex = .7)
segments(x0 = bar.spots[15], x1 = bar.spots[15], y0 = 0, y1 = y.max, xpd = NA, lwd = 3, col = "black", lty = c("41"))
mtext(text = "Biomass (mg/L)", side = 2, line = 1.25, outer = F)

par(fig = c(.42,.84,0,1), new = T)
y.max = 100
bar.spots <- barplot(spring.av.perc, col = col.key$color, border = NA, las = 2, names.arg = rep("",ncol(spring.av.perc)), axes = F, xlim = c(-.3,31.7), ylim = c(0,y.max))
box()
axis(side = 2, line = 0, las = 2, cex.axis = .7,tck = -.025, labels = F, at = seq.int(from = 0, to = 100, by = 25), lwd = 0, lwd.ticks = 1)
axis(side = 2, lwd = 0, line = -.6, las = 2, cex.axis = .7, at = seq.int(from = 0, to = 100, by = 25))
text(x = bar.spots[index.labels], y = -1 * y.max / 80, labels = colnames(spring.av.perc)[index.labels], xpd = NA, srt = 90, adj = 1, cex = .7)
segments(x0 = bar.spots[15], x1 = bar.spots[15], y0 = 0, y1 = y.max, xpd = NA, lwd = 3, col = "black", lty = c("41"))
mtext(text = "Biomass (%)", side = 2, line = 1.25, outer = F)

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

dev.off()
