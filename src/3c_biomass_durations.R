# RRR

phyto <- readRDS(file = "data_processed/3b_interp_table.rds")

output.duration.file <- "data_processed/3c_biomass_duration.csv"

plot.file <- "figs/3c_biomass_duration_1mgL.pdf"

colnames(phyto)
rownames(phyto) <- phyto$day
phyto <- phyto[ ,-1]
phyto <- as.matrix(phyto)
phyto[1:3,1:3]

mean.biomass <- apply(X = phyto, MARGIN = 2, FUN = mean, na.rm = T)
boxplot(mean.biomass)
boxplot(phyto)
boxplot(as.vector(phyto))

high.biomass <- 1

phyto.high <- phyto > high.biomass
total.high.days <- colSums(phyto.high, na.rm = T)

par(mar = c(4,4,1,.5))
barplot(total.high.days, names.arg = sub(pattern = "\\.bio",replacement = "",x = names(total.high.days)), las = 2, ylab = "Total Days with Biomass > 1 mg/L")

boxplot(list("Before\n(1995-2009)" = total.high.days[1:15], "After\n(2010-2020)" = total.high.days[16:26]), ylab = "Total Days with Biomass > 1 mg/L")

boxplot(list("1995-2001" = total.high.days[1:7],"2002-2009" = total.high.days[8:15], "2010-2020" = total.high.days[16:26]), ylab = "Total Days with Biomass > 1 mg/L")
# DO we know why Mendota was clearer for 2002-2009?


boxes <- list("Before\n(1995-2009)" = total.high.days[1:15], "After\n(2010-2020)" = total.high.days[16:26])
p.val <- t.test(x = boxes[[1]], y = boxes[[2]])
p.val$p.value

# ----
pdf(file = plot.file, width = 3, height = 3)
par(mar = c(3,3.5,1,.5))
x.locs <- boxplot(x = boxes, axes = F, col = c("steelblue","orange2"))
# points(x = jitter(x = rep(1,length(boxes[[1]])), factor = 4), y = boxes[[1]])
# points(x = jitter(x = rep(2,length(boxes[[2]])), factor = 4), y = boxes[[2]])
box()
mtext(text = names(boxes), side = 1, line = 1.75, at = c(1,2))
mtext(text = "Total Days with Biomass > 1 mg/L", side = 2, line = 2.25)
axis(side = 2, las = 2, labels = F, lwd = 0, lwd.ticks = 1, tck = -.025)
axis(side = 2, las = 2, labels = T, line = -.5, lwd = 0)
axis(side = 1, lwd = 0, lwd.ticks = 1, labels = F, at = c(1,2))
mtext(text = "p < 0.005", side = 1, line = -1.25, at = 2.3, cex = .7)
dev.off()

# ---- save total high days for Robert ----

x <- data.frame("Year" = as.numeric(sub(pattern = "\\.bio", replacement = "", x = colnames(phyto))),
                "Days.0.5.mg.L" = NA, "Days.1.mg.L" = NA, "Days.1.5.mg.L" = NA, "Days.2.mg.L" = NA, "Days.3.mg.L" = NA)

high.biomass <- .5
phyto.high <- phyto > high.biomass
total.high.days <- colSums(phyto.high, na.rm = T)
x$Days.0.5.mg.L <- total.high.days

high.biomass <- 1
phyto.high <- phyto > high.biomass
total.high.days <- colSums(phyto.high, na.rm = T)
x$Days.1.mg.L <- total.high.days

high.biomass <- 1.5
phyto.high <- phyto > high.biomass
total.high.days <- colSums(phyto.high, na.rm = T)
x$Days.1.5.mg.L <- total.high.days

high.biomass <- 2
phyto.high <- phyto > high.biomass
total.high.days <- colSums(phyto.high, na.rm = T)
x$Days.2.mg.L <- total.high.days

high.biomass <- 3
phyto.high <- phyto > high.biomass
total.high.days <- colSums(phyto.high, na.rm = T)
x$Days.3.mg.L <- total.high.days

barplot(x$Days.0.5.mg.L, names.arg = x$Year, las = 2, main = .5)
barplot(x$Days.1.mg.L, names.arg = x$Year, las = 2, main = 1)
barplot(x$Days.1.5.mg.L, names.arg = x$Year, las = 2, main = 1.5)
barplot(x$Days.2.mg.L, names.arg = x$Year, las = 2, main = 2)
barplot(x$Days.3.mg.L, names.arg = x$Year, las = 2, main = 3)
boxplot(list("Before\n(1995-2009)" = x$Days.0.5.mg.L[1:15], "After\n(2010-2020)" = x$Days.0.5.mg.L[16:26]), main = ".5")
boxplot(list("Before\n(1995-2009)" = x$Days.1.mg.L[1:15], "After\n(2010-2020)" = x$Days.1.mg.L[16:26]), main = "1")
boxplot(list("Before\n(1995-2009)" = x$Days.1.5.mg.L[1:15], "After\n(2010-2020)" = x$Days.1.5.mg.L[16:26]), main = "1.5")
boxplot(list("Before\n(1995-2009)" = x$Days.2.mg.L[1:15], "After\n(2010-2020)" = x$Days.2.mg.L[16:26]), main = "2")
boxplot(list("Before\n(1995-2009)" = x$Days.3.mg.L[1:15], "After\n(2010-2020)" = x$Days.3.mg.L[16:26]), main = "3")

write.csv(x = x, file = output.duration.file, row.names = F)
