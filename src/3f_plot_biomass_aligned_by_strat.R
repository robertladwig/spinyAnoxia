# RRR
# make plots of biomass averages aligned by stratification onset on the x-axis

# input 1
phyto.all <- readRDS("data_processed/3d_biomass_aligned_by_strat_onset_interp_tables_all.rds")
phyto.swf <- readRDS("data_processed/3d_biomass_aligned_by_strat_onset_interp_tables_pre-post.rds")
label.x <- "Days since stratification onset"
plot.start <- "figs/3f_biomas_aligned_by_stratification/onset_"

# input 2
phyto.all <- readRDS("data_processed/3e_biomass_aligned_by_fall_mixing_interp_tables_all.rds")
phyto.swf <- readRDS("data_processed/3e_biomass_aligned_by_fall_mixing_interp_tables_pre-post.rds")
label.x <- "Days since fall mixing"
plot.start <- "figs/3f_biomass_aligned_by_stratification/mixing_"

# ---- functions ----

fill.btwn.lines <- function(X, Y1, Y2, Color, xpd = F){
  index <- is.na(X) | is.na(Y1) | is.na(Y2)
  X <- X[!index]
  Y1 <- Y1[!index]
  Y2 <- Y2[!index]
  poly.x <- c(X, X[length(X):1])
  poly.y <- c(Y1, Y2[length(Y2):1])
  polygon(x = poly.x, y = poly.y, col = Color, border = NA, xpd = xpd)
}

# ----
names(phyto.swf$pre)

for (strat.def in names(phyto.swf$pre)){
  plot.name <- paste0(plot.start,"_",strat.def,".pdf")
  pdf(file = plot.name, width = 6.5, height = 3)
  # pdf(file = "figs/biomass_increase_at_edges.pdf", width = 6.5, height = 3)
  par(mar = c(3,2.8,1.5,.5), xaxs = "i")
  x.dims <- c(min(phyto.all[[strat.def]]$day), max(phyto.all[[strat.def]]$day))
  y.dims <- c(0, max(max(phyto.swf$pre[[strat.def]]$ave, na.rm = T), max(phyto.swf$post[[strat.def]]$ave, na.rm = T)))
  plot(x = x.dims, y = c(y.dims[1], y.dims[2] + y.dims[2] / 20), type = "n", ann = F, axes = F)
  index <- is.na(phyto.swf$pval[[strat.def]]$is.less.05)
  fill.btwn.lines(X = phyto.swf$pval[[strat.def]]$day[-index], Y1 = rep(0,length(phyto.swf$pval[[strat.def]]$day[-index])), 
                  Y2 = phyto.swf$pval[[strat.def]]$is.less.05[-index] * 10, Color = adjustcolor("grey",.5), xpd = F)
  lines(x = phyto.swf$pre[[strat.def]]$day, y = phyto.swf$pre[[strat.def]]$ave, col = "steelblue", lwd = 4)
  lines(x = phyto.swf$post[[strat.def]]$day, y = phyto.swf$post[[strat.def]]$ave, col = "orange2", lwd = 4)
  box()
  axis(side = 1, labels = F, lwd = 0, lwd.ticks = 1)
  axis(side = 1, labels = T, lwd = 0, line = -.4)
  axis(side = 2, labels = F, lwd = 0, lwd.ticks = 1)
  axis(side = 2, labels = T, lwd = 0, line = -.2, las = 2)
  mtext(text = "Mean Biomass (mg/L)", side = 2, line = 1.75, outer = F)
  mtext(text = label.x, side = 1, line = 1.75, outer = F)
  mtext(text = "1995 - 2009", side = 3, at = .2, outer = T, line = -3, col = "steelblue")
  mtext(text = "2010 - 2020", side = 3, at = .2, outer = T, line = -4, col = "orange2")
  # mtext(text = c("*","*"), side = 3, line = -1, at = c(51, 293))
  mtext(text = strat.def, side = 3, line = .25, cex = 1.2)
  
  dev.off()
}



