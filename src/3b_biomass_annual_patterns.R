# RRR
# make a plot of average biomass split before/after swf

# ---- set-up ----

library(lubridate)

# in:
phyt <- readRDS(file = "data_processed/3a_phyto_list.rds")

# out:
file.phyt.split <- "data_processed/3b_phyto_split_by_year.rds"

file.phyt.interp <- "data_processed/3b_phyto-interp_split_by_year.rds"

file.phyto.stats <- "data_processed/3b_phyto_stats.rds"

file.phyt.tab.all <- "data_processed/3b_interp_table.rds"

file.plot <- "figs/3b_mean_biomass_before_and_after.pdf"

# ---- functions ----
make.empty.list.structure <- function(ListNames){
  # the ListNames can be something like c("OTU", "kingdom","phylum","class","order","family/lineage","genus/clade","species/tribe")
  empty.list <- list(NULL)
  for (e in 1:length(ListNames)){
    empty.list[[e]] <- 0
    names(empty.list)[e] <- ListNames[e]
  }
  return(empty.list)
}

split.data.by.year <- function(tot){
  my.split <- make.empty.list.structure(ListNames = unique(tot$year))
  for (y in 1:length(my.split)){
    yr <- as.numeric(names(my.split)[y])
    index <- tot$year == yr
    my.split[[y]] <- tot[index, ]
  }
  return(my.split)
}

get.daily.interp <- function(vals){
  my.interp <- make.empty.list.structure(ListNames = names(vals))
  for (y in 1:length(my.interp)){
    yr <- as.numeric(names(my.interp)[y])
    my.x <- vals[[y]]$yday
    my.y <- vals[[y]]$biomass
    if (length(my.x) > 1 & sum(!is.na(my.x)) > 1){
      my.fun <- approxfun(x = my.x, y = my.y, method = "linear")
      new.x <- min(my.x):max(my.x)
      daily.interp <- my.fun(v = new.x)
      daily.interp <- data.frame("day" = new.x, "bio" = daily.interp)
      colnames(daily.interp)[2] <- paste0(yr, ".bio")
      my.interp[[y]] <- daily.interp
    }
  }
  return(my.interp)
}

get.aligned.tables <- function(x.min, x.max, year.range, my.interp){
  my.tab <- data.frame("day" = x.min:x.max)
  for (yr in year.range){
    yr.data <- my.interp[[as.character(yr)]]
    if (is.null(yr.data)){next}
    if (length(yr.data) == 1){next}
    my.tab <- merge(x = my.tab, y = yr.data, by = "day", all.x = TRUE, all.y = FALSE)
  }
  return(my.tab)
}

get.group.aves.etc <- function(my.tab){
  my.tab$ave <- apply(X = my.tab[ ,-1], MARGIN = 1, FUN = mean, na.rm = T)
  my.tab$sd <- apply(X = my.tab[ ,-c(1,ncol(my.tab))], MARGIN = 1, FUN = sd, na.rm = T)
  temp <- !is.na(my.tab)
  my.tab$n <- apply(X = temp[ ,-c(1,ncol(my.tab), ncol(my.tab) - 1)], MARGIN = 1, FUN = sum, na.rm = T)
  return(my.tab)
}

get.significance <- function(my.pre, my.post){
  year.cols.pre <- 1:ncol(my.pre)
  year.cols.pre <- year.cols.pre[-c(1,(ncol(my.pre)-3):ncol(my.pre))]
  year.cols.post <- 1:ncol(my.post)
  year.cols.post <- year.cols.post[-c(1,(ncol(my.post)-3):ncol(my.post))]
  
  my.p.vals <- data.frame("day" = my.pre$day, "pval" = NA)
  
  for (r in 1:nrow(my.pre)){
    if (sum(!is.na(my.pre[r, year.cols.pre])) < 2 | sum(!is.na(my.post[r, year.cols.post])) < 2){
      my.p.vals$pval[r] <- NA
      next
    }
    my.t <- t.test(x = my.pre[r, year.cols.pre], y = my.post[r, year.cols.post], alternative = "two.sided")
    my.p.vals$pval[r] <- my.t$p.value
  }
  
  my.p.vals$is.less.05 <- my.p.vals$pval < .05
  return(my.p.vals)
}

fill.btwn.lines <- function(X, Y1, Y2, Color, xpd = F){
  index <- is.na(X) | is.na(Y1) | is.na(Y2)
  X <- X[!index]
  Y1 <- Y1[!index]
  Y2 <- Y2[!index]
  poly.x <- c(X, X[length(X):1])
  poly.y <- c(Y1, Y2[length(Y2):1])
  polygon(x = poly.x, y = poly.y, col = Color, border = NA, xpd = xpd)
}


# ---- process data ----

phyt <- phyt$tot
phyt$yday <- yday(phyt$date)
phyt$year <- year(phyt$date)
head(phyt)

phyt.split <- split.data.by.year(tot = phyt)

phyt.interp <- get.daily.interp(vals = phyt.split)

phyt.tab.pre <- get.aligned.tables(x.min = min(phyt$yday), x.max = max(phyt$yday), 
                                   year.range = 1995:2009, my.interp = phyt.interp)
phyt.tab.post <- get.aligned.tables(x.min = min(phyt$yday), x.max = max(phyt$yday), 
                                    year.range = 2010:2020, my.interp = phyt.interp)
phyt.tab.all <- get.aligned.tables(x.min = min(phyt$yday), x.max = max(phyt$yday),
                                   year.range = unique(phyt$year), my.interp = phyt.interp)

phyt.ave.pre <- get.group.aves.etc(my.tab = phyt.tab.pre)
phyt.ave.post <- get.group.aves.etc(my.tab = phyt.tab.post)

phyt.pval <- get.significance(my.pre = phyt.ave.pre, my.post = phyt.ave.post)
head(phyt.pval)

# ---- save data ----

saveRDS(object = phyt.split, file = file.phyt.split)

saveRDS(object = phyt.interp, file = file.phyt.interp)

saveRDS(object = list("pre" = phyt.ave.pre, "post" = phyt.ave.post, "pval" = phyt.pval), file = file.phyto.stats)

saveRDS(object = phyt.tab.all, file = file.phyt.tab.all)

# ---- plot data quickly ----

plot(x = c(min(phyt$yday), max(phyt$yday)), y = c(0, max(phyt$biomass)), type = "n", ann = F)
for (c in 2:ncol(phyt.tab.pre)){
  lines(x = phyt.tab.pre$day, y = phyt.tab.pre[ ,c], col = "steelblue")
}
for (c in 2:ncol(phyt.tab.post)){
  lines(x = phyt.tab.post$day, y = phyt.tab.post[ ,c], col = "orange2")
}
lines(x = phyt.ave.pre$day, y = phyt.ave.pre$ave, col = "steelblue", lwd = 4)
lines(x = phyt.ave.post$day, y = phyt.ave.post$ave, col = "orange2", lwd = 4)
fill.btwn.lines(X = phyt.ave.pre$day,
                Y2 = phyt.ave.pre$ave + phyt.ave.pre$sd,
                Y1 = phyt.ave.pre$ave - phyt.ave.pre$sd, Color = adjustcolor("steelblue",.4))
fill.btwn.lines(X = phyt.ave.post$day,
                Y2 = phyt.ave.post$ave + phyt.ave.post$sd,
                Y1 = phyt.ave.post$ave - phyt.ave.post$sd, Color = adjustcolor("orange2",.4))
mtext(text = "Total Biomass (mg/L)", side = 2, line = 2, outer = F)
mtext(text = "Day of year", side = 1, line = 2, outer = F)


plot(x = c(min(phyt$yday), max(phyt$yday)), y = c(0, max(phyt.ave.post$ave, na.rm = T)), type = "n", ann = F)
lines(x = phyt.ave.pre$day, y = phyt.ave.pre$ave, col = "steelblue", lwd = 4)
lines(x = phyt.ave.post$day, y = phyt.ave.post$ave, col = "orange2", lwd = 4)
fill.btwn.lines(X = phyt.ave.pre$day,
                Y2 = phyt.ave.pre$ave + phyt.ave.pre$sd,
                Y1 = phyt.ave.pre$ave - phyt.ave.pre$sd, Color = adjustcolor("steelblue",.4))
fill.btwn.lines(X = phyt.ave.post$day,
                Y2 = phyt.ave.post$ave + phyt.ave.post$sd,
                Y1 = phyt.ave.post$ave - phyt.ave.post$sd, Color = adjustcolor("orange2",.4))

# ----
pdf(file = file.plot, width = 6.5, height = 3)
# pdf(file = "figs/biomass_increase_at_edges.pdf", width = 6.5, height = 3)
par(mar = c(3,2.8,1,.5), yaxs = "i", xaxs = "i")
plot(x = c(min(phyt$yday), max(phyt$yday)), y = c(0, max(phyt.ave.post$ave, na.rm = T) + max(phyt.ave.post$ave, na.rm = T)/20), type = "n", ann = F, axes = F)
index <- is.na(phyt.pval$is.less.05)
fill.btwn.lines(X = phyt.pval$day[-index], Y1 = rep(0,length(phyt.pval$day[-index])), 
                Y2 = phyt.pval$is.less.05[-index] * 7.5, Color = adjustcolor("grey",.5), xpd = F)
lines(x = phyt.ave.pre$day, y = phyt.ave.pre$ave, col = "steelblue", lwd = 4)
lines(x = phyt.ave.post$day, y = phyt.ave.post$ave, col = "orange2", lwd = 4)
box()
axis(side = 1, labels = F, lwd = 0, lwd.ticks = 1)
axis(side = 1, labels = T, lwd = 0, line = -.4)
axis(side = 2, labels = F, lwd = 0, lwd.ticks = 1)
axis(side = 2, labels = T, lwd = 0, line = -.2, las = 2)
mtext(text = "Mean Biomass (mg/L)", side = 2, line = 1.75, outer = F)
mtext(text = "Day of year", side = 1, line = 1.75, outer = F)
mtext(text = "1995 - 2009", side = 3, at = 100, line = -2, col = "steelblue")
mtext(text = "2010 - 2020", side = 3, at = 100, line = -3.5, col = "orange2")
mtext(text = c("*","*"), side = 3, line = -1, at = c(51, 293))

dev.off()

# ----
days.na <- unique(c(phyt.ave.pre$day[is.na(phyt.ave.pre$ave)], phyt.ave.post$day[is.na(phyt.ave.post$ave)]))
day.start <- max(days.na[days.na < 200])
day.end <- min(days.na[days.na > 200])
abline(v = day.start, lty = 2)
abline(v = day.end, lty = 2)



