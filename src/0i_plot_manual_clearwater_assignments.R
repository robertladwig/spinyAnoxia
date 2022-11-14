# RRR

# ---- set-up ----

library(lubridate)

x <- read.csv("data_processed/0h_secchi_with_manual_season_defns.csv")

season.defs <- readRDS("data_processed/0h_years_with_clearwater_season_dates.rds")

ice <- readRDS(file = "data_processed/0c_ice_seasons_split_by_year.rds")

plot.folder <- "figs/0i_clearwater_season_definitions/"

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

fill.under.lines <- function(X, Y, YAxisMin, Color, xpd = F){
  poly.x <- c(min(X), X, max(X))
  poly.y <- c(YAxisMin, Y, YAxisMin )
  polygon(x = poly.x, y = poly.y, col = Color, border = NA, xpd = xpd)
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

# ---- make plots ----

tail(x)
str(x)
x$Date <- parse_date_time(x = paste(x$Year,x$Month,x$Day), orders = "ymd", tz = "Etc/GMT-5")
x$yDay <- yday(x$Date)

x.ax.lines <- yday(parse_date_time(x = c(paste(2000,1:12,1),"2000 12 31"), orders = "ymd"))
x.ax.labs <- yday(parse_date_time(x = paste(2000,1:12,15), orders = "ymd"))
x.ax.text <- month(parse_date_time(x = paste(2000,1:12,15), orders = "ymd"), label = T, abbr = T)

max(x$Secchi.Depth.m)
y.max <- 13.5


for (yr in unique(x$Year)){
  i <- which(x$Year == yr)
  secc <- x[i, ]
  png(filename = file.path(plot.folder, paste0(yr,".png")), width = 6.5, height = 4, units = "in", res = 150)
  par(mar = c(2.5,3,1.5,0))
  plot(x = c(0,365), y = c(-13.5,0), type = "n", axes = F, ann = F)
  # lines(x = secc$yDay, y = -secc$Secchi.Depth.m)
  fill.under.lines(X = secc$yDay, Y = -secc$Secchi.Depth.m, YAxisMin = 0, Color = adjustcolor("blue",.1))
  fill.under.lines(X = secc$yDay, Y = -secc$Secchi.Depth.m, YAxisMin = -13.3, Color = adjustcolor("blue",.7))
  axis(side = 1, at = x.ax.lines, labels = F, line = 0)
  axis(side = 1, at = x.ax.labs, labels = x.ax.text, lwd = 0, las = 2, cex.axis = .7, line = -.5)
  axis(side = 2, at = seq.int(from = 0, to = -13, by = -2), labels = F, line = 0)
  axis(side = 2, at = seq.int(from = 0, to = -13, by = -2), labels = seq.int(from = 0, to = 13, by = 2), lwd = 0, las = 2, line = -.25)
  mtext(text = yr, side = 3, line = .25, cex = 1.2)
  mtext(text = "Depth (m)", side = 2, line = 2)
  thaw <- yday(ice$Spring.Ice.Off[ice$Summer.Year == yr])
  abline(v = thaw, lwd = 3, lty = 2, col = "snow4")
  abline(v = yday(season.defs$Clear.Start[season.defs$Year == yr]), col = "darkblue", lwd = 3)
  abline(v = yday(season.defs$Clear.End[season.defs$Year == yr]), col = "darkblue", lwd = 3)
  # text(x = secc$yDay, y = -secc$Secchi.Depth.m, labels = secc$yDay, srt = 90, cex = .7)
  points(x = secc$yDay, y = -secc$Secchi.Depth.m, pch = 19, col = "blue", cex = .5)
  dev.off()
}



