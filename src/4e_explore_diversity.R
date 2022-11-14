library(vegan)
library(lubridate)

phyto.list <- readRDS("robin-data/2022-06-08_phyto_list.rds")
# key <- readRDS("robin-data/2022-07-25_season_dates/seasons_by_sample.rds")
key <- readRDS("robin-data/2022-10-02_season_dates/seasons_by_sample.rds")

# plot.folder <- "plots/2022-08-02_diversity_plots/"
plot.folder <- "plots/2022-10-02_diversity_plots/"

data.folder <- "robin-data/2022-10-02_diversity_metrics/"

phyto.list$div[is.na(phyto.list$div)] <- 0
phyto.list$gen[is.na(phyto.list$gen)] <- 0
phyto.list$tax[is.na(phyto.list$tax)] <- 0

diversity.table <- data.frame(key)

# division-level
diversity.table$division.shannon <- diversity(x = phyto.list$div, index = "shannon", MARGIN = 2)

diversity.table$division.simpson <- diversity(x = phyto.list$div, index = "simpson", MARGIN = 2)

diversity.table$division.invsimpson <- diversity(x = phyto.list$div, index = "invsimpson", MARGIN = 2)

pres.abs <- phyto.list$div > 0
diversity.table$division.richness <- colSums(pres.abs)

# genera-level
diversity.table$genera.shannon <- diversity(x = phyto.list$gen, index = "shannon", MARGIN = 2)

diversity.table$genera.simpson <- diversity(x = phyto.list$gen, index = "simpson", MARGIN = 2)

diversity.table$genera.invsimpson <- diversity(x = phyto.list$gen, index = "invsimpson", MARGIN = 2)

pres.abs <- phyto.list$gen > 0
diversity.table$genera.richness <- colSums(pres.abs)

# taxa-level
diversity.table$taxon.shannon <- diversity(x = phyto.list$tax, index = "shannon", MARGIN = 2)

diversity.table$taxon.simpson <- diversity(x = phyto.list$tax, index = "simpson", MARGIN = 2)

diversity.table$taxon.invsimpson <- diversity(x = phyto.list$tax, index = "invsimpson", MARGIN = 2)

pres.abs <- phyto.list$tax > 0
diversity.table$taxon.richness <- colSums(pres.abs)

head(diversity.table)

my.file <- file.path(data.folder,"diversity_by_sample.rds")
cat(my.file)
saveRDS(object = diversity.table, file = my.file)

# ---- get diversity year averages ----

diversity.by.year <- data.frame("Year" = rep(x = 1996:2020, each = 4), "Season" = c("ice-on","spring","stratified","fall"), 
                                "division.shannon"=NA, "division.simpson"=NA, "division.invsimpson"=NA,"division.richness"=NA,
                                "genera.shannon"=NA, "genera.simpson"=NA, "genera.invsimpson"=NA,"genera.richness"=NA,
                                "taxon.shannon"=NA, "taxon.simpson"=NA, "taxon.invsimpson"=NA,"taxon.richness"=NA)

for (yr in unique(diversity.by.year$Year)){
  for (s in unique(diversity.by.year$Season)){
    for (c in 5:16){
      my.metric <- colnames(diversity.table)[c]
      my.col <- which(colnames(diversity.by.year) == my.metric)
      my.data <- diversity.table[diversity.table$Year == yr & diversity.table$Season == s,c]
      diversity.by.year[diversity.by.year$Year == yr & diversity.by.year$Season == s, my.col] <- mean(my.data, na.rm = T)
    }
  }
}
head(diversity.by.year)

my.file <- file.path(data.folder,"diversity_by_year.rds")
cat(my.file)
saveRDS(object = diversity.by.year, file = my.file)


# ---- get diversity invasion-group averages ----

empty.table <- data.frame("metric" = c("mean.pre","sd.pre","mean.post","sd.post","p.value"), 
                          "division.shannon"=NA, "division.simpson"=NA, "division.invsimpson"=NA,"division.richness"=NA,
                          "genera.shannon"=NA, "genera.simpson"=NA, "genera.invsimpson"=NA,"genera.richness"=NA,
                          "taxon.shannon"=NA, "taxon.simpson"=NA, "taxon.invsimpson"=NA,"taxon.richness"=NA)
diversity.pvals <- list("ice-on" = empty.table, "spring" = empty.table, "stratified" = empty.table, "fall" = empty.table)

for (s in names(diversity.pvals)){
  for (c in colnames(diversity.pvals[[s]])[-1]){
    pre <- diversity.by.year$Year < 2010 & diversity.by.year$Season == s
    post <- diversity.by.year$Year >= 2010 & diversity.by.year$Season == s
    diversity.pvals[[s]][1,c] <- mean(diversity.by.year[pre,c], na.rm = T)
    diversity.pvals[[s]][2,c] <- sd(diversity.by.year[pre,c], na.rm = T)
    diversity.pvals[[s]][3,c] <- mean(diversity.by.year[post,c], na.rm = T)
    diversity.pvals[[s]][4,c] <- sd(diversity.by.year[post,c], na.rm = T)
    # my.t <- t.test(x = diversity.by.year[pre,c], y = diversity.by.year[post,c])
    # my.t <- kruskal.test(x = list(x = diversity.by.year[pre,c], y = diversity.by.year[post,c]))
    my.t <- wilcox.test(x = diversity.by.year[pre,c], y = diversity.by.year[post,c])
    diversity.pvals[[s]][5,c] <- my.t$p.value
  }
}

my.file <- file.path(data.folder,"diversity_by_invasion.rds")
cat(my.file)
saveRDS(object = diversity.pvals, file = my.file)

# look at diversity over time ----

x.ax <- unique(diversity.table$Year)
x.ax.ticks <- parse_date_time(paste0("1-1-",x.ax), "mdy")
index <- seq.int(from = 1, by = 5, length.out = 6)
x.ax.lab.loc <- x.ax.ticks[index]
x.ax.lab <- x.ax[index]

c = 5 # 5-16
for (c in colnames(diversity.table)[5:16]){
  
  filename <- paste0(plot.folder, c,".pdf")
  cat(filename, "\n")
  pdf(file = filename, width = 8, height = 10.5)
  
  # all dates
  r <-1:nrow(diversity.table)
  pre <- diversity.table$Year < 2010
  post <- diversity.table$Year >= 2010
  
  par(fig = c(0,.8,.8,1), mar = c(2,2,.5,.5), oma = c(.1,1,.1,1), cex.axis = .7)
  plot(x = diversity.table$Date[r], y = diversity.table[r,c], type = "n", ann = F, axes = F)
  points(x = diversity.table$Date[r], y = diversity.table[r,c], cex = .5)
  lines(x = diversity.table$Date[r], y = diversity.table[r,c])
  mtext(text = "all dates", side = 4, line = .5)
  box()
  axis(side = 1, lwd = 0, lwd.ticks = 1, at = x.ax.ticks, labels = F)
  axis(side = 1, lwd = 0, labels = x.ax.lab, at = x.ax.lab.loc, line = -.5)
  axis(side = 2,lwd = 0, lwd.ticks = 1, labels = F)
  axis(side = 2,las = 2, lwd = 0, labels = T, line = -.5)
  
  par(fig = c(.8,1,.8,1), mar = c(2,2,.5,.5), oma = c(.1,2,.1,0), new = T)
  boxplot(x = list("pre" = diversity.table[pre,c], "post" = diversity.table[post,c]), axes = F)
  box()
  axis(side = 1, at = c(1,2), labels = F, lwd = 0, lwd.ticks = 1)
  axis(side = 1, at = c(1,2), labels = c("pre","post"), lwd = 0, line = -.5)
  axis(side = 2,lwd = 0, lwd.ticks = 1, labels = F)
  axis(side = 2,las = 2, lwd = 0, labels = T, line = -.5)
  
  # spring
  r <- diversity.table$Season == "spring"
  # pre <- diversity.table$Season == "spring" & diversity.table$Year < 2010
  # post <- diversity.table$Season == "spring" & diversity.table$Year >= 2010
  pre <- diversity.by.year$Season == "spring" & diversity.by.year$Year < 2010
  post <- diversity.by.year$Season == "spring" & diversity.by.year$Year >= 2010
  
  par(fig = c(0,.8,.6,.8), mar = c(2,2,.5,.5), oma = c(.1,1,.1,1), new = T)
  plot(x = diversity.table$Date[r], y = diversity.table[r,c], type = "n", ann = F, axes = F)
  points(x = diversity.table$Date[r], y = diversity.table[r,c], cex = .5)
  lines(x = diversity.table$Date[r], y = diversity.table[r,c])
  mtext(text = "spring", side = 4, line = .5)
  box()
  axis(side = 1, lwd = 0, lwd.ticks = 1, at = x.ax.ticks, labels = F)
  axis(side = 1, lwd = 0, labels = x.ax.lab, at = x.ax.lab.loc, line = -.5)
  axis(side = 2,lwd = 0, lwd.ticks = 1, labels = F)
  axis(side = 2,las = 2, lwd = 0, labels = T, line = -.5)
  
  par(fig = c(.8,1,.6,.8), mar = c(2,2,.5,.5), oma = c(.1,2,.1,0), new = T)
  # boxplot(x = list("pre" = diversity.table[pre,c], "post" = diversity.table[post,c]), axes = F)
  boxplot(x = list("pre" = diversity.by.year[pre,c], "post" = diversity.by.year[post,c]), axes = F)
  stripchart(x = list("pre" = diversity.by.year[pre,c], "post" = diversity.by.year[post,c]), method = "jitter", jitter = .3, vertical = T, add = T, pch = 19, cex = .5)
  box()
  my.p <- diversity.pvals$spring[5,c]
  mtext(text = paste("p =", round(my.p, 6)), side = 3, cex = .7, xpd = NA)
  axis(side = 1, at = c(1,2), labels = F, lwd = 0, lwd.ticks = 1)
  axis(side = 1, at = c(1,2), labels = c("pre","post"), lwd = 0, line = -.5)
  axis(side = 2,lwd = 0, lwd.ticks = 1, labels = F)
  axis(side = 2,las = 2, lwd = 0, labels = T, line = -.5)
  
  # stratified
  r <- diversity.table$Season == "stratified"
  # pre <- diversity.table$Season == "stratified" & diversity.table$Year < 2010
  # post <- diversity.table$Season == "stratified" & diversity.table$Year >= 2010
  pre <- diversity.by.year$Season == "stratified" & diversity.by.year$Year < 2010
  post <- diversity.by.year$Season == "stratified" & diversity.by.year$Year >= 2010
  
  par(fig = c(0,.8,.4,.6), mar = c(2,2,.5,.5), oma = c(.1,1,.1,1), new = T)
  plot(x = diversity.table$Date[r], y = diversity.table[r,c], type = "n", ann = F, axes = F)
  points(x = diversity.table$Date[r], y = diversity.table[r,c], cex = .5, xpd = NA)
  lines(x = diversity.table$Date[r], y = diversity.table[r,c])
  mtext(text = "stratified", side = 4, line = .5)
  box()
  axis(side = 1, lwd = 0, lwd.ticks = 1, at = x.ax.ticks, labels = F)
  axis(side = 1, lwd = 0, labels = x.ax.lab, at = x.ax.lab.loc, line = -.5)
  axis(side = 2,lwd = 0, lwd.ticks = 1, labels = F)
  axis(side = 2,las = 2, lwd = 0, labels = T, line = -.5)
  
  par(fig = c(.8,1,.4,.6), mar = c(2,2,.5,.5), oma = c(.1,2,.1,0), new = T)
  # boxplot(x = list("pre" = diversity.table[pre,c], "post" = diversity.table[post,c]), axes = F)
  boxplot(x = list("pre" = diversity.by.year[pre,c], "post" = diversity.by.year[post,c]), axes = F)
  stripchart(x = list("pre" = diversity.by.year[pre,c], "post" = diversity.by.year[post,c]), method = "jitter", jitter = .3, vertical = T, add = T, pch = 19, cex = .5)
  box()
  my.p <- diversity.pvals$stratified[5,c]
  mtext(text = paste("p =", round(my.p, 6)), side = 3, cex = .7)
  axis(side = 1, at = c(1,2), labels = F, lwd = 0, lwd.ticks = 1)
  axis(side = 1, at = c(1,2), labels = c("pre","post"), lwd = 0, line = -.5)
  axis(side = 2,lwd = 0, lwd.ticks = 1, labels = F)
  axis(side = 2,las = 2, lwd = 0, labels = T, line = -.5)
  
  mtext(text = c, side = 2, outer = T, line = .75)
  
  # fall
  r <- diversity.table$Season == "fall"
  # pre <- diversity.table$Season == "fall" & diversity.table$Year < 2010
  # post <- diversity.table$Season == "fall" & diversity.table$Year >= 2010
  pre <- diversity.by.year$Season == "fall" & diversity.by.year$Year < 2010
  post <- diversity.by.year$Season == "fall" & diversity.by.year$Year >= 2010
  
  par(fig = c(0,.8,.2,.4), mar = c(2,2,.5,.5), oma = c(.1,1,.1,1), new = T)
  plot(x = diversity.table$Date[r], y = diversity.table[r,c], type = "n", ann = F, axes = F)
  points(x = diversity.table$Date[r], y = diversity.table[r,c], cex = .5)
  lines(x = diversity.table$Date[r], y = diversity.table[r,c])
  mtext(text = "fall", side = 4, line = .5)
  box()
  axis(side = 1, lwd = 0, lwd.ticks = 1, at = x.ax.ticks, labels = F)
  axis(side = 1, lwd = 0, labels = x.ax.lab, at = x.ax.lab.loc, line = -.5)
  axis(side = 2,lwd = 0, lwd.ticks = 1, labels = F)
  axis(side = 2,las = 2, lwd = 0, labels = T, line = -.5)
  
  par(fig = c(.8,1,.2,.4), mar = c(2,2,.5,.5), oma = c(.1,2,.1,0), new = T)
  # boxplot(x = list("pre" = diversity.table[pre,c], "post" = diversity.table[post,c]), axes = F)
  boxplot(x = list("pre" = diversity.by.year[pre,c], "post" = diversity.by.year[post,c]), axes = F)
  stripchart(x = list("pre" = diversity.by.year[pre,c], "post" = diversity.by.year[post,c]), method = "jitter", jitter = .3, vertical = T, add = T, pch = 19, cex = .5)
  box()
  my.p <- diversity.pvals$fall[5,c]
  mtext(text = paste("p =", round(my.p, 6)), side = 3, cex = .7, xpd = NA)
  axis(side = 1, at = c(1,2), labels = F, lwd = 0, lwd.ticks = 1)
  axis(side = 1, at = c(1,2), labels = c("pre","post"), lwd = 0, line = -.5)
  axis(side = 2,lwd = 0, lwd.ticks = 1, labels = F)
  axis(side = 2,las = 2, lwd = 0, labels = T, line = -.5)
  
  
  # ice-on
  r <- diversity.table$Season == "ice-on"
  # pre <- diversity.table$Season == "ice-on" & diversity.table$Year < 2010
  # post <- diversity.table$Season == "ice-on" & diversity.table$Year >= 2010
  pre <- diversity.by.year$Season == "ice-on" & diversity.by.year$Year < 2010
  post <- diversity.by.year$Season == "ice-on" & diversity.by.year$Year >= 2010
  
  par(fig = c(0,.8,0,.2), mar = c(2,2,.5,.5), oma = c(.1,1,.1,1), new = T)
  plot(x = diversity.table$Date[r], y = diversity.table[r,c], type = "n", ann = F, axes = F)
  points(x = diversity.table$Date[r], y = diversity.table[r,c], cex = .5)
  lines(x = diversity.table$Date[r], y = diversity.table[r,c])
  mtext(text = "ice-on", side = 4, line = .5)
  box()
  axis(side = 1, lwd = 0, lwd.ticks = 1, at = x.ax.ticks, labels = F)
  axis(side = 1, lwd = 0, labels = x.ax.lab, at = x.ax.lab.loc, line = -.5)
  axis(side = 2,lwd = 0, lwd.ticks = 1, labels = F)
  axis(side = 2,las = 2, lwd = 0, labels = T, line = -.5)
  
  par(fig = c(.8,1,.0,.2), mar = c(2,2,.5,.5), oma = c(.1,2,.1,0), new = T)
  # boxplot(x = list("pre" = diversity.table[pre,c], "post" = diversity.table[post,c]), axes = F)
  boxplot(x = list("pre" = diversity.by.year[pre,c], "post" = diversity.by.year[post,c]), axes = F)
  stripchart(x = list("pre" = diversity.by.year[pre,c], "post" = diversity.by.year[post,c]), method = "jitter", jitter = .3, vertical = T, add = T, pch = 19, cex = .5)
  box()
  my.p <- diversity.pvals$`ice-on`[5,c]
  mtext(text = paste("p =", round(my.p, 6)), side = 3, cex = .7, xpd = NA)
  axis(side = 1, at = c(1,2), labels = F, lwd = 0, lwd.ticks = 1)
  axis(side = 1, at = c(1,2), labels = c("pre","post"), lwd = 0, line = -.5)
  axis(side = 2,lwd = 0, lwd.ticks = 1, labels = F)
  axis(side = 2,las = 2, lwd = 0, labels = T, line = -.5)
  
  dev.off()
}
