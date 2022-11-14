# RRR

# get daily interpolated biomass tables, and av/sd/pval stats, with biomass aligned by stratification onset
# get a list of tables, with elements of the list for regular yday and for each stratification model used

# ---- set-up ----

library(lubridate)

strat <- read.csv(file = "data_processed/stratification_end.csv") # all in year 2000 dates FYI

phyto <- readRDS("data_processed/3b_phyto_split_by_year.rds")

output.folder <- "data_processed/3e_biomass_aligned_by_fall_mixing//"

file.phyto.interps.all <- "data_processed/3e_biomass_aligned_by_fall_mixing_interp_tables_all.rds"
file.phyto.SWF.comparison <- "data_processed/3e_biomass_aligned_by_fall_mixing_interp_tables_pre-post.rds"


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
} # used previously, data imported after this step here

get.daily.interp <- function(vals, my.col){
  # my.col are the column you want to use, yday, or days.since.strat...
  my.interp <- make.empty.list.structure(ListNames = names(vals))
  for (y in 1:length(my.interp)){
    yr <- as.numeric(names(my.interp)[y])
    my.x <- vals[[y]][ ,my.col]
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

get.min.max.dates <- function(my.phy){
  my.min <- NULL
  my.max <- NULL
  for (yr in names(my.phy)){
    my.min <- min(my.min, min(my.phy[[yr]]$day))
    my.max <- max(my.max, max(my.phy[[yr]]$day))
  }
  return(c(my.min, my.max))
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

# ---- prep data ----

strat$linear <- yday(parse_date_time(x = strat$linear, orders = "ymd"))
strat$constant.high <- yday(parse_date_time(x = strat$constant.high, orders = "ymd")) # note 2 of these are 1, so probably should be NA- 2008, 2016
strat$constant.low <- yday(parse_date_time(x = strat$constant.low, orders = "ymd"))
strat$spline <- yday(parse_date_time(x = strat$spline, orders = "ymd"))

# no 1995 in strat data
phyto <- phyto[-1]

for (yr in names(phyto)){
  my.phy <- phyto[[yr]]
  my.str <- strat[strat$year == as.numeric(yr), ,drop=FALSE]
  my.phy$days.since.strat.linear <- my.phy$yday - my.str$linear
  my.phy$days.since.strat.constant.high <- my.phy$yday - my.str$constant.high
  my.phy$days.since.strat.constant.low <- my.phy$yday - my.str$constant.low
  my.phy$days.since.strat.spline <- my.phy$yday - my.str$spline
  phyto[[yr]] <- my.phy
}

# ---- process data ----

colnames(phyto$`1996`)
phyto.linear <- get.daily.interp(vals = phyto, my.col = "days.since.strat.linear")
phyto.high <- get.daily.interp(vals = phyto, my.col = "days.since.strat.constant.high")
phyto.low <- get.daily.interp(vals = phyto, my.col = "days.since.strat.constant.low")
phyto.spline <- get.daily.interp(vals = phyto, my.col = "days.since.strat.spline")
phyto.interp <- get.daily.interp(vals = phyto, my.col = "yday")

phyto.interps.pre <- make.empty.list.structure(ListNames = c("yday","linear","constant.high","constant.low","spline"))
phyto.interps.post <- make.empty.list.structure(ListNames = c("yday","linear","constant.high","constant.low","spline"))
phyto.interps.all <- make.empty.list.structure(ListNames = c("yday","linear","constant.high","constant.low","spline"))

lims <- get.min.max.dates(my.phy = phyto.interp)
phyto.interps.pre$yday <- get.aligned.tables(x.min = lims[1], x.max = lims[2], year.range = 1996:2009, my.interp = phyto.interp)
phyto.interps.post$yday <- get.aligned.tables(x.min = lims[1], x.max = lims[2], year.range = 2010:2020, my.interp = phyto.interp)
phyto.interps.all$yday <- get.aligned.tables(x.min = lims[1], x.max = lims[2], year.range = 1996:2020, my.interp = phyto.interp)
phyto.interps.pre$yday <- get.group.aves.etc(my.tab = phyto.interps.pre$yday)
phyto.interps.post$yday <- get.group.aves.etc(my.tab = phyto.interps.post$yday)
phyto.interps.all$yday <- get.group.aves.etc(my.tab = phyto.interps.all$yday)

lims <- get.min.max.dates(my.phy = phyto.linear)
phyto.interps.pre$linear <- get.aligned.tables(x.min = lims[1], x.max = lims[2], year.range = 1996:2009, my.interp = phyto.linear)
phyto.interps.post$linear <- get.aligned.tables(x.min = lims[1], x.max = lims[2], year.range = 2010:2020, my.interp = phyto.linear)
phyto.interps.all$linear <- get.aligned.tables(x.min = lims[1], x.max = lims[2], year.range = 1996:2020, my.interp = phyto.linear)
phyto.interps.pre$linear <- get.group.aves.etc(my.tab = phyto.interps.pre$linear)
phyto.interps.post$linear <- get.group.aves.etc(my.tab = phyto.interps.post$linear)
phyto.interps.all$linear <- get.group.aves.etc(my.tab = phyto.interps.all$linear)

lims <- get.min.max.dates(my.phy = phyto.high)
phyto.interps.pre$constant.high <- get.aligned.tables(x.min = lims[1], x.max = lims[2], year.range = 1996:2009, my.interp = phyto.high)
phyto.interps.post$constant.high <- get.aligned.tables(x.min = lims[1], x.max = lims[2], year.range = 2010:2020, my.interp = phyto.high)
phyto.interps.all$constant.high <- get.aligned.tables(x.min = lims[1], x.max = lims[2], year.range = 1996:2020, my.interp = phyto.high)
phyto.interps.pre$constant.high <- get.group.aves.etc(my.tab = phyto.interps.pre$constant.high)
phyto.interps.post$constant.high <- get.group.aves.etc(my.tab = phyto.interps.post$constant.high)
phyto.interps.all$constant.high <- get.group.aves.etc(my.tab = phyto.interps.all$constant.high)

lims <- get.min.max.dates(my.phy = phyto.low)
phyto.interps.pre$constant.low <- get.aligned.tables(x.min = lims[1], x.max = lims[2], year.range = 1996:2009, my.interp = phyto.low)
phyto.interps.post$constant.low <- get.aligned.tables(x.min = lims[1], x.max = lims[2], year.range = 2010:2020, my.interp = phyto.low)
phyto.interps.all$constant.low <- get.aligned.tables(x.min = lims[1], x.max = lims[2], year.range = 1996:2020, my.interp = phyto.low)
phyto.interps.pre$constant.low <- get.group.aves.etc(my.tab = phyto.interps.pre$constant.low)
phyto.interps.post$constant.low <- get.group.aves.etc(my.tab = phyto.interps.post$constant.low)
phyto.interps.all$constant.low <- get.group.aves.etc(my.tab = phyto.interps.all$constant.low)

lims <- get.min.max.dates(my.phy = phyto.spline)
phyto.interps.pre$spline <- get.aligned.tables(x.min = lims[1], x.max = lims[2], year.range = 1996:2009, my.interp = phyto.spline)
phyto.interps.post$spline <- get.aligned.tables(x.min = lims[1], x.max = lims[2], year.range = 2010:2020, my.interp = phyto.spline)
phyto.interps.all$spline <- get.aligned.tables(x.min = lims[1], x.max = lims[2], year.range = 1996:2020, my.interp = phyto.spline)
phyto.interps.pre$spline <- get.group.aves.etc(my.tab = phyto.interps.pre$spline)
phyto.interps.post$spline <- get.group.aves.etc(my.tab = phyto.interps.post$spline)
phyto.interps.all$spline <- get.group.aves.etc(my.tab = phyto.interps.all$spline)

phyto.pvals <- make.empty.list.structure(ListNames = c("yday","linear","constant.high","constant.low","spline"))
phyto.pvals$yday <- get.significance(my.pre = phyto.interps.pre$yday, my.post = phyto.interps.post$yday)
phyto.pvals$linear <- get.significance(my.pre = phyto.interps.pre$linear, my.post = phyto.interps.post$linear)
phyto.pvals$constant.high <- get.significance(my.pre = phyto.interps.pre$constant.high, my.post = phyto.interps.post$constant.high)
phyto.pvals$constant.low <- get.significance(my.pre = phyto.interps.pre$constant.low, my.post = phyto.interps.post$constant.low)
phyto.pvals$spline <- get.significance(my.pre = phyto.interps.pre$spline, my.post = phyto.interps.post$spline)

phyto.SWF.comparison <- list("pre" = phyto.interps.pre, "post" = phyto.interps.post, "pval" = phyto.pvals)

# ---- save data ----

saveRDS(object = phyto.interps.all, file = file.phyto.interps.all)
saveRDS(object = phyto.SWF.comparison, file = file.phyto.SWF.comparison)





