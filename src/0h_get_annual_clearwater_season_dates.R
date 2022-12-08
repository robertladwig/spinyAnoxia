# RRR

library(lubridate)

manual <- read.csv(file = "data_processed/0h_secchi_with_manual_season_defns.csv")

created.season.dates.csv <- "data_processed/0h_years_with_clearwater_season_dates.csv"
created.season.dates.rds <- "data_processed/0h_years_with_clearwater_season_dates.rds"

# ---- get yearly season dates ----

head(manual)
manual$Date <- parse_date_time(x = paste(manual$Year, manual$Month, manual$Day), orders = "ymd", tz = "Etc/GMT-5")

yearly <- data.frame("Year" = unique(manual$Year), "Clear.Start" = manual$Date[1], "Clear.End" = manual$Date[1], "Max.Clearwater.Depth.m" = NA, "Max.Clearwater.Depth.Date" = manual$Date[1])

for (yr in unique(manual$Year)){
  my.secc <- manual[manual$Year == yr, ,drop = F]
  my.yearly <- yearly[yearly$Year == yr, ,drop = F]
  if(any(is.na(my.secc$Secchi.Season))){
    my.yearly$Clear.Start <- NA
    my.yearly$Clear.End <- NA
    my.yearly$Max.Clearwater.Depth.m <- NA
    my.yearly$Max.Clearwater.Depth.Date <- NA[1]
  }else{
    i.clear <- which(my.secc$Secchi.Season == "clearwater")
    i.start <- i.clear[1]
    i.end <- i.clear[length(i.clear)]
    
    last.day.bloom <- my.secc$Date[i.start - 1]
    first.day.clear <- my.secc$Date[i.start] 
    
    last.day.clear <- my.secc$Date[i.end]
    first.day.sum <- my.secc$Date[i.end + 1]
    
    clear.start <- ceiling_date((first.day.clear - last.day.bloom) / 2 + last.day.bloom, unit = "day", change_on_boundary = FALSE) # change_on_boundary = TRUE
    clear.end <- floor_date((first.day.sum - last.day.clear) / 2 + last.day.clear, unit = "day") 
    
    clear.start <- force_tz(time = clear.start, tzone = "Etc/GMT-5")
    clear.end <- force_tz(time = clear.end, tzone = "Etc/GMT-5")
    
    max.depth <- max(my.secc$Secchi.Depth.m[i.clear])
    max.date <- my.secc$Date[i.clear][my.secc$Secchi.Depth.m[i.clear] == max.depth]
    max.date <- force_tz(time = max.date, tzone = "Etc/GMT-5")
    
    my.yearly$Clear.Start <- clear.start
    my.yearly$Clear.End <- clear.end
    my.yearly$Max.Clearwater.Depth.m <- max.depth
    my.yearly$Max.Clearwater.Depth.Date <- max.date[1]
  }
  yearly[yearly$Year == yr, ] <- my.yearly
}


# ---- add NA for missing years ----

temp <- data.frame("Year" = 1900:2020)
yearly <- merge(x = temp, y = yearly, by = "Year", all = T)


# ---- refine season boundaries based on limony data ----
# from 2000 - 2011 limony 16S data has samples on days lacking secchi measurements, 
# can refine the boundaries based on chloroplasts and cyanos abundances.
# if sample has high chloroplasts, it's still in spring bloom. if has high cyanos, it's into the summer bloom.

yearly[yearly$Year == 2000, "Clear.Start"] <- parse_date_time(x = "4/12/00", orders = "mdy", tz = "Etc/GMT-5") # 4/14 is in btwn secchi measures, include it in clearwater b/c chloroplasts had dropped. Clear start is now halfway btwn last secchi on 4/10 and 16S on 4/14 instead of halfway btwn the 2 secchis on 4/10 and 4/25. BCC it is not well clustered with anything.
yearly[yearly$Year == 2000, "Clear.End"] <- parse_date_time(x = "5/31/00", orders = "mdy", tz = "Etc/GMT-5") # difficult call. Went with more stringent defn, used midpoint btwn 16S sample and secchi sample. Including the second dip puts clearwater overlapping with anoxia.
yearly[yearly$Year == 2001, "Clear.End"] <- parse_date_time(x = "6/22/01", orders = "mdy", tz = "Etc/GMT-5") # 6/18 is on its way up already, do midpoint btwn 6/18 secchi and establishing lake anoxia.
yearly[yearly$Year == 2002, "Clear.Start"] <- parse_date_time(x = "4/24/02", orders = "mdy", tz = "Etc/GMT-5") # sparse secchi. 4/24 is getting clear, 4/17 and 4/23 both look like spring bloom from BCC, so have it start on 4/24
yearly[yearly$Year == 2004, "Clear.End"] <- parse_date_time(x = "6/23/04", orders = "mdy", tz = "Etc/GMT-5") # bimodal, tough call. Including second dip, but using cyano increase as endpoint instead of later secchi
yearly[yearly$Year == 2005, "Clear.End"] <- parse_date_time(x = "6/21/05", orders = "mdy", tz = "Etc/GMT-5") # adjust endpoint with start of cyanos in closer sample
yearly[yearly$Year == 2006, "Clear.Start"] <- parse_date_time(x = "3/24/06", orders = "mdy", tz = "Etc/GMT-5") # no spring bloom, so clear.start == ice.off
yearly[yearly$Year == 2006, "Clear.End"] <- parse_date_time(x = "6/17/06", orders = "mdy", tz = "Etc/GMT-5") # adjusted endpoint with closer cyano sample
yearly[yearly$Year == 2007, "Clear.End"] <- parse_date_time(x = "6/5/07", orders = "mdy", tz = "Etc/GMT-5") # day rounding
yearly[yearly$Year == 2009, "Clear.Start"] <- parse_date_time(x = "5/2/09", orders = "mdy", tz = "Etc/GMT-5") # can narrow date using chloroplast drop
yearly[yearly$Year == 2009, "Clear.End"] <- parse_date_time(x = "6/24/09", orders = "mdy", tz = "Etc/GMT-5") # using earlier cyano sample to narrow endpoint
yearly[yearly$Year == 2010, "Clear.Start"] <- parse_date_time(x = "5/14/10", orders = "mdy", tz = "Etc/GMT-5") # can narrow date using drop in chloroplasts
yearly[yearly$Year == 2010, "Clear.End"] <- parse_date_time(x = "6/04/10", orders = "mdy", tz = "Etc/GMT-5") # narrowed with 16S sample
yearly[yearly$Year == 2012, "Clear.Start"] <- parse_date_time(x = "4/10/12", orders = "mdy", tz = "Etc/GMT-5") # day rounding
yearly[yearly$Year == 2013, "Clear.End"] <- parse_date_time(x = "6/17/13", orders = "mdy", tz = "Etc/GMT-5") # day rounding

# ---- export ----

write.csv(x = yearly, file = created.season.dates.csv, quote = T, row.names = F)
saveRDS(object = yearly, file = created.season.dates.rds)


