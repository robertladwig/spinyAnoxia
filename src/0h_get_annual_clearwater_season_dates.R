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
    
    clear.start <- ceiling_date((first.day.clear - last.day.bloom) / 2 + last.day.bloom, unit = "day", change_on_boundary = TRUE)
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

# ---- export ----

write.csv(x = yearly, file = created.season.dates.csv, quote = T, row.names = F)
saveRDS(object = yearly, file = created.season.dates.rds)


