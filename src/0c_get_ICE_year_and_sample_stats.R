# RRR
# I already formatted the ice quite a bit for my "summer centric" perspective
# split at new year so that there are spring and fall on/off dates separately, with NAs for no ice

library("lubridate")

ice <- readRDS("data_input/LTER_Mendota_ice_knb-lter-ntl.33.35.rds")

# ---- reformat ----

head(ice)

ice <- ice[ ,c(1,4,2,3)]
str(ice)
ice$Summer.Year <- as.numeric(ice$Summer.Year)

ice$Spring.Ice.On <- parse_date_time(x = paste(ice$Summer.Year,1,1), orders = "ymd", tz = "Etc/GMT-5")
ice$Fall.Ice.Off <- parse_date_time(x = paste(ice$Summer.Year,12,31), orders = "ymd", tz = "Etc/GMT-5")
ice <- ice[ ,c(1,2,5,3,4,6)]
head(ice)
for (r in 2:nrow(ice)){
  if (year(ice$Fall.Ice.On[r-1]) == ice$Summer.Year[r]){
    ice$Spring.Ice.On[r] <- ice$Fall.Ice.On[r-1]
    ice$Fall.Ice.On[r-1] <- NA
    ice$Fall.Ice.Off[r-1] <- NA
  }
}
head(ice)
tail(ice)
ice$Fall.Ice.Off[ice$Summer.Year == 2020] <- NA

# ---- export ----

saveRDS(object = ice, file = "data_processed/0c_ice_seasons_split_by_year.rds")


