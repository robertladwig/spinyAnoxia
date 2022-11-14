# RRR
# get the long-term southern lakes ice data from EDI
# filter out just Mendota
# save as rds in my data folder
# https://portal.edirepository.org/nis/codeGeneration?packageId=knb-lter-ntl.33.35&statisticalFileType=r

my.format.rds <- "data_input/LTER_Mendota_ice_knb-lter-ntl.33.35.rds"

# ---- EDI download script ----

# Package ID: knb-lter-ntl.33.35 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: Ice Duration - Madison Lakes Area 1853 - current.
# Data set creator:  John Magnuson - University of Wisconsin 
# Data set creator:  Stephen Carpenter - University of Wisconsin 
# Data set creator:  Emily Stanley - University of Wisconsin 
# Contact:  NTL Information Manager -  University of Wisconsin  - ntl.infomgr@gmail.com
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/33/35/f5bc02452cafcd461c49bd7429d8b40c" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

                   
 dt1 <-read.csv(infile1,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "lakeid",     
                    "season",     
                    "iceon",     
                    "ice_on",     
                    "iceoff",     
                    "ice_off",     
                    "ice_duration",     
                    "year4"    ), check.names=TRUE)
               
unlink(infile1)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                
if (class(dt1$lakeid)!="factor") dt1$lakeid<- as.factor(dt1$lakeid)
if (class(dt1$season)!="factor") dt1$season<- as.factor(dt1$season)
if (class(dt1$iceon)=="factor") dt1$iceon <-as.numeric(levels(dt1$iceon))[as.integer(dt1$iceon) ]               
if (class(dt1$iceon)=="character") dt1$iceon <-as.numeric(dt1$iceon)
if (class(dt1$iceoff)=="factor") dt1$iceoff <-as.numeric(levels(dt1$iceoff))[as.integer(dt1$iceoff) ]               
if (class(dt1$iceoff)=="character") dt1$iceoff <-as.numeric(dt1$iceoff)
if (class(dt1$ice_duration)=="factor") dt1$ice_duration <-as.numeric(levels(dt1$ice_duration))[as.integer(dt1$ice_duration) ]               
if (class(dt1$ice_duration)=="character") dt1$ice_duration <-as.numeric(dt1$ice_duration)
if (class(dt1$year4)=="factor") dt1$year4 <-as.numeric(levels(dt1$year4))[as.integer(dt1$year4) ]               
if (class(dt1$year4)=="character") dt1$year4 <-as.numeric(dt1$year4)
                
# Convert Missing Values to NA for non-dates
                


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(lakeid)
summary(season)
summary(iceon)
summary(ice_on)
summary(iceoff)
summary(ice_off)
summary(ice_duration)
summary(year4) 
                # Get more details on character variables
                 
summary(as.factor(dt1$lakeid)) 
summary(as.factor(dt1$season))
detach(dt1)               
        
# ---- my processing ----
 
library(lubridate)

colnames(dt1)

index <- which(dt1$lakeid == "ME")
ice <- dt1[index, ]
sum(duplicated(ice$year4)) # seems it doesn't include mult on/off per season
head(ice)
cbind(1:ncol(ice),colnames(ice))
ice <- ice[ ,c(2,8,4,6,7,8)]
head(ice)

# just get rid of the confusing missing data in the second year, so remove 1853 and 1854 since 54 is not recorded
ice <- ice[-(1:2), ]
head(ice)
tail(ice)

# # fill missing years
# ice <- rbind(ice[1:2, ], data.frame(season = "1854-1855", year4 = 1854, ice_on = NA, ice_off = NA, ice_duration = NA, year4.1 = 1854), ice[3:nrow(ice), ])
# all.equal(1852:2019, ice$year4) # good that was the only one

ice$ice_on <- parse_date_time(x = ice$ice_on, orders = "ymd", tz = "Etc/GMT-5")
ice$ice_off <- parse_date_time(x = ice$ice_off, orders = "ymd", tz = "Etc/GMT-5")

head(ice)
colnames(ice)[1] <- "Winter.Years"
colnames(ice)[2] <- "Summer.Year"
colnames(ice)[3] <- "Fall.Ice.On"
colnames(ice)[4] <- "Spring.Ice.Off"
colnames(ice)[5] <- "Prev.Winter.Duration"
ice <- ice[ ,c(1,2,4,3,5)]
ice$Summer.Year <- substr(x = ice$Winter.Years, start = 6, stop = 9)
head(ice)
tail(ice)
ice$Fall.Ice.On <- c(ice$Fall.Ice.On[-1],NA)
# ice$Prev.Winter.Duration <- c(ice$Prev.Winter.Duration[-1],NA)
head(ice)
tail(ice)

ice <- ice[ ,-1]

head(ice)
tail(ice)

# ---- save file ----

cat("Making file: ", my.format.rds)
saveRDS(object = ice, file = my.format.rds)

# ~ end ~