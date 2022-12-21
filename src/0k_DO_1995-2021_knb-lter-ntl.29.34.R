# https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-ntl.29.34
# doi:10.6073/pasta/d768a48454a0154bdaa5c93a34c6c672

save.data.as <- "data_input/0k_DO_1995-2021_knb-lter-ntl.29.34.rds"

# ---- download script ----

# Package ID: knb-lter-ntl.29.34 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: Physical Limnology of Primary Study Lakes 1981 - current.
# Data set creator:  John Magnuson - University of Wisconsin 
# Data set creator:  Stephen Carpenter - University of Wisconsin 
# Data set creator:  Emily Stanley - University of Wisconsin 
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/29/34/03e232a1b362900e0f059859abe8eb97" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "lakeid",     
                 "year4",     
                 "daynum",     
                 "sampledate",     
                 "depth",     
                 "rep",     
                 "sta",     
                 "event",     
                 "wtemp",     
                 "o2",     
                 "o2sat",     
                 "deck",     
                 "light",     
                 "frlight",     
                 "flagdepth",     
                 "flagwtemp",     
                 "flago2",     
                 "flago2sat",     
                 "flagdeck",     
                 "flaglight",     
                 "flagfrlight"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$lakeid)!="factor") dt1$lakeid<- as.factor(dt1$lakeid)
if (class(dt1$year4)=="factor") dt1$year4 <-as.numeric(levels(dt1$year4))[as.integer(dt1$year4) ]               
if (class(dt1$year4)=="character") dt1$year4 <-as.numeric(dt1$year4)
if (class(dt1$daynum)=="factor") dt1$daynum <-as.numeric(levels(dt1$daynum))[as.integer(dt1$daynum) ]               
if (class(dt1$daynum)=="character") dt1$daynum <-as.numeric(dt1$daynum)                                   
# attempting to convert dt1$sampledate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1sampledate<-as.Date(dt1$sampledate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1sampledate) == length(tmp1sampledate[!is.na(tmp1sampledate)])){dt1$sampledate <- tmp1sampledate } else {print("Date conversion failed for dt1$sampledate. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1sampledate) 
if (class(dt1$depth)=="factor") dt1$depth <-as.numeric(levels(dt1$depth))[as.integer(dt1$depth) ]               
if (class(dt1$depth)=="character") dt1$depth <-as.numeric(dt1$depth)
if (class(dt1$rep)!="factor") dt1$rep<- as.factor(dt1$rep)
if (class(dt1$sta)!="factor") dt1$sta<- as.factor(dt1$sta)
if (class(dt1$event)!="factor") dt1$event<- as.factor(dt1$event)
if (class(dt1$wtemp)=="factor") dt1$wtemp <-as.numeric(levels(dt1$wtemp))[as.integer(dt1$wtemp) ]               
if (class(dt1$wtemp)=="character") dt1$wtemp <-as.numeric(dt1$wtemp)
if (class(dt1$o2)=="factor") dt1$o2 <-as.numeric(levels(dt1$o2))[as.integer(dt1$o2) ]               
if (class(dt1$o2)=="character") dt1$o2 <-as.numeric(dt1$o2)
if (class(dt1$o2sat)=="factor") dt1$o2sat <-as.numeric(levels(dt1$o2sat))[as.integer(dt1$o2sat) ]               
if (class(dt1$o2sat)=="character") dt1$o2sat <-as.numeric(dt1$o2sat)
if (class(dt1$deck)=="factor") dt1$deck <-as.numeric(levels(dt1$deck))[as.integer(dt1$deck) ]               
if (class(dt1$deck)=="character") dt1$deck <-as.numeric(dt1$deck)
if (class(dt1$light)=="factor") dt1$light <-as.numeric(levels(dt1$light))[as.integer(dt1$light) ]               
if (class(dt1$light)=="character") dt1$light <-as.numeric(dt1$light)
if (class(dt1$frlight)!="factor") dt1$frlight<- as.factor(dt1$frlight)
if (class(dt1$flagdepth)!="factor") dt1$flagdepth<- as.factor(dt1$flagdepth)
if (class(dt1$flagwtemp)!="factor") dt1$flagwtemp<- as.factor(dt1$flagwtemp)
if (class(dt1$flago2)!="factor") dt1$flago2<- as.factor(dt1$flago2)
if (class(dt1$flago2sat)!="factor") dt1$flago2sat<- as.factor(dt1$flago2sat)
if (class(dt1$flagdeck)!="factor") dt1$flagdeck<- as.factor(dt1$flagdeck)
if (class(dt1$flaglight)!="factor") dt1$flaglight<- as.factor(dt1$flaglight)
if (class(dt1$flagfrlight)!="factor") dt1$flagfrlight<- as.factor(dt1$flagfrlight)

# Convert Missing Values to NA for non-dates



# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(lakeid)
summary(year4)
summary(daynum)
summary(sampledate)
summary(depth)
summary(rep)
summary(sta)
summary(event)
summary(wtemp)
summary(o2)
summary(o2sat)
summary(deck)
summary(light)
summary(frlight)
summary(flagdepth)
summary(flagwtemp)
summary(flago2)
summary(flago2sat)
summary(flagdeck)
summary(flaglight)
summary(flagfrlight) 
# Get more details on character variables

summary(as.factor(dt1$lakeid)) 
summary(as.factor(dt1$rep)) 
summary(as.factor(dt1$sta)) 
summary(as.factor(dt1$event)) 
summary(as.factor(dt1$frlight)) 
summary(as.factor(dt1$flagdepth)) 
summary(as.factor(dt1$flagwtemp)) 
summary(as.factor(dt1$flago2)) 
summary(as.factor(dt1$flago2sat)) 
summary(as.factor(dt1$flagdeck)) 
summary(as.factor(dt1$flaglight)) 
summary(as.factor(dt1$flagfrlight))
detach(dt1)               


# ---- RRR get Mendota O2 ----

library(lubridate)
library(tidyr)

index <- which(dt1$lakeid == "ME")
men <- dt1[index, ]
str(men)
unique(men$sta) # only 1 station, must be deep hole
unique(men$rep) # only 1
unique(men$event) # empty

unique(men$flagdepth) # no flags
unique(men$flago2) # no flags

cbind(1:ncol(men), colnames(men))

oxy <- data.frame("Year" = year(men$sampledate), "Month" = month(men$sampledate), "Day" = day(men$sampledate), "Hour" = hour(men$sampledate), "Minute" = minute(men$sampledate),
                  "Depth.m" = men$depth, "Dissolved.Oxygen.mg.L" = men$o2, 
                  "Notes.Dissolved.Oxygen" = NA, "Source.Dissolved.Oxygen" = "knb-lter-ntl.29.34")

index <- which(is.na(oxy$Dissolved.Oxygen.mg.L))
oxy <- oxy[-index, ]

oxy.long <- oxy

# Round all the deep depths to nearest .5 m ----
index <- which(oxy$Depth.m == 19.2)
oxy[index,]
oxy$Depth.m[index] <- 19

index <- which(oxy$Depth.m == 21.3)
oxy[index,]
oxy$Depth.m[index] <- 21.5

index <- which(oxy$Depth.m == 21.7)
oxy[index,]
oxy$Depth.m[index] <- 21.5

index <- which(oxy$Depth.m == 21.9)
oxy[index,]
oxy$Depth.m[index] <- 22

index <- which(oxy$Depth.m == 22.3)
oxy[index,]
oxy$Depth.m[index] <- 22.5

index <- which(oxy$Depth.m == 22.8)
oxy[index,]
oxy$Depth.m[index] <- 23

index <- which(oxy$Depth.m == 22.9)
oxy[index,]
oxy$Depth.m[index] <- 23

index <- which(oxy$Depth.m == 23.1)
oxy[index,]
oxy$Depth.m[index] <- 23

index <- which(oxy$Depth.m == 23.2)
oxy[index,]
oxy$Depth.m[index] <- 23

index <- which(oxy$Depth.m == 23.3)
oxy[index,]
oxy$Depth.m[index] <- 23.5

index <- which(oxy$Depth.m == 23.6)
oxy[index,]
oxy$Depth.m[index] <- 23.5

index <- which(oxy$Depth.m == 23.7)
oxy[index,]
oxy$Depth.m[index] <- 23.5

index <- which(oxy$Depth.m == 23.8)
oxy[index,]
oxy$Depth.m[index] <- 24

index <- which(oxy$Depth.m == 23.9)
oxy[index,]
oxy$Depth.m[index] <- 24

index <- which(oxy$Depth.m == 24.1 | oxy$Depth.m == 24.2)
oxy[index,]
oxy$Depth.m[index] <- 24

index <- which(oxy$Depth.m == 24.3 | oxy$Depth.m == 24.4 | oxy$Depth.m == 24.6 | oxy$Depth.m == 24.7)
oxy[index,]
oxy$Depth.m[index] <- 24.5

index <- which(oxy$Depth.m == 24.8 | oxy$Depth.m == 24.9)
oxy[index,]
oxy$Depth.m[index] <- 25

index <- which(oxy$Depth.m == 25.1)
oxy[index,]
oxy$Depth.m[index] <- 25

index <- which(oxy$Depth.m == 25.2)
oxy[index,]
oxy$Depth.m[index] <- 25

unique(oxy$Depth.m)

unique(oxy$Hour)
unique(oxy$Minute)

oxy$People <- "NTL-LTER Basecrew"

# the rep values appeared after rounding the values to nearest .5 m, just average them
oxy.wide <- pivot_wider(data = oxy, 
                        id_cols = c("Year","Month","Day","People","Notes.Dissolved.Oxygen","Source.Dissolved.Oxygen"), 
                        names_from = "Depth.m", values_from = "Dissolved.Oxygen.mg.L", values_fn = mean)
head(oxy.wide)
colnames(oxy.wide)
depths <- colnames(oxy.wide)[9:ncol(oxy.wide)]
depths <- as.numeric(depths)
index <- order(depths)
depths[index]
oxy.wide <- oxy.wide[ ,c(1:8,(9:ncol(oxy.wide))[index])]
colnames(oxy.wide)

unique(oxy.long$Depth.m)

# ---- export ----

saveRDS(object = oxy.wide, file = save.data.as)

