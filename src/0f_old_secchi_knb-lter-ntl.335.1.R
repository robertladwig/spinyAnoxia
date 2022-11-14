# RRR
# 1984 - 2007
# These are all lake mendota, all measured by hand (so no buoy)
# https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-ntl.335.1

file.secchi <- "data_input/0f_secchi_1900-1989_knb-lter-ntl.335.2.rds"

# ---- EDI download script ----

# Package ID: knb-lter-ntl.335.2 Cataloging System:https://pasta.edirepository.org.
# Data set title: Lake Mendota water temperature secchi depth snow depth ice thickness and meterological conditions 1894 - 2007.
# Data set creator:  Dale Robertson -  
# Contact:  Dale Robertson -    - dzrobert@usgs.gov
# Contact:  NTL Information Manager -  University of Wisconsin  - infomgr@lter.limnology.wisc.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/335/2/20b8938a700553fb8e3e4485f80a4c66" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "sampledate",     
                 "year4",     
                 "month",     
                 "day",     
                 "reps",     
                 "average",     
                 "observer",     
                 "ObsTime",     
                 "Loc",     
                 "depth",     
                 "watertemp"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

# attempting to convert dt1$sampledate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1sampledate<-as.Date(dt1$sampledate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1sampledate) == length(tmp1sampledate[!is.na(tmp1sampledate)])){dt1$sampledate <- tmp1sampledate } else {print("Date conversion failed for dt1$sampledate. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1sampledate) 
if (class(dt1$year4)=="factor") dt1$year4 <-as.numeric(levels(dt1$year4))[as.integer(dt1$year4) ]               
if (class(dt1$year4)=="character") dt1$year4 <-as.numeric(dt1$year4)
if (class(dt1$month)=="factor") dt1$month <-as.numeric(levels(dt1$month))[as.integer(dt1$month) ]               
if (class(dt1$month)=="character") dt1$month <-as.numeric(dt1$month)
if (class(dt1$day)=="factor") dt1$day <-as.numeric(levels(dt1$day))[as.integer(dt1$day) ]               
if (class(dt1$day)=="character") dt1$day <-as.numeric(dt1$day)
if (class(dt1$reps)=="factor") dt1$reps <-as.numeric(levels(dt1$reps))[as.integer(dt1$reps) ]               
if (class(dt1$reps)=="character") dt1$reps <-as.numeric(dt1$reps)
if (class(dt1$average)=="factor") dt1$average <-as.numeric(levels(dt1$average))[as.integer(dt1$average) ]               
if (class(dt1$average)=="character") dt1$average <-as.numeric(dt1$average)
if (class(dt1$observer)!="factor") dt1$observer<- as.factor(dt1$observer)
if (class(dt1$Loc)!="factor") dt1$Loc<- as.factor(dt1$Loc)
if (class(dt1$depth)!="factor") dt1$depth<- as.factor(dt1$depth)
if (class(dt1$watertemp)=="factor") dt1$watertemp <-as.numeric(levels(dt1$watertemp))[as.integer(dt1$watertemp) ]               
if (class(dt1$watertemp)=="character") dt1$watertemp <-as.numeric(dt1$watertemp)

# Convert Missing Values to NA for non-dates

dt1$reps <- ifelse((trimws(as.character(dt1$reps))==trimws("NA")),NA,dt1$reps)               
suppressWarnings(dt1$reps <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$reps))==as.character(as.numeric("NA"))),NA,dt1$reps))
dt1$average <- ifelse((trimws(as.character(dt1$average))==trimws("NA")),NA,dt1$average)               
suppressWarnings(dt1$average <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$average))==as.character(as.numeric("NA"))),NA,dt1$average))
dt1$observer <- as.factor(ifelse((trimws(as.character(dt1$observer))==trimws("NA")),NA,as.character(dt1$observer)))
dt1$Loc <- as.factor(ifelse((trimws(as.character(dt1$Loc))==trimws("NA")),NA,as.character(dt1$Loc)))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(sampledate)
summary(year4)
summary(month)
summary(day)
summary(reps)
summary(average)
summary(observer)
summary(ObsTime)
summary(Loc)
summary(depth)
summary(watertemp) 
# Get more details on character variables

summary(as.factor(dt1$observer)) 
summary(as.factor(dt1$Loc)) 
summary(as.factor(dt1$depth))
detach(dt1)               


inUrl2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/335/2/52de63e85ae40e89957a24673d93b176" 
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl"))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")


dt2 <-read.csv(infile2,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "sampledate",     
                 "year4",     
                 "month",     
                 "day",     
                 "reps",     
                 "average",     
                 "observer",     
                 "ObsTime",     
                 "Loc",     
                 "Depth",     
                 "AT",     
                 "CD.CV",     
                 "WD",     
                 "WD.1",     
                 "SN",     
                 "ICE",     
                 "SECC"    ), check.names=TRUE)

unlink(infile2)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

# attempting to convert dt2$sampledate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp2sampledate<-as.Date(dt2$sampledate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp2sampledate) == length(tmp2sampledate[!is.na(tmp2sampledate)])){dt2$sampledate <- tmp2sampledate } else {print("Date conversion failed for dt2$sampledate. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp2sampledate) 
if (class(dt2$year4)=="factor") dt2$year4 <-as.numeric(levels(dt2$year4))[as.integer(dt2$year4) ]               
if (class(dt2$year4)=="character") dt2$year4 <-as.numeric(dt2$year4)
if (class(dt2$month)=="factor") dt2$month <-as.numeric(levels(dt2$month))[as.integer(dt2$month) ]               
if (class(dt2$month)=="character") dt2$month <-as.numeric(dt2$month)
if (class(dt2$day)=="factor") dt2$day <-as.numeric(levels(dt2$day))[as.integer(dt2$day) ]               
if (class(dt2$day)=="character") dt2$day <-as.numeric(dt2$day)
if (class(dt2$reps)=="factor") dt2$reps <-as.numeric(levels(dt2$reps))[as.integer(dt2$reps) ]               
if (class(dt2$reps)=="character") dt2$reps <-as.numeric(dt2$reps)
if (class(dt2$average)=="factor") dt2$average <-as.numeric(levels(dt2$average))[as.integer(dt2$average) ]               
if (class(dt2$average)=="character") dt2$average <-as.numeric(dt2$average)
if (class(dt2$observer)!="factor") dt2$observer<- as.factor(dt2$observer)
if (class(dt2$Loc)!="factor") dt2$Loc<- as.factor(dt2$Loc)
if (class(dt2$Depth)=="factor") dt2$Depth <-as.numeric(levels(dt2$Depth))[as.integer(dt2$Depth) ]               
if (class(dt2$Depth)=="character") dt2$Depth <-as.numeric(dt2$Depth)
if (class(dt2$AT)=="factor") dt2$AT <-as.numeric(levels(dt2$AT))[as.integer(dt2$AT) ]               
if (class(dt2$AT)=="character") dt2$AT <-as.numeric(dt2$AT)
if (class(dt2$CD.CV)!="factor") dt2$CD.CV<- as.factor(dt2$CD.CV)
if (class(dt2$WD)=="factor") dt2$WD <-as.numeric(levels(dt2$WD))[as.integer(dt2$WD) ]               
if (class(dt2$WD)=="character") dt2$WD <-as.numeric(dt2$WD)
if (class(dt2$WD.1)!="factor") dt2$WD.1<- as.factor(dt2$WD.1)
if (class(dt2$SN)=="factor") dt2$SN <-as.numeric(levels(dt2$SN))[as.integer(dt2$SN) ]               
if (class(dt2$SN)=="character") dt2$SN <-as.numeric(dt2$SN)
if (class(dt2$ICE)=="factor") dt2$ICE <-as.numeric(levels(dt2$ICE))[as.integer(dt2$ICE) ]               
if (class(dt2$ICE)=="character") dt2$ICE <-as.numeric(dt2$ICE)
if (class(dt2$SECC)=="factor") dt2$SECC <-as.numeric(levels(dt2$SECC))[as.integer(dt2$SECC) ]               
if (class(dt2$SECC)=="character") dt2$SECC <-as.numeric(dt2$SECC)

# Convert Missing Values to NA for non-dates

dt2$reps <- ifelse((trimws(as.character(dt2$reps))==trimws("NA")),NA,dt2$reps)               
suppressWarnings(dt2$reps <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$reps))==as.character(as.numeric("NA"))),NA,dt2$reps))
dt2$average <- ifelse((trimws(as.character(dt2$average))==trimws("NA")),NA,dt2$average)               
suppressWarnings(dt2$average <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$average))==as.character(as.numeric("NA"))),NA,dt2$average))
dt2$observer <- as.factor(ifelse((trimws(as.character(dt2$observer))==trimws("NA")),NA,as.character(dt2$observer)))
dt2$Loc <- as.factor(ifelse((trimws(as.character(dt2$Loc))==trimws("NA")),NA,as.character(dt2$Loc)))
dt2$Depth <- ifelse((trimws(as.character(dt2$Depth))==trimws("NA")),NA,dt2$Depth)               
suppressWarnings(dt2$Depth <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Depth))==as.character(as.numeric("NA"))),NA,dt2$Depth))
dt2$AT <- ifelse((trimws(as.character(dt2$AT))==trimws("NA")),NA,dt2$AT)               
suppressWarnings(dt2$AT <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$AT))==as.character(as.numeric("NA"))),NA,dt2$AT))
dt2$WD <- ifelse((trimws(as.character(dt2$WD))==trimws("NA")),NA,dt2$WD)               
suppressWarnings(dt2$WD <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$WD))==as.character(as.numeric("NA"))),NA,dt2$WD))
dt2$SN <- ifelse((trimws(as.character(dt2$SN))==trimws("NA")),NA,dt2$SN)               
suppressWarnings(dt2$SN <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$SN))==as.character(as.numeric("NA"))),NA,dt2$SN))
dt2$ICE <- ifelse((trimws(as.character(dt2$ICE))==trimws("NA")),NA,dt2$ICE)               
suppressWarnings(dt2$ICE <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$ICE))==as.character(as.numeric("NA"))),NA,dt2$ICE))
dt2$SECC <- ifelse((trimws(as.character(dt2$SECC))==trimws("NA")),NA,dt2$SECC)               
suppressWarnings(dt2$SECC <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$SECC))==as.character(as.numeric("NA"))),NA,dt2$SECC))


# Here is the structure of the input data frame:
str(dt2)                            
attach(dt2)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(sampledate)
summary(year4)
summary(month)
summary(day)
summary(reps)
summary(average)
summary(observer)
summary(ObsTime)
summary(Loc)
summary(Depth)
summary(AT)
summary(CD.CV)
summary(WD)
summary(WD.1)
summary(SN)
summary(ICE)
summary(SECC) 
# Get more details on character variables

summary(as.factor(dt2$observer)) 
summary(as.factor(dt2$Loc)) 
summary(as.factor(dt2$CD.CV)) 
summary(as.factor(dt2$WD.1))
detach(dt2)                      
        
# ---- my processing ----

# LOC is location in Mendota, 1 = off picnic point, 2 = deep hole
# wind speed in mph, snow depth is cm, cloud cover is percent

library("tidyr")
library(lubridate)


# Other Old Env. Data ---- 

str(dt2)

data2 <- dt2
data2 <- data2[ ,c(1,8,7,9,10:17)]
colnames(data2)[4] <- "Mendota.Location"
colnames(data2)[6] <- "Air.Temp.C"
colnames(data2)[7] <- "Cloud.Cover.perc"
colnames(data2)[8] <- "Wind.Speed.mph"
colnames(data2)[9] <- "Wind.Direction"
colnames(data2)[10] <- "Snow.Depth.cm"
colnames(data2)[11] <- "Ice.Thickness.cm"
colnames(data2)[12] <- "Secchi.Depth.m"
data2$observer <- as.character(data2$observer)
data2$Mendota.Location <- as.character(data2$Mendota.Location)
data2$Wind.Direction <- as.character(data2$Wind.Direction)
data2$Cloud.Cover.perc <- as.character(data2$Cloud.Cover.perc)
str(data2)

data2$observer[data2$observer == "AL"] <- "A. Lathbury"
data2$observer[data2$observer == "BJ"] <- "E.A. Birge C. Juday and Associates"
data2$observer[data2$observer == "BV"] <- "B. Vigon"
data2$observer[data2$observer == "CM"] <- "A. Colmer and E. McCoy"
data2$observer[data2$observer == "DE"] <- "J. Delfino"
data2$observer[data2$observer == "EL"] <- "E.F. Lee and Associates"
data2$observer[data2$observer == "JC"] <- "J. Coleman"
data2$observer[data2$observer == "JD"] <- "J.A. Dutton"
data2$observer[data2$observer == "JH"] <- "J. Hawley"
data2$observer[data2$observer == "JS"] <- "J.T. Scott"
data2$observer[data2$observer == "KJ"] <- "K. John"
data2$observer[data2$observer == "KM"] <- "K. Mackenthun"
data2$observer[data2$observer == "KS"] <- "K. Stewart"
data2$observer[data2$observer == "LN"] <- "Lines"
data2$observer[data2$observer == "MT"] <- "M.L. Torrey"
data2$observer[data2$observer == "MZ"] <- "MacZ."
data2$observer[data2$observer == "RB"] <- "R. Bryson and Associates"
data2$observer[data2$observer == "RH"] <- "R. Horrall"
data2$observer[data2$observer == "RL"] <- "R. Loeffler"
data2$observer[data2$observer == "RR"] <- "R.A. Ragotzkie"
data2$observer[data2$observer == "RS"] <- "R. Stauffer"
data2$observer[data2$observer == "ST"] <- "R. Stone"
data2$observer[data2$observer == "TB"] <- "T. Brock"
data2$observer[data2$observer == "UW"] <- "University of Wisconsin Limnology Classes"
data2$observer[data2$observer == "WD"] <- "WDNR - Mostly under Richard Lathrop"
data2$observer[data2$observer == "WS"] <- "W. Sonzogni"

unique(data2$observer)
unique(data2$ObsTime)

stupid <- data2$ObsTime
stupid <- as.character(stupid)
index.3 <- nchar(stupid) == 3 & !is.na(stupid)
index.4 <- nchar(stupid) == 4 & !is.na(stupid)

my.hrs <- rep(NA, length(stupid))
my.hrs[index.3] <- substr(x = stupid[index.3], start = 1, stop = 1)
my.hrs[index.4] <- substr(x = stupid[index.4], start = 1, stop = 2)

my.min <- rep(NA, length(stupid))
my.min[index.3] <- substr(x = stupid[index.3], start = 2, stop = 3)
my.min[index.4] <- substr(x = stupid[index.4], start = 3, stop = 4)

all.equal(paste0(stupid), sub("NANA","NA",paste0(my.hrs,my.min))) # good

head(data2)

data2$Year <- year(data2$sampledate)
data2$Month <- month(data2$sampledate)
data2$Day <- day(data2$sampledate)
data2$Hour <- my.hrs
data2$Minute <- my.min

unique(data2$Depth) # I'm not sure what this is referring to.... the lake depth? b/c these are all surface samples... online says "Water Depth of measurement"

secchi <- data.frame("Year" = data2$Year, "Month" = data2$Month, "Day" = data2$Day, 
                     "Secchi.Depth.m" = data2$Secchi.Depth.m, "s.Secchi.Depth.m" = NA, 
                     "People" = data2$observer, "Notes.Secchi" = data2$Mendota.Location,
                     "Source.Secchi" = "NTL LTER dataset of 1894 to 2007, dt2 in EDI package ID knb-lter-ntl.335.1")

secchi <- secchi[!is.na(secchi$Secchi.Depth.m), ]
unique(secchi$Year) # 1900-1989


# ---- Export ----

saveRDS(object = secchi, file = file.secchi)
