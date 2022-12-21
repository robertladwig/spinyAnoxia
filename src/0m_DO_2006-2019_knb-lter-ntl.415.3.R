# https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-ntl.415.3
# doi:10.6073/pasta/9aededa7f406e2c204dc3f8249f53b9d

save.data.as <- "data_input/0m_DO_2006-2019_knb-lter-ntl.415.3.rds"

# ---- EDI download script for R ----

# Package ID: knb-lter-ntl.415.3 Cataloging System:https://pasta.edirepository.org.
# Data set title: Lake Mendota Microbial Observatory Temperature, Dissolved Oxygen, pH, and conductivity       data, 2006-present..
# Data set creator:  Robin Rohwer - North Temperate Lakes LTER 
# Data set creator:  Katherine McMahon - North Temperate Lakes LTER 
# Contact:  Katherine McMahon -  North Temperate Lakes LTER  - trina.mcmahon@wisc.edu
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/415/3/d748c9a0f9ca5c25fd631f24073fba5c" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "Year",     
                 "Month",     
                 "Day",     
                 "Hour",     
                 "Minute",     
                 "Second",     
                 "Depth.m",     
                 "Temperature.C",     
                 "Dissolved.Oxygen.mg.L",     
                 "pH",     
                 "Specific.Conductance.uS.cm",     
                 "Barometer.kPA",     
                 "Unit.ID"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$Year)=="factor") dt1$Year <-as.numeric(levels(dt1$Year))[as.integer(dt1$Year) ]               
if (class(dt1$Year)=="character") dt1$Year <-as.numeric(dt1$Year)
if (class(dt1$Month)=="factor") dt1$Month <-as.numeric(levels(dt1$Month))[as.integer(dt1$Month) ]               
if (class(dt1$Month)=="character") dt1$Month <-as.numeric(dt1$Month)
if (class(dt1$Day)=="factor") dt1$Day <-as.numeric(levels(dt1$Day))[as.integer(dt1$Day) ]               
if (class(dt1$Day)=="character") dt1$Day <-as.numeric(dt1$Day)
if (class(dt1$Hour)=="factor") dt1$Hour <-as.numeric(levels(dt1$Hour))[as.integer(dt1$Hour) ]               
if (class(dt1$Hour)=="character") dt1$Hour <-as.numeric(dt1$Hour)
if (class(dt1$Minute)=="factor") dt1$Minute <-as.numeric(levels(dt1$Minute))[as.integer(dt1$Minute) ]               
if (class(dt1$Minute)=="character") dt1$Minute <-as.numeric(dt1$Minute)
if (class(dt1$Second)=="factor") dt1$Second <-as.numeric(levels(dt1$Second))[as.integer(dt1$Second) ]               
if (class(dt1$Second)=="character") dt1$Second <-as.numeric(dt1$Second)
if (class(dt1$Depth.m)=="factor") dt1$Depth.m <-as.numeric(levels(dt1$Depth.m))[as.integer(dt1$Depth.m) ]               
if (class(dt1$Depth.m)=="character") dt1$Depth.m <-as.numeric(dt1$Depth.m)
if (class(dt1$Temperature.C)=="factor") dt1$Temperature.C <-as.numeric(levels(dt1$Temperature.C))[as.integer(dt1$Temperature.C) ]               
if (class(dt1$Temperature.C)=="character") dt1$Temperature.C <-as.numeric(dt1$Temperature.C)
if (class(dt1$Dissolved.Oxygen.mg.L)=="factor") dt1$Dissolved.Oxygen.mg.L <-as.numeric(levels(dt1$Dissolved.Oxygen.mg.L))[as.integer(dt1$Dissolved.Oxygen.mg.L) ]               
if (class(dt1$Dissolved.Oxygen.mg.L)=="character") dt1$Dissolved.Oxygen.mg.L <-as.numeric(dt1$Dissolved.Oxygen.mg.L)
if (class(dt1$pH)=="factor") dt1$pH <-as.numeric(levels(dt1$pH))[as.integer(dt1$pH) ]               
if (class(dt1$pH)=="character") dt1$pH <-as.numeric(dt1$pH)
if (class(dt1$Specific.Conductance.uS.cm)=="factor") dt1$Specific.Conductance.uS.cm <-as.numeric(levels(dt1$Specific.Conductance.uS.cm))[as.integer(dt1$Specific.Conductance.uS.cm) ]               
if (class(dt1$Specific.Conductance.uS.cm)=="character") dt1$Specific.Conductance.uS.cm <-as.numeric(dt1$Specific.Conductance.uS.cm)
if (class(dt1$Barometer.kPA)=="factor") dt1$Barometer.kPA <-as.numeric(levels(dt1$Barometer.kPA))[as.integer(dt1$Barometer.kPA) ]               
if (class(dt1$Barometer.kPA)=="character") dt1$Barometer.kPA <-as.numeric(dt1$Barometer.kPA)
if (class(dt1$Unit.ID)!="factor") dt1$Unit.ID<- as.factor(dt1$Unit.ID)

# Convert Missing Values to NA for non-dates

dt1$Dissolved.Oxygen.mg.L <- ifelse((trimws(as.character(dt1$Dissolved.Oxygen.mg.L))==trimws("-99999")),NA,dt1$Dissolved.Oxygen.mg.L)               
suppressWarnings(dt1$Dissolved.Oxygen.mg.L <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$Dissolved.Oxygen.mg.L))==as.character(as.numeric("-99999"))),NA,dt1$Dissolved.Oxygen.mg.L))
dt1$pH <- ifelse((trimws(as.character(dt1$pH))==trimws("-99999")),NA,dt1$pH)               
suppressWarnings(dt1$pH <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$pH))==as.character(as.numeric("-99999"))),NA,dt1$pH))
dt1$Specific.Conductance.uS.cm <- ifelse((trimws(as.character(dt1$Specific.Conductance.uS.cm))==trimws("NA")),NA,dt1$Specific.Conductance.uS.cm)               
suppressWarnings(dt1$Specific.Conductance.uS.cm <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Specific.Conductance.uS.cm))==as.character(as.numeric("NA"))),NA,dt1$Specific.Conductance.uS.cm))
dt1$Barometer.kPA <- ifelse((trimws(as.character(dt1$Barometer.kPA))==trimws("NA")),NA,dt1$Barometer.kPA)               
suppressWarnings(dt1$Barometer.kPA <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Barometer.kPA))==as.character(as.numeric("NA"))),NA,dt1$Barometer.kPA))
dt1$Unit.ID <- as.factor(ifelse((trimws(as.character(dt1$Unit.ID))==trimws("NA")),NA,as.character(dt1$Unit.ID)))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(Year)
summary(Month)
summary(Day)
summary(Hour)
summary(Minute)
summary(Second)
summary(Depth.m)
summary(Temperature.C)
summary(Dissolved.Oxygen.mg.L)
summary(pH)
summary(Specific.Conductance.uS.cm)
summary(Barometer.kPA)
summary(Unit.ID) 
# Get more details on character variables

summary(as.factor(dt1$Unit.ID))
detach(dt1)               


inUrl2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/415/3/22da50a9bf813999c3e2987af86ca8e7" 
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl"))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")


dt2 <-read.csv(infile2,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "Year",     
                 "Month",     
                 "Day",     
                 "Hour",     
                 "Minute",     
                 "Depth.m",     
                 "Water.Temp.C",     
                 "People",     
                 "Instrument"    ), check.names=TRUE)

unlink(infile2)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt2$Year)=="factor") dt2$Year <-as.numeric(levels(dt2$Year))[as.integer(dt2$Year) ]               
if (class(dt2$Year)=="character") dt2$Year <-as.numeric(dt2$Year)
if (class(dt2$Month)=="factor") dt2$Month <-as.numeric(levels(dt2$Month))[as.integer(dt2$Month) ]               
if (class(dt2$Month)=="character") dt2$Month <-as.numeric(dt2$Month)
if (class(dt2$Day)=="factor") dt2$Day <-as.numeric(levels(dt2$Day))[as.integer(dt2$Day) ]               
if (class(dt2$Day)=="character") dt2$Day <-as.numeric(dt2$Day)
if (class(dt2$Hour)=="factor") dt2$Hour <-as.numeric(levels(dt2$Hour))[as.integer(dt2$Hour) ]               
if (class(dt2$Hour)=="character") dt2$Hour <-as.numeric(dt2$Hour)
if (class(dt2$Minute)=="factor") dt2$Minute <-as.numeric(levels(dt2$Minute))[as.integer(dt2$Minute) ]               
if (class(dt2$Minute)=="character") dt2$Minute <-as.numeric(dt2$Minute)
if (class(dt2$Depth.m)=="factor") dt2$Depth.m <-as.numeric(levels(dt2$Depth.m))[as.integer(dt2$Depth.m) ]               
if (class(dt2$Depth.m)=="character") dt2$Depth.m <-as.numeric(dt2$Depth.m)
if (class(dt2$Water.Temp.C)=="factor") dt2$Water.Temp.C <-as.numeric(levels(dt2$Water.Temp.C))[as.integer(dt2$Water.Temp.C) ]               
if (class(dt2$Water.Temp.C)=="character") dt2$Water.Temp.C <-as.numeric(dt2$Water.Temp.C)
if (class(dt2$People)!="factor") dt2$People<- as.factor(dt2$People)
if (class(dt2$Instrument)!="factor") dt2$Instrument<- as.factor(dt2$Instrument)

# Convert Missing Values to NA for non-dates

dt2$Hour <- ifelse((trimws(as.character(dt2$Hour))==trimws("NA")),NA,dt2$Hour)               
suppressWarnings(dt2$Hour <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Hour))==as.character(as.numeric("NA"))),NA,dt2$Hour))
dt2$Minute <- ifelse((trimws(as.character(dt2$Minute))==trimws("NA")),NA,dt2$Minute)               
suppressWarnings(dt2$Minute <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Minute))==as.character(as.numeric("NA"))),NA,dt2$Minute))
dt2$Water.Temp.C <- ifelse((trimws(as.character(dt2$Water.Temp.C))==trimws("NA")),NA,dt2$Water.Temp.C)               
suppressWarnings(dt2$Water.Temp.C <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Water.Temp.C))==as.character(as.numeric("NA"))),NA,dt2$Water.Temp.C))
dt2$People <- as.factor(ifelse((trimws(as.character(dt2$People))==trimws("NA")),NA,as.character(dt2$People)))
dt2$Instrument <- as.factor(ifelse((trimws(as.character(dt2$Instrument))==trimws("nan")),NA,as.character(dt2$Instrument)))


# Here is the structure of the input data frame:
str(dt2)                            
attach(dt2)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(Year)
summary(Month)
summary(Day)
summary(Hour)
summary(Minute)
summary(Depth.m)
summary(Water.Temp.C)
summary(People)
summary(Instrument) 
# Get more details on character variables

summary(as.factor(dt2$People)) 
summary(as.factor(dt2$Instrument))
detach(dt2)               


inUrl3  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/415/3/4c0fa94c12d7000aa8f7224882bd095e" 
infile3 <- tempfile()
try(download.file(inUrl3,infile3,method="curl"))
if (is.na(file.size(infile3))) download.file(inUrl3,infile3,method="auto")


dt3 <-read.csv(infile3,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "Year",     
                 "Month",     
                 "Day",     
                 "Hour",     
                 "Minute",     
                 "Depth.m",     
                 "Dissolved.Oxygen.mg.L",     
                 "People",     
                 "Instrument",     
                 "Notes.Dissolved.Oxygen"    ), check.names=TRUE)

unlink(infile3)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt3$Year)=="factor") dt3$Year <-as.numeric(levels(dt3$Year))[as.integer(dt3$Year) ]               
if (class(dt3$Year)=="character") dt3$Year <-as.numeric(dt3$Year)
if (class(dt3$Month)=="factor") dt3$Month <-as.numeric(levels(dt3$Month))[as.integer(dt3$Month) ]               
if (class(dt3$Month)=="character") dt3$Month <-as.numeric(dt3$Month)
if (class(dt3$Day)=="factor") dt3$Day <-as.numeric(levels(dt3$Day))[as.integer(dt3$Day) ]               
if (class(dt3$Day)=="character") dt3$Day <-as.numeric(dt3$Day)
if (class(dt3$Hour)=="factor") dt3$Hour <-as.numeric(levels(dt3$Hour))[as.integer(dt3$Hour) ]               
if (class(dt3$Hour)=="character") dt3$Hour <-as.numeric(dt3$Hour)
if (class(dt3$Minute)=="factor") dt3$Minute <-as.numeric(levels(dt3$Minute))[as.integer(dt3$Minute) ]               
if (class(dt3$Minute)=="character") dt3$Minute <-as.numeric(dt3$Minute)
if (class(dt3$Depth.m)=="factor") dt3$Depth.m <-as.numeric(levels(dt3$Depth.m))[as.integer(dt3$Depth.m) ]               
if (class(dt3$Depth.m)=="character") dt3$Depth.m <-as.numeric(dt3$Depth.m)
if (class(dt3$Dissolved.Oxygen.mg.L)=="factor") dt3$Dissolved.Oxygen.mg.L <-as.numeric(levels(dt3$Dissolved.Oxygen.mg.L))[as.integer(dt3$Dissolved.Oxygen.mg.L) ]               
if (class(dt3$Dissolved.Oxygen.mg.L)=="character") dt3$Dissolved.Oxygen.mg.L <-as.numeric(dt3$Dissolved.Oxygen.mg.L)
if (class(dt3$People)!="factor") dt3$People<- as.factor(dt3$People)
if (class(dt3$Instrument)!="factor") dt3$Instrument<- as.factor(dt3$Instrument)
if (class(dt3$Notes.Dissolved.Oxygen)!="factor") dt3$Notes.Dissolved.Oxygen<- as.factor(dt3$Notes.Dissolved.Oxygen)

# Convert Missing Values to NA for non-dates

dt3$Dissolved.Oxygen.mg.L <- ifelse((trimws(as.character(dt3$Dissolved.Oxygen.mg.L))==trimws("NA")),NA,dt3$Dissolved.Oxygen.mg.L)               
suppressWarnings(dt3$Dissolved.Oxygen.mg.L <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$Dissolved.Oxygen.mg.L))==as.character(as.numeric("NA"))),NA,dt3$Dissolved.Oxygen.mg.L))
dt3$Instrument <- as.factor(ifelse((trimws(as.character(dt3$Instrument))==trimws("nan")),NA,as.character(dt3$Instrument)))
dt3$Notes.Dissolved.Oxygen <- as.factor(ifelse((trimws(as.character(dt3$Notes.Dissolved.Oxygen))==trimws("NA")),NA,as.character(dt3$Notes.Dissolved.Oxygen)))


# Here is the structure of the input data frame:
str(dt3)                            
attach(dt3)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(Year)
summary(Month)
summary(Day)
summary(Hour)
summary(Minute)
summary(Depth.m)
summary(Dissolved.Oxygen.mg.L)
summary(People)
summary(Instrument)
summary(Notes.Dissolved.Oxygen) 
# Get more details on character variables

summary(as.factor(dt3$People)) 
summary(as.factor(dt3$Instrument)) 
summary(as.factor(dt3$Notes.Dissolved.Oxygen))
detach(dt3)               

# ---- RRR pull out DO data ----

library(tidyr)

new.do <- dt1
old.do <- dt3

cbind(1:ncol(new.do), colnames(new.do))

new.do <- new.do[ ,-c(6,8,10,11,12)]

new.do <- data.frame(new.do[ ,1:7], "People" = "Rohwer Microbial Observatory team", "Notes.Dissolved.Oxygen" = as.character(new.do$Unit.ID), "Source.Dissolved.Oxygen" = "knb-lter-ntl.415.3")

cbind(1:ncol(old.do), colnames(old.do))

old.do$Notes.Dissolved.Oxygen <- paste(as.character(old.do$Instrument), as.character(old.do$Notes.Dissolved.Oxygen), sep = "; ")

old.do$Notes.Dissolved.Oxygen <- sub(x = old.do$Notes.Dissolved.Oxygen, pattern = "; NA", replacement = "")
old.do$Notes.Dissolved.Oxygen <- sub(x = old.do$Notes.Dissolved.Oxygen, pattern = "NA", replacement = "")
old.do$Notes.Dissolved.Oxygen[old.do$Notes.Dissolved.Oxygen == ""] <- NA
unique(old.do$Notes.Dissolved.Oxygen)
old.do <- old.do[ ,-9]

old.do$Source.Dissolved.Oxygen = "knb-lter-ntl.415.3"

index <- which(old.do$Depth.m == .1)
old.do$Depth.m[index] <- 0

all.do <- rbind(old.do, new.do)

wide.do <- pivot_wider(data = all.do, 
                       id_cols = c("Year","Month","Day","Hour","Minute","People","Notes.Dissolved.Oxygen","Source.Dissolved.Oxygen"), 
                       names_from = "Depth.m", values_from = "Dissolved.Oxygen.mg.L")

depths <- colnames(wide.do)[9:ncol(wide.do)]
depths <- as.numeric(depths)
index <- order(depths)
wide.do <- wide.do[ ,c(1:8,(9:ncol(wide.do))[index])]
colnames(wide.do)[9:ncol(wide.do)] <- paste0(colnames(wide.do)[9:ncol(wide.do)],"m")
colnames(wide.do)

# ---- export data ----

saveRDS(object = wide.do, file = save.data.as)
