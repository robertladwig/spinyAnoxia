# RRR
# 1995 - 2020
# LTER base crew measurements
# https://portal.edirepository.org/nis/mapbrowse?scope=knb-lter-ntl&identifier=31&revision=30

my.formatted.secchi.depths <- "data_input/0d_secchi-1995-2020_knb-lter-ntl.31.30.rds"

# ---- EDI download script ----

# Package ID: knb-lter-ntl.31.30 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: Secchi Disk Depth; Other Auxiliary Base Crew Sample Data 1981 - current.
# Data set creator:  John Magnuson - University of Wisconsin 
# Data set creator:  Stephen Carpenter - University of Wisconsin 
# Data set creator:  Emily Stanley - University of Wisconsin 
# Contact:  NTL Information Manager -  University of Wisconsin  - ntl.infomgr@gmail.com
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/31/30/5a5a5606737d760b61c43bc59460ccc9" 
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
                    "sta",     
                    "secview",     
                    "secnview",     
                    "timeon",     
                    "timeoff",     
                    "airtemp",     
                    "windir",     
                    "windspd",     
                    "waveht",     
                    "cloud",     
                    "ice"    ), check.names=TRUE)
               
unlink(infile1)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                
if (class(dt1$lakeid)!="factor") dt1$lakeid<- as.factor(dt1$lakeid)
if (class(dt1$year4)=="factor") dt1$year4 <-as.numeric(levels(dt1$year4))[as.integer(dt1$year4) ]               
if (class(dt1$year4)=="character") dt1$year4 <-as.numeric(dt1$year4)
if (class(dt1$daynum)=="factor") dt1$daynum <-as.numeric(levels(dt1$daynum))[as.integer(dt1$daynum) ]               
if (class(dt1$daynum)=="character") dt1$daynum <-as.numeric(dt1$daynum)                                   

# I commented out, want to use the other posix format that lubridate uses
# # attempting to convert dt1$sampledate dateTime string to R date structure (date or POSIXct)                                
# tmpDateFormat<-"%Y-%m-%d"
# tmp1sampledate<-as.Date(dt1$sampledate,format=tmpDateFormat)
# # Keep the new dates only if they all converted correctly
# if(length(tmp1sampledate) == length(tmp1sampledate[!is.na(tmp1sampledate)])){dt1$sampledate <- tmp1sampledate } else {print("Date conversion failed for dt1$sampledate. Please inspect the data and do the date conversion yourself.")}                                                                    
# rm(tmpDateFormat,tmp1sampledate) 

if (class(dt1$sta)!="factor") dt1$sta<- as.factor(dt1$sta)
if (class(dt1$secview)=="factor") dt1$secview <-as.numeric(levels(dt1$secview))[as.integer(dt1$secview) ]               
if (class(dt1$secview)=="character") dt1$secview <-as.numeric(dt1$secview)
if (class(dt1$secnview)=="factor") dt1$secnview <-as.numeric(levels(dt1$secnview))[as.integer(dt1$secnview) ]               
if (class(dt1$secnview)=="character") dt1$secnview <-as.numeric(dt1$secnview)
if (class(dt1$airtemp)=="factor") dt1$airtemp <-as.numeric(levels(dt1$airtemp))[as.integer(dt1$airtemp) ]               
if (class(dt1$airtemp)=="character") dt1$airtemp <-as.numeric(dt1$airtemp)
if (class(dt1$windir)!="factor") dt1$windir<- as.factor(dt1$windir)
if (class(dt1$windspd)=="factor") dt1$windspd <-as.numeric(levels(dt1$windspd))[as.integer(dt1$windspd) ]               
if (class(dt1$windspd)=="character") dt1$windspd <-as.numeric(dt1$windspd)
if (class(dt1$waveht)=="factor") dt1$waveht <-as.numeric(levels(dt1$waveht))[as.integer(dt1$waveht) ]               
if (class(dt1$waveht)=="character") dt1$waveht <-as.numeric(dt1$waveht)
if (class(dt1$cloud)=="factor") dt1$cloud <-as.numeric(levels(dt1$cloud))[as.integer(dt1$cloud) ]               
if (class(dt1$cloud)=="character") dt1$cloud <-as.numeric(dt1$cloud)
if (class(dt1$ice)!="factor") dt1$ice<- as.factor(dt1$ice)
                
# Convert Missing Values to NA for non-dates
                


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(lakeid)
summary(year4)
summary(daynum)
summary(sampledate)
summary(sta)
summary(secview)
summary(secnview)
summary(timeon)
summary(timeoff)
summary(airtemp)
summary(windir)
summary(windspd)
summary(waveht)
summary(cloud)
summary(ice) 
                # Get more details on character variables
                 
summary(as.factor(dt1$lakeid)) 
summary(as.factor(dt1$sta)) 
summary(as.factor(dt1$windir)) 
summary(as.factor(dt1$ice))
detach(dt1)               
        
# ---- my processing ----

library(lubridate)


head(dt1)
index <- which(dt1$lakeid == "ME")

men <- dt1[index, ]
head(men)
unique(men$year4)
unique(men$sta) # OK good, there's only 1 station in Mendota so don't need to worry about a key for stn number
unique(men$secview) # This is secchi depth using a plexiglass viewer
unique(men$secnview) # this is regular secchi depth (no viewer)
unique(men$timeon) # time sampling started- units = ???? this looks like cumulative minutes? 8am = 480, 12pm = 720, 5pm = 1020
unique(men$timeoff) # time sampling started- units = ???? NONONO this is just military time missing the colon btwn hrs and minutes!!!
unique(men$airtemp) # look like celcius?
unique(men$windir) # kind of messy b/c inconsistent capitalization
unique(men$windspd) # kts or mph?
unique(men$waveht) # very handy! 
unique(men$cloud) # guess a percentage covered estimate?
unique(men$ice) # a binary there or not?


# ---- secchi ----
cbind(1:ncol(men), colnames(men))
secchi <- men[ ,c(2,4,6,7)]
head(secchi)
index <- which(!is.na(secchi$secview))
secchi[index, ]
# ok, for the 2 time secview exists and secnview doesn't, use the secview measurements
index <- which(!is.na(secchi$secview) & is.na(secchi$secnview))
secchi$secnview[index] <- secchi$secview[index]
secchi <- cbind(secchi, "notes" = "")
secchi$notes[index] <- "this secchi measurement was taken using a plexiglass viewer"
secchi[index, ]
secchi <- secchi[ ,-3]
colnames(secchi) <- c("year","sample.date", "secchi.depth.UNIT", "notes.secchi")
head(secchi)

# ---- parse time of day ----

tod <- men[ ,8:9]
tod <- as.matrix(tod)
tod <- apply(tod, 2, as.character)
nchar(tod)
index <- which(nchar(tod) == 3)
tod[index] <- paste0("0",tod[index]) 
index <- which(nchar(tod) == 2)
tod[index] <- paste0(tod[index], "00") 
index <- which(nchar(tod) == 1)
tod[index] <- paste0("0",tod[index], "00") 
unique(nchar(tod))
tod[ ,1] <- paste0(substr(x = tod[ ,1], start = 1, stop = 2), ":", substr(x = tod[ ,1], start = 3, stop = 4))
tod[ ,2] <- paste0(substr(x = tod[ ,2], start = 1, stop = 2), ":", substr(x = tod[ ,2], start = 3, stop = 4))
index <- grep(pattern = "NA", x = tod, value = F)
tod[index] <- NA
tod <- as.data.frame(tod)
tod[ ,1] <- parse_date_time(x = tod[ ,1], orders = "HM")
tod[ ,2] <- parse_date_time(x = tod[ ,2], orders = "HM")

tod <- cbind(tod, "time" = parse_date_time(x = "01:01", orders = "HM"))
head(tod)
index <- !is.na(tod$timeon) & is.na(tod$timeoff)
tod$time[index] <- tod$timeon[index]
index <- is.na(tod$timeon) & !is.na(tod$timeoff)
tod$time[index] <- tod$timeoff
index <- is.na(tod$timeon) & is.na(tod$timeoff)
tod$time[index] <- NA
index <- !is.na(tod$timeon) & !is.na(tod$timeoff)
tod$time[index] <- tod$timeon[index] + ((tod$timeoff[index] - tod$timeon[index]) / 2)
head(tod)
tod$time <- as.character(tod$time)
tod$time <- sub(pattern = "^0000-01-01 ", replacement = "", x = tod$time)
tod$time <- substr(x = tod$time, start = 1, stop = 5)

men <- cbind(men, "sampling.time.HH.MM" = tod$time)

# ---- save files ----

# need to know secchi units!
my.formatted.secchi.depths
saveRDS(object = secchi, file = my.formatted.secchi.depths)

