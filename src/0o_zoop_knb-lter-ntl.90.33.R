# Package ID: knb-lter-ntl.90.33 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: Zooplankton - Madison Lakes Area 1997 - current.
# Data set creator:  John Magnuson - University of Wisconsin 
# Data set creator:  Stephen Carpenter - University of Wisconsin 
# Data set creator:  Emily Stanley - University of Wisconsin 
# Metadata Provider:  NTL Information Manager - University of Wisconsin 
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/90/33/5880c7ba184589e239aec9c55f9d313b" 
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
                 "sample_date",     
                 "station",     
                 "towdepth",     
                 "species_code",     
                 "species_name",     
                 "density",     
                 "individuals_measured",     
                 "avg_length"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$lakeid)!="factor") dt1$lakeid<- as.factor(dt1$lakeid)
if (class(dt1$year4)=="factor") dt1$year4 <-as.numeric(levels(dt1$year4))[as.integer(dt1$year4) ]               
if (class(dt1$year4)=="character") dt1$year4 <-as.numeric(dt1$year4)                                   
# attempting to convert dt1$sample_date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1sample_date<-as.Date(dt1$sample_date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1sample_date) == length(tmp1sample_date[!is.na(tmp1sample_date)])){dt1$sample_date <- tmp1sample_date } else {print("Date conversion failed for dt1$sample_date. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1sample_date) 
if (class(dt1$station)!="factor") dt1$station<- as.factor(dt1$station)
if (class(dt1$towdepth)=="factor") dt1$towdepth <-as.numeric(levels(dt1$towdepth))[as.integer(dt1$towdepth) ]               
if (class(dt1$towdepth)=="character") dt1$towdepth <-as.numeric(dt1$towdepth)
if (class(dt1$species_code)!="factor") dt1$species_code<- as.factor(dt1$species_code)
if (class(dt1$species_name)!="factor") dt1$species_name<- as.factor(dt1$species_name)
if (class(dt1$density)=="factor") dt1$density <-as.numeric(levels(dt1$density))[as.integer(dt1$density) ]               
if (class(dt1$density)=="character") dt1$density <-as.numeric(dt1$density)
if (class(dt1$individuals_measured)=="factor") dt1$individuals_measured <-as.numeric(levels(dt1$individuals_measured))[as.integer(dt1$individuals_measured) ]               
if (class(dt1$individuals_measured)=="character") dt1$individuals_measured <-as.numeric(dt1$individuals_measured)
if (class(dt1$avg_length)=="factor") dt1$avg_length <-as.numeric(levels(dt1$avg_length))[as.integer(dt1$avg_length) ]               
if (class(dt1$avg_length)=="character") dt1$avg_length <-as.numeric(dt1$avg_length)

# Convert Missing Values to NA for non-dates



# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(lakeid)
summary(year4)
summary(sample_date)
summary(station)
summary(towdepth)
summary(species_code)
summary(species_name)
summary(density)
summary(individuals_measured)
summary(avg_length) 
# Get more details on character variables

summary(as.factor(dt1$lakeid)) 
summary(as.factor(dt1$station)) 
summary(as.factor(dt1$species_code)) 
summary(as.factor(dt1$species_name))
detach(dt1)               

# ---- RRR processing starts ----

zoop <- dt1

library(data.table)
library(lubridate)
library(stringr)

zoop <- as.data.table(zoop)

zoop <- zoop[lakeid == "ME"]

zoop[ ,sample_date := parse_date_time(x = sample_date, orders = "ymd")]

zoop[ ,species_name := as.character(species_name)]
# there is a weird blank space character in the Bytho name. Also, I hate having them in all caps.

index <- grep(pattern = "BYTHOTREPHES LONGIMANUS", x = zoop$species_name)
zoop$species_name[index] <- "BYTHOTREPHES LONGIMANUS"
zoop[ ,species_name := str_to_title(species_name)]

zoop$yday <- yday(zoop$sample_date)

unique(zoop$station)

zoop <- zoop[ ,.(sample_date, year4, yday, towdepth, species_code, species_name, density, individuals_measured, avg_length)]

colnames(zoop)[colnames(zoop) == "density"] <- "density.num.m2" # number / m2 NOT per m3!!
colnames(zoop)[colnames(zoop) == "avg_length"] <- "avg_length.mm" # in millimeters 

# from Jake on the m2 thing:
# You need to divide the density by the tow depth to get number per cubic meter for the 1997-current southern lakes data 
# (where density is in number per meter squared), and multiply by 1000 to get number per cubic meter for the 1982-current 
# northern lakes data (where density is in number per liter). The northern lakes are sampled using long Schindler-Patalas 
# traps, so are just whatever was in the volume of the trap. The southern lakes are sampled with vertical net hauls, the 
# areal density estimate is the surface area of the net. That way you can adjust the depth number to get volumetric density 
# either for the whole tow, or (to match Dick Lathrop's approach) the portion of the two that went through oxygenated water 
# (which assumes you only captured zoops in oxygenated water).

unique(zoop$towdepth)

zoop$density.num.m3 <- zoop$density.num.m2 * zoop$towdepth

saveRDS(object = zoop, file = "data_input/0o_zoop_knb-lter-ntl.90.33.rds")
