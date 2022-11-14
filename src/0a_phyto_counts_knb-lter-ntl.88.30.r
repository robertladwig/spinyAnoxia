# Package ID: knb-lter-ntl.88.30 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: Phytoplankton - Madison Lakes Area 1995 - current.
# Data set creator:  John Magnuson - University of Wisconsin 
# Data set creator:  Stephen Carpenter - University of Wisconsin 
# Data set creator:  Emily Stanley - University of Wisconsin 
# Contact:  NTL Information Manager -  University of Wisconsin  - ntl.infomgr@gmail.com
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/88/30/f2de15b2fff6ae962a04c150c0a1c510" 
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
                    "sampledate",     
                    "sta",     
                    "depth_range",     
                    "division",     
                    "taxa_name",     
                    "gald",     
                    "cells_per_nu",     
                    "nu_per_ml",     
                    "cells_per_ml",     
                    "biovolume_conc",     
                    "biomass_conc",     
                    "relative_total_biovolume",     
                    "genus"    ), check.names=TRUE)
               
unlink(infile1)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                
if (class(dt1$lakeid)!="factor") dt1$lakeid<- as.factor(dt1$lakeid)
if (class(dt1$year4)=="factor") dt1$year4 <-as.numeric(levels(dt1$year4))[as.integer(dt1$year4) ]               
if (class(dt1$year4)=="character") dt1$year4 <-as.numeric(dt1$year4)                                   
# attempting to convert dt1$sampledate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1sampledate<-as.Date(dt1$sampledate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1sampledate) == length(tmp1sampledate[!is.na(tmp1sampledate)])){dt1$sampledate <- tmp1sampledate } else {print("Date conversion failed for dt1$sampledate. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1sampledate) 
if (class(dt1$sta)!="factor") dt1$sta<- as.factor(dt1$sta)
if (class(dt1$depth_range)!="factor") dt1$depth_range<- as.factor(dt1$depth_range)
if (class(dt1$division)!="factor") dt1$division<- as.factor(dt1$division)
if (class(dt1$taxa_name)!="factor") dt1$taxa_name<- as.factor(dt1$taxa_name)
if (class(dt1$gald)=="factor") dt1$gald <-as.numeric(levels(dt1$gald))[as.integer(dt1$gald) ]               
if (class(dt1$gald)=="character") dt1$gald <-as.numeric(dt1$gald)
if (class(dt1$cells_per_nu)=="factor") dt1$cells_per_nu <-as.numeric(levels(dt1$cells_per_nu))[as.integer(dt1$cells_per_nu) ]               
if (class(dt1$cells_per_nu)=="character") dt1$cells_per_nu <-as.numeric(dt1$cells_per_nu)
if (class(dt1$nu_per_ml)=="factor") dt1$nu_per_ml <-as.numeric(levels(dt1$nu_per_ml))[as.integer(dt1$nu_per_ml) ]               
if (class(dt1$nu_per_ml)=="character") dt1$nu_per_ml <-as.numeric(dt1$nu_per_ml)
if (class(dt1$cells_per_ml)=="factor") dt1$cells_per_ml <-as.numeric(levels(dt1$cells_per_ml))[as.integer(dt1$cells_per_ml) ]               
if (class(dt1$cells_per_ml)=="character") dt1$cells_per_ml <-as.numeric(dt1$cells_per_ml)
if (class(dt1$biovolume_conc)=="factor") dt1$biovolume_conc <-as.numeric(levels(dt1$biovolume_conc))[as.integer(dt1$biovolume_conc) ]               
if (class(dt1$biovolume_conc)=="character") dt1$biovolume_conc <-as.numeric(dt1$biovolume_conc)
if (class(dt1$biomass_conc)=="factor") dt1$biomass_conc <-as.numeric(levels(dt1$biomass_conc))[as.integer(dt1$biomass_conc) ]               
if (class(dt1$biomass_conc)=="character") dt1$biomass_conc <-as.numeric(dt1$biomass_conc)
if (class(dt1$relative_total_biovolume)=="factor") dt1$relative_total_biovolume <-as.numeric(levels(dt1$relative_total_biovolume))[as.integer(dt1$relative_total_biovolume) ]               
if (class(dt1$relative_total_biovolume)=="character") dt1$relative_total_biovolume <-as.numeric(dt1$relative_total_biovolume)
if (class(dt1$genus)!="factor") dt1$genus<- as.factor(dt1$genus)
                
# Convert Missing Values to NA for non-dates
                


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(lakeid)
summary(year4)
summary(sampledate)
summary(sta)
summary(depth_range)
summary(division)
summary(taxa_name)
summary(gald)
summary(cells_per_nu)
summary(nu_per_ml)
summary(cells_per_ml)
summary(biovolume_conc)
summary(biomass_conc)
summary(relative_total_biovolume)
summary(genus) 
                # Get more details on character variables
                 
summary(as.factor(dt1$lakeid)) 
summary(as.factor(dt1$sta)) 
summary(as.factor(dt1$depth_range)) 
summary(as.factor(dt1$division)) 
summary(as.factor(dt1$taxa_name)) 
summary(as.factor(dt1$genus))
detach(dt1)               



# ---- RRR ---- 

library(lubridate)

unique(dt1$lakeid)
men <- dt1[dt1$lakeid == "ME", ]
head(men)

unique(men$sta) # only 1
unique(men$depth_range) # 0-8 and 0-2 m pulls
colnames(men)

men <- men[ ,-c(1,2,4)]

men$Year <- year(men$sampledate)
men$Month <- month(men$sampledate)
men$Day <- day(men$sampledate)
men$Date <- parse_date_time(x = paste(men$Year, men$Month, men$Day), orders = "ymd", tz = "Etc/GMT-5")

colnames(men)
men <- men[ ,-1]
colnames(men)
men <- men[ ,c(12:15,1:11)]
colnames(men)
men <- men[ ,c(1:6,15,7,8:14)]
head(men)

# what are these columns??
# nu = natural unit, (i.e., colonies, filaments, or single cells)
# note: Multiple entries for the same species on the same date may be due to different variants or vegetative states - (e.g., colonial or attached vs. free cell.)
# note: one million cubic Micrometers of biovolume Per mL of water are equal to a biovolume concentration of one mm^3/mL == 1 mg/L (if cell density ~ water)
# A tube sampler is used for the 0-8 m Lake Mendota samples
# gald = avg greatest axial linear dimension, um
# cells_per_nu =  per natural unit 
# biovolume concentration = micrometerCubedPerMilliliter
# biomass conc = milligramsPerLiter
# rel total biovolume = perc

colnames(men)[5] <- "Sample.Depth.m"
colnames(men)[6:8] <- paste0("lter.", colnames(men)[6:8])
colnames(men)[9] <- "Size.length.um"
colnames(men)[10] <- "Size.cells.unit"
colnames(men)[11] <- "Count.units.mL"
colnames(men)[12] <- "Count.cells.mL"
colnames(men)[13] <- "Biovolume.um3.mL"
colnames(men)[14] <- "Biomass.mg.L"
colnames(men)[15] # Still don't understand!!

colnames(men)

# remove trailing white spaces from names:
grep(pattern = "Microcystis", x = men$lter.taxa_name, ignore.case = T, value = T)

men$lter.taxa_name <- sub(pattern = " $", "", men$lter.taxa_name)
men$lter.genus <- sub(pattern = " $", "", men$lter.genus)
men$lter.division <- sub(pattern = " $", "", men$lter.division)

men$lter.taxa_name <- sub(pattern = " $", "", men$lter.taxa_name)
men$lter.genus <- sub(pattern = " $", "", men$lter.genus)
men$lter.division <- sub(pattern = " $", "", men$lter.division)

men$lter.taxa_name <- sub(pattern = " $", "", men$lter.taxa_name)
men$lter.genus <- sub(pattern = " $", "", men$lter.genus)
men$lter.division <- sub(pattern = " $", "", men$lter.division)
# ---- export ----

saveRDS(object = men, file = "data_input/0a_phyto_knb-lter-ntl.88.30.rds")


