# Package ID: knb-lter-ntl.416.1 Cataloging System:https://pasta.edirepository.org.
# Data set title: Lake Mendota Microbial Observatory Secchi Disk Measurements 2012-present.
# Data set creator:  Robin Rohwer - North Temperate Lakes LTER 
# Data set creator:  Katherine McMahon - North Temperate Lakes LTER 
# Contact:  Katherine McMahon -    - trina.mcmahon@wisc.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

my.saveto.file <- "data_input/0e_secchi-2012-2019_knb-lter-ntl.416.1.rds"

# ---- download script ----

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/416/1/6b4502e8093d7ef8eb9a7a647b4ed28b" 
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
                 "Secchi.Depth.m",     
                 "s.Secchi.Depth.m",     
                 "People",     
                 "Notes.Secchi"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$Year)=="factor") dt1$Year <-as.numeric(levels(dt1$Year))[as.integer(dt1$Year) ]               
if (class(dt1$Year)=="character") dt1$Year <-as.numeric(dt1$Year)
if (class(dt1$Month)=="factor") dt1$Month <-as.numeric(levels(dt1$Month))[as.integer(dt1$Month) ]               
if (class(dt1$Month)=="character") dt1$Month <-as.numeric(dt1$Month)
if (class(dt1$Day)=="factor") dt1$Day <-as.numeric(levels(dt1$Day))[as.integer(dt1$Day) ]               
if (class(dt1$Day)=="character") dt1$Day <-as.numeric(dt1$Day)
if (class(dt1$Secchi.Depth.m)=="factor") dt1$Secchi.Depth.m <-as.numeric(levels(dt1$Secchi.Depth.m))[as.integer(dt1$Secchi.Depth.m) ]               
if (class(dt1$Secchi.Depth.m)=="character") dt1$Secchi.Depth.m <-as.numeric(dt1$Secchi.Depth.m)
if (class(dt1$s.Secchi.Depth.m)=="factor") dt1$s.Secchi.Depth.m <-as.numeric(levels(dt1$s.Secchi.Depth.m))[as.integer(dt1$s.Secchi.Depth.m) ]               
if (class(dt1$s.Secchi.Depth.m)=="character") dt1$s.Secchi.Depth.m <-as.numeric(dt1$s.Secchi.Depth.m)
if (class(dt1$People)!="factor") dt1$People<- as.factor(dt1$People)
if (class(dt1$Notes.Secchi)!="factor") dt1$Notes.Secchi<- as.factor(dt1$Notes.Secchi)

# Convert Missing Values to NA for non-dates

dt1$s.Secchi.Depth.m <- ifelse((trimws(as.character(dt1$s.Secchi.Depth.m))==trimws("NA")),NA,dt1$s.Secchi.Depth.m)               
suppressWarnings(dt1$s.Secchi.Depth.m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$s.Secchi.Depth.m))==as.character(as.numeric("NA"))),NA,dt1$s.Secchi.Depth.m))
dt1$Notes.Secchi <- as.factor(ifelse((trimws(as.character(dt1$Notes.Secchi))==trimws("NA")),NA,as.character(dt1$Notes.Secchi)))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(Year)
summary(Month)
summary(Day)
summary(Secchi.Depth.m)
summary(s.Secchi.Depth.m)
summary(People)
summary(Notes.Secchi) 
# Get more details on character variables

summary(as.factor(dt1$People)) 
summary(as.factor(dt1$Notes.Secchi))
detach(dt1)               

# ---- save file ----

mo <- dt1

saveRDS(object = mo, file = my.saveto.file)



