# https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-ntl.400.3
# doi:10.6073/pasta/e99f419b3c7c5e546c379734dde12a8d

save.data.as <- "data_input/0l_DO_2017-2020_knb-lter-ntl.400.3.rds"

# ---- EDI download script for R ----

# Package ID: knb-lter-ntl.400.3 Cataloging System:https://pasta.edirepository.org.
# Data set title: Lake Mendota Multiparameter Sonde Profiles: 2017 - current.
# Data set creator:  John Magnuson - University of Wisconsin 
# Data set creator:  Stephen Carpenter - University of Wisconsin 
# Data set creator:  Emily Stanley - University of Wisconsin 
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/400/3/db0ea4973347e79d9d6599ffc7111ec0" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "lakeid",     
                 "sampledate",     
                 "sampletime",     
                 "depth",     
                 "wtemp",     
                 "do_raw",     
                 "do_sat",     
                 "ph",     
                 "spec_cond",     
                 "chlor_rfu",     
                 "phyco_rfu",     
                 "turbidity",     
                 "fdom",     
                 "flag_wtemp",     
                 "flag_do_raw",     
                 "flag_do_sat",     
                 "flag_ph",     
                 "flag_spec_cond",     
                 "flag_chlor_rfu",     
                 "flag_phyco_rfu",     
                 "flag_turbidity",     
                 "flag_fdom"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$lakeid)!="factor") dt1$lakeid<- as.factor(dt1$lakeid)                                   
# attempting to convert dt1$sampledate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1sampledate<-as.Date(dt1$sampledate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1sampledate) == length(tmp1sampledate[!is.na(tmp1sampledate)])){dt1$sampledate <- tmp1sampledate } else {print("Date conversion failed for dt1$sampledate. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1sampledate) 
if (class(dt1$depth)=="factor") dt1$depth <-as.numeric(levels(dt1$depth))[as.integer(dt1$depth) ]               
if (class(dt1$depth)=="character") dt1$depth <-as.numeric(dt1$depth)
if (class(dt1$wtemp)=="factor") dt1$wtemp <-as.numeric(levels(dt1$wtemp))[as.integer(dt1$wtemp) ]               
if (class(dt1$wtemp)=="character") dt1$wtemp <-as.numeric(dt1$wtemp)
if (class(dt1$do_raw)=="factor") dt1$do_raw <-as.numeric(levels(dt1$do_raw))[as.integer(dt1$do_raw) ]               
if (class(dt1$do_raw)=="character") dt1$do_raw <-as.numeric(dt1$do_raw)
if (class(dt1$do_sat)=="factor") dt1$do_sat <-as.numeric(levels(dt1$do_sat))[as.integer(dt1$do_sat) ]               
if (class(dt1$do_sat)=="character") dt1$do_sat <-as.numeric(dt1$do_sat)
if (class(dt1$ph)=="factor") dt1$ph <-as.numeric(levels(dt1$ph))[as.integer(dt1$ph) ]               
if (class(dt1$ph)=="character") dt1$ph <-as.numeric(dt1$ph)
if (class(dt1$spec_cond)=="factor") dt1$spec_cond <-as.numeric(levels(dt1$spec_cond))[as.integer(dt1$spec_cond) ]               
if (class(dt1$spec_cond)=="character") dt1$spec_cond <-as.numeric(dt1$spec_cond)
if (class(dt1$chlor_rfu)=="factor") dt1$chlor_rfu <-as.numeric(levels(dt1$chlor_rfu))[as.integer(dt1$chlor_rfu) ]               
if (class(dt1$chlor_rfu)=="character") dt1$chlor_rfu <-as.numeric(dt1$chlor_rfu)
if (class(dt1$phyco_rfu)=="factor") dt1$phyco_rfu <-as.numeric(levels(dt1$phyco_rfu))[as.integer(dt1$phyco_rfu) ]               
if (class(dt1$phyco_rfu)=="character") dt1$phyco_rfu <-as.numeric(dt1$phyco_rfu)
if (class(dt1$turbidity)=="factor") dt1$turbidity <-as.numeric(levels(dt1$turbidity))[as.integer(dt1$turbidity) ]               
if (class(dt1$turbidity)=="character") dt1$turbidity <-as.numeric(dt1$turbidity)
if (class(dt1$fdom)=="factor") dt1$fdom <-as.numeric(levels(dt1$fdom))[as.integer(dt1$fdom) ]               
if (class(dt1$fdom)=="character") dt1$fdom <-as.numeric(dt1$fdom)
if (class(dt1$flag_wtemp)!="factor") dt1$flag_wtemp<- as.factor(dt1$flag_wtemp)
if (class(dt1$flag_do_raw)!="factor") dt1$flag_do_raw<- as.factor(dt1$flag_do_raw)
if (class(dt1$flag_do_sat)!="factor") dt1$flag_do_sat<- as.factor(dt1$flag_do_sat)
if (class(dt1$flag_ph)!="factor") dt1$flag_ph<- as.factor(dt1$flag_ph)
if (class(dt1$flag_spec_cond)!="factor") dt1$flag_spec_cond<- as.factor(dt1$flag_spec_cond)
if (class(dt1$flag_chlor_rfu)!="factor") dt1$flag_chlor_rfu<- as.factor(dt1$flag_chlor_rfu)
if (class(dt1$flag_phyco_rfu)!="factor") dt1$flag_phyco_rfu<- as.factor(dt1$flag_phyco_rfu)
if (class(dt1$flag_turbidity)!="factor") dt1$flag_turbidity<- as.factor(dt1$flag_turbidity)
if (class(dt1$flag_fdom)!="factor") dt1$flag_fdom<- as.factor(dt1$flag_fdom)

# Convert Missing Values to NA for non-dates



# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(lakeid)
summary(sampledate)
summary(sampletime)
summary(depth)
summary(wtemp)
summary(do_raw)
summary(do_sat)
summary(ph)
summary(spec_cond)
summary(chlor_rfu)
summary(phyco_rfu)
summary(turbidity)
summary(fdom)
summary(flag_wtemp)
summary(flag_do_raw)
summary(flag_do_sat)
summary(flag_ph)
summary(flag_spec_cond)
summary(flag_chlor_rfu)
summary(flag_phyco_rfu)
summary(flag_turbidity)
summary(flag_fdom) 
# Get more details on character variables

summary(as.factor(dt1$lakeid)) 
summary(as.factor(dt1$flag_wtemp)) 
summary(as.factor(dt1$flag_do_raw)) 
summary(as.factor(dt1$flag_do_sat)) 
summary(as.factor(dt1$flag_ph)) 
summary(as.factor(dt1$flag_spec_cond)) 
summary(as.factor(dt1$flag_chlor_rfu)) 
summary(as.factor(dt1$flag_phyco_rfu)) 
summary(as.factor(dt1$flag_turbidity)) 
summary(as.factor(dt1$flag_fdom))
detach(dt1)               

# ---- RRR script to pull out Mendota DO profiles ----

library(lubridate)
library(tidyr)

exo <- dt1

plot.profiles <- function(my.do, my.folder, index.depths){
  depths <- colnames(my.do)[index.depths] |>
    sub(pattern = "m", replacement = "") |>
    as.numeric()
  for (r in 1:nrow(my.do)){
    day.label <- paste0("row ",r," - ", month(my.do$Month[r], abbr = T, label = TRUE), " ",my.do$Day[r], ", ", my.do$Year[r])
    vals <- my.do[r, index.depths]
    deps <- depths[!is.na(vals)]
    vals <- vals[!is.na(vals)]
    
    jpeg(filename = file.path(my.folder, paste0(day.label,".jpg")), units = "in", width = 5.5, height = 7, res = 90, quality = 50)
    par(mar = c(8,6.25,2,4.75))
    plot(x = c(0,33), y = c(-25,0), ann = F, axes = F, type = "n")
    box(which = "plot")
    mtext(text = day.label, side = 3, line = .5, cex = 1.2)
    
    points(x = vals, y = -deps, bg = "pink", pch = 21, cex = 2, xpd = T)
    text(x = vals, y = -deps, labels = deps, cex = .7)
    
    axis(side = 1, at = c(0,5,10,15,20,25,30), labels = F, tck = -.02, line = 0)
    axis(side = 1, at = c(0,5,10,15,20,25,30), labels = T, lwd = 0, line = -.7)
    mtext(text = "Dissolved Oxygen (mg/L)", side = 1, line = 3)
    
    axis(2, at = seq(0,-25,-5), labels = F, las = 2, line = 0)
    axis(2, at = seq(0,-25,-5), labels = seq(0,25,5), las = 2, lwd = 0, line = -.25)
    mtext(text = "Depth (m)", side = 2, line = 3)
    
    if (!is.na(my.do$Notes.Dissolved.Oxygen[r])){
      mtext(text = paste("Notes:", my.do$Notes.Dissolved.Oxygen[r]), side = 1, line = 6, at = -12, adj = 0, cex = .7)
    }
    if (!is.na(my.do$Source.Dissolved.Oxygen[r])){
      mtext(text = paste("Data source:", my.do$Source.Dissolved.Oxygen[r]), side = 1, line = 6.75, at = -12, adj = 0, cex = .7)  
    }
    
    dev.off()
  }
}

head(exo)
cbind(1:ncol(exo), colnames(exo))
unique(exo$lakeid) # all mendota
exo <- exo[ ,-1]
exo$sampletime <- parse_date_time(x = exo$sampletime, orders = "HMS")

oxy <- data.frame("Year" = year(exo$sampledate), "Month" = month(exo$sampledate), "Day" = day(exo$sampledate),
                  "Hour" = hour(exo$sampletime), "Minute" = minute(exo$sampletime), 
                  "Depth.m" = exo$depth, "Dissolved.Oxygen.mg.L" = exo$do_raw, 
                  "People" = "Stanley Lab",
                  "Notes.Dissolved.Oxygen" = as.character(exo$flag_do_raw), 
                  "Source.Dissolved.Oxygen" = "knb-lter-ntl.400.3")

index <- which(is.na(oxy$Dissolved.Oxygen.mg.L))
oxy <- oxy[-index, ]
unique(oxy$Notes.Dissolved.Oxygen)
oxy$Notes.Dissolved.Oxygen <- NA
unique(oxy$Depth.m)

# get single time for each profile
oxy.wide <- oxy
oxy.wide$Timestamp <- parse_date_time(x = paste(oxy$Year,oxy$Month,oxy$Day,oxy$Hour,oxy$Minute), orders = "ymdHM", tz = "Etc/GMT-5")
oxy.wide$Date <-  parse_date_time(x = paste(oxy$Year,oxy$Month,oxy$Day), orders = "ymd", tz = "Etc/GMT-5")

for (p in unique(oxy.wide$Date)){
  index <- which(oxy.wide$Date == p)
  cat(as.character(p),"---", length(index),"\n")
  ave.time <- mean(oxy.wide$Timestamp[index])
  ave.time <- round_date(x = ave.time, unit = "minute")
  oxy.wide$Timestamp[index] <- ave.time
}

oxy.wide$Hour <- hour(oxy.wide$Timestamp)
oxy.wide$Minute <- minute(oxy.wide$Timestamp)

colnames(oxy.wide)
oxy.wide <- pivot_wider(data = oxy.wide, 
                        id_cols = c("Year","Month","Day","People","Notes.Dissolved.Oxygen","Source.Dissolved.Oxygen"), 
                        names_from = "Depth.m", values_from = "Dissolved.Oxygen.mg.L")

# plot.profiles(my.do = oxy.wide, my.folder = "~/Desktop/DO_plots/exo/", index.depths = 7:ncol(oxy.wide))
# all look good

depths <- colnames(oxy.wide)[7:ncol(oxy.wide)]
depths <- as.numeric(depths)
index <- order(depths)
oxy.wide <- oxy.wide[ ,c(1:6,(7:ncol(oxy.wide))[index])]
colnames(oxy.wide)[7:ncol(oxy.wide)] <- paste0(colnames(oxy.wide)[7:ncol(oxy.wide)],"m")
colnames(oxy.wide)

# ---- Export Files ----

saveRDS(object = oxy.wide, file = save.data.as)



