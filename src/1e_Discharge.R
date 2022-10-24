#### DataRetrieval Tutorial ###
#### Working with USGS gage data from the Yahara River ####
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
# Load packages (you may need to install them first)
library(dataRetrieval)
library(tidyverse)
library(sf) # pacakge for creating spatial files
library(mapview) # cool package for mapping
library(patchwork) # For adding plots side by side
library(lubridate)

# We're going to be looking at the Yahara River (a river that flows into Lake Mendota)
# USGS 05427718 YAHARA RIVER AT WINDSOR, WI
# https://waterdata.usgs.gov/nwis/uv/?site_no=05427718&PARAmeter_cd=00045
siteNumber <- "05427718" 
YaharaInfo <- readNWISsite(siteNumber)

# Use the readNWISsite function to obtain all of the information available for a particular USGS site (or sites) such 
# as full station name, drainage area, latitude, and longitude. readNWISsite can also access information about multiple 
# sites with a vector input.
siteINFO <- readNWISsite(siteNumber)

# Make the data spatial with the sf (we'll learn about maps next week)
# Just need to tell it which columns have the lat/long data, and the crs (in this case 4326 is WGS 84)
siteINFO.sf <- st_as_sf(siteINFO, coords = c("dec_long_va","dec_lat_va"), remove = FALSE, crs=4326)

# Mapview: make an interactive map quickly:
mapview(siteINFO.sf, layer.name="USGS 05427718 YAHARA RIVER AT WINDSOR, WI")
# Zoom out to see Lake Mendota and Madison

# Use whatNWISdata To discover what data is available for a particular USGS
# site, including measured parameters, period of record, and number of samples
# (count), use the whatNWISdata function. 

# It is possible to limit the retrieval
# information to a subset of services. The possible choices for services are:
# "dv" (daily values), "uv", or "iv" (unit values), "qw" (water-quality), "sv"
# (sites visits), "pk" (peak measurements), "gw" (groundwater levels), "ad"
# (sites included in USGS Annual Water Data Reports External Link), "aw" (sites
# monitored by the USGS Active Groundwater Level Network External Link), and
# "id" (historical instantaneous values).
#
# In the following example, we limit the retrieved data to only daily data. The
# default for "service" is all, which returns all of the available data for that
# site. Likewise, there are arguments for parameter code (parameterCd) and
# statistic code (statCd) to filter the results. The default for both is to
# return all possible values (all). The returned count_nu for "uv" data is the
# count of days with returned data, not the actual count of returned values.

# Table 3: Commonly used USGS Stat Codes
# StatCode	shortName
# 00001	Maximum
# 00002	Minimum
# 00003	Mean
# 00008	Median

dailyDataAvailable <- whatNWISdata(siteNumber = siteNumber,
                                   service="dv", statCd="00003")

# What parameter are measured? 
dailyDataAvailable$parm_cd
parameterINFO <- readNWISpCode(dailyDataAvailable$parm_cd)
parameterINFO$parameter_nm

# Get daily data
parameterCd <- c("00010","00060")  # Temperature and discharge
statCd <- c("00001","00003")  # Mean and maximum
startDate <- "1995-01-01"
endDate <- "2021-12-31"
yahara.daily <- readNWISdv(siteNumbers = siteNumber, 
                           parameterCd, startDate, endDate, statCd=statCd)

# Make a plot of 2020 mean vs maximum temperature
ggplot(yahara.daily) +
  geom_path(aes(x = Date, y = X_00010_00003), col = 'red') +
  geom_path(aes(x = Date, y = X_00010_00001), col = 'red4') +
  ylab('Temperature (degC)') +
  theme_bw()

#### QUESTION (1) ####
# What was the mean annual temperature of the river? 
# [Show code]

#### QUESTION (2) ####
# What was the warmest temperature of the river in 2020? What day did this occur on? 

# Let's get raw data instead of the daily values 
# Raw data (in this case every 15 minutes)
yahara.raw = readNWISuv(siteNumbers = siteNumber, parameterCd=c("00045","00060"), startDate = startDate, endDate = endDate)

#### QUESTION (3) ####
# What is parameter code 00045 measuring? What are the units?
# hint use parameterCdFile %>% filter(parameter_cd == _____)

# Let's create our own mean daily value for discharge 
yahara.daily2 = yahara.raw %>% mutate(Date = as.Date(dateTime)) %>% # create date column 
  group_by(Date) %>% 
  summarise(meanDischarge = mean(X_00060_00000), sumPrecip = sum(X_00045_00000))

# Let's join with the early daily data to see if there's a difference
yahara.daily2 %>% left_join(yahara.daily) %>% # What is it joining by? 
  ggplot() +
  geom_point(aes(x = X_00060_00003, y = meanDischarge)) +
  geom_abline() # this adds a 1:1 line

#### QUESTION (4) ####
# Why might our calculated daily mean be different from the daily values we downloaded that were calculated by the USGS?

# Let's plot mean daily discharge and total daily precipitation (add units to the y-axis labels)
p1 = yahara.daily2 %>% ggplot() +
  geom_path(aes(x = Date, y = meanDischarge)) +
  ylab('Discharge')

p2 = yahara.daily2 %>% ggplot() +
  geom_col(aes(x = Date, y =  sumPrecip)) +
  ylab('Precipitation')

p1/p2

df <- yahara.daily2 %>%
  mutate(year = year(Date)) %>%
  group_by(year) %>%
  summarise(discharge = mean(meanDischarge, na.rm = T),
            max.discharge = max(meanDischarge, na.rm = T),
            min.discharge = min(meanDischarge, na.rm = T),
            precip = mean(sumPrecip, na.rm = T))

ggplot(df, aes(year, max.discharge)) +
  geom_line()

write_csv(df, '../data_processed/discharge.csv')

#### QUESTION (5) ####
# Just by observing the two plots, how is precipitation related to discharge? 

#### QUESTION (6) ####
# How could you assess this quantitatively? [You don't need to run an analysis, 
# rather just tell me how you could assess the relationship between precipitation and discharge?]

# Here we'll show how to use dplyr to aggregate our data to monthly timesteps,
# and then plot that data. One new function we show below is summarize_all which
# means we apply a set of functions (more than one) to all the columns in our
# dataframe. Since we group by month, we are calculating the mean, min, and max
# of temperature, flow, and date. The mutate function allows us to add new
# columns, and so we could easily add different time intervals here and then
# group_by those variables, and summarize by a different time interval.

yahara.month <- yahara.daily %>% 
  rename(flow_cfs =  X_00060_00003) %>% # let's rename our columns 
  select(Date, flow_cfs) %>% # select columns the we want (date and flow)
  mutate(month = lubridate::month(Date, label=T)) %>% 
  select(-Date) %>% 
  group_by(month) %>% 
  summarize_all(c("mean", "max", "min"), na.rm = T) # take the mean, max, and, min of all cols 
#**** we ignored NA values, which if we were being good data scientists/ecologists, we wouldn't do! 

# Let's plot 
ggplot() + 
  geom_pointrange(data = yahara.month, aes(x=month, y= mean,
                                           ymin = min, ymax = max,
                                           group = month), color="blue",alpha=0.5) +
  theme_bw() +
  labs(x="2020", y="Discharge (cfs)", title="Yahara River at Windsor", subtitle="Mean Monthly Discharge")

#### 










