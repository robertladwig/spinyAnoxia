library(glmtools)
library(zoo)
library(tidyverse)
library(ggplot2)
library(pracma)
library(lubridate)
library(reshape2)
library(patchwork) # Plot all lakes
library(rLakeAnalyzer)

# Package ID: knb-lter-ntl.29.30 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: Physical Limnology of Primary Study Lakes 1981 - current.
# Data set creator:  John Magnuson - University of Wisconsin
# Data set creator:  Stephen Carpenter - University of Wisconsin
# Data set creator:  Emily Stanley - University of Wisconsin
# Contact:  NTL Information Manager -  University of Wisconsin  - ntl.infomgr@gmail.com
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/29/30/03e232a1b362900e0f059859abe8eb97"
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

# set wd to current dir of script and source functions
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source('1a_StratificationFunctions.R')

# Choose density criteria
dens.difference = 0.1 # cutoff for stratification in g/kg 0.05 #
p.duration = list() # for figures
p.start = list() # for figures
p.end = list() # for figures
p.start.ribbon = list() # for figures
p.end.ribbon = list() # for figures
p.duration.ribbon = list() # for figures
lake.strat.list = list() # for data


lakes =  c('ME')

for (ll in 1:length(lakes)) {
lake = lakes[ll]

# observed DO data from LTER
lter.df = dt1 %>% dplyr::filter(lakeid == lake) %>%
  select(sampledate,depth,wtemp,o2) %>%
  dplyr::filter(!is.na(wtemp))
# only keep depths with  more than 100 sampling points for robust interpolation
keep.depths = data.frame(depths = table(lter.df$depth)) %>% dplyr::filter(depths.Freq > 100)
lter.df = lter.df %>% dplyr::filter(depth %in% keep.depths$depths.Var1)
table(lter.df$depth) # check on depths

times_obs <- unique(lter.df$sampledate)

deps = unique(lter.df$depth)
approx.dep <- seq(0,floor(max(deps)-1),1)

# interpolated observed temp data
app.lter.df <- matrix(NA, nrow = length(approx.dep), ncol = length(times_obs))

for (ii in 1:ncol(app.lter.df)){
  id <- which(lter.df$sampledate == times_obs[ii])
  if (sum(!is.na(lter.df$wtemp[id])) >= 2) { # more than 2 water samples
    app.lter.df[,ii] <- na.approx(lter.df$wtemp[id],lter.df$depth[id],
                                 approx.dep,na.rm = FALSE, rule = 2)
  } else {
    app.lter.df[,ii] <- rep(NA,length(approx.dep))
  }
}

list.lter.df <- list('time' = times_obs, 'depth' = approx.dep, 'sim' = app.lter.df)


sim.df = data.frame(time = list.lter.df$time[col(list.lter.df$sim)], depth = list.lter.df$depth[row(list.lter.df$sim)],
           sim = c(list.lter.df$sim))

# now we interpolate/extrapolate observed data using LINEAR, CONSTANT and SPLINE interpolation (not cubic, because hell know why it's not working at all)
filled.time <- seq.Date(list.lter.df$time[1], list.lter.df$time[length(list.lter.df$time)], by = 'day')
filled.time = filled.time[year(filled.time) > year(times_obs[1])]


interp.method <- function(method) {

empty.matrix = matrix(NA, nrow = nrow(list.lter.df$sim), ncol = length(filled.time))

  # loop through depths
  for (jj in 1:length(approx.dep)){

    if (method == 'constant.1') {
      empty.matrix[jj,] <- approx(list.lter.df$time, list.lter.df$sim[jj,], filled.time, method = 'constant', f = 0)$y
    } else if (method == 'constant.2') {
      empty.matrix[jj,] <- approx(list.lter.df$time, list.lter.df$sim[jj,], filled.time, method = 'constant', f = 1)$y
    } else if (method == 'linear') {
      empty.matrix[jj,] <- approx(list.lter.df$time, list.lter.df$sim[jj,], filled.time)$y
    } else if (method == 'spline') {
      empty.matrix[jj,] <- spline(list.lter.df$time, list.lter.df$sim[jj,], xout = filled.time, method = "fmm")$y
    }
  }
interp.list = list('time' = filled.time, depth = approx.dep,'sim' = empty.matrix)
return(interp.list)
}

do.constant.1 = interp.method(method = 'constant.1')
do.constant.2 = interp.method(method = 'constant.2')
do.linear = interp.method(method = 'linear')
do.spline = interp.method(method = 'spline')

get_ssi_first <- function(data){
  hypso <- read_csv('../data_input/LakeEnsemblR_bathymetry_standard.csv')
  H <- hypso$Depth_meter
  A <- hypso$Area_meterSquared

  areas <- approx(H, A, unique(data$depth))$y
  depths = unique(data$depth)

  df.hyp <-  data.frame('depths' = depths, 'areas' = areas)

  bath = data.frame('depths' = depths, 'areas' = areas)

  df.ssi <- matrix(NA, ncol = 1 + nrow(do.spline$sim), nrow = length(data$time) * length(data$depth))
  df.ssi = data.frame(df.ssi)
  colnames(df.ssi) <- c('datetime' ,# 'depth',
    paste0('wtr_',seq(1:nrow(do.spline$sim))))
  df.ssi$datetime = rep(data$time)#, each = length(data$depth))
  # df.ssi$depth = rep(data$depth,  length(data$time))

  for (i in seq(1:nrow(do.spline$sim))){
    df.ssi[,1+i] = data$sim[i,]
  }

  ssi <- ts.schmidt.stability(wtr = df.ssi, bathy = df.hyp)

  ggplot(ssi) + geom_line(aes(datetime, schmidt.stability
                              )) + xlim(as.Date('2010-01-01'), as.Date('2010-12-31'))

  max.ssi <-  ssi%>%
    mutate(year = year(datetime), doy = lubridate::yday(datetime)) %>%
    group_by(year) %>%
    summarise(max.ssi = max(schmidt.stability))

  ssi.na <- ssi
  ssi.na$schmidt.stability = ifelse(ssi.na$schmidt.stability > mean(max.ssi$max.ssi)*0.1, ssi.na$schmidt.stability, NA)

  df =  ssi.na%>%
    mutate(year = year(datetime), doy = lubridate::yday(datetime)) %>%
    group_by(year) %>%
    summarise(schmidt.start = attributes(na.contiguous(schmidt.stability))$tsp[1])
  return(max.ssi) # changed from df to max.ssi, 3/29/2023
}

df.ssi = get_ssi_first(do.spline)



strat.constant.1 <- output_stratdur(df = do.constant.1)
strat.constant.2 <- output_stratdur(df = do.constant.2)
strat.linear <- output_stratdur(df = do.linear)
strat.spline <- output_stratdur(df = do.spline)

strat.duration <- data.frame('year' = strat.linear$start$year,
                             'linear' = strat.linear$duration,
                             'constant.low' = strat.constant.2$start$X2 - strat.constant.1$start$X1,
                             'constant.high' = strat.constant.1$start$X2 - strat.constant.2$start$X1,
                             'spline' = strat.spline$duration)
lake.strat.duration <- reshape2::melt(strat.duration, 'year')

# Function to convert doy to date
ytoy <- function(x, year) as.Date(x, origin = paste0("2000-01-01"))

strat.start <- data.frame('year' = strat.linear$start$year,
                          'linear' = strat.linear$start$X1,
                          'constant.high' = strat.constant.1$start$X1,
                          'constant.low' = strat.constant.2$start$X1,
                          'spline' = strat.spline$start$X1) %>%
  mutate_at(c("linear", "constant.high","constant.low","spline"), ~ytoy(., year = year))
lake.strat.start <- reshape2::melt(strat.start, 'year')


strat.end <- data.frame('year' = strat.linear$start$year,
                        'linear' = strat.linear$start$X2,
                        'constant.high' = strat.constant.1$start$X2,
                        'constant.low' = strat.constant.2$start$X2,
                        'spline' = strat.spline$start$X2) %>%
  mutate_at(c("linear", "constant.high","constant.low","spline"), ~ytoy(., year = year))
lake.strat.end <- reshape2::melt(strat.end, 'year')

lake.strat = lake.strat.duration %>% left_join(lake.strat.start, by = c('year','variable')) %>%
  left_join(lake.strat.end, by = c('year','variable'))
names(lake.strat) = c('year','interp','duration','strat.start','strat.end')

lake.strat.list[[ll]] = lake.strat

p.start.ribbon[[ll]] = ggplot(strat.start) +
  geom_ribbon(aes(x = year, ymin = constant.low, ymax = constant.high), fill = 'grey80') +
  geom_line(aes(x = year, y = linear)) +
  geom_point(aes(x = year, y = linear)) +
  geom_line(aes(x = year, y = spline), linetype = 2, color = 'red3') +
  ylab('Stratification Start') +
  ggtitle(lake) +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  theme_bw(base_size = 9)

p.end.ribbon[[ll]] = ggplot(strat.end) +
  geom_ribbon(aes(x = year, ymin = constant.low, ymax = constant.high), fill = 'grey80') +
  geom_line(aes(x = year, y = linear)) +
  geom_point(aes(x = year, y = linear)) +
  geom_line(aes(x = year, y = spline), linetype = 2, color = 'red3') +
  ylab('Stratification End') +
  ggtitle(lake) +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  theme_bw(base_size = 9)

p.duration.ribbon[[ll]] = ggplot(strat.duration) +
  geom_ribbon(aes(x = year, ymin = constant.low, ymax = constant.high), fill = 'grey80') +
  geom_line(aes(x = year, y = linear)) +
  geom_point(aes(x = year, y = linear)) +
  geom_line(aes(x = year, y = spline), linetype = 2, color = 'red3') +
  ylab('Stratification Duration') +
  ggtitle(lake) +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  theme_bw(base_size = 9)


}


write_csv(strat.duration,'../data_processed/stratification.csv')
write_csv(strat.start,'../data_processed/stratification_start.csv')
write_csv(strat.end,'../data_processed/stratification_end.csv')
write_csv(df.ssi, '../data_processed/ssi.csv') # changed from ssi_startdate100.csv to ssi.csv 3/29/23

