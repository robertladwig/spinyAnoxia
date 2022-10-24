setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(tidyverse)
library(timeSeries)
library(statcomp)
library(zoo)
library(rLakeAnalyzer)
library(lubridate)
library(pracma)
library(broom)
library(ggpmisc)
library(patchwork)

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

full.do <- readRDS('../data_input/8_Combined_LTER_DO_data_with_full_profiles.rds')

df.livingstone = data.frame('year' = NULL,
                            'depth' = NULL,
                            'jz' = NULL,
                            'alphaz' = NULL,
                            'id' = NULL)
coeff = data.frame('year' = NULL,
                   'Jz' = NULL,
                   'Jv' = NULL,
                   'Ja' = NULL,
                   'id' = NULL)
anoxicfactor = data.frame('year' = NULL,
                          'AF' = NULL,
                          'id' = NULL)

df.lag = data.frame('year' = NULL,
           'timelag' = NULL,
           'id' = NULL)

hypso <- read_csv('../data_input/LakeEnsemblR_bathymetry_standard.csv')
H <- hypso$Depth_meter
A <- hypso$Area_meterSquared

df <- dt1 %>%
  dplyr::filter(lakeid == 'ME') %>%
  dplyr::select(year4, sampledate, depth, wtemp, o2, flago2)

summary(as.factor(df$depth))

if (23 > max(H)){
  H <- c(23, H)
  A <- c(min(A), A)
}

# calculate the q factor for alpha
if (length(A) <= 2){
  areas <- approx(H, A, seq(max(H), min(A),-1))$y
  depths <- seq(max(H), min(H),-1)
} else {
  areas = A
  depths = H
}


fit_q <- function(x, areas, depths){

  pred_areas <- max(areas) * (1 - depths/max(depths))^x
  fit <- sqrt(sum((areas - pred_areas)^2)/length(areas))
  return(fit)
}
# use Brent method to fit q
q <- optim(par = 0.1, fn = fit_q, areas = areas, depths = depths, method = 'Brent', lower = 0.5, upper = 2.0)

# visual check
plot(depths, areas)
lines(depths, max(areas) * (1 - depths/max(depths))^q$par, col = 'red')

strat.onset <- read_csv('../data_processed/stratification_start.csv')
strat.onset = strat.onset %>%
  group_by(year) %>%
  mutate(mean = mean(yday(linear), yday(constant.high), yday(constant.low), yday(spline)))

ssi.start <- read_csv('../data_processed/ssi_startdate100.csv')


for (id.year in unique(df$year4)[-1]){
  obs <- df %>%
    dplyr::filter(year4 == id.year) %>%
    filter_at(vars(o2), all_vars(!is.na(.)))

  dat1 <- matrix(NA, nrow = length(seq(0, 23, 0.5)), ncol = length(unique(obs$sampledate)))
  ph1 <- matrix(NA, nrow = 4, ncol = length(unique(obs$sampledate)))
  for (i in unique(obs$sampledate)){

    obs.dt <- obs %>%
      dplyr::filter(sampledate == i)


    if(length(na.omit(obs.dt$o2)) < 2){
      next
    }

    dat1[, match(i, unique(obs$sampledate))] <- approx(obs.dt$depth,
                                                       obs.dt$o2,
                                                       seq(0, 23, 0.5), rule = 2)$y
    ph1[1, match(i, unique(obs$sampledate))] <- thermo.depth(approx(obs.dt$depth,
                                                                    obs.dt$wtemp,
                                                                    seq(0, 23, 0.5), rule = 2)$y, seq(0, 23, 0.5))
    ph1[2, match(i, unique(obs$sampledate))] <- obs.dt$wtemp[which.min(obs.dt$depth)] - obs.dt$wtemp[which.max(obs.dt$depth)]
    ph1[3, match(i, unique(obs$sampledate))] <- center.buoyancy(approx(obs.dt$depth,
                                                                    obs.dt$wtemp,
                                                                    seq(0, 23, 0.5), rule = 2)$y, seq(0, 23, 0.5))
    ph1[4, match(i, unique(obs$sampledate))] <- meta.depths(approx(obs.dt$depth,
                                                                       obs.dt$wtemp,
                                                                       seq(0, 23, 0.5), rule = 2)$y, seq(0, 23, 0.5))[2]
  }
  if(yday(unique(obs$sampledate))[which.max(colSums(dat1))] < 250 &&
     min(dat1[, which.max(colSums(dat1))]) >= 5 &&
     yday(unique(obs$sampledate))[which.max(colSums(dat1))] > 60){
    max.date <- which.max(colSums(dat1))
  } else {
    ix = which(yday(unique(obs$sampledate)) < 250 &
                 yday(unique(obs$sampledate)) > 60)
    iy = which(apply(dat1,2,min) >= 5)
    # max.date <- which.max(colSums(dat1[, na.omit(match(iy,ix))[1]]))
    max.date <- ix[ na.omit(match(iy,ix))][1]
  }
  if (colSums(dat1)[max.date+1] < colSums(dat1)[max.date] &&
      colSums(dat1)[max.date+2] > colSums(dat1)[max.date + 1] ){
    max.date <- which.max(colSums(dat1))+2
  }
  
  strat.onset.date <- strat.onset$mean[match(id.year, strat.onset$year)]
  absdiff <- (abs(yday(unique(obs$sampledate)) - strat.onset.date)) #yday('1989-04-15')
  max.date2 <- which.min(absdiff)
  
  ssi.start.date <- ssi.start$schmidt.start[match(id.year, ssi.start$year)]
  
  if (max.date > max.date2){
    max.date <- max.date
  } else {
    max.date <- max.date
  }

  
  therm.dep <- ceiling(mean(ph1[4,], na.rm = T))

  dat2 <- dat1[which(seq(0, 23, 0.5) == therm.dep) : nrow(dat1), max.date:ncol(dat1)]
  deps <- seq(0,23,0.5)[which(seq(0, 23, 0.5) == therm.dep) : nrow(dat1)]
  times <- yday(unique(obs$sampledate)[max.date:ncol(dat1)])


  plot( yday(unique(obs$sampledate)),dat1[(which(seq(0, 23, 0.5) == therm.dep) : nrow(dat1))[1],], lty = 1, ylim = c(0, max(na.omit(dat1[1,]))),
       ylab = 'DO conc [mg/m3]')
  abline(v = yday(unique(obs$sampledate))[max.date], col = 'red', lty =2, lwd = 3)
  for (p in (which(seq(0, 23, 0.5) == therm.dep) : nrow(dat1))[2]:max((which(seq(0, 23, 0.5) == therm.dep) : nrow(dat1)))){
    lines(yday(unique(obs$sampledate)),dat1[p,])
  }

  thresh <- 1.5
  
  areas.af <- approx(depths, areas, seq(0,23,0.5))$y
  dat.af = matrix(NA, ncol = length(seq(yday(unique(obs$sampledate))[1], max(yday(unique(obs$sampledate))), 1)), nrow = nrow(dat1))
  for (m in 1:nrow(dat.af)){
    dat.af[m,] <- interp1(yday(unique(obs$sampledate)),
                      dat1[m, ],
                      xi = seq(yday(unique(obs$sampledate))[1], max(yday(unique(obs$sampledate))), 1),
                      method = 'spline')
  }
  dat.af2 <- dat.af[, which(seq(yday(unique(obs$sampledate))[1], max(yday(unique(obs$sampledate))), 1) > 120)]
  dat.af3 <- ifelse(dat.af2 <= thresh, 1, NA)

  seq.af <- na.contiguous(apply(dat.af3, 2, function(x) which.min(x)[1]))
  sum(areas.af[seq.af])/max(areas.af)

  anoxicfactor = rbind(anoxicfactor, data.frame('year' = id.year,
                            # 'AF' = sum(areas.af[seq.af])/max(areas.af),
                            'AF' = sum(areas.af[apply(dat.af3, 2, function(x) which.min(x)[1])], na.rm = T)/max(areas.af),
                            'id' = 'ME'))

  for (j in 1:nrow(dat2)){


    start = dat2[j,1]
    end = dat2[j, which(dat2[j,] < thresh)[1]]

    start.time = times[1]
    end.time = times[which(dat2[j,] < thresh)[1]]
    
    if (j == nrow(dat2)){
      df.lag <- rbind(df.lag, data.frame('year' = id.year,
                                         'timelag' = end.time - strat.onset.date,#strat.onset.date,ssi.start.date,
                                         'id' = 'ME'))
    }


    if (all(dat2[j,] > 1)){
      next
    }

    dat3 <- interp1(times[1:which(dat2[j,] <= thresh)[1]],
                    dat2[j, 1:which(dat2[j,] <= thresh)[1]],
                    xi = seq(times[1], times[which(dat2[j,] <= thresh)[1]], 1),
                    method = 'linear')

    times.in <- seq(times[1], times[which(dat2[j,] <= thresh)[1]], 1)

    df.livingstone <- rbind(df.livingstone, data.frame('year' = id.year,
               'depth' = deps[j],
               'jz' = (dat3[1] - dat3[which(dat3 <= thresh)[1]]) / (times.in[1] - times.in[which(dat3 <= thresh)[1]]),
               'alphaz' = q$par/(max(deps)+1 - deps[j]),
               'id' = 'ME'))

    ggplot(subset(df.livingstone, year == id.year)) +
      geom_point(aes(alphaz, jz)) +
      scale_x_continuous(trans='log10') +
      theme_bw()
  }
}

for (l in unique(df.livingstone$year)){
  dit.l <- df.livingstone %>%
    dplyr::filter(year == l)

  ggplot(dit.l) +
    geom_point(aes(alphaz, jz)) +
    scale_x_continuous(trans='log10') +
    theme_bw()

  sum.mod <- lm(abs(jz) ~ log(alphaz), data = dit.l)
  p  <-summary(sum.mod)$coefficients[,"Pr(>|t|)"][2]
  
  
  
  if (!is.na(p) && p <= 0.05){
    coeff = rbind(coeff, data.frame('year' = l,
                                    'Jz' = abs(median(dit.l$jz, na.rm = T)),
                                    'Jv' = sum.mod$coefficients[1],
                                    'Ja' = sum.mod$coefficients[2],
                                    'id' = "ME"))
  } else {
    print(paste0(l,' violates assumptions: p= ',as.numeric(p)))
    coeff = rbind(coeff, data.frame('year' = l,
                                    'Jz' = NA,
                                    'Jv' = NA,
                                    'Ja' = NA,
                                    'id' = "ME"))
  }
  
  # coeff = rbind(coeff, data.frame('year' = l,
  #                                 'Jz' = abs(median(dit.l$jz, na.rm = T)),
  #                                 'Jv' = sum.mod$coefficients[1],
  #                                 'Ja' = sum.mod$coefficients[2],
  #                                 'id' = "ME"))

}

my.formula <- y ~ log(x)
ggplot(df.livingstone, aes(alphaz, abs(jz))) +
  geom_point() +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ log(x)) +
  stat_poly_eq(formula = my.formula,
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
                 parse = TRUE,size = rel(4.5),
                 label.y = 0.05,
                 label.x = 0.1) +
  facet_wrap(~year,scales = "free") +
  theme_bw() +
    # ylab(expression("Diss. oxygen depletion rate [g"*~m^{-3}*","*~d^{-1}))
  labs(y= expression("Diss. oxygen depletion rate J(z) [g"*~m^{-3}*""*~d^{-1}*"]"),
         x= expression("\u03b1(z) ["~m^{2}*""*~m^{-3}*"]"))+
  theme(text = element_text(size=20),
          legend.position = "none",
          axis.text.x = element_text(angle=0, hjust=1))
ggsave('../figs/livingstone_regressions.png', dpi = 300, width = 22,height = 9, units = 'in')

g1 <- ggplot(anoxicfactor) +
  geom_line(aes(year, AF)) +
  geom_point(aes(year, AF)) +
  ylab('Anoxic Factor (days per season)') + xlab('') +
  theme_bw()
g2 <- ggplot(coeff, aes(year, Jz, col = 'Volumetric')) +
  geom_line(aes(year, Jv, col = 'Volumetric')) +
  # geom_line(aes(year, Jz, col = 'Median Flux')) +
  geom_point(aes(year, Jv, col = 'Volumetric')) +
  # geom_point(aes(year, Jz, col = 'Median Flux')) +
  geom_smooth(method = "loess", size = 1.5) +
  geom_line(aes(year, Ja , col = 'Areal')) +
  geom_point(aes(year, Ja , col = 'Areal')) +
  geom_smooth(aes(year, Ja , col = 'Areal'), method = "loess", size = 1.5) +
  scale_y_continuous(sec.axis = sec_axis(~.*1, name = expression("Areal flux ["*g~m^{-2}*d^{-1}*"]"))) +
  ylab(expression("Volumetric flux ["*g~m^{-3}*d^{-1}*"]")) + xlab('') +
  theme_bw()+
  theme(axis.line.y.right = element_line(color = "red"),
        axis.ticks.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"),
        axis.title.y.right = element_text(color = "red"),
        axis.line.y.left = element_line(color = "darkcyan"),
        axis.ticks.y.left = element_line(color = "darkcyan"),
        axis.text.y.left = element_text(color = "darkcyan"),
        axis.title.y.left = element_text(color = "darkcyan"),
        legend.position = "none"
  ); g1 / g2


write_csv(anoxicfactor, '../data_processed/anoxicfactor.csv')
write_csv(coeff, '../data_processed/dosinks.csv')
write_csv(df.lag, '../data_processed/timelag.csv')
