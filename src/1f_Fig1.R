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
library(Boruta)
library(caret)
library(relaimpo)
library(corrplot)
library(RColorBrewer)


strat <- read_csv('../data_processed/stratification.csv')

anoxic <- read_csv("../data_processed/anoxicfactor.csv")

fluxes <- read_csv('../data_processed/dosinks.csv')

biomass <- read_csv('../data_processed/3c_biomass_duration.csv')

discharge <- read_csv('../data_processed/discharge.csv')

cw <- readRDS('../data_processed/yearly_clearwater_stats.rds')

nutrients <- read_csv('../data_processed/nutrients.csv')

spiny <- read_csv('../data_processed/spiny.csv')


col.pre <- "steelblue"
col.post <- "orange3"

df.strat <- strat %>%
  group_by(year) %>%
  mutate(med = mean(linear, constant.low, constant.high, spline)) %>%
  dplyr::select(year, med, linear, constant.low, constant.high, spline)

df.anoxic <- anoxic %>%
  dplyr::filter(year != 1995 & year != 2021) %>%
  dplyr::select(year, AF)

df.flux <- fluxes %>%
  dplyr::filter(year != 1995 & year != 2021) %>%
  dplyr::select(year, Jz, Jv, Ja)

df.biomass <- biomass %>%
  dplyr::filter(Year != 1995 & Year != 2021) %>%
  rename(year = Year)

df.nutrients <- nutrients %>%
  dplyr::filter(year != 1995 & year != 2021) %>%
  rename(year = year)

df.discharge <- discharge %>%
  dplyr::filter(year != 1995 & year != 2021) 

df.cw <- cw %>%
  dplyr::filter(Year > 1995) %>%
  rename(year = Year)

df.spiny <- spiny %>%
  dplyr::filter(year != 1995 & year != 2021) %>%
  rename(year = year, Spiny = mean)

df <- merge(df.strat, df.anoxic, by = 'year')
df <- merge(df, df.flux, by = 'year')
df <- merge(df, df.biomass, by = 'year')
df <- merge(df, df.discharge, by = 'year')
df <- merge(df, df.cw, by = 'year')
df <- merge(df, df.nutrients, by = 'year')
df <- merge(df, df.spiny, by = 'year')

str(df)
head(df)


cool.col <- c("#00AFBB", "#E7B800", "#FC4E07")


g5 <- ggplot(df) +
  geom_line(aes(year, AF)) +
  geom_point(aes(year, AF)) +
  ylab('Anoxic Factor (days per season)') + xlab('') +
  geom_vline(xintercept=2010, linetype = 'dashed') +
  theme_bw()
g6 <- ggplot(df) +
  geom_ribbon(aes(x = year, ymin = constant.low, ymax = constant.high), fill = 'grey80') +
  geom_line(aes(x = year, y = linear)) +
  geom_point(aes(x = year, y = linear)) +
  geom_line(aes(x = year, y = spline), linetype = 2, color = 'red3') +
  ylab('Stratification Duration (days)') + xlab('') +
  geom_vline(xintercept=2010, linetype = 'dashed') +
  theme_bw()
g7 <- ggplot(df, aes(year, Jz, col = 'Volumetric'), col = cool.col[1]) +
  geom_line(aes(year, Jv, col = 'Volumetric'), col = cool.col[1]) +
  # geom_line(aes(year, Jz, col = 'Median Flux')) +
  geom_point(aes(year, Jv, col = 'Volumetric'), col = cool.col[1]) +
  # geom_point(aes(year, Jz, col = 'Median Flux')) +
  geom_smooth(method = "loess", size = 1.5, col = cool.col[1]) +
  geom_line(aes(year, Ja , col = 'Areal'), col = cool.col[2]) +
  geom_point(aes(year, Ja , col = 'Areal'), col = cool.col[2]) +
  geom_smooth(aes(year, Ja , col = 'Areal'), method = "loess", size = 1.5, col = cool.col[2]) +
  scale_y_continuous(sec.axis = sec_axis(~.*1, name = expression("Areal flux ["*g~m^{-2}*d^{-1}*"]"))) +
  ylab(expression("Volumetric flux ["*g~m^{-3}*d^{-1}*"]")) + xlab('') +
  geom_vline(xintercept=2010, linetype = 'dashed') +
  theme_bw()+
  theme(axis.line.y.right = element_line(color = cool.col[2]),
        axis.ticks.y.right = element_line(color =cool.col[2]),
        axis.text.y.right = element_text(color = cool.col[2]),
        axis.title.y.right = element_text(color = cool.col[2]),
        axis.line.y.left = element_line(color = cool.col[1]),
        axis.ticks.y.left = element_line(color = cool.col[1]),
        axis.text.y.left = element_text(color = cool.col[1]),
        axis.title.y.left = element_text(color = cool.col[1]),
        legend.position = "none"
  )
g8 <- ggplot(df) +
  geom_line(aes(year, Days.0.5.mg.L)) +
  geom_point(aes(year, Days.0.5.mg.L)) +

  ylab('Biomass over 0.5 mg/L (days per year)') + xlab('') +
  theme_bw()+
  theme(legend.position="bottom") +
  geom_vline(xintercept=2010, linetype = 'dashed') +
  theme(legend.title=element_blank())

g9 <- ggplot(df) +
  geom_ribbon(aes(x = year, ymin = min.discharge, ymax = discharge), fill = 'grey80') +
  geom_line(aes(year, discharge)) +
  geom_point(aes(year, discharge)) +
  ylab('Yahara Q (cfs)') + xlab('') +
  geom_vline(xintercept=2010, linetype = 'dashed') +
  theme_bw(); 
g10 <- ggplot(df) +
  geom_line(aes(year, Clearwater.Duration)) +
  geom_point(aes(year, Clearwater.Duration)) +
  ylab('Clearwater dur. (days)') + xlab('') +
  geom_vline(xintercept=2010, linetype = 'dashed') +
  theme_bw(); 
g11 <- ggplot(df) +
  geom_line(aes(year, pH)) +
  geom_point(aes(year, pH)) +
  ylab('pH (-)') + xlab('') +
  geom_vline(xintercept=2010, linetype = 'dashed') +
  theme_bw(); 
g12 <- ggplot(df) +
  geom_line(aes(year, PO4.P_surf ), linetype = 'solid') +
  geom_point(aes(year, PO4.P_surf )) +
  geom_line(aes(year, PO4.P_bot), linetype = 'dashed') +
  geom_point(aes(year, PO4.P_bot)) +
  ylab('SRP (mg/L)') + xlab('') +
  geom_vline(xintercept=2010, linetype = 'dashed') +
  theme_bw(); 
g13 <- ggplot(df) +
  geom_line(aes(year, NO3.NO2.N_surf), linetype = 'solid') +
  geom_point(aes(year, NO3.NO2.N_surf)) +
  geom_line(aes(year, NO3.NO2.N_bot ), linetype = 'dashed') +
  geom_point(aes(year, NO3.NO2.N_bot)) +
  ylab('NO3-NO2-N (mg/L)') + xlab('') +
  geom_vline(xintercept=2010, linetype = 'dashed') +
  theme_bw(); 
g14 <- ggplot(df) +
  geom_line(aes(year, RSi)) +
  geom_point(aes(year, RSi)) +
  ylab('React. Silica (mg/L)') + xlab('') +
  geom_vline(xintercept=2010, linetype = 'dashed') +
  theme_bw(); 
g15 <- ggplot(df) +
  geom_line(aes(year, Spiny)) +
  geom_point(aes(year, Spiny)) +
  ylab('Spiny Waterflea (counts)') + xlab('') +
  geom_vline(xintercept=2010, linetype = 'dashed') +
  theme_bw(); 








library(ggpubr)
df.prior = df %>%
  mutate('class' = ifelse(year < 2010, 'prior 2010','post 2010')) %>%
  dplyr::select(class, AF, med, Jz, Jv, Days.0.5.mg.L, discharge, Clearwater.Duration, pH, PO4.P_surf, NO3.NO2.N_surf, RSi, Spiny)
m.df.prior <- reshape2::melt(df.prior, id = 'class')

compare_means(value ~ class, data = m.df.prior %>% dplyr::filter(variable == 'AF'))
compare_means(value ~ class, data =  m.df.prior %>% dplyr::filter(variable == 'AF'), method ="kruskal.test")

p1 <- ggboxplot( m.df.prior %>% dplyr::filter(variable == 'AF'), x = "class", y = "value",
                palette = "jco", xlab = '', ylab = 'Anoxic Factor (days)',fill = c(col.pre, col.post),
                add = "jitter")
#  Add p-value
p1 = p1 + stat_compare_means(label = 'p.format')
# Change method
# p1 + stat_compare_means(method = "t.test")

compare_means(value ~ class, data = m.df.prior %>% dplyr::filter(variable == 'med'))
compare_means(value ~ class, data =  m.df.prior %>% dplyr::filter(variable == 'med'), method ="kruskal.test")

p2 <- ggboxplot( m.df.prior %>% dplyr::filter(variable == 'med'), x = "class", y = "value",
                 palette = "jco", xlab = '', ylab = 'Stratification duration (days)',fill = c(col.pre, col.post),
                 add = "jitter")
#  Add p-value
p2 = p2 + stat_compare_means(label = 'p.format');p2

compare_means(value ~ class, data = m.df.prior %>% dplyr::filter(variable == 'Jz'))
compare_means(value ~ class, data =  m.df.prior %>% dplyr::filter(variable == 'Jz'), method ="kruskal.test")

p3 <- ggboxplot( m.df.prior %>% dplyr::filter(variable == 'Jz'), x = "class", y = "value",
                 palette = "jco", xlab = '', ylab = 'Total oxygen sink (mg/L)',fill = c(col.pre, col.post),
                 add = "jitter")
#  Add p-value
p3 = p3 + stat_compare_means(label = 'p.format')

sddef = m.df.prior %>% dplyr::filter(variable == 'Jz')%>% group_by(class) %>% summarise(mean = mean(value),
                                                                                sdv= sd(value))
sddef$mean[1] - sddef$mean[2]
sqrt(sddef$sdv[1]**2 - sddef$sdv[2]**2)

compare_means(value ~ class, data = m.df.prior %>% dplyr::filter(variable == 'Days.0.5.mg.L'))
compare_means(value ~ class, data =  m.df.prior %>% dplyr::filter(variable == 'Days.0.5.mg.L'), method ="kruskal.test")

p4 <- ggboxplot( m.df.prior %>% dplyr::filter(variable == 'Days.0.5.mg.L'), x = "class", y = "value",
                 palette = "jco", xlab = '', ylab = 'Biomass over 0.5 mg/L (days)',fill = c(col.pre, col.post),
                 add = "jitter")
#  Add p-value
p4 = p4 + stat_compare_means(label = 'p.format')


sddef = m.df.prior %>% dplyr::filter(variable == 'Days.0.5.mg.L')%>% group_by(class)%>% summarise(mean = mean(value),
                                                                                          sdv= sd(value))
sddef$mean[1] - sddef$mean[2]
sqrt(abs(sddef$sdv[1]**2 - sddef$sdv[2]**2))

compare_means(value ~ class, data = m.df.prior %>% dplyr::filter(variable == 'discharge'))
compare_means(value ~ class, data =  m.df.prior %>% dplyr::filter(variable == 'discharge'), method ="kruskal.test")

p5 <- ggboxplot( m.df.prior %>% dplyr::filter(variable == 'discharge'), x = "class", y = "value",
                 palette = "jco", xlab = '', ylab = 'Discharge',fill = c(col.pre, col.post),
                 add = "jitter")
#  Add p-value
p5 = p5 + stat_compare_means(label = 'p.format')

compare_means(value ~ class, data = m.df.prior %>% dplyr::filter(variable == 'Clearwater.Duration'))
compare_means(value ~ class, data =  m.df.prior %>% dplyr::filter(variable == 'Clearwater.Duration'), method ="kruskal.test")

p6 <- ggboxplot( m.df.prior %>% dplyr::filter(variable == 'Clearwater.Duration'), x = "class", y = "value",
                 palette = "jco", xlab = '', ylab = 'Clearwater duration (days)',fill = c(col.pre, col.post),
                 add = "jitter")
#  Add p-value
p6 = p6 + stat_compare_means(label = 'p.format')

sddef = m.df.prior %>% dplyr::filter(variable == 'Clearwater.Duration')%>% group_by(class) %>% summarise(mean = mean(value),
                                                                                                 sdv= sd(value))
sddef$mean[1] - sddef$mean[2]
sqrt(abs(sddef$sdv[1]**2 - sddef$sdv[2]**2))

compare_means(value ~ class, data = m.df.prior %>% dplyr::filter(variable == 'pH'))
compare_means(value ~ class, data =  m.df.prior %>% dplyr::filter(variable == 'pH'), method ="kruskal.test")

p7 <- ggboxplot( m.df.prior %>% dplyr::filter(variable == 'pH'), x = "class", y = "value",
                 palette = "jco", xlab = '', ylab = 'pH',fill = c(col.pre, col.post),
                 add = "jitter")
#  Add p-value
p7 = p7 + stat_compare_means(label = 'p.format')

compare_means(value ~ class, data = m.df.prior %>% dplyr::filter(variable == 'PO4.P_surf'))
compare_means(value ~ class, data =  m.df.prior %>% dplyr::filter(variable == 'PO4.P_surf'), method ="kruskal.test")

p8 <- ggboxplot( m.df.prior %>% dplyr::filter(variable == 'PO4.P_surf'), x = "class", y = "value",
                 palette = "jco", xlab = '', ylab = 'SRP surf (mg/L)',fill = c(col.pre, col.post),
                 add = "jitter")
#  Add p-value
p8 = p8 + stat_compare_means(label = 'p.format')

compare_means(value ~ class, data = m.df.prior %>% dplyr::filter(variable == 'NO3.NO2.N_surf'))
compare_means(value ~ class, data =  m.df.prior %>% dplyr::filter(variable == 'NO3.NO2.N_surf'), method ="kruskal.test")

p9 <- ggboxplot( m.df.prior %>% dplyr::filter(variable == 'NO3.NO2.N_surf'), x = "class", y = "value",
                 palette = "jco", xlab = '', ylab = 'NO3-NO2-N surf (mg/L)',fill = c(col.pre, col.post),
                 add = "jitter")
#  Add p-value
p9 = p9 + stat_compare_means(label = 'p.format')

compare_means(value ~ class, data = m.df.prior %>% dplyr::filter(variable == 'RSi'))
compare_means(value ~ class, data =  m.df.prior %>% dplyr::filter(variable == 'RSi'), method ="kruskal.test")

p10 <- ggboxplot( m.df.prior %>% dplyr::filter(variable == 'RSi'), x = "class", y = "value",
                 palette = "jco", xlab = '', ylab = 'RSi (mg/L)', fill = c(col.pre, col.post),
                 add = "jitter")
#  Add p-value
p10 = p10 + stat_compare_means(label = 'p.format')


compare_means(value ~ class, data = m.df.prior %>% dplyr::filter(variable == 'Spiny'))
compare_means(value ~ class, data =  m.df.prior %>% dplyr::filter(variable == 'Spiny'), method ="kruskal.test")

p11 <- ggboxplot( m.df.prior %>% dplyr::filter(variable == 'Spiny'), x = "class", y = "value",
                  palette = "jco", xlab = '', ylab = 'Spiny (counts)', fill = c(col.pre, col.post),
                  add = "jitter")
#  Add p-value
p11 = p11 + stat_compare_means(label = 'p.format')



# find breaking point
library(bfast)
library(zoo)
library(strucchange)
library(xts)

ts.af =  ts(df$AF, start= 1996, frequency = 1)

plot(ts.af)

plot(merge(
       AF = as.zoo(ts.af),
       zoo(mean(AF), time(AF)),
       CUSUM = cumsum(AF - mean(AF)),
       zoo(0, time(AF)),
       MOSUM = rollapply(AF - mean(AF), 4, sum),
       zoo(0, time(AF))
     ), screen = c(1, 1, 2, 2, 3, 3), main = "", xlab = "Time",
  col = c(1, 4, 1, 4, 1, 4) )

plot(merge(
        AF = as.zoo(ts.af),
        zoo(c(NA, cumsum(head(AF, -1))/1:99), time(AF)),
        CUSUM = cumsum(c(0, recresid(lm(AF ~ 1)))),
        zoo(0, time(AF))
      ), screen = c(1, 1, 2, 2), main = "", xlab = "Time",
   col = c(1, 4, 1, 4) )

AF = as.zoo(ts.af)
plot(1996 + 4:25, sapply(4:25, function(i) {
  before <- 1:i
  after <- (i+1):4
  res <- c(AF[before] - mean(AF[before]), AF[after] - mean(AF[after]))
   sum(res^2)
   }), type = "b", xlab = "Time", ylab = "RSS")

bp.nile <- breakpoints(AF ~ 1)
nile.fac <- breakfactor(bp.nile, breaks = 1 )
fm1.nile <- lm(AF ~ nile.fac - 1)
plot(bp.nile)

ocus.nile <- efp(AF ~ 1, type = "OLS-CUSUM")


opar <- par(mfrow=c(2,1), mar=c(2,2,0,2))
plot(ocus.nile, alt.boundary=F,main="")
abline(v= 2010, lty=2, col='red')
plot(AF, ylab="Annual Flow of the river Nile") > abline(h= mean(AF),col='blue')
abline(v= 2010, lty=2, col='red')
lines(ts(predict(fm1.nile),start=1996,freq=1), col='darkgreen',lwd=2)
par(opar)

brekpn <- data.frame('year' = df$year,
                     'order' = as.numeric(ts(predict(fm1.nile),start=1996,freq=1)))

g5 <- ggplot(df) +
  geom_line(aes(year, AF)) +
  geom_point(aes(year, AF)) +
  ylab('Anoxic Factor (days per season)') + xlab('') +
  geom_hline(yintercept=mean(AF), col = col.pre) +
  geom_vline(xintercept=2010, linetype = 'dashed') +
  geom_line(data = brekpn, aes(year, order), col = col.post) +
  theme_bw(); g5

plt1 <- (g5 / g6 / g7  / g10 /g8  /g12 /g13/ g14 ) 
plt2 <-  (p1 / p2 /p3 /p6 /p4  / p8  / p9 /p10)

plt1 | plt2 +plot_layout(guides = 'collect',widths = c(500, 1))

plt1 <- (g5 + ggtitle("A") +  p1)  + plot_layout(guides = 'collect',widths = c(2, 1)) #anoxic
plt2 <- (g6 + ggtitle("F")+ p2)  + plot_layout(guides = 'collect',widths = c(2, 1)) # strat
plt3 <- (g7 + ggtitle("C")+ p3)  + plot_layout(guides = 'collect',widths = c(2, 1)) # do flux
plt4 <- (g10 + ggtitle("E")+ p6)  + plot_layout(guides = 'collect',widths = c(2, 1)) # clear
plt5 <- (g8 + ggtitle("D")+ p4)  + plot_layout(guides = 'collect',widths = c(2, 1)) # biomass
plt6 <- (g12 + ggtitle("G")+ p8)  + plot_layout(guides = 'collect',widths = c(2, 1)) # P
plt7 <- (g13 + ggtitle("H")+ p9)  + plot_layout(guides = 'collect',widths = c(2, 1)) # N
plt8 <- (g14 + ggtitle("I")+ p10) + plot_layout(guides = 'collect',widths = c(2, 1)) # Si
plt9 <- (g15 + ggtitle("B")+ p11) + plot_layout(guides = 'collect',widths = c(2, 1)) # spiny
plt10 <- (g11 + ggtitle("J")+ p7) + plot_layout(guides = 'collect',widths = c(2, 1)) # pH



fig.plt <- (plt1 | plt9) / (plt3 | plt5) / (plt4 | plt2) / (plt6 | plt7) ; fig.plt

ggsave(plot = fig.plt , '../figs_publication/Fig1.png', dpi = 300, units = 'in', width = 17, height = 13)

