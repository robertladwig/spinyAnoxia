# Load libraries
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

# Load Files
strat <- read_csv('data_processed/physical_timings.csv')
anoxic <- read_csv("data_processed/anoxicfactor.csv")
fluxes <- read_csv('data_processed/dosinks.csv')
biomass <- read_csv('data_processed/3c_biomass_duration.csv')
discharge <- read_csv('data_processed/discharge.csv')
cw <- readRDS('data_processed/yearly_clearwater_stats.rds')
nutrients <- read_csv('data_processed/nutrients.csv')
spiny <- read_csv('data_processed/spiny.csv')

# Define colors 
col.pre <- "steelblue"
col.post <- "orange3"

df.strat <- strat

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

# Merge dataframes 
df <- merge(df.strat, df.anoxic, by = 'year')
df <- merge(df, df.flux, by = 'year')
df <- merge(df, df.biomass, by = 'year')
df <- merge(df, df.discharge, by = 'year')
df <- merge(df, df.cw, by = 'year')
df <- merge(df, df.nutrients, by = 'year')
df <- merge(df, df.spiny, by = 'year')

# Define colors
cool.col <- c("#00AFBB", "#E7B800", "#FC4E07")

# Function to plot timeseries 
plotG <- function(df, var, ylabs) {
  g1 = ggplot(df) +
    geom_line(aes_string('year', var), size = 0.3) +
    geom_point(aes_string('year', var), size = 1) +
    ylab(ylabs) + xlab('') +
    geom_vline(xintercept=2010, linetype = 'dashed') +
    theme_bw(base_size = 8) +
    theme(axis.title.x = element_blank())
  return(g1)
}

g5 = plotG(df, 'AF', 'Anoxic Factor (days)')
g8 = plotG(df, 'Days.0.5.mg.L', 'Biomass > 0.5 mg/L (days)')
g10 = plotG(df, 'Clearwater.Duration', 'CWP (days)')
g11 = plotG(df, 'pH', 'pH')
g12 = plotG(df, 'PO4.P_surf', 'SRP (mg/L)') +
  geom_line(aes(year, PO4.P_bot), linetype = 'dashed', size = 0.3) +
  geom_point(aes(year, PO4.P_bot), size = 1)
g13 = plotG(df, 'NO3.NO2.N_surf', 'NO3-NO2-N (mg/L)') +
  geom_line(aes(year, NO3.NO2.N_bot ), linetype = 'dashed', size = 0.3) +
  geom_point(aes(year, NO3.NO2.N_bot), size = 1)
g14 = plotG(df, 'RSi', 'React. Silica (mg/L)')
g15 = plotG(df, 'Spiny', 'Spiny Waterflea (counts)')
g16 = plotG(df, 'ice_duration', 'Ice duration (days)')

g6 = plotG(df, 'linear','Days Stratified') +
  geom_ribbon(aes(x = year, ymin = constant.low, ymax = constant.high), fill = 'grey80') +
  geom_line(aes(x = year, y = linear), size = 0.3) +
  geom_point(aes(x = year, y = linear), size = 1) +
  geom_line(aes(x = year, y = spline), linetype = 2, color = 'red3', size = 0.3)

g7 = plotG(df, 'Jz', 'Vol. & Areal Fluxes' ) + #expression("Volumetric flux ["*g~m^{-3}*d^{-1}*"]")
  geom_smooth(aes(year, Jz, col = 'black'), method = "loess", size = 0.3, alpha = 0.2) +
  geom_line(aes(year, Ja , col = 'gold'), size = 0.3) +
  geom_point(aes(year, Ja , col = 'gold'), size = 1) +
  geom_smooth(aes(year, Ja , col = 'gold'), method = "loess", size = 0.3, alpha = 0.2) +
  scale_color_identity(guide = "legend", labels = c('Vol.','Areal')) +
  theme(legend.position = c(0.2,0.9),
        legend.text = element_text(size = 6),
        legend.title = element_blank(), 
        legend.background = element_rect(fill = "transparent"),
        legend.key.height = unit(0.1, 'cm'))

library(ggpubr)
df.prior = df %>%
  mutate('class' = ifelse(year < 2010, 'prior 2010','post 2010')) %>%
  dplyr::select(class, AF, strat_duration, Jz, Jv, Days.0.5.mg.L, discharge, ice_duration, Clearwater.Duration, pH, PO4.P_surf, NO3.NO2.N_surf, RSi, Spiny)
m.df.prior <- reshape2::melt(df.prior, id = 'class')

compare_means(value ~ class, data = m.df.prior %>% dplyr::filter(variable == 'AF'))
compare_means(value ~ class, data =  m.df.prior %>% dplyr::filter(variable == 'AF'), method ="kruskal.test")

# Function to plot boxplots 
plotBP <- function(var, ylabs) {
  p1 <- ggboxplot(data = m.df.prior %>% dplyr::filter(variable == var), x = "class", y = "value",
                  xlab = '', ylab = ylabs, fill = 'class', palette = c(col.pre, col.post), #palette = "jco", 
                  add = "jitter", size = 0.2, add.params = list(size = 0.8)) +
    scale_y_continuous(expand = expansion(mult = c(0.05,0.2))) +
    scale_x_discrete(labels = c('< 2010', '> 2010'))
  #  Add p-value
  p1 = p1 + stat_compare_means(label = 'p.format', size = 2, vjust = -1) + 
    theme_classic(base_size = 8) +
    theme(legend.position = 'none',
          axis.title.x = element_blank())
  return(p1)
}


p1 = plotBP('AF','Anoxic Factor')
p2 = plotBP('med', 'Days Stratified')
p3 = plotBP('Jz', 'Total oxygen sink')
p4 = plotBP('Days.0.5.mg.L', 'Biomass > 0.5 mg/L')
p6 = plotBP('Clearwater.Duration', 'CWP')
p8 = plotBP('PO4.P_surf', 'SRP surf')
p9 = plotBP('NO3.NO2.N_surf', 'NO3-NO2-N surf')
p11 = plotBP('Spiny', 'Spiny Waterflea')
p10 = plotBP('RSi', 'RSi')
p11 = plotBP('ice_duration', 'Ice period')


# compare_means(value ~ class, data = m.df.prior %>% dplyr::filter(variable == 'med'))
# compare_means(value ~ class, data =  m.df.prior %>% dplyr::filter(variable == 'med'), method ="kruskal.test")
# 
# compare_means(value ~ class, data = m.df.prior %>% dplyr::filter(variable == 'Jz'))
# compare_means(value ~ class, data =  m.df.prior %>% dplyr::filter(variable == 'Jz'), method ="kruskal.test")
# 
# sddef = m.df.prior %>% dplyr::filter(variable == 'Jz')%>% group_by(class) %>% summarise(mean = mean(value),
#                                                                                 sdv= sd(value))
# sddef$mean[1] - sddef$mean[2]
# sqrt(sddef$sdv[1]**2 - sddef$sdv[2]**2)
# 
# compare_means(value ~ class, data = m.df.prior %>% dplyr::filter(variable == 'Days.0.5.mg.L'))
# compare_means(value ~ class, data =  m.df.prior %>% dplyr::filter(variable == 'Days.0.5.mg.L'), method ="kruskal.test")
# 
# sddef = m.df.prior %>% dplyr::filter(variable == 'Days.0.5.mg.L')%>% group_by(class)%>% summarise(mean = mean(value),
#                                                                                           sdv= sd(value))
# sddef$mean[1] - sddef$mean[2]
# sqrt(abs(sddef$sdv[1]**2 - sddef$sdv[2]**2))
# 
# compare_means(value ~ class, data = m.df.prior %>% dplyr::filter(variable == 'discharge'))
# compare_means(value ~ class, data =  m.df.prior %>% dplyr::filter(variable == 'discharge'), method ="kruskal.test")
# 
# p5 <- ggboxplot( m.df.prior %>% dplyr::filter(variable == 'discharge'), x = "class", y = "value",
#                  palette = "jco", xlab = '', ylab = 'Discharge',fill = c(col.pre, col.post),
#                  add = "jitter")
# #  Add p-value
# p5 = p5 + stat_compare_means(label = 'p.format') + theme_bw(base_size = 8)
# 
# compare_means(value ~ class, data = m.df.prior %>% dplyr::filter(variable == 'Clearwater.Duration'))
# compare_means(value ~ class, data =  m.df.prior %>% dplyr::filter(variable == 'Clearwater.Duration'), method ="kruskal.test")
# 
# sddef = m.df.prior %>% dplyr::filter(variable == 'Clearwater.Duration')%>% group_by(class) %>% summarise(mean = mean(value),
#                                                                                                  sdv= sd(value))
# sddef$mean[1] - sddef$mean[2]
# sqrt(abs(sddef$sdv[1]**2 - sddef$sdv[2]**2))
# 
# compare_means(value ~ class, data = m.df.prior %>% dplyr::filter(variable == 'pH'))
# compare_means(value ~ class, data =  m.df.prior %>% dplyr::filter(variable == 'pH'), method ="kruskal.test")
# 
# p7 <- ggboxplot( m.df.prior %>% dplyr::filter(variable == 'pH'), x = "class", y = "value",
#                  palette = "jco", xlab = '', ylab = 'pH',fill = c(col.pre, col.post),
#                  add = "jitter")
# #  Add p-value
# p7 = p7 + stat_compare_means(label = 'p.format') + theme_bw(base_size = 8)
# 
# compare_means(value ~ class, data = m.df.prior %>% dplyr::filter(variable == 'PO4.P_surf'))
# compare_means(value ~ class, data =  m.df.prior %>% dplyr::filter(variable == 'PO4.P_surf'), method ="kruskal.test")
# 
# compare_means(value ~ class, data = m.df.prior %>% dplyr::filter(variable == 'NO3.NO2.N_surf'))
# compare_means(value ~ class, data =  m.df.prior %>% dplyr::filter(variable == 'NO3.NO2.N_surf'), method ="kruskal.test")
# 
# compare_means(value ~ class, data = m.df.prior %>% dplyr::filter(variable == 'RSi'))
# compare_means(value ~ class, data =  m.df.prior %>% dplyr::filter(variable == 'RSi'), method ="kruskal.test")
# 
# compare_means(value ~ class, data = m.df.prior %>% dplyr::filter(variable == 'Spiny'))
# compare_means(value ~ class, data =  m.df.prior %>% dplyr::filter(variable == 'Spiny'), method ="kruskal.test")



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


# opar <- par(mfrow=c(2,1), mar=c(2,2,0,2))
# plot(ocus.nile, alt.boundary=F,main="")
# abline(v= 2010, lty=2, col='red')
# plot(AF, ylab="Annual Flow of the river Nile") > abline(h= mean(AF),col='blue')
# abline(v= 2010, lty=2, col='red')
# lines(ts(predict(fm1.nile),start=1996,freq=1), col='darkgreen',lwd=2)
# par(opar)

brekpn <- data.frame('year' = df$year,
                     'order' = as.numeric(ts(predict(fm1.nile),start=1996,freq=1)))

# Update g5 with breakpoints
g5 <- plotG(df, 'AF', 'Anoxic Factor (days)') +
  geom_vline(xintercept = 2010, linetype = 'dashed', size = 0.4) +
  geom_line(data = brekpn, aes(year, order), col = col.post, size = 0.4)

plt1 <- (g5 + ggtitle("A)") + p1) + plot_layout(widths = c(2, 1)) #anoxic
plt2 <- (g6 + ggtitle("F)")+ p2) + plot_layout(widths = c(2, 1)) # strat
plt3 <- (g7 + ggtitle("C)")+ p3) + plot_layout(widths = c(2, 1)) # do flux
plt4 <- (g10 + ggtitle("E)")+ p6) + plot_layout(widths = c(2, 1)) # clear
plt5 <- (g8 + ggtitle("D)")+ p4) + plot_layout(widths = c(2, 1)) # biomass
plt6 <- (g12 + ggtitle("G)")+ p8) + plot_layout(widths = c(2, 1)) # P
plt7 <- (g13 + ggtitle("H)")+ p9) + plot_layout(widths = c(2, 1)) # N
# plt8 <- (g14 + ggtitle("I)")+ p10) + plot_layout(widths = c(2, 1)) # Si
plt9 <- (g15 + ggtitle("B)")+ p11) + plot_layout(widths = c(2, 1)) # spiny
# plt10 <- (g11 + ggtitle("J)")+ p7) + plot_layout(widths = c(2, 1)) # pH

# Final figure
fig.plt <- (plt1 | plt9) / (plt3 | plt5) / (plt4 | plt2) / (plt6 | plt7) &
  theme(plot.title = element_text(size = 7, face = "bold"))

ggsave(plot = fig.plt , 'figs_publication/Fig1a.png', dpi = 300, units = 'in', width = 6.5, height = 6)

