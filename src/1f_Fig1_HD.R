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
# library(caret)
library(relaimpo)
library(corrplot)
library(RColorBrewer)

# Load Files
strat <- read_csv('data_processed/stratification.csv')
ssi <- read_csv('data_processed/ssi.csv')
anoxic <- read_csv("data_processed/anoxicfactor.csv")
fluxes <- read_csv('data_processed/dosinks.csv')
biomass <- read_csv('data_processed/3c_biomass_duration.csv')
discharge <- read_csv('data_processed/discharge.csv')
cw <- read_csv('data_processed/clearwater.csv')#readRDS('data_processed/yearly_clearwater_stats.rds')
nutrients <- read_csv('data_processed/nutrients.csv')
spiny <- read_csv('data_processed/spiny.csv')
physics <- read_csv('data_processed/physical_timings.csv')
rainfall <- read_csv('data_processed/precipitation.csv')
zoops <- read_csv('data_processed/0r_annual_zoop_biomass.csv')
phyto <- read_csv('data_processed/3a_phyto_annual_average_biomass.csv')
stoicho <- read_csv('data_processed/stoichiometry.csv')

# Define colors
col.pre <- "steelblue"
col.post <- "orange3"

df.strat <- strat
df.physics <- physics
df.ssi <- ssi %>%
  rename(St = max.ssi)

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
  dplyr::filter(year > 1995)

df.spiny <- spiny %>%
  dplyr::filter(year != 1995 & year != 2021) %>%
  rename(year = year, Spiny = mean)

df.rainfall <- rainfall %>%
  dplyr::filter(wateryear >= 1995 & wateryear <= 2021) %>%
  dplyr::select(c(wateryear, cumsum_pp)) %>%
  rename(year = wateryear, CumPP = cumsum_pp)

df.zoops <- zoops %>%
  dplyr::filter(year4 >= 1995 & year4 <= 2021) %>%
  rename(year = year4, Mendotae = `Daphnia Mendotae`, Pulicaria =  `Daphnia Pulicaria`, Bythrophes = `Bythotrephes Longimanus`) %>%
  dplyr::select(year, tot.zoops.no.SWF, Mendotae, Pulicaria, Bythrophes, Daphnia)

df.phyto <- phyto %>%
  dplyr::filter(Year >= 1995 & Year <= 2021) %>%
  rename(year = Year) %>%
  dplyr::select(year, Bacillariophyta, Cyanophyta)

df.stoicho <- stoicho %>%
  dplyr::filter(year >= 1995 & year <= 2021)

# Merge dataframes
df <- merge(df.strat, df.anoxic, by = 'year')
df <- merge(df, df.flux, by = 'year')
df <- merge(df, df.biomass, by = 'year')
df <- merge(df, df.discharge, by = 'year')
df <- merge(df, df.cw, by = 'year')
df <- merge(df, df.nutrients, by = 'year')
df <- merge(df, df.spiny, by = 'year')
df <- merge(df, df.physics, by = 'year')
df <- merge(df, df.ssi, by = 'year')
df <- merge(df, df.rainfall, by = 'year')
df <- merge(df, df.zoops, by = 'year')
df <- merge(df, df.phyto, by = 'year')

df <- merge(df, df.stoicho, by = 'year')

df = df %>%
  mutate(stoch_surf_summer = ( NO3.NO2.N_surf /1000 * 14) / ( PO4.P_surf / 1000 *31 ),
         stoch_bottom_summer = ( NO3.NO2.N_bot /1000 * 14) / ( PO4.P_bot / 1000 *31 ))


df = df %>% dplyr::filter(year < 2015)

# Define colors
cool.col <- c("#00AFBB", "#E7B800", "#FC4E07")

# Function to plot timeseries
plotG <- function(df, var, ylabs, ylimit) {
  g1 = ggplot(df) +
    geom_line(aes_string('year', var), size = 0.3) +
    geom_point(aes_string('year', var), size = 1) +
    ylab(ylabs) + xlab('') +
    ylim(ylimit) +
    expand_limits(y = ylimit) +
    geom_vline(xintercept=2010, linetype = 'dashed') +
    theme_bw(base_size = 8) +
    theme(axis.title.x = element_blank())
  return(g1)
}

g5 = plotG(df, 'AF', 'Anoxic factor (days)', ylimit = c(40,80))
g8 = plotG(df, 'Days.1.mg.L', 'Biomass > 1.0 mg/L (days)', ylimit = c(70,310))
g10 = plotG(df, 'Clearwater.Duration', 'CWP (meter-days)', ylimit = c(150,550))
g11 = plotG(df, 'pH', 'pH', ylimit = c(0,15))
g12 = plotG(df, 'PO4.P_surf', 'SRP (mg/L)', ylimit = c(0,0.35)) +
  geom_line(aes(year, PO4.P_bot), linetype = 'dashed', size = 0.3) +
  geom_point(aes(year, PO4.P_bot), size = 1)
g13 = plotG(df, 'NO3.NO2.N_surf', 'NO3-NO2-N (mg/L)', ylimit = c(0,0.8)) +
  geom_line(aes(year, NO3.NO2.N_bot ), linetype = 'dashed', size = 0.3) +
  geom_point(aes(year, NO3.NO2.N_bot), size = 1)
g14 = plotG(df, 'RSi', 'React. Silica (mg/L)', ylimit = c(0,15))
g15 = plotG(df, 'Spiny', 'Spiny water flea (counts)', ylimit = c(0,15))
g16 = plotG(df, 'ice_duration', 'Ice duration (days)', ylimit = c(25,130))


g6 = plotG(df, 'linear','Stratification (days)', ylimit = c(120,250)) +
  geom_ribbon(aes(x = year, ymin = constant.low, ymax = constant.high), fill = 'grey80') +
  geom_line(aes(x = year, y = linear), size = 0.3) +
  geom_point(aes(x = year, y = linear), size = 1) +
  geom_line(aes(x = year, y = spline), linetype = 2, color = 'red3', size = 0.3)

g7 = plotG(df, 'Jz', 'DO flux (mg/L/d)', ylimit = c(0.1,0.25) ) #+ #expression("Volumetric flux ["*g~m^{-3}*d^{-1}*"]")
  # geom_smooth(aes(year, Jv, col = 'black'), method = "loess", size = 0.3, alpha = 0.2) +
  # geom_line(aes(year, Ja , col = 'gold'), size = 0.3) +
  # geom_point(aes(year, Ja , col = 'gold'), size = 1) +
  # geom_smooth(aes(year, Ja , col = 'gold'), method = "loess", size = 0.3, alpha = 0.2) +
  # scale_color_identity(guide = "legend", labels = c('Vol.','Areal')) +
  # theme(legend.position = c(0.2,0.9),
  #       legend.text = element_text(size = 6),
  #       legend.title = element_blank(),
  #       legend.background = element_rect(fill = "transparent"),
  #       legend.key.height = unit(0.1, 'cm'))
g17 = plotG(df, 'St', 'Schmidt stability (J/m2)', ylimit = c(500,950)) #bquote('Number VS'~Number^2)
g17 = plotG(df, 'St', bquote('Schmidt stability (J/'~m^2~')'), ylimit = c(500,950)) #bquote('Number VS'~Number^2)
g18 = plotG(df, 'CumPP', 'Precipitation (mm)', ylimit = c(600,1300))
g19 = plotG(df, 'sum.discharge', bquote('Discharge ('~m^3~'/d)'), ylimit = c(5700,18000))
g20 = plotG(df, 'Mendotae', 'D. mendotae (mg/L)', ylimit = c(0,30))
g21 = plotG(df, 'Pulicaria', 'D. pulicaria (mg/L)', ylimit = c(0,90))
g22 = plotG(df, 'Bythrophes', 'Spiny water flea (mg/L)', ylimit = c(0,300))
g23 = plotG(df, 'Bacillariophyta', 'Diatoms (mg/L)', ylimit = c(0,3))
g24 = plotG(df, 'Cyanophyta', 'Cyanobacteria (mg/L)', ylimit = c(0,5))
g25 = plotG(df, 'stoch_surf ', 'N:P (molar)', ylimit = c(0,5)) +
  geom_line(aes(year, stoch_bottom_summer  ), linetype = 'dashed', size = 0.3) +
  geom_point(aes(year, stoch_bottom_summer ), size = 1)
g26 = plotG(df, 'stoch_surf ', 'N:P (molar)', ylimit = c(0,7.5)) +
  geom_line(aes(year, stoch_bottom  ), linetype = 'dashed', size = 0.3) +
  geom_point(aes(year, stoch_bottom ), size = 1)
g27 = plotG(df, 'stoch_surf ', 'N:P (molar)', ylimit = c(0,7.5))

library(ggpubr)
df.prior = df %>%
  mutate('class' = ifelse(year < 2010, 'prior 2010','post 2010')) %>%
  dplyr::select(class, AF, strat_duration, Jz, Jv, Days.1.mg.L, discharge, ice_duration, Clearwater.Duration, pH, PO4.P_surf, NO3.NO2.N_surf, RSi, Spiny,
                St, CumPP, sum.discharge, Bythrophes, Pulicaria, Mendotae, Bacillariophyta, Cyanophyta,
                stoch_bottom, stoch_surf)
m.df.prior <- reshape2::melt(df.prior, id = 'class')

compare_means(value ~ class, data = m.df.prior %>% dplyr::filter(variable == 'AF'))
compare_means(value ~ class, data =  m.df.prior %>% dplyr::filter(variable == 'AF'), method ="kruskal.test")

# Function to plot boxplots
plotBP <- function(var, ylabs) {
  p1 <- ggboxplot(data = m.df.prior %>% dplyr::filter(variable == var), x = "class", y = "value",
                  xlab = '', ylab = ylabs, fill = 'class', palette = c(col.pre, col.post), #palette = "jco",
                  add = "jitter", size = 0.2, add.params = list(size = 0.8, shape = 21)) +
    # scale_y_continuous(expand = expansion(mult = c(0.05,0.2)), limits = ylimit) +
    # expand_limits(y = ylimit) +
    scale_x_discrete(labels = c('< 2010', '\u2265 2010'))
  #  Add p-value
  p1 = p1 + stat_compare_means(label = 'p.format', size = 2, vjust = -1) +
    theme_classic(base_size = 8) +
    theme(legend.position = 'none',
          axis.title.y = element_blank(),
          axis.title.x = element_blank())
  return(p1)
}

p1 = plotBP('AF',''); p1
p2 = plotBP('strat_duration', '')
p3 = plotBP('Jz', '')
p4 = plotBP('Days.1.mg.L', '')
p6 = plotBP('Clearwater.Duration', '')
p8 = plotBP('PO4.P_surf', '')
p9 = plotBP('NO3.NO2.N_surf', '')
p11 = plotBP('Spiny', '')
p10 = plotBP('RSi', '')
p12 = plotBP('ice_duration', '')
p13 = plotBP('St', '')
p14 = plotBP('CumPP', '')
p15 = plotBP('sum.discharge', '')
p16 = plotBP('Mendotae', '')
p17 = plotBP('Pulicaria', '')
p18 = plotBP('Bythrophes', '')
p19 = plotBP('Bacillariophyta', '')
p20 = plotBP('Cyanophyta', '')
p22 = plotBP('stoch_surf', '')

# compare_means(value ~ class, data = m.df.prior %>% dplyr::filter(variable == 'AF'))
# m.df.prior %>% dplyr::filter(variable == 'AF') %>% dplyr::group_by(class) %>% summarise(mean = mean(value), sd = sd(value))
# compare_means(value ~ class, data =  m.df.prior %>% dplyr::filter(variable == 'AF'), method ="kruskal.test")

# compare_means(value ~ class, data = m.df.prior %>% dplyr::filter(variable == 'med'))
# compare_means(value ~ class, data =  m.df.prior %>% dplyr::filter(variable == 'med'), method ="kruskal.test")
#
compare_means(value ~ class, data = m.df.prior %>% dplyr::filter(variable == 'Jv'))
compare_means(value ~ class, data =  m.df.prior %>% dplyr::filter(variable == 'Jv'), method ="kruskal.test")

m.df.prior %>% dplyr::filter(variable == 'Jv') %>% dplyr::group_by(class) %>% summarise(mean = mean(value), sd = sd(value))

sddef = m.df.prior %>% dplyr::filter(variable == 'Jv')%>% group_by(class) %>% summarise(mean = mean(value),
                                                                                sdv= sd(value))
sddef$mean[1] - sddef$mean[2]
sqrt(sddef$sdv[1]**2 - sddef$sdv[2]**2)

sddef = m.df.prior %>% dplyr::filter(variable == 'Bacillariophyta')%>% group_by(class) %>% summarise(mean = mean(value),
                                                                                        sdv= sd(value))
sddef$mean[1] - sddef$mean[2]
sqrt(sddef$sdv[1]**2 - sddef$sdv[2]**2)

sddef = m.df.prior %>% dplyr::filter(variable == 'Mendotae')%>% group_by(class) %>% summarise(mean = mean(value),
                                                                                                     sdv= sd(value))
sddef$mean[1] - sddef$mean[2]
sqrt(sddef$sdv[1]**2 - sddef$sdv[2]**2)

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
# library(strucchange)
library(xts)

ts.af =  ts(df$AF, start= 1996, frequency = 1)
plot(ts.af)
#
# plot(merge(
#        AF = as.zoo(ts.af),
#        zoo(mean(AF), time(AF)),
#        CUSUM = cumsum(AF - mean(AF)),
#        zoo(0, time(AF)),
#        MOSUM = rollapply(AF - mean(AF), 4, sum),
#        zoo(0, time(AF))
#      ), screen = c(1, 1, 2, 2, 3, 3), main = "", xlab = "Time",
#   col = c(1, 4, 1, 4, 1, 4) )
#
# plot(merge(
#         AF = as.zoo(ts.af),
#         zoo(c(NA, cumsum(head(AF, -1))/1:99), time(AF)),
#         CUSUM = cumsum(c(0, recresid(lm(AF ~ 1)))),
#         zoo(0, time(AF))
#       ), screen = c(1, 1, 2, 2), main = "", xlab = "Time",
#    col = c(1, 4, 1, 4) )

AF = as.zoo(ts.af)
# plot(1996 + 4:25, sapply(4:25, function(i) {
#   before <- 1:i
#   after <- (i+1):4
#   res <- c(AF[before] - mean(AF[before]), AF[after] - mean(AF[after]))
#    sum(res^2)
#    }), type = "b", xlab = "Time", ylab = "RSS")

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
g5 <- plotG(df, 'AF', 'Anoxic factor (days)', ylimit = c(40,80)) +
  geom_vline(xintercept = 2010, linetype = 'dashed', size = 0.4) +
  geom_line(data = brekpn, aes(year, order), col = col.post, size = 0.4)


plt1 <- (g5 + ggtitle("A)") + p1)  + plot_layout(widths = c(2, 1)) & scale_y_continuous(limits = layer_scales(g5)$y$range$range, expand = expansion(mult = c(0.05,0.2))) #anoxic
plt2 <- (g6 + ggtitle("J)")+ p2) + plot_layout(widths = c(2, 1)) & scale_y_continuous(limits = layer_scales(g6)$y$range$range, expand = expansion(mult = c(0.05,0.2))) # strat
plt3 <- (g7 + ggtitle("B)")+ p3) + plot_layout(widths = c(2, 1)) & scale_y_continuous(limits = layer_scales(g7)$y$range$range, expand = expansion(mult = c(0.05,0.2))) # do flux
plt4 <- (g10 + ggtitle("C)")+ p6) + plot_layout(widths = c(2, 1)) & scale_y_continuous(limits = layer_scales(g10)$y$range$range, expand = expansion(mult = c(0.05,0.2))) # clear
plt5 <- (g8 + ggtitle("G)")+ p4) + plot_layout(widths = c(2, 1)) & scale_y_continuous(limits = layer_scales(g8)$y$range$range, expand = expansion(mult = c(0.05,0.2))) # biomass
plt6 <- (g12 + ggtitle("M)")+ p8) + plot_layout(widths = c(2, 1)) & scale_y_continuous(limits = layer_scales(g12)$y$range$range, expand = expansion(mult = c(0.05,0.2))) # P
plt9 <- (g15 + ggtitle("D)")+ p11) + plot_layout(widths = c(2, 1)) & scale_y_continuous(limits = layer_scales(g15)$y$range$range, expand = expansion(mult = c(0.05,0.2))) # Spiny
plt10 <- (g16 + ggtitle("L)")+ p12) + plot_layout(widths = c(2, 1)) & scale_y_continuous(limits = layer_scales(g16)$y$range$range, expand = expansion(mult = c(0.05,0.2))) # Ice
# plt7 <- (g13 + ggtitle("H)")+ p9) + plot_layout(widths = c(2, 1)) # N
# plt8 <- (g14 + ggtitle("I)")+ p10) + plot_layout(widths = c(2, 1)) # Si
# plt10 <- (g11 + ggtitle("J)")+ p7) + plot_layout(widths = c(2, 1)) # pH
plt11 <- (g17 + ggtitle("K)")+ p13) + plot_layout(widths = c(2, 1)) & scale_y_continuous(limits = layer_scales(g17)$y$range$range, expand = expansion(mult = c(0.05,0.2))) # Schmidt
plt12 <- (g18 + ggtitle("A)")+ p14) + plot_layout(widths = c(2, 1)) & scale_y_continuous(limits = layer_scales(g18)$y$range$range, expand = expansion(mult = c(0.05,0.2))) # Rainfall
plt13 <- (g13 + ggtitle("N)")+ p9) + plot_layout(widths = c(2, 1)) & scale_y_continuous(limits = layer_scales(g13)$y$range$range, expand = expansion(mult = c(0.05,0.2))) # NO3
plt14 <- (g19 + ggtitle("B)")+ p15) + plot_layout(widths = c(2, 1)) & scale_y_continuous(limits = layer_scales(g19)$y$range$range, expand = expansion(mult = c(0.05,0.2))) # Discharge

plt15 <- (g20 + ggtitle("E)")+ p16) + plot_layout(widths = c(2, 1)) & scale_y_continuous(limits = layer_scales(g20)$y$range$range, expand = expansion(mult = c(0.05,0.2))) # mendotae
plt16 <- (g21 + ggtitle("F)")+ p17) + plot_layout(widths = c(2, 1)) & scale_y_continuous(limits = layer_scales(g21)$y$range$range, expand = expansion(mult = c(0.05,0.2))) # pulicaria
plt17 <- (g22 + ggtitle("D)")+ p18) + plot_layout(widths = c(2, 1)) & scale_y_continuous(limits = layer_scales(g22)$y$range$range, expand = expansion(mult = c(0.05,0.2))) # bythrophes

plt18 <- (g23 + ggtitle("H)")+ p19) + plot_layout(widths = c(2, 1)) & scale_y_continuous(limits = layer_scales(g23)$y$range$range, expand = expansion(mult = c(0.05,0.2))) # diatoms
plt19 <- (g24 + ggtitle("I)")+ p20) + plot_layout(widths = c(2, 1)) & scale_y_continuous(limits = layer_scales(g24)$y$range$range, expand = expansion(mult = c(0.05,0.2))) # cyanos

plt20 <- (g26 + ggtitle("O)")+ p22) + plot_layout(widths = c(2, 1)) & scale_y_continuous(limits = layer_scales(g26)$y$range$range, expand = expansion(mult = c(0.05,0.2))) # stoich

plt21 <- (g27 + ggtitle("A)")+ p22) + plot_layout(widths = c(2, 1)) & scale_y_continuous(limits = layer_scales(g27)$y$range$range, expand = expansion(mult = c(0.05,0.2))) # stoich

# Final figure
fig.plt <- (plt1 | plt9) / (plt3 | plt5) / (plt3 | plt5) / (plt5 | plt4) / (plt2 | plt11)/ (plt6 | plt13) / (plt12 | plt14)&
  theme(plot.title = element_text(size = 7, face = "bold"))

ggsave(plot = fig.plt , 'figs_publication/Fig1a.png', dpi = 500, units = 'in', width = 6.5, height = 8.5)

fig.plt <- (plt1 | plt3 | plt4) / (plt17 | plt15 | plt16   ) / ( plt5 | plt18 | plt19)  / (plt2 | plt11 | plt10) / (plt6 | plt13 | plt20 )&
  theme(plot.title = element_text(size = 7, face = "bold"))
ggsave(plot = fig.plt , 'figs_publication/Fig1a_3x4_si.png', dpi = 500, units = 'in', width = 9.5, height = 7)
# ggsave(plot = fig.plt , 'figs_publication/Fig1a.png', dpi = 500, units = 'in', width = 6.5, height = 8.5)

fig.plt <- (plt12 | plt14 ) &
  theme(plot.title = element_text(size = 7, face = "bold"))
ggsave(plot = fig.plt , 'figs_publication/SI_Fig2.png', dpi = 500, units = 'in', width = 9, height = 2)
plt12
plt14

