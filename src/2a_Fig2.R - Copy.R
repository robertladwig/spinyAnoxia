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

# PLOT: correlation plot for importrant predictors
# png(file = "results/Fig_Correlation.png",res = 300,width = 216,height = 216, units = 'mm')
# c.plot <- corrplot(res, type = "upper", order = "alphabet", addCoef.col = "black",
#                    tl.col = "black", tl.srt = 45, method = 'color', sig.level = 0.001, insig = "blank",
#                    col=(brewer.pal(n=8, name="RdYlBu")));c.plot
# dev.off()


strat <- read_csv('../output/stratification.csv')

anoxic <- read_csv("../output/anoxicfactor.csv")

fluxes <- read_csv('../output/dosinks.csv')

biomass <- read_csv('../output/biomass_duration.csv')

discharge <- read_csv('../output/discharge.csv')

cw <- readRDS('../data/yearly_clearwater_stats.rds')

nutrients <- read_csv('../output/nutrients.csv')

spiny <- read_csv('../output/spiny.csv')


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

mod <- lm(AF ~ med + Days.0.5.mg.L + Jz, data = df)
summary(mod)

boruta_output <- Boruta(AF ~ med + Jz + Jv + Ja + 
                          Days.0.5.mg.L + Days.1.mg.L + Days.1.5.mg.L +
                          Days.2.mg.L + Days.3.mg.L +
                          discharge + max.discharge + min.discharge +
                          Clearwater.Duration + Prev.Winter.Duration + Max.Clearwater.Depth.m + Spiny +
                          pH + PO4.P_surf+ PO4.P_bot+ NO3.NO2.N_surf+ NO3.NO2.N_bot+ RSi, 
                        data = df, doTrace=2,
                        maxRuns = 1e5)  # perform Boruta search
boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])  # collect Confirmed and Tentative variables
print(boruta_signif)  # significant variables
plot(boruta_output, cex.axis=1.5, las=3, xlab="", main="")  # plot variable importance

final.boruta <- TentativeRoughFix(boruta_output)
print(final.boruta)
plot(final.boruta)
boruta.df <- attStats(final.boruta)
boruta_signif =getSelectedAttributes(final.boruta, withTentative = F)
print(boruta.df)
print(boruta_signif)

idx = which( colnames(df) %in% boruta_signif)
hyp.data = df[,idx]
hyp.data$AF = df$AF
sc.info <- scale(hyp.data)
hyp.data <- as.data.frame(scale(hyp.data))


hypo1 <- lm(AF ~ ., data = hyp.data)
summary(hypo1)
sum.hypo1 <-summary(hypo1)
step(hypo1)

hypo1 <- lm(AF ~ med + Days.0.5.mg.L  + Clearwater.Duration , data = hyp.data)
hypo1 <- lm(AF ~ med + Days.0.5.mg.L + Jz + Clearwater.Duration + Spiny, data = hyp.data)
hypo1 <- lm(AF ~ med + Days.0.5.mg.L + Clearwater.Duration + Spiny + PO4.P_surf  + PO4.P_bot , data = hyp.data)

sum.hypo1 <-summary(hypo1)

AIC(hypo1)
BIC(hypo1)

drop1(hypo1, test = 'F')

relImportance <-calc.relimp(hypo1, type = "lmg", rela = TRUE)
sort(relImportance$lmg, decreasing=TRUE)
boot <- boot.relimp(hypo1, b = 1000, type = c("lmg",
                                              "last", "first", "pratt"), rank = TRUE,
                    diff = TRUE, rela = TRUE)
booteval.relimp(boot, lev =0.9, nodiff=TRUE) # print result

varImp(hypo1, scale = TRUE)


# 2. Regression line + confidence intervals
# modeleq <- paste0('y = ', round(sum.hypo1$coefficients[1,1],2),
#                   ' + ',round(sum.hypo1$coefficients[2,1],2),' Strat.dur',
#                   ' + ',round(sum.hypo1$coefficients[3,1],2),' Days.0.5',
#                   ' + ',round(sum.hypo1$coefficients[4,1],2),' Total.sink',
#                   ' + ',round(sum.hypo1$coefficients[5,1],2),' ClearWat.dur',
#                   ' + ',round(sum.hypo1$coefficients[6,1],2),' SWF',
#                   ' + e, where e ~ N(0,',round(sum.hypo1$sigma,2),")")
modeleq <- paste0('y = ', round(sum.hypo1$coefficients[1,1],2),
                  ' + ',round(sum.hypo1$coefficients[2,1],2),' Strat.dur',
                  ' + ',round(sum.hypo1$coefficients[3,1],2),' Days.0.5',
                  ' + ',round(sum.hypo1$coefficients[4,1],2),' ClearWat.dur',
                  ' + ',round(sum.hypo1$coefficients[5,1],2),' SWF',
                  ' + ',round(sum.hypo1$coefficients[6,1],2),' PO4.surf',
                  ' + ',round(sum.hypo1$coefficients[7,1],2),' PO4.bot',
                  ' + e, where e ~ N(0,',round(sum.hypo1$sigma,2),")")
modeleq1 <- paste0('y = ', round(sum.hypo1$coefficients[1,1],2),
                  ' + ',round(sum.hypo1$coefficients[2,1],2),' Strat.dur',
                  ' + ',round(sum.hypo1$coefficients[3,1],2),' Days.0.5',
                  ' + ',round(sum.hypo1$coefficients[4,1],2),' ClearWat.dur',
                  ' + ')
modeleq2 <- paste0(round(sum.hypo1$coefficients[5,1],2),' SWF',
                  ' + ',round(sum.hypo1$coefficients[6,1],2),' PO4.surf',
                  ' + ',round(sum.hypo1$coefficients[7,1],2),' PO4.bot',
                  ' + e, where e ~ N(0,',round(sum.hypo1$sigma,2),")")
library(latex2exp)

pred.int <- predict(hypo1, interval = "confidence")
pred.int <- pred.int * attr(sc.info, 'scaled:scale')[9] + attr(sc.info, 'scaled:center')[9]

mydata <- cbind(data.frame('AF'  = df$AF), pred.int)

# PLOT: linear model
p <- ggplot(mydata, aes(fit, AF)) +
  stat_smooth(method = lm, col = 'black') +
  geom_point(size = 2) +
  xlab('Predicted Anoxic Factor [d per season]')+
  ylab('Anoxic Factor [d per season]')+
  theme_minimal()+
  # annotate("text", x = 61, y = 82.5, label = modeleq1, size = 3)+
  # annotate("text", x = 61, y = 80, label = modeleq2, size = 3)+
  annotate("text", x = 50, y = 77.5, label = (paste0('R2 = ',round(sum.hypo1$r.squared,2))), size =5) +
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=0, hjust=1));p
p.linear <- p + geom_line(aes(y = lwr), color = "grey", linetype = "dashed")+
  geom_line(aes(y = upr), color = "grey", linetype = "dashed"); p.linear

ggsave(file=paste0('../figs/linearModel.png'), p.linear, dpi = 300,width = 216,height = 216, units = 'mm')



hyp.data2 <- hyp.data[, c(1,2,3,6,7)]


colnames(hyp.data2) = c('Strat.dur', 'Total.sink', 'Days.0.5','ClearWat.dur', 'Anoxic.Factor', "SWF")

res=cor(hyp.data2, method = c("pearson"))


# PLOT: correlation plot for importrant predictors
png(file = "../figs/model.png",res = 300,width = 216,height = 216, units = 'mm')
c.plot <- corrplot(res, type = "upper", order = "alphabet", addCoef.col = "black",
                   tl.col = "black", tl.srt = 45, method = 'color', sig.level = 0.001, insig = "blank",
                   col=(brewer.pal(n=8, name="RdYlBu")));c.plot
dev.off()


my.formula <- y ~ (x)


g1 <- ggplot(df,aes(med, AF)) + 
  geom_point() +
  xlab('Mean stratification duration (d)') + ylab('Anoxic factor (d)') +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ (x)) +
  stat_poly_eq(formula = my.formula,
               aes(label = paste(..rr.label.., sep = "~~~")), # ..eq.label.., ..rr.label..
               parse = TRUE,size = rel(4.5),
               label.y = 0.05,
               label.x = 0.1) +
  theme_bw()
g2 <- ggplot(df,aes(Jv, AF)) + 
  geom_point() +
  xlab('Volumetric sink (g/m3/d))') + ylab('Anoxic factor (d)') +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ (x)) +
  stat_poly_eq(formula = my.formula,
               aes(label = paste(..rr.label.., sep = "~~~")),
               parse = TRUE,size = rel(4.5),
               label.y = 0.05,
               label.x = 0.1) +
  theme_bw()
g3 <- ggplot(df,aes(Ja, AF)) + 
  geom_point() +
  xlab('Areal sink (g/m2/d)') + ylab('Anoxic factor (d)') +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ (x)) +
  stat_poly_eq(formula = my.formula,
               aes(label = paste( ..rr.label.., sep = "~~~")),
               parse = TRUE,size = rel(4.5),
               label.y = 0.05,
               label.x = 0.1) +
  theme_bw()
g4 <- ggplot(df,aes(Jz, AF)) + 
  geom_point() +
  xlab('Total sink (g/m3/d)') + ylab('Anoxic factor (d)') +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ (x)) +
  stat_poly_eq(formula = my.formula,
               aes(label = paste( ..rr.label.., sep = "~~~")),
               parse = TRUE,size = rel(4.5),
               label.y = 0.05,
               label.x = 0.1) +
  theme_bw()
g5 <- ggplot(df,aes(Days.0.5.mg.L, AF)) + 
  geom_point() +
  xlab('Biomass over 0.5 mg/L (d)') + ylab('Anoxic factor (d)') +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ (x)) +
  stat_poly_eq(formula = my.formula,
               aes(label = paste( ..rr.label.., sep = "~~~")),
               parse = TRUE,size = rel(4.5),
               label.y = 0.05,
               label.x = 0.1) +
  theme_bw()
g6 <- ggplot(df,aes(Days.1.5.mg.L, AF)) + 
  geom_point() +
  xlab('Biomass over 1 mg/L (d)') + ylab('Anoxic factor (d)') +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ (x)) +
  stat_poly_eq(formula = my.formula,
               aes(label = paste( ..rr.label.., sep = "~~~")),
               parse = TRUE,size = rel(4.5),
               label.y = 0.05,
               label.x = 0.1) +
  theme_bw()
g7 <- ggplot(df,aes(discharge, AF)) + 
  geom_point() +
  xlab('Yahara Q (cfs)') + ylab('Anoxic factor (d)') +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ (x)) +
  stat_poly_eq(formula = my.formula,
               aes(label = paste(..rr.label.., sep = "~~~")),
               parse = TRUE,size = rel(4.5),
               label.y = 0.05,
               label.x = 0.1) +
  theme_bw()
g8 <- ggplot(df,aes(Clearwater.Duration, AF)) + 
  geom_point() +
  xlab('Clearwater duration') + ylab('Anoxic factor (d)') +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ (x)) +
  stat_poly_eq(formula = my.formula,
               aes(label = paste( ..rr.label.., sep = "~~~")),
               parse = TRUE,size = rel(4.5),
               label.y = 0.05,
               label.x = 0.1) +
  theme_bw()

g9 <- ggplot(df,aes(Spiny, AF)) + 
  geom_point() +
  xlab('Spiny waterflea biomass (?)') + ylab('Anoxic factor (d)') +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ (x)) +
  stat_poly_eq(formula = my.formula,
               aes(label = paste( ..rr.label.., sep = "~~~")),
               parse = TRUE,size = rel(4.5),
               label.y = 0.05,
               label.x = 0.1) +
  theme_bw()

g10 <- ggplot(df,aes(PO4.P_surf, AF)) + 
  geom_point() +
  xlab('Epilimnetic phosphate (mg/L)') + ylab('Anoxic factor (d)') +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ (x)) +
  stat_poly_eq(formula = my.formula,
               aes(label = paste( ..rr.label.., sep = "~~~")),
               parse = TRUE,size = rel(4.5),
               label.y = 0.05,
               label.x = 0.1) +
  theme_bw()

g11 <- ggplot(df,aes(PO4.P_bot, AF)) + 
  geom_point() +
  xlab('Hypolimnetic phosphate (mg/L)') + ylab('Anoxic factor (d)') +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ (x)) +
  stat_poly_eq(formula = my.formula,
               aes(label = paste( ..rr.label.., sep = "~~~")),
               parse = TRUE,size = rel(4.5),
               label.y = 0.05,
               label.x = 0.1) +
  theme_bw()

g <- (g1 + g2) / (g3 + g4)  / (g5 + g6) / (g7 + g8); g
g <- (g1 + g4) / (g5 + g8)  / (p.linear) + plot_annotation(tag_levels = 'A') + theme_bw(); g
g <- (g1 + g5) / (g8 + g9)  / (g10 + g11) / (p.linear) + plot_annotation(tag_levels = 'A') + theme_bw(); g
ggsave(plot = g, '../figs/Fig2_woCorr.png', dpi = 300, units = 'in', width = 7, height = 10)

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
  ylab('Phosphate (mg/L)') + xlab('') +
  geom_vline(xintercept=2010, linetype = 'dashed') +
  theme_bw(); 
g13 <- ggplot(df) +
  geom_line(aes(year, NO3.NO2.N_surf), linetype = 'solid') +
  geom_point(aes(year, NO3.NO2.N_surf)) +
  geom_line(aes(year, NO3.NO2.N_bot ), linetype = 'dashed') +
  geom_point(aes(year, NO3.NO2.N_bot)) +
  ylab('Nitrate (mg/L)') + xlab('') +
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
  ylab('Spiny Waterflea (?)') + xlab('') +
  geom_vline(xintercept=2010, linetype = 'dashed') +
  theme_bw(); 


g5 / g6 / g7 / g9 /g10 /g8 / g11 /g12 /g13/ g14





library(ggpubr)
df.prior = df %>%
  mutate('class' = ifelse(year < 2010, 'prior 2010','post 2010')) %>%
  dplyr::select(class, AF, med, Jz, Jv, Days.0.5.mg.L, discharge, Clearwater.Duration, pH, PO4.P_surf, NO3.NO2.N_surf, RSi, Spiny)
m.df.prior <- reshape2::melt(df.prior, id = 'class')

compare_means(value ~ class, data = m.df.prior %>% dplyr::filter(variable == 'AF'))
compare_means(value ~ class, data =  m.df.prior %>% dplyr::filter(variable == 'AF'), method ="kruskal.test")

p1 <- ggboxplot( m.df.prior %>% dplyr::filter(variable == 'AF'), x = "class", y = "value",
                palette = "jco", xlab = '', ylab = 'Anoxic Factor',fill = c(col.pre, col.post),
                add = "jitter")
#  Add p-value
p1 = p1 + stat_compare_means(label = 'p.format')
# Change method
# p1 + stat_compare_means(method = "t.test")

compare_means(value ~ class, data = m.df.prior %>% dplyr::filter(variable == 'med'))
compare_means(value ~ class, data =  m.df.prior %>% dplyr::filter(variable == 'med'), method ="kruskal.test")

p2 <- ggboxplot( m.df.prior %>% dplyr::filter(variable == 'med'), x = "class", y = "value",
                 palette = "jco", xlab = '', ylab = 'Stratification duration',fill = c(col.pre, col.post),
                 add = "jitter")
#  Add p-value
p2 = p2 + stat_compare_means(label = 'p.format');p2

compare_means(value ~ class, data = m.df.prior %>% dplyr::filter(variable == 'Jz'))
compare_means(value ~ class, data =  m.df.prior %>% dplyr::filter(variable == 'Jz'), method ="kruskal.test")

p3 <- ggboxplot( m.df.prior %>% dplyr::filter(variable == 'Jz'), x = "class", y = "value",
                 palette = "jco", xlab = '', ylab = 'Total oxygen sink',fill = c(col.pre, col.post),
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
                 palette = "jco", xlab = '', ylab = 'Biomass over 0.5 mg/L',fill = c(col.pre, col.post),
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
                 palette = "jco", xlab = '', ylab = 'Clearwater duration',fill = c(col.pre, col.post),
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
                 palette = "jco", xlab = '', ylab = 'SRP surf',fill = c(col.pre, col.post),
                 add = "jitter")
#  Add p-value
p8 = p8 + stat_compare_means(label = 'p.format')

compare_means(value ~ class, data = m.df.prior %>% dplyr::filter(variable == 'NO3.NO2.N_surf'))
compare_means(value ~ class, data =  m.df.prior %>% dplyr::filter(variable == 'NO3.NO2.N_surf'), method ="kruskal.test")

p9 <- ggboxplot( m.df.prior %>% dplyr::filter(variable == 'NO3.NO2.N_surf'), x = "class", y = "value",
                 palette = "jco", xlab = '', ylab = 'NO3-NO2-N surf',fill = c(col.pre, col.post),
                 add = "jitter")
#  Add p-value
p9 = p9 + stat_compare_means(label = 'p.format')

compare_means(value ~ class, data = m.df.prior %>% dplyr::filter(variable == 'RSi'))
compare_means(value ~ class, data =  m.df.prior %>% dplyr::filter(variable == 'RSi'), method ="kruskal.test")

p10 <- ggboxplot( m.df.prior %>% dplyr::filter(variable == 'RSi'), x = "class", y = "value",
                 palette = "jco", xlab = '', ylab = 'RSi', fill = c(col.pre, col.post),
                 add = "jitter")
#  Add p-value
p10 = p10 + stat_compare_means(label = 'p.format')


compare_means(value ~ class, data = m.df.prior %>% dplyr::filter(variable == 'Spiny'))
compare_means(value ~ class, data =  m.df.prior %>% dplyr::filter(variable == 'Spiny'), method ="kruskal.test")

p11 <- ggboxplot( m.df.prior %>% dplyr::filter(variable == 'Spiny'), x = "class", y = "value",
                  palette = "jco", xlab = '', ylab = 'Spiny', fill = c(col.pre, col.post),
                  add = "jitter")
#  Add p-value
p11 = p11 + stat_compare_means(label = 'p.format')

(g5 / g6 / g7 / g9 / g10 /g8 / g11 /g12 /g13/ g14 ) | (p1 / p2 /p3 /p5/p6 /p4 /p7 / p8  / p9 /p10)


ggsave(plot = (g5 / g6 / g7 / g9 / g10 /g8) | (p1 / p2 /p3 /p5/p6 /p4), '../figs/timeseries_comparison.png', dpi = 300, units = 'in', width = 20, height = 17)

ggsave(plot = (g5 / g6 / g7 / g9 / g10 /g8 / g11 /g12 /g13/ g14 ) | (p1 / p2 /p3 /p5/p6 /p4 /p7 / p8  / p9 /p10) + plot_layout(guides = 'collect'), '../figs/timeseries_comparison.png', dpi = 300, units = 'in', width = 20, height = 30)


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

png(file = "../figs/Fig_Breakpoint.png",res = 300,width = 216,height = 216, units = 'mm')
opar <- par(mfrow=c(2,1), mar=c(2,2,0,2))
plot(ocus.nile, alt.boundary=F,main="")
abline(v= 2010, lty=2, col='red')
plot(AF, ylab="Annual Flow of the river Nile") > abline(h= mean(AF),col='blue')
abline(v= 2010, lty=2, col='red')
lines(ts(predict(fm1.nile),start=1996,freq=1), col='darkgreen',lwd=2)
par(opar)
dev.off()

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


fig.plt <- (plt1 | plt2) / (plt3 | plt4) / (plt5 | plt6) / (plt7 | plt8) / (plt9 | plt10); fig.plt
fig.plt <- (plt1 | plt9) / (plt3 | plt5) / (plt4 | plt2) / (plt6 | plt7) / (plt8 | plt10); fig.plt
fig.plt <- (plt1 | plt9) / (plt3 | plt5) / (plt4 | plt2) / (plt6 | plt7) ; fig.plt
ggsave(plot = fig.plt , '../figs/Fig1_wBreakpoint.png', dpi = 300, units = 'in', width = 17, height = 16)
ggsave(plot = fig.plt , '../figs/Fig1_wBreakpoint.png', dpi = 300, units = 'in', width = 17, height = 13)

