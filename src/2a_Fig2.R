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




physics <- read_csv('../data_processed/physical_timings.csv')

anoxic <- read_csv("../data_processed/anoxicfactor.csv")

fluxes <- read_csv('../data_processed/dosinks.csv')

biomass <- read_csv('../data_processed/3c_biomass_duration.csv')

discharge <- read_csv('../data_processed/discharge.csv')

cw <- readRDS('../data_processed/yearly_clearwater_stats.rds')

nutrients <- read_csv('../data_processed/nutrients.csv')

spiny <- read_csv('../data_processed/spiny.csv')


col.pre <- "steelblue"
col.post <- "orange3"

df.physics <- physics 

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

df <- merge(df.physics, df.anoxic, by = 'year')
df <- merge(df, df.flux, by = 'year')
df <- merge(df, df.biomass, by = 'year')
df <- merge(df, df.discharge, by = 'year')
df <- merge(df, df.cw, by = 'year')
df <- merge(df, df.nutrients, by = 'year')
df <- merge(df, df.spiny, by = 'year')

str(df)
head(df)

df.red <- df[, c("AF",'strat_on' , "strat_off" , "strat_duration" , 
                 "ice_on" , "ice_off" , "ice_duration" , 
                 "Jz" , "Jv" , "Ja" , 
                 "Days.0.5.mg.L" , "Days.1.mg.L" , "Days.1.5.mg.L" ,
                 "Days.2.mg.L" , "Days.3.mg.L" ,
                 "discharge" , "max.discharge" , "min.discharge" ,
                 "Clearwater.Duration" , "Prev.Winter.Duration" , "Max.Clearwater.Depth.m" , "Spiny" ,
                 "pH" , "PO4.P_surf", "PO4.P_bot", "NO3.NO2.N_surf", "NO3.NO2.N_bot", "RSi")]
sc.info <- scale(df.red)
df.data <- as.data.frame(scale(df.red))
boruta_output <- Boruta(AF ~ ., 
                        data = df.data, doTrace=2,
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

idx = which( colnames(df.data) %in% boruta_signif)
hyp.data = df.data[,idx]
hyp.data$AF = df.data$AF
# sc.info <- scale(hyp.data)
# hyp.data <- as.data.frame(scale(hyp.data))


hypo1 <- lm(AF ~ ., data = hyp.data)
summary(hypo1)
sum.hypo1 <-summary(hypo1)
step(hypo1)


hypo1 <- lm(AF ~ strat_off + ice_duration + Days.1.mg.L + PO4.P_surf + 
              PO4.P_bot + Spiny , data = hyp.data)

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


modeleq1 <- paste0('y = ', round(sum.hypo1$coefficients[1,1],2),
                  ' + ',round(sum.hypo1$coefficients[2,1],2),' Strat.off',
                  ' + ',round(sum.hypo1$coefficients[3,1],2),' Ice.dur',
                  ' + ',round(sum.hypo1$coefficients[4,1],2),' Days.1',
                  ' + ')
modeleq2 <- paste0(round(sum.hypo1$coefficients[5,1],2),' SWF',
                  ' + ',round(sum.hypo1$coefficients[6,1],2),' PO4.surf',
                  ' + ',round(sum.hypo1$coefficients[7,1],2),' PO4.bot',
                  ' + e, where e ~ N(0,',round(sum.hypo1$sigma,2),")")

pred.int <- predict(hypo1, interval = "confidence")
pred.int <- pred.int * attr(sc.info, 'scaled:scale')[1] + attr(sc.info, 'scaled:center')[1]

mydata <- cbind(data.frame('AF'  = df$AF), pred.int)

# PLOT: linear model
p <- ggplot(mydata, aes(fit, AF)) +
  stat_smooth(method = lm, col = 'black') +
  geom_point(size = 2) +
  xlab('Predicted Anoxic Factor [d per season]')+
  ylab('Anoxic Factor [d per season]')+
  theme_minimal()+
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ (x)) +
  stat_poly_eq(formula = my.formula,
               aes(label = paste( ..rr.label.., sep = "~~~")),
               parse = TRUE,size = rel(4.5),
               label.y = 0.05,
               label.x = 0.1)  +
  # annotate("text", x = 61, y = 82.5, label = modeleq1, size = 3)+
  # annotate("text", x = 61, y = 80, label = modeleq2, size = 3)+
  # annotate("text", x = 60, y = 77.5, label = (paste0('R2 = ',round(sum.hypo1$r.squared,2))), size =5) +
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=0, hjust=1));p
p.linear <- p + geom_line(aes(y = lwr), color = "grey", linetype = "dashed")+
  geom_line(aes(y = upr), color = "grey", linetype = "dashed"); p.linear




hyp.data2 <- hyp.data[, c(1,2,3,6,7)]


colnames(hyp.data2) = c('Strat.dur', 'Total.sink', 'Days.0.5','ClearWat.dur', 'Anoxic.Factor', "SWF")

res=cor(hyp.data2, method = c("pearson"))




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
g5 <- ggplot(df,aes(Days.1.mg.L, AF)) + 
  geom_point() +
  xlab('Biomass over 1.0 mg/L (d)') + ylab('Anoxic factor (d)') +
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
  xlab('Spiny waterflea biomass (counts)') + ylab('Anoxic factor (d)') +
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
g12 <- ggplot(df,aes(strat_off, AF)) + 
  geom_point() +
  xlab('Strat. breakdown date (-)') + ylab('Anoxic factor (d)') +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ (x)) +
  stat_poly_eq(formula = my.formula,
               aes(label = paste( ..rr.label.., sep = "~~~")),
               parse = TRUE,size = rel(4.5),
               label.y = 0.05,
               label.x = 0.1) +
  theme_bw()
g13 <- ggplot(df,aes(ice_duration, AF)) + 
  geom_point() +
  xlab('Ice duration (d)') + ylab('Anoxic factor (d)') +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ (x)) +
  stat_poly_eq(formula = my.formula,
               aes(label = paste( ..rr.label.., sep = "~~~")),
               parse = TRUE,size = rel(4.5),
               label.y = 0.05,
               label.x = 0.1) +
  theme_bw()


g <- (g12 + g13) / (g5 + g9)  / (g10 + g11) / (p.linear) + plot_annotation(tag_levels = 'A') + theme_bw(); g
ggsave(plot = g, '../figs_publication/Fig2.png', dpi = 300, units = 'in', width = 7, height = 10)
