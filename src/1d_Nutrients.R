# install.packages("devtools")
# devtools::install_github("hdugan/NTLlakeloads")
library(NTLlakeloads)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(tidyverse)
library(lubridate)

# Use these objects names, hardcoded at the moment
LTERtemp = loadLTERtemp() # Download NTL LTER data from EDI
LTERnutrients_old = loadLTERnutrients() # Download NTL LTER data from EDI
LTERions = loadLTERions() # Download NTL LTER data from EDI

availableVars()

# Hilary will kill me when she sees how I butchered her awesome functions...
addNutr <- read_csv('../data_input/SLOH_2019-21.csv') |> 
  dplyr::filter(lakeid == 'ME') %>%
  mutate(sampledate = as.Date(sampledate))

# addPH <- read_csv('../data_input/Mendota_pH.csv')
# 
# idxPH <- match(as.Date(addPH$sampledate), as.Date(addNutr$sampledate))
# addPH_df = data.frame('lakeid' = 'ME', 'sampledate' = addPH[na.omit(idxPH), 'sampledate'],
#                       'depth' = 0, 'flag' = addPH[na.omit(idxPH), 'flag_avg_ph'],
#                       'param' = 'ph', 'value' = addPH[na.omit(idxPH), 'avg_ph']) %>%
#   rename(flag = flag_avg_ph, value = avg_ph) %>%
#   mutate(sampledate = as.Date(sampledate))
# 
# addNutr_all <- rbind(addNutr, addPH_df)

addNutr_df <- addNutr %>%
  mutate(lakeid = as.character(lakeid), sampledate = as.POSIXct(sampledate),
         year4 = year(sampledate), daynum = yday(sampledate)) %>%
  spread(param, value) %>%
  rename(drp_sloh = DRP_SLOH, drsif_sloh = DRSIF_SLOH, no3no2_sloh = NO3NO2_SLOH) %>%
  dplyr::select(-flag)

idx_colnames = which(colnames(LTERnutrients_old) %in% colnames(addNutr_df))
columnsToAdd <- colnames(LTERnutrients_old)[-idx_colnames] 
addNutr_df[, columnsToAdd] <- NA

LTERnutrients <- rbind(LTERnutrients_old, addNutr_df) %>%
  group_by(lakeid) %>%
  arrange(sampledate, depth)

# ggplot(subset(LTERnutrients, lakeid == "ME" & depth == 0), aes(sampledate, ph)) + geom_point()

# Original stuff
df.pH = weeklyInterpolate(lakeAbr = 'ME', var = 'ph', dataset = LTERnutrients, maxdepth = 24,
                          constrainMethod = 'zero', setThreshold = 0.1, printFigs = F)

plotTimeseries(df.pH$weeklyInterpolated, var = 'ph')

df.tp = weeklyInterpolate(lakeAbr = 'ME', var = 'drp_sloh', dataset = LTERnutrients, maxdepth = 24, 
                          constrainMethod = 'zero', setThreshold = 0.1, printFigs = F)

plotTimeseries(df.tp$weeklyInterpolated, var = 'drp_sloh')

df.tn = weeklyInterpolate(lakeAbr = 'ME', var = 'no3no2_sloh', dataset = LTERnutrients, maxdepth = 24, 
                          constrainMethod = 'zero', setThreshold = 0.1, printFigs = F)

plotTimeseries(df.tn$weeklyInterpolated, var = 'no3no2_sloh')

df.sil = weeklyInterpolate(lakeAbr = 'ME', var = 'drsif_sloh', dataset = LTERnutrients, maxdepth = 24, 
                          constrainMethod = 'zero', setThreshold = 0.1, printFigs = F)

plotTimeseries(df.sil$weeklyInterpolated, var = 'drsif_sloh')


hypso <- read_csv('../data_input/LakeEnsemblR_bathymetry_standard.csv')
H <- hypso$Depth_meter
A <- hypso$Area_meterSquared

areas <- approx(H, A, unique(df.tp$weeklyInterpolated$depth))$y
depths = unique(df.tp$weeklyInterpolated$depth)
bath = data.frame('depths' = depths, 'areas' = areas)

df.ph = data.frame('datetime' = unique(df.pH$weeklyInterpolated$date), 'ph' = NA)
for (i in (df.ph$datetime)){
  data = df.pH$weeklyInterpolated %>%
    dplyr::filter(date == i, depth <= 13) 
  d.bath = bath %>%
    dplyr::filter(depths <= 13)
  df.ph$ph[match(i,df.ph$datetime)] = ( data$var %*% d.bath$areas ) / sum(d.bath$areas)
}

df.phos = data.frame('datetime' = unique(df.tp$weeklyInterpolated$date), 'tp_surf' = NA, 'tp_bottom' = NA)
for (i in (df.phos$datetime)){
  data = df.tp$weeklyInterpolated %>%
    dplyr::filter(date == i, depth <= 13) 
  d.bath = bath %>%
    dplyr::filter(depths <= 13)
  df.phos$tp_surf[match(i,df.phos$datetime)] = ( data$var %*% d.bath$areas ) / sum(d.bath$areas)
  
  data = df.tp$weeklyInterpolated %>%
    dplyr::filter(date == i, depth > 13) 
  d.bath = bath %>%
    dplyr::filter(depths > 13)
  df.phos$tp_bottom[match(i,df.phos$datetime)] = ( data$var %*% d.bath$areas ) / sum(d.bath$areas)
  
}

df.nitrate = data.frame('datetime' = unique(df.tp$weeklyInterpolated$date), 'tn_surf' = NA, 'tn_bottom' = NA)
for (i in (df.nitrate$datetime)){
  data = df.tn$weeklyInterpolated %>%
    dplyr::filter(date == i, depth <= 13) 
  d.bath = bath %>%
    dplyr::filter(depths <= 13)
  df.nitrate$tn_surf[match(i,df.nitrate$datetime)] = ( data$var %*% d.bath$areas ) / sum(d.bath$areas)
  
  data = df.tn$weeklyInterpolated %>%
    dplyr::filter(date == i, depth > 13) 
  d.bath = bath %>%
    dplyr::filter(depths > 13)
  df.nitrate$tn_bottom[match(i,df.nitrate$datetime)] = ( data$var %*% d.bath$areas ) / sum(d.bath$areas)
  
}


df.silica = data.frame('datetime' = unique(df.sil$weeklyInterpolated$date), 'sil' = NA)
for (i in (df.silica$datetime)){
  data = df.sil$weeklyInterpolated %>%
    dplyr::filter(date == i, depth <= 13) 
  d.bath = bath %>%
    dplyr::filter(depths <= 13)
  df.silica$sil[match(i,df.silica$datetime)] = ( data$var %*% d.bath$areas ) / sum(d.bath$areas)
}

start <- read_csv('../data_processed/stratification_start.csv')
end <- read_csv('../data_processed/stratification_end.csv')

df = data.frame('year' =start$year, 'pH' = NA, 'PO4.P_surf' = NA, 'PO4.P_bot' = NA, 'NO3.NO2.N_surf' = NA, 'NO3.NO2.N_bot' = NA, 'RSi' = NA)
for (i in df$year){
  st = start %>%
    dplyr::filter(year == i) %>%
    mutate(doy = mean(lubridate::yday(linear),lubridate::yday(constant.high),lubridate::yday(constant.low),lubridate::yday(spline)))
  en = end %>%
    dplyr::filter(year == i) %>%
    mutate(doy = mean(lubridate::yday(linear),lubridate::yday(constant.high),lubridate::yday(constant.low),lubridate::yday(spline)))
  
  idx = match(i,df$year)
  
  df1 = df.ph %>%
    mutate(year = year(datetime),doy = lubridate::yday(datetime)) %>%
    dplyr::filter(year == i) %>%
    dplyr::filter(doy >= st$doy & doy <= en$doy) %>%
    summarise(mean = mean(ph))
  df$pH[idx] = as.numeric(df1)
  
  df2 = df.phos %>%
    mutate(year = year(datetime),doy = lubridate::yday(datetime)) %>%
    dplyr::filter(year == i) %>%
    dplyr::filter(doy >= st$doy & doy <= en$doy) %>%
    summarise(surf = mean(tp_surf), bot = mean(tp_bottom))
  df$'PO4.P_surf'[match(i,df$year)] = as.numeric(df2[1])
  df$'PO4.P_bot'[match(i,df$year)] = as.numeric(df2[2])
  
  df3 = df.nitrate %>%
    mutate(year = year(datetime),doy = lubridate::yday(datetime)) %>%
    dplyr::filter(year == i) %>%
    dplyr::filter(doy >= st$doy & doy <= en$doy) %>%
    summarise(surf = mean(tn_surf), bot = mean(tn_bottom))
  df$'NO3.NO2.N_surf'[match(i,df$year)] = as.numeric(df3[1])
  df$'NO3.NO2.N_bot'[match(i,df$year)] = as.numeric(df3[2])
  
  df4 = df.silica %>%
    mutate(year = year(datetime),doy = lubridate::yday(datetime)) %>%
    dplyr::filter(year == i) %>%
    dplyr::filter(doy >= st$doy & doy <= en$doy) %>%
    summarise(mean = mean(sil))
  df$RSi[match(i,df$year)] = as.numeric(df4)
}

write_csv(df, '../data_processed/nutrients.csv')



df.stochio = merge(df.phos, df.nitrate, by = 'datetime')

ggplot(df.stochio) +
  geom_line(aes(datetime, tn_surf/tp_surf, col = 'surface')) +
  geom_line(aes(datetime, tn_bottom/tp_bottom, col = 'bottom'))

ggplot(df.stochio) +
  geom_line(aes(datetime, tn_surf, col = 'surface')) +
  geom_line(aes(datetime, tn_bottom, col = 'bottom'))


ggplot(df.stochio) +
  geom_line(aes(datetime, tp_surf, col = 'surface')) +
  geom_line(aes(datetime, tp_bottom, col = 'bottom'))

# mg/L 
# N 14 g/mole
# P 31 g/mole
# O 16 g/mole
# NO3-NO2
Nitrate = 14# 14*2 + 16 *5
# PO4
Phosphate = 31 #31 + 16 * 4

volume_epi = hypso %>%
  dplyr::filter(Depth_meter   <= 13) %>%
  summarise(sum = sum(Area_meterSquared))

volume_hypo = hypso %>%
  dplyr::filter(Depth_meter   > 13) %>%
  summarise(sum = sum(Area_meterSquared))

df.stochio = df.stochio %>%
  mutate(tn_surf_molar = (tn_surf/ 1000 * Nitrate) * 1,
         tp_surf_molar = (tp_surf/ 1000 * Phosphate) * 1,
         tn_bottom_molar = (tn_bottom/ 1000 * Nitrate) * 1,
         tp_bottom_molar = (tp_bottom/ 1000 * Phosphate) * 1)

ggplot(df.stochio) +
  geom_line(aes(datetime, tn_surf_molar / tp_surf_molar, col = 'surface')) +
  geom_line(aes(datetime, tn_bottom_molar / tp_bottom_molar, col = 'bottom')) + 
  xlab("") + ylab('N:P (molar)') + labs(colour = 'Strata')+
  theme_minimal()

df.stochio_year = df.stochio %>%
  mutate(year = year(datetime),
         stoch_surf = tn_surf_molar / tp_surf_molar,
         stoch_bottom = tn_bottom_molar / tp_bottom_molar) %>%
  dplyr::select(year, stoch_surf, stoch_bottom) %>%
  group_by(year) %>%
  summarise_all(mean)

ggplot(df.stochio_year) +
  geom_line(aes(year, stoch_surf, col = 'surface')) + 
  geom_line(aes(year, stoch_bottom, col = 'bottom')) 

write_csv(df.stochio_year, '../data_processed/stoichiometry.csv')
