# 2012-08-17 202.00

####### Load packages 
library(tidyverse)
library(scales)

anox <- readRDS("data_processed/0n_combined_DO-full_profiles.rds") # pull from data I curated elsewhere- just quick for now
strat <- readRDS("data_processed/3g_stratification_dates.rds")
strat <- strat$mean

# ---- remove spring anoxia sketchy ones ----

# 2007-5-14
# 2015-5-24
# 2015-5-28

index <- anox$Year == 2007 & anox$Month == 5 & anox$Day == 14
plot(x = anox$DO.mg.L[index], y = -anox$Depth.m[index])
# profile looks fine, but clearly stratified to ~ 10 m when the lake supposedly just stratified 8.75 days earlier
# so removing because I think there is no explanation for how this cannot be wrong?
anox <- anox[!index, ]

index <- anox$Year == 2015 & anox$Month == 5 & anox$Day == 24
plot(x = anox$DO.mg.L[index], y = -anox$Depth.m[index])
# looks like shit, how did that make it past my screening
anox <- anox[!index, ]

index <- anox$Year == 2015 & anox$Month == 5 & anox$Day == 28
plot(x = anox$DO.mg.L[index], y = -anox$Depth.m[index])
# obviously something wrong with the probe
anox <- anox[!index, ]

# ---- remove summer sketchy depth = 0 ones ----

# 2013-07-29
# 2016-07-06
# 2016-07-08

index <- anox$Year == 2013 & anox$Month == 7 & anox$Day == 29
plot(x = anox$DO.mg.L[index], y = -anox$Depth.m[index])
# super wack, how did this make it past screening I freaking already checked them all
anox <- anox[!index, ]

index <- anox$Year == 2016 & anox$Month == 7 & anox$Day == 06
plot(x = anox$DO.mg.L[index], y = -anox$Depth.m[index])
# super wack, how did this make it past screening I freaking already checked them all
anox <- anox[!index, ]

index <- anox$Year == 2016 & anox$Month == 7 & anox$Day == 08
plot(x = anox$DO.mg.L[index], y = -anox$Depth.m[index])
# super wack, how did this make it past screening I freaking already checked them all
anox <- anox[!index, ]

# ---- format anox ----

anox$yday <- yday(anox$Date)
anox <- anox[anox$Depth.m <= 20, ] # trim profiles to extend to 20 m only
anox$days.since.strat <- NA
for (yr in strat$year){
  anox$days.since.strat[anox$Year == yr] <- anox$yday[anox$Year == yr] - strat$start[strat$year == yr]
}
anox <- anox[order(anox$Date), ]


ntl2 = anox |> rename(o2 = DO.mg.L, sampledate = Date, depth = Depth.m) |> 
  mutate(sampledate = as.Date(sampledate)) |> 
  mutate(o2 = if_else(o2 < 0, 0, o2))


# Normal scatter plot colorded by depth
ggplot(ntl2 %>% dplyr::filter(o2 >= 0)) +
  geom_point(aes(x = sampledate, y = depth, color = o2))

# Contour Map... you can see the problems 
ggplot(ntl2) +
  geom_contour_filled(aes(x = sampledate, y = depth, z = o2)) +
  geom_point(aes(x = sampledate, y = depth)) +
  scale_y_reverse() +
  scale_color_distiller() 

# Vertical linear interpolation of water column concentrations 
interpData <- function(observationDF, date, maxdepth) {
  a = observationDF %>% dplyr::filter(sampledate == date)
  if (sum(!is.na(a$o2)) == 0) {
    print('nothing')
    return(NULL)
  }
  
  b = a %>% dplyr::filter(!is.na(o2))
  if (max(b$depth) < (maxdepth/2)) {
    print('too shallow')
    return(NULL)
  }
  
  yout = approx(x = a$depth, y = a$o2, xout = c(0:maxdepth), rule = 2)
  return(yout$y)
}

maxdepth = 20 # Should be depth of lowest sample, not necessarily depth of lake 
usedates = ntl2 %>%
  dplyr::distinct(sampledate) 

f <- lapply(X = usedates$sampledate, FUN = interpData, observationDF = ntl2,
            maxdepth = maxdepth)

f = as.data.frame(do.call(cbind, f))
names(f) = usedates$sampledate

# Bind list into dataframe
f2 = bind_cols(depth = 0:maxdepth,f) %>%
  pivot_longer(-1, names_to = 'sampledate', values_to = 'var') %>%
  arrange(sampledate,depth) %>%
  mutate(sampledate = as.Date(sampledate))

# Heat map 
for(i in 1995:2020) {

ggplot(f2) +
  guides(fill = guide_colorsteps(barheight = unit(3, "cm"))) +
  geom_contour_filled(aes(x = sampledate, y = depth, z = var), breaks = seq(-1,20,by = 2)) +
  geom_point(data = ntl2, aes(x = sampledate, y = depth), size = 0.25, color = 'white') +
  scale_y_reverse()  +
  ylab('Depth (m)') + 
  labs(fill = "DO (mg/L)", title = i) +
  theme_bw(base_size = 8) +
  scale_x_date(breaks = "2 month", minor_breaks = "1 month", labels=date_format("%b"),
               limits = c(as.Date(paste0(i,'-01-01')), as.Date(paste0(i,'-12-31'))))

  ggsave(paste0('figs/oyxgenProfiles/Mendota_',i,'.png'), width = 6, height = 3, dpi = 500)
}
