library(tidyverse)
library(lubridate)

# download ice data
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/33/35/f5bc02452cafcd461c49bd7429d8b40c" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
ice.df <-read_csv(infile1)

ice.df2 = ice.df |> filter(lakeid == 'ME') |> 
  mutate(ice_on = ymd(ice_on), ice_off = ymd(ice_off)) |> 
  select(year4, ice_on, ice_off) |> 
  filter(year4 >= 1994) |> 
  mutate(year4 = year(ice_off))

strat_duration <-      read_csv('../data_processed/stratification.csv') %>%
  filter(year < 2021) %>%
  group_by(year) %>%
  mutate(med = mean((linear), (constant.low), 
                    (constant.high), (spline)))
on_strat <-   read_csv('../data_processed/stratification_start.csv') %>%
  filter(year < 2021) %>%
  group_by(year) %>%
  mutate(med = mean(yday(linear), yday(constant.low), 
                    yday(constant.high), yday(spline)))
off_strat <-  read_csv('../data_processed/stratification_end.csv') %>%
  filter(year < 2021) %>%
  group_by(year) %>%
  mutate(med = mean(yday(linear), yday(constant.low), 
                    yday(constant.high), yday(spline)))
ice_duration <- ice.df2 %>% 
  dplyr::filter(year4 > 1995)%>%
  mutate(ice_duration = as.numeric(ice_off - ice_on)) %>%
  mutate(ice_on = yday(ice_on)) %>%
  mutate(ice_off = yday(ice_off)) %>%
  rename(year = year4) 

df <- data.frame('year' = strat_duration$year,
                 'strat_on' = on_strat$med,
                 'strat_off' = off_strat$med,
                 'strat_duration' = strat_duration$med,
                 'ice_on' = ice_duration$ice_on,
                 'ice_off' = ice_duration$ice_off,
                 'ice_duration' = ice_duration$ice_duration
)

write_csv(df, '../data_processed/physical_timings.csv')
