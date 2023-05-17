setwd('~/Documents/DSI/spinyAnoxia/')
library(tidyverse)
library(lubridate)
library(smwrBase)

df <- read.csv('data_input/climate_madisonAirport.csv', skip = 6)

climate_df <- df %>%
  mutate(datetime = as.POSIXct(date),
         pcpn_mm = as.numeric(pcpn) * 0.0254 * 1000,
         year = year(datetime),
         doy = yday(datetime),
         wateryear = waterYear(datetime, numeric = T))

ggplot(climate_df) +
  geom_line(aes(doy, pcpn_mm)) +
  xlab('') + ylab("") +
  facet_wrap(~ wateryear) +
  theme_bw()

ggplot(climate_df) +
  geom_boxplot(aes(wateryear, pcpn_mm, group = wateryear)) +
  xlab('') + ylab("") +
  theme_bw()

summary_df <- climate_df %>%
  group_by(wateryear) %>%
  summarise(mean_pp = mean(pcpn_mm, na.rm = T),
            max_pp = max(pcpn_mm, na.rm = T),
            min_pp = min(pcpn_mm, na.rm = T),
            cumsum_pp = sum(pcpn_mm, na.rm = T))

mean_cumsum_pp <- mean(summary_df$cumsum_pp)

ggplot(summary_df) +
  geom_line(aes(wateryear, cumsum_pp)) +
  geom_hline(yintercept = mean_cumsum_pp, linetype="dashed",
             color = "grey") +
  xlab('Hydrological year') + ylab("Cumulative precipitation (mm)") +
  xlim(c(1995, 2022)) +
  theme_bw()

write_csv(summary_df, '../data_processed/precipitation.csv')
