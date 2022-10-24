rm(list = ls())

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(tidyverse)

swf <- readRDS('../data_input/4_SWF_sources_combined.rds')

swf %>%
  dplyr::filter(Source == 'Jake Walsh (knb-lter-ntl.342.1 dt1 ME DH )')

swf %>%
  dplyr::filter(Source == 'Jake Walsh (knb-lter-ntl.342.1 dt1 ME LTER-DH)')

swf.df <- swf %>%
  dplyr::filter(Source == 'LTER base crew zooplankton (knb-lter-ntl.90.31 dt1 ME)') %>%
  mutate(year = year(Date)) %>%
  group_by(year) %>%
  summarise(mean = mean(SWF.num.m3))

ggplot(swf.df, aes(year, mean)) + geom_line()

write_csv(swf.df, '../data_processed/spiny.csv')
