library(tidyverse)
library(scales)
library(lubridate)

# Load physical data
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/29/30/03e232a1b362900e0f059859abe8eb97"
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))

physical <- read_csv(infile1) |> filter(lakeid == 'ME') |> 
  filter(sampledate != as.Date('2007-05-14')) ### BAD OXYGEN DATA


# load stratification data
straton = read_csv('data_processed/stratification_start.csv') |> 
  mutate(straton = `year<-`(linear, year)) |> 
  select(year4 = year, straton)
stratoff = read_csv('data_processed/stratification_end.csv') |> 
  mutate(stratoff = `year<-`(linear, year)) |> 
  select(year4 = year, stratoff)

do.df = physical |> 
  left_join(straton) |> 
  left_join(stratoff) |> 
  filter(sampledate > straton & sampledate < stratoff) |> 
  group_by(sampledate) |> 
  filter(o2 < 1.5) |> 
  filter(depth == min(depth))  |> 
  mutate(group = if_else(year4 < 2010, 'Pre','Post')) |> 
  mutate(group = factor(group, levels = c('Pre','Post')))

ggplot(do.df) +
  geom_smooth(aes(x = as.Date(daynum, origin = as.Date("2000-01-01")), y = depth, group = group, color = group, fill = group), alpha = 0.4, size = 0.03) +
  geom_path(aes(x = as.Date(daynum, origin = as.Date("2000-01-01")), y = depth, group = year4, color = group),  size = 0.3) +
  geom_point(aes(x = as.Date(daynum, origin = as.Date("2000-01-01")), y = depth, group = year4, fill = group), shape = 21, stroke = 0.2) +
  scale_fill_manual(values = c('steelblue','orange3')) +
  scale_color_manual(values = c('steelblue','orange3')) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b") +
  scale_y_reverse() +
  ylab("Depth (m) of anoxia transition"~("<"~1.5~mg~L^-1)) +
  theme_bw(base_size = 9) +
  theme(axis.title.x = element_blank(), 
        legend.position = c(0.9,0.85),
        legend.title = element_blank())

ggsave('figs_publication/Fig5_HD.png', dpi = 500, units = 'in', width = 6.5, height = 3)

