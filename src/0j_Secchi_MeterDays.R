library(tidyverse)
library(zoo)
require(pracma)

secchi = as.tibble(read_rds(file = 'data_input/0g_secchi_combined.rds')) |> 
  mutate(Date = as.Date(Date))

secchi |> filter(Year > 1995) |> 
  ggplot() +
  geom_point(aes(x = Date, y = Secchi.Depth.m)) + 
  geom_path(aes(x = Date, y = Secchi.Depth.m)) + 
  facet_wrap(~Year, scales = 'free') +
  scale_y_reverse()


out.df = data.frame(year = 1995:2020, seccchiArea = NA)
years = 1995:2020
for (i in 1:length(years)) {
  year.data = secchi |> filter(Year == years[i]) |> 
    filter(Month >= 4 & Month <= 6)
  
  a = data.frame(Date = seq.Date(from = year.data$Date[1], to = tail(year.data$Date, 1), by = 'day')) |> 
    left_join(year.data |> select(Date, Secchi.Depth.m)) |> 
    mutate(Secchi.interp = na.approx(Secchi.Depth.m))
  
  out.df$seccchiArea[i] = trapz(1:nrow(a),a$Secchi.interp)
  
}

ggplot(out.df) +
  geom_point(aes(x = year, y = seccchiArea)) +
  geom_line(aes(x = year, y = seccchiArea))
