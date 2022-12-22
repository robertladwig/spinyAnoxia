# RRR

save.combined.as <- "data_processed/0n_combined_DO-full_profiles.rds"

library(tidyr)
library(dplyr)
library(lubridate)

lter <- readRDS("data_input/0k_DO_1995-2021_knb-lter-ntl.29.34.rds")
exo <- readRDS("data_input/0l_DO_2017-2020_knb-lter-ntl.400.3.rds")
memo <- readRDS("data_input/0m_DO_2006-2019_knb-lter-ntl.415.3.rds")

colnames(lter)
colnames(exo)
colnames(memo)

# long format to combine datasets ----

lter <- pivot_longer(data = lter, cols = 7:ncol(lter), names_to = "Depth.m", values_to = "DO.mg.L", values_drop_na = T)
head(lter)

exo <- pivot_longer(data = exo, cols = 7:ncol(exo), names_to = "Depth.m", values_to = "DO.mg.L", values_drop_na = T)
head(exo)

memo <- pivot_longer(data = memo, cols = 7:ncol(memo), names_to = "Depth.m", values_to = "DO.mg.L", values_drop_na = T)
head(memo)

all.do <- rbind(lter, exo, memo)

# combine same-day measurements by averaging the replicated depths ----

all.do <- pivot_wider(data = all.do, 
                      id_cols = c("Year","Month","Day",), 
                      names_from = "Depth.m", values_from = "DO.mg.L", values_fn = mean)

# remove incomplete profiles ----

all.do <- pivot_longer(data = all.do, cols = 4:ncol(all.do), names_to = "Depth.m", values_to = "DO.mg.L")

all.do$Depth.m <- all.do$Depth.m |> 
  sub(pattern = "m", replacement = "") |>
  as.numeric()

all.do$Date <- paste(all.do$Year,all.do$Month,all.do$Day, sep = "-")

# say a complete profile has at least 5 epi and 5 hypo measurements
for (d in unique(all.do$Date)){
  index.all.do <- which(all.do$Date == d)
  my.do <- all.do[index.all.do, ]
  my.epi <- sum(my.do$Depth.m <= 10 & !is.na(my.do$DO.mg.L))
  my.hypo <- sum(my.do$Depth.m > 10 & !is.na(my.do$DO.mg.L))
  if (my.epi < 5 | my.hypo < 5){
    all.do <- all.do[-index.all.do, ]
    cat("removing", d, "epi-",my.epi,"hypo-",my.hypo,"\n")
  }
}

# simplify date columns ----

all.do$Date <- parse_date_time(x = all.do$Date, orders = "ymd")
all.do <- all.do[ ,c(1:3,6,4,5)]

# save long-formatted combined and filtered data ----

saveRDS(object = all.do, file = save.combined.as)
