# RRR
# Since I found more data on EDI, I'm going to add it in now and 
# not try to go back to my older scripts and add it in there.

library(lubridate)

lter <- readRDS("data_input/0d_secchi-1995-2020_knb-lter-ntl.31.30.rds")
memo <- readRDS("data_input/0e_secchi-2012-2019_knb-lter-ntl.416.1.rds")
old <- readRDS("data_input/0f_secchi_1900-1989_knb-lter-ntl.335.2.rds")

created.file <- "data_input/0g_secchi_combined.rds"

# ---- match formats ----

str(lter)
str(memo)
str(old)

lter$sample.date <- parse_date_time(x = lter$sample.date, orders = "ymd", tz = "Etc/GMT-5")
lter <- data.frame("Year" = year(lter$sample.date), "Month" = month(lter$sample.date), "Day" = day(lter$sample.date), 
                          "Secchi.Depth.m" = lter$secchi.depth.UNIT, "s.Secchi.Depth.m" = NA, "People" = "not recorded", "Notes.Secchi" = lter$notes.secchi, 
                          "Source.Secchi" = "knb-lter-ntl.31.30")
lter$s.Secchi.Depth.m <- as.numeric(lter$s.Secchi.Depth.m)
index <- which(is.na(lter$Secchi.Depth.m))
lter <- lter[-index, ]

memo$People <- as.character(memo$People)
memo$Notes.Secchi <- as.character(memo$Notes.Secchi)
memo$Source.Secchi <- "knb-lter-ntl.416.1"

old$Source.Secchi <- "knb-lter-ntl.335.2"

lter$Date <- parse_date_time(x = paste(lter$Year, lter$Month, lter$Day), orders = "ymd", tz = "Etc/GMT-5")
memo$Date <- parse_date_time(x = paste(memo$Year, memo$Month, memo$Day), orders = "ymd", tz = "Etc/GMT-5")
old$Date <- parse_date_time(x = paste(old$Year, old$Month, old$Day), orders = "ymd", tz = "Etc/GMT-5")

# ---- no dups within datasets ----

any(duplicated(lter$Date))

memo <- memo[-which(memo$Notes.Secchi == "measured using the newly painted CFL outreach secchi"), ]
any(duplicated(memo$Date))

any(duplicated(old$Date))
dup.dates <- old$Date[duplicated(old$Date)]
index.dups <- which(old$Date %in% dup.dates)
no.dups <- old[-index.dups, ]
just.dups <- old[index.dups, ]
new.avs <- aggregate(x = just.dups$Secchi.Depth.m, by = list(just.dups$Date), FUN = mean, na.rm = T)
new.sds <- aggregate(x = just.dups$Secchi.Depth.m, by = list(just.dups$Date), FUN = sd, na.rm = T)
new.ppl <- aggregate(x = just.dups$People, by = list(just.dups$Date), FUN = paste, collapse = ", ")
new.ppl$x <- sub(pattern = "R. Stauffer, R. Stauffer", replacement = "R. Stauffer", x = new.ppl$x)
new.ppl$x <- sub(pattern = "R. Stauffer, R. Stauffer", replacement = "R. Stauffer", x = new.ppl$x)
new.ppl$x <- sub(pattern = "R. Stauffer, R. Stauffer", replacement = "R. Stauffer", x = new.ppl$x)
new.ppl$x <- sub(pattern = "R.A. Ragotzkie, R.A. Ragotzkie", replacement = "R.A. Ragotzkie", x = new.ppl$x)
new.ppl$x <- sub(pattern = "E.A. Birge C. Juday and Associates, E.A. Birge C. Juday and Associates", replacement = "E.A. Birge C. Juday and Associates", x = new.ppl$x)
new.notes <- aggregate(x = just.dups$Notes.Secchi, by = list(just.dups$Date), FUN = paste, collapse = "; ") 
all.equal(new.avs$Group.1,new.sds$Group.1)
all.equal(new.avs$Group.1,new.ppl$Group.1)
all.equal(new.avs$Group.1,new.notes$Group.1)
new.old <- data.frame("Year" = year(new.avs$Group.1), "Month" = month(new.avs$Group.1), "Day" = day(new.avs$Group.1), 
                      "Secchi.Depth.m" = new.avs$x, "s.Secchi.Depth.m" = new.sds$x, "People" = new.ppl$x, "Notes.Secchi" = new.notes$x, 
                      "Source.Secchi" = "knb-lter-ntl.335.2", "Date" = new.avs$Group.1)
old <- rbind(no.dups, new.old)


# ---- combine tables and remove duplicates ----

all.secchi <- rbind(old, lter, memo)
index <- order(all.secchi$Date)
all.secchi <- all.secchi[index, ]


# ---- combine duplicated dates ----

dup.dates <- all.secchi$Date[duplicated(all.secchi$Date)]
index.dups <- which(all.secchi$Date %in% dup.dates)

no.dups <- all.secchi[-index.dups, ]
just.dups <- all.secchi[index.dups, ]

new.avs <- aggregate(x = just.dups$Secchi.Depth.m, by = list(just.dups$Date), FUN = mean, na.rm = T)
new.sds <- aggregate(x = just.dups$Secchi.Depth.m, by = list(just.dups$Date), FUN = sd, na.rm = T)
new.ppl <- aggregate(x = just.dups$People, by = list(just.dups$Date), FUN = paste, collapse = ", ")
new.notes <- aggregate(x = just.dups$Notes.Secchi, by = list(just.dups$Date), FUN = paste, collapse = "; ") 
new.source <- aggregate(x = just.dups$Source.Secchi, by = list(just.dups$Date), FUN = paste, collapse = "; ") 
new.all <- data.frame("Year" = year(new.avs$Group.1), "Month" = month(new.avs$Group.1), "Day" = day(new.avs$Group.1), 
                      "Secchi.Depth.m" = new.avs$x, "s.Secchi.Depth.m" = new.sds$x, "People" = new.ppl$x, "Notes.Secchi" = new.notes$x, 
                      "Source.Secchi" = new.source$x, "Date" = new.avs$Group.1)

all.secchi <- rbind(no.dups, new.all)
index <- order(all.secchi$Date)
all.secchi <- all.secchi[index, ]

# ---- Export ----

cat(created.file)
saveRDS(object = all.secchi, file = created.file)
