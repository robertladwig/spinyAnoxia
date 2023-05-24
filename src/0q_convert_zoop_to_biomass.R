# RRR

# plan: merge the formula table with the full table in R, export as csv
# open the merged table in excel, run the excel-formatted formula cells (yeah.)
# read in the formatted excel file and save it as an unchanging R object

library(data.table)

# ---- merge tables ----

zoops <- readRDS(file = "data_input/0o_zoop_knb-lter-ntl.90.33.rds")
conversions <- readRDS(file = "data_input/0p_zoop_conversions_knb-lter-ntl.376.2.rds")

# check that names match for merging

names.zoops <- unique(zoops$species_name)
names.conv <- unique(conversions$species_name) # way more!
setdiff(x = names.zoops, y = names.conv)
grep(pattern = "Daphnia", x = unique(conversions$species_name), ignore.case = T, value = T)
# this is weird that it's missing Daphnia Mendotae and Pulicaria... just the most important ones??
setdiff(y = names.zoops, x = names.conv)


# ok, missing daphnia AND bytho... probably just do zoop counts for now.