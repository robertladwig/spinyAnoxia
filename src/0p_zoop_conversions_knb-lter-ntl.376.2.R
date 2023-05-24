# Package ID: knb-lter-ntl.376.2 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER Zooplankton conversion formulas length to biomass.
# Data set creator:  Timothy Kratz - University of Wisconsin 
# Data set creator:  Pamela Montz -  
# Data set creator:  Thomas Frost -  
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/376/2/64717bde7ad91be0104f9c48ebad4c89" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "larger_group",     
                 "species",     
                 "length",     
                 "mass_formula",     
                 "legnth_reference",     
                 "mass_formula_reference",     
                 "notes"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$larger_group)!="factor") dt1$larger_group<- as.factor(dt1$larger_group)
if (class(dt1$species)!="factor") dt1$species<- as.factor(dt1$species)
if (class(dt1$length)=="factor") dt1$length <-as.numeric(levels(dt1$length))[as.integer(dt1$length) ]               
if (class(dt1$length)=="character") dt1$length <-as.numeric(dt1$length)
if (class(dt1$mass_formula)!="factor") dt1$mass_formula<- as.factor(dt1$mass_formula)
if (class(dt1$legnth_reference)!="factor") dt1$legnth_reference<- as.factor(dt1$legnth_reference)
if (class(dt1$mass_formula_reference)!="factor") dt1$mass_formula_reference<- as.factor(dt1$mass_formula_reference)
if (class(dt1$notes)!="factor") dt1$notes<- as.factor(dt1$notes)

# Convert Missing Values to NA for non-dates



# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(larger_group)
summary(species)
summary(length)
summary(mass_formula)
summary(legnth_reference)
summary(mass_formula_reference)
summary(notes) 
# Get more details on character variables

summary(as.factor(dt1$larger_group)) 
summary(as.factor(dt1$species)) 
summary(as.factor(dt1$mass_formula)) 
summary(as.factor(dt1$legnth_reference)) 
summary(as.factor(dt1$mass_formula_reference)) 
summary(as.factor(dt1$notes))
detach(dt1)               

# ---- start RRR processing ----

zoop <- dt1

library(data.table)
library(stringr)

zoop <- as.data.table(zoop)

zoop <- zoop[larger_group != ""] # extra blank lines in the download

colnames(zoop)[colnames(zoop) == "length"] <- "length.mm" # length is in millimeters

# OK the formulas are excel formulas. the C1 C2 C3 etc is the length column. the length column is the average length of the organisms. 
# this is gonna be really annoying... they are not all the same format, either, and there are no spaces to use as delimiters
# plan: merge the tables in R, then calculate in excel, then export results back to R omfg

# need a species name with the matching capitalizations.

zoop[ ,species_name := str_to_title(species)]

saveRDS(object = zoop, file = "data_input/0p_zoop_conversions_knb-lter-ntl.376.2.rds")
