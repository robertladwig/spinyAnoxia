# RRR

# # plan: merge the formula table with the full table in R, export as csv
# # open the merged table in excel, run the excel-formatted formula cells (yeah.)
# # read in the formatted excel file and save it as an unchanging R object
# 
# library(data.table)
# 
# # ---- merge tables ----
# 
# zoops <- readRDS(file = "data_input/0o_zoop_knb-lter-ntl.90.33.rds")
# conversions <- readRDS(file = "data_input/0p_zoop_conversions_knb-lter-ntl.376.2.rds")
# 
# # check that names match for merging
# 
# names.zoops <- unique(zoops$species_name)
# names.conv <- unique(conversions$species_name) # way more!
# setdiff(x = names.zoops, y = names.conv)
# grep(pattern = "Daphnia", x = unique(conversions$species_name), ignore.case = T, value = T)
# # this is weird that it's missing Daphnia Mendotae and Pulicaria... just the most important ones??
# setdiff(y = names.zoops, x = names.conv)
# 
# 
# # ok, missing daphnia AND bytho... probably just do zoop counts for now.



# New Plan: use Jake's conversion script ------------

zoops <- readRDS(file = "data_input/0o_zoop_knb-lter-ntl.90.33.rds")

# ---- Jake Walsh's function ----

#zoop_ldw = zooplankton length to dry weight function
#l = length in mm, species = species name --> dw = dry weight in ug
zoop_ldw <- function(l, species){
  bosmina.shaped <- c("ALONA", "ACROPERUS", "BOSMINA", "CAMPTOCERCUS", "EURYCERCUS", "MACROTHRI",
                      "SCAPHOLEBERIS", "SIDA CRYSTALLINA", "OPHRYOXUS", "LEYDIGIA",
                      "PLEUROXUS", "SIMOCEPHALUS", "CLADOCERAN")
  rotifera <- c("ANURAEOPSIS", "ASCOMORPHA", "ASPLANCHNA", "BRACHIONUS", "CEPHALODELLA",
                "COLLOTHECA", "COLURELLA", "CONOCHIL", "ENCENTRUM", "EUCHLANIS",
                "FILINIA", "GASTROPUS", "HEXARTHA", "ILYOCRYPTUS", "KELLICOTTIA", "KERATELLA",
                "LECANE", "LEPADELLA", "MANFREDIUM", "MONOMMATA", "MONOSTYLA", "MYTILINA",
                "NOTHOLCA", "NOTOMMATA", "PLOESOMA", "POLYARTHRA", "POMPHOLYX",
                "SCAPHOLEBERIS", "SYNCHAETA", "TESTUDINELLA", "TRICHO", "TROCHO", 
                "UNIDENTIFIED", "UNKNOWN", "BLANK")
  calanoida <- c("DIAPT", "LIMNOCALANUS", "CALA", "EPISCH")
  nauplii <- c("COPEPODITES", "NAUP")
  cyclopoida <- c("CYCLO")
  other.copepoda <- c("ERGASILIUS", "HARPACTICOID")
  sm.cladocera <- c(bosmina.shaped, "CHYD", "DIAPH", "HOLO")
  pred.cladocera <- c("BYTHO", "POLYPHEMUS", "LEPTODORA")
  daphnia <- c("DAPH")
  
  dw <- numeric(length(l))
  for(i in 1:length(l)){
    
    species.i <- NA
    
    if(grepl("CYCLO", species[i], ignore.case = T)){
      species.i <- "CYCLO"
    }
    
    if(grepl("COPEPODITES", species[i], ignore.case = T)){
      species.i <- "CYCLO"
    }
    
    if(grepl("HARPACTICOID", species[i], ignore.case = T)){
      species.i <- "CYCLO"
    }
    
    if(grepl("ERGASILUS", species[i], ignore.case = T)){
      species.i <- "CYCLO"
    }
    
    if(grepl("DIAPT", species[i], ignore.case = T)){
      species.i <- "DIAPT"
    }
    
    if(grepl("CHYD", species[i], ignore.case = T)){
      species.i <- "CHYD"
    }
    
    if(grepl("DAPHNIA", species[i], ignore.case = T)){
      species.i <- "DAPHNIA"
    }
    
    if(grepl("DAPHNIA PULICARIA", species[i], ignore.case = T)){
      species.i <- "DAPHNIA PULICARIA"
    }
    
    if(grepl("DIAPH", species[i], ignore.case = T)){
      species.i <- "DIAPH"
    }
    
    if(grepl("BOSM", species[i], ignore.case = T)){
      species.i <- "BOSM"
    }
    
    if(grepl("LIMNOCALANUS", species[i], ignore.case = T)){
      species.i <- "DIAPT"
    }
    
    if(grepl("EPISCH", species[i], ignore.case = T)){
      species.i <- "DIAPT"
    }
    
    if(grepl("CALA", species[i], ignore.case = T)){
      species.i <- "DIAPT"
    }
    
    if(grepl("HOLOPEDIUM", species[i], ignore.case = T)){
      species.i <- "HOLO"
    }
    
    if(grepl("BYTHO", species[i], ignore.case = T)){
      species.i <- "BYTHOTREPHES"
    }
    
    if(grepl("POLYPHEMUS", species[i], ignore.case = T)){
      species.i <- "BYTHOTREPHES"
    }
    
    if(grepl("LEPTODORA", species[i], ignore.case = T)){
      species.i <- "LEPTODORA"
    }
    
    if(grepl("NAUP", species[i], ignore.case = T)){
      species.i <- "NAUP"
    }
    
    if(grepl(paste(bosmina.shaped, collapse = "|"), species[i], ignore.case = T)){
      species.i <- "BOSM"
    }
    
    if(grepl(paste(rotifera, collapse = "|"), species[i], ignore.case = T)){
      species.i <- NA
    }
    
    species_list <- c("DIAPH","BOSM","DAPHNIA PULICARIA","DAPHNIA",
                      "CYCLO","DIAPT","NAUP","LEPTODORA","BYTHOTREPHES", 
                      "CHYD", "HOLO", "MISC")
    
    alpha_vec <- c(5.07463,15.0533,10.674,5.48,7.8279,3.4740,2.0091,0.43955,9.49,
                   exp(4.453), exp(2.073), 9.4)
    beta_vec <- c(3.0468,2.5294,2.093,2.200,2.553,2.2634,0.469,2.67,3.11,
                  3.64, 3.19, 2.6)
    
    l.i <- l[i]
    
    if(is.na(species.i)){
      dw[i] <- alpha_vec[species_list == "MISC"]*(l.i^beta_vec[species_list == "MISC"])
    }else{
      dw[i] <- alpha_vec[species_list == species.i]*(l.i^beta_vec[species_list == species.i])
    }
    
  }
  
  return(dw)
  
}

# ---- add est. dry weight to table ----

zoops$est.dry.wt.ug <- zoop_ldw(l = zoops$avg_length.mm, species = zoops$species_name)

zoops$Biomass.mg.L <- (zoops$est.dry.wt.ug * 1e-3) * zoops$density.num.m3 * (1/1000) # 1m3 / 1000L

# ---- save data ----

saveRDS(object = zoops, file = "data_processed/0q_zoop_abundances.rds")
