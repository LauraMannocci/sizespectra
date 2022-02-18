#load all functions
devtools::load_all()

#to use pipe operator
library(magrittr)


########################################################################################################################################
# read and clean bruvs data
########################################################################################################################################


########################################################################################################################################
########### pelagic bruvs


### read pelagic bruvs data

meta <- read_meta_pelagic()
maxn <- read_maxn_pelagic()
fl <- read_fl_pelagic()


### clean data

# clean maxn
maxn <- clean_maxn_pelagic(maxn)

# clean fl
fl <- clean_fl_pelagic(fl)

# clean meta
meta <- clean_meta_pelagic(meta)



### select fish families

fish_families <- c("Nomeidae", "Istiophoridae", "Fistulariidae", "Coryphaenidae", "Scombridae", "Carangidae", "Myliobatidae", 
                   "Echeneidae", "Gerreidae", "Carcharhinidae", "Sphyrnidae", "Exocoetidae", "Monacanthidae", "Lamnidae", 
                   "Balistidae", "Molidae", "Caproidae", "Centriscidae", "Centrolophidae", "Dasyatidae", "Clupeidae", 
                   "Blenniidae", "Mullidae", "Priacanthidae", "Sphyraenidae", "Apogonidae", "Tetraodontidae", 
                   "Pomacentridae", "Lutjanidae", "Labridae", "Syngnathidae", "Sparidae", "Kyphosidae", "Rachycentridae", 
                   "Belonidae", "Acanthuridae", "Caesionidae", "Aulostomidae", "Platax", "Engraulidae", "Chanidae", 
                   "Nematistiidae", "Lobotidae", "Macroramphosidae", "Ephippidae")

# fl
fl <- select_fish_families_pelagic(fl, fish_families) 

# maxn
maxn <- select_fish_families_pelagic(maxn, fish_families) 




### keep only opcodes that are available in meta in fl and maxn

# fl
fl <- keep_opcode_in_meta(meta, fl)

# maxn
maxn <- keep_opcode_in_meta(meta, maxn)



### remove from fl the opcodes not available in maxn, ie the single opcodes

#fl
fl <- remove_single_opcodes(fl, maxn)

#number of opcodes
length(unique(fl$NewOpCode))
length(unique(maxn$NewOpCode))

#number of strings
length(unique(fl$String))
length(unique(maxn$String))



### assign empty opcodes in meta
meta <- assign_empty_opcodes_meta_pelagic(meta, maxn)



### write meta opcodes for pelagic bruvs
write_meta_opcodes_pelagic(meta)



### write meta strings for pelagic bruvs
write_meta_strings_pelagic(meta)



### Add mean fork length data to maxn data based on a hierarchy for pelagic bruvs
maxn <- add_mean_fl_to_maxn_data_pelagic(fl, maxn)


### Add fork length data for individuals counted in maxn but with no available fork length
### for these individuals we assign the mean fork length calculated in maxn data  
### for pelagic bruvs

fl <- add_individual_fl_data_pelagic(fl, maxn)



### Estimate weight from observed length for a taxa using rfishbase
fl <- estimate_weight_from_length(fl)



### rename objects before saving
maxn_pelagic = maxn
fl_pelagic = fl
meta_pelagic = meta



### save objects
save(maxn_pelagic, fl_pelagic, meta_pelagic, file = here::here("1_read_clean_pelagic.RData"))



########################################################################################################################################
########### benthic bruvs

### read benthic bruvs data

expeds <- read_exped()
fl <- read_fl_benthic()
maxn <- read_maxn_benthic()
meta <- read_meta_benthic()



### clean data

#clean meta
meta <- clean_meta_benthic(meta)

#clean fl
fl <- clean_fl_benthic(fl)

#clean maxn
maxn <- clean_maxn_benthic(maxn)



### remove expeditions that are not permitted to be used in this study

#select expeds based on permission
expeds_notpermitted <- expeds$`New Exped`[expeds$Permission == "No"]


#maxn
maxn <- removed_notpermitted_expeditions_benthic(maxn, expeds_notpermitted)

#fl
fl <- removed_notpermitted_expeditions_benthic(fl, expeds_notpermitted)

#meta
meta <- removed_notpermitted_expeditions_benthic(meta, expeds_notpermitted)




### select fish families

fish_families <- c("Glaucosomatidae", "Labridae", "Lethrinidae", "Pomacanthidae", "Scaridae", "Serranidae", "Sparidae", "Carangidae",   
                   "Chaetodontidae", "Haemulidae", "Kyphosidae", "Mullidae", "Pomacentridae", "Dasyatidae", "Acanthuridae", "Muraenidae",   
                   "Siganidae","Pseudochromidae", "Monacanthidae", "Tetraodontidae","Heterodontidae", "Lutjanidae", "Aulopidae", "Orectolobidae", "Clupeidae",    
                   "Nemipteridae",  "Enoplosidae", "Myliobatidae",  "Scorpaenidae", "Scorpididae",   "Rhinidae", "Carcharhinidae","Sphyrnidae",   
                   "Arripidae","Caesionidae",  "Scombridae",    "Gobiidae", "Pinguipedidae", "Cheilodactylidae", "Aracanidae",    "Microcanthidae",
                   "Platycephalidae", "Rhinobatidae", "Urolophidae",   "Ephippidae",   "Synodontidae",  "Blenniidae",   "Caesioscorpididae", "Apogonidae",   "Plesiopidae",   
                   "Echeneidae",   "Cirrhitidae",   "Girellidae",   "Neosebastidae", "Scyliorhinidae", "Lamnidae", "Hemigaleidae",  "Sillaginidae",  "Grammistidae", 
                   "Sphyraenidae",  "Pempheridae",  "Gerreidae","Alopiidae",  "Odacidae", "Sebastidae",   "Pleuronectidae", "Parascylliidae", "Rachycentridae","Zanclidae",    
                   "Antennariidae", "Veliferidae",  "Triakidae","Terapontidae", "Berycidae","Dinolestidae", "Diodontidae",   "Oplegnathidae", "Pristiophoridae",                                  
                   "Chironemidae",  "Gempylidae",    "Mobulidae",    "Balistidae",    "Cheloniidae",  "Holocentridae",  "Aulostomidae",  "Ginglymostomatidae",                              
                   "Ostraciidae",   "Malacanthidae", "Clinidae", "Microdesmidae", "Stegostomatidae", "Fistulariidae", "Belonidae",  "Albulidae","Priacanthidae",
                   "Hemiscylliidae","Tripterygiidae", "Odontaspididae","Plotosidae",   "Elopidae", "Paralichthyidae", "Bothidae", "Ariidae", "Latidae",  "Oneirodidae",  
                   "Syngnathidae",  "Callionymidae", "Opistognathidae", "Centriscidae", "Trichonotidae", "Pristidae",   "Congridae","Chanidae", "Samaridae",
                   "Aplodactylidae", "Zeidae",   "Melanostomiidae", "Monodactylidae","Leiognathidae", "Atherinidae",   "Hemiramphidae", "Pentacerotidae",                                 
                   "Triglidae", "Rhinopteridae", "Istiophoridae", "Soleidae", "Pimelodidae",  "Chirocentridae","Chimaeridae",  "Chlorophthalmidae",
                   "Squalidae",    "Polymixiidae",  "Centrophoridae", "Macrouridae",   "Centrolophidae", "Polyprionidae", "Zeniontidae", "Hexanchidae",   
                   "Euclichthyidae",  "Torpedinidae",  "Macroramphosidae", "Rajidae",  "Trichiuridae", "Myxinidae","Trachichthyidae", "Etmopteridae",  "Ophidiidae",   
                   "Moridae",  "Molidae",  "Acropomatidae", "Myctophidae",  "Tetrarogidae",  "Salangidae",   "Scatophagidae", "Acanthoclinidae" , "Ophichthidae",  
                   "Gobiesocidae", "Carapidae","Peristediidae", "labridae", "carangidae",  "balistidae",    "cirrhitidae",   "Polynemidae",   "Mugilidae",    
                   "Eleotridae",    "Tetrabrachiidae" , "caesionidae",   "serranidae",   "Trygonorrhinidae", "Sciaenidae",   "Pomatomidae",   "Stomiidae",    
                   "Rhombosoleidae","Monocentridae", "Rhyncobatus",  "Glaucostegidae", "ACANTHURIDAE",  "BALISTIDAE", "BLENNIDEA","LABRIDAE", "MULLIDAE", 
                   "PINGUIPEDIDAE", "POMACENTRIDAE", "SERRANIDAE",   "CARANGIDAE",    "CHAETODONTIDAE", "LETHRINIDAE",   "MOBULIDAE", "NEMIPTERIDAE",  
                   "POMACANTHIDAE", "SCARIDAE", "SIGANIDAE",  "TETRAODONTIDAE","CIRRHITIDAE",  "DASYATIDAE",    "HOLOCENTRIDAE", "LUTJANIDAE",  "ANTHIINAE", 
                   "CAESIONIDAE",   "GOBIIDAE", "PSEUDOCHROMIDAE", "MURAENIDAE", "HAEMULIDAE",  "EPHIPPIDAE", "MICRODESMIDAE", "SPHYRAENIDAE", "MONACANTHIDAE", 
                   "MALACANTHIDAE", "DIODONTIDAE",   "ECHENEIDIDAE", "SPARIDAE", "OSTRACIIDAE",   "KYPHOSIDAE",    "SYNODONTIDAE",  "BLENNIDAE",  
                   "CHARCHARHINIDAE", "PEMPHERIDAE",  "APLOACTINIDAE", "CONGRIDAE", "PRIACANTHIDAE", "HEMIRAMPHIDAE", "SCORPAENIDAE",  "APOGONIDAE",   
                   "FISTULARIIDAE", "TORPEDINIDAE",  "Gadidae", "Lotidae", "Cottidae",   "Trachinidae",  "Anarhichadidae")                                  


#maxn
maxn = select_fish_families_benthic(maxn, fish_families)


#fl
fl = select_fish_families_benthic(fl, fish_families)




### keep only opcodes that are available in meta in fl and maxn

# fl
fl <- keep_opcode_in_meta(meta, fl)

# maxn
maxn <- keep_opcode_in_meta(meta, maxn)



### remove from fl the opcodes not available in maxn, ie the single opcodes

#fl
fl <- remove_single_opcodes(fl, maxn)

#number of opcodes
length(unique(fl$NewOpCode))
length(unique(maxn$NewOpCode))



### assign empty opcodes in meta
meta <- assign_empty_opcodes_meta_benthic(meta, maxn)



### write meta opcodes for benthic bruvs
write_meta_opcodes_benthic(meta)



### Add mean fork length data to maxn data based on a hierarchy for benthic bruvs
maxn <- add_mean_fl_to_maxn_data_benthic(fl, maxn)





### Add fork length data for individuals counted in maxn but with no available fork length
### for these individuals we assign the mean fork length calculated in maxn data  
### for benthic bruvs

fl <- add_individual_fl_data_benthic(fl, maxn)



### Estimate weight from observed length for a taxa using rfishbase
fl <- estimate_weight_from_length(fl)



### rename objects before saving
maxn_benthic = maxn
fl_benthic = fl
meta_benthic = meta



### save objects
save(maxn_benthic, fl_benthic, meta_benthic, file = here::here("1_read_clean_benthic.RData"))




