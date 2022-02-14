#load all functions
devtools::load_all()

#to use pipe operator
library(magrittr)

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




### keep only opcodes that are available in meta in data (fl and maxn) 

# fl
fl <- keep_opcode_in_meta_pelagic(meta, fl)

# maxn
maxn <- keep_opcode_in_meta_pelagic(meta, maxn)



### remove from a data (fl or maxn) the opcode not available in the other data (maxn or fl), ie the single opcodes

#fl
fl <- remove_single_opcodes_pelagic(fl, maxn)

#maxn
maxn <- remove_single_opcodes_pelagic(maxn, fl)

#checks
length(unique(fl$NewOpCode))
length(unique(maxn$NewOpCode)) 



### assign empty opcodes in meta
meta <- assign_empty_opcodes_meta_pelagic(meta, maxn)



### Add mean fork length data to maxn data based on a hierarchy for pelagic bruvs
maxn <- add_mean_fl_to_maxn_data_pelagic(fl, maxn)
  
  
### Add fork length data for individuals counted in maxn but with no available fork length
### for these individuals we assign the mean fork length calculated in maxn data  
### for pelagic bruvs

fl <- add_individual_fl_data_pelagic(fl, maxn)





 ########################################################################################################################################
########### benthic bruvs

#read
expeds <- readxl::read_excel(here::here("data", "benthic", "All expeditions 2021_09_16 FIN.xlsx"), sheet = 1)

fl <- readxl::read_excel(here::here("data", "benthic", "BenthicFL.xlsx"), sheet = 1)

maxn <- readxl::read_excel(here::here("data", "benthic", "BenthicMaxN.xlsx"), sheet = 1)

meta <- readxl::read_excel(here::here("data", "benthic", "BenthicMeta.xlsx"), sheet = 1)


#select expeds based on permission = yes
expeds %>% 
  dplyr::rename("NewExped" = "New Exped") -> expeds

expeds_permitted <- expeds$NewExped[expeds$Permission == "Yes"]


#clean meta
meta %>% 
  #select columns
  dplyr::select("NewOpcode",	"New Exped", "Date", "Year", "Month", "Lat", "Long", "Time In",	"USE?",	"Location", "Site", 
                "Biotic", "Substrate") %>% 
  #rename columns
  dplyr::rename("exped" ="New Exped",
                "use" ="USE?",
                "time_in" = "Time In") %>% 
  #remove one expedition not available in maxn or fl
  dplyr::filter(exped != "Cape Howe_2006") %>% 
  #remove nas
  tidyr::drop_na() %>% 
  #remove invalid stations
  dplyr::filter(use == "Yes") %>% 
  #select expeditions
  dplyr::filter(exped %in% expeds_permitted) -> meta


#clean fl
fl %>% 
  #select columns
  dplyr::select("Sample ID",  "Expedition", "Family", "Genus", "Binomial", "Length (mm)") %>% 
  #rename columns
  dplyr::rename("NewOpCode" = "Sample ID",
                "Lengthmm" = "Length (mm)") %>% 
  #calculate length in cm
  dplyr::mutate(Lengthcm = Lengthmm / 1000) %>% 
  dplyr::select(-"Lengthmm") %>% 
  #select expeditions
  dplyr::filter(Expedition %in% expeds_permitted) %>% 
  #replace juvenile and unknown in species and genus by NA
  dplyr::mutate(Binomial = dplyr::na_if(Binomial, "Juvenile sp")) %>% 
  dplyr::mutate(Genus = dplyr::na_if(Genus, "Juvenile")) %>% 
  dplyr::mutate(Binomial = dplyr::na_if(Binomial, "Unknown sp")) %>% 
  dplyr::mutate(Binomial = dplyr::na_if(Binomial, "Unknown 1")) %>% 
  dplyr::mutate(Binomial = dplyr::na_if(Binomial, "Unknown Gadidae")) %>% 
  dplyr::mutate(Genus = dplyr::na_if(Genus, "Unknown")) %>% 
  dplyr::mutate(Genus = dplyr::na_if(Genus, "sp")) -> fl


#clean maxn
maxn %>% 
  #select columns
  dplyr::select(c("SampleID", "Exped", "Family", "Genus", "Binomial", "MaxN", "FL (cm)", "nFL", "source")) %>% 
  #rename columns
  dplyr::rename("NewOpCode" = "SampleID",
                "FLcm" = "FL (cm)") %>% 
  #select expeditions
  dplyr::filter(Exped %in% expeds_permitted) %>% 
  #replace juvenile and unknown in species and genus by NA
  dplyr::mutate(Binomial = dplyr::na_if(Binomial, "Juvenile sp")) %>% 
  dplyr::mutate(Genus = dplyr::na_if(Genus, "Juvenile")) %>% 
  dplyr::mutate(Binomial = dplyr::na_if(Binomial, "Unknown sp")) %>% 
  dplyr::mutate(Binomial = dplyr::na_if(Binomial, "Unknown 1")) %>% 
  dplyr::mutate(Binomial = dplyr::na_if(Binomial, "Unknown Gadidae")) %>% 
  dplyr::mutate(Genus = dplyr::na_if(Genus, "Unknown")) %>% 
  dplyr::mutate(Genus = dplyr::na_if(Genus, "sp")) %>% 
  #ignore NAs in maxN
  tidyr::drop_na(MaxN) %>% 
  #remove duplicates
  dplyr::distinct() ->  maxn



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
  

fl %>% 
  #select families
  dplyr::filter(Family %in% families) -> dat_families


#retrieve individuals with unknown family but available species name
fl %>% 
  dplyr::filter(Family %in% c("Unknown", "Juvenile")) %>% 
  dplyr::filter(is.na(Binomial) == FALSE)  -> dat_unknown

# bind
new <- rbind(dat_unknown, dat_families)


  
  
#check that same expeds in fl and maxn
length(unique(fl$Expedition))
length(unique(maxn$Exped)) #10 exped in maxn but not fl

setdiff(maxn$Exped, fl$Expedition)

setdiff(meta$exped, maxn$Exped)

setdiff(meta$exped, fl$Expedition)






#look for duplicates opcodes and add a suffix - report to fl and maxn













