#' Read meta data for pelagic bruvs
#'
#' @param  
#'
#' @return
#' @export

read_meta_pelagic <- function(){
  
  readxl::read_excel(here::here("data", "pelagic", "Pelagic compilation archive 2020_05_26.xlsx"), 
                     sheet = 1)

}





#' Read maxn data for pelagic bruvs
#'
#' @param  
#'
#' @return
#' @export
#'

read_maxn_pelagic <- function(){
  
  readxl::read_excel(here::here("data", "pelagic", "Pelagic compilation archive 2020_05_26.xlsx"), 
                   sheet = 2)
  
}


#' Get missing maxn data for pelagic bruvs (2 expeditions)
#'
#' @param  
#'
#' @return
#' @export

get_missing_maxn_pelagic <- function(){
  
  data <- readxl::read_excel(here::here("data", "pelagic", "TBL gal gct.xlsx"), 
                             sheet = "MaxN")
  data %>% 
    dplyr::filter(Exped %in% c("Galapagos 2019", "Gracetown 2020")) -> new
  
  return(new)
  
}


#' Read fl data for pelagic bruvs
#'
#' @param  
#'
#' @return
#' @export
#'

read_fl_pelagic <- function(){
  
  readxl::read_excel(here::here("data", "pelagic", "Pelagic compilation archive 2020_05_26.xlsx"), 
                   sheet = 3)
  
}


#' Get missing fl data for pelagic bruvs (2 expeditions)
#'
#' @param  
#'
#' @return
#' @export

get_missing_fl_pelagic <- function(){
  
  data <- readxl::read_excel(here::here("data", "pelagic", "TBL gal gct.xlsx"), 
                             sheet = "FL")
  
  data %>% 
    dplyr::filter(Exped %in% c("Galapagos 2019", "Gracetown 2020")) -> new
  
  return(new)
  
}


#' Clean maxn data for pelagic bruvs
#'
#' @param dat 
#'
#' @return
#' @export
#'

clean_maxn_pelagic <- function(dat){
  
  dat %>% 
    #select columns 
    dplyr::select("New OpCode", "String", "Exped", "Location", "Family", "Binomial", "MaxN") %>% 
    #rename
    dplyr::rename("NewOpCode" = "New OpCode") %>% 
    #replace one expedition name
    dplyr::mutate(Exped = stringr::str_replace(Exped, "Wandoo 2017 autumn", "Wandoo autumn 2017")) %>%
    #extract last four letters of expedition to get year
    dplyr::mutate(Year = stringr::str_sub(Exped, start=-4)) %>% 
    #add Genus column (original genus column is wrong)
    dplyr::mutate(Genus = stringr::word(Binomial, 1)) %>% 
    #correct one species name
    dplyr::mutate(Binomial = stringr::str_replace(Binomial, "Remora Remora", "Remora remora")) %>% 
    #replace juvenile and unknown in species and genus by NA
    dplyr::mutate(Binomial = dplyr::na_if(Binomial, "Juvenile sp")) %>% 
    dplyr::mutate(Binomial = dplyr::na_if(Binomial, "juvenile sp")) %>% 
    dplyr::mutate(Genus = dplyr::na_if(Genus, "Juvenile")) %>% 
    dplyr::mutate(Genus = dplyr::na_if(Genus, "juvenile")) %>% 
    dplyr::mutate(Binomial = dplyr::na_if(Binomial, "Unknown sp")) %>% 
    dplyr::mutate(Binomial = dplyr::na_if(Binomial, "Unknown sp1")) %>% 
    dplyr::mutate(Binomial = dplyr::na_if(Binomial, "Unknown sp2")) %>% 
    dplyr::mutate(Binomial = dplyr::na_if(Binomial, "Unknown sp3")) %>% 
    dplyr::mutate(Genus = dplyr::na_if(Genus, "Unknown")) %>% 
    #select columns (to reorder)
    dplyr::select("NewOpCode", "String", "Exped", "Year", "Location", "Family", "Genus", "Binomial", "MaxN") %>% 
    #ignore NAs in maxN
    tidyr::drop_na(MaxN) %>% 
    #remove duplicates
    dplyr::distinct() -> new
  
  return(new)
  
}


#' Clean fl data for pelagic bruvs
#'
#' @param dat 
#'
#' @return
#' @export
#'
#'
clean_fl_pelagic <- function(dat){
  
  dat %>% 
    #select columns
    dplyr::select("New OpCode", "String", "Exped", "Location", "Family", "Binomial", "Length (cm)") %>% 
    #add Genus column (original genus column is wrong)
    dplyr::mutate(Genus = stringr::word(Binomial, 1)) %>% 
    #rename columns
    dplyr::rename("NewOpCode" = "New OpCode",
                  "Lengthcm" = "Length (cm)") %>% 
    #replace one expedition name
    dplyr::mutate(Exped = stringr::str_replace(Exped, "Wandoo 2017 autumn", "Wandoo autumn 2017")) %>%
    #extract last four letters of expedition to get year
    dplyr::mutate(Year = stringr::str_sub(Exped, start=-4)) %>% 
    #correct one species name
    dplyr::mutate(Binomial = stringr::str_replace(Binomial, "Remora Remora", "Remora remora")) %>% 
    #correct sp1/sp2
    dplyr::mutate(Binomial = stringr::str_replace(Binomial, "sp1", "sp")) %>%
    dplyr::mutate(Binomial = stringr::str_replace(Binomial, "sp2", "sp")) %>%
    #correct unknown families
    dplyr::mutate(Family = ifelse(Binomial == "Cantherhines fronticinctus", "Monacanthidae", Family)) %>% 
    dplyr::mutate(Family = ifelse(Binomial == "Decapterus sp", "Carangidae", Family)) %>% 
    dplyr::mutate(Family = ifelse(Binomial == "Carangidae sp", "Carangidae", Family)) %>% 	
    dplyr::mutate(Family = ifelse(Binomial == "Pristiapogon exostigma", "Apogonidae", Family)) %>% 
    dplyr::mutate(Family = ifelse(Binomial == "Psenes sp", "Nomeidae", Family)) %>% 
    #replace juvenile and unknown in species and genus by NA
    dplyr::mutate(Binomial = dplyr::na_if(Binomial, "Juvenile sp")) %>% 
    dplyr::mutate(Binomial = dplyr::na_if(Binomial, "juvenile sp")) %>% 
    dplyr::mutate(Genus = dplyr::na_if(Genus, "Juvenile")) %>% 
    dplyr::mutate(Genus = dplyr::na_if(Genus, "juvenile")) %>% 
    dplyr::mutate(Binomial = dplyr::na_if(Binomial, "Unknown sp")) %>% 
    dplyr::mutate(Binomial = dplyr::na_if(Binomial, "Unknown sp1")) %>% 
    dplyr::mutate(Binomial = dplyr::na_if(Binomial, "Unknown sp2")) %>% 
    dplyr::mutate(Binomial = dplyr::na_if(Binomial, "Unknown sp3")) %>% 
    dplyr::mutate(Genus = dplyr::na_if(Genus, "Unknown")) %>% 
    #select columns (to reorder)
    dplyr::select("NewOpCode", "String", "Exped", "Year", "Location", "Family", "Genus", "Binomial", "Lengthcm") %>% 
    #remove opcodes with no available string or expedition (these opcodes are not listed in meta)
    tidyr::drop_na() -> new
  
  return(new)
  
}



#' Bind pelagic (maxn or fl) data
#'
#' @param dat1 
#' @param dat2 
#'
#' @return
#' @export
#'

bind_pelagic <- function(dat1, dat2){
  
  rbind(dat1, dat2) -> new
  
  return(new)
  
}
  
  


#' Clean meta data for pelagic bruvs
#'
#' @param dat 
#'
#' @return
#' @export
#'

clean_meta_pelagic <- function(dat){
  
  dat %>% 
    #select columns
    dplyr::select("New OpCode",	"5STRING",	"Exped.", "Location", "Sublocation",	"USE?",	"Grounded?", "Date",	"Year",
                  "Month",	"Long in",	"Lat in",	"Time In") %>% 
    #rename columns
    dplyr::rename("NewOpCode" = "New OpCode",
                  "string" = "5STRING",
                  "exped" ="Exped.",
                  "use" ="USE?",
                  "grounded" ="Grounded?",
                  "lon_in" = "Long in",
                  "lat_in" = "Lat in" , 
                  "time_in" = "Time In") %>% 
    #get month
    dplyr::mutate(Month = format(Date, "%m")) %>% 
    #reformat time in 
    dplyr::mutate(time_in = strftime(time_in, format="%H:%M:%S")) %>% 
    #remove invalid stations
    dplyr::filter(is.na(use) | use != "No") %>% 
    dplyr::filter(is.na(grounded) | grounded == "no") %>% 
    #drop nas
    tidyr::drop_na(NewOpCode) -> new
  
  return(new)
  
}



#' select fish families in pelagic bruvs (fl/maxn data)
#'
#' @param dat 
#' @param families 
#'
#' @return
#' @export
#'

select_fish_families_pelagic <- function(dat, families){
  
  dat %>% 
    #select families
    dplyr::filter(Family %in% families) -> dat_families
  
  #retrieve individuals with unknown family but available species name
  dat %>% 
    dplyr::filter(Family == "Unknown") %>% 
    dplyr::filter(is.na(Binomial) == FALSE)  -> dat_unknown
  
  # bind
  new <- rbind(dat_unknown, dat_families)

  return(new)
  
}






#' keep only opcodes that are available in meta in data (fl or maxn) for pelagic or benthic bruvs
#'
#' @param meta_dat 
#' @param dat 
#'
#' @return
#' @export
#'

keep_opcode_in_meta <- function(meta_dat, dat){
    
  # get opcodes available in meta
  opcode_meta <- unique(meta_dat$NewOpCode)
  
  # get opcodes available in dat
  opcode_dat <- unique(dat$NewOpCode)
  
  cat("nb of differing opcodes:", length(opcode_meta) - length(opcode_dat))
  
  # keep only opcodes that are available in meta
  dat %>% 
    dplyr::filter(NewOpCode %in% opcode_meta) -> new
  
  return(new)
  
}
    

#' remove from fl the opcodes not available in maxn, ie the single opcodes, for pelagic or benthic bruvs
#'
#' @param dat_fl 
#' @param dat_maxn 
#'
#' @return
#' @export
#'

remove_single_opcodes <- function(dat_fl, dat_maxn){
  
  # get opcodes in dat_fl but not in dat_maxn
  opcodes <- setdiff(dat_fl$NewOpCode, dat_maxn$NewOpCode)
  
  dat_fl %>% 
    #remove opcodes that are in dat_fl but not in dat_maxn
    dplyr::filter(!NewOpCode %in% opcodes) -> new
  
  cat("nb of unique opcodes removed from data:", length(unique(dat_fl$NewOpCode)) - length(unique(new$NewOpCode)))
  
  return(new)
  
}
  
    

#' Assign empty opcodes in meta for pelagic bruvs
#'
#' @param dat_meta 
#' @param dat_maxn 
#'
#' @return
#' @export
#'

assign_empty_opcodes_meta_pelagic <- function(dat_meta, dat_maxn){
    
  #get opcodes in meta but not in maxn -> these opcodes have no individual seen
  opcode_in_meta_only <- setdiff(dat_meta$NewOpCode, dat_maxn$NewOpCode)
  
  #get strings in meta but not in maxn -> these strings have no individual seen
  string_in_meta_only <- setdiff(dat_meta$string, dat_maxn$String)
  
  #proportion of empty strings
  cat("proportion of presumably empty strings", length(string_in_meta_only) / length(unique(dat_meta$string)), sep="\n")
  
  #proportion of empty opcodes
  cat("proportion of presumably empty opcodes",  length(opcode_in_meta_only) / length(unique(dat_meta$NewOpCode)), sep="\n")
  
  #add column "empty" to meta assigned to yes when no individuals were seen 
  dat_meta %>% 
    dplyr::mutate(empty = dplyr::case_when(NewOpCode %in% opcode_in_meta_only ~ "yes", 
                                           !NewOpCode %in% opcode_in_meta_only ~ "no")) -> new
  
  return(new)
  
}



#' Add mean fork length data to maxn data based on a hierarchy for pelagic bruvs
#'
#' @param dat_fl 
#' @param dat_maxn 
#'
#' @return
#' @export
#'

add_mean_fl_to_maxn_data_pelagic <- function (dat_fl, dat_maxn) {
  
  ##### filter nmax data with/without species
  
  dat_maxn %>% 
    dplyr::filter(!is.na(Binomial)) -> dat_maxn_species
  
  dat_maxn %>% 
    dplyr::filter(is.na(Binomial)) -> dat_maxn_no_species
  
  
  ###### case when species exists
  
  #calculate mean fl per species
  
  #calculate mean fl per species per opcode
  dat_fl %>% 
    dplyr::group_by(NewOpCode, Binomial) %>% 
    dplyr::summarise(mean_sp_opcode = mean(Lengthcm)) -> mean_sp_opcode
  
  #calculate mean fl per species per string
  dat_fl %>% 
    dplyr::group_by(String, Binomial) %>% 
    dplyr::summarise(mean_sp_string = mean(Lengthcm)) -> mean_sp_string
  
  #calculate mean fl per species per expedition
  dat_fl %>% 
    dplyr::group_by(Exped, Binomial) %>% 
    dplyr::summarise(mean_sp_exped = mean(Lengthcm)) -> mean_sp_exped
  
  #calculate mean fl per species per expedition year
  dat_fl %>%
    dplyr::group_by(Year, Binomial) %>%
    dplyr::summarise(mean_sp_exped_year = mean(Lengthcm)) -> mean_sp_exped_year
  
  #calculate mean fl per species for all expeditions
  dat_fl %>% 
    dplyr::group_by(Binomial) %>% 
    dplyr::summarise(mean_sp_all_exped = mean(Lengthcm)) -> mean_sp_all_exped
  
  #calculate mean fl per genus for all expeditions
  dat_fl %>% 
    dplyr::group_by(Genus) %>% 
    dplyr::summarise(mean_genus_all_exped = mean(Lengthcm)) -> mean_genus_all_exped
  
  #calculate mean fl per family for all expeditions
  dat_fl %>% 
    dplyr::group_by(Family) %>% 
    dplyr::summarise(mean_family_all_exped = mean(Lengthcm)) -> mean_family_all_exped
  
  #calculate mean fl for whole assemblage for all expeditions
  mean_assemblage = mean(dat_fl$Lengthcm, na.rm=T)
  
  
  #add mean fl to maxn based on hierarchy
  
  dat_maxn_species %>% 
    # join mean_opcode 
    dplyr::left_join(mean_sp_opcode, by = c("NewOpCode", "Binomial")) %>% 
    # join mean_string 
    dplyr::left_join(mean_sp_string, by = c("String", "Binomial")) %>% 
    # join mean_exped 
    dplyr::left_join(mean_sp_exped, by = c("Exped", "Binomial"))  %>% 
    # join mean_exped_year
    dplyr::left_join(mean_sp_exped_year, by = c("Year", "Binomial"))  %>% 
    # join mean_all_exped 
    dplyr::left_join(mean_sp_all_exped, by = c("Binomial"))  %>% 
    # join mean_all_exped genus
    dplyr::left_join(mean_genus_all_exped, by = c("Genus"))  %>% 
    # join mean_all_exped family
    dplyr::left_join(mean_family_all_exped, by = c("Family"))  %>% 
    # add mean_assemblage
    dplyr::mutate(mean_assemblage = mean_assemblage) %>%
    # fill mean length column hierachically for that individual 
    dplyr::mutate("mean_fl" = ifelse(!is.na(mean_sp_opcode), 
                                     mean_sp_opcode, 
                                     ifelse(!is.na(mean_sp_string),
                                            mean_sp_string,
                                            ifelse(!is.na(mean_sp_exped),
                                                   mean_sp_exped,
                                                   ifelse(!is.na(mean_sp_exped_year),
                                                          mean_sp_exped_year,
                                                          ifelse(!is.na(mean_sp_all_exped),
                                                                 mean_sp_all_exped,
                                                                 ifelse(!is.na(mean_genus_all_exped),
                                                                        mean_genus_all_exped,
                                                                        ifelse(!is.na(mean_family_all_exped),
                                                                               mean_family_all_exped,
                                                                               mean_assemblage))))))))  %>%    
    #select columns
    dplyr::select("NewOpCode", "String", "Exped", "Year", "Location", "Family", "Genus", "Binomial", "MaxN", "mean_fl") -> dat_maxn_species
  
  
  ###### case when species does not exist
  
  #calculate mean fl per family
  
  #calculate mean fl per family per opcode
  dat_fl %>% 
    dplyr::group_by(NewOpCode, Family) %>% 
    dplyr::summarise(mean_fam_opcode = mean(Lengthcm)) -> mean_fam_opcode
  
  #calculate mean fl per Family per string
  dat_fl %>% 
    dplyr::group_by(String, Family) %>% 
    dplyr::summarise(mean_fam_string = mean(Lengthcm)) -> mean_fam_string
  
  #calculate mean fl per Family per expedition
  dat_fl %>% 
    dplyr::group_by(Exped, Family) %>% 
    dplyr::summarise(mean_fam_exped = mean(Lengthcm)) -> mean_fam_exped
  
  #calculate mean fl per Family per expedition year
  dat_fl %>%
    dplyr::group_by(Year, Family) %>%
    dplyr::summarise(mean_fam_exped_year = mean(Lengthcm)) -> mean_fam_exped_year
  
  #calculate mean fl per Family for all expeditions
  dat_fl %>% 
    dplyr::group_by(Family) %>% 
    dplyr::summarise(mean_fam_all_exped = mean(Lengthcm)) -> mean_fam_all_exped
  
  #calculate mean fl for whole assemblage for all expeditions
  mean_assemblage = mean(dat_fl$Lengthcm)
  
  
  #add mean fl to maxn based on hierarchy
  
  dat_maxn_no_species %>% 
    # join mean_opcode 
    dplyr::left_join(mean_fam_opcode, by = c("NewOpCode", "Family")) %>% 
    # join mean_string 
    dplyr::left_join(mean_fam_string, by = c("String", "Family")) %>% 
    # join mean_exped 
    dplyr::left_join(mean_fam_exped, by = c("Exped", "Family"))  %>% 
    # join mean_exped_year
    dplyr::left_join(mean_fam_exped_year, by = c("Year", "Family"))  %>% 
    # join mean_all_exped family
    dplyr::left_join(mean_fam_all_exped, by = c("Family"))  %>% 
    # add mean_assemblage
    dplyr::mutate(mean_assemblage = mean_assemblage) %>%
    # fill mean length column hierachically for that individual 
    dplyr::mutate("mean_fl" = ifelse(!is.na(mean_fam_opcode), 
                                     mean_fam_opcode, 
                                     ifelse(!is.na(mean_fam_string),
                                            mean_fam_string,
                                            ifelse(!is.na(mean_fam_exped),
                                                   mean_fam_exped,
                                                   ifelse(!is.na(mean_fam_exped_year),
                                                          mean_fam_exped_year,
                                                          ifelse(!is.na(mean_fam_all_exped),
                                                                 mean_fam_all_exped,
                                                                 mean_assemblage)))))) %>% 
    #select columns
    dplyr::select("NewOpCode", "String", "Exped", "Year", "Location", "Family", "Genus", "Binomial", "MaxN", "mean_fl") -> dat_maxn_no_species
    
  ###bind nmax data with and without species
  rbind(dat_maxn_species, dat_maxn_no_species) -> dat_maxn
  
  return(dat_maxn)
  
}



#' Add fork length data for individuals counted in maxn but with no available fork length
#' for these individuals we assign the mean fork length calculated in maxn data  
#' for pelagic bruvs
#'
#' @param dat_fl 
#' @param dat_maxn 
#'
#' @return
#' @export
#'

add_individual_fl_data_pelagic <- function (dat_fl, dat_maxn) {
    
  #add rows to fl data for individuals with missing length 
  
  #add computed column to dat_fl
  dat_fl %>% 
    dplyr::mutate("computed" = "no") -> dat_fl
  
  #initiatlize empty raw
  r0 <- data.frame("NewOpCode" = "initial",
                   "String" = "initial",
                   "Exped" = "initial",
                   "Year" = "initial",
                   "Location" = "initial",
                   "Family" = "initial",
                   "Genus" = "initial",
                   "Binomial" = "initial",
                   "Lengthcm" = "initial", 
                   "computed" = "initial")
  
  #loop on opcodes
  for (op in (unique(dat_maxn$NewOpCode))){
    cat("-----------------opcode", op, "\n")
    
    #for given opcode loop on species 
    for (sp in unique(dat_maxn$Binomial[dat_maxn$NewOpCode == op])){
      cat("-----------------species", sp, "\n") 
      
      #get fl data corresponding to given opcode and species
      datfl = subset(dat_fl, dat_fl$NewOpCode == op & dat_fl$Binomial == sp)
      nfl = nrow(datfl)
      
      #get maxn data corresponding to given opcode and species
      datmaxn = subset(dat_maxn, dat_maxn$NewOpCode == op & dat_maxn$Binomial == sp)
      
      #retrieve maxn value if available, otherwise set maxn to 0
      if (nrow(datmaxn) >= 1){
        valmaxn <- datmaxn$MaxN
      }else{
        valmaxn <- 0
      }
      
      #handle duplicated maxn (if there are 2 available maxn, keep larger one)
      if (length(valmaxn) > 1){ 
        valmaxn = max(valmaxn)
      }
      
      #calculate difference between maxn and nb of length measures
      d = valmaxn - nfl
      cat("difference between maxn and nb of length measures is", d, "\n")
      
      #create rows corresponding to individuals with missing length and set their length to mean length calculated in maxn (mean_fl)
      if (d > 0) {
        #case when the opcode exists in fl
        if (nfl != 0){
          #create row without length
          r = unique(datfl[, c("NewOpCode", "String", "Exped", "Year", "Location", "Family", "Genus", "Binomial")]) 
          #add length column to that row
          r$Lengthcm <- unique(datmaxn$mean_fl)
          #add computed column to that row
          r$computed <- "yes"
          #bind the rows d times
          r <- do.call("rbind", replicate(d, r, simplify = FALSE))
          #add these rows to previous rows
          r0 <- rbind(r0, r)
        }
        #case when opcode does not exist in fl
        if (nfl == 0){
          #create row without length
          r <- data.frame("NewOpCode" = op,
                           "String" = dat_maxn$String[dat_maxn$NewOpCode == op],
                           "Exped" = dat_maxn$Exped[dat_maxn$NewOpCode == op],
                           "Year" = dat_maxn$Year[dat_maxn$NewOpCode == op],
                           "Location" = dat_maxn$Location[dat_maxn$NewOpCode == op],
                           "Family" = dat_maxn$Family[dat_maxn$NewOpCode == op],
                           "Genus" = dat_maxn$Genus[dat_maxn$NewOpCode == op],
                           "Binomial" = dat_maxn$Binomial[dat_maxn$NewOpCode == op])
          #add length column to that row
          r$Lengthcm <- unique(datmaxn$mean_fl)
          #add computed column to that row
          r$computed <- "yes"
          #bind the rows d times
          r <- do.call("rbind", replicate(d, r, simplify = FALSE))
          #add these rows to previous rows
          r0 <- rbind(r0, r)
        }
      }
    }
  }
  
  #clean r0
  r0 %>% 
    dplyr::filter(NewOpCode != "initial") %>% 
    dplyr::mutate(Lengthcm = as.numeric(Lengthcm)) -> r0
  
  #bind these rows to fl
  rbind(dat_fl, r0) -> new
  
  cat("nb of added fl rows: ",  nrow(r0))
  
  #plot length distributions
  png(here::here("outputs", "pelagic", "pelagic_length_distributions.png"))
  par(mfrow=c(3,1))
  hist(log(dat_fl$Lengthcm), ylim = c(0, 22000), main = "former fl data")
  hist(log(r0$Lengthcm), ylim = c(0, 22000), main = "added fl data")
  hist(log(new$Lengthcm), ylim = c(0, 22000), main = "new fl data (former + added)")
  dev.off()
  
  return(new)
  
}



#' Write meta opcodes for pelagic bruvs
#'
#' @param dat 
#'
#' @return
#' @export
#'

write_meta_opcodes_pelagic <- function (dat) {
  
  write.csv(dat, here::here("outputs", "pelagic", "pelagic_opcodes.csv"), row.names = FALSE)
  
}




#' Write meta strings for pelagic bruvs
#'
#' @param dat 
#'
#' @return
#' @export
#'

write_meta_strings_pelagic <- function (dat) {
  
  #get strings
  dat %>% 
    dplyr::select("string", "exped", "Location", "Sublocation", "use", "grounded", "Date", "Year", "Month", "lon_in", "lat_in", "time_in", "empty") %>% 
    dplyr::group_by(string) %>% 
    #keep information (lon lat time) from first row
    dplyr::slice(1) -> dat
    
  write.csv(dat, here::here("outputs", "pelagic", "pelagic_strings.csv"), row.names = FALSE)
  
}



#' Read meta data for benthic bruvs
#'
#' @param  
#'
#' @return
#' @export

read_meta_benthic <- function(){
  
  readxl::read_excel(here::here("data", "benthic", "BenthicMeta.xlsx"), 
                     sheet = 1)
  
}

#' Read fl data for benthic bruvs
#'
#' @param  
#'
#' @return
#' @export

read_fl_benthic <- function(){
  
  readxl::read_excel(here::here("data", "benthic", "BenthicFL.xlsx"), 
                     sheet = 1)
  
}



#' Read fl data for benthic bruvs
#'
#' @param  
#'
#' @return
#' @export

read_maxn_benthic <- function(){
  
  readxl::read_excel(here::here("data", "benthic", "BenthicMaxN.xlsx"), 
                     sheet = 1)
  
}


#' Read exped data (all bruvs)
#'
#' @param  
#'
#' @return
#' @export

read_exped <- function(){
  
  readxl::read_excel(here::here("data", "All expeditions 2021_09_16 FIN.xlsx"), 
                     sheet = 1)
  
}



#' Clean meta data for benthic bruvs
#'
#' @param dat 
#'
#' @return
#' @export
#'

clean_meta_benthic <- function(dat){
    
  dat %>% 
    #select columns
    dplyr::select("NewOpcode",	"New Exped", "Date", "Year", "Month", "Lat", "Long", "Time In",	"USE?",	"Location", "Site", 
                  "Biotic", "Substrate") %>% 
    #rename columns
    dplyr::rename("Exped" ="New Exped",
                  "use" ="USE?",
                  "time_in" = "Time In",
                  "NewOpCode" = "NewOpcode",
                  "lat_in" = "Lat", 
                  "lon_in" = "Long") %>% 
    #reformat time in 
    dplyr::mutate(time_in = strftime(time_in, format="%H:%M:%S")) %>% 
    #get year and month
    dplyr::mutate(Year = format(Date, "%Y")) %>% 
    dplyr::mutate(Month = format(Date, "%m")) %>% 
    #correct years
    dplyr::mutate(Year = stringr::str_replace(Year, "10420", "2016")) %>%
    dplyr::mutate(Year = stringr::str_replace(Year, "84316", "2016")) %>%
    #remove one expedition not available in maxn or fl
    dplyr::filter(Exped != "Cape Howe_2006") %>% 
    #remove na lat/lon
    tidyr::drop_na(lat_in) %>% 
    tidyr::drop_na(lon_in) %>% 
    #remove na date
    tidyr::drop_na(Date) %>% 
    #remove invalid coordinates
    dplyr::filter(!NewOpCode %in% c("BRUV1_26102015", "SBB09_173")) %>% 
    #remove invalid stations
    dplyr::filter(is.na(use) | use != "No") %>% 
    #handle one erroneous exped 
    dplyr::mutate(Exped = dplyr::case_when(NewOpCode == "BRUV2_07112016" ~ "Sudan_2016",
                                           NewOpCode != "BRUV2_07112016" ~ Exped)) %>% 
    # remove duplicates
    dplyr::distinct() %>% 
    #rename duplicate opcodes related to HitraFroya
    dplyr::mutate(NewOpCode = dplyr::case_when(NewOpCode == "BRUV3_23102016" & time_in == "09:24:21" ~ "BRUV3_23102016_bis",
                                               NewOpCode == "2N09" & Exped == "HitraFrøya2020" ~ "2N09_bis", 
                                               NewOpCode == "2N14" &  Exped == "HitraFrøya2020" ~  "2N14_bis",
                                              NewOpCode == "2A12" &  Exped == "HitraFrøya2020" ~  "2A12_bis",
                                              NewOpCode == "2A13" &  Exped == "HitraFrøya2020" ~  "2A13_bis",
                                              NewOpCode == "2C01" &  Exped == "HitraFrøya2020" ~  "2C01_bis",
                                              NewOpCode == "2C10" &  Exped == "HitraFrøya2020" ~  "2C10_bis",
                                              NewOpCode == "2K08" &  Exped == "HitraFrøya2020" ~  "2K08_bis",
                                              NewOpCode == "2K07" &  Exped == "HitraFrøya2020" ~  "2K07_bis",
                                              NewOpCode == "2M08" &  Exped == "HitraFrøya2020" ~  "2M08_bis",
                                              NewOpCode == "2M11" &  Exped == "HitraFrøya2020" ~  "2M11_bis",
                                              NewOpCode == "2B05" &  Exped == "HitraFrøya2020" ~  "2B05_bis",
                                              TRUE ~ NewOpCode)) -> new
  return(new)
  
}







#' Clean fl data for benthic bruvs
#'
#' @param dat 
#'
#' @return
#' @export
#'

clean_fl_benthic <- function(dat){
  
  dat %>% 
    #select columns
    dplyr::select("Sample ID",  "Expedition", "Family", "Genus", "Binomial", "Length (mm)") %>% 
    #rename columns
    dplyr::rename("NewOpCode" = "Sample ID",
                  "Lengthmm" = "Length (mm)",
                  "Exped" = "Expedition") %>% 
    #add Genus column (original genus column is wrong)
    dplyr::mutate(Genus = stringr::word(Binomial, 1)) %>% 
    #calculate length in cm
    dplyr::mutate(Lengthcm = Lengthmm / 10) %>% 
    dplyr::select(-"Lengthmm") %>% 
    #replace juvenile and unknown in species and genus by NA
    dplyr::mutate(Binomial = dplyr::na_if(Binomial, "Juvenile sp")) %>% 
    dplyr::mutate(Genus = dplyr::na_if(Genus, "Juvenile")) %>% 
    dplyr::mutate(Binomial = dplyr::na_if(Binomial, "Unknown sp")) %>% 
    dplyr::mutate(Binomial = dplyr::na_if(Binomial, "Unknown 1")) %>% 
    dplyr::mutate(Binomial = dplyr::na_if(Binomial, "Unknown Gadidae")) %>% 
    dplyr::mutate(Genus = dplyr::na_if(Genus, "Unknown")) %>% 
    dplyr::mutate(Genus = dplyr::na_if(Genus, "sp")) %>% 
    #correct species names
    dplyr::mutate(Binomial = stringr::str_replace(Binomial, "variola louti", "Variola louti")) %>%
    dplyr::mutate(Binomial = stringr::str_replace(Binomial, "thalassoma lunare", "Thalassoma lunare")) %>%
    dplyr::mutate(Binomial = stringr::str_replace(Binomial, "plectropomus pessuliferus", "Plectropomus pessuliferus")) %>%
    dplyr::mutate(Binomial = stringr::str_replace(Binomial, "plectropomus leopardus", "Plectropomus leopardus")) %>%
    dplyr::mutate(Binomial = stringr::str_replace(Binomial, "pomacentrus indicus", "Pomacentrus indicus")) %>%
    dplyr::mutate(Binomial = stringr::str_replace(Binomial, "pentapodus paradiseus", "Pentapodus paradiseus")) %>%
    dplyr::mutate(Binomial = stringr::str_replace(Binomial, "mulloidichthys flavolineatus", "Mulloidichthys flavolineatus")) %>%
    dplyr::mutate(Binomial = stringr::str_replace(Binomial, "meiacanthus smithi", "Meiacanthus smithi")) %>%
    dplyr::mutate(Binomial = stringr::str_replace(Binomial, "macolor sp", "Macolor sp")) %>%
    dplyr::mutate(Binomial = stringr::str_replace(Binomial, "lethrinus microdon", "Lethrinus microdon")) %>%
    dplyr::mutate(Binomial = stringr::str_replace(Binomial, "gymnocranius grandoculis", "Gymnocranius grandoculis")) %>%
    dplyr::mutate(Binomial = stringr::str_replace(Binomial, "girella tephraeops", "Girella tephraeops")) %>%
    dplyr::mutate(Binomial = stringr::str_replace(Binomial, "decapterus sp", "Decapterus sp")) %>%
    dplyr::mutate(Binomial = stringr::str_replace(Binomial, "ctenochaetus striatus", "Ctenochaetus striatus")) %>%
    dplyr::mutate(Binomial = stringr::str_replace(Binomial, "coris auricularis", "Coris auricularis")) %>%
    dplyr::mutate(Binomial = stringr::str_replace(Binomial, "chromis viridis", "Chromis viridis")) %>%
    dplyr::mutate(Binomial = stringr::str_replace(Binomial, "Caesio caerulaurea ", "Caesio caerulaurea")) %>%
    dplyr::mutate(Binomial = stringr::str_replace(Binomial, "blenniidae sp", "Blenniidae sp")) %>%
    dplyr::mutate(Binomial = stringr::str_replace(Binomial, "amblygobius semicinctus", "Amblygobius semicinctus")) %>%
    dplyr::mutate(Binomial = stringr::str_replace(Binomial, "amblyeleotris aurora", "Amblyeleotris aurora")) %>%
    #create year column from exped column
    dplyr::mutate(Year = unlist(stringi::stri_extract_all(Exped, regex = "[0-9]{4}"))) %>% 
    #correct expedition names
    dplyr::mutate(Exped = stringr::str_replace(Exped, "Dampier Pluto 2008", "Dampier (Pluto)_2008")) %>%
    dplyr::mutate(Exped = stringr::str_replace(Exped, "Barrow 2010", "Barrow_2010")) %>%
    dplyr::mutate(Exped = stringr::str_replace(Exped, "Barrow 2009", "Barrow_2009")) %>%
    dplyr::mutate(Exped = stringr::str_replace(Exped, "Barrow 2008", "Barrow_2008")) %>%
    dplyr::mutate(Exped = stringr::str_replace(Exped, "Wandoo_2017_05_s", "Wandoo_2017_05s")) %>%
    dplyr::mutate(Exped = stringr::str_replace(Exped, "Wandoo_2017_09_s", "Wandoo_2017_09s")) %>%
    dplyr::mutate(Exped = stringr::str_replace(Exped, "Wandoo_2018_04_s", "Wandoo_2018_04s")) %>%
    dplyr::mutate(Exped = stringr::str_replace(Exped, "Wandoo_2018_09_s", "Wandoo_2018_09s")) %>%
    dplyr::mutate(Exped = stringr::str_replace(Exped, "Wandoo_2019_04_s", "Wandoo_2019_04s")) %>%
    dplyr::mutate(Exped = stringr::str_replace(Exped, "Wandoo_2019_09_s", "Wandoo_2019_09s")) %>%
    dplyr::mutate(Exped = stringr::str_replace(Exped, "Sudan2015", "Sudan_2015")) %>%
    dplyr::mutate(Exped = stringr::str_replace(Exped, "Sudan2016", "Sudan_2016")) %>%
    dplyr::mutate(Exped = stringr::str_replace(Exped, "Sudan2017", "Sudan_2017")) %>%
    #convert stations to upper case for New Caledonia and French Polynesia expeditions
    dplyr::mutate(NewOpCode = dplyr::case_when(Exped %in% c("New Caledonia_2012", "New Caledonia_2013", "New Caledonia_2014",  "French Polynesia_2013") ~ stringr::str_to_upper(NewOpCode),
                                               !Exped %in% c("New Caledonia_2012", "New Caledonia_2013", "New Caledonia_2014",  "French Polynesia_2013") ~ NewOpCode)) %>% 
    #change station names for Dampier (Pluto)_2008 expedition
    plyr::mutate(NewOpCode = dplyr::case_when(Exped == "Dampier (Pluto)_2008" ~ paste0("PLU08_", stringr::str_sub(NewOpCode, start = -3)),
                                              Exped != "Dampier (Pluto)_2008" ~ NewOpCode))  %>% 
    #rename duplicate opcodes related to HitraFroya
    dplyr::mutate(NewOpCode = dplyr::case_when(NewOpCode == "2N09" & Exped == "HitraFrøya2020" ~ "2N09_bis", 
                                               NewOpCode == "2N14" &  Exped == "HitraFrøya2020" ~  "2N14_bis",
                                               NewOpCode == "2A12" &  Exped == "HitraFrøya2020" ~  "2A12_bis",
                                               NewOpCode == "2A13" &  Exped == "HitraFrøya2020" ~  "2A13_bis",
                                               NewOpCode == "2C01" &  Exped == "HitraFrøya2020" ~  "2C01_bis",
                                               NewOpCode == "2C10" &  Exped == "HitraFrøya2020" ~  "2C10_bis",
                                               NewOpCode == "2K08" &  Exped == "HitraFrøya2020" ~  "2K08_bis",
                                               NewOpCode == "2K07" &  Exped == "HitraFrøya2020" ~  "2K07_bis",
                                               NewOpCode == "2M08" &  Exped == "HitraFrøya2020" ~  "2M08_bis",
                                               NewOpCode == "2M11" &  Exped == "HitraFrøya2020" ~  "2M11_bis",
                                               NewOpCode == "2B05" &  Exped == "HitraFrøya2020" ~  "2B05_bis",
                                               TRUE ~ NewOpCode)) %>% 
    #remove two erroneous species record
    dplyr::filter(!(Binomial == "Pastinachus sephen" & 	NewOpCode == "PLU08_127")) %>% 
    dplyr::filter(!(Binomial == "Aethaloperca rogaa" & 	NewOpCode == "BRUV6_28102016")) %>% 
    #rename duplicate opcodes related to opcode BRUV3_23102016 : assign half species to each duplicate
    dplyr::mutate(NewOpCode = dplyr::case_when(NewOpCode == "BRUV3_23102016" & Binomial %in% c("Acanthurus nigrofuscus", "Balistapus undulatus", "Caesio striata", "Halichoeres scapularis", 
                                                                                               "Thalassoma lunare", "Parupeneus forsskali", "Chromis dimidiata", "Cephalopholis hemistiktos") ~ "BRUV3_23102016_bis",
                                               TRUE ~ NewOpCode)) -> new
  
  return(new)
  
}



#' Clean maxn data for benthic bruvs
#'
#' @param dat 
#'
#' @return
#' @export
#'

clean_maxn_benthic <- function(dat){
  
  dat %>% 
    #select columns
    dplyr::select(c("SampleID", "Exped", "Family", "Genus", "Binomial", "MaxN")) %>% 
    #rename columns
    dplyr::rename("NewOpCode" = "SampleID") %>% 
    #add Genus column (original genus column is wrong)
    dplyr::mutate(Genus = stringr::word(Binomial, 1)) %>% 
    #replace juvenile and unknown in species and genus by NA
    dplyr::mutate(Binomial = dplyr::na_if(Binomial, "Juvenile sp")) %>% 
    dplyr::mutate(Genus = dplyr::na_if(Genus, "Juvenile")) %>% 
    dplyr::mutate(Binomial = dplyr::na_if(Binomial, "Unknown sp")) %>% 
    dplyr::mutate(Binomial = dplyr::na_if(Binomial, "Unknown 1")) %>% 
    dplyr::mutate(Binomial = dplyr::na_if(Binomial, "Unknown Gadidae")) %>% 
    dplyr::mutate(Genus = dplyr::na_if(Genus, "Unknown")) %>% 
    dplyr::mutate(Genus = dplyr::na_if(Genus, "sp")) %>% 
    #correct species names
    dplyr::mutate(Binomial = stringr::str_replace(Binomial, "variola louti", "Variola louti")) %>%
    dplyr::mutate(Binomial = stringr::str_replace(Binomial, "thalassoma lunare", "Thalassoma lunare")) %>%
    dplyr::mutate(Binomial = stringr::str_replace(Binomial, "plectropomus pessuliferus", "Plectropomus pessuliferus")) %>%
    dplyr::mutate(Binomial = stringr::str_replace(Binomial, "plectropomus leopardus", "Plectropomus leopardus")) %>%
    dplyr::mutate(Binomial = stringr::str_replace(Binomial, "pomacentrus indicus", "Pomacentrus indicus")) %>%
    dplyr::mutate(Binomial = stringr::str_replace(Binomial, "pentapodus paradiseus", "Pentapodus paradiseus")) %>%
    dplyr::mutate(Binomial = stringr::str_replace(Binomial, "mulloidichthys flavolineatus", "Mulloidichthys flavolineatus")) %>%
    dplyr::mutate(Binomial = stringr::str_replace(Binomial, "meiacanthus smithi", "Meiacanthus smithi")) %>%
    dplyr::mutate(Binomial = stringr::str_replace(Binomial, "macolor sp", "Macolor sp")) %>%
    dplyr::mutate(Binomial = stringr::str_replace(Binomial, "lethrinus microdon", "Lethrinus microdon")) %>%
    dplyr::mutate(Binomial = stringr::str_replace(Binomial, "gymnocranius grandoculis", "Gymnocranius grandoculis")) %>%
    dplyr::mutate(Binomial = stringr::str_replace(Binomial, "girella tephraeops", "Girella tephraeops")) %>%
    dplyr::mutate(Binomial = stringr::str_replace(Binomial, "decapterus sp", "Decapterus sp")) %>%
    dplyr::mutate(Binomial = stringr::str_replace(Binomial, "ctenochaetus striatus", "Ctenochaetus striatus")) %>%
    dplyr::mutate(Binomial = stringr::str_replace(Binomial, "coris auricularis", "Coris auricularis")) %>%
    dplyr::mutate(Binomial = stringr::str_replace(Binomial, "chromis viridis", "Chromis viridis")) %>%
    dplyr::mutate(Binomial = stringr::str_replace(Binomial, "Caesio caerulaurea ", "Caesio caerulaurea")) %>%
    dplyr::mutate(Binomial = stringr::str_replace(Binomial, "blenniidae sp", "Blenniidae sp")) %>%
    dplyr::mutate(Binomial = stringr::str_replace(Binomial, "amblygobius semicinctus", "Amblygobius semicinctus")) %>%
    dplyr::mutate(Binomial = stringr::str_replace(Binomial, "amblyeleotris aurora", "Amblyeleotris aurora")) %>%
    #create year column from exped column
    dplyr::mutate(Year = unlist(stringi::stri_extract_all(Exped, regex = "[0-9]{4}"))) %>% 
    #correct expedition names
    dplyr::mutate(Exped = stringr::str_replace(Exped, "Wandoo_2017_05_s", "Wandoo_2017_05s")) %>%
    dplyr::mutate(Exped = stringr::str_replace(Exped, "Wandoo_2017_09_s", "Wandoo_2017_09s")) %>%
    dplyr::mutate(Exped = stringr::str_replace(Exped, "Wandoo_2018_04_s", "Wandoo_2018_04s")) %>%
    dplyr::mutate(Exped = stringr::str_replace(Exped, "Wandoo_2018_09_s", "Wandoo_2018_09s")) %>%
    dplyr::mutate(Exped = stringr::str_replace(Exped, "Wandoo_2019_04_s", "Wandoo_2019_04s")) %>%
    dplyr::mutate(Exped = stringr::str_replace(Exped, "Wandoo_2019_09_s", "Wandoo_2019_09s")) %>%
    dplyr::mutate(Exped = stringr::str_replace(Exped, "Sudan2015", "Sudan_2015")) %>%
    dplyr::mutate(Exped = stringr::str_replace(Exped, "Sudan2016", "Sudan_2016")) %>%
    dplyr::mutate(Exped = stringr::str_replace(Exped, "Sudan2017", "Sudan_2017")) %>%
    #convert stations to upper case for New Caledonia and French Polynesia expeditions
    dplyr::mutate(NewOpCode = dplyr::case_when(Exped %in% c("New Caledonia_2012", "New Caledonia_2013", "New Caledonia_2014",  "French Polynesia_2013") ~ stringr::str_to_upper(NewOpCode),
                                               !Exped %in% c("New Caledonia_2012", "New Caledonia_2013", "New Caledonia_2014",  "French Polynesia_2013") ~ NewOpCode)) %>% 
    #handle one erroneous exped
    dplyr::mutate(Exped = dplyr::case_when(NewOpCode == "BRUV2_07112016" ~ "Sudan_2016",
                                           NewOpCode != "BRUV2_07112016" ~ Exped)) %>% 
    #ignore NAs in maxN
    tidyr::drop_na(MaxN) %>% 
    #remove duplicates
    dplyr::distinct() %>% 
    #rename duplicate opcodes related to HitraFroya
    dplyr::mutate(NewOpCode = dplyr::case_when(NewOpCode == "2N09" & Exped == "HitraFrøya2020" ~ "2N09_bis", 
                                               NewOpCode == "2N14" &  Exped == "HitraFrøya2020" ~  "2N14_bis",
                                               NewOpCode == "2A12" &  Exped == "HitraFrøya2020" ~  "2A12_bis",
                                               NewOpCode == "2A13" &  Exped == "HitraFrøya2020" ~  "2A13_bis",
                                               NewOpCode == "2C01" &  Exped == "HitraFrøya2020" ~  "2C01_bis",
                                               NewOpCode == "2C10" &  Exped == "HitraFrøya2020" ~  "2C10_bis",
                                               NewOpCode == "2K08" &  Exped == "HitraFrøya2020" ~  "2K08_bis",
                                               NewOpCode == "2K07" &  Exped == "HitraFrøya2020" ~  "2K07_bis",
                                               NewOpCode == "2M08" &  Exped == "HitraFrøya2020" ~  "2M08_bis",
                                               NewOpCode == "2M11" &  Exped == "HitraFrøya2020" ~  "2M11_bis",
                                               NewOpCode == "2B05" &  Exped == "HitraFrøya2020" ~  "2B05_bis",
                                               TRUE ~ NewOpCode)) %>% 
    #rename duplicate opcodes related to opcode BRUV3_23102016 : assign half species to each duplicate
    dplyr::mutate(NewOpCode = dplyr::case_when(NewOpCode == "BRUV3_23102016" & Binomial %in% c("Acanthurus nigrofuscus", "Balistapus undulatus", "Caesio striata", "Halichoeres scapularis", 
                                                                                               "Thalassoma lunare", "Parupeneus forsskali", "Chromis dimidiata", "Cephalopholis hemistiktos") ~ "BRUV3_23102016_bis",
                                               TRUE ~ NewOpCode)) -> new
  
  return(new)
  
}



#' Removed expeditions that are not permitted to be used in this study for benthic bruvs
#'
#' @param dat 
#' @param expeds_permit 
#'
#' @return
#' @export
#'

removed_notpermitted_expeditions_benthic <- function(dat, expeds_notpermit){
  
  #select expeditions
  dat %>% 
    dplyr::filter(!Exped %in% expeds_notpermit) -> new
  
  return(new)
  
}
    





#' select fish families in benthic bruvs (fl/maxn data)
#'
#' @param dat 
#' @param families 
#'
#' @return
#' @export
#'

select_fish_families_benthic <- function(dat, families){
    
  dat %>% 
    #select families
    dplyr::filter(Family %in% families) %>% 
    #convert families to lower case with first lettr as upper case 
    dplyr::mutate(Family = stringr::str_to_title(Family)) -> dat_families
  
  #retrieve individuals with unknown family but available species name
  dat %>% 
    dplyr::filter(Family %in% c("Unknown", "Juvenile")) %>% 
    dplyr::filter(is.na(Binomial) == FALSE)  -> dat_unknown
  
  # bind
  rbind(dat_unknown, dat_families) -> new
  
  return(new)
  
}





#' Assign empty opcodes in meta for benthic bruvs
#'
#' @param dat_meta 
#' @param dat_maxn 
#'
#' @return
#' @export
#'

assign_empty_opcodes_meta_benthic <- function(dat_meta, dat_maxn){
  
  #get opcodes in meta but not in maxn -> these opcodes have no individual seen
  opcode_in_meta_only <- setdiff(dat_meta$NewOpCode, dat_maxn$NewOpCode)
  
  #proportion of empty opcodes
  cat("proportion of presumably empty opcodes",  length(opcode_in_meta_only) / length(unique(dat_meta$NewOpCode)), sep="\n")
  
  #add column "empty" to meta assigned to yes when no individuals were seen 
  dat_meta %>% 
    dplyr::mutate(empty = dplyr::case_when(NewOpCode %in% opcode_in_meta_only ~ "yes", 
                                           !NewOpCode %in% opcode_in_meta_only ~ "no")) -> new
  
  return(new)
  
}





#' Add mean fork length data to maxn data based on a hierarchy for benthic bruvs
#'
#' @param dat_fl 
#' @param dat_maxn 
#'
#' @return
#' @export
#'

add_mean_fl_to_maxn_data_benthic <- function (dat_fl, dat_maxn) {
  
  ##### filter nmax data with/without species
  
  dat_maxn %>% 
    dplyr::filter(!is.na(Binomial)) -> dat_maxn_species
  
  dat_maxn %>% 
    dplyr::filter(is.na(Binomial)) -> dat_maxn_no_species
  
  
  ###### case when species exists
  
  #calculate mean fl per species
  
  #calculate mean fl per species per opcode
  dat_fl %>% 
    dplyr::group_by(NewOpCode, Binomial) %>% 
    dplyr::summarise(mean_sp_opcode = mean(Lengthcm)) -> mean_sp_opcode
  
  #calculate mean fl per species per expedition
  dat_fl %>% 
    dplyr::group_by(Exped, Binomial) %>% 
    dplyr::summarise(mean_sp_exped = mean(Lengthcm)) -> mean_sp_exped
  
  #calculate mean fl per species per expedition year
  dat_fl %>%
    dplyr::group_by(Year, Binomial) %>%
    dplyr::summarise(mean_sp_exped_year = mean(Lengthcm)) -> mean_sp_exped_year
  
  #calculate mean fl per species for all expeditions
  dat_fl %>% 
    dplyr::group_by(Binomial) %>% 
    dplyr::summarise(mean_sp_all_exped = mean(Lengthcm)) -> mean_sp_all_exped
  
  #calculate mean fl per genus for all expeditions
  dat_fl %>% 
    dplyr::group_by(Genus) %>% 
    dplyr::summarise(mean_genus_all_exped = mean(Lengthcm)) -> mean_genus_all_exped
  
  #calculate mean fl per family for all expeditions
  dat_fl %>% 
    dplyr::group_by(Family) %>% 
    dplyr::summarise(mean_family_all_exped = mean(Lengthcm)) -> mean_family_all_exped
  
  #calculate mean fl for whole assemblage for all expeditions
  mean_assemblage = mean(dat_fl$Lengthcm, na.rm=T)
  
  
  #add mean fl to maxnbased on hierarchy
  
  dat_maxn_species %>% 
    # join mean_opcode 
    dplyr::left_join(mean_sp_opcode, by = c("NewOpCode", "Binomial")) %>% 
    # join mean_exped 
    dplyr::left_join(mean_sp_exped, by = c("Exped", "Binomial"))  %>% 
    # join mean_exped_year
    dplyr::left_join(mean_sp_exped_year, by = c("Year", "Binomial"))  %>% 
    # join mean_all_exped 
    dplyr::left_join(mean_sp_all_exped, by = c("Binomial"))  %>% 
    # join mean_all_exped genus
    dplyr::left_join(mean_genus_all_exped, by = c("Genus"))  %>% 
    # join mean_all_exped family
    dplyr::left_join(mean_family_all_exped, by = c("Family"))  %>% 
    # add mean_assemblage
    dplyr::mutate(mean_assemblage = mean_assemblage) %>%
    # fill mean length column hierachically for that individual 
    dplyr::mutate("mean_fl" = ifelse(!is.na(mean_sp_opcode), 
                                     mean_sp_opcode, 
                                     ifelse(!is.na(mean_sp_exped),
                                            mean_sp_exped,
                                            ifelse(!is.na(mean_sp_exped_year),
                                                   mean_sp_exped_year,
                                                   ifelse(!is.na(mean_sp_all_exped),
                                                          mean_sp_all_exped,
                                                          ifelse(!is.na(mean_genus_all_exped),
                                                                 mean_genus_all_exped,
                                                                 ifelse(!is.na(mean_family_all_exped),
                                                                        mean_family_all_exped,
                                                                        mean_assemblage)))))))  %>%    
    #select columns
    dplyr::select("NewOpCode", "Exped", "Year", "Family", "Genus", "Binomial", "MaxN", "mean_fl") -> dat_maxn_species
  
  
  ###### case when species does not exist
  
  #calculate mean fl per family
  
  #calculate mean fl per family per opcode
  dat_fl %>% 
    dplyr::group_by(NewOpCode, Family) %>% 
    dplyr::summarise(mean_fam_opcode = mean(Lengthcm)) -> mean_fam_opcode
  
  #calculate mean fl per Family per expedition
  dat_fl %>% 
    dplyr::group_by(Exped, Family) %>% 
    dplyr::summarise(mean_fam_exped = mean(Lengthcm)) -> mean_fam_exped
  
  #calculate mean fl per Family per expedition year
  dat_fl %>%
    dplyr::group_by(Year, Family) %>%
    dplyr::summarise(mean_fam_exped_year = mean(Lengthcm)) -> mean_fam_exped_year
  
  #calculate mean fl per Family for all expeditions
  dat_fl %>% 
    dplyr::group_by(Family) %>% 
    dplyr::summarise(mean_fam_all_exped = mean(Lengthcm)) -> mean_fam_all_exped
  
  #calculate mean fl for whole assemblage for all expeditions
  mean_assemblage = mean(dat_fl$Lengthcm, na.rm=T)
  
  
  #add mean fl to maxn based on hierarchy
  
  dat_maxn_no_species %>% 
    # join mean_opcode 
    dplyr::left_join(mean_fam_opcode, by = c("NewOpCode", "Family")) %>% 
    # join mean_exped 
    dplyr::left_join(mean_fam_exped, by = c("Exped", "Family"))  %>% 
    # join mean_exped_year
    dplyr::left_join(mean_fam_exped_year, by = c("Year", "Family"))  %>% 
    # join mean_all_exped family
    dplyr::left_join(mean_fam_all_exped, by = c("Family"))  %>% 
    # add mean_assemblage
    dplyr::mutate(mean_assemblage = mean_assemblage) %>%
    # fill mean length column hierachically for that individual 
    dplyr::mutate("mean_fl" = ifelse(!is.na(mean_fam_opcode), 
                                     mean_fam_opcode, 
                                     ifelse(!is.na(mean_fam_exped),
                                            mean_fam_exped,
                                            ifelse(!is.na(mean_fam_exped_year),
                                                   mean_fam_exped_year,
                                                   ifelse(!is.na(mean_fam_all_exped),
                                                          mean_fam_all_exped,
                                                          mean_assemblage))))) %>% 
    #select columns
    dplyr::select("NewOpCode", "Exped", "Year", "Family", "Genus", "Binomial", "MaxN", "mean_fl") -> dat_maxn_no_species
  
  ###bind nmax data with and without species
  rbind(dat_maxn_species, dat_maxn_no_species) -> dat_maxn
  
  return(dat_maxn)
  
}





#' Add fork length data for individuals counted in maxn but with no available fork length
#' for these individuals we assign the mean fork length calculated in maxn data  
#' for benthic bruvs
#'
#' @param dat_fl 
#' @param dat_maxn 
#'
#' @return
#' @export
#'

add_individual_fl_data_benthic <- function (dat_fl, dat_maxn) {
  
  #add rows to fl data for individuals with missing length 
  
  #add computed column to dat_fl
  dat_fl %>% 
    dplyr::mutate("computed" = "no") -> dat_fl
  
  #initiatlize empty raw
  r0 <- data.frame("NewOpCode" = "initial",
                   "Exped" = "initial",
                   "Year" = "initial",
                   "Family" = "initial",
                   "Genus" = "initial",
                   "Binomial" = "initial",
                   "Lengthcm" = "initial", 
                   "computed" = "initial")
  
  #loop on opcodes
  for (op in (unique(dat_maxn$NewOpCode))){
    cat("-----------------opcode", op, "\n")
    
    #for given opcode loop on species 
    for (sp in unique(dat_maxn$Binomial[dat_maxn$NewOpCode == op])){
      cat("-----------------species", sp, "\n") 
      
      #get fl data corresponding to given opcode and species
      datfl = subset(dat_fl, dat_fl$NewOpCode == op & dat_fl$Binomial == sp)
      nfl = nrow(datfl)
      
      #get maxn data corresponding to given opcode and species
      datmaxn = subset(dat_maxn, dat_maxn$NewOpCode == op & dat_maxn$Binomial == sp)
      
      #retrieve maxn value if available, otherwise set maxn to 0
      if (nrow(datmaxn) >= 1){
        valmaxn <- datmaxn$MaxN
      }else{
        valmaxn <- 0
      }
      
      #handle duplicated maxn (if there are 2 available maxn, keep larger one)
      if (length(valmaxn) > 1){ 
        valmaxn = max(valmaxn)
      }
      
      #calculate difference between maxn and nb of length measures
      d = valmaxn - nfl
      cat("difference between maxn and nb of length measures is", d, "\n")
      
      #create rows corresponding to individuals with missing length and set their length to mean length calculated in maxn (mean_fl)
      if (d > 0) {
        #case when the opcode exists in fl
        if (nfl != 0){
          #create row without length
          r = unique(datfl[, c("NewOpCode", "Exped", "Year", "Family", "Genus", "Binomial")]) 
          #add length column to that row
          r$Lengthcm <- unique(datmaxn$mean_fl)
          #add computed column to that row
          r$computed <- "yes"
          #bind the rows d times
          r <- do.call("rbind", replicate(d, r, simplify = FALSE))
          #add these rows to previous rows
          r0 <- rbind(r0, r)
        }
        #case when opcode does not exist in fl
        if (nfl == 0){
          #create row without length
          r <- data.frame("NewOpCode" = op,
                          "Exped" = dat_maxn$Exped[dat_maxn$NewOpCode == op],
                          "Year" = dat_maxn$Year[dat_maxn$NewOpCode == op],
                          "Family" = dat_maxn$Family[dat_maxn$NewOpCode == op],
                          "Genus" = dat_maxn$Genus[dat_maxn$NewOpCode == op],
                          "Binomial" = dat_maxn$Binomial[dat_maxn$NewOpCode == op])
          #add length column to that row
          r$Lengthcm <- unique(datmaxn$mean_fl)
          #add computed column to that row
          r$computed <- "yes"
          #bind the rows d times
          r <- do.call("rbind", replicate(d, r, simplify = FALSE))
          #add these rows to previous rows
          r0 <- rbind(r0, r)
        }
      }
    }
  }
  
  #clean r0
  r0 %>% 
    dplyr::filter(NewOpCode != "initial") %>% 
    dplyr::mutate(Lengthcm = as.numeric(Lengthcm)) -> r0
  
  #bind these rows to fl
  rbind(dat_fl, r0) -> new
  
  cat("nb of added fl rows: ",  nrow(r0))
  
  #plot length distributions
  png(here::here("outputs", "benthic", "benthic_length_distributions.png"))
  par(mfrow=c(3,1))
  hist(log(dat_fl$Lengthcm), ylim = c(0, 22000), main = "former fl data")
  hist(log(r0$Lengthcm), ylim = c(0, 22000), main = "added fl data")
  hist(log(new$Lengthcm), ylim = c(0, 22000), main = "new fl data (former + added)")
  dev.off()
  
  return(new)
  
}




#' Write meta opcodes for benthic bruvs
#'
#' @param dat 
#'
#' @return
#' @export
#'

write_meta_opcodes_benthic <- function (dat) {
  
  write.csv(dat, here::here("outputs", "benthic", "benthic_opcodes.csv"), row.names = FALSE)
  
}
  




#
#' Estimate weight from observed length for a taxa using rfishbase
#'
#' @param data 
#'
#' @return
#' @export
#'

estimate_weight_from_length = function(data) {
  
  if(!all(c("Binomial", "Lengthcm") %in% names(data))) {
    print("Data must contain species name (Binomial) and observed length (Lengthcm)")
    break
  }
  
  #all lengths are assumed to be fork length with unit in cm
  if(!("length_type" %in% names(data))) {data$length_type = "FL"}
  if(!("length_units" %in% names(data))) {data$length_units = "cm"}
  
  # First we need to convert observed fork length (FL) to total length (TL) in centimeters if not already in those units
  
  length_length_data = NA
  
  if(any(data$length_type == "FL")) {
    
    # Get the length to length conversions from Fishbase
    
    tryCatch(length_length_data <- data.frame(rfishbase::length_length(unique(data$Binomial), fields = c("Species", "Length1","Length2","a","b"))),
             error=function(e){})
    
    names(length_length_data)[1] = "Binomial"
    
    length_length_data = length_length_data[length_length_data$a == 0 & !is.na(length_length_data$Binomial),]
    
    length_length_data = length_length_data[length_length_data$Length1 %in% c("TL", "FL") & length_length_data$Length2 %in% c("TL", "FL"),]
    
    
    if(nrow(length_length_data) == 1 & is.na(length_length_data$a[1]))  {data$TL_FLmean_ratio = 1} else {
      
      # print(length_length_data)
      
      if(is.data.frame(length_length_data) & nrow(length_length_data > 0)) {
        
        length_length_data$TL_FLratio = NA
        
        # Find the TL:FL ratios (either direction)
        
        length_length_data[length_length_data$Length1 == ("TL") & length_length_data$Length2 == ("FL"),]$TL_FLratio = length_length_data[length_length_data$Length1 == ("TL") & length_length_data$Length2 == ("FL"),]$b
        length_length_data[length_length_data$Length1 == ("FL") & length_length_data$Length2 == ("TL"),]$TL_FLratio = 1/(length_length_data[length_length_data$Length1 == ("FL") & length_length_data$Length2 == ("TL"),]$b)
        
        #print(length_length_data)
        
        # Calculate the mean TL:FL ratio
        
        TL_FLmean_ratio = length_length_data %>% 
          dplyr::group_by(Binomial) %>% 
          dplyr::summarise(TL_FLmean_ratio = mean(TL_FLratio, na.rm = TRUE))
        
        
      }
      
    }
    
    
    data = merge(data, TL_FLmean_ratio, by = "Binomial", all.x = TRUE)
    
    data$TL_FLmean_ratio[is.na(data$TL_FLmean_ratio)] = 1
    
    
    
  } else {data$TL_FLmean_ratio = 1}
  
  # Apply this to the observed fork length
  
  data$conv_length = data$Lengthcm * data$TL_FLmean_ratio
  
  # Convert length from meters to centimeters if needed
  
  data$conv_length[data$length_units == "m"] =  data$conv_length[data$length_units == "m"] * 100
  
  # print(paste("TL(cm) =", conv_length))
  
  # Get the a and b estimates for the length to weight equation W = a * L^b
  
  tryCatch(a_b <- rfishbase::estimate(unique(data$Binomial), fields = c("Species", "a", "b")),
           error=function(e) {})
  
  names(a_b)[1] = "Binomial"
  
  # print(paste("a =", a_b$a))
  # print(paste("b =", a_b$b))
  
  # fill values for missing species using values from genus and family
  
  # calculate mean values per genus
  a_b %>% 
    dplyr::mutate(Genus = stringr::word(Binomial, 1)) %>% 
    dplyr::group_by(Genus) %>% 
    dplyr::summarise(mean_a_genus = mean(a, na.rm = T),
                     mean_b_genus = mean(b, na.rm = T)) -> mean_data_genus
  
  # calculate mean values per family
  
  data %>% 
    dplyr::select("Family", "Binomial") %>% 
    dplyr::distinct() -> data_family
  
  a_b %>% 
    dplyr::left_join(data_family, by = "Binomial") %>% 
    dplyr::group_by(Family) %>% 
    dplyr::summarise(mean_a_family = mean(a, na.rm = T),
                     mean_b_family = mean(b, na.rm = T)) -> mean_data_family
  
  # calculate mean values for assemblage
  a_b %>% 
    dplyr::summarise(mean_a = mean(a, na.rm = T), 
                     mean_b = mean(b, na.rm = T)) -> mean_data_assemblage
  
  # hierarchically fill missing values
  a_b %>% 
    # join mean genus
    dplyr::mutate(Genus = stringr::word(Binomial, 1)) %>% 
    dplyr::left_join(mean_data_genus, by = c("Genus")) %>% 
    # join mean family 
    dplyr::left_join(data_family, by = "Binomial") %>% 
    dplyr::left_join(mean_data_family, by = c("Family"))  %>%       
    # add mean assemblage 
    dplyr::mutate(mean_assemblage_a = mean_data_assemblage$mean_a) %>%
    dplyr::mutate(mean_assemblage_b = mean_data_assemblage$mean_b) %>%
    # fill mean length column hierachically for that species 
    dplyr::mutate("a" = ifelse(!is.na(a), 
                               a, 
                               ifelse(!is.na(mean_a_genus),
                                      mean_a_genus,
                                      ifelse(!is.na(mean_a_family),
                                             mean_a_family,
                                             mean_assemblage_a)))) %>% 
    dplyr::mutate("b" = ifelse(!is.na(b), 
                               b, 
                               ifelse(!is.na(mean_b_genus),
                                      mean_b_genus,
                                      ifelse(!is.na(mean_b_family),
                                             mean_b_family,
                                             mean_assemblage_b)))) %>% 
    dplyr::select("Binomial",  "a", "b") -> a_b
  
    data = merge(data, a_b, by = "Binomial", all.x = TRUE)
  
  # Calculate the weight based on the (converted) length and divide by 1000 to return it in kilograms
  
  data$weight_kg = (data$a * data$conv_length^data$b)/1000
  
  
  return(data)
  
}








#' merge pelagic meta with pelagic FL
#'
#' @param dat_meta
#' @param dat_fl
#'
#' @return
#' @export
#' @import dplyr

merge_fl_pelagic_meta = function(dat_meta, dat_fl) {
  
  dat_meta %>%
    dplyr::mutate(Type = "Midwater") %>%
    dplyr::mutate(Type = as.factor(Type)) %>% 
    dplyr::rename("Exped" = "exped")  -> dat_meta_new
  
  dat_fl %>%
    dplyr::select("NewOpCode", "Family", "Binomial", "Lengthcm", "weight_kg") %>%
    dplyr::full_join(dat_meta_new[, c("NewOpCode", "string","lon_in", "lat_in", "Exped", "Type", "Date")], by ="NewOpCode") -> dat_fl_meta
  
  return(dat_fl_meta)
  
}



#' merge benthic meta with benthic FL
#'
#' @param dat_meta
#' @param dat_fl
#'
#' @return
#' @export
#' @import dplyr

merge_fl_benthic_meta = function(dat_meta, dat_fl) {
  
  dat_meta %>%
    dplyr::mutate(Type = "Seabed") %>%
    dplyr::mutate(Type = as.factor(Type))  %>% 
    dplyr::rename("Exped" = "exped")  -> dat_meta_new
  
  dat_fl %>%
    dplyr::select("NewOpCode", "Family", "Binomial", "Lengthcm", "weight_kg") %>%
    dplyr::full_join(dat_meta_new[, c("NewOpCode","lon_in", "lat_in", "Exped", "Type", "Date")], by ="NewOpCode") -> dat_fl_meta
  
  return(dat_fl_meta)
  
}


#' save merged meta with FL (benthic or pelagic)
#'
#' @param dat
#' @param type
#'
#' @return
#' @export
#'

write_merged_fl_meta  <-  function(dat, type){
  
  write.csv(dat, file = here::here("outputs", type, paste0("merged_fl_",type,"_meta.csv")))
  
}



#' rbind pelagic and benthic meta coordinates
#' @param dat_pelagic
#' @param dat_benthic
#'
#' @return
#' @export
#'
#
rbind_meta_coordinates <- function(dat_pelagic, dat_benthic){
  
  dat_pelagic %>%
    dplyr::mutate(Type = "Midwater") %>%
    dplyr::mutate(Type = as.factor(Type)) -> dat_pelagic
  
  dat_benthic %>%
    dplyr::mutate(Type = "Seabed") %>%
    dplyr::mutate(Type = as.factor(Type)) -> dat_benthic
  
  
  dat_pelagic_benthic <- rbind(dat_pelagic[c("NewOpCode", "lat_in", "lon_in", "Type")], dat_benthic[c("NewOpCode", "lat_in", "lon_in", "Type")])
  
  return(dat_pelagic_benthic)
  
}




#' rbind benthic and pelagic lengths with meta
#'
#' @param dat_pelagic_fl_meta
#' @param dat_benthic_fl_meta
#'
#' @return
#' @export
#'

rbind_fl_meta <- function (dat_pelagic_fl_meta, dat_benthic_fl_meta){
  
  dat_pelagic_benthic_fl_meta <- rbind(dat_pelagic_fl_meta[, c("NewOpCode", "Family", "Binomial", "Lengthcm", "weight_kg", "Exped", "Type", "lat_in", "lon_in")], 
                                       dat_benthic_fl_meta[, c("NewOpCode", "Family", "Binomial", "Lengthcm", "weight_kg", "Exped", "Type", "lat_in", "lon_in")])
  
  return(dat_pelagic_benthic_fl_meta)
  
}


