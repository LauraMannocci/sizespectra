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
    dplyr::select("New OpCode", "String", "Exped", "Location", "Family", "Binomial", "MaxN", "Biomass",  
                  "Wt",	"a",	"b",	"LWR Source") %>% 
    #rename
    dplyr::rename("NewOpCode" = "New OpCode",
                  "LWRSource" = "LWR Source") %>% 
    #replace one expedition name
    dplyr::mutate(Exped = stringr::str_replace(Exped, "Wandoo 2017 autumn", "Wandoo autumn 2017")) %>%
    #extract last four letters of expedition to get year
    dplyr::mutate(Year = stringr::str_sub(Exped, start=-4)) %>% 
    #add Genus column (original genus column is wrong)
    dplyr::mutate(Genus = stringr::word(Binomial, 1)) %>% 
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
    dplyr::select("NewOpCode", "String", "Exped", "Year", "Location", "Family", "Genus", "Binomial", "MaxN", "Biomass",  
                  "Wt",	"a",	"b",	"LWRSource") %>% 
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
    #remove nas
    tidyr::drop_na() %>% 
    #remove invalid stations
    dplyr::filter(use == "Yes") %>% 
    dplyr::filter(grounded == "no") -> new
  
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






#' keep only opcodes that are available in meta in data (fl or maxn) for pelagic bruvs
#'
#' @param meta_dat 
#' @param dat 
#'
#' @return
#' @export
#'

keep_opcode_in_meta_pelagic <- function(meta_dat, dat){
    
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
    

#' remove from a data (fl or maxn) the opcode not available in the other data (maxn or fl), ie the single opcodes, for pelagic bruvs
#'
#' @param dat1 
#' @param dat2 
#'
#' @return
#' @export
#'

remove_single_opcodes_pelagic <- function(dat1, dat2){
  
  # get opcode in dat1 but not in dat2
  opcodes <- setdiff(dat1$NewOpCode, dat2$NewOpCode)
  
  dat1 %>% 
    #remove opcode in dat1 but not in dat2
    dplyr::filter(!NewOpCode %in% opcodes) -> new
  
  cat("nb of opcodes removed from data:", nrow(dat1) - nrow(new))
  
  return(new)
  
}
  
    

# assign empty opcodes in meta
assign_empty_opcodes_meta_pelagic <- function(dat_meta, dat_maxn){
    
  #get opcode in meta but not in maxn -> these opcodes have no individual seen
  opcode_in_meta_only <- setdiff(dat_meta$NewOpCode, dat_maxn$NewOpCode)
  
  #get strings in meta but not in maxn -> these strings have no individual seen
  string_in_meta_only <- setdiff(dat_meta$string, dat_maxn$String)
  
  #proportion of empty strings
  cat("proportion of empty strings", length(string_in_meta_only) / length(unique(dat_meta$string)), sep="\n")
  
  #proportion of empty opcodes
  cat("proportion of empty opcodes",  length(opcode_in_meta_only) / length(unique(dat_meta$NewOpCode)), sep="\n")
  
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
  
  
  #calculate mean fl
  
  #calculate mean fl per species per opcode
  dat_fl %>% 
    dplyr::group_by(NewOpCode, Binomial) %>% 
    dplyr::summarise(mean_fl_opcode = mean(Lengthcm)) -> mean_opcode
  
  #calculate mean fl per species per string
  dat_fl %>% 
    dplyr::group_by(String, Binomial) %>% 
    dplyr::summarise(mean_fl_string = mean(Lengthcm)) -> mean_string
  
  #calculate mean fl per species per expedition
  dat_fl %>% 
    dplyr::group_by(Exped, Binomial) %>% 
    dplyr::summarise(mean_fl_exped = mean(Lengthcm)) -> mean_exped
  
  #calculate mean fl per species per expedition per year
  dat_fl %>%
    dplyr::group_by(Exped, Year, Binomial) %>%
    dplyr::summarise(mean_fl_exped_year = mean(Lengthcm)) -> mean_exped_year
  
  #calculate mean fl per species for all expeditions
  dat_fl %>% 
    dplyr::group_by(Binomial) %>% 
    dplyr::summarise(mean_fl_all_exped = mean(Lengthcm)) -> mean_all_exped
  
  #calculate mean fl per genus for all expeditions
  dat_fl %>% 
    dplyr::group_by(Genus) %>% 
    dplyr::summarise(mean_fl_all_exped_genus = mean(Lengthcm)) -> mean_all_exped_genus
  
  #calculate mean fl per family for all expeditions
  dat_fl %>% 
    dplyr::group_by(Family) %>% 
    dplyr::summarise(mean_fl_all_exped_family = mean(Lengthcm)) -> mean_all_exped_family
  
  #calculate mean fl for whole assemblage for all expeditions
  mean_assemblage = mean(dat_fl$Lengthcm)
  
  
  #add mean fl to maxn
  
  dat_maxn %>% 
    # join mean_opcode 
    dplyr::left_join(mean_opcode, by = c("NewOpCode", "Binomial")) %>% 
    # join mean_string 
    dplyr::left_join(mean_string, by = c("String", "Binomial")) %>% 
    # join mean_exped 
    dplyr::left_join(mean_exped, by = c("Exped", "Binomial"))  %>% 
    # join mean_exped_year
    dplyr::left_join(mean_exped_year, by = c("Exped", "Year", "Binomial"))  %>% 
    # join mean_all_exped 
    dplyr::left_join(mean_all_exped, by = c("Binomial"))  %>% 
    # join mean_all_exped genus
    dplyr::left_join(mean_all_exped_genus, by = c("Genus"))  %>% 
    # join mean_all_exped family
    dplyr::left_join(mean_all_exped_family, by = c("Family"))  %>% 
    # add mean_assemblage
    dplyr::mutate(mean_fl_assemblage = mean_assemblage) %>%
    # fill mean length column hierachically for that species 
    dplyr::mutate("mean_fl" = ifelse(!is.na(mean_fl_opcode), 
                                     mean_fl_opcode, 
                                     ifelse(!is.na(mean_fl_string),
                                            mean_fl_string,
                                            ifelse(!is.na(mean_fl_exped),
                                                   mean_fl_exped,
                                                   ifelse(!is.na(mean_fl_exped_year),
                                                          mean_fl_exped_year,
                                                          ifelse(!is.na(mean_fl_all_exped),
                                                                 mean_fl_all_exped,
                                                                 ifelse(!is.na(mean_fl_all_exped_genus),
                                                                        mean_fl_all_exped_genus,
                                                                        ifelse(!is.na(mean_fl_all_exped_family),
                                                                               mean_fl_all_exped_family,
                                                                               mean_fl_assemblage)))))))) -> dat_maxn
  
  
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
  
  #loop on opcode
  for (op in (unique(dat_fl$NewOpCode))){
    cat("-----------------opcode", op, "\n")
    
    #for given opcode loop on species 
    for (sp in unique(dat_fl$Binomial[dat_fl$NewOpCode == op])){
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
      
      #create rows by corresponding to individuals with missing length and set their length to mean length calculated in maxn (mean_fl)
      if (d > 0) {
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
  png(here::here("outputs", "pelagic_length_distributions.png"))
  par(mfrow=c(3,1))
  hist(log(dat_fl$Lengthcm), ylim = c(0, 22000), main = "former fl data")
  hist(log(r0$Lengthcm), ylim = c(0, 22000), main = "added fl data")
  hist(log(new$Lengthcm), ylim = c(0, 22000), main = "new fl data (former + added)")
  dev.off()
  
  return(new)
  
}
