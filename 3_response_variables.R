#load all functions ----
devtools::document()
devtools::load_all() ### load packages and functions in R folder


# MAKE PELAGIC RESPONSES----

##read pelagic sizes----

SpecLenAll <- read_size_pelagic()


##choose min pelagic size (in kg). must be consistent with benthic ----
SpecLen <- subset_size(SpecLenAll, 0.001) 


##make pelagic mean median and max size----
data_to_export <- mean_median_max_pelagic(SpecLen)

##make pelagic modes - this takes a while----
data_to_export <- modes_pelagic(SpecLen, data_to_export)

##make pelagic beta slopes - this takes a while----
data_to_export <- beta_slope_pelagic(SpecLen, data_to_export)

#subset sensible betaslope
data_to_export <- subset_betaslope(data_to_export, -6, 1)

write.table(data_to_export,file=here::here("data", "response", "size_response_pelagic.txt"), row.names = FALSE)






# MAKE BENTHIC RESPONSES-----

##read benthic size----
SpecLenAll <- read_size_benthic()

##choose min benthic size (in kg). must be consistent with pelagic ----
SpecLen <- subset_size(SpecLenAll, 0.0010) 

##make benthic mean median and max size----
data_to_export <- mean_median_max_benthic(SpecLen)


##make benthic modes - this takes a while----
data_to_export <- modes_benthic(SpecLen, data_to_export)


##make benthic beta slopes - this takes a while----
data_to_export <- beta_slope_benthic(SpecLen, data_to_export)

#subset sensible betaslope - not needed
#data_to_export <- subset_betaslope(data_to_export, -6, 1)

write.table(data_to_export,file=here::here("data", "response", "size_response_benthic.txt"), row.names = FALSE)




# MERGE PELAGIC ENVAR WITH META ------


#envar_pelagic <- readr::read_csv(here::here("data", "envar", "PelagicData_dec2021.csv"), col_names = TRUE)
envar_pelagic <- readRDS(here::here("data", "envar", "pelagicdata_020622.rds"))

#ggplot2::ggplot(envar_pelagic, aes(x=distPort, y=LinearDistpop)) + geom_point()
  

### compute average pelagic environmental variable by date/exped----

envar_pelagic <- envar_pelagic %>% 
  dplyr::group_by(`Exped.`, Date) %>% 
  dplyr::summarise(dplyr::across(dplyr::everything(), ~ summarise_fun(.x))) %>% 
  dplyr::ungroup()

## make key----
envar_pelagic$Date <- as.character(as.Date(as.character(envar_pelagic$Date), format = "%d/%m/%Y"))
envar_pelagic$key <- paste(envar_pelagic$Exped., envar_pelagic$Date, sep = "__")


#read pelagic size response and merge with envar

response_pelagic <- read.table(here::here("data", "response", "size_response_pelagic.txt"), header = TRUE)

size_response_envar_pelagic <- merge(envar_pelagic, response_pelagic, by = "key", all = TRUE)
colnames(size_response_envar_pelagic)


#turn depth positive

size_response_envar_pelagic$Bathymetry <- abs(size_response_envar_pelagic$Bathymetry)* -1

#retain clean columns

size_response_envar_pelagic %>% 
  #select columns
  dplyr::select("key","Exped.","Date.x","Year", "Month",
                "TERRITORY1","conflicts",                    
                 "RuleofLaw_mean","NoViolence_mean","GovernmentEffectiveness_mean","Voice_mean",                   
                 "Corruption_mean","HDI_mean","Country","NGO",     
                 "MarineEcosystemDependency","SAU","TravelTime_market","LinearDistcities",             
                 "TravelTime_pop","LinearDistpop","FE_PurseSeine","FE_DriftingLongline",          
                 "FE_FixedGear","FE_OtherFishing","FE_Trawlers","distPort",                     
                 "distSeamounts", "distCoralReef","Bathymetry","Slope","distCoast",
                  "PP", "CHL","SST_mean",                     
                 "SST_sd","sst_week","protection",                       
                 "mean_lat","mean_long","mean_maxsize","std_maxsize",
                 "cv_maxsize","median_maxsize","string_number","mean_meansize",                
                 "std_meansize","cv_meansize","median_meansize","mean_mediansize",              
                 "std_mediansize","cv_mediansize","median_mediansize","excess_mass",                  
                 "modes_p_value","unimode","first_mode","second_mode",                  
                 "betaslope", "betaslope_ci_inf","betaslope_ci_sup") -> size_response_envar_pelagic_clean          




#write clean response with envar pelagic

size_response_envar_pelagic_clean$bruvs_type <- c("pelagic")


write.table(size_response_envar_pelagic_clean,here::here("data", "response", file="size_response_envar_pelagic_clean.txt"))



# MERGE BENTHIC ENVAR WITH META ----
# load clean benthic length data


envar_benthic <- readRDS(here::here("data", "envar", "benthicdata_150622.rds"))

envar_benthic %>% dplyr::mutate(distMarket = as.numeric(distMarket)) %>% 
                  dplyr::mutate(tt_market = as.numeric(tt_market))-> envar_benthic


#merge benthic envar and meta 
#envar_meta_newopcode <- merge(meta_benthic, envar_benthic, by.x = "NewOpCode", by.y = "NewOpCode")
#ggplot2::ggplot(envar_benthic, aes(x=distMarket, y=distPort)) + geom_point()


# join measured depth to benthic bruvs data with environmental variables
envar_benthic <- join_measured_depth_benthic(envar_benthic)


### compute average benthic environmental variable by date/exped----

envar_benthic <- envar_benthic %>% 
  dplyr::group_by(`Exped`, Date) %>% 
  dplyr::summarise(dplyr::across(dplyr::everything(), ~ summarise_fun(.x))) %>% 
  dplyr::ungroup()


# make key

envar_benthic$key <- paste(envar_benthic$Exped, envar_benthic$Date, sep = "__")


#read benthic size response and merge with envar

response_benthic <- read.table(here::here("data", "response", "size_response_benthic.txt"), header = TRUE)

size_response_envar_benthic <- merge(envar_benthic, response_benthic, by = "key", all = TRUE)


#correct bathymetry data with measured depth data

size_response_envar_benthic <- correct_bathy_benthic(size_response_envar_benthic)

#retain clean columns
#dim(size_response_envar_benthic)

size_response_envar_benthic %>% 
  #select columns
  dplyr::select("key","Exped.x","Year","Month", "Date.x",                     
                "bathy","slope","distPort","distCoast", "DriftingLongline","OtherFishing",                
                "Trawlers","FixedGear","PurseSeine","TERRITORY1",
                "RuleofLaw_mean","NoViolence_mean","GovernmentEffectiveness_mean",
                  "Voice_mean","Corruption_mean","distSeamounts","MarineEcosystemDependency","conflicts",                   
                  "HDI_mean","sst_mean","sst_week","CHL", "sst_sd","PP", "protection", "distCoralReef",               
                  "distMarket","tt_market","mean_lat",                    
                  "mean_long","mean_maxsize","std_maxsize","cv_maxsize","median_maxsize",              
                  "BRUVS_number","mean_meansize","std_meansize","cv_meansize","median_meansize",             
                  "mean_mediansize","std_mediansize","cv_mediansize","median_mediansize","excess_mass",                 
                  "modes_p_value","unimode","first_mode","second_mode","betaslope",                   
                  "betaslope_ci_inf","betaslope_ci_sup")        %>% 
  #rename to match pelagic envar names
  dplyr::rename("Exped." = "Exped.x",
                "Bathymetry" = "bathy",
                "Slope"= "slope",
                "FE_DriftingLongline"="DriftingLongline",
                "FE_OtherFishing"="OtherFishing",                
                "FE_Trawlers"="Trawlers",
                "FE_FixedGear"="FixedGear",
                "FE_PurseSeine"="PurseSeine",
                "SST_mean"="sst_mean",
                "SST_sd"="sst_sd",
                "TravelTime_market"="tt_market",
                "string_number"="BRUVS_number" ) -> size_response_envar_benthic_clean

                
        



#turn depths negative (in case where it positive)
size_response_envar_benthic_clean$Bathymetry <- ifelse(size_response_envar_benthic_clean$Bathymetry > 0,
                                                       abs(size_response_envar_benthic_clean$Bathymetry)* -1,
                                                       size_response_envar_benthic_clean$Bathymetry)


size_response_envar_benthic_clean$bruvs_type <- c("benthic")


write.table(size_response_envar_benthic_clean,here::here("data", "response", file="size_response_envar_benthic_clean.txt"))




#COMBINE BENTHIC AND PELAGIC RESPONSE  ----



pelagic_response_envar <- read.table(here::here("data", "response", "size_response_envar_pelagic_clean.txt"), header = TRUE)
benthic_response_envar <- read.table(here::here("data", "response", "size_response_envar_benthic_clean.txt"), header = TRUE)


pelagic_benthic_response_envar_clean <- plyr::rbind.fill(pelagic_response_envar, benthic_response_envar)


write.table(pelagic_benthic_response_envar_clean, here::here("data","response", file="pelagic_benthic_response_envar_clean.txt"))

