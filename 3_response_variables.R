#load all functions ----
devtools::document()
devtools::load_all() ### load packages and functions in R folder


# MAKE PELAGIC RESPONSES----

##read pelagic sizes----

SpecLen <- read_size_pelagic()

##make pelagic mean median and max size----
data_to_export <- mean_median_max_pelagic(SpecLen)

##make pelagic modes - this takes a while----
data_to_export <- modes_pelagic(SpecLen, data_to_export)

##make pelagic beta slopes - this takes a while----
data_to_export <- beta_slope_pelagic(SpecLen, data_to_export)

write.table(data_to_export,file=here::here("data", "response", "size_response_pelagic.txt"), row.names = FALSE)



# MAKE BENTHIC RESPONSES-----

##read benthic size----

SpecLen <- read_size_benthic()

##make benthic mean median and max size----

data_to_export <- mean_median_max_benthic(SpecLen)


##make benthic modes - this takes a while----

data_to_export <- modes_benthic(SpecLen, data_to_export)


##make benthic beta slopes - this takes a while----

data_to_export <- beta_slope_benthic(SpecLen, data_to_export)

write.table(data_to_export,file=here::here("data", "response", "size_response_benthic.txt"), row.names = FALSE)


#merge and export benthic responses




# MERGE PELAGIC ENVAR WITH META ------


envar_pelagic <- readr::read_csv(here::here("data", "envar", "PelagicData_dec2021.csv"), col_names = TRUE)


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

#retain clean columns

dim(size_response_envar_pelagic)

size_response_envar_pelagic_clean  <- size_response_envar_pelagic[ , c(1, 2, 18, 19, 67:123)]


#turn depth negative

size_response_envar_pelagic_clean$Bathymetry <- abs(size_response_envar_pelagic_clean$Bathymetry)* -1


#write clean response with envar pelagic


write.table(size_response_envar_pelagic_clean,here::here("outputs", "pelagic", file="size_response_envar_pelagic_clean.txt"))


# MERGE BENTHIC ENVAR WITH META ----
# load clean benthic length data


#load(here::here("data", "envar","benthic_data_inprogress220422.Rdata"))

envar_benthic <- readRDS(here::here("data", "envar", "benthicdata_inprogress250422.rds"))

#merge benthic envar and meta 
#envar_meta_newopcode <- merge(meta_benthic, envar_benthic, by.x = "NewOpCode", by.y = "NewOpCode")


### compute average pelagic environmental variable by date/exped----

envar_benthic <- envar_benthic %>% 
  dplyr::group_by(`Exped`, Date) %>% 
  dplyr::summarise(dplyr::across(dplyr::everything(), ~ summarise_fun(.x))) %>% 
  dplyr::ungroup()


# make key
#envar_benthic$Date <- as.character(as.Date(as.character(envar_benthic$Date), format = "%d/%m/%Y")) #because of Date formating this step is not required

envar_benthic$key <- paste(envar_benthic$Exped, envar_benthic$Date, sep = "__")


#read benthic size response and merge with envar

response_benthic <- read.table(here::here("data", "response", "size_response_benthic.txt"), header = TRUE)

size_response_envar_benthic <- merge(envar_benthic, response_benthic, by = "key", all = TRUE)

summary(size_response_envar_benthic)

#retain clean columns
colnames(size_response_envar_benthic)
dim(size_response_envar_benthic)
size_response_envar_benthic_clean  <- size_response_envar_benthic[ , c(1, 2, 3, 6, 7, 15:22, 24, 27:32, 35:57)]

colnames(size_response_envar_benthic_clean)
colnames(pelagic_response_envar)

colnames(size_response_envar_benthic_clean) = c("key","Exped.","Date.y","Year","Month", 
                                                "Bathymetry","Slope","distPort","FE_DriftingLongline","FE_OtherFishing",
                                                "FE_Trawlers","FE_FixedGear","FE_PurseSeine","TERRITORY1","RuleofLaw_mean",
                                                "NoViolence_mean","GovernmentEffectiveness_mean","Voice_mean","Corruption_mean","distSeamounts",
                                                "mean_lat","mean_long","mean_maxsize","std_maxsize","cv_maxsize",
                                                "median_maxsize","string_number","mean_meansize","std_meansize","cv_meansize",
                                                "median_meansize","mean_mediansize","std_mediansize","cv_mediansize","median_mediansize",
                                                "excess_mass", "modes_p_value","unimode","first_mode","second_mode",
                                                "betaslope", "betaslope_ci_inf","betaslope_ci_sup") 

size_response_envar_benthic_clean$bruvs_type <- c("benthic")


#turn depths negative
size_response_envar_benthic_clean$Bathymetry <- abs(size_response_envar_benthic_clean$Bathymetry)* -1



write.table(size_response_envar_benthic_clean,here::here("outputs", "benthic", file="size_response_envar_benthic_clean.txt"))



#COMBINE BENTHIC AND PELAGIC RESPONSE  ----

pelagic_response_envar <- read.table(here::here("outputs", "pelagic", "size_response_envar_pelagic_clean.txt"), header = TRUE)

pelagic_response_envar$bruvs_type <- c("pelagic")


benthic_response_envar <- read.table(here::here("outputs", "benthic", "size_response_envar_benthic_clean.txt"), header = TRUE)

pelagic_benthic_response_envar_clean <- plyr::rbind.fill(pelagic_response_envar, benthic_response_envar)

summary(pelagic_benthic_response_envar_clean)

pelagic_benthic_response_envar_clean <- pelagic_benthic_response_envar_clean %>% tidyr::drop_na(Exped.)

write.table(pelagic_benthic_response_envar_clean,here::here("outputs", file="pelagic_benthic_response_envar_clean.txt"))

