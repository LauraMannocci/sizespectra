#load all functions ----
devtools::document()
devtools::load_all() ### load packages and functions in R folder



# explore bruvs data

########### PELAGIC bruvs-----

# load pelagic data-- 

load("1_read_clean_pelagic_old.RData")
fl_pelagic <- tidyr::drop_na(fl_pelagic, weight_kg)

########### BENTHIC bruvs----


# load benthic data---

load("1_read_clean_benthic_old.RData")
fl_benthic <- tidyr::drop_na(fl_benthic, weight_kg)


### load pelagic benthic meta and merged fork lengths data

load("1_read_clean_pelagic_benthic_old.RData")
fl_pelagic_benthic_meta <- tidyr::drop_na(fl_pelagic_benthic_meta, weight_kg)
data = fl_pelagic_benthic_meta[fl_pelagic_benthic_meta$weight_kg > 0.001, ]# select individuals larger than minsize kg


### load pelagic benthic meta summary
pelagic_benthic_sum <- read_data_pelagic_benthic_sum()


################# Fig 1 data bruvs screen grab ################### ----

draw_screengrab_combo()


################# Fig 2 sampling effort  ###################----
#map shapefiles
WorldData <- ggplot2::map_data("world") #%>% filter(region != "Antarctica") %>% fortify
#dsn_mar_layer <- "/Users/tomletessier/Documents/R stuff/Rcourse_graphics_Nov2011/"
#mar <- rgdal::readOGR(dsn = dsn_mar_layer, layer = "World_Maritime_Boundaries_v8") #maritime boundary

#map with expedition location with eez
#fig_map_sum <- globalmap_sum_eez(world = WorldData, mar = mar, meta_pb = pelagic_benthic_sum)

#map with expedition location with eez
fig_map_sum <- globalmap_sum(world = WorldData, meta_pb = pelagic_benthic_sum)



##### species rank order weights with marginal violin  MAIN BODY
fig_sp_rank <- fl_species_ord_marg(data = fl_pelagic_benthic_meta,lower.line=0.001, mid.line = 0.1, upper.line=10, minsize =0.001)# define quantiles for lines

### multiplot
multi_fig_sample(fig_map_sum, fig_sp_rank)



#### Fig S1 forklength against weight log-log ----
fig_fl_length_weight <- fl_lengthweight(data = fl_pelagic_benthic_meta)




#### Fig 3 weight against latitudinal band  ----

# ggridges weight against lat bad
fig_ridges <- figridges(dat = fl_pelagic_benthic_meta, min_size = 0.001, lat_band = 6)   #tropical latitude = 23.10S-23.10N, subtropical latitude = +/-23.5N-38N, temperate 38-70

# bin data and lm coefs
bin_global_lm <- bin_global_points_lm(fl_pelagic_benthic_meta,0.001, 6)#sizes, minimum size (in kg), and no of latitudinal bans - also saves the coefs in table

#multiplot
ex_data_lat_cdp(fig_ridges, bin_global_lm)


#### Fig S7 weight against longitudinal band with regression ----


fig_ridges_lon <- figridges_lon(dat = fl_pelagic_benthic_meta, min_size = 0.001, lon_band = 6)  


# bin data and lm coefs longitude sensitivity
bin_global_lm_lon <- bin_global_points_lm_lon(fl_pelagic_benthic_meta,0.001, 6)#sizes, minimum size (in kg), and no of latitudinal bans - also saves the coefs in table

#multiplot
ex_data_lat_cdp_lon(fig_ridges_lon, bin_global_lm_lon)


#### data exploration
#median values
median(fl_pelagic$weight_kg)
# [1] 0.008759282
median(fl_benthic$weight_kg)
# [1] 0.0585

#sum of weights 
sum(fl_pelagic_benthic_meta$weight_kg,na.rm=TRUE)
#sum of weights grouped by Type
aggregate(fl_pelagic_benthic_meta$weight_kg, by=list(Category=fl_pelagic_benthic_meta$Type), FUN=sum, na.rm= TRUE)



# 
# tab_betaslope %>%
#   #select columns
#   select_at(vars("bruvs", "Bathymetry", "distSeamounts", "conflicts", "RuleofLaw_mean", "GovernmentEffectiveness_mean", "Voice_mean", "Corruption_mean", "HDI_mean", "MarineEcosystemDependency", "TravelTime_market")) %>% 
#   gather(key = "key", value = "value", -bruvs) %>%
#   group_by(bruvs, key) %>%
#   summarise(mean = mean(value), min = min(value), max =max(value)) -> envar_table_ms
# write.csv(envar_table_ms, here::here("outputs", "table", file="envar_table_ms_benthic_pelagic.csv"))
# 
# #calculate range and means for a table
# 
# tab %>%
#   #select columns
#   select_at(vars("Bathymetry", "distSeamounts", "distCoralReef","Slope","distCoast", "PP","CHL","SST_mean",'SST_sd','sst_week',"SAU","FE_PurseSeine", "FE_DriftingLongline", "FE_FixedGear", "conflicts", "RuleofLaw_mean", "NoViolence_mean", "GovernmentEffectiveness_mean", "Voice_mean", "Corruption_mean", "HDI_mean", "NGO", "MarineEcosystemDependency", "TravelTime_market", "LinearDistcities", "TravelTime_pop", "LinearDistpop", "distPort")) %>% 
#   #gather(value = "value") %>%
#   summarise_if(is.numeric, list(mean, min, max), na.rm = TRUE) -> envar_table_ms
#  
# tab -> envar_table_ms
# write.csv(envar_table_ms, here::here("outputs", "table", file="envar_table_ms.csv"))
# 








