#load all functions ----
devtools::document()
devtools::load_all() ### load packages and functions in R folder

#read port and markets data
market <-read_data_with_market()
port <-read_data_with_port()

# port and market maps
map_port <- globalmap_port(world = WorldData, mar = mar, dat = port)
map_market <- globalmap_market(world = WorldData, mar = mar, dat = market)


# explore bruvs data

########### PELAGIC bruvs-----

# load pelagic data-- 

load("1_read_clean_pelagic.RData")


## make map meta
#make_map_meta(meta_pelagic, "pelagic")


########### BENTHIC bruvs----


# load benthic data---

load("1_read_clean_benthic.RData")


### make map meta
make_map_meta(meta_benthic, "benthic")


### load pelagic benthic meta and merged fork lengths data

load("1_read_clean_pelagic_benthic.RData")

fl_pelagic_benthic_meta <- tidyr::drop_na(fl_pelagic_benthic_meta, weight_kg)


### load pelagic benthic meta summary

pelagic_benthic_sum <- read_data_pelagic_benthic_sum()



################# SAMPLING EFFORT MAP ################### MAIN BODY----
#map shapefiles
WorldData <- ggplot2::map_data("world") #%>% filter(region != "Antarctica") %>% fortify
dsn_mar_layer <- "/Users/tomletessier/Documents/R stuff/Rcourse_graphics_Nov2011/"
mar <- rgdal::readOGR(dsn = dsn_mar_layer, layer = "World_Maritime_Boundaries_v8") #maritime boundary


#map
fig_map <- globalmap(world = WorldData, mar = mar, meta_pb = meta_pelagic_benthic)


#map with summary

fig_map_sum <- globalmap_sum(world = WorldData, mar = mar, meta_pb = pelagic_benthic_sum)



#with overlap by lat ban
fig_ridges_overlap <- figridges_overlap(dat = fl_pelagic_benthic_meta, min_size = 0.0001, lat_band = 20, bandw = 0.2)   #tropical latitude = 23.10S-23.10N, subtropical latitude = +/-23.5N-38N, temperate 38-70

#with overlap by exped
fig_ridges_exped <- figridges_overlap_exped(dat = fl_pelagic_benthic_meta,min_size = 0.001, bandw = 0.2, scale= 30, alpha=0.05)   #tropical latitude = 23.10S-23.10N, subtropical latitude = +/-23.5N-38N, temperate 38-70

##### species rank order weights with marginal violin  MAIN BODY

fig_sp_rank <- fl_species_ord_marg(data = fl_pelagic_benthic_meta,lower.line=0.001, mid.line = 0.1, upper.line=10, minsize =0.00001)# define quantiles for lines



#### Extended Data Fig 2 weight against latitudinal band with corresponding cummulative distribution SUPPLEMENTS----
### ggridges weight against lat bad


fig_ridges <- figridges(dat = fl_pelagic_benthic_meta, min_size = 0.00001, lat_band = 6)   #tropical latitude = 23.10S-23.10N, subtropical latitude = +/-23.5N-38N, temperate 38-70
#pareto distribution - cumulative distribution plotting - more appropriate to MLE 
#points_lcd <- cum_dist_plot(fl_pelagic_benthic_meta, 0.00001, 6) #sizes, minimum size (in kg), and no of latitudinal bans
#ex_data_lat_cdp(fig_ridges, points_lcd)

# bin data and lm coefs
bin_global_lm <- bin_global_points_lm(fl_pelagic_benthic_meta,0.001, 6)#sizes, minimum size (in kg), and no of latitudinal bans - also saves the coefs in table


#Extended Data Fig 2
#multiplot
ex_data_lat_cdp(fig_ridges, bin_global_lm)


### min max size spectra by latitudinal band
ggplot(data=fl_pelagic_benthic_meta, aes(x=abs(lat_in), colour = Type, fill =Type)) + geom_histogram(position ="dodge", alpha=.4) ### check number of bruvs with lat



#bin dat with quadratic polynomial fit using nonlinear regression  (Yurista et al 2014 Can J Fish Aqua. Sci) log10(g·m^2 · g^1) A0.5(C)[log10g) * B]2
bin_global_quad <- bin_global_points_quad(fl_pelagic_benthic_meta, 0.003, 5)
### global size spectra with regression using a stacked histogramm
hist_spectra <- hist_spectra(data=fl_pelagic_benthic_meta)
### global size spectra with regression using non-stacked histogramm
hist_nonstack <- hist_spectra_nonstack(data=fl_pelagic_benthic_meta)
### global biomass pyramid
biomass_pyramid <- biomass_pyramid(data=fl_pelagic_benthic_meta)


## PLOT RESPONSE----
# read data with response variables
tab <- read_data_with_vars()

#betaslope histogram
fig_betaslope <- hist_betaslope(tab)

#modes
fig_modes <- density_modes(tab)
fig_modes_violin <- violin_modes(tab)

##response vs response
tab <- read_data_with_vars()
tab_firstmode <- clean_data_with_vars(tab, "logFirstmode")

response_vs_response(tab_firstmode)


###Fig 1 multiplot sampling overview/species rank order/response variables MAIN BODY FIG 1----


multi_fig_sample(fig_map_sum, fig_sp_rank)

###Fig 2 response variable ----
response_fig(fl_pelagic_benthic_meta,  tab, min_size=0.001, lat_band =20, bandw = 0.2, scale= 30, alpha=0.3)



 ###Fig 3 conceptual diagram plus response----
conceptual_dia(fl_pelagic_benthic_meta,  tab, tab_firstmode,min_size=0.01, bandw = 0.2, scale= 30, alpha=0.05)



 ### forklength against weight log-log SUPPLEMENTARY MATERIAL 
  
fig_fl_length_weight <- fl_lengthweight(data = fl_pelagic_benthic_meta)



##### species rank order weights by quantiles SUPPLEMENTARY MATERIAL

fig_fl_species_rank_vert <- fl_species_rank_order_vert(data = fl_pelagic_benthic_meta, lower.line=0.01, upper.line=0.99)# define quantiles for lines
fig_fl_species_rank_upper <- fl_species_rank_order_quan(data = fl_pelagic_benthic_meta, lower.quan = 0.99, upper.quan = 1, breaks = c(1, 10, 100,1000))
fig_fl_species_rank_lower <- fl_species_rank_order_quan(data = fl_pelagic_benthic_meta, lower.quan = 0, upper.quan = 0.01, breaks = c(0.0001, 0.001, 0.01, 0.1))


            

#####TEMPORAL EFFORT - supplementary figure

meta_benthic2 <-meta_benthic[!(meta_benthic$NewOpCode=="BRUV3_30102016" | meta_benthic$NewOpCode=="BRUV2_03112016" | meta_benthic$NewOpCode=="BRUV5_01122016"| meta_benthic$NewOpCode=="BRUV5_22102016" | meta_benthic$NewOpCode=="BRUV6_22102016"),]

hist(meta_benthic2$Date, breaks = "years")


date_hist <- ggplot(data = meta_benthic2, aes(x=Date))+
  geom_histogram(fill = 'orange', alpha=.5) +
  # Change the fill colour to differentiate it
  geom_histogram(data=meta_pelagic, fill='#077DAA', alpha=.5) +
  labs(title = "Temporal distribution of BRUVS effort")+
  labs(y="Yearly BRUVS deployments")+
  labs(x="Year")+
  date_hist

#### data exploration


fl_pelagic_benthic_meta2 <- subset(fl_pelagic_benthic_meta, weight_kg >45)
nrow(fl_pelagic_benthic_meta2)
sort(table(fl_pelagic_benthic_meta2$Type))



nrow(unique(fl_pelagic_benthic_meta$Binomial))

sort(table(fl_pelagic_benthic_meta$Binomial))

nlevels(as.factor(fl_pelagic_benthic_meta$Binomial))

# median(fl_pelagic$weight_kg)
# [1] 0.008759282
# median(fl_benthic$weight_kg)
# [1] 0.0585

### count unique locations

levels(meta_pelagic$Location)

levels(as.factor(rbind(meta_pelagic$Location, meta_benthic$Location)))
levels(as.factor(rbind(meta_pelagic$exped, meta_benthic$Exped)))

#sum of weights 
sum(fl_pelagic_benthic_meta$weight_kg,na.rm=TRUE)
#sum of weights grouped by Type
aggregate(fl_pelagic_benthic_meta$weight_kg, by=list(Category=fl_pelagic_benthic_meta$Type), FUN=sum, na.rm= TRUE)

#count Na lengths within expeds

numberofNa <- aggregate(weight_kg ~ Exped, data=fl_pelagic_benthic_meta, function(x) {sum(is.na(x))}, na.action = NULL)
numberofOpcode <-aggregate(data = fl_pelagic_benthic_meta, NewOpCode ~ Exped,function(x) length(unique(x)))

sum(numberofOpcode$NewOpcode)


#sum weight within bins
options(scipen=7)

##Global size spectra with regression


dat_hist <- ggplot_build(size_hist)
pel_bent_dat <- dat_hist$data[[2]]
pel_bent_lm <- lm(pel_bent_dat$count ~ pel_bent_dat$x)
coef(pel_bent_lm)[["pel_bent_dat$x"]]
size_hist + geom_line(data = pel_bent_lm,aes(x=pel_bent_dat$x, y=(pel_bent_)[fitted.values]))


#locations for Jessica

pelagic_locations <- as.data.frame(levels(as.factor(fl_pelagic$Exped)))
write.table(pelagic_locations, here::here("outputs", "table", file="Letessier_pelagic_locations.csv"))

benthic_locations <- as.data.frame(levels(as.factor(fl_benthic$Exped)))
write.table(benthic_locations, here::here("outputs", "table", file="Letessier_benthic_locations.csv"))


#calculate range and means for envars to be modified for a table with benthic/pelagic ranges


tab_betaslope %>%
  #select columns
  select_at(vars("bruvs", "Bathymetry", "distSeamounts", "conflicts", "RuleofLaw_mean", "GovernmentEffectiveness_mean", "Voice_mean", "Corruption_mean", "HDI_mean", "MarineEcosystemDependency", "TravelTime_market")) %>% 
  gather(key = "key", value = "value", -bruvs) %>%
  group_by(bruvs, key) %>%
  summarise(mean = mean(value), min = min(value), max =max(value)) -> envar_table_ms
write.csv(envar_table_ms, here::here("outputs", "table", file="envar_table_ms_benthic_pelagic.csv"))

#calculate range and means for a table

tab %>%
  #select columns
  select_at(vars("Bathymetry", "distSeamounts", "distCoralReef","Slope","distCoast", "PP","CHL","SST_mean",'SST_sd','sst_week',"SAU","FE_PurseSeine", "FE_DriftingLongline", "FE_FixedGear", "conflicts", "RuleofLaw_mean", "NoViolence_mean", "GovernmentEffectiveness_mean", "Voice_mean", "Corruption_mean", "HDI_mean", "NGO", "MarineEcosystemDependency", "TravelTime_market", "LinearDistcities", "TravelTime_pop", "LinearDistpop", "distPort")) %>% 
  #gather(value = "value") %>%
  summarise_if(is.numeric, list(mean, min, max), na.rm = TRUE) -> envar_table_ms
 
tab -> envar_table_ms
write.csv(envar_table_ms, here::here("outputs", "table", file="envar_table_ms.csv"))




macro_plankton<- fl_pelagic_benthic_meta[which(fl_pelagic_benthic_meta$Lengthcm<=20&fl_pelagic_benthic_meta$Lengthcm>2),]
summary(macro_plankton)

meso_plankton <- fl_pelagic_benthic_meta[which(fl_pelagic_benthic_meta$Lengthcm<=2&fl_pelagic_benthic_meta$Lengthcm>.2),]
summary(meso_plankton)
table(as.factor(meso_plankton$Binomial))

all_plankton <- fl_pelagic_benthic_meta[which(fl_pelagic_benthic_meta$Lengthcm<=4),]
summary(all_plankton)

under_one_gramme <- fl_pelagic_benthic_meta[which(fl_pelagic_benthic_meta$weight_kg<=0.001),]
summary(under_one_gramme)

