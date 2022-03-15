#load all functions
devtools::document()
devtools::load_all() ### load packages and functions in R folder


########################################################################################################################################
# explore bruvs data
########################################################################################################################################



########################################################################################################################################
########### pelagic bruvs

### load pelagic data

load("1_read_clean_pelagic.RData")



### make map meta
make_map_meta(meta_pelagic, "pelagic")



########################################################################################################################################
########### benthic bruvs


### load benthic data

load("1_read_clean_benthic.RData")


### make map meta
make_map_meta(meta_benthic, "benthic")

make_map_meta(fl_pelagic_meta, "benthic")


### load pelagic benthic meta and merged fork lengths data

load("1_read_clean_pelagic_benthic.RData")


################# SAMPLING EFFORT MAP ################### MAIN BODY
#map shapefiles
WorldData <- ggplot2::map_data("world") #%>% filter(region != "Antarctica") %>% fortify
dsn_mar_layer <- "/Users/tomletessier/Documents/R stuff/Rcourse_graphics_Nov2011/"
mar <- rgdal::readOGR(dsn = dsn_mar_layer, layer = "World_Maritime_Boundaries_v8") #maritime boundary



#map
fig_map <- globalmap(world = WorldData, mar = mar, meta_pb = meta_pelagic_benthic)


#### plot density plot of benthic and pelagic lengths ####


### ggridges weight against lat MAIN BODY

fig_ridges <- figridges(data = fl_pelagic_benthic_meta)


### ggridges MLE slope against lat MAIN BODY


slope_ridge_plot <- ggplot(slope_pelagic_benthic, aes(x=beta_slope, y= lat_in))+
  ggridges::geom_density_ridges(rel_min_height = 0.01, aes(y = cut_width(lat_in, 10), fill = Type), alpha = 0.7)+
  #scale_x_log10()+
  #xlab('Weight (kg)') +ylab('Latitude')+
  scale_fill_manual(values = c("Midwater" = '#077DAA', 'Seabed' = 'orange'))+
  theme_light() +theme(legend.position = "bottom", axis.title=element_text(size=22),legend.title = element_blank(),
                       legend.text = element_text(size =22), 
                       axis.text.x = element_text(size=16),
                       axis.text.y = element_text(size=16))

slope_ridge_plot


##### species rank order weights by quantiles  MAIN BODY
fig_sp_rank<- fl_species_rank_order_hori(data = fl_pelagic_benthic_meta, lower.line=0.01, upper.line=0.99)# define quantiles for lines



###Fig 1 multiplot sampling overview/species rank order/response variables

fig_1_multi <- cowplot::ggdraw() +
  cowplot::draw_plot(fig_map, 0, .51, 1, .54) +
  cowplot::draw_plot(fig_sp_rank+ggplot2::theme(legend.position="none"),  0, .25,   1,    .32) +
  cowplot::draw_plot(fig_median+ggplot2::theme(legend.position="none"),   0,    0, .33,  .25) +
  cowplot::draw_plot(fig_slope+ggplot2::theme(legend.position="none"),  .33,    0, .34, .25) +
  cowplot::draw_plot(fig_max+ggplot2::theme(legend.position="none"),  .66,    0, .34, .25) 
  

ggsave(fig_1_multi, filename = here::here("outputs", "fig_1_multi.png"), width = 14, height = 18, units = "in", dpi =300)


### forklength against weight log-log SUPPLEMENTARY MATERIAL 

fig_fl_length_weight <- fl_lengthweight(data = fl_pelagic_benthic_meta)



##### species rank order weights by quantiles SUPPLEMENTARY MATERIAL

fig_fl_species_rank_vert <- fl_species_rank_order_vert(data = fl_pelagic_benthic_meta, lower.line=0.01, upper.line=0.99)# define quantiles for lines

fig_fl_species_rank_upper <- fl_species_rank_order_quan(data = fl_pelagic_benthic_meta, lower.quan = 0.99, upper.quan = 1, breaks = c(1, 10, 100,1000))

fig_fl_species_rank_lower <- fl_species_rank_order_quan(data = fl_pelagic_benthic_meta, lower.quan = 0, upper.quan = 0.01, breaks = c(0.0001, 0.001, 0.01, 0.1))




              
##### plot binomial ridge weights
library(tidyverse)
library(ggridges)

fl_pelagic_benthic_meta_max <- fl_pelagic_benthic_meta [ which (fl_pelagic_benthic_meta$weight_kg >1),]

fl_pelagic_benthic_meta_max %>% 
  dplyr::mutate(Binomial= fct_reorder(.f = Binomial, .x = weight_kg, .fun = mean)) %>% 
  ggplot(aes(x=weight_kg, y=Binomial)) + 
  geom_density_ridges(rel_min_height = 0.01, aes(x=weight_kg, y=Binomial, height = weight_kg, fill =Type, colour = Type, alpha = 0.7), stat = "identity", scale = 3)+
  scale_y_discrete(guide = guide_axis(n.dodge=5), label=abbreviate)+
  scale_x_log10() + scale_fill_manual(values = c("Midwater" = '#077DAA', 'Seabed' = 'orange'))+ scale_colour_manual(values = c("Midwater" = '#077DAA', 'Seabed' = 'orange'))


BRUVSeffort <- ggdraw() +
  draw_plot(PelBRUVSmap, 0, .5, 1, .5) +
  draw_plot(BeBRUVSmap, 0, 0, 1, .5) 
BRUVSeffort


##################### plot response variable from David ###### MAIN BODY
####### plot MLE slope

slope_benthic <- readr::read_table(here::here("data", "dm_response", "slope_MLE_by_day_10indmin_benthic.txt"), col_names = TRUE)
slope_benthic %>% 
  dplyr::mutate(Type = "Seabed") %>% 
  dplyr::mutate(Type = as.factor(Type)) -> slope_benthic


slope_pelagic <- readr::read_table(here::here("data", "dm_response", "slope_MLE_by_day_10indmin_pelagic.txt"), col_names = TRUE)
slope_pelagic %>% 
  dplyr::mutate(Type = "Midwater") %>% 
  dplyr::mutate(Type = as.factor(Type)) -> slope_pelagic

slope_pelagic_benthic <- rbind(slope_pelagic, slope_benthic)


MLE_map <- response_globalmap(world = WorldData, mar = mar, meta_pb = slope_pelagic_benthic)

##compute medians

slope_pelagic_benthic %>%
  group_by(Type) %>%
  summarize(median=median(beta_slope)) -> slope_pelagic_benthic_median 

## density plot of MLE slope                               
fig_slope <- ggplot2::ggplot() + geom_density(data = slope_pelagic_benthic, aes(x= beta_slope, fill = Type), alpha = .6)+
  geom_vline(data = slope_pelagic_benthic_median, aes(xintercept = median, color = Type), linetype ="dashed", size=1.5)+
  scale_colour_manual(values = c("Midwater" = '#077DAA', 'Seabed' = 'darkorange'))+
  scale_fill_manual(values = c("Midwater" = '#077DAA', 'Seabed' = 'orange')) + xlim(c(-1.7, -0.5))+xlab("Beta slope value")+
  theme_light() +theme(legend.position = "bottom", axis.title=element_text(size=20),legend.title = element_blank(),
                       legend.text = element_text(size =22), 
                       axis.text.x = element_text(size=16),
                       axis.text.y = element_text(size=16))
fig_slope
ggsave(fig_slope_MLE_dense, filename = here::here("outputs", "fig_slope_density.png"), width = 12, height = 10, units = "in", dpi =300)


### density plot of maxsize plots
maxsize_pelagic <- readr::read_table(here::here("data","dm_response", "medianmaxsize_pelagic.txt"), col_names = TRUE)

maxsize_map_pelagic <- response_globalmap(world = WorldData, mar = mar, meta_pb = maxsize_pelagic)



maxsize_benthic <- readr::read_table(here::here("data","dm_response", "cov", "medianmaxsize_benthic.txt"), col_names = TRUE)

maxsize_pelagic %>% 
  dplyr::mutate(Type = "Midwater") %>% 
  dplyr::mutate(Type = as.factor(Type)) -> maxsize_pelagic

maxsize_benthic %>% 
  dplyr::mutate(Type = "Seabed") %>% 
  dplyr::mutate(Type = as.factor(Type)) -> maxsize_benthic

maxsize_pelagic_benthic <- rbind(maxsize_pelagic, maxsize_benthic)


fig_max <- ggplot2::ggplot() + ggplot2::geom_density(data = maxsize_pelagic_benthic, aes(x= median.max.size, fill = Type), alpha = .6)+
  scale_colour_manual(values = c("Midwater" = '#077DAA', 'Seabed' = 'darkorange'))+
  scale_x_log10(limits=c(0.0001,10000), breaks =c(1, 10, 1000))+xlab("Max body size (kg)")+
  scale_fill_manual(values = c("Midwater" = '#077DAA', 'Seabed' = 'orange'))+
  theme_light() +theme(legend.position = "bottom", axis.title=element_text(size=22),legend.title = element_blank(),
                       legend.text = element_text(size =20), 
                       axis.text.x = element_text(size=16),
                       axis.text.y = element_text(size=16))
fig_max
options(scipen=1)

  
ggsave(fig_max, filename = here::here("outputs", "fig_median_maxsize_dense.png"), width = 12, height = 10, units = "in", dpi =300)

### density plot of median_meansize plots


median_mean_size_pelagic <- readr::read_table(here::here("data", "dm_response","medianmeansize_pelagic.txt"), col_names = TRUE)

mediansize_map_pelagic <- response_globalmap(world = WorldData, mar = mar, meta_pb = median_mean_size_pelagic)


median_mean_size_benthic <- readr::read_table(here::here("data","dm_response", "cov", "medianmeansize_benthic.txt"), col_names = TRUE)

median_mean_size_pelagic %>% 
  dplyr::mutate(Type = "Midwater") %>% 
  dplyr::mutate(Type = as.factor(Type)) -> median_mean_size_pelagic

median_mean_size_benthic %>% 
  dplyr::mutate(Type = "Seabed") %>% 
  dplyr::mutate(Type = as.factor(Type)) -> median_mean_size_benthic

median_mean_size_pelagic_benthic <- rbind(median_mean_size_pelagic, median_mean_size_benthic)


fig_median <- ggplot2::ggplot() + geom_density(data = median_mean_size_pelagic_benthic, aes(x= median.median.size, fill = Type), alpha = .6)+
  scale_colour_manual(values = c("Midwater" = '#077DAA', 'Seabed' = 'darkorange'))+
  scale_fill_manual(values = c("Midwater" = '#077DAA', 'Seabed' = 'orange')) +
  scale_x_log10(breaks =c(0.1, 1, 10)) + xlab("Median body size (kg)")+
  theme_light() +theme(legend.position = "bottom", axis.title=element_text(size=22),legend.title = element_blank(),
                       legend.text = element_text(size =20), 
                       axis.text.x = element_text(size=16),
                       axis.text.y = element_text(size=16))

fig_median

ggsave(fig_median_size_dense, filename = here::here("outputs", "fig_median_size_dense.png"), width = 12, height = 10, units = "in", dpi =300)


################ Maturity ##############

maturity_pelagic <- readr::read_table(here::here("data", "dm_response", "maturity_pelagic.txt"), col_names = TRUE)
maturity_benthic <- readr::read_table(here::here("data", "dm_response", "maturity_benthic.txt"), col_names = TRUE)

maturity_pelagic %>% 
  dplyr::mutate(Type = "Midwater") %>% 
  dplyr::mutate(Type = as.factor(Type)) -> maturity_pelagic

maturity_benthic %>% 
  dplyr::mutate(Type = "Seabed") %>% 
  dplyr::mutate(Type = as.factor(Type)) -> maturity_benthic

maturity_pelagic_benthic <- rbind(maturity_pelagic, maturity_benthic)

fig_mean_maturity <- ggplot2::ggplot() + geom_density(data = maturity_pelagic_benthic, aes(x= mean_maturity, fill = Type), alpha = .6)+
  scale_colour_manual(values = c("Midwater" = '#077DAA', 'Seabed' = 'darkorange'))+
  scale_fill_manual(values = c("Midwater" = '#077DAA', 'Seabed' = 'orange')) +
  #scale_x_log10(breaks =c(0.1, 1, 10)) + xlab("Median body size (kg)")+
  theme_light() +theme(legend.position = "bottom")

ggsave(fig_mean_maturity, filename = here::here("outputs","maturity", "fig_mean_maturity.png"), width = 8, height = 6, units = "in", dpi =300)



fig_median_maturity <- ggplot2::ggplot() + geom_density(data = maturity_pelagic_benthic, aes(x= median_maturity, fill = Type), alpha = .6)+
  scale_colour_manual(values = c("Midwater" = '#077DAA', 'Seabed' = 'darkorange'))+
  scale_fill_manual(values = c("Midwater" = '#077DAA', 'Seabed' = 'orange')) +
  #scale_x_log10(breaks =c(0.1, 1, 10)) + xlab("Median body size (kg)")+
  theme_light() +theme(legend.position = "bottom")

ggsave(fig_median_maturity, filename = here::here("outputs","maturity", "fig_median_maturity.png"), width = 8, height = 6, units = "in", dpi =300)


