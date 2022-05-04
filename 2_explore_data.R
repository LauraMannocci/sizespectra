#load all functions ----
devtools::document()
devtools::load_all() ### load packages and functions in R folder


# explore bruvs data



########### PELAGIC bruvs-----

# load pelagic data-- 

load("1_read_clean_pelagic.RData")


## make map meta
#make_map_meta(meta_pelagic, "pelagic")



#read clean pelagic response with envar (produced using david_code.R)

response_pelagic <- read.table(here::here("outputs", "pelagic", file="size_response_envar_pelagic_clean.txt"))



########### BENTHIC bruvs----


# load benthic data---

load("1_read_clean_benthic.RData")


### make map meta
make_map_meta(meta_benthic, "benthic")





### load pelagic benthic meta and merged fork lengths data

load("1_read_clean_pelagic_benthic.RData")



################# SAMPLING EFFORT MAP ################### MAIN BODY----
#map shapefiles
WorldData <- ggplot2::map_data("world") #%>% filter(region != "Antarctica") %>% fortify
dsn_mar_layer <- "/Users/tomletessier/Documents/R stuff/Rcourse_graphics_Nov2011/"
mar <- rgdal::readOGR(dsn = dsn_mar_layer, layer = "World_Maritime_Boundaries_v8") #maritime boundary


#map
fig_map <- globalmap(world = WorldData, mar = mar, meta_pb = meta_pelagic_benthic)

### ggridges weight against lat MAIN BODY

fig_ridges <- figridges(data = fl_pelagic_benthic_meta)   #tropical latitude = 23.10S-23.10N, subtropical latitude = +/-23.5N-38N, temperate 38-70

##### species rank order weights with marginal violin  MAIN BODY

fig_sp_rank <- fl_species_ord_marg(data = fl_pelagic_benthic_meta, lower.line=0.001, mid.line = 1, upper.line=100)# define quantiles for lines



### min max size spectra by latitudinal band

ggplot(data=fl_pelagic_benthic_meta, aes(x=abs(lat_in), colour = Type, fill =Type)) + geom_histogram(position ="dodge", alpha=.4) ### check number of bruvs with lat

fl_pelagic_benthic_meta_max = fl_pelagic_benthic_meta[fl_pelagic_benthic_meta$weight_kg >  0.001, ]# select predatory fish - individuals larger than 35 cm
fl_pelagic_benthic_meta_min = fl_pelagic_benthic_meta[fl_pelagic_benthic_meta$weight_kg <  0.8, ]# select forage fish
fl_pelagic_benthic_meta_min = fl_pelagic_benthic_meta_min[fl_pelagic_benthic_meta_min$weight_kg > 0.002, ]


#pareto distribution - cumulative distribution plotting - more appropriate to MLE 
options(scipen=3)

points_lcd <- ggplot2::ggplot(fl_pelagic_benthic_meta, aes(weight_kg), alpha=.6)+
  stat_ecdf(geom="smooth", aes(y = 1 - ..y.., colour=Type), pad =FALSE) +
  scale_y_log10(breaks = c(0.0001, 0.01, 1), labels =c('0.01%','1%','100%'))+ scale_x_log10()+theme_light() +
  scale_colour_manual(values = c("Midwater" = '#077DAA', 'Seabed' = 'darkorange'))+
  theme(legend.position = "bottom", legend.title = element_blank(), axis.title.x=element_text(size=22),
      legend.text = element_text(size =22),axis.text.x = element_text(size=16),
      axis.text.y = element_text(size=16), axis.title.y=element_text(size=22),strip.text.y = element_text(size = 16))+
  xlab('Body size (x, kg)')+ylab('Proportion of values ≥ x')+
  facet_grid(cut_number(-abs(lat_in), n=6) ~.)
  

points_lcd

ggsave(points_lcd, filename = here::here("outputs", "points_lcd.jpeg"), width = 10, height = 16, units = "in", dpi =300)#render cowplots in jpeg less you get seethrough bits


# bin data
options(scipen=6)
points_max <- ggplot2::ggplot()+
  stat_bin(data = fl_pelagic_benthic_meta_max, aes(x=weight_kg, group = cut_number(abs(lat_in), n=6)), alpha=.5, position = "identity",fill = NA, colour = 'darkgreen', geom = "point")+
  stat_bin(data = subset(fl_pelagic_benthic_meta_max, Type =="Seabed") , aes(x=weight_kg, group = cut_number(abs(lat_in), n=6)), fill = NA, colour = 'darkorange', alpha = 0.6, position = "identity", boundary=0.33, geom = "point")+
  stat_bin(data = subset(fl_pelagic_benthic_meta_max, Type =="Midwater"), aes(x=weight_kg, group = cut_number(abs(lat_in), n=6)), fill = NA, colour = '#077DAA', alpha = 0.6, position = "identity", boundary=0.33, geom = "point")+
  scale_y_log10(oob = scales::squish_infinite)+scale_x_log10()+theme_light()

points_min <- ggplot2::ggplot()+
  stat_bin(data = fl_pelagic_benthic_meta_min, aes(x=weight_kg, group = cut_number(abs(lat_in), n=6)), alpha=.5, position = "identity",fill = NA, colour = 'darkgreen', geom = "point")+
  stat_bin(data = subset(fl_pelagic_benthic_meta_min, Type =="Midwater"),aes(x=weight_kg, group = cut_number(abs(lat_in), n=6)), fill = NA, colour = '#077DAA', alpha = 0.6, position = "identity", boundary=0.33, geom = "point")+
  stat_bin(data = subset(fl_pelagic_benthic_meta_min, Type =="Seabed"), aes(x=weight_kg, group = cut_number(abs(lat_in), n=6)), fill = NA, colour = 'darkorange', alpha = 0.6, position = "identity", boundary=0.33, geom = "point")+
  scale_y_log10(oob = scales::squish_infinite)+scale_x_log10()+theme_light()

  dat_combined_max <- subset(layer_data(points_max, i = 1), count>0.1)
  dat_pelagic_max  <- subset(layer_data(points_max, i = 3), count>0.1)
  dat_benthic_max  <- subset(layer_data(points_max, i = 2), count>0.1)
  
  dat_combined_min <- subset(layer_data(points_min, i = 1), count>0.1)
  dat_pelagic_min  <- subset(layer_data(points_min, i = 2), count>0.1)
  dat_benthic_min  <- subset(layer_data(points_min, i = 3), count>0.1)
                             
points_max_min <-ggplot2::ggplot()+
                geom_point(data = dat_combined_max, aes(10^x, count/0.81), colour = 'darkgreen',alpha = 0.6)+
                geom_point(data = dat_pelagic_max, aes(10^x, count/.62), colour = '#077DAA',alpha = 0.6)+
                geom_point(data = dat_benthic_max, aes(10^x, count), colour = 'darkorange',alpha = 0.6)+
               #geom_point(data = dat_combined_min, aes(10^x, count/0.81), colour = 'darkgreen',alpha = 0.6)+
               #geom_point(data = dat_pelagic_min, aes(10^x, count/.62), colour = '#077DAA',alpha = 0.6)+
               #geom_point(data = dat_benthic_min, aes(10^x, count), colour = 'darkorange',alpha = 0.6)+
                #max > .8 kg size regressions
                stat_smooth(data = dat_combined_max, aes(10^x, count/0.81, group = group), colour = 'darkgreen', method  = "lm", alpha = 0.1, size = 0.1,se=F)+
                stat_smooth(data = dat_combined_max, aes(10^x, count/0.81), colour = 'darkgreen', fill = "darkgreen", method  = "lm", alpha = 0.4, size = 1)+
                stat_smooth(data = dat_pelagic_max, aes(10^x, count/.62, group = group), colour ='#077DAA', method  = "lm", alpha = 0.1,size = 0.1,se=F)+
                stat_smooth(data = dat_pelagic_max, aes(10^x, count/.62), colour ='#077DAA', fill='#077DAA', method  = "lm", alpha = 0.4,size = 1)+
                stat_smooth(data = dat_benthic_max, aes(10^x, count, group = group), colour ='darkorange', method  = "lm", alpha = 0.1,size = 0.1,se=F)+
                stat_smooth(data = dat_benthic_max, aes(10^x, count), colour ='darkorange',fill ='darkorange', method  = "lm", alpha = 0.4,size = 1)+
                #min 0.002 > kg < .8 size regressions
               # stat_smooth(data = subset(dat_combined_min, count>0.001), aes(10^x, count/0.81, group = group), colour = 'darkgreen', method  = "lm", alpha = 0.1, size = 0.1,se=F)+
               # stat_smooth(data = subset(dat_combined_min, count>0.001), aes(10^x, count/0.81), colour = 'darkgreen', fill = "darkgreen", method  = "lm", alpha = 0.4, size = 1)+
               # stat_smooth(data = subset(dat_pelagic_min, count>0.001), aes(10^x, count/.62, group=group), colour ='#077DAA', method  = "lm", alpha = 0.1, size = 0.1,se=F)+
               # stat_smooth(data = subset(dat_pelagic_min, count>0.001), aes(10^x, count/.62), colour ='#077DAA', fill ='#077DAA', method  = "lm", alpha = 0.4, size = 1)+
               # stat_smooth(data = subset(dat_benthic_min, count>0.001), aes(10^x, count, group=group), colour ='darkorange', method  = "lm", alpha = 0.1, size = 0.1,se=F)+
               # stat_smooth(data = subset(dat_benthic_min, count>0.001), aes(10^x, count), colour ='darkorange',fill ='darkorange', method  = "lm", alpha = 0.4, size = 1)+
                scale_y_log10(oob = scales::squish_infinite) + scale_x_log10() + theme_light() + xlab("Body size (kg)")+ ylab("Count")+
                theme(legend.position = "none", axis.title.x=element_text(size=22),legend.title = element_blank(),
                legend.text = element_text(size =22),axis.text.x = element_text(size=16),
                axis.text.y = element_text(size=16), axis.title.y=element_text(size=22))
  
points_max_min

ggsave(points_max_min, filename = here::here("outputs", "points_hist_max.jpeg"), width = 16, height = 10, units = "in", dpi =300)#render cowplots in jpeg less you get seethrough bits


#linear model for coefficient - account for different in number bruvs between pelagic and benthic (and then adjusting the combined)
lm_combined_max <- lm(log(count/0.81) ~ x, data = dat_combined_max)
lm_pelagic_max <- lm(log(count/.62) ~ x, data = dat_pelagic_max)
lm_benthic_max <- lm(log(count) ~ x, data = dat_benthic_max)
lm_combined_min <- lm(log(count/0.81) ~ x, data = dat_combined_min)
lm_pelagic_min <- lm(log(count/.62) ~ x, data = dat_pelagic_min)
lm_benthic_min <- lm(log(count) ~ x, data = dat_benthic_min)

lm_combined_max_cf <-  cbind(coef(lm_combined_max), confint(lm_combined_max))
lm_pelagic_max_cf  <-  cbind(coef(lm_pelagic_max), confint(lm_pelagic_max))
lm_benthic_max_cf  <-  cbind(coef(lm_benthic_max), confint(lm_benthic_max))
lm_combined_min_cf <-  cbind(coef(lm_combined_min), confint(lm_combined_min))
lm_pelagic_min_cf  <-  cbind(coef(lm_pelagic_min), confint(lm_pelagic_min))
lm_benthic_min_cf  <-  cbind(coef(lm_benthic_min), confint(lm_benthic_min))

lm_coefs <- rbind(lm_combined_max_cf, lm_pelagic_max_cf, lm_benthic_max_cf, lm_combined_min_cf, lm_pelagic_min_cf, lm_benthic_min_cf)

lm_coefs_max <- rbind(lm_combined_max_cf, lm_pelagic_max_cf, lm_benthic_max_cf)

write.table(lm_coefs_max, file=here::here("outputs", "table", "lm_coefs_max.csv"))



#attempting quadratic polynomial using nonlinear regression techniques (Yurista et al 2014 Can J Fish Aqua. Sci)
# log10(g·m2 · g1)  A0.5(C)[log10g)  B]2

points_max_quad <- ggplot2::ggplot()+
  geom_point(data = dat_combined_max, aes(10^x, count/0.81, group = group), colour = 'darkgreen',alpha = 0.6)+
  geom_point(data = dat_pelagic_max, aes(10^x, count/.62), colour = '#077DAA',alpha = 0.6)+
  geom_point(data = dat_benthic_max, aes(10^x, count), colour = 'darkorange',alpha = 0.6)+
  stat_smooth(data = dat_combined_max, aes(10^x, count/0.81, group = group), formula = y ~ x + I(x^2), colour = 'darkgreen',fill = "darkgreen", method  = "lm", alpha =0.6)+
  #stat_smooth(data = dat_combined_max, aes(10^x, count/0.81), formula = y ~ x + I(x^2), colour = 'darkgreen', fill = "darkgreen", method  = "lm", alpha = 0.4, size = 1)+
  stat_smooth(data = dat_pelagic_max, aes(10^x, count/.62, group = group),formula = y ~ x + I(x^2), colour ='#077DAA',fill='#077DAA', method  = "lm", alpha = 0.6)+
  #stat_smooth(data = dat_pelagic_max, aes(10^x, count/.62), formula = y ~ x + I(x^2),colour ='#077DAA', fill='#077DAA', method  = "lm", alpha = 0.4,size = 1)+
  stat_smooth(data = dat_benthic_max, aes(10^x, count, group = group), formula = y ~ x + I(x^2), colour ='darkorange',fill ='darkorange', method  = "lm", alpha = 0.6)+
  #stat_smooth(data = dat_benthic_max, aes(10^x, count), formula = y ~ x + I(x^2), colour ='darkorange',fill ='darkorange', method  = "lm", alpha = 0.4,size = 1)+
  scale_y_log10(oob = scales::squish_infinite) + scale_x_log10()+theme_light()+ xlab("Body size (kg)")+ ylab("Count")+
  theme(legend.position = "none", axis.title.x=element_text(size=22),legend.title = element_blank(),
        legend.text = element_text(size =22),axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16), axis.title.y=element_text(size=22))+
        facet_grid(rows = vars(-group))+
        geom_vline(xintercept = 26.42, colour = 'darkgrey', linetype="dashed", size=1)
  
points_max_quad 

ggsave(points_max_quad, filename = here::here("outputs", "points_max_quad.jpeg"), width = 10, height = 16, units = "in", dpi =300)#render cowplots in jpeg less you get seethrough bits

#linear quadratic regressions

quad_combined_max <- lm(log(count/0.81) ~ x + x^2*as.factor(group), data = dat_combined_max)
quad_pelagic_max <- lm(log(count/0.62) ~ x + x^2, data = dat_pelagic_max)
quad_benthic_max <- lm(log(count) ~ x + x^2, data = dat_benthic_max)

summary(lm_combined_max)
summary(quad_combined_max)

summary(lm_pelagic_max)
summary(quad_pelagic_max)

summary(lm_benthic_max)
summary(quad_benthic_max)


cm <- rbind(coef(quad_pelagic_max),coef(quad_benthic_max)) # Coefficient matrix
c(-solve(cbind(cm[,2],-1)) %*% cm[,1])
#[1] 4.319192 3.113860


### global size spectra with regression using a stacked histogramm

hist_spectra <- hist_spectra(data=fl_pelagic_benthic_meta)



### global biomass pyramid

biomass_pyramid <- biomass_pyramid(data=fl_pelagic_benthic_meta)


###Fig 1 multiplot sampling overview/species rank order/response variables MAIN BODY FIG 1

fig_1_sample <- cowplot::ggdraw() +
  cowplot::draw_plot(fig_map, 0, .60, 1, .40) +
  cowplot::draw_plot(fig_sp_rank,  0, .30,  1,  .30) +
  cowplot::draw_plot(biomass_pyramid,  0, 0,  .5,  .30)+ 
  cowplot::draw_plot(hist_spectra,  .5, 0,  .45,  .30) 
  
ggsave(fig_1_sample, filename = here::here("outputs", "fig_1_sample.jpeg"), width = 16, height = 20, units = "in", dpi =300)#render cowplots in jpeg less you get seethrough bits





### ggridges responses slope against lat MAIN BODY

## beta slope
slope_ridge_plot <- ggplot(slope_pelagic_benthic, aes(x=beta_slope, y= lat_in))+
  ggridges::geom_density_ridges(rel_min_height = 0.01, aes(y = cut(lat_in, breaks = c(-38, -32.5, -27.5, -22.5, -17.5, -12.5, -7.5, -2.5,  7.5,  22.5, 32.5, 35, 67.5)), fill = Type, colour = Type), 
                                alpha = 0.6, scale =1.8, jittered_points = TRUE, quantile_lines = TRUE, quantiles = 0.5, vline_size = 1.5, 
                                position = position_points_jitter(height = 0.2, yoffset= 0.2, adjust_vlines = TRUE),point_size = 0.01, point_alpha = 0)+
  scale_x_continuous(limits = c(-2, 0))+
  #xlab('Weight (kg)') +ylab('Latitude')+
  scale_colour_manual(values = c("Midwater" = '#077DAA', 'Seabed' = 'darkorange'))+
  scale_fill_manual(values = c("Midwater" = '#077DAA', 'Seabed' = 'orange'), guide = guide_legend(label.position = "bottom",label.theme = element_text(angle = 90, size=22), 
                        ncol = 1))+
  theme_light() +theme(legend.position = "right", axis.title=element_text(size=22),legend.title = element_blank(),
                       legend.text = element_text(size =22), 
                       axis.text.x = element_text(size=16),
                       axis.text.y = element_text(size=16), axis.title.y=element_blank())

slope_ridge_plot
## max size
options(scipen=4)
max_ridge_plot <- ggplot(maxsize_pelagic_benthic, aes(x=median_max_size, y= lat_in))+
  ggridges::geom_density_ridges(rel_min_height = 0.01, aes(y = cut_width(lat_in, 10), fill = Type), alpha = 0.7)+
  scale_x_log10()+
  xlab('Max body size (kg)') +ylab('Latitude')+
  scale_fill_manual(values = c("Midwater" = '#077DAA', 'Seabed' = 'orange'))+
  theme_light() +theme(legend.position = "none", axis.title=element_text(size=22),legend.title = element_blank(),
                       legend.text = element_text(size =22), 
                       axis.text.x = element_text(size=16),
                       axis.text.y = element_blank(), axis.title.y=element_blank())
max_ridge_plot

#median size
median_ridge_plot <- ggplot(median_mean_size_pelagic_benthic, aes(x=median_median_size, y= lat_in))+
  ggridges::geom_density_ridges(rel_min_height = 0.01, aes(y = cut_width(lat_in, 10), fill = Type), alpha = 0.7)+
  scale_x_log10()+
  xlab('Median body size (kg)') +ylab('Latitude')+
  scale_fill_manual(values = c("Midwater" = '#077DAA', 'Seabed' = 'orange')) +
  theme_light() +theme(legend.position = "none", axis.title=element_text(size=22),legend.title = element_blank(),
                       legend.text = element_text(size =22), 
                       axis.text.x = element_text(size=16),
                       axis.text.y = element_text(size=16))
median_ridge_plot

fig_2_response <- cowplot::ggdraw() +
  cowplot::draw_plot(median_ridge_plot, 0, 0, .33, 1) +
  cowplot::draw_plot(max_ridge_plot,  0.3, 0,  .35,  1)+ 
  cowplot::draw_plot(slope_ridge_plot,  0.65, 0,  .35,  1) 

fig_2_response
  
ggsave(fig_2_response, filename = here::here("outputs", "fig_2_response.png"), width = 16, height = 16, units = "in", dpi =300)
  
### forklength against weight log-log SUPPLEMENTARY MATERIAL 
  
fig_fl_length_weight <- fl_lengthweight(data = fl_pelagic_benthic_meta)



##### species rank order weights by quantiles SUPPLEMENTARY MATERIAL

fig_fl_species_rank_vert <- fl_species_rank_order_vert(data = fl_pelagic_benthic_meta, lower.line=0.01, upper.line=0.99)# define quantiles for lines

fig_fl_species_rank_upper <- fl_species_rank_order_quan(data = fl_pelagic_benthic_meta, lower.quan = 0.99, upper.quan = 1, breaks = c(1, 10, 100,1000))

fig_fl_species_rank_lower <- fl_species_rank_order_quan(data = fl_pelagic_benthic_meta, lower.quan = 0, upper.quan = 0.01, breaks = c(0.0001, 0.001, 0.01, 0.1))


              
##### weights facet_grid stacked by exped.
options(scipen=4)

fig_weights <- ggplot2::ggplot() + geom_density(data = fl_pelagic_benthic_meta, 
                                                aes(x= weight_kg, fill = Type, colour = Type, group = Exped), 
                                                alpha = .3, adjust = 3, position = "stack") + 
  facet_grid(Type ~ ., scales ="free_y")+
  #scale_x_continuous(limits = c(0, 1))+
  scale_x_log10()+
  scale_colour_manual(values = c("Midwater" = '#077DAA', 'Seabed' = 'darkorange'))+
  scale_fill_manual(values = c("Midwater" = '#077DAA', 'Seabed' = 'orange')) +# xlim(c(-1.7, -0.5))+#xlab("Beta slope value")+
  theme_light() +theme(legend.position = "bottom", axis.title=element_text(size=20),legend.title = element_blank(),
                       legend.text = element_text(size =22), 
                       axis.text.x = element_text(size=16),
                       axis.text.y = element_text(size=16))
fig_weights


##################### plot response variable from David ###### MAIN BODY ####
####### plot MLE slope

slope_pelagic <- read.table(here::here("data", "response", "slope_MLE_by_day_20indmin_pelagic.txt"), header = TRUE)

slope_benthic <- read.table(here::here("data", "response", "slope_MLE_by_day_20indmin_benthic.txt"), col_names = TRUE)


slope_pelagic %>% 
  dplyr::mutate(Type = "Midwater") %>% 
  dplyr::mutate(Type = as.factor(Type)) %>% 
  dplyr::filter(beta_slope <0.5 ) %>%
  dplyr::mutate(number_of_BRUVS = number_of_BRUVS* 5)-> slope_pelagic

slope_benthic %>% 
  dplyr::mutate(Type = "Seabed") %>% 
  dplyr::mutate(Type = as.factor(Type)) -> slope_benthic


slope_pelagic_benthic <- rbind(slope_pelagic, slope_benthic)


ggplot2::ggplot() + geom_density(data=slope_pelagic_benthic, aes(x=number_of_BRUVS, fill =Type), alpha =.5) 

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




### plot modes-----

mode_pelagic <- read.table(here::here("data", "response", "modes_20indmin_pelagic.txt"), header = TRUE)
mode_benthic <- read.table(here::here("data", "response", "modes_20indmin_benthic.txt"), header = TRUE)

summary(mode_benthic)

### density plot of maxsize plots
maxsize_pelagic <- readr::read_table(here::here("data","dm_response", "medianmaxsize_pelagic.txt"), col_names = TRUE)

maxsize_map_pelagic <- response_globalmap(world = WorldData, mar = mar, meta_pb = maxsize_pelagic)


maxsize_benthic <- readr::read_table(here::here("data","dm_response", "medianmaxsize_benthic.txt"), col_names = TRUE)

maxsize_map_benthic <- response_globalmap(world = WorldData, mar = mar, meta_pb = maxsize_benthic)



maxsize_pelagic %>% 
  dplyr::mutate(Type = "Midwater") %>% 
  dplyr::mutate(Type = as.factor(Type)) -> maxsize_pelagic

maxsize_benthic %>% 
  dplyr::mutate(Type = "Seabed") %>% 
  dplyr::mutate(Type = as.factor(Type)) -> maxsize_benthic

maxsize_pelagic_benthic <- rbind(maxsize_pelagic, maxsize_benthic)


fig_max <- ggplot2::ggplot() + ggplot2::geom_density(data = maxsize_pelagic_benthic, aes(x= median_max_size, fill = Type), alpha = .6)+
  scale_colour_manual(values = c("Midwater" = '#077DAA', 'Seabed' = 'darkorange'))+
  scale_x_log10(limits=c(0.0001,10000), breaks =c(1, 10, 1000))+xlab("Max body size (kg)")+
  scale_fill_manual(values = c("Midwater" = '#077DAA', 'Seabed' = 'orange'))+
  theme_light() +theme(legend.position = "bottom", axis.title=element_text(size=22),legend.title = element_blank(),
                       legend.text = element_text(size =20), 
                       axis.text.x = element_text(size=16),
                       axis.text.y = element_text(size=16))
fig_max
options(scipen=1)

  
ggsave(fig_max, filename = here::here("outputs", "median_maxsize_dense.png"), width = 12, height = 10, units = "in", dpi =300)

### density plot of median_meansize plots


median_mean_size_pelagic <- readr::read_table(here::here("data", "dm_response","medianmeansize_pelagic.txt"), col_names = TRUE)

mediansize_map_pelagic <- response_globalmap(world = WorldData, mar = mar, meta_pb = median_mean_size_pelagic)


median_mean_size_benthic <- readr::read_table(here::here("data","dm_response", "medianmeansize_benthic.txt"), col_names = TRUE)

median_mean_size_pelagic %>% 
  dplyr::mutate(Type = "Midwater") %>% 
  dplyr::mutate(Type = as.factor(Type)) -> median_mean_size_pelagic

median_mean_size_benthic %>% 
  dplyr::mutate(Type = "Seabed") %>% 
  dplyr::mutate(Type = as.factor(Type)) -> median_mean_size_benthic

median_mean_size_pelagic_benthic <- rbind(median_mean_size_pelagic, median_mean_size_benthic)


fig_median <- ggplot2::ggplot() + geom_density(data = median_mean_size_pelagic_benthic, aes(x= median_median_size, fill = Type), alpha = .6)+
  scale_colour_manual(values = c("Midwater" = '#077DAA', 'Seabed' = 'darkorange'))+
  scale_fill_manual(values = c("Midwater" = '#077DAA', 'Seabed' = 'orange')) +
  scale_x_log10(breaks =c(0.1, 1, 10)) + xlab("Median body size (kg)")+
  theme_light() +theme(legend.position = "bottom", axis.title=element_text(size=22),legend.title = element_blank(),
                       legend.text = element_text(size =20), 
                       axis.text.x = element_text(size=16),
                       axis.text.y = element_text(size=16))

fig_median

ggsave(fig_median, filename = here::here("outputs", "median_size_dense.png"), width = 12, height = 10, units = "in", dpi =300)


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


fl_pelagic_benthic_meta2 <- subset(fl_pelagic_benthic_meta, weight_kg >100)
nrow(fl_pelagic_benthic_meta2)

median(fl_pelagic$weight_kg)
[1] 0.008759282
median(fl_benthic$weight_kg)
[1] 0.0585

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




  


