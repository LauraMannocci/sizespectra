#' read market location
#'
#' @return
#' @export
#'

read_data_with_market <- function(){
  
  read.csv(here::here("data", "market.csv"))
  
}



#' read port location
#'
#' @return
#' @export
#'

read_data_with_port <- function(){
  
  read.csv(here::here("data", "named_anchorages_v1_20181108.csv"))
  
}



#' make photo combination
#'
#' @return
#' @export
#' @import cowplot
#' @import ggplot2
#'

draw_screengrab_combo <- function(){
  
  white_shark_pel <- cowplot::ggdraw() + cowplot::draw_image(here::here("photo","vlcsnap-2022-12-21-09h20m25s642.png")) 
  #hammerhead_shark_pel <- cowplot::ggdraw() + cowplot::draw_image(here::here("photo","Sphyrna_mokarran_c.png")) 
  #mako_shark_pel <- cowplot::ggdraw() + cowplot::draw_image(here::here("photo","RAW19P_103_Isurus_oxyrinchus.jpg")) 
  silky_shark_pel <- cowplot::ggdraw() + cowplot::draw_image(here::here("photo","vlcsnap-2021-10-22-04h02m24s551.png")) 
  yft_pel <- cowplot::ggdraw() + cowplot::draw_image(here::here("photo","Malpelo Thunnus albacares.png")) 
  juv_carang_pel <- cowplot::ggdraw() + cowplot::draw_image(here::here("photo","juvenile jacks  and scads - Carangidae sp.png")) 
  ad_caranx_pel <- cowplot::ggdraw() + cowplot::draw_image(here::here("photo","Caranx latus b.png")) 
  caesio_chrys_ben <- cowplot::ggdraw() + cowplot::draw_image(here::here("photo","PCI1_064_CaesioChrysozona.jpg")) 
  lut_boh_ben <- cowplot::ggdraw() + cowplot::draw_image(here::here("photo","PCI1_213_LutjanusBohar_Schooling.jpg")) 
  cod_ben <- cowplot::ggdraw() + cowplot::draw_image(here::here("photo","FR07 Rigg 1 Inntian G. morhua.jpg")) 
  pigha_ben <- cowplot::ggdraw() + cowplot::draw_image(here::here("photo","Sharks and cod.PNG")) 
  kyph_ben <- cowplot::ggdraw() + cowplot::draw_image(here::here("photo","kyphosus.png")) 
  crosshatch_ben <- cowplot::ggdraw() + cowplot::draw_image(here::here("photo","cross_hatch.png")) 
  cod_multi_ben <- cowplot::ggdraw() + cowplot::draw_image(here::here("photo","Screenshot (9).png")) 
  tiger_multi_ben <- cowplot::ggdraw() + cowplot::draw_image(here::here("photo","Cockburn_Sound_CSN20_088_Galeocerdo_cuvier.png")) 
  berm_multi_ben <- cowplot::ggdraw() + cowplot::draw_image(here::here("photo","Bermuda_BDA2307_025_Clepticus_parrae-14cm copy.png"))
  
    bruvs_screen_grab_combo <- ggdraw() +
    draw_plot(white_shark_pel,   0,   0.8, 0.55, 0.2) +
    draw_plot(silky_shark_pel,   0,   0.6, 0.55, 0.2) +
    draw_plot(yft_pel,           0,   0.4, 0.55, 0.2) +
    draw_plot(ad_caranx_pel,     0,   0.2, 0.55, 0.2) +
    draw_plot(juv_carang_pel,    0,     0, 0.55, 0.2) +
    draw_plot(tiger_multi_ben, 0.5,   0.8, 0.55, 0.2) +
    draw_plot(lut_boh_ben,     0.5,   0.6, 0.55, 0.2) +
    draw_plot(pigha_ben,       0.504, 0.4, 0.55, 0.2) +
    draw_plot(caesio_chrys_ben,0.5,   0.2, 0.55, 0.2) +
    draw_plot(berm_multi_ben,  0.504,   0.0, 0.55, 0.2) +
      
      
    draw_plot_label(c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"), c(0.01, 0.01, 0.01, 0.01, 0.01, 0.51, 0.51, 0.51, 0.515, 0.515), c(1, .8, .6, .4, .2, 1, .8, .6, .4, .2), size = 12)
  
  print(bruvs_screen_grab_combo)
  
  ggsave(bruvs_screen_grab_combo, filename = here::here("outputs", "fig_1_bruvs_screengrab.jpg"), width = 7.5, height = 10, units = "in", dpi =300)
  
  invisible(bruvs_screen_grab_combo)
}




#' read pelagic benthic summary  location
#'
#' @return
#' @export
#' @import readxl
#'

read_data_pelagic_benthic_sum <- function(){
  
  read_excel(here::here("data", "benthic", "PelagicBenthicMetaSum.xlsx"))
  
}



#' Global map of BRUVS sampling effort
#'
#' Pelagic in blue and benthic in yellow
#'
#' @param world the output of `ggplot2::map_data()`
#' @param mar an sp object: world marine boundaries
#' @param meta_pb coordinates of...
#'
#' @return A ggplot2 object
#' 
#' @export
#' @import ggplot2
#' 
#' @examples
#' ## ...

globalmap <- function(world, mar, meta_pb){
  
  gplot <- ggplot() +
    geom_map(data = world, map = world,
             aes(x = long, y = lat, group = group, map_id=region),fill="gray60",color="gray60", size=0.2) + 
    geom_path(aes(long, lat, group=group),data=mar, color="gray80")+ coord_fixed(1.3, xlim = c(-170, 170), ylim = c(-53, 77)) + 
    geom_point(data= meta_pb, aes(lon_in, lat_in, fill = Type, size = Type), shape = 21, alpha = 0.9) + 
    theme_light()+ theme(legend.position = "bottom", legend.box = "horizontal", 
                         #legend.margin=margin(t = -.6,b=0.5, unit='cm'),
                         panel.spacing=unit(x=c(0,0,0,0),units="mm"), axis.title.y= element_blank(), axis.title.x = element_blank(),axis.text.y = element_text(size = 16),axis.text.x = element_text(size = 16), 
                         legend.text = element_text(size =18),  legend.title = element_blank())+
                         #plot.margin = margin(t = 0,  r = 0,b = 0,  l = 0)) +
    scale_size_manual(values = c("Midwater" =  7, 'Seabed' = 4))+
    scale_y_continuous(breaks = c(-45, -30, -15, 0, 15, 30, 45, 60, 75), labels = c("45° S", "30° S", "15° S", "0", "15° N", "30° N", "45° N", "60° N", "75° N"))+
    scale_x_continuous(breaks = c(-160, -120, -60, 0, 60, 120, 160), labels = c("160° W", "120° W", "60° W", "0", "60° E", "120° E", "160° E"))+
    
    scale_fill_manual(values = c("Midwater" = '#077DAA', 'Seabed' = 'orange'), labels = c("Midwater BRUVS (n=6,701)", "Seabed BRUVS (n=10,710)")) +
    guides(fill = guide_legend(override.aes = list(size = 5)), size = "none")
  
  print(gplot)
  
  ggsave(gplot, filename = here::here("outputs", "globalmap.png"), width = 20, height = 10, units = "in", dpi =300)

  invisible(gplot)
}


#' Global map of BRUVS sampling effort SUMMARY
#'
#' Pelagic in blue and benthic in yellow
#'
#' @param world the output of `ggplot2::map_data()`
#' @param mar an sp object: world marine boundaries
#' @param meta_pb coordinates of...
#'
#' @return A ggplot2 object
#' 
#' @export
#' @import ggplot2
#' 
#' @examples
#' ## ...

globalmap_sum <- function(world, mar, meta_pb){
  
  set.seed(7)
  
  
  
  gplot <- ggplot() +
    geom_map(data = world, map = world,
             aes(x = long, y = lat, group = group, map_id=region),fill="gray60",color="gray60", size=0.2) + 
    geom_path(aes(long, lat, group=group),data=mar, color="gray80")+ coord_fixed(1.3, xlim = c(-170, 170), ylim = c(-53, 77)) + 
    geom_jitter(data= meta_pb, aes(mean_long, mean_lat, fill = as.factor(Type), size = Deployments), shape = 21, alpha = 0.7, width = 2.2, height = 2.2) +
    scale_size_binned(range =c(3,16))+
    theme_light()+ theme(legend.position = "bottom", legend.box = "horizontal", 
                         #legend.margin=margin(t = -.6,b=0.5, unit='cm'),
                         panel.spacing=unit(x=c(0,0,0,0),units="mm"), axis.title.y= element_blank(), axis.title.x = element_blank(),axis.text.y = element_text(size = 16),axis.text.x = element_text(size = 16), 
                         legend.text = element_text(size =18),  legend.title = element_blank())+
    #plot.margin = margin(t = 0,  r = 0,b = 0,  l = 0)) +
   # scale_size_manual(values = c("Midwater" =  7, 'Seabed' = 4))+
    scale_y_continuous(breaks = c(-45, -30, -15, 0, 15, 30, 45, 60, 75), labels = c("45° S", "30° S", "15° S", "0", "15° N", "30° N", "45° N", "60° N", "75° N"))+
    scale_x_continuous(breaks = c(-160, -120, -60, 0, 60, 120, 160), labels = c("160° W", "120° W", "60° W", "0", "60° E", "120° E", "160° E"))+
    
    scale_fill_manual(values = c("Midwater" = '#077DAA', 'Seabed' = 'orange'), labels = c("Pelagic BRUVS (n=6,701)", "Benthic BRUVS (n=10,710)")) +
    guides(fill = guide_legend(override.aes = list(size = 5)))
  
  print(gplot)
  
  ggsave(gplot, filename = here::here("outputs", "globalmap_sum.png"), width = 20, height = 10, units = "in", dpi =300)
  
  invisible(gplot)
}




#' Global map of ports
#'
#'
#' @param world the output of `ggplot2::map_data()`
#' @param mar an sp object: world marine boundaries
#' @param dat coordinates of...
#'
#' @return A ggplot2 object
#' 
#' @export
#' @import ggplot2
#' 
#' @examples
#' ## ...

globalmap_port <- function(world, mar, dat){
  
  gplot <- ggplot() +
    geom_map(data = world, map = world,
             aes(x = long, y = lat, group = group, map_id=region),fill="gray60",color="gray60", size=0.2) + 
    geom_path(aes(long, lat, group=group),data=mar, color="gray80")+ coord_fixed(1.3, xlim = c(-170, 170), ylim = c(-53, 77)) + 
    geom_point(data = dat, aes(lon, lat), colour = "black", fill ="green", shape =21, size = 2) + 
    theme_light()+ theme(legend.position = "bottom", legend.box = "horizontal", 
                         #legend.margin=margin(t = -.6,b=0.5, unit='cm'),
                         panel.spacing=unit(x=c(0,0,0,0),units="mm"), axis.title.y= element_blank(), axis.title.x = element_blank(),axis.text.y = element_text(size = 16),axis.text.x = element_text(size = 16), 
                         legend.text = element_text(size =18),  legend.title = element_blank())+
    #plot.margin = margin(t = 0,  r = 0,b = 0,  l = 0)) +
    scale_y_continuous(breaks = c(-45, -30, -15, 0, 15, 30, 45, 60, 75), labels = c("45° S", "30° S", "15° S", "0", "15° N", "30° N", "45° N", "60° N", "75° N"))+
    scale_x_continuous(breaks = c(-160, -120, -60, 0, 60, 120, 160), labels = c("160° W", "120° W", "60° W", "0", "60° E", "120° E", "160° E"))
    

  print(gplot)
  
  ggsave(gplot, filename = here::here("outputs", "map_port.png"), width = 20, height = 10, units = "in", dpi =300)
  
  invisible(gplot)
}


#' Global map of market
#'
#'
#' @param world the output of `ggplot2::map_data()`
#' @param mar an sp object: world marine boundaries
#' @param dat coordinates of...
#'
#' @return A ggplot2 object
#' 
#' @export
#' @import ggplot2
#' 
#' @examples
#' ## ...

globalmap_market <- function(world, mar, dat){
  
  gplot <- ggplot() +
    geom_map(data = world, map = world,
             aes(x = long, y = lat, group = group, map_id=region),fill="gray60",color="gray60", size=0.2) + 
    geom_path(aes(long, lat, group=group),data=mar, color="gray80")+ coord_fixed(1.3, xlim = c(-170, 170), ylim = c(-53, 77)) + 
    geom_point(data = dat, aes(x=X, y=Y), colour = "black", fill = "red", shape = 21, size =2) + 
    theme_light()+ theme(legend.position = "bottom", legend.box = "horizontal", 
                         #legend.margin=margin(t = -.6,b=0.5, unit='cm'),
                         panel.spacing=unit(x=c(0,0,0,0),units="mm"), axis.title.y= element_blank(), axis.title.x = element_blank(),axis.text.y = element_text(size = 16),axis.text.x = element_text(size = 16), 
                         legend.text = element_text(size =18),  legend.title = element_text())+
    #plot.margin = margin(t = 0,  r = 0,b = 0,  l = 0)) +
    scale_y_continuous(breaks = c(-45, -30, -15, 0, 15, 30, 45, 60, 75), labels = c("45° S", "30° S", "15° S", "0", "15° N", "30° N", "45° N", "60° N", "75° N"))+
    scale_x_continuous(breaks = c(-160, -120, -60, 0, 60, 120, 160), labels = c("160° W", "120° W", "60° W", "0", "60° E", "120° E", "160° E"))
    
  
  print(gplot)
  
  ggsave(gplot, filename = here::here("outputs", "map_market.png"), width = 20, height = 10, units = "in", dpi =300)
  
  invisible(gplot)
}


#' Global map of response variables
#'
#' 
#'
#' @param world the output of `ggplot2::map_data()`
#' @param mar an sp object: world marine boundaries
#' @param meta_pb coordinates of...
#'
#' @return A ggplot2 object
#' 
#' @export
#' @import ggplot2
#' 
#' @examples
#' ## ...

response_globalmap <- function(world, mar, meta_pb){
  
  gplot <- ggplot() +
    geom_map(data = world, map = world,
             aes(x = long, y = lat, group = group, map_id=region),fill="gray60",color="gray60", size=0.2) + 
    geom_path(aes(long, lat, group=group),data=mar, color="gray80")+ coord_fixed(1.3, xlim = c(-170, 170), ylim = c(-53, 77)) + 
    geom_point(data= meta_pb, aes(lon_in, lat_in, fill = beta_slope, shape = Type, size = Type), alpha = 0.9) + 
    theme_light()+ theme(legend.position = "bottom", legend.box = "horizontal", 
                         panel.spacing=unit(x=c(0,0,0,0),units="mm"), axis.title.y= element_blank(), axis.title.x = element_blank(),axis.text.y = element_text(size = 16),axis.text.x = element_text(size = 16), 
                         legend.text = element_text(size =18),  legend.title = element_blank())+
    scale_size_manual(values = c("Midwater" =  7, 'Seabed' = 4))+
    scale_y_continuous(breaks = c(-45, -30, -15, 0, 15, 30, 45, 60, 75), labels = c("45° S", "30° S", "15° S", "0", "15° N", "30° N", "45° N", "60° N", "75° N"))+
    scale_x_continuous(breaks = c(-160, -120, -60, 0, 60, 120, 160), labels = c("160° W", "120° W", "60° W", "0", "60° E", "120° E", "160° E"))+
    scale_shape_manual(values = c("Midwater" =  21, 'Seabed' = 24))+
    scale_fill_continuous(limits = c(-2, -0.5))+
    guides(fill = guide_legend(override.aes = list(size = 5)), size = "none")
  
  print(gplot)
  
  ggsave(gplot, filename = here::here("outputs", "response_globapmap.png"), width = 20, height = 10, units = "in", dpi =300)
  
  invisible(gplot)
}

#' Title fig ridges 
#' 
#' try this to include mode https://rpkgs.datanovia.com/ggpubr/reference/stat_central_tendency.html
#'
#' @param data size data
#' @param min_size minimum size in kg
#' @param lat_band number of latitudinal bans
#' @import ggplot2 ggridges ggstance ggpp
#' @import fishualize
#' @return
#' @export
#'
#' @examples
#' 
figridges <- function(dat, min_size, lat_band){
  options(scipen=7)
  
  data = dat[dat$weight_kg > min_size, ]# individuals larger than minsize kg
  
  
  #rigplot <- ggplot(data, aes(x=weight_kg, y= lat_in, height = ..ndensity..))+
  rigplot <- ggplot(data, aes(x=weight_kg, y=cut(abs(lat_in), breaks = c(0, 10, 15, 20, 23, 33, 65))))+
  
   #ggridges::stat_density_ridges(rel_min_height = 0.01, aes(y = cut_width(lat_in, 5), colour = Type, fill = Type), 
    ggridges::stat_density_ridges(aes(colour = Type, fill =Type, height = ..ndensity..), scale = 0.8, alpha = 0.5, jittered_points = TRUE, 
                                  quantile_lines = TRUE, quantiles = c(0.5, 0.95), 
                                  #position = position_points_jitter(yoffset= 0.2, adjust_vlines = TRUE),
                                  point_size = 0.01, point_alpha = 0)+
    stat_summaryh(fun.x=median, geom="text", aes(colour=Type, label=sprintf("%1.3f", ..x..)),
                  position = position_jitternudge(height = 0.22, y = 0.21, seed = 5,  nudge.from ="jittered.y"), size=5,show.legend = FALSE)+
    stat_summaryh(fun.x=function(z) { quantile(z,0.95) }, geom="text", aes(colour=Type, label=sprintf("%1.1f", ..x..)),
                  position = position_jitternudge(height = 0.22, y = 0.21, seed = 5,  nudge.from ="jittered.y"), size=5,show.legend = FALSE)+
  
    
    scale_x_log10(limits = c(5e-04, 1200), breaks = c(0.001, 0.1, 100), labels = c(0.001, 0.1, 100))+
    xlab('Body size (kg)') +ylab('Frequency density')+
    scale_fill_manual(values = c("Midwater" = '#077DAA', 'Seabed' = 'orange'), labels = c('Pelagic', 'Benthic'))+
    scale_colour_manual(values = c("Midwater" = '#077DAA', 'Seabed' = 'darkorange'), labels = c('Pelagic', 'Benthic'))+
    scale_linetype_manual(breaks=c(0.5,1), values =c("dotted", "solid"))+ 
    scale_y_discrete(expand = expansion(add = c(0.05, 1)))+
    theme_light() +theme(legend.position = c(.80,.97),legend.background = element_rect(fill='transparent'), axis.title=element_text(size=22),legend.title = element_blank(),
                         legend.text = element_text(size =16), 
                         axis.text.x = element_text(size=16),
                         axis.text.y = element_text(size=20))+
    theme(plot.margin = margin(1,1,0,0, "cm"))+
    coord_cartesian(clip = "off")
  print(rigplot)
  
  ggsave(rigplot, filename = here::here("outputs", "fig_ridges.png"), width = 10, height = 16, units = "in", dpi =300)
  
  invisible(rigplot)
  
}


#' Title fig ridges sensitivity long 
#' 
#' try this to include mode https://rpkgs.datanovia.com/ggpubr/reference/stat_central_tendency.html
#'
#' @param data size data
#' @param min_size minimum size in kg
#' @param lon_band number of longitudinal bans
#' @import ggplot2 ggridges ggstance
#' @import fishualize
#' @return
#' @export
#'
#' @examples
#' 
figridges_lon <- function(dat, min_size, lon_band){
  options(scipen=7)
  
  data = dat[dat$weight_kg > min_size, ]# individuals larger than minsize kg
  

  rigplot <- ggplot(data, aes(x=weight_kg, y = cut_number(lon_in, lon_band)))+
    #ggridges::stat_density_ridges(rel_min_height = 0.01, aes(y = cut_width(lat_in, 5), colour = Type, fill = Type), 
      ggridges::stat_density_ridges(aes(colour = Type, fill =Type, height = ..ndensity..), scale = 0.8, alpha = 0.5, jittered_points = TRUE, 
                                    quantile_lines = TRUE, quantiles = c(0.5, 1), 
                                    #position = position_points_jitter(yoffset= 0.2, adjust_vlines = TRUE),
                                    point_size = 0.01, point_alpha = 0)+
      stat_summaryh(fun.x=median, geom="text", aes(colour=Type, label=sprintf("%1.3f", ..x..)),
                    position = position_jitternudge(height = 0.22, y = 0.21, seed = 5,  nudge.from ="jittered.y"), size=5,show.legend = FALSE)+
    stat_summaryh(fun.x=function(z) { quantile(z,0.95) }, geom="text", aes(colour=Type, label=sprintf("%1.1f", ..x..)),
                  position = position_jitternudge(height = 0.22, y = 0.21, seed = 5,  nudge.from ="jittered.y"), size=5,show.legend = FALSE)+
    
    scale_x_log10(limits = c(5e-04, 1200), breaks = c(0.001, 0.1, 100), labels = c(0.001, 0.1, 100))+
    xlab('Body size (kg)') +ylab('Frequency density')+
    scale_fill_manual(values = c("Midwater" = '#077DAA', 'Seabed' = 'orange'), labels = c('Pelagic', 'Benthic'))+
    scale_colour_manual(values = c("Midwater" = '#077DAA', 'Seabed' = 'darkorange'), labels = c('Pelagic', 'Benthic'))+
    scale_linetype_manual(breaks=c(0.5,1), values =c("dotted", "solid"))+ 
    scale_y_discrete(expand = expansion(add = c(0.05, 1)))+
    theme_light() +theme(legend.position = c(.80,.97),legend.background = element_rect(fill='transparent'), axis.title=element_text(size=22),legend.title = element_blank(),
                         legend.text = element_text(size =16), 
                         axis.text.x = element_text(size=16),
                         axis.text.y = element_text(size=20))+
    theme(plot.margin = margin(1,1,0,0, "cm"))+
    coord_cartesian(clip = "off")
  
  print(rigplot)
  
  ggsave(rigplot, filename = here::here("outputs", "fig_ridges_lon.png"), width = 10, height = 16, units = "in", dpi =300)
  
  invisible(rigplot)
  
}


#' Title fig ridges overlap
#' 
#' try this to include mode https://rpkgs.datanovia.com/ggpubr/reference/stat_central_tendency.html
#'
#' @param data size data
#' @param min_size minimum size in kg
#' @param lat_band number of latitudinal bans
#' @import ggplot2 ggridges ggstance
#' @import fishualize
#' @return
#' @export
#'
#' @examples
#' 
figridges_overlap <- function(dat, min_size, lat_band, bandw){
  
  data = dat[dat$weight_kg > min_size, ]# select predatory fish - individuals larger than minsize kg
  
  rigplot <- ggplot(data, aes(x=weight_kg, y= lat_in, height = ..ndensity..))+
    #ggridges::stat_density_ridges(rel_min_height = 0.01, aes(y = cut_width(lat_in, 5), colour = Type, fill = Type), 
    #ggridges::geom_density_ridges(rel_min_height = 0.01, aes(y = cut(abs(lat_in), breaks = c(0, 10, 20, 30, 40, 70)), fill = Type, colour = Type), 
    ggridges::geom_density_ridges(bandwidth = bandw, rel_min_height = 0.005, aes(y = cut_number(abs(lat_in), n=lat_band), fill = Type), colour = "#FFFFFF", 
                                  alpha = 0.1, scale =20)+
    scale_x_log10(limits = c(0.001, 1200), breaks = c(0.01, 1, 100), labels = c(0.01, 1, 100))+
    xlab('Body size (kg)') +ylab('Probability density')+
    scale_fill_manual(values = c("Midwater" = '#077DAA', 'Seabed' = 'orange'))+
    scale_colour_manual(values = c("Midwater" = '#077DAA', 'Seabed' = 'darkorange'))+
    scale_linetype_manual(breaks=c(0.5,1), values =c("dotted", "solid"))+ 
    scale_y_discrete(expand = c(0.1, 0))+
    theme_light() +theme(strip.background = element_blank(),strip.text.x = element_blank(),panel.grid.major.y = element_blank(),legend.position = c(.9,.9), axis.title=element_text(size=22),legend.title = element_blank(),
                         legend.text = element_text(size =16), 
                         axis.text.x = element_text(size=16),
                         axis.text = element_blank(), 
                         legend.background = element_rect(fill = "transparent"))+
    theme(plot.margin = margin(1,1,0,1.2, "cm"))+
    coord_cartesian(clip = "off")+facet_wrap(~Type, nrow =2)

  
  print(rigplot)
  
  ggsave(rigplot, filename = here::here("outputs", "fig_ridges_modes.png"), width = 12, height = 6, units = "in", dpi =300)
  
  invisible(rigplot)
  
}

#' length weight relationship with marginal density plot
#'
#' @param data 
#' @import ggplot2 ggExtra ggblend
#' @return
#' @export
#'
#' @examples
#' 
fl_lengthweight <- function(data){

  
  levels(data$Type) <- c("Pelagic", "Benthic")
  
  length_weight <- 
    ggExtra::ggMarginal(
    ggplot2::ggplot() +
    geom_point(data = data, aes(x= Lengthcm, y=weight_kg, colour = Type, fill = Type), alpha = 0.01, size = 0.5)+
    scale_colour_manual(values = c("Pelagic" = '#077DAA', 'Benthic' = 'orange'))+
    scale_fill_manual(values = c("Pelagic" = '#077DAA', 'Benthic' = 'orange')) +
    scale_x_log10() + scale_y_log10(breaks= c(0.1, 1, 10))+ 
    xlab("Fork length (cm)")+ylab("Body size (kg)")+
    theme_light() +theme(legend.position = "bottom", axis.title=element_text(size=22),legend.title = element_blank(),
                       legend.text = element_text(size =22), 
                       axis.text.x = element_text(size=16),
                       axis.text.y = element_text(size=16))+
    guides(fill = guide_legend(override.aes = list(size = 2, alpha= 1 )))
              , groupFill= TRUE, groupColour = TRUE, alpha = .6, size = 4)
  
  print(length_weight)
  
  ggsave(length_weight, filename = here::here("outputs", "Extended_data_length_weight.jpeg"), width = 10, height = 10, units = "in", dpi =300)

  invisible(length_weight)
}

#' species rank order weights vertical
#'
#' @param data weights 
#' @param lower.line lower dashed line y- intercept
#' @param mid.line middle line y- intercept
#' @param upper.line upper dashed line y-intercept
#' 
#' @import ggplot2 ggExtra fishualize
#' @return
#' @export
#'
#' @examples
#' 
 fl_species_ord_marg <- function(data, lower.line, mid.line, upper.line, minsize){
  
   data = data[data$weight_kg > minsize, ]# select predatory fish - individuals larger than minsize kg
   
   
  fl_speciesrank <-  ggplot2::ggplot()+
    #geom_jitter(data= data, aes(x=reorder(Binomial, weight_kg, na.rm = TRUE), y= weight_kg,  colour = Type, alpha= Type), size = 0.5, width = 1.5)|> blend("lighten")+
    geom_jitter(data= data, aes(x=reorder(Binomial, weight_kg, na.rm = TRUE), y=weight_kg,  colour = Type, alpha= Type), size = 0.5)+
          #scale_y_log10(name  = "Bigger individuals (kg, n = 880,242)", breaks= c(0.001, 1, 100), labels= c("0.001", "1", "100"))+
          scale_y_log10(name  = "Body size (kg)", limits =c(0.001,1000),  breaks= c(0.001, 0.01, 0.1, 1, 10, 100),labels= c("0.001","0.01", "0.1","1", "10", "100"))+
    
    labs(x="Species identified on BRUVS (n = 1,460)")+
    theme(legend.position = "none", axis.title.y = element_text(size=20, angle = 90),
            legend.text = element_text(size =16),axis.text.x = element_text(size=16),
            axis.text.y = element_text(size=16), axis.title.x = element_text(size=20),
            axis.ticks.x = element_blank(), 
          axis.line = element_line(arrow = arrow(type='closed', length = unit(10,'pt')))) +
    scale_colour_manual(values = c("Midwater" = '#077DAA', 'Seabed' = 'orange'))+ 
    scale_x_discrete(label = NULL)+
    scale_alpha_discrete(range = c(0.80, 0.05))
    
    #benthic mean of medians
    #(0.050+0.073+0.090+0.089+0.120+0.027)/6 = 0.74
    #benthic mean of 0.95 quantiles
    #(0.355+2.052+2.112+2.189+2.935+1.414)/6 = 1.84
    
    #benthic quantiles
    #geom_hline(yintercept=0.74, na.rm =TRUE, linetype="solid", colour = "darkorange")+
    #geom_hline(yintercept=1.84, na.rm =TRUE, linetype="dashed",colour = "darkorange")+ 

    #pelagic mean of medians
    #(0.014+0.134+0.004+0.006+0.009+0.015)/6 =0.030
    #pelagic mean of 0.95 quantiles
    #(0.355+83.342+10.057+32.821+15.303+12.97)/6 = 25.808
    
    #pelagic quantiles
    #geom_hline(yintercept=0.030, na.rm =TRUE, linetype="solid", colour = "#077DAA")+ 
    #geom_hline(yintercept=25, na.rm =TRUE, linetype="dashed", colour = "#077DAA")


  fl_species <- ggExtra::ggMarginal(fl_speciesrank,groupFill= TRUE, groupColour = TRUE, type = "violin", alpha = .6, size = 2, margins = "y")


  
  print(fl_species)
  ggsave(fl_species, filename = here::here("outputs", "species_ord_marg.png"), width = 20, height = 12, units = "in", dpi =300)
  
  #ggsave(fl_species, filename = here::here("outputs", "species_ord_marg.png"), width = 20, height = 12, units = "in", dpi =300)
  
  invisible(fl_species)

}

#' species rank order weights horizontal
#'
#' @param data 
#' @import ggplot2 
#' @return
#' @export
#'
#' @examples
#' 
fl_species_rank_order_vert <- function(data, lower.line, upper.line){
  options(scipen=4)
  fl_speciesrank <- data %>% 
    ggplot2::ggplot(aes(x=reorder(Binomial,weight_kg, na.rm = TRUE), y=weight_kg)) +
    geom_jitter(aes(colour = Type, alpha = Type), size = 0.6, width = 1.5) + 
    scale_y_log10(breaks= c(0.001, 1, 100))+ #coord_flip()+
    labs(y="Weight (kg)", x="Species")+theme_classic()+
    theme(legend.position = "bottom", axis.title=element_text(size=26),legend.title = element_blank(),
          legend.text = element_text(size =26),axis.text.x = element_text(size=22),axis.text.y = element_text(size=22))+
    guides(fill = guide_legend(override.aes = list(size = 20, alpha= 1)))+
    scale_colour_manual(values = c("Midwater" = '#077DAA', 'Seabed' = 'orange'))+
    scale_x_discrete(guide = guide_axis(n.dodge=3), label = NULL)+
    scale_alpha_discrete(range = c(0.9, 0.15))+ 
    geom_hline(yintercept=quantile(data$weight_kg, upper.line, na.rm =TRUE), linetype="dashed")+ 
    geom_hline(yintercept=quantile(data$weight_kg, lower.line, na.rm =TRUE), linetype="dashed")
  
  print(fl_speciesrank)
  
  ggsave(fl_speciesrank, filename = here::here("outputs", "species_rank_vert.png"), width = 25, height = 15, units = "in", dpi =300)
  
  invisible(fl_speciesrank)
  
}


#' species rank order according to weight quantiles (upper and lower)
#'
#' @param data 
#' @param lower.quan 
#' @param upper.quan 
#'
#' @import ggplot2 dplyr
#' @return
#' @export
#'

fl_species_rank_order_quan <- function(data, lower.quan, upper.quan, breaks){
  options(scipen=4)
  fl_speciesrank <- data %>% 
    dplyr::filter(quantile(weight_kg, lower.quan, na.rm =TRUE)< weight_kg) %>% 
    dplyr::filter(quantile(weight_kg, upper.quan, na.rm =TRUE)> weight_kg) %>% 
    ggplot2::ggplot(aes(x=reorder(Binomial,weight_kg, na.rm = TRUE), y=weight_kg)) +
    geom_jitter(aes(colour = Type, alpha = Type), size = 3 , width = 0) + 
    scale_y_log10(breaks= breaks)+coord_flip()+
    labs(y="Weight (kg)", x="Species")+theme_bw()+
    theme(legend.position = "bottom", axis.title=element_blank(),legend.title = element_blank(),
          legend.text = element_text(size =16),axis.text.x = element_text(size=16),axis.text.y = element_text(size=14, face = "italic"))+
    scale_colour_manual(values = c("Midwater" = '#077DAA', 'Seabed' = 'orange'))+
    scale_x_discrete(guide = guide_axis(n.dodge=2))+
    scale_alpha_discrete(range = c(0.5, 0.2))+
    guides(fill = guide_legend(override.aes = list(size = 2, alpha= 1 )))
    
  
  print(fl_speciesrank) 
  
  ggsave(fl_speciesrank, filename = here::here("outputs", paste0("species_rank_",lower.quan,"_",upper.quan,".png")), width = 15, height = 16, units = "in", dpi =300)
  
  invisible(fl_speciesrank)
  
}



#' stacked histogram with weight bins 
#'
#' @param data 
#'
#' @import ggplot2 
#' @return
#' @export
#'
#' 

hist_spectra <- function(data){
  options(scipen=7)
  hist_spec <- 
    ggplot2::ggplot(data=data) + 
    ggplot2::geom_histogram(data=data, aes(x=weight_kg), alpha = 0.6, binwidth = 75, fill = '#077DAA', colour = '#077DAA') + 
    ggplot2::geom_histogram(data=subset(data, Type =="Seabed"),aes(x=weight_kg,fill=Type, colour = Type), alpha = 0.6, binwidth = 75)+
    scale_y_log10()+theme_light()+
    theme(legend.position = "none", axis.title.y = element_text(size=20),
          legend.text = element_text(size =16),axis.text.x = element_text(size=16),
          axis.text.y = element_text(size=16), axis.title.x = element_text(size=20))+
    scale_fill_manual(values = c("Midwater" = '#077DAA', 'Seabed' = 'orange'))+
    scale_colour_manual(values = c("Midwater" = '#077DAA', 'Seabed' = 'darkorange'))+xlab("Body size (kg)")

    print(hist_spec)

    ggsave(hist_spec, filename = here::here("outputs", "hist_spec.png"), width = 10, height =8, units = "in", dpi =300)
    
    invisible(hist_spec)
    
}


#' biomass pyramid
#'
#' @param data 
#'
#' @import ggplot2 
#' @return
#' @export
#'
#' 

biomass_pyramid <- function(data){
  options(scipen=7)
  biomass_pyr <-  
    ggplot2::ggplot(data) + 
    ggplot2::geom_histogram(data=subset(data, Type =="Midwater"), aes(x=weight_kg, weight = weight_kg*(-1), fill=Type, colour =Type),binwidth  = 75, boundary =0, alpha = 0.6)+
    ggplot2::geom_histogram(data=subset(data, Type =="Seabed"), aes(x=weight_kg, weight = weight_kg, fill=Type, colour = Type),binwidth  = 75, boundary =0,alpha = 0.6)+
    scale_fill_manual(values = c("Midwater" = '#077DAA', 'Seabed' = 'orange'))+
    scale_colour_manual(values = c("Midwater" = '#077DAA', 'Seabed' = 'darkorange'))+
    theme_light()+ theme(legend.position = "none", axis.title.y = element_text(size=20),
          legend.text = element_text(size =16),axis.text.x = element_text(size=16),
          axis.text.y = element_text(size=16), axis.title.x = element_text(size=20))+
    scale_x_continuous(breaks=seq(0,1250,200),labels=seq(0,1200, 200), expand = c(0,0), limits = c(-50, 1250)) +coord_flip()+
    scale_y_continuous(breaks=seq(-100000, 350000, 100000), labels =c(100000,0,100000,200000,300000))+
    ylab("Biomass (kg)")+xlab("Body size (kg)")
  
  print(biomass_pyr)
  
  ggsave(biomass_pyr, filename = here::here("outputs", "biomass_pyramid.png"), width = 10, height =8, units = "in", dpi =300)
  
  invisible(biomass_pyr)
  
}


#' summarise envar 
#'
#' @param x
#'
#' @return
#' @export
#'

summarise_fun <- function(x) {
  if (is.numeric(x)) {
    return(mean(x, na.rm = TRUE))
  } else {
    return(x[1])
  }
}



#' Cumulative distribution plotting - MLE
#'
#' @param dat size data
#' @param minsize minimum lat side
#' @param lat_band number of lat bands
#' @import ggplot2
#' @return 
#' @export
#'


cum_dist_plot <- function(dat, minsize, lat_band){

#options(scipen=5)
  
 dat2 = dat[dat$weight_kg > minsize, ]# select predatory fish - individuals larger than minsize kg
  

qplot <- ggplot(dat2, aes(weight_kg, colour = Type))+
  stat_ecdf(geom="smooth", aes(y = 1 - ..y..), pad = FALSE) +
  scale_y_log10(breaks = c(0.0001, 0.01, 1), labels =c('0.01%','1%','100%'))+theme_light() +
  scale_colour_manual(values = c("Midwater" = '#077DAA', 'Seabed' = 'darkorange'))+
  scale_x_log10(limits = c(0.0001, 1200), breaks = c(0.0001, 0.1, 100), labels = c(0.0001, 0.1, 100))+
  theme(strip.background = element_blank(),legend.position = "none", legend.title = element_blank(), axis.title=element_text(size=22), 
        axis.title.x=element_text(size=22),
        axis.title.y=element_text(size=22),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16),strip.text.y = element_text(size = 16))+
  xlab('Body size (x, kg)')+ylab('Proportion of values ≥ x')+
  #facet_wrap(~cut_number(abs(lat_in), n=lat_band), nrow =6)
  facet_grid(cut(abs(lat_in), breaks = c(0, 10, 15, 20, 23, 33, 65)) ~ .) 
                              

print(qplot)

ggsave(qplot, filename = here::here("outputs", "points_lcd.jpeg"), width = 10, height = 16, units = "in", dpi =300)#render cowplots in jpeg less you get seethrough bits

invisible(qplot)

}

#' extended data multiplot 
#'
#' @param lat weights lat plot
#' @param cdp cumulative density plot

#' @import ggplot2 
#' @import cowplot
#' @return 
#' @export
#'


ex_data_lat_cdp <- function(lat, cdp){
  
  
  multi <- ggdraw()+
    draw_plot(lat, 0, 0, .5, 1)+
    draw_plot(cdp, 0.5, 0, .5, 0.98)+
    draw_plot_label(c("A", "B"), c(0, .5), c(1,1), size =22)
  print(multi)
  
  ggsave(multi, filename = here::here("outputs", "fig_3_lat_lcd.jpeg"), width = 12, height = 12, units = "in", dpi =300)#render cowplots in jpeg less you get seethrough bits
  
  invisible(multi)
  
}

#' extended data multiplot 
#'
#' @param lat weights lat plot
#' @param cdp cumulative density plot

#' @import ggplot2 
#' @import cowplot
#' @return 
#' @export
#'


ex_data_lat_cdp_lon <- function(lat, cdp){
  
  
  multi <- ggdraw()+
    draw_plot(lat, 0, 0, .5, 1)+
    draw_plot(cdp, 0.5, 0, .5, 0.98)+
    draw_plot_label(c("A", "B"), c(0, .5), c(1,1), size =22)
  print(multi)
  
  ggsave(multi, filename = here::here("outputs", "Extended_Data_Fig_lat_lcd_lon.jpeg"), width = 12, height = 12, units = "in", dpi =300)#render cowplots in jpeg less you get seethrough bits
  
  invisible(multi)
  
}


#' Title
#'
#' @param dat 
#' @param minsize 
#'
#' @return plot of bing_global and lm coefs
#' @export
#'
#' @examples
#' 
#' 
bin_global_points_lm <- function(dat2, minsize, lat_band){

 
  #dat = fl_pelagic_benthic_meta
  #minsize = 0.001
  #lat_band = 6
  #options(scipen=5)
  
  dat2 = dat2[dat2$weight_kg >  minsize, ]# individuals larger than 
  dat2['log_weight'] <- log10(dat2$weight_kg)
  
  # generate counts in bin on the log scale
  points_max <- ggplot2::ggplot()+
   ggplot2::stat_bin(data = dat2, aes(x=log_weight, group = cut_number(abs(lat_in), n=lat_band)),binwidth = 1, alpha=.5, position = "identity",fill = NA, colour = 'darkgreen', geom = "point")+
   ggplot2::stat_bin(data = subset(dat2, Type =="Seabed") , aes(x=log_weight, group = cut_number(abs(lat_in),  n=lat_band)), binwidth = 1, fill = NA, colour = 'darkorange', alpha = 0.6, position = "identity", geom = "point")+
   ggplot2::stat_bin(data = subset(dat2, Type =="Midwater"), aes(x=log_weight, group = cut_number(abs(lat_in),  n=lat_band)), binwidth = 1, fill = NA, colour = '#077DAA', alpha = 0.6, position = "identity", geom = "point")+
  theme_light()


  # extract counts
  dat_pelagic_max  <- subset(layer_data(points_max, i = 3))
  dat_benthic_max  <- subset(layer_data(points_max, i = 2))
  
  # visualise size-spectra
  # heren we normalise by dividing counts by the bin width i.e x weight in kg, then logging
  #This makes the liner regression coefficient comparable to the exponent b estimate.
  #see Heather et al (2020, Ecology Letters), and Platt and Denmar (1977)
    
  dat_pelagic_max['norm_y'] <- log10(dat_pelagic_max$count/10^dat_pelagic_max$x)
  dat_benthic_max['norm_y'] <- log10(dat_benthic_max$count/10^dat_benthic_max$x)

    
   # repeat plots using log units
  
  points_max_min <-ggplot2::ggplot()+
    stat_smooth(data = dat_pelagic_max, aes(x, norm_y), colour ='#077DAA', method  = "lm", alpha = 0.1, size = 0.3,se=F)+
    #stat_smooth(data = dat_pelagic_max, aes(x, norm_y), colour ='#077DAA', fill='#077DAA', method  = "lm", alpha = 0.2,size = .3)+
    stat_smooth(data = dat_benthic_max, aes(x, norm_y), colour ='darkorange', method  = "lm", alpha = 0.1, size = 0.3,se=F)+
    #stat_smooth(data = dat_benthic_max, aes(x, norm_y), colour ='darkorange',fill ='darkorange', method  = "lm", alpha = 0.2, size = .3)+
    geom_point(data = dat_pelagic_max, aes(x, norm_y), colour = '#077DAA',alpha = 0.6)+
    geom_point(data = dat_benthic_max, aes(x, norm_y), colour = 'darkorange',alpha = 0.6)+
    theme_light() + xlab("Body size (kg)")+ ylab(bquote(Log[10](Count)))+
    scale_x_continuous(breaks= c( -3, -2, -1, 0, 1, 2), labels = c(.001, .01, .1, 1, 10, 100), limits = c(-3.5, 3.5))+
    theme(legend.position = "none", axis.title.x=element_text(size=22),legend.title = element_blank(),
          legend.text = element_text(size =22),axis.text.x = element_text(size=16),
          axis.text.y = element_text(size=16), axis.title.y=element_text(size=22), strip.background = element_blank())+facet_grid(-group ~ .)

 
  #linear regression model
  lm_pelagic_max_sub1 <- lm(norm_y ~ x, data = subset(dat_pelagic_max, group == "1"))
  lm_benthic_max_sub1 <- lm(norm_y ~ x, data = subset(dat_benthic_max, group == "1"))
  lm_pelagic_max_sub2 <- lm(norm_y ~ x, data = subset(dat_pelagic_max, group == "2"))
  lm_benthic_max_sub2 <- lm(norm_y ~ x, data = subset(dat_benthic_max, group == "2"))
  lm_pelagic_max_sub3 <- lm(norm_y ~ x, data = subset(dat_pelagic_max, group == "3"))
  lm_benthic_max_sub3 <- lm(norm_y ~ x, data = subset(dat_benthic_max, group == "3"))
  lm_pelagic_max_sub4 <- lm(norm_y ~ x, data = subset(dat_pelagic_max, group == "4"))
  lm_benthic_max_sub4 <- lm(norm_y ~ x, data = subset(dat_benthic_max, group == "4"))
  lm_pelagic_max_sub5 <- lm(norm_y ~ x, data = subset(dat_pelagic_max, group == "5"))
  lm_benthic_max_sub5 <- lm(norm_y ~ x, data = subset(dat_benthic_max, group == "5"))
  lm_pelagic_max_sub6 <- lm(norm_y ~ x, data = subset(dat_pelagic_max, group == "6"))
  lm_benthic_max_sub6 <- lm(norm_y ~ x, data = subset(dat_benthic_max, group == "6"))
  
#combined coefficients
  lm_pelagic_max_cf_sub1  <-  cbind(coef(lm_pelagic_max_sub1), confint(lm_pelagic_max_sub1))
  lm_pelagic_max_cf_sub2  <-  cbind(coef(lm_pelagic_max_sub2), confint(lm_pelagic_max_sub2))
  lm_pelagic_max_cf_sub3  <-  cbind(coef(lm_pelagic_max_sub3), confint(lm_pelagic_max_sub3))
  lm_pelagic_max_cf_sub4  <-  cbind(coef(lm_pelagic_max_sub4), confint(lm_pelagic_max_sub4))
  lm_pelagic_max_cf_sub5  <-  cbind(coef(lm_pelagic_max_sub5), confint(lm_pelagic_max_sub5))
  lm_pelagic_max_cf_sub6  <-  cbind(coef(lm_pelagic_max_sub6), confint(lm_pelagic_max_sub6))
  
  lm_benthic_max_cf_sub1  <-  cbind(coef(lm_benthic_max_sub1), confint(lm_benthic_max_sub1))
  lm_benthic_max_cf_sub2  <-  cbind(coef(lm_benthic_max_sub2), confint(lm_benthic_max_sub2))
  lm_benthic_max_cf_sub3  <-  cbind(coef(lm_benthic_max_sub3), confint(lm_benthic_max_sub3))
  lm_benthic_max_cf_sub4  <-  cbind(coef(lm_benthic_max_sub4), confint(lm_benthic_max_sub4))
  lm_benthic_max_cf_sub5  <-  cbind(coef(lm_benthic_max_sub5), confint(lm_benthic_max_sub5))
  lm_benthic_max_cf_sub6  <-  cbind(coef(lm_benthic_max_sub6), confint(lm_benthic_max_sub6))
  
  lm_pelagic_cf <- rbind(lm_pelagic_max_cf_sub1,lm_pelagic_max_cf_sub2,lm_pelagic_max_cf_sub3,lm_pelagic_max_cf_sub4,lm_pelagic_max_cf_sub5,lm_pelagic_max_cf_sub6)
  lm_benthic_cf <- rbind(lm_benthic_max_cf_sub1, lm_benthic_max_cf_sub2, lm_benthic_max_cf_sub3, lm_benthic_max_cf_sub4, lm_benthic_max_cf_sub5, lm_benthic_max_cf_sub6)
   
  #turn to dataframe
  lm_pelagic_cf <- as.data.frame(lm_pelagic_cf) 
  lm_benthic_cf <- as.data.frame(lm_benthic_cf) 

 #rename
 lm_pelagic_cf$type <-  c('pelagic', 'pelagic', 'pelagic', 'pelagic', 'pelagic', 'pelagic', 'pelagic', 'pelagic', 'pelagic', 'pelagic', 'pelagic', 'pelagic')
 lm_pelagic_cf$lat_bracket <- c('(33, 65]','(33, 65]', '(23, 33]','(23, 33]','(20, 23]', '(20, 23]','(15, 20]','(15, 20]', '(10,15]', '(10,15]','(0,10]','(0,10]')
 lm_pelagic_cf$parameter <- c('intercept', 'slope estimate')
 colnames(lm_pelagic_cf)[1] ="estimate"
 
 lm_benthic_cf$type <-  c('benthic', 'benthic','benthic','benthic','benthic','benthic','benthic','benthic','benthic','benthic','benthic','benthic') 
 lm_benthic_cf$lat_bracket <- c('(33, 65]','(33, 65]', '(23, 33]','(23, 33]','(20, 23]', '(20, 23]','(15, 20]','(15, 20]', '(10,15]', '(10,15]','(0,10]','(0,10]')
 lm_benthic_cf$parameter <- c('intercept', 'slope estimate')
 colnames(lm_benthic_cf)[1] ="estimate"
 both <- rbind(lm_pelagic_cf, lm_benthic_cf)

  write.csv(lm_pelagic_cf, file=here::here("outputs", "table", "bin_pelagic_lm_coefs.csv"))
  write.csv(lm_benthic_cf, file=here::here("outputs", "table", "bin_benthic_lm_coefs.csv"))
  write.csv(both, file=here::here("outputs", "table", "Extended_table_lat_slopes.csv"))
  
  
  print(points_max_min)
  
  ggsave(points_max_min, filename = here::here("outputs", "bin_global_lm.jpeg"), width = 16, height = 10, units = "in", dpi =300)#render cowplots in jpeg less you get seethrough bits
  invisible(points_max_min)
  

}


#' Title
#'
#' @param dat 
#' @param minsize 
#'
#' @return plot of bing_global and lm coefs
#' @export
#'
#' @examples
#' 
#' 
bin_global_points_lm_lon <- function(dat, minsize, lon_band){
  
  
  dat2 = dat[dat$weight_kg >  minsize, ]# individuals larger than 
  dat2['log_weight'] <- log10(dat2$weight_kg)
  
  # generate counts in bin on the log scale
  points_max <- ggplot2::ggplot()+
    ggplot2::stat_bin(data = dat2, aes(x=log_weight, group = cut_number(lon_in, n=lon_band)),binwidth = 1, alpha=.5, position = "identity",fill = NA, colour = 'darkgreen', geom = "point")+
    ggplot2::stat_bin(data = subset(dat2, Type =="Seabed") , aes(x=log_weight, group = cut_number(lon_in,  n=lon_band)), binwidth = 1, fill = NA, colour = 'darkorange', alpha = 0.6, position = "identity", geom = "point")+
    ggplot2::stat_bin(data = subset(dat2, Type =="Midwater"), aes(x=log_weight, group = cut_number(lon_in,  n=lon_band)), binwidth = 1, fill = NA, colour = '#077DAA', alpha = 0.6, position = "identity", geom = "point")+
    theme_light()
  
  
  # extract counts
  dat_pelagic_max  <- subset(layer_data(points_max, i = 3))
  dat_benthic_max  <- subset(layer_data(points_max, i = 2))
  
  # visualise size-spectra
  # heren we normalise by dividing counts by the bin width i.e x weight in kg, then logging
  #This makes the liner regression coefficient comparable to the exponent b estimate.
  #see Heather et al (2020, Ecology Letters), and Platt and Denmar (1977)
  
  dat_pelagic_max['norm_y'] <- log10(dat_pelagic_max$count/10^dat_pelagic_max$x)
  dat_benthic_max['norm_y'] <- log10(dat_benthic_max$count/10^dat_benthic_max$x)
  
  
  # repeat plots using log units
  
  points_max_min <-ggplot2::ggplot()+
    stat_smooth(data = dat_pelagic_max, aes(x, norm_y), colour ='#077DAA', method  = "lm", alpha = 0.1, size = 0.3,se=F)+
    #stat_smooth(data = dat_pelagic_max, aes(x, norm_y), colour ='#077DAA', fill='#077DAA', method  = "lm", alpha = 0.2,size = .3)+
    stat_smooth(data = dat_benthic_max, aes(x, norm_y), colour ='darkorange', method  = "lm", alpha = 0.1, size = 0.3,se=F)+
    #stat_smooth(data = dat_benthic_max, aes(x, norm_y), colour ='darkorange',fill ='darkorange', method  = "lm", alpha = 0.2, size = .3)+
    geom_point(data = dat_pelagic_max, aes(x, norm_y), colour = '#077DAA',alpha = 0.6)+
    geom_point(data = dat_benthic_max, aes(x, norm_y), colour = 'darkorange',alpha = 0.6)+
    theme_light() + xlab("Body size (kg)")+ ylab(bquote(Log[10](Count)))+
    scale_x_continuous(breaks= c( -3, -2, -1, 0, 1, 2), labels = c(.001, .01, .1, 1, 10, 100), limits = c(-3.5, 3.5))+
    theme(legend.position = "none", axis.title.x=element_text(size=22),legend.title = element_blank(),
          legend.text = element_text(size =22),axis.text.x = element_text(size=16),
          axis.text.y = element_text(size=16), axis.title.y=element_text(size=22), strip.background = element_blank())+facet_grid(-group ~ .)
  
  
  #linear regression model
  lm_pelagic_max_sub1 <- lm(norm_y ~ x, data = subset(dat_pelagic_max, group == "1"))
  lm_benthic_max_sub1 <- lm(norm_y ~ x, data = subset(dat_benthic_max, group == "1"))
  lm_pelagic_max_sub2 <- lm(norm_y ~ x, data = subset(dat_pelagic_max, group == "2"))
  lm_benthic_max_sub2 <- lm(norm_y ~ x, data = subset(dat_benthic_max, group == "2"))
  lm_pelagic_max_sub3 <- lm(norm_y ~ x, data = subset(dat_pelagic_max, group == "3"))
  lm_benthic_max_sub3 <- lm(norm_y ~ x, data = subset(dat_benthic_max, group == "3"))
  lm_pelagic_max_sub4 <- lm(norm_y ~ x, data = subset(dat_pelagic_max, group == "4"))
  lm_benthic_max_sub4 <- lm(norm_y ~ x, data = subset(dat_benthic_max, group == "4"))
  lm_pelagic_max_sub5 <- lm(norm_y ~ x, data = subset(dat_pelagic_max, group == "5"))
  lm_benthic_max_sub5 <- lm(norm_y ~ x, data = subset(dat_benthic_max, group == "5"))
  lm_pelagic_max_sub6 <- lm(norm_y ~ x, data = subset(dat_pelagic_max, group == "6"))
  lm_benthic_max_sub6 <- lm(norm_y ~ x, data = subset(dat_benthic_max, group == "6"))
  
  #combined coefficients
  lm_pelagic_max_cf_sub1  <-  cbind(coef(lm_pelagic_max_sub1), confint(lm_pelagic_max_sub1))
  lm_pelagic_max_cf_sub2  <-  cbind(coef(lm_pelagic_max_sub2), confint(lm_pelagic_max_sub2))
  lm_pelagic_max_cf_sub3  <-  cbind(coef(lm_pelagic_max_sub3), confint(lm_pelagic_max_sub3))
  lm_pelagic_max_cf_sub4  <-  cbind(coef(lm_pelagic_max_sub4), confint(lm_pelagic_max_sub4))
  lm_pelagic_max_cf_sub5  <-  cbind(coef(lm_pelagic_max_sub5), confint(lm_pelagic_max_sub5))
  lm_pelagic_max_cf_sub6  <-  cbind(coef(lm_pelagic_max_sub6), confint(lm_pelagic_max_sub6))
  
  lm_benthic_max_cf_sub1  <-  cbind(coef(lm_benthic_max_sub1), confint(lm_benthic_max_sub1))
  lm_benthic_max_cf_sub2  <-  cbind(coef(lm_benthic_max_sub2), confint(lm_benthic_max_sub2))
  lm_benthic_max_cf_sub3  <-  cbind(coef(lm_benthic_max_sub3), confint(lm_benthic_max_sub3))
  lm_benthic_max_cf_sub4  <-  cbind(coef(lm_benthic_max_sub4), confint(lm_benthic_max_sub4))
  lm_benthic_max_cf_sub5  <-  cbind(coef(lm_benthic_max_sub5), confint(lm_benthic_max_sub5))
  lm_benthic_max_cf_sub6  <-  cbind(coef(lm_benthic_max_sub6), confint(lm_benthic_max_sub6))
  
  lm_pelagic_cf <- rbind(lm_pelagic_max_cf_sub1,lm_pelagic_max_cf_sub2,lm_pelagic_max_cf_sub3,lm_pelagic_max_cf_sub4,lm_pelagic_max_cf_sub5,lm_pelagic_max_cf_sub6)
  lm_benthic_cf <- rbind(lm_benthic_max_cf_sub1, lm_benthic_max_cf_sub2, lm_benthic_max_cf_sub3, lm_benthic_max_cf_sub4, lm_benthic_max_cf_sub5, lm_benthic_max_cf_sub6)
  
  #turn to dataframe
  lm_pelagic_cf <- as.data.frame(lm_pelagic_cf) 
  lm_benthic_cf <- as.data.frame(lm_benthic_cf) 
  
  #rename
  lm_pelagic_cf$type <-  c('pelagic', 'pelagic', 'pelagic', 'pelagic', 'pelagic', 'pelagic', 'pelagic', 'pelagic', 'pelagic', 'pelagic', 'pelagic', 'pelagic')
  lm_pelagic_cf$lon_bracket <- c('(123, 167]','(123, 167]','(117, 124]','(117, 124]', '(115, 117]', '(115, 117]','(114, 115]','(114, 115]','(72, 114]','(72, 114]', '(-179, 72]','(-179, 72]')
  lm_pelagic_cf$parameter <- c('intercept', 'slope estimate')
  colnames(lm_pelagic_cf)[1] ="estimate"
  
  lm_benthic_cf$type <-  c('benthic', 'benthic','benthic','benthic','benthic','benthic','benthic','benthic','benthic','benthic','benthic','benthic') 
  lm_benthic_cf$lon_bracket <- c('(123, 167]','(123, 167]','(117, 124]','(117, 124]', '(115, 117]', '(115, 117]','(114, 115]','(114, 115]','(72, 114]','(72, 114]', '(-179, 72]','(-179, 72]')
  lm_benthic_cf$parameter <- c('intercept', 'slope estimate')
  colnames(lm_benthic_cf)[1] ="estimate"
  both <- rbind(lm_pelagic_cf, lm_benthic_cf)
  

  write.csv(lm_pelagic_cf, file=here::here("outputs", "table", "bin_pelagic_lm_coefs_lon.csv"))
  write.csv(lm_benthic_cf, file=here::here("outputs", "table", "bin_benthic_lm_coefs_lon.csv"))
  
  write.csv(both, file=here::here("outputs", "table", "Extended_table_lon_slopes.csv"))
  
  
  print(points_max_min)
  
  ggsave(points_max_min, filename = here::here("outputs", "bin_global_lm_lon.jpeg"), width = 16, height = 10, units = "in", dpi =300)#render cowplots in jpeg less you get seethrough bits
  
  invisible(points_max_min)
  
  
}



#' Title
#'
#' @param dat 
#' @param minsize 
#'
#' @return plot of bing_global and lm coefs
#' @export
#'
#' @examples
#' 
#' 
bin_global_points_quad <- function(dat, minsize, lat_band){
  
  dat2 = dat[dat$weight_kg >  minsize, ]# select predatory fish - individuals larger than 35 cm
  #fl_pelagic_benthic_meta_min = dat[dat$weight_kg <  0.8, ]# select forage fish
  #fl_pelagic_benthic_meta_min = fl_pelagic_benthic_meta_min[fl_pelagic_benthic_meta_min$weight_kg > 0.002, ]
  
  points_max <- ggplot2::ggplot()+
    stat_bin(data = dat2, aes(x=weight_kg, group = cut_number(abs(lat_in), n=lat_band)), alpha=.5, position = "identity",fill = NA, colour = 'darkgreen', geom = "point")+
    stat_bin(data = subset(dat2, Type =="Seabed") , aes(x=weight_kg, group = cut_number(abs(lat_in), n=lat_band)), fill = NA, colour = 'darkorange', alpha = 0.6, position = "identity", boundary=0.33, geom = "point")+
    stat_bin(data = subset(dat2, Type =="Midwater"), aes(x=weight_kg, group = cut_number(abs(lat_in), n=lat_band)), fill = NA, colour = '#077DAA', alpha = 0.6, position = "identity", boundary=0.33, geom = "point")+
    scale_y_log10(oob = scales::squish_infinite)+scale_x_log10()+theme_light()
  
  # points_min <- ggplot2::ggplot()+
  # stat_bin(data = fl_pelagic_benthic_meta_min, aes(x=weight_kg, group = cut_number(abs(lat_in), n=6)), alpha=.5, position = "identity",fill = NA, colour = 'darkgreen', geom = "point")+
  # stat_bin(data = subset(fl_pelagic_benthic_meta_min, Type =="Midwater"),aes(x=weight_kg, group = cut_number(abs(lat_in), n=6)), fill = NA, colour = '#077DAA', alpha = 0.6, position = "identity", boundary=0.33, geom = "point")+
  # stat_bin(data = subset(fl_pelagic_benthic_meta_min, Type =="Seabed"), aes(x=weight_kg, group = cut_number(abs(lat_in), n=6)), fill = NA, colour = 'darkorange', alpha = 0.6, position = "identity", boundary=0.33, geom = "point")+
  # scale_y_log10(oob = scales::squish_infinite)+scale_x_log10()+theme_light()
  
  dat_combined_max <- subset(layer_data(points_max, i = 1), count>0.1)#ignore zeros cause of log scale
  dat_pelagic_max  <- subset(layer_data(points_max, i = 3), count>0.1)#ignore zeros cause of log scale
  dat_benthic_max  <- subset(layer_data(points_max, i = 2), count>0.1)#ignore zeros cause of log scale

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

print(points_max_quad) 

ggsave(points_max_quad, filename = here::here("outputs", "bin_global_quad.jpeg"), width = 10, height = 16, units = "in", dpi =300)#render cowplots in jpeg less you get seethrough bits

invisible(points_max_quad)

#quadratic regressions

#quad_combined_max <- lm(log(count/0.81) ~ x + x^2*as.factor(group), data = dat_combined_max)
#quad_pelagic_max <- lm(log(count/0.62) ~ x + x^2, data = dat_pelagic_max)
#quad_benthic_max <- lm(log(count) ~ x + x^2, data = dat_benthic_max)

#cm <- rbind(coef(quad_pelagic_max),coef(quad_benthic_max)) # Coefficient matrix
#(-solve(cbind(cm[,2],-1)) %*% cm[,1])


}

#' hist plot betaslope
#'
#' @param dat 
#' @import ggplot2
#' @return figure of a response variable
#' @export
#'
#' 


## density plot of MLE slope                               
hist_betaslope <- function(dat){
  
  #reorder levels
  #dat$bruvs_type2 <- relevel(as.factor(dat$bruvs_type), 'pelagic')

  #plot fig
  fig <- ggplot() +
    #geom_density(data = dat, aes(x= betaslope, fill = bruvs_type), alpha = .6)+
    geom_histogram(data = dat, aes(x= betaslope, fill = bruvs_type), alpha = .4)+
    #scale_colour_manual(values = c("pelagic" = '#077DAA', 'benthic' = 'darkorange'))+
  scale_fill_manual(values = c("pelagic" = '#077DAA', 'benthic' = 'orange')) + xlim(c(-2.5, 0))+xlab("Beta slope")+
  theme_light() + theme(legend.position = "top", axis.title=element_text(size=20),legend.title = element_blank(),
                       legend.text = element_text(size =20), 
                       axis.text.x = element_text(size=16),
                       axis.text.y = element_text(size=16))
print(fig)

ggsave(fig, filename = here::here("outputs", "hist_betaslope.png"), width = 6, height = 6, units = "in", dpi =300)

invisible(fig)

}


#' Density plot modes
#'
#' @param dat 
#' @import ggplot2
#' @return figure of a response variable
#' @export
#'
#' 


## density plot of MLE slope                               
density_modes <- function(dat){
  
  #reorder levels
  #dat$bruvs_type2 <- relevel(as.factor(dat$bruvs_type), 'pelagic')
  options(scipen=4)
  
  #ling format
  df = bind_rows(
    #first_mode
    data.frame(mode = dat$first_mode, mode_type ="first", bruvs_type = dat$bruvs_type, stringsAsFactors = T),
    #second_mode
    data.frame(mode = dat$second_mode, mode_type = "second", bruvs_type =dat$bruvs_type, stringsAsFactors = T)
  )
  
  df$bruvs_mode <- paste(df$bruvs_type, df$mode_type, sep = "_")
  
  
  #plot fig
  fig <- ggplot() + geom_density(data=df, aes(x=mode, colour = bruvs_mode, fill =bruvs_mode, alpha = bruvs_mode))+
    scale_colour_manual(name = "bruvs and mode", values = c("pelagic_first" = "#077DAA", "pelagic_second" ="#077DAA","benthic_first"="orange","benthic_second"="orange"))+
    scale_fill_manual(name = "bruvs and mode",  values = c("pelagic_first" = "#077DAA","pelagic_second" = "#077DAA","benthic_first"="orange","benthic_second"="orange"))+
    scale_alpha_manual(name = "bruvs and mode",  values = c("pelagic_first"=0.5,"pelagic_second"=0.1,"benthic_first"= 0.5,"benthic_second"=0.1))+
    theme_light() + scale_x_log10()+xlab ("Modal value ")
  #labels =c("pelagic (first)",  "pelagic (second)", "benthic (first)","benthic (second)"), 
  print(fig)
  
  ggsave(fig, filename = here::here("outputs", "density_modes.png"), width = 6, height = 6, units = "in", dpi =300)
  
  invisible(fig)
  
}

#' violin plot modes
#'
#' @param dat 
#' @import ggplot2
#' @return figure of a response variable
#' @export
#'
#' 


## violin plot of MLE slope                               
violin_modes <- function(dat){
  
  #reorder levels
  #dat$bruvs_type2 <- relevel(as.factor(dat$bruvs_type), 'pelagic')
  options(scipen=4)
  
  #ling format
  df = bind_rows(
    #first_mode
    data.frame(mode = dat$first_mode, mode_type ="first", bruvs_type = dat$bruvs_type, stringsAsFactors = T),
    #second_mode
    data.frame(mode = dat$second_mode, mode_type = "second", bruvs_type =dat$bruvs_type, stringsAsFactors = T)
  )
  
  df$bruvs_mode <- paste(df$bruvs_type, df$mode_type, sep = "_")
  
  #plot fig
  fig <- ggplot(data=df, aes(x=bruvs_mode, y = mode, colour = bruvs_mode, fill =bruvs_mode, alpha = bruvs_mode)) + #geom_violin()+
    scale_colour_manual(name = "bruvs and mode", values = c("pelagic_first" = "#077DAA", "pelagic_second" ="#077DAA","benthic_first"="orange","benthic_second"="orange"))+
    scale_fill_manual(name = "bruvs and mode",  values = c("pelagic_first" = "#077DAA","pelagic_second" = "#077DAA","benthic_first"="orange","benthic_second"="orange"))+
    scale_alpha_manual(name = "bruvs and mode",  values = c("pelagic_first"=0.5,"pelagic_second"=0.1,"benthic_first"= 0.5,"benthic_second"=0.1))+
    theme_void() + geom_boxplot(width=0.3)+
    coord_flip() +ylab("Modal value ")+ xlab("")+
    theme(panel.border = element_blank(),legend.position = "none", axis.title=element_blank(),legend.title = element_blank(),
                         legend.text = element_blank(), 
                         axis.text.y = element_blank(),
                         axis.text.x = element_blank())+
    scale_y_log10(limits = c(0.001, 1200), breaks = c(0.01, 1, 100), labels = c(0.01, 1, 100))
  print(fig)
  
  ggsave(fig, filename = here::here("outputs", "violin_modes.png"), width = 12, height = 6, units = "in", dpi =300)
  
  invisible(fig)
  
}
#' stacked histogram with weight bins 
#'
#' @param data 
#'
#' @import ggplot2 
#' @return
#' @export
#'
#' 

hist_spectra_nonstack <- function(data){
  options(scipen=7)
  
  hist_spec <- 
    ggplot2::ggplot(data=data) + 
    ggplot2::geom_histogram(data=subset(data, Type == "Midwater"), aes(x=weight_kg), alpha = 0.4, binwidth = 75, fill = '#077DAA', colour = '#077DAA') + 
    ggplot2::geom_histogram(data=subset(data, Type =="Seabed"), aes(x=weight_kg), alpha = 0.4, binwidth = 75, fill = "darkorange", colour = "orange")+
    scale_y_log10()+theme_light()+
    theme(legend.position = "none", axis.title.y = element_text(size=20),
          legend.text = element_text(size =16),axis.text.x = element_text(size=16),
          axis.text.y = element_text(size=16), axis.title.x = element_text(size=20))+
    scale_fill_manual(values = c("Midwater" = '#077DAA', 'Seabed' = 'orange'))+
    scale_colour_manual(values = c("Midwater" = '#077DAA', 'Seabed' = 'darkorange'))+xlab("Body size (kg)")
  
  print(hist_spec)
  
  ggsave(hist_spec, filename = here::here("outputs", "hist_spec_nonstacked.png"), width = 10, height =8, units = "in", dpi =300)
  invisible(hist_spec)
  
}


#' response variable multiplot 
#'
#' @param dat 
#'
#' @import ggplot2 
#' @import cowplot
#' @import dplyr
#' @import grid
#' @import purrr
#' @import fishualize
#' 
#' @return
#' @export
#'
#' 



response_fig <- function(dat2, dat, tab_first, tab_second, min_size, lat_band, bandw, scale, alpha){
  # dat2 =fl_pelagic_benthic_meta
  # dat = tab_betaslope 
  # tab_first = tab_firstmode
  # tab_second = tab_secondmode
  # min_size=0.001
  # lat_band =15
  # bandw = 0.2
  # scale= 30
  # alpha=0.3
  
  dat2 <- dat2[which(dat2$weight_kg > min_size),]
  
  
  options(scipen=4)
  levels(dat2$Type) <- c("Pelagic", "Benthic")
  
  fig_ridges_exped <- ggplot(dat2, aes(x=weight_kg, height = ..ndensity..))+
    ggridges::geom_density_ridges(bandwidth = bandw, rel_min_height = 0.005, aes(y = cut_number(abs(lat_in), n=lat_band), fill = Type), alpha = alpha, colour = "#FFFFFF", 
                                   scale = scale)+
    scale_x_log10(limits = c(0.0001, 1400), breaks = c(0.01, 1, 100), labels = c(0.01, 1, 100))+
    xlab('Body size (kg)') +ylab('Probability density')+
    scale_fill_manual(values = c("Pelagic" = '#077DAA', 'Benthic' = 'orange'))+
    scale_colour_manual(values = c("Pelagic" = '#077DAA', 'Benthic' = 'darkorange'))+
    scale_y_discrete(expand = c(0.1, 0))+
    theme_light() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),strip.text.x = element_blank(),panel.grid.major.y = element_blank(),
          legend.position = c(.88,.9), 
          axis.title=element_text(size=16),legend.title = element_blank(),
          legend.text = element_text(size =14), 
          axis.text.x = element_text(size=16),
          axis.text.y = element_blank(),
          legend.background = element_rect(fill = "transparent"))+
    coord_cartesian(clip = "off")+facet_wrap(~Type, nrow =2)#+ guides(colour = guide_legend(override.aes = list(alpha = .4)))
  
  #CFD distribution

  qplot <- ggplot(dat2, aes(weight_kg, group = cut_number(abs(lat_in), n=lat_band), colour = Type))+
    stat_ecdf(geom="smooth", aes(y = 1 - ..y..), pad= FALSE, size=.4, alpha=.3) +
    scale_y_log10(breaks = c(0.0001, 0.01, 1), labels =c('0.01%','1%','100%'))+ scale_x_log10(breaks = c(0.01, 1, 100), labels = c(0.01, 1, 100))+theme_light() +
    scale_colour_manual(values = c("Pelagic" = '#077DAA', 'Benthic' = 'orange'))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          strip.background = element_blank(),legend.position = "none", legend.title = element_blank(), axis.title.x=element_text(size=16),
          legend.text = element_text(size =16),axis.text.x = element_text(size=16),
          axis.text.y = element_text(size=16), axis.title.y=element_text(size=16),strip.text.y = element_text(size = 16))+
    xlab('Body size (x, kg)')+ylab('Proportion of values ≥ x')+facet_wrap(~Type, nrow =2)
  
  
  #modal histogramme
  #long format
  df = bind_rows(
    #first_mode
    data.frame(mode = tab_first$logFirstmode, mode_type ="Small fishes", bruvs_type = tab_first$bruvs_type, stringsAsFactors = T),
    #second_mode
    data.frame(mode = tab_second$logSecondmode, mode_type = "Large fishes", bruvs_type =tab_second$bruvs_type, stringsAsFactors = T)
  )

  df$bruvs_mode <- paste(df$bruvs_type, df$mode_type, sep = "_")
  df$bruvs_mode <- factor(df$bruvs_mode, levels = c("benthic_second", "pelagic_second","benthic_first","pelagic_first"))
  
  df <- tidyr::drop_na(df, mode)
  
# if you want to include mean values. Doesn't work for some reason
  df %>%
    dplyr::group_by(bruvs_type, mode_type) %>%
    dplyr::summarise(Mean.mode = mean(10^mode)) -> df2

  fig_modes_hist <- ggplot()+
    geom_histogram(data=df, aes(x=10^mode, colour = bruvs_type, fill =bruvs_type), alpha=.4) + 
    scale_colour_manual(name = "", values = c("pelagic" = "#077DAA", "benthic"="darkorange"))+
    scale_fill_manual(name = "",  values = c("pelagic" = "#077DAA","benthic"="orange"))+
    xlab("Body size (kg)") +theme_light()+ylim(0,220)+ylab("count")+
    theme(panel.border = element_rect(linetype = "dashed", size=1.5, fill = NA), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "none",  axis.text.x = element_text(size=16),
          strip.background =element_rect(fill="white"),
          strip.placement = "inside",
          strip.text = element_text(size=16, colour ="black"),
          axis.title.x = element_text(size=16),
          axis.title.y = element_text(size=16), axis.text.y = element_text(size=12))+
    geom_vline(data=df2, aes(xintercept = Mean.mode, colour = bruvs_type)) +
    scale_x_log10(limits = c(0.001, 1400), breaks = c(0.01, 1, 100), labels = c(0.01, 1, 100))+
        facet_wrap(~mode_type, nrow=2, strip.position = "bottom")


  dat <- tidyr::drop_na(dat, betaslope)
  
  dat %>%
    dplyr::group_by(bruvs_type) %>%
    dplyr::summarise(Mean.beta = mean(betaslope)) -> df3

  fig_betaslope <- ggplot() +
    geom_histogram(data = dat, aes(x= betaslope, fill = bruvs_type, colour = bruvs_type), alpha =.4)+
    scale_colour_manual(values = c("pelagic" = "#077DAA", "benthic"="darkorange"))+
    scale_fill_manual(values = c("pelagic" = '#077DAA', 'benthic' = 'orange')) + xlim(c(-2.5, 0))+xlab("Size-spectra slope value")+ylab("count")+
    theme_light() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none", 
                          axis.title=element_text(size=16),
                          legend.title = element_blank(),
                          legend.text = element_text(size =16), 
                          axis.text.x = element_text(size=16),
                          axis.text.y = element_text(size=16))+ 
    geom_vline(data=df3, aes(xintercept = Mean.beta, colour = bruvs_type))+
    add_fishape(family = "Pomacanthidae", option = "Centropyge_loricula",  xmin = .0, xmax = .05, ymin = 0.87, ymax = 0.92, scaled = TRUE, xlim = c(-2.5, 0), ylim =c(0, 230), alpha =.4)+
    add_fishape(family = "Carangidae", option = "Caranx_melampygus",  xmin = .1, xmax = .23, ymin = 0.8, ymax = 0.88, scaled = TRUE, xlim = c(-2.5, 0), ylim =c(0, 230), alpha =.4)+
    add_fishape(family = "Scombridae", option = "Thunnus_albacares",  xmin = .23, xmax = .43, ymin = 0.67, ymax = 0.87, scaled = TRUE, xlim = c(-2.5, 0), ylim =c(0, 230), alpha =.4)+
    geom_polygon(data=data.frame(x=c(-2.45, -1.9, -2.45), y = c(165, 165, 200)), aes(x=x, y=y), fill=NA, colour = "grey", alpha =.4)+
    geom_polygon(data=data.frame(x=c(-2.45, -2.1, -2.45), y = c(165, 165, 200)), aes(x=x, y=y), fill=NA, colour = "grey", alpha =.4, linetype ='dotted')
    #annotate("text", x=-2, y=155, label= "n = 1041", size = 6)

  
  fig_response <- ggdraw()+
    draw_plot(fig_ridges_exped,     0.01, .5,    .49,   .5)+
    draw_plot(qplot,             0.5, .5,   .5,   .52)+
    draw_plot(fig_modes_hist, 0, 0, 0.5, 0.5)+
    draw_plot(fig_betaslope,     0.5, 0,   .5,   .5)+
    draw_plot_label(c("A", "B", "C", "D"), c(0, 0,  0.5,.5 ), c(1, .52,  1, .52), size = 22, fontface = "bold")
  
  
  fig_response <- fig_response + 
    geom_curve(aes(x = 0.47, y = .9, xend = 0.6, yend = .9),arrow = arrow(length = unit(0.01, "npc"),type="closed"), curvature = -0.3)+
    geom_curve(aes(x = 0.47, y = .65, xend = 0.6, yend = .65),arrow = arrow(length = unit(0.01, "npc"),type="closed"), curvature = 0.3)+
    geom_curve(aes(x = 0.19, y = .6, xend = 0.19, yend = .42),arrow = arrow(length = unit(0.01, "npc"),type="closed"), curvature = 0.3)+
    geom_curve(aes(x = 0.42, y = .6, xend = 0.42, yend = .25),arrow = arrow(length = unit(0.01, "npc"),type="closed"), curvature = -0.3)+
    geom_curve(aes(x = 0.9, y = .9, xend = 0.9, yend = 0.3),arrow = arrow(length = unit(0.01, "npc"),type="closed"), curvature = -0.3)+
    geom_curve(aes(x = 0.9, y = .6, xend = 0.9, yend = .3),arrow = arrow(length = unit(0.01, "npc"), type="closed"), curvature = -0.3)
  
  rect_mode1 <- rectGrob(
    x = unit(1.4, "in"),
    y = unit(11.65, "in"),
    width = unit(1.5, "in"),
    height = unit(4.9, "in"),
    hjust = 0, vjust = 1,
    gp = gpar(col =  "black", alpha = 0.7, lty = "dashed", lwd = 1.5)
  ) 
  
  
  rect_mode3 <- rectGrob(
    x = unit(3.5, "in"),
    y = unit(11.65, "in"),
    width = unit(1.65, "in"),
    height = unit(4.9, "in"),
    hjust = 0, vjust = 1,
    gp = gpar(col =  "black", alpha = 0.7, lty = "dashed", lwd = 1.5)
  ) 
  
  
  fig_response <- fig_response + draw_grob(rect_mode1) + draw_grob(rect_mode3)#+
    
  
  fig_response <- fig_response +
    #firstmode
    add_fishape(family = "Pomacanthidae", option = "Centropyge_loricula",  xmin = .35, xmax = .38, ymin = 0.46, ymax = 0.48, alpha =.4)+
    add_fishape(family = "Carcharhinidae", option = "Triaenodon_obesus",  xmin = .38, xmax = .43, ymin = 0.44, ymax = 0.49, alpha =.4)+
    add_fishape(family = "Kyphosidae", option = "Kyphosus_cinerascens",  xmin = .36, xmax = .39, ymin = 0.43, ymax = 0.46, alpha =.4)+
    add_fishape(family = "Muraenidae", option = "Gymnothorax_javanicus",  xmin = .3, xmax = .36, ymin = 0.43, ymax = 0.47, alpha =.4)+
    #annotate("text", x=0.35, y=.42, label= "n = 919", size = 6)+
    #secondmode
    add_fishape(family = "Scombridae", option = "Thunnus_albacares",  xmin = .17, xmax = .24, ymin = 0.15, ymax = 0.25, alpha =.4)+
    add_fishape(family = "Alopiidae", option = "Alopias_vulpinus",  xmin = .17, xmax = .25, ymin = 0.21, ymax = 0.26, alpha =.4)+
    add_fishape(family = "Rhincodontidae", option = "Rhincodon_typus",  xmin = .24, xmax = .34, ymin = 0.13, ymax = 0.23, alpha =.4)+
    add_fishape(family = "Mobulidae", option = "Mobula_birostris",  xmin = .25, xmax = .33, ymin = 0.2, ymax = 0.25, alpha =.4)#+
    #annotate("text", x=0.22, y=.155, label= "n = 919", size = 6)

  print(fig_response)
  ggsave(fig_response, filename = here::here("outputs", "fig_2_response.jpeg"), width = 12, height = 12, units = "in", dpi =300)#render cowplots in jpeg less you get seethrough bits
    
}

#' response variable multiplot 
#'
#' @param dat 
#'
#' @import ggplot2 
#' @import cowplot
#' @import dplyr
#' @import grid
#' @import purrr
#' @import fishualize
#' 
#' @return
#' @export
#'
#' 



response_fig_new <- function(dat, tab_first, tab_second, min_size){

  
  #dat2 <- dat2[which(dat2$weight_kg > min_size),]
  
  options(scipen=4)
  #levels(dat2$Type) <- c("Pelagic", "Benthic")
  
  #modal histogramme
  #long format
  df = bind_rows(
    #first_mode
    data.frame(mode = tab_first$logFirstmode, mode_type ="Small fishes", bruvs_type = tab_first$bruvs_type, stringsAsFactors = T),
    #second_mode
    data.frame(mode = tab_second$logSecondmode, mode_type = "Large fishes", bruvs_type =tab_second$bruvs_type, stringsAsFactors = T)
  )
  
  df$bruvs_mode <- paste(df$bruvs_type, df$mode_type, sep = "_")
  df$bruvs_mode <- factor(df$bruvs_mode, levels = c("benthic_second", "pelagic_second","benthic_first","pelagic_first"))
  
  df <- tidyr::drop_na(df, mode)
  
  # if you want to include mean values. Doesn't work for some reason
  df %>%
    dplyr::group_by(bruvs_type, mode_type) %>%
    dplyr::summarise(Mean.mode = mean(10^mode)) -> df2
  
  fig_modes_hist <- ggplot()+
    geom_histogram(data=df, aes(x=10^mode, colour = bruvs_type, fill =bruvs_type), alpha=.4) + 
    scale_colour_manual(name = "", values = c("pelagic" = "#077DAA", "benthic"="darkorange"))+
    scale_fill_manual(name = "",  values = c("pelagic" = "#077DAA","benthic"="orange"))+
    xlab("Body size (kg)") +theme_light()+ylim(0,220)+ylab("count")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "none",  axis.text.x = element_text(size=16),
          strip.background =element_rect(fill="white"),
          strip.placement = "inside",
          strip.text = element_text(size=16, colour ="black"),
          axis.title.x = element_text(size=16),
          axis.title.y = element_text(size=16), axis.text.y = element_text(size=12))+
    geom_vline(data=df2, aes(xintercept = Mean.mode, colour = bruvs_type)) +
    scale_x_log10(limits = c(0.001, 1400), breaks = c(0.01, 1, 100), labels = c(0.01, 1, 100))+
    facet_wrap(~mode_type, nrow=2, strip.position = "bottom")
  
  
  dat <- tidyr::drop_na(dat, betaslope)
  
  dat %>%
    dplyr::group_by(bruvs_type) %>%
    dplyr::summarise(Mean.beta = mean(betaslope)) -> df3
  
  fig_betaslope <- ggplot() +
    geom_histogram(data = dat, aes(x= betaslope, fill = bruvs_type, colour = bruvs_type), alpha =.4)+
    scale_colour_manual(values = c("pelagic" = "#077DAA", "benthic"="darkorange"))+
    scale_fill_manual(values = c("pelagic" = '#077DAA', 'benthic' = 'orange')) + xlim(c(-2.5, 0))+xlab("Size spectra slope value")+ylab("count")+
    theme_light() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none", 
                          axis.title=element_text(size=16),
                          legend.title = element_blank(),
                          legend.text = element_text(size =16), 
                          axis.text.x = element_text(size=16),
                          axis.text.y = element_text(size=16))+ 
    geom_vline(data=df3, aes(xintercept = Mean.beta, colour = bruvs_type))
    # add_fishape(family = "Pomacanthidae", option = "Centropyge_loricula",  xmin = .0, xmax = .05, ymin = 0.87, ymax = 0.92, scaled = TRUE, xlim = c(-2.5, 0), ylim =c(0, 230), alpha =.4)+
    # add_fishape(family = "Carangidae", option = "Caranx_melampygus",  xmin = .1, xmax = .23, ymin = 0.8, ymax = 0.88, scaled = TRUE, xlim = c(-2.5, 0), ylim =c(0, 230), alpha =.4)+
    # add_fishape(family = "Scombridae", option = "Thunnus_albacares",  xmin = .23, xmax = .43, ymin = 0.67, ymax = 0.87, scaled = TRUE, xlim = c(-2.5, 0), ylim =c(0, 230), alpha =.4)
  #   geom_polygon(data=data.frame(x=c(-2.45, -1.9, -2.45), y = c(165, 165, 200)), aes(x=x, y=y), fill=NA, colour = "grey", alpha =.4)+
  #   geom_polygon(data=data.frame(x=c(-2.45, -2.1, -2.45), y = c(165, 165, 200)), aes(x=x, y=y), fill=NA, colour = "grey", alpha =.4, linetype ='dotted')
  # #annotate("text", x=-2, y=155, label= "n = 1041", size = 6)
  
  
  fig_response <- ggdraw()+
    draw_plot(fig_modes_hist, 0.01, 0, 0.48, 1)+
    draw_plot(fig_betaslope,     0.5, 0,   .5,   1)+
    draw_plot_label(c("A", "B"), c(0, 0.51), c(1, 1), size = 22, fontface = "bold")
  
  
  # fig_response <- fig_response +
  #   #firstmode
  #   add_fishape(family = "Pomacanthidae", option = "Centropyge_loricula",  xmin = .35, xmax = .38, ymin = 0.86, ymax = 0.88, alpha =.4)+
  #   add_fishape(family = "Carcharhinidae", option = "Triaenodon_obesus",  xmin = .38, xmax = .43, ymin = 0.84, ymax = 0.89, alpha =.4)+
  #   add_fishape(family = "Kyphosidae", option = "Kyphosus_cinerascens",  xmin = .36, xmax = .39, ymin = 0.83, ymax = 0.86, alpha =.4)+
  #   add_fishape(family = "Muraenidae", option = "Gymnothorax_javanicus",  xmin = .3, xmax = .36, ymin = 0.83, ymax = 0.87, alpha =.4)+
  #   #annotate("text", x=0.35, y=.42, label= "n = 919", size = 6)+
  #   #secondmode
  #   add_fishape(family = "Scombridae", option = "Thunnus_albacares",  xmin = .17, xmax = .24, ymin = 0.35, ymax = 0.4, alpha =.4)+
  #   add_fishape(family = "Alopiidae", option = "Alopias_vulpinus",  xmin = .17, xmax = .25, ymin = 0.41, ymax = 0.46, alpha =.4)+
  #   add_fishape(family = "Rhincodontidae", option = "Rhincodon_typus",  xmin = .24, xmax = .34, ymin = 0.33, ymax = 0.41, alpha =.4)+
  #   add_fishape(family = "Mobulidae", option = "Mobula_birostris",  xmin = .25, xmax = .33, ymin = 0.4, ymax = 0.48, alpha =.4)#+
  # #annotate("text", x=0.22, y=.155, label= "n = 919", size = 6)
  # 
  print(fig_response)
  ggsave(fig_response, filename = here::here("outputs", "fig_S2_response_new.jpeg"), width = 12, height = 6, units = "in", dpi =300)#render cowplots in jpeg less you get seethrough bits
  
}



#' conceptual diagram with model and size spectra fishing effects 
#'
#' @param data 
#'
#' @import ggplot2 
#' @import cowplot
#' @import dplyr
#' @import purrr
#' 
#' @return
#' @export
#'
#' 

#code adapted from https://github.com/FreddieJH/inverts_size_spec.
#Heather FJ, Blanchard JL, Edgar GJ, Trebilco R, Stuart‐Smith RD. 2021. Globally consistent reef size spectra integrating fishes and invertebrates. Ecology Letters 24:572–579.


conceptual_dia <- function(dat2, dat, data, min_size, bandw, scale, alpha){
 

  set.seed(1)
  # setting parameters for normal distribution plots (for steepening effect)
  first_mode <- rlnorm(n = 10, meanlog = 1.5, sdlog = 0.2) + 30
  second_mode <- rlnorm(n = 10, meanlog = 2.5, sdlog = 0.1) + 60
  first_sd   <- rnorm(n = 10, mean = 8, sd = 1)
  second_sd   <- rnorm(n = 10, mean = 8, sd = 1)
  # Figure 1A
  
  #modal distribution of sizes
  p <-
    tibble(type = "first", spp =1:10, mean = first_mode, sd = first_sd) %>% 
    bind_rows(tibble(type = "second", spp =1:10, mean = second_mode, sd = second_sd)) %>% 
    mutate(vals = map2(mean, sd, .f=~dnorm(0:100, mean=.x, sd=.y))) %>% 
    unnest(cols = "vals") %>% 
    mutate(id = paste0(type, spp)) %>% 
    mutate(x = rep(0:100, 20)) %>% 
    ggplot2::ggplot(aes(x, vals, fill=type, group=id)) +
    ggplot2::geom_area(position = 'identity', alpha=0.1, aes(colour=type)) +
    theme_light() +
    theme(plot.title = element_text(size =16, hjust = 0.5), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = c(0.87, 0.9),
          legend.background = element_rect(fill = "transparent"), axis.title.y= element_text(size=16), axis.title.x = element_text(size=16),axis.text.y = element_text(size = 16),axis.text.x = element_text(size = 16), legend.text = element_text(size = 16), legend.title = element_blank()) + labs(x = "Body size (kg)",y = "Probability density") +
    scale_fill_manual(name="Mode", labels = c("First mode", "Second mode"), values =  c("darkgreen", "lightgreen"))+ 
    scale_colour_manual(name="Mode", labels = c("First mode", "Second mode"), values =  c("darkgreen", "lightgreen"))+
    scale_x_continuous(breaks = c(20, 50, 80), labels = c("0.01", "1", "100"))+xlab("Body size (kg)")+ylab("")
  
  
  ####hypothesised effects of fishing on modes
  
  #second mode smaller
  p2 <- p + geom_segment(aes(x = 70, y = 0.02, xend = 50, yend = 0.02),
                         arrow = arrow(length = unit(0.3, "cm")), linetype = "solid")+
    theme(legend.position = "null")+xlab("")+ylab("Probability density")+ggtitle("Predator depletion")
  
  #first mode smaller
  p3 <- p + geom_segment(aes(x = 30, y = 0.03, xend = 10, yend = 0.03),
                         arrow = arrow(length = unit(0.3, "cm")), linetype = "solid")+
    theme(legend.position = "null")+xlab("")+ggtitle("Prey fish depletion")
  
  #first and second mode smaller
  
  p4 <- p + geom_segment(aes(x = 30, y = 0.03, xend = 10, yend = 0.03),
                         arrow = arrow(length = unit(0.3, "cm")), linetype = "solid")+
    geom_segment(aes(x = 70, y = 0.02, xend = 50, yend = 0.02),
                 arrow = arrow(length = unit(0.3, "cm")), linetype = "solid")+
    theme(legend.position = "null")+xlab("")+ggtitle("Predator and prey depletion")
  
  
  # first mode smaller and second mode bigger
  p5 <-p + geom_segment(aes(x = 35, y = 0.03, xend = 55, yend = 0.03),
                        arrow = arrow(length = unit(0.5, "cm")), linetype = "dashed")+ 
    geom_segment(aes(x = 70, y = 0.02, xend = 50, yend = 0.02),
                 arrow = arrow(length = unit(0.3, "cm")), linetype = "solid")+
    theme(legend.position = "none")+
    ##for legend
    geom_segment(aes(x = 40, y = 0.07, xend = 55, yend = 0.07),
                 arrow = arrow(length = unit(0.3, "cm")), linetype = "solid")+
    geom_segment(aes(x = 40, y = 0.065, xend = 55, yend = 0.065),
                 arrow = arrow(length = unit(0.5, "cm")), linetype = "dashed")+
    annotate("text", x=80, y=0.07, label= "Direct effect", size=5)+
    annotate("text", x=80, y=.065, label= "Indirect effect", size =5)+xlab("")+ggtitle("Trophic release, following \n predator depletion")
  
  
  #steepening of size spectra
  p1a <-
    tibble(mass = 1:10, fished = (6 + (-1.4*mass)), unfished =  (6 + (-1*mass))) %>% 
    gather(key = "type", value = "abundance", -mass) %>% 
    ggplot2::ggplot(aes(mass, abundance)) +
    ggplot2::geom_ribbon(aes(ymin=(6 + (-1.4*mass)), ymax=(6 + (-1*mass))), alpha=0.3, col="transparent") +
    ggplot2::geom_line(aes(linetype = type), size = 2, colour = "darkgreen", alpha =.5) + 
    #ggplot2::geom_point(aes(fill = type), col="black", shape=21, size=3) + 
    theme_light() +
    theme(plot.title = element_text(size =16, hjust = 0.5),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none", 
          legend.background = element_rect(fill = "transparent")) + 
    labs(x = "Body size (kg)",
         y = "Log(abundance)") +ggtitle("Slope pronounced steepening") +
    theme(axis.text.x = element_text(size=16), axis.text.y = element_blank(),axis.title = element_text(size =16), legend.text =element_text(size =16), legend.title = element_blank())+
    scale_linetype_manual(values=c("dotted", "solid"))+scale_x_continuous(breaks = c(2.5, 5, 7.5), labels = c("0.01", "1", "100"))
  
  
  #shallowing of size spectra
  p1b <-
    tibble(mass = 1:10, fished = (4 + (-0.8*mass)), unfished =  (6 + (-1*mass))) %>% 
    gather(key = "type", value = "abundance", -mass) %>% 
    ggplot2::ggplot(aes(mass, abundance)) +
    ggplot2::geom_ribbon(aes(ymin=(4 + (-0.8*mass)), ymax=(6 + (-1*mass))), alpha=0.3, col="transparent") +
    ggplot2::geom_line(aes(linetype = type), size = 2, colour = "darkgreen", alpha =.5) + 
    #ggplot2::geom_point(aes(fill = type), col="black", shape=21, size=3) + 
    theme_light() +ggtitle("Slope shallowing") +
    theme(plot.title = element_text(size =16, hjust = 0.5),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none", 
          legend.background = element_rect(fill = "transparent")) + 
    labs(x = "Body size (kg)",
         y = "")+ theme(axis.text.x = element_text(size=16), axis.text.y = element_blank(),axis.title = element_text(size =16), legend.text =element_text(size =16), legend.title = element_blank())+
    scale_linetype_manual(values=c("dotted", "solid"))+scale_x_continuous(breaks = c(2.5, 5, 7.5), labels = c("0.01", "1", "100"))
  
  
  #stable size spectra - proportional depletion
  p1c <-
    tibble(mass = 1:10, fished = (4 + (-1*mass)), unfished =  (6 + (-1*mass))) %>% 
    gather(key = "type", value = "abundance", -mass) %>% 
    ggplot2::ggplot(aes(mass, abundance)) +
    ggplot2::geom_ribbon(aes(ymin=(4 + (-1*mass)), ymax=(6 + (-1*mass))), alpha=0.3, col="transparent") +
    ggplot2::geom_line(aes(linetype = type), size = 2, colour = "darkgreen", alpha =.5) + 
    #ggplot2::geom_point(aes(fill = type), col="black", shape=21, size=3) + 
    theme_light() +
    theme(plot.title = element_text(size =16, hjust = 0.5),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none", 
          legend.background = element_rect(fill = "transparent")) + 
    labs(x = "Body size (kg)",
         y = "")+ggtitle("Slope the same") + theme(axis.text.x = element_text(size=16), axis.text.y = element_blank(),axis.title = element_text(size =16), legend.text =element_text(size =16), legend.title = element_blank())+
    scale_linetype_manual(values=c("dotted", "solid"))+scale_x_continuous(breaks = c(2.5, 5, 7.5), labels = c("0.01", "1", "100"))
  
  # steepening of size spectra with trophic release
  p1d <-
    tibble(mass = 1:10, Fished = (9 + (-1.4*mass)), Unfished =  (6 + (-.85*mass))) %>% 
    gather(key = "type", value = "abundance", -mass) %>% 
    ggplot2::ggplot(aes(mass, abundance)) +
    ggplot2::geom_ribbon(aes(ymin=(9 + (-1.4*mass)), ymax=(6 + (-.85*mass))), alpha=0.3, col="transparent") +
    ggplot2::geom_line(aes(linetype = type), size = 2, colour = "darkgreen", alpha =.5) + 
    #ggplot2::geom_point(aes(fill = type), col="black", shape=21, size=3) + 
    theme_light() +
    theme(plot.title = element_text(size =16, hjust = 0.5),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = c(0.8, 0.8), 
          legend.background = element_rect(fill = "transparent")) + 
    labs(x = "Body size (kg)",
         y = "")+ggtitle("Slope modest steepening")+  theme(axis.text.x = element_text(size =16), axis.text = element_blank(),axis.title = element_text(size =16), legend.text =element_text(size =16), legend.title = element_blank())+
    scale_linetype_manual(values=c("dotted", "solid"))+scale_x_continuous(breaks = c(2.5, 5, 7.5), labels = c("0.01", "1", "100"))
  
  #combined for multiplot
  
  fig_cptual <-     ggdraw()+
    draw_plot(p+ylab("Probability density"), 0.25, 0.6, 0.45, 0.4)+
    draw_plot(p2, 0,    0.3, 0.25,0.3)+
    draw_plot(p3, 0.25, 0.3, 0.25,0.3)+
    draw_plot(p4, 0.50, 0.3, 0.25,0.3)+
    draw_plot(p5, 0.75, 0.3, 0.25,0.315)+
    draw_plot(p1a, 0,    0, 0.25, 0.3)+
    draw_plot(p1b, 0.25, 0, 0.25, 0.3)+
    draw_plot(p1c, 0.50, 0, 0.25, 0.3)+
    draw_plot(p1d, 0.75, 0, 0.25, 0.3)+
    draw_plot_label(c("a", "b", "c", "d", "e", "f", "g", "h", "i"), c(0.25, 0, .25, 0.5, .75, 0, .25, 0.5, .75), c(1, .6, .6, .6, .6, .3, .3,.3,.3), size = 22, fontface = "bold")

  print(fig_cptual)
  ggsave(fig_cptual, filename = here::here("outputs", "fig_3_cptual.jpeg"), width = 16, height = 14, units = "in", dpi =300)#render cowplots in jpeg less you get seethrough bits
}


#' response vs response
#'
#' @param data 
#'
#' @import ggplot2 
#' @import cowplot
#' @return
#' @export
#'
#' 

response_vs_response <- function(data){
  options(scipen=5)
 
  
  #firstmode vs secondmode
  fmode_smode <- ggplot(data = tab_firstmode,aes(x=first_mode, y=second_mode, colour= bruvs_type, fill = bruvs_type)) + geom_point(shape = ".")+ 
    geom_smooth(method='lm')+scale_x_log10(breaks = c(0.001, 0.1, 10))+
    geom_abline(slope=1, intercept = 2.2)+
    scale_y_log10()+theme_light()+
    theme(legend.position = "none", axis.title.y = element_text(size=16),
          legend.text = element_text(size =16),axis.text.x = element_text(size=16),
          axis.text.y = element_text(size=16), axis.title.x = element_text(size=16))+
    scale_fill_manual(values = c("pelagic" = '#077DAA', 'benthic' = 'orange'))+  
    scale_colour_manual(values = c("pelagic" = '#077DAA', 'benthic' = 'darkorange')) +xlab("First mode (kg)")+ylab("Second mode (kg)")
  
  #firstmode vs betaslope
  fmode_beta <- ggplot(data = data,aes(x=first_mode, y=betaslope, colour= bruvs_type, fill = bruvs_type)) + geom_point(shape = ".")+ 
    geom_smooth(method='lm')+scale_x_log10(breaks = c(0.001, 0.1, 10)) +theme_light()+
    theme(legend.position = "none", axis.title.y = element_text(size=16),
          legend.text = element_text(size =16),axis.text.x = element_text(size=16),
          axis.text.y = element_text(size=16), axis.title.x = element_text(size=16))+
    scale_fill_manual(values = c("pelagic" = '#077DAA', 'benthic' = 'orange'))+
    scale_colour_manual(values = c("pelagic" = '#077DAA', 'benthic' = 'darkorange')) + xlab("First mode (kg)")
  
  #secondmode vs betaslope
  smode_beta <- ggplot(data = tab_firstmode,aes(x=second_mode, y=betaslope, colour= bruvs_type, fill = bruvs_type)) + geom_point(shape = ".")+ 
    geom_smooth(method='lm')+scale_x_log10(breaks = c(0.01, 1, 100))+theme_light()+
    theme(legend.position = "none", axis.title.y = element_text(size=16),
          legend.text = element_text(size =16),axis.text.x = element_text(size=16),
          axis.text.y = element_text(size=16), axis.title.x = element_text(size=16))+
    scale_fill_manual(values = c("pelagic" = '#077DAA', 'benthic' = 'orange'))+
    scale_colour_manual(values = c("pelagic" = '#077DAA', 'benthic' = 'darkorange')) + xlab("Second mode (kg)")
  

  r_vs_r <- ggdraw()+
    draw_plot(fmode_smode,0, 0, .33, 1)+
    draw_plot(fmode_beta,0.33, 0, .33,1)+
    draw_plot(smode_beta,0.66, 0, .33,1)

  print(r_vs_r)
  
  ggsave(r_vs_r, filename = here::here("outputs", "r_vs_r.png"), width = 14, height =6, units = "in", dpi =300)
  invisible(r_vs_r)
  
}



#' Title fig ridges overlap by exped
#' 
#' try this to include mode https://rpkgs.datanovia.com/ggpubr/reference/stat_central_tendency.html
#'
#' @param data size data
#' @param min_size minimum size in kg
#' @param lat_band number of latitudinal bans
#' @import ggplot2 ggridges ggstance
#' @import fishualize
#' @return
#' @export
#'
#' @examples
#' 
figridges_overlap_exped <- function(dat, min_size, bandw, scale, alpha){
  
  data = dat[dat$weight_kg > min_size, ]# select predatory fish - individuals larger than minsize kg
  
  rigplot <- ggplot(data, aes(x=weight_kg, y= Exped, height = ..ndensity..))+
    #ggridges::stat_density_ridges(rel_min_height = 0.01, aes(y = cut_width(lat_in, 5), colour = Type, fill = Type), 
    #ggridges::geom_density_ridges(rel_min_height = 0.01, aes(y = cut(abs(lat_in), breaks = c(0, 10, 20, 30, 40, 70)), fill = Type, colour = Type), 
    ggridges::geom_density_ridges(bandwidth = bandw, rel_min_height = 0.005, aes(y = Exped, fill = Type), colour = "#FFFFFF", 
                                  alpha = alpha, scale = scale)+
    scale_x_log10(limits = c(0.001, 1200), breaks = c(0.01, 1, 100), labels = c(0.01, 1, 100))+
    xlab('Body size (kg)') +ylab('Probability density')+
    scale_fill_manual(values = c("Midwater" = '#077DAA', 'Seabed' = 'orange'))+
    scale_colour_manual(values = c("Midwater" = '#077DAA', 'Seabed' = 'darkorange'))+
    scale_linetype_manual(breaks=c(0.5,1), values =c("dotted", "solid"))+ 
    scale_y_discrete(expand = c(0.1, 0))+
    theme_light() +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),strip.text.x = element_blank(),panel.grid.major.y = element_blank(),legend.position = "NA", axis.title=element_text(size=22),legend.title = element_blank(),
                         legend.text = element_text(size =16), 
                         axis.text.x = element_text(size=16),
                         axis.text = element_blank(), 
                         legend.background = element_rect(fill = "transparent"))+
    theme(plot.margin = margin(1,1,0,1.2, "cm"))+
    coord_cartesian(clip = "off")+facet_wrap(~Type, nrow =2)

  
  print(rigplot)
  
  ggsave(rigplot, filename = here::here("outputs", "fig_ridges_modes.png"), width = 12, height = 6, units = "in", dpi =300)
  
  invisible(rigplot)
  
}


#' multiplot fig 1 sampling effort and rank order species 

#' @param covariate_name name of covariate
#' 
#' @import ggplot2
#' @import cowplot
#' @import fishualize
#' @return
#' @export
#'
#'

multi_fig_sample <- function(fig_map, fig_sp_rank){
  
  fish_sil <- cowplot::ggdraw() + cowplot::draw_image(here::here("photo","fish_fig_2.jpeg"))
  
  
  
  fig_2_sample <- cowplot::ggdraw() +
    cowplot::draw_plot(fig_map, 0, .39, 1, .63) +
    cowplot::draw_plot(fig_sp_rank,  0, 0,  1,  .4)+
    cowplot::draw_plot( fish_sil,  0.08, 0.24,  .60,  .3)+
    draw_plot_label(c("A", "B", "C"), c(0, 0, .7), c(1, .43, .43), size = 26, fontface = "bold")
  
  
  # fig_2_sample <- fig_2_sample +
  #   fishualize::add_fishape(family = "Pomacanthidae", option = "Centropyge_loricula",  xmin = .09, xmax = .115, ymin = 0.37, ymax = 0.42, alpha=.4)+
  #   fishualize::add_fishape(family = "Kyphosidae", option = "Kyphosus_cinerascens",  xmin = .2, xmax = .25, ymin = 0.37, ymax = 0.42, alpha=.4)+
  #   fishualize::add_fishape(family = "Muraenidae", option = "Gymnothorax_javanicus",  xmin = .31, xmax = .4, ymin = 0.35, ymax = 0.44, alpha =.4)+
  #   fishualize::add_fishape(family = "Scombridae", option = "Thunnus_albacares",  xmin = .435, xmax = .52, ymin = 0.37, ymax = 0.42, alpha=.4)+
  #   fishualize::add_fishape(family = "Rhincodontidae", option = "Rhincodon_typus",  xmin = .56, xmax = .69, ymin = 0.35, ymax = 0.44, alpha =.4)

  print(fig_2_sample)
  
  invisible(fig_2_sample)
  
  ggsave(fig_2_sample, filename = here::here("outputs", "fig_2_sample.jpeg"), width = 16, height = 16, units = "in", dpi =300)#render cowplots in jpeg less you get seethrough bits
  
  
  
}



