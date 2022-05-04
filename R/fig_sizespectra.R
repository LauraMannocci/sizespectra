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
#' @param data 
#' @import ggplot2 ggridges ggstance
#' @import fishualize
#' @return
#' @export
#'
#' @examples
#' 
figridges <- function(data){
  
  rigplot <- ggplot(data, aes(x=weight_kg, y= lat_in))+
    #ggridges::stat_density_ridges(rel_min_height = 0.01, aes(y = cut_width(lat_in, 5), colour = Type, fill = Type), 
    #ggridges::geom_density_ridges(rel_min_height = 0.01, aes(y = cut(abs(lat_in), breaks = c(0, 10, 20, 30, 40, 70)), fill = Type, colour = Type), 
    ggridges::geom_density_ridges(rel_min_height = 0.01, aes(y = cut_number(abs(lat_in), n=6), fill = Type, colour = Type), 
    alpha = 0.6, scale =1.8, jittered_points = TRUE, quantile_lines = TRUE, quantiles = c(0.5, 1), vline_size = 1.5, 
                                  position = position_points_jitter(height = 0.2, yoffset= 0.2, adjust_vlines = TRUE),
                                  point_size = 0.01, point_alpha = 0)+
    scale_x_log10(limits = c(5e-06, 1200), breaks = c(0.001, 0.1, 100), labels = c(0.001, 0.1, 100))+
    xlab('Body size (kg)') +ylab('Latitude')+
    scale_fill_manual(values = c("Midwater" = '#077DAA', 'Seabed' = 'orange'))+
    scale_colour_manual(values = c("Midwater" = '#077DAA', 'Seabed' = 'darkorange'))+
    theme_light() +theme(legend.position = "bottom", axis.title=element_text(size=22),legend.title = element_blank(),
                         legend.text = element_text(size =22), 
                         axis.text.x = element_text(size=16),
                         axis.text.y = element_text(size=16))+
    coord_cartesian(clip = "off")#+
  #fishualize::add_fishape(family = "Pomacanthidae", option = "Centropyge_loricula",  xmin = -4.4, xmax = -3.9, ymin = 15.6, ymax =16.1)+
  #fishualize::add_fishape(family = "Pomacanthidae", option = "Centropyge_loricula",  xmin = -4.7, xmax = -4.2, ymin = 16.1, ymax =16.3)+
  #fishualize::add_fishape(family = "Blenniidae", option = "Antennablennius_adenensis",  xmin = -5, xmax = -4.3, ymin = 15.5, ymax =16)+
  #fishualize::add_fishape(family = "Rhincodontidae", option = "Rhincodon_typus", xmin= 0.8, xmax = 3.35, ymin = 14.1 , ymax = 17)+
  #fishualize::add_fishape(family = "Alopiidae", option = "Alopias_vulpinus", xmin= 1.7, xmax = 3.32, ymin = 15.6 , ymax = 17)
  #ggstance::stat_summaryh(fun.x=median, geom="text", aes(label=sprintf("%1.1f", ..x..)),
                 # position=position_nudge(x=-0.1), size=3.5)
  
  print(rigplot)
  
  ggsave(rigplot, filename = here::here("outputs", "ridges.png"), width = 10, height = 16, units = "in", dpi =300)
  
  invisible(rigplot)
  
}


#' length weight relationship with marginal density plot
#'
#' @param data 
#' @import ggplot2 ggExtra
#' @return
#' @export
#'
#' @examples
#' 
fl_lengthweight <- function(data){

  length_weight <- 
    ggExtra::ggMarginal(
    ggplot2::ggplot() +
    geom_point(data = data, aes(x= Lengthcm, y=weight_kg, colour = Type, fill = Type), alpha = 0.01, size = 0.5)+
    scale_colour_manual(values = c("Midwater" = '#077DAA', 'Seabed' = 'orange'))+
    scale_fill_manual(values = c("Midwater" = '#077DAA', 'Seabed' = 'orange')) +
    scale_x_log10() + scale_y_log10(breaks= c(0.1, 1, 10))+ 
    xlab("length (cm)")+ylab("weight (kg)")+
    theme_light() +theme(legend.position = "bottom", axis.title=element_text(size=22),legend.title = element_blank(),
                       legend.text = element_text(size =22), 
                       axis.text.x = element_text(size=16),
                       axis.text.y = element_text(size=16))+
    guides(fill = guide_legend(override.aes = list(size = 2, alpha= 1 )))
              , groupFill= TRUE, groupColour = TRUE, alpha = .6, size = 4)
  
  print(length_weight)
  
  ggsave(length_weight, filename = here::here("outputs", "length_weight.png"), width = 10, height = 10, units = "in", dpi =300)

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
 fl_species_ord_marg <- function(data, lower.line, mid.line, upper.line){
  
  fl_speciesrank <-  
    ggExtra::ggMarginal(
    ggplot2::ggplot()+
    geom_jitter(data= data, aes(x=reorder(Binomial, weight_kg, na.rm = TRUE), y= weight_kg,  colour = Type, alpha= Type), size = 0.5, width = 1.5)+
    scale_y_log10(name  = "Body size", breaks= c(0.001, 1, 100), labels= c("1g", "1kg", "100kg"))+
    labs(x="Species")+
    theme(legend.position = "none", axis.title.y = element_text(size=20, angle = 90),
            legend.text = element_text(size =16),axis.text.x = element_text(size=16),
            axis.text.y = element_text(size=16), axis.title.x = element_text(size=20),
            #panel.grid.major.x = element_blank(),
            axis.ticks.x = element_blank())+
    scale_colour_manual(values = c("Midwater" = '#077DAA', 'Seabed' = 'orange'))+ 
    scale_x_discrete(label = NULL)+
    scale_alpha_discrete(range = c(0.80, 0.05))+ 
    geom_hline(yintercept=upper.line, na.rm =TRUE, linetype="dotted")+ 
    geom_hline(yintercept=mid.line, na.rm =TRUE, linetype="dotted")+
    geom_hline(yintercept=lower.line, na.rm =TRUE, linetype="dotted")+
    geom_hline(yintercept=0.058, na.rm =TRUE, linetype="solid",colour = "sienna2")+ 
    geom_hline(yintercept=0.0087, na.rm =TRUE, linetype="solid",colour = "darkblue"),
    groupFill= TRUE, groupColour = TRUE, type = "violin", 
    alpha = .6, size = 2, margins = "y", draw_quantiles = c(0.05, 0.5, 0.95)) 
    #fishualize::add_fishape(family = "Pomacanthidae", option = "Centropyge_loricula",  xmin = 0.1, xmax = 0.3, ymin = 0.7, ymax = 0.9, xlim = c(0, 1460), ylim = c(0.1, 100), scaled = TRUE)#+
    #fishualize::add_fishape(family = "Pomacanthidae", option = "Centropyge_loricula",  xmin = 0.15, xmax = 0.25, ymin = 0.75, ymax =0.85, scaled = TRUE)+
    #fishualize::add_fishape(family = "Blenniidae", option = "Antennablennius_adenensis",  xmin = 0.2, xmax = 0.3, ymin = 0.85, ymax =0.95, scaled = TRUE)+
    #fishualize::add_fishape(family = "Rhincodontidae", option = "Rhincodon_typus", xmin= 0.75, xmax = 0.95, ymin = 0.6 , ymax = 0.9, scaled = TRUE)+
    #fishualize::add_fishape(family = "Alopiidae", option = "Alopias_vulpinus", xmin= 0.7, xmax = 0.95, ymin = 0.75 , ymax = 0.9, scaled = TRUE)
  
  print(fl_speciesrank)
  
  ggsave(fl_speciesrank, filename = here::here("outputs", "species_ord_marg.png"), width = 20, height = 12, units = "in", dpi =300)
  
  invisible(fl_speciesrank)

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


    