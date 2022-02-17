#' Make map of meta for pelagic/benthic bruvs
#'
#' @param meta 
#' @param type 
#'
#' @return
#' @export
#'

make_map_meta <- function(meta, type){
    
  #convert to spatial dataframe
  meta_sp = meta
  sp::coordinates(meta_sp) <- ~ lon_in + lat_in
  
  #map
  png(here::here("outputs", type, paste0("map_meta_", type,".png")), width = 1440, height = 960,)
  maps::map('world', fill=T, col= "grey")
  sp::plot(meta_sp, add = T, pch = 19, col = 2, cex = 0.7)
  dev.off()

} 