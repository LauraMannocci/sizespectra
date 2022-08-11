
#' plot predictor variables for each bruvs type
#' @param dat data 
#' @param envar_name envar variables
#' @import ggplot2
#' @return
#' @export
#'


bruvs_var_range <- function(dat, envar_name) { 
  
  envar_plot <- ggplot(data=dat, aes(x=bruvs, y= dat[[envar_name]] )) +
  stat_summary(fun = mean, fun.min = min, fun.max = max, colour = c('orange','#077DAA')) +
  theme(axis.text = element_text(size = 10)) +
  theme(axis.title = element_text(size = 10)) +
  #ylim(0, 6000)+
  coord_flip() +xlab("") +ylab(envar_name)
  print(envar_plot)
  invisible(envar_plot)
  
}


#' plot predictor variables for each bruvs type
#' @param var_logBathy bahty
#' @param var_logDistCR distance to coral reef
#' @param var_logDistSM seamount
#' @param var_logDistP port
#' @param var_logDistC coast
#' @param var_logTTM travel time marke
#' @param var_logCHL chal
#' @param var_logSST sst_mean
#' @param var_SST_sd sst_SD
#' @param var_SST_GEm goverment effectivness
#' 
#' @import ggplot2
#' @import cowplot
#' @return
#' @export
#'
#'

multi_envar_range <- function() {
  
  multi_envar <- ggdraw() +
  draw_plot(var_logBathy, 0, 0, .5, .2) +
  draw_plot(var_logDistCR, 0, .2, .5, .2) +
  draw_plot(var_logDistSM, 0, .4, .5, .2) +
  draw_plot(var_logDistP, 0, .6, .5, .2) +
  draw_plot(var_logDistC, 0, .8, .5, .2) +
  draw_plot(var_logTTM, 0.5, .0, .5, .2) +
  draw_plot(var_logCHL, 0.5, .2, .5, .2) +
  draw_plot(var_logSST, 0.5, .4, .5, .2) +
  draw_plot(var_SST_sd, 0.5, .6, .5, .2) +
  draw_plot(var_SST_GEm, 0.5, .8, .5, .2) +
  draw_plot_label(c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"), c(0,.5, .0, 0.5,0,0.5,0,0.5,0, .5), c(1, 1, .8, .8, .6, .6, .4, .4, .2, .2), size = 10)

  print(multi_envar)

  ggsave(multi_envar, filename = here::here("outputs", "supp_fig_envar_range.jpeg"), width = 10, height = 10, units = "in", dpi =300)
  
  invisible(multi_envar)
  
  
}

#' plot predictor variables for each bruvs type plus protection
#' @param prot_var_logBathy bahty
#' @param prot_var_logDistCR distance to coral reef
#' @param prot_var_logDistSM seamount
#' @param prot_var_logDistP port
#' @param prot_var_logDistC coast
#' @param prot_var_logTTM travel time marke
#' @param prot_var_logCHL chal
#' @param prot_var_logSST sst_mean
#' @param prot_var_SST_sd sst_SD
#' @param prot_var_SST_GEm goverment effectivness
#' 
#' @import ggplot2
#' @import cowplot
#' @return
#' @export
#'
#'

multi_envar_range_cat <- function() {
  
  multi_envar <- ggdraw() +
    draw_plot(prot_var_logBathy,  0,   0, .5, .16) +
    draw_plot(prot_var_logDistCR, 0, .16, .5, .16) +
    draw_plot(prot_var_logDistSM, 0, .33, .5, .16) +
    draw_plot(prot_var_logDistP,  0, .50, .5, .16) +
    draw_plot(prot_var_logDistC,  0, .67, .5, .16) +
    draw_plot(prot_var_logTTM,  0, .84, .5, .16) +
    draw_plot(prot_var_logCHL,  0.5,   0.16, .5, .16) +
    draw_plot(prot_var_logSST,  0.5, .33, .5, .16) +
    draw_plot(prot_var_SST_sd,  0.5, .50, .5, .16) +
    draw_plot(prot_var_GEm,     0.5,   .67, .5, .16) +
    draw_plot(prot_var_Slope,     0.5,   .84, .5, .16) +
    draw_plot_label(c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K"), c(0,.5, .0, 0.5,0,0.5,0,0.5,0, .5,0), c(1, 1, .84, .84, .67, .67, .5, .5, .33, .33, .16), size = 10)
  
  print(multi_envar)
  
  ggsave(multi_envar, filename = here::here("outputs", "supp_fig_envar_range_cat.jpeg"), width = 10, height = 12, units = "in", dpi =300)
  
  invisible(multi_envar)
  
  
}


#' read data with predictor variables
#'
#' @return
#' @export
#'

read_data_with_vars <- function(){

  read.table(here::here("data", "response", "pelagic_benthic_response_envar_clean.txt"))

}



#' clean data with predictor variables **ATTENTION need to update with new variables
#'
#' @param dat dataframe with predictor variables
#' 
#' @return
#' @export
#'

clean_data_with_vars <- function(dat, response_name){
   
 

    # transformation of variables
  dat %>% dplyr::mutate(protection = ifelse(is.na(protection), "Out", protection),) -> dat
  
  protection_use = as.factor(dat$protection)
  logconflicts = log10(dat$conflicts+1)
  logNGO = log10(dat$NGO+1)
  logTTM = log10(dat$TravelTime_market+1)
  logSAU = log10(dat$SAU+1)
  logTTP = log10(dat$TravelTime_pop+1)
  logSAU = log10(dat$SAU+1)
  logDistP = log10(dat$distPort+1)
  logDistSM = log10(dat$distSeamounts+1)
  logDistCR = log10(dat$distCoralReef+1)
  logDistC = log10(dat$distCoast+1)
  logBathy = log10(-dat$Bathymetry)
  logPP = log10(dat$PP+1)
  logCHL = log10(dat$CHL+1)
  logSST = log10(dat$SST_mean)
  bruvs = as.factor(dat$bruvs_type)
  exped = as.factor(dat$TERRITORY1)
  logFirstmode = log10(dat$first_mode+1)
  logSecondmode = log10(dat$second_mode+1)
  logUnimode = log10(dat$unimode+1)
  logDiffmode = log10(dat$second_mode - dat$first_mode)
  diffNormmode = (dat$second_mode - dat$first_mode)/dat$second_mode
  
  # bind
  dat = cbind(dat, exped, logconflicts,logNGO,logTTM,logSAU,logTTP, logDistP, logDistSM,logDistCR,logDistC,logBathy,logPP,logCHL,logSST,bruvs, protection_use,logFirstmode,logSecondmode, logUnimode, logDiffmode, diffNormmode)

  # drop nas in some variables
  dat %>%
    tidyr::drop_na(logBathy) %>%
    tidyr::drop_na(response_name) %>%
    tidyr::drop_na(mean_lat) %>%
    tidyr::drop_na(mean_long) -> dat
   
    dat  %>% 
  # rename protection levels
    #no_take, partly_protected, not_protected
      #dplyr::mutate(protection_use = dplyr::recode(protection_use, All = "no_take", None = "partly_protected", `Not reported` = "partly_protected", Out = "not_protected", Part = "no_take")) -> dat
    
      #no_take, not_protected 
    dplyr::mutate(protection_use = dplyr::recode(protection_use, All = "protected", None = "part_protected", `Not reported` = "part_protected", Out = "not_protected", Part = "protected")) -> dat
     
    #email from Laure
    # All = inside fully no-take MPA => YES
    # part = inside an MPA that is partly no-take - that is, an MPA where a portion of it is no-take. Does this mean that the coordinates are within the no-take portion? => YES, but we don't know if the coordinates are within the no take area. We just know that the coordinates are within the MPA that is partly no-take
    # none =  outside MPA => NO, none means that the coordinates are within a MPA without "no take zone"
    # not reported = inside MPA but the status of that MPA not reported. => YES
    # NA = outside or unknown data

    
  return(dat)


}





#' make correlogram of predictors ** ATTENTION need to update variables
#'
#' @param dat dataframe with predictor variables
#'
#' @return
#' @export
#'

make_correlogram_vars <- function(dat){

  #need to load library for pnael.cor tp work
  library(corrgram)
  cor <- corrgram::corrgram(dat[,c("logPP","logCHL","logSST","SST_sd", "HDI_mean","logNGO","Voice_mean","distSeamounts",
                            "MarineEcosystemDependency","logBathy","logDistC","logDistSM","GovernmentEffectiveness_mean",
                            "logDistP","logSAU","logconflicts","logDistCR","logTTM","logTTP")],
                     lower.panel=panel.cor, upper.panel=panel.shade)

  cor

  return(cor)

}









#' fit gls models with betaslope as response variable and bruvs as factor of all covariates
#' with no autocorrelation  **ATTENTION need to update with new variables
#'
#' @param data dataframe with predictor variables
#'
#' @return
#' @export
#'

fit_gls_no_cor_betaslope <- function(data){

  #no autocorrelation
  print("fitting gls with formula : betaslope ~ bruvs*(logBathy + Slope + logDistP + logDistSM + GovernmentEffectiveness_mean)")
  mod <- nlme::gls(betaslope ~ bruvs*(logBathy + Slope + logDistP + logDistSM + GovernmentEffectiveness_mean), data = data)

  #get aic
  a <- AIC(mod)
  print(a)

  #return model
  print("returning model")
  return(mod)

}









#' compare gls models with betaslope as response variable and bruvs as factor of all covariates
#' with different basic autocorrelation structures (form=~1)  **ATTENTION need to update with new variables
#'
#' @param data dataframe with predictor variables
#' 
#' @return
#' @export
#'

compare_gls_basic_cor_betaslope <- function(data){

  #basic autocorrelation based on order of the observations in the data as a covariate, and no groups.
  print("fitting gls with formula : betaslope ~ bruvs*(logBathy + Slope + logDistP + logDistSM + GovernmentEffectiveness_mean)")
  mod1 <- nlme::gls(betaslope ~ bruvs*(logBathy + Slope + logDistP + logDistSM + GovernmentEffectiveness_mean), data = data, correlation = nlme::corAR1(form=~1))
  mod2 <- nlme::gls(betaslope ~ bruvs*(logBathy + Slope + logDistP + logDistSM + GovernmentEffectiveness_mean), data = data, correlation = nlme::corExp(form=~1))
  mod3 <- nlme::gls(betaslope ~ bruvs*(logBathy + Slope + logDistP + logDistSM + GovernmentEffectiveness_mean), data = data, correlation = nlme::corGaus(form=~1))
  mod4 <- nlme::gls(betaslope ~ bruvs*(logBathy + Slope + logDistP + logDistSM + GovernmentEffectiveness_mean), data = data, correlation = nlme::corLin(form=~1))
  mod5 <- nlme::gls(betaslope ~ bruvs*(logBathy + Slope + logDistP + logDistSM + GovernmentEffectiveness_mean), data = data, correlation = nlme::corRatio(form=~1))
  mod6 <- nlme::gls(betaslope ~ bruvs*(logBathy + Slope + logDistP + logDistSM + GovernmentEffectiveness_mean), data = data, correlation = nlme::corSpher(form=~1))

  #get aic for all models
  a <- AIC(mod1, mod2, mod3, mod4, mod5, mod6)
  print(a)

  #get model with lowest aic
  m <- which.min(a$AIC)
  bestmod <- paste0("mod", m)
  if (bestmod == "mod1") print("best model is the one with corAR1")
  if (bestmod == "mod2") print("best model is the one with corExp")
  if (bestmod == "mod3") print("best model is the one with corGaus")
  if (bestmod == "mod4") print("best model is the one with corLin")
  if (bestmod == "mod5") print("best model is the one with corRatio")
  if (bestmod == "mod6") print("best model is the one with corSpher")

  #return best model
  print("returning best model")
  return(get(paste(bestmod)))

}





#' compare gls models with betaslope as response variable and bruvs as factor of all covariates
#' with different spatial autocorrelation structures (form=~long+lat)  **ATTENTION need to update with new variables
#'
#' @param data dataframe with predictor variables
#'
#' @return
#' @export
#'

compare_gls_spatial_cor_betaslope <- function(data){

  #basic autocorrelation based on order of the observations in the data as a covariate, and no groups.
  print("fitting gls with formula : betaslope ~ bruvs*(logBathy + Slope + logDistP + logDistSM + GovernmentEffectiveness_mean)")

  mod1 <- nlme::gls(betaslope ~ bruvs*(logBathy + Slope + logDistP + logDistSM + GovernmentEffectiveness_mean), data = data, correlation = nlme::corAR1(form=~mean_long+mean_lat))
  mod2 <- nlme::gls(betaslope ~ bruvs*(logBathy + Slope + logDistP + logDistSM + GovernmentEffectiveness_mean), data = data, correlation = nlme::corExp(form=~mean_long+mean_lat))
  mod3 <- nlme::gls(betaslope ~ bruvs*(logBathy + Slope + logDistP + logDistSM + GovernmentEffectiveness_mean), data = data, correlation = nlme::corGaus(form=~mean_long+mean_lat))
  mod4 <- nlme::gls(betaslope ~ bruvs*(logBathy + Slope + logDistP + logDistSM + GovernmentEffectiveness_mean), data = data, correlation = nlme::corLin(form=~mean_long+mean_lat))
  mod5 <- nlme::gls(betaslope ~ bruvs*(logBathy + Slope + logDistP + logDistSM + GovernmentEffectiveness_mean), data = data, correlation = nlme::corRatio(form=~mean_long+mean_lat))
  mod6 <- nlme::gls(betaslope ~ bruvs*(logBathy + Slope + logDistP + logDistSM + GovernmentEffectiveness_mean), data = data, correlation = nlme::corSpher(form=~mean_long+mean_lat))
  
  #get aic for all models
  a <- AIC(mod1, mod2, mod3, mod4, mod5, mod6)
  print(a)

  #get model with lowest aic
  m <- which.min(a$AIC)
  bestmod <- paste0("mod", m)
  if (bestmod == "mod1") print("best model is the one with corAR1")
  if (bestmod == "mod2") print("best model is the one with corExp")
  if (bestmod == "mod3") print("best model is the one with corGaus")
  if (bestmod == "mod4") print("best model is the one with corLin")
  if (bestmod == "mod5") print("best model is the one with corRatio")
  if (bestmod == "mod6") print("best model is the one with corSpher")

  #return best model
  print("returning best model")
  return(get(paste(bestmod)))

}







#' get diagnostic plots for gls model
#'
#' @param model gls model
#' @param model_name name of model
#' @param dat dataframe with predictor variables
#'
#' @return
#' @export
#'

get_gls_diagnostics <- function(dat, model, model_name){

  #residuals vs fitted
  png(here::here("outputs", "model_outputs", paste0(model_name, "_resid_vs_fitted", ".png")))
  plot(residuals(model, type = "normalized") ~ fitted(model))
  dev.off()

  #variograms
  png(here::here("outputs",  "model_outputs", paste0(model_name, "_variogram_longlat", ".png")))
  print(plot(nlme:::Variogram(model, form = ~ mean_long+mean_lat, resType = "normalized", data = dat)))
  dev.off()

  png(here::here("outputs",  "model_outputs", paste0(model_name, "_variogram", ".png"))) ##attention not sur this one makes sense
  print(plot(nlme:::Variogram(model, form = ~1, resType = "normalized", data = dat)))
  dev.off()

  #autocorrelation plot
  png(here::here("outputs",  "model_outputs", paste0(model_name, "_acf", ".png")))
  acf(model$residuals)
  dev.off()

}






#' get partial plots for given predictor of gls model  **ATTENTION need to update with new variables
#'
#' @param dat dataframe with predictor variables
#' @param model gls model
#' @param model_name charecter name of gls model
#' @param vars vector of predictor varibles
#'
#' @return
#' @export
#'

get_partial_plot <- function(dat, model, model_name, vars){

  plot <- list()

  #loop on model variables
  for (i in 1:length(vars)){

    var_to_plot <- vars[i]
    other_vars <- vars[!vars %in% var_to_plot]

    #get means for other predictors and replace in new dataframe for prediction
    datnew <- dat
     
    # if("protection_use" %in% other_vars){
    #   other_vars <- other_vars[other_vars!="protection_use"]
    # }
    
    means <- apply(datnew[, other_vars], MARGIN = 2, FUN = function(x) mean(x, na.rm = TRUE))
    
    datnew[, other_vars] <- rep(means, each = nrow(datnew))
    
    # if (!vars[i]== "protection_use"){
    #   datnew$protection_use <- "out"
    # }
    #get predictions and standard errors of predictions on new data
    pred <- AICcmodavg::predictSE.gls(model, datnew, se.fit=T)
    fit <- pred$fit
    se.fit <- pred$se.fit

    #add fits and se to new data
    datnew <- cbind(datnew, fit, se.fit)
    datnew$upper = datnew$fit + 2* datnew$se.fit
    datnew$lower = datnew$fit - 2* datnew$se.fit
    datnew$var_to_plot = datnew[,var_to_plot]

    #get data subsets
    dat_pel <- subset(datnew, bruvs == "pelagic")
    dat_ben <- subset(datnew, bruvs == "benthic")

    #partial plot with color-coded factor representing pelagic/benthic

    if (var_to_plot %in% c("logBathy", "logDistP", "logDistSM", "logSST", "logCHL", "logPP", "logDistCR", "logTTM")){

      #log transformation
      plot[[i]] <- ggplot2::ggplot() +
        # pelagic
        ggplot2::geom_line(data = dat_pel, ggplot2::aes(x = 10^var_to_plot, y = fit, colour='#077DAA')) + #predictions
        ggplot2::geom_ribbon(data = dat_pel, ggplot2::aes(x = 10^var_to_plot,
                                        ymin = lower,
                                        ymax = upper,
                                        fill='#077DAA'),#95% confidence intervals
                    alpha=0.5, show.legend=T) +

        # benthic
        ggplot2::geom_line(data = dat_ben, ggplot2::aes(x = 10^var_to_plot, y = fit, colour='orange')) + #predictions
        ggplot2::geom_ribbon(data = dat_ben, ggplot2::aes(x = 10^var_to_plot,
                                        ymin = lower,
                                        ymax = upper,
                                        fill='orange'),#95% confidence intervals
                    alpha=0.5,  show.legend=T) +
        ggplot2::labs(y = "fit", x = substring(var_to_plot, 4))+
        ggplot2::scale_colour_manual(name = "", values = c('#077DAA', 'orange'),
                            labels = c("pelagic", "benthic"), aesthetics = c("colour", "fill"))+
        ggplot2::theme_classic()+
        ggplot2::theme(legend.position = 'none')

    }else{
      
      
      if(var_to_plot=="protection_use"){
        
        plot[[i]] <- ggplot2::ggplot() +
          # pelagic
          
          ggplot2::geom_point(data = dat_pel, ggplot2::aes(x = var_to_plot, y = fit, colour='#077DAA')) + #predictions
          ggplot2::geom_errorbar(data = dat_pel, ggplot2::aes(x= var_to_plot, ymin=lower, ymax=upper,colour='#077DAA')) +#95% confidence intervals
          
          
          # benthic
          ggplot2::geom_point(data = dat_ben, ggplot2::aes(x = var_to_plot, y = fit, colour='orange')) + #predictions
          ggplot2::geom_errorbar(data = dat_ben, ggplot2::aes(x= var_to_plot, ymin=lower, ymax=upper,colour='orange')) +#95% confidence intervals
          
          ggplot2::labs(y = "fit", x = var_to_plot)+
          ggplot2::scale_colour_manual(name = "", values = c('#077DAA', 'orange'),
                                       labels = c("pelagic", "benthic"), aesthetics = c("colour", "fill"))+
          ggplot2::theme_classic()+
          ggplot2::theme(legend.position = 'none') +
          ggplot2::scale_x_continuous(breaks = c(1, 2, 3), labels = c("no_take", "out", "partly_protected"))
        
      }else{
      
     
        plot[[i]] <- ggplot2::ggplot() +
          # pelagic
          ggplot2::geom_line(data = dat_pel, ggplot2::aes(x = var_to_plot, y = fit, colour='#077DAA')) + #predictions
          ggplot2::geom_ribbon(data = dat_pel, ggplot2::aes(x = var_to_plot,
                                          ymin = lower,
                                          ymax = upper,
                                          fill='#077DAA'),#95% confidence intervals
                      alpha=0.5, show.legend=T) +
  
          # benthic
          ggplot2::geom_line(data = dat_ben, ggplot2::aes(x = var_to_plot, y = fit, colour='orange')) + #predictions
          ggplot2::geom_ribbon(data = dat_ben, ggplot2::aes(x = var_to_plot,
                                          ymin = lower,
                                          ymax = upper,
                                          fill='orange'),#95% confidence intervals
                      alpha=0.5,  show.legend=T) +
          ggplot2::labs(y = "fit", x = var_to_plot)+
          ggplot2::scale_colour_manual(name = "", values = c('#077DAA', 'orange'),
                              labels = c("pelagic", "benthic"), aesthetics = c("colour", "fill"))+
          ggplot2::theme_classic()+
          ggplot2::theme(legend.position = 'none')

      }
    }

  }

  #save multiplot
  png(here::here("outputs",  "model_outputs", paste0("partial_plot_", model_name, ".png")), width = 960, height = 960)
  Rmisc::multiplot(plotlist = plot, cols = 2)
  dev.off()

}

#' get coefficient plot for model 
#'
#' @param model gls
#' @param model_name 
#' @import ggplot2
#' @import sjPlot sjlabelled sjmisc
#' 
#' @return
#' @export
#'

coef_plot <- function(model, model_name){
  
  mod_coef <- plot_model(model, sort.est = TRUE, show.values = TRUE, value.offset = .3, type = "std2")
  
  print(mod_coef)
  
  ggsave(mod_coef, filename = here::here("outputs", "model_outputs", paste0(model_name, "_coef_plot", ".png")), width = 10, height = 15, units = "in", dpi =300)
  
  invisible(mod_coef)
}


#' get coefficient plot for model for selection terms
#'
#' @param model gls
#' @param model_name 
#' @import ggplot2
#' @import sjPlot sjlabelled sjmisc
#' 
#' @return
#' @export
#'

coef_plot_terms <- function(model, model_name){
  
  mod_coef <- plot_model(model, sort.est = TRUE, show.values = TRUE, value.offset = .3)
  
  print(mod_coef)
  
  ggsave(mod_coef, filename = here::here("outputs", "model_outputs", paste0(model_name, "_coef_plot_terms", ".png")), width = 10, height = 15, units = "in", dpi =300)
  
  invisible(mod_coef)
}



#' get adjusted R2 for model
#'
#' @param model gls
#' @param model_name 
#' @import performance
#
#' @return
#' @export
#'


get_adj_r2 <- function(model){
  
  
  model_perf <- model_performance(model)
  
  return(model_perf)
  
}





#function adapted from https://github.com/cran/MASS/blob/master/R/stepAIC.R

#' Title
#'
#' @param object 
#' @param scope 
#' @param scale 
#' @param direction 
#' @param trace 
#' @param keep 
#' @param steps 
#' @param use.start 
#' @param k 
#' @param ... 
#'
#' @return
#' @export
#'
stepAIC_mod <-
  function(object, scope, scale = 0,
           direction = c("both", "backward", "forward"),
           trace = 1, keep = NULL, steps = 1000, use.start = FALSE, k = 2, ...)
  {
    mydeviance <- function(x, ...)
    {
      dev <- deviance(x)
      if(!is.null(dev)) dev else extractAIC(x, k=0)[2L]
    }
    
    cut.string <- function(string)
    {
      if(length(string) > 1L)
        string[-1L] <- paste("\n", string[-1L], sep = "")
      string
    }
    
    re.arrange <- function(keep)
    {
      namr <- names(k1 <- keep[[1L]])
      namc <- names(keep)
      nc <- length(keep)
      nr <- length(k1)
      array(unlist(keep, recursive = FALSE), c(nr, nc), list(namr, namc))
    }
    
    step.results <- function(models, fit, object, usingCp=FALSE)
    {
      change <- sapply(models, "[[", "change")
      rd <- sapply(models, "[[", "deviance")
      dd <- c(NA, abs(diff(rd)))
      rdf <- sapply(models, "[[", "df.resid")
      ddf <- c(NA, abs(diff(rdf)))
      AIC <- sapply(models, "[[", "AIC")
      heading <- c("Stepwise Model Path \nAnalysis of Deviance Table",
                   "\nInitial Model:", deparse(formula(object)),
                   "\nFinal Model:", deparse(formula(fit)),
                   "\n")
      aod <-
        if(usingCp)
          data.frame(Step = change, Df = ddf, Deviance = dd,
                     "Resid. Df" = rdf, "Resid. Dev" = rd,
                     Cp = AIC, check.names = FALSE)
      else data.frame(Step = change, Df = ddf, Deviance = dd,
                      "Resid. Df" = rdf, "Resid. Dev" = rd,
                      AIC = AIC, check.names = FALSE)
      attr(aod, "heading") <- heading
      class(aod) <- c("Anova", "data.frame")
      fit$anova <- aod
      fit
    }
    
    Terms <- terms(object)
    object$formula <- Terms
    if(inherits(object, "lme"))  object$call$fixed <- Terms
    else if(inherits(object, "gls")) object$call$model <- Terms
    else object$call$formula <- Terms
    if(use.start) warning("'use.start' cannot be used with R's version of 'glm'")
    md <- missing(direction)
    direction <- match.arg(direction)
    backward <- direction == "both" | direction == "backward"
    forward <- direction == "both" | direction == "forward"
    if(missing(scope)) {
      fdrop <- numeric()
      fadd <- attr(Terms, "factors")
      if(md) forward <- FALSE
    } else {
      if(is.list(scope)) {
        fdrop <- if(!is.null(fdrop <- scope$lower))
          attr(terms(update.formula(object, fdrop)), "factors")
        else numeric()
        fadd <- if(!is.null(fadd <- scope$upper))
          attr(terms(update.formula(object, fadd)), "factors")
      } else {
        fadd <- if(!is.null(fadd <- scope))
          attr(terms(update.formula(object, scope)), "factors")
        fdrop <- numeric()
      }
    }
    models <- vector("list", steps)
    if(!is.null(keep)) keep.list <- vector("list", steps)
    n <- nobs(object, use.fallback = TRUE)  # might be NA
    fit <- object
    bAIC <- extractAIC(fit, scale, k = k, ...)
    edf <- bAIC[1L]
    bAIC <- bAIC[2L]
    if(is.na(bAIC))
      stop("AIC is not defined for this model, so 'stepAIC' cannot proceed")
    if(bAIC == -Inf)
      stop("AIC is -infinity for this model, so 'stepAIC' cannot proceed")
    nm <- 1
    Terms <- terms(fit)
    if(trace) {
      cat("Start:  AIC=", format(round(bAIC, 2)), "\n",
          cut.string(deparse(formula(fit))), "\n\n", sep='')
      utils::flush.console()
    }
    models[[nm]] <- list(deviance = mydeviance(fit), df.resid = n - edf,
                         change = "", AIC = bAIC)
    if(!is.null(keep)) keep.list[[nm]] <- keep(fit, bAIC)
    usingCp <- FALSE
    while(steps > 0) {
      steps <- steps - 1
      AIC <- bAIC
      ffac <- attr(Terms, "factors")
      ## don't drop strata terms
      if(!is.null(sp <- attr(Terms, "specials")) &&
         !is.null(st <- sp$strata)) ffac <- ffac[-st,]
      scope <- factor.scope(ffac, list(add = fadd, drop = fdrop))
      aod <- NULL
      change <- NULL
      if(backward && length(scope$drop)) {
        aod <- dropterm(fit, scope$drop, scale = scale,
                        trace = max(0, trace - 1), k = k, ...)
        rn <- row.names(aod)
        row.names(aod) <- c(rn[1L], paste("-", rn[-1L], sep=" "))
        ## drop all zero df terms first.
        if(any(aod$Df == 0, na.rm=TRUE)) {
          zdf <- aod$Df == 0 & !is.na(aod$Df)
          nc <- match(c("Cp", "AIC"), names(aod))
          nc <- nc[!is.na(nc)][1L]
          ch <- abs(aod[zdf, nc] - aod[1, nc]) > 0.01
          if(any(is.finite(ch) & ch)) {
            warning("0 df terms are changing AIC")
            zdf <- zdf[!ch]
          }
          ## drop zero df terms first: one at time since they
          ## may mask each other
          if(length(zdf) > 0L)
            change <- rev(rownames(aod)[zdf])[1L]
        }
      }
      if(is.null(change)) {
        if(forward && length(scope$add)) {
          aodf <- addterm(fit, scope$add, scale = scale,
                          trace = max(0, trace - 1), k = k, ...)
          rn <- row.names(aodf)
          row.names(aodf) <- c(rn[1L], paste("+", rn[-1L], sep=" "))
          aod <-
            if(is.null(aod)) aodf
          else rbind(aod, aodf[-1, , drop=FALSE])
        }
        attr(aod, "heading") <- NULL
        if(is.null(aod) || ncol(aod) == 0) break
        ## need to remove any terms with zero df from consideration
        nzdf <- if(!is.null(aod$Df)) aod$Df != 0 | is.na(aod$Df)
        aod <- aod[nzdf, ]
        if(is.null(aod) || ncol(aod) == 0) break
        nc <- match(c("Cp", "AIC"), names(aod))
        nc <- nc[!is.na(nc)][1L]
        o <- order(aod[, nc])
        if(trace) {
          print(aod[o,  ])
          utils::flush.console()
        }
        if(o[1L] == 1) break
        change <- rownames(aod)[o[1L]]
      }
      usingCp <- match("Cp", names(aod), 0) > 0
      ## may need to look for a 'data' argument in parent
      fit <- update(fit, paste("~ .", change), evaluate = FALSE)
      fit <- eval.parent(fit)
      nnew <- nobs(fit, use.fallback = TRUE)
      if(all(is.finite(c(n, nnew))) && nnew != n)
        stop("number of rows in use has changed: remove missing values?")
      Terms <- terms(fit)
      bAIC <- extractAIC(fit, scale, k = k, ...)
      edf <- bAIC[1L]
      bAIC <- bAIC[2L]
      if(trace) {
        cat("\nStep:  AIC=", format(round(bAIC, 2)), "\n",
            cut.string(deparse(formula(fit))), "\n\n", sep='')
        utils::flush.console()
      }
      ## add a tolerance as dropping 0-df terms might increase AIC slightly
      if(bAIC >= AIC + 1e-7) break
      nm <- nm + 1
      models[[nm]] <-
        list(deviance = mydeviance(fit), df.resid = n - edf,
             change = change, AIC = bAIC)
      if(!is.null(keep)) keep.list[[nm]] <- keep(fit, bAIC)
    }
    if(!is.null(keep)) fit$keep <- re.arrange(keep.list[seq(nm)])
    step.results(models = models[seq(nm)], fit, object, usingCp)
  }

extractAIC.loglm <- function(fit, scale, k = 2, ...)
{
  edf <- fit$n - fit$df
  c(edf,  fit$deviance + k * edf)
}

## defer to nlme
if(FALSE) {
  extractAIC.lme <- function(fit, scale, k = 2, ...)
  {
    if(fit$method != "ML") stop("AIC undefined for REML fit")
    res <- logLik(fit)
    edf <- attr(res, "df")
    c(edf,  -2*res + k * edf)
  }
  
  extractAIC.gls <- function(fit, scale, k = 2, ...)
  {
    if(fit$method != "ML") stop("AIC undefined for REML fit")
    res <- logLik(fit)
    edf <- attr(res, "df")
    c(edf,  -2*res + k * edf)
  }
  
  terms.gls <- terms.lme <- function(x, ...) terms(formula(x), ...)
}


#' marginal plots for the empirical covariate range of logged variable
#'
#' @param dat dataframe with covariates
#' @param mod model of interest dataframe with predictor variables
#' @param var_name name of covariate of interest
#' @return
#' @import ggeffects ggplot2 
#' @export
#'

marg_plot_log <- function(response, mod_name, dat, mod, var, var_name, group, condition){

  #function
  var_to_plot <- dat[var_name]
  group_var <- dat[group]

pred <- as.data.frame(ggeffects::ggpredict(mod, terms= c(var, group), condition = condition)) 

#select empirical range of covariate

min(var_to_plot[group_var=="pelagic"]) -> min_var_pel 
max(var_to_plot[group_var=="pelagic"]) -> max_var_pel 
min(var_to_plot[group_var=="benthic"]) -> min_var_ben
max(var_to_plot[group_var=="benthic"]) -> max_var_ben

#filter predictions to empirical range
pred %>%    
  dplyr::mutate(x = dplyr::case_when(group=="pelagic" ~ replace(x, x<min_var_pel | x>max_var_pel, NA),
                                     group=="benthic" ~ replace(x, x<min_var_ben | x>max_var_ben, NA))) -> pred
pred  %>%
  tidyr::drop_na(x) -> pred

min.rang <- min(pred$conf.low)
max.rang <- max(pred$conf.high)


#plot marginal plot
marg <- ggplot(pred, aes(x, predicted, group=group, colour = group, fill=group)) +
  geom_line() + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .3, linetype=0) +
   scale_colour_manual(name = "", values = c('orange','#077DAA'), aesthetics = c("colour", "fill"))+
  theme_light()+ labs(x = var_name, y = response)+ ylim(min.rang, max.rang)

print(marg)

ggsave(marg, filename = here::here("outputs", "model_outputs", paste0(mod_name, "_marg_plot_", var_name,".png")), width = 6, height = 6, units = "in", dpi =300)

invisible(marg)

}
  
  
#' marginal plots for the empirical covariate range of non_logged variable
#'
#' @param dat dataframe with covariates
#' @param mod model of interest dataframe with predictor variables
#' @param var_name name of covariate of interest
#' @return
#' @import ggeffects ggplot2 
#' @export
#'

marg_plot <- function(response, mod_name, dat, mod, var, var_name, group){
  
  #function
  var_to_plot <- dat[var_name]
  group_var <- dat[group]
  
  pred <- as.data.frame(ggeffects::ggpredict(mod, terms= c(var, group), condition = c(logBathy = 1.5, logDistCR = 4.5, logSST = 1.4, logDistP = 1.7, logCHL=.10, SST_sd = 0.7, GovernmentEffectiveness_mean = 1, protection_use ="not_protected"))) 
  
  #select empirical range of covariate
  
  min(var_to_plot[group_var=="pelagic"]) -> min_var_pel 
  max(var_to_plot[group_var=="pelagic"]) -> max_var_pel 
  min(var_to_plot[group_var=="benthic"]) -> min_var_ben
  max(var_to_plot[group_var=="benthic"]) -> max_var_ben
  
  #filter predictions to empirical range
  pred %>%    
    dplyr::mutate(x = dplyr::case_when(group=="pelagic" ~ replace(x, x<min_var_pel | x>max_var_pel, NA),
                                       group=="benthic" ~ replace(x, x<min_var_ben | x>max_var_ben, NA))) -> pred
  pred  %>%
    tidyr::drop_na(x) -> pred
  #plot marginal plot
  marg <- ggplot(pred, aes(x, predicted, group=group, colour = group, fill=group)) +
    geom_line() + 
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .3, linetype=0) +
    scale_colour_manual(name = "", values = c('orange','#077DAA'), aesthetics = c("colour", "fill"))+
    theme_light()+ labs(x = var_name, y = response)
  
  print(marg)
  
  ggsave(marg, filename = here::here("outputs", "model_outputs", paste0(mod_name, "_marg_plot_", var_name,".png")), width = 6, height = 6, units = "in", dpi =300)
  
  invisible(marg)
  
}


#' marginal plots for categorical co_variates
#'
#' @param dat dataframe with covariates
#' @param mod model of interest dataframe with predictor variables
#' @param var_name name of covariate of interest
#' @return
#' @import ggeffects ggplot2 
#' @export
#'

marg_plot_cat_covar <- function(response, mod_name, dat, mod, var, var_name, group, group2, condition){
  # response = "beta_slope"
  # mod_name = "mod_sim_betaslope_re"
  # dat = tab_betaslope
  # mod= mod_sim_betaslope_re
  # var = "GovernmentEffectiveness_mean [all]"
  # var_name = "GovernmentEffectiveness_mean" 
  # group = "bruvs"
  # group2 = "protection_use"
  

  #function
  var_to_plot <- dat[var_name]
  group_var <- dat[group]
  group_var2 <-dat[group2]
  
  pred <- as.data.frame(ggeffects::ggpredict(mod, terms= c(var, group, group2), condition= condition)) #between 40 and 469 meter depth"logBathy [1.35:1.85]"
  
  #select empirical range of covariate bruvs
  
  min(var_to_plot[group_var=="pelagic"]) -> min_var_pel 
  max(var_to_plot[group_var=="pelagic"]) -> max_var_pel 
  min(var_to_plot[group_var=="benthic"]) -> min_var_ben
  max(var_to_plot[group_var=="benthic"]) -> max_var_ben
  
  #filter predictions to empirical range of bruvs
  pred %>%    
    dplyr::mutate(x = dplyr::case_when(group=="pelagic" ~ replace(x, x<min_var_pel | x>max_var_pel, NA),
                                       group=="benthic" ~ replace(x, x<min_var_ben | x>max_var_ben, NA))) -> pred
  
  #select empirical range of covariate
# 
#   min(var_to_plot[group_var2=="no_take"]) -> min_var_notake
#   max(var_to_plot[group_var2=="no_take"]) -> max_var_notake
#   min(var_to_plot[group_var2=="partly_protected"]) -> min_var_part
#   max(var_to_plot[group_var2=="partly_protected"]) -> max_var_part
#   min(var_to_plot[group_var2=="not_protected"]) -> min_var_not
#   max(var_to_plot[group_var2=="not_protected"]) -> max_var_not
# 
#   #filter predictions to empirical range of protection_use
#   pred %>%
#     dplyr::mutate(x = dplyr::case_when(group2=="no_take" ~ replace(x, x<min_var_notake | x>max_var_notake, NA),
#                                        group2=="partly_protected" ~ replace(x, x<min_var_part | x>max_var_part, NA),
#                                        group2=="not_protected" ~ replace(x, x<min_var_not | x>max_var_not, NA), )) -> pred
# 
#   
  #plot marginal plot
  pred  %>%
    tidyr::drop_na(x) -> pred
  
  marg <- ggplot(pred, aes(x, predicted, group=group, colour = group, fill=group)) +
    geom_line() + 
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .3, linetype=0) +
    scale_colour_manual(name = "", values = c('orange','#077DAA'), aesthetics = c("colour", "fill"))+
    theme_light()+ labs(x = var_name, y = response) + facet_wrap(~facet)#+ylim(-5.5, 3.5)
  
  print(marg)
  
  ggsave(marg, filename = here::here("outputs", "model_outputs", paste0(mod_name, "_marg_plot_",group2,"_", var_name, ".png")), width = 10, height = 6, units = "in", dpi =300)
  
  invisible(marg)
  
}


#' marginal plots for of logged bathy variable
#'
#' @param dat dataframe with covariates
#' @param mod model of interest dataframe with predictor variables
#' @param var_name name of covariate of interest
#' @return
#' @import ggeffects ggplot2 ggforce
#' @export
#'

marg_plot_bathy <- function(response, mod_name, dat, mod, var, var_name, group){
  
  #function
  var_to_plot <- dat[var_name]
  group_var <- dat[group]
  
  pred <- as.data.frame(ggeffects::ggpredict(mod, terms= c(var, group),condition=c(logSST = 1.4, logDistP = 1.7, logCHL=.10, logDistC = 4, SST_sd = 0.7, logTTM = 3, GovernmentEffectiveness_mean = 1, logDistSM = 5))) 
  
  #select empirical range of covariate
  
  #min(var_to_plot[group_var=="pelagic"]) 
  1.17 -> min_var_pel #this is 15 meter depth. makes no sense to have deeper pelagics
  max(var_to_plot[group_var=="pelagic"]) -> max_var_pel 
  #min(var_to_plot[group_var=="benthic"])
  0.69 -> min_var_ben #this is 5 meter depth. makes no sense to have deeper benthic
  max(var_to_plot[group_var=="benthic"]) -> max_var_ben
  
  
  #filter predictions to empirical range
  pred %>%    
    dplyr::mutate(x = dplyr::case_when(group=="pelagic" ~ replace(x, x<min_var_pel | x>max_var_pel, NA),
                                       group=="benthic" ~ replace(x, x<min_var_ben | x>max_var_ben, NA))) -> pred
  pred  %>%
    tidyr::drop_na(x) -> pred
  
  min.rang <- min(pred$conf.low+1)
  max.rang <- max(pred$conf.high-1)
  
  
  #plot marginal plot
  marg <- ggplot(pred, aes(x, predicted, group=group, colour = group, fill=group)) +
    geom_line() + 
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .3, linetype=0) +
    scale_colour_manual(name = "", values = c('orange','#077DAA'), aesthetics = c("colour", "fill"))+
    theme_light()+ labs(x = "Bathymetry", y = response) +  coord_cartesian(ylim=c(min.rang,max.rang)) + facet_zoom(xlim = c(0, 2.5)) 
  
  print(marg)
  
  ggsave(marg, filename = here::here("outputs", "model_outputs", paste0(mod_name, "_marg_plot_bathy", var_name, ".png")), width = 6, height = 6, units = "in", dpi =300)
  
  invisible(marg)
  
}  

#' marginal plots for categorical co_variates
#'
#' @param dat dataframe with covariates
#' @param mod model of interest dataframe with predictor variables
#' @param var_name name of covariate of interest
#' @return
#' @import ggeffects ggplot2 
#' @export
#'

marg_plot_cat_catvar <- function(response, mod_name, dat, mod, var, var_name, group, condition){

  
  #function
  var_to_plot <- dat[var_name]
  group_var <- dat[group]

  # c(logBathy = 2.5, logSST = 1.4, logDistP = 1.7, logCHL=.10, logDistC = 4, SST_sd = 0.7, logTTM = 3.5, GovernmentEffectiveness_mean = 1.5)
  #pred <- as.data.frame(ggeffects::ggpredict(mod, terms= c(var, group), condition=c(logBathy = 2, CHL=.30, logDistC = 1.3, SST_sd = 1, TravelTime_market = 5000))) #between 40 and 469 meter depth"logBathy [1.35:1.85]"
  pred <- ggeffects::ggpredict(mod, terms= c(var, group), condition = condition) #between 40 and 469 meter depth"logBathy [1.35:1.85]"
  
  #select empirical range of covariate bruvs
  
  # min(var_to_plot[group_var=="pelagic"]) -> min_var_pel 
  # max(var_to_plot[group_var=="pelagic"]) -> max_var_pel 
  # min(var_to_plot[group_var=="benthic"]) -> min_var_ben
  # max(var_to_plot[group_var=="benthic"]) -> max_var_ben
  # 
  # #filter predictions to empirical range of bruvs
  # pred %>%    
  #   dplyr::mutate(x = dplyr::case_when(group=="pelagic" ~ replace(x, x<min_var_pel | x>max_var_pel, NA),
  #                                      group=="benthic" ~ replace(x, x<min_var_ben | x>max_var_ben, NA))) -> pred
  # 
  #   
  #plot marginal plot
  # pred  %>%
  #   tidyr::drop_na(x) -> pred
  
  # marg <- ggplot(pred, aes(y=predicted, group=x, colour = group, fill=group)) +
  #   geom_jitter() + 
  #   geom_pointrange(aes(ymin=conf.low, ymax=conf.high))+
  #   scale_colour_manual(name = "", values = c('orange','#077DAA'), aesthetics = c("colour", "fill")) +
  #   theme_light()+ labs(x = var_name, y = response) #+ facet_wrap(~facet)#+ylim(-5.5, 3.5)
  # 
  marg <- plot(pred)#+ylim(-10,10)
  marg <- marg + scale_colour_manual(name = "", values = c('orange','#077DAA'), aesthetics = c("colour", "fill"))+theme_light() 
  print(marg)
  
  ggsave(marg, filename = here::here("outputs", "model_outputs", paste0(mod_name,"_marg_plot_", group, "_", var_name ,".png")), width = 10, height = 6, units = "in", dpi =300)
  
  invisible(marg)
  
}


#' fit fully staturated gls models with betaslope as response variable and bruvs as factor of all covariates
#' with autocorrelation  **ATTENTION need to update with new variables
#'
#' @param data dataframe with predictor variables
#'
#' @return
#' @export
#'

fit_gls_sat_cor_betaslope <- function(data){
  
  
  #autocorrelation
  print("fitting gls with formula: betaslope ~ bruvs * protection_use  * (logBathy + logTTM + GovernmentEffectiveness_mean +logDistC + logDistP+ logDistCR+ Slope+
                                             poly(logSST,2)  + poly(logCHL,2) +logDistSM + poly(SST_sd,2))")
  
  mod <- nlme::gls(betaslope ~ bruvs * protection_use * (logBathy + logTTM + GovernmentEffectiveness_mean + logDistC+ logDistP + logDistCR + Slope +
                                             poly(logSST,2) + poly(logCHL,2) +logDistSM + poly(SST_sd,2)), data = tab_betaslope,  correlation = nlme::corRatio(form=~1),method='ML') 
  
  
  #get aic
  a <- AIC(mod)
  print(a)
  
  #return model
  print("returning model")
  return(mod)
  
}

#' fit simplified gls model with betaslope as response variable and bruvs as factor of all covariates
#' with autocorrelation  **ATTENTION need to update with new variables
#'
#' @param data dataframe with predictor variables
#'
#' @return
#' @export
#'

fit_gls_sim_cor_betaslope <- function(data){
  

  #autocorrelation
  print("fitting gls with formula: betaslope ~ bruvs + protection_use + logBathy + logTTM + poly(GovernmentEffectiveness_mean,      2) + logDistC + logDistP + logDistCR + Slope + poly(logSST,      2) + poly(logCHL, 2) + poly(SST_sd, 2) + bruvs:protection_use +      bruvs:logBathy + bruvs:logTTM + bruvs:poly(GovernmentEffectiveness_mean,      2) + bruvs:logDistC + bruvs:logDistP + bruvs:logDistCR +      bruvs:Slope + bruvs:poly(logSST, 2) + bruvs:poly(logCHL,      2) + bruvs:poly(SST_sd, 2) + protection_use:logBathy + protection_use:logTTM +      protection_use:poly(GovernmentEffectiveness_mean, 2) + protection_use:logDistC +      protection_use:logDistP + protection_use:logDistCR + protection_use:Slope +      protection_use:poly(logCHL, 2) + bruvs:protection_use:logBathy +      bruvs:protection_use:logTTM + bruvs:protection_use:logDistC +      bruvs:protection_use:logDistP + bruvs:protection_use:logDistCR +      bruvs:protection_use:Slope + bruvs:protection_use:poly(logCHL,      2)")
  
  #this one is good and works
  #mod <- nlme::gls(betaslope ~ bruvs + protection_use + logBathy + logTTM + poly(GovernmentEffectiveness_mean,      2) + logDistC + logDistP + logDistCR + Slope + poly(logSST,      2) + poly(logCHL, 2) + poly(SST_sd, 2) + bruvs:protection_use +      bruvs:logBathy + bruvs:logTTM + bruvs:poly(GovernmentEffectiveness_mean,      2) + bruvs:logDistC + bruvs:logDistP + bruvs:logDistCR +      bruvs:Slope + bruvs:poly(logSST, 2) + bruvs:poly(logCHL,      2) + bruvs:poly(SST_sd, 2) + protection_use:logBathy + protection_use:logTTM +      protection_use:poly(GovernmentEffectiveness_mean, 2) + protection_use:logDistC +      protection_use:logDistP + protection_use:logDistCR + protection_use:Slope +      protection_use:poly(logCHL, 2) + bruvs:protection_use:logBathy +      bruvs:protection_use:logTTM + bruvs:protection_use:logDistC +      bruvs:protection_use:logDistP + bruvs:protection_use:logDistCR +      bruvs:protection_use:Slope + bruvs:protection_use:poly(logCHL,      2), data =tab_betaslope,  correlation = nlme::corRatio(form=~1),method='ML') 
  
  mod <- nlme::gls(betaslope ~ bruvs + protection_use + logBathy + logTTM + GovernmentEffectiveness_mean + 
                     logDistC + logDistP + logDistCR + Slope + poly(logSST, 2) + 
                     poly(logCHL, 2) + poly(SST_sd, 2) + bruvs:protection_use + 
                     bruvs:logBathy + bruvs:logTTM + bruvs:GovernmentEffectiveness_mean + 
                     bruvs:logDistC + bruvs:logDistP + bruvs:logDistCR + bruvs:Slope + 
                     bruvs:poly(logSST, 2) + bruvs:poly(logCHL, 2) + bruvs:poly(SST_sd, 
                                                                                2) + protection_use:logBathy + protection_use:logTTM + protection_use:GovernmentEffectiveness_mean + 
                     protection_use:logDistC + protection_use:logDistP + protection_use:logDistCR + 
                     protection_use:Slope + protection_use:poly(logCHL, 2) + bruvs:protection_use:logBathy + 
                     bruvs:protection_use:logTTM + bruvs:protection_use:GovernmentEffectiveness_mean + 
                     bruvs:protection_use:logDistC + bruvs:protection_use:logDistP + 
                     bruvs:protection_use:logDistCR + bruvs:protection_use:Slope + 
                     bruvs:protection_use:poly(logCHL, 2), data =tab_betaslope,  correlation = nlme::corRatio(form=~1),method='ML')
  
  #get aic
  a <- AIC(mod)
  print(a)
  
  #return model
  print("returning model")
  return(mod)
  
}




#' fit fully staturated gls models with median_maxSize as response variable and bruvs as factor of all covariates
#' with autocorrelation  **ATTENTION need to update with new variables
#'
#' @param data dataframe with predictor variables
#'
#' @return
#' @export
#'

fit_gls_sat_cor_median_maxsize <- function(data){
  
  data = tab_median_maxsize
  
  #autocorrelation
  print("fitting gls with formula: median_maxsize ~ bruvs * protection_use  * (logBathy + logTTM+poly(GovernmentEffectiveness_mean,2)+logDistC + logDistP+ logDistCR+ Slope+
                                             poly(logSST,2)  + poly(logCHL,2) +logDistSM + poly(SST_sd,2))")
  
  mod <- nlme::gls(median_maxsize ~ bruvs * protection_use * (logBathy+logTTM + poly(GovernmentEffectiveness_mean,2) + logDistC+ logDistP+ logDistCR + Slope +
                                                           poly(logSST,2)  + poly(logCHL,2) +logDistSM + poly(SST_sd,2)), data = tab_median_maxsize,  correlation = nlme::corRatio(form=~1),method='ML') 
  
  
  #get aic
  a <- AIC(mod)
  print(a)
  
  #return model
  print("returning model")
  return(mod)
  
}

#' fit simplified gls models with median_maxSize as response variable and bruvs as factor of all covariates
#' with autocorrelation 
#'
#' @param data dataframe with predictor variables
#'
#' @return
#' @export
#'

fit_gls_sim_cor_median_maxsize <- function(data){
  
  data = tab_median_maxsize
  
  #autocorrelation
  print("fitting gls with formula: median_maxsize ~ bruvs + protection_use + logBathy + logTTM + 
    poly(GovernmentEffectiveness_mean, 2) + logDistP + logDistCR + 
    Slope + poly(logSST, 2) + poly(logCHL, 2) + logDistSM + poly(SST_sd, 
    2) + bruvs:protection_use + bruvs:logBathy + bruvs:logTTM + 
    bruvs:poly(GovernmentEffectiveness_mean, 2) + bruvs:logDistP + 
    bruvs:logDistCR + bruvs:Slope + bruvs:poly(logSST, 2) + bruvs:poly(logCHL, 
    2) + bruvs:logDistSM + protection_use:logBathy + protection_use:logTTM + 
    protection_use:poly(GovernmentEffectiveness_mean, 2) + protection_use:logDistP + 
    protection_use:logDistCR + protection_use:Slope + protection_use:poly(logSST, 
    2) + protection_use:poly(logCHL, 2) + protection_use:logDistSM + 
    bruvs:protection_use:logBathy + bruvs:protection_use:logTTM + 
    bruvs:protection_use:poly(GovernmentEffectiveness_mean, 2) + 
    bruvs:protection_use:logDistP + bruvs:protection_use:logDistCR + 
    bruvs:protection_use:Slope + bruvs:protection_use:poly(logSST, 
    2) + bruvs:protection_use:poly(logCHL, 2) + bruvs:protection_use:logDistSM")
  
  mod <- nlme::gls(median_maxsize ~ bruvs + protection_use + logBathy + logTTM + 
                     poly(GovernmentEffectiveness_mean, 2) + logDistP + logDistCR + 
                     Slope + poly(logSST, 2) + poly(logCHL, 2) + logDistSM + poly(SST_sd, 
                                                                                  2) + bruvs:protection_use + bruvs:logBathy + bruvs:logTTM + 
                     bruvs:poly(GovernmentEffectiveness_mean, 2) + bruvs:logDistP + 
                     bruvs:logDistCR + bruvs:Slope + bruvs:poly(logSST, 2) + bruvs:poly(logCHL, 
                                                                                        2) + bruvs:logDistSM + protection_use:logBathy + protection_use:logTTM + 
                     protection_use:poly(GovernmentEffectiveness_mean, 2) + protection_use:logDistP + 
                     protection_use:logDistCR + protection_use:Slope + protection_use:poly(logSST, 
                                                                                           2) + protection_use:poly(logCHL, 2) + protection_use:logDistSM + 
                     bruvs:protection_use:logBathy + bruvs:protection_use:logTTM + 
                     bruvs:protection_use:poly(GovernmentEffectiveness_mean, 2) + 
                     bruvs:protection_use:logDistP + bruvs:protection_use:logDistCR + 
                     bruvs:protection_use:Slope + bruvs:protection_use:poly(logSST, 
                                                                            2) + bruvs:protection_use:poly(logCHL, 2) + bruvs:protection_use:logDistSM, data = tab_median_maxsize,  correlation = nlme::corRatio(form=~1),method='ML') 
  
  
  #get aic
  a <- AIC(mod)
  print(a)
  
  #return model
  print("returning model")
  return(mod)
  
}

#' fit fully saturated gls models with firstmode as response variable and bruvs as factor of all covariates
#' with autocorrelation  **ATTENTION need to update with new variables
#'
#' @param data dataframe with predictor variables
#'
#' @return
#' @export
#'

fit_gls_sat_cor_firstmode <- function(data){
  
  
  #autocorrelation
  print("fitting gls with formula: logFirstmode ~ bruvs * protection_use  * (logBathy + logTTM + GovernmentEffectiveness_mean +logDistC + logDistP+ logDistCR+ Slope+
                                             poly(logSST,2)  + poly(logCHL,2) +logDistSM + poly(SST_sd,2))")
  
  mod <- nlme::gls(logFirstmode ~ bruvs * protection_use * (logBathy + logTTM + GovernmentEffectiveness_mean + logDistC+ logDistP + logDistCR + Slope +
                                                           poly(logSST,2) + poly(logCHL,2) +logDistSM + poly(SST_sd,2)), data = tab_firstmode,  correlation = nlme::corRatio(form=~1),method='ML') 
  
  
  #get aic
  a <- AIC(mod)
  print(a)
  
  #return model
  print("returning model")
  return(mod)
  
}

#' fit fully saturated gls models with firstmode as response variable and bruvs as factor of all covariates
#' with autocorrelation  **ATTENTION need to update with new variables
#'
#' @param data dataframe with predictor variables
#'
#' @return
#' @export
#'

fit_gls_sim_cor_firstmode <- function(data){
  
  
  #autocorrelation
  print("fitting gls with formula: logFirstmode ~ bruvs + protection_use + logBathy + logTTM + GovernmentEffectiveness_mean + 
    logDistC + logDistP + Slope + poly(logSST, 2) + logDistSM + 
    poly(SST_sd, 2) + bruvs:protection_use + bruvs:logBathy + 
    bruvs:logTTM + bruvs:GovernmentEffectiveness_mean + bruvs:logDistP + 
    bruvs:Slope + bruvs:poly(logSST, 2) + bruvs:logDistSM + bruvs:poly(SST_sd, 
    2) + protection_use:logBathy + protection_use:logTTM + protection_use:GovernmentEffectiveness_mean + 
    protection_use:logDistP + protection_use:Slope + protection_use:poly(logSST, 
    2) + protection_use:logDistSM + protection_use:poly(SST_sd, 
    2) + bruvs:protection_use:logTTM + bruvs:protection_use:GovernmentEffectiveness_mean + 
    bruvs:protection_use:logDistP + bruvs:protection_use:Slope + 
    bruvs:protection_use:poly(logSST, 2) + bruvs:protection_use:logDistSM + 
    bruvs:protection_use:poly(SST_sd, 2)")
  
  mod <- nlme::gls(logFirstmode ~ bruvs + protection_use + logBathy + logTTM + GovernmentEffectiveness_mean + 
                     logDistC + logDistP + Slope + poly(logSST, 2) + logDistSM + 
                     poly(SST_sd, 2) + bruvs:protection_use + bruvs:logBathy + 
                     bruvs:logTTM + bruvs:GovernmentEffectiveness_mean + bruvs:logDistP + 
                     bruvs:Slope + bruvs:poly(logSST, 2) + bruvs:logDistSM + bruvs:poly(SST_sd, 
                                                                                        2) + protection_use:logBathy + protection_use:logTTM + protection_use:GovernmentEffectiveness_mean + 
                     protection_use:logDistP + protection_use:Slope + protection_use:poly(logSST, 
                                                                                          2) + protection_use:logDistSM + protection_use:poly(SST_sd, 
                                                                                                                                              2) + bruvs:protection_use:logTTM + bruvs:protection_use:GovernmentEffectiveness_mean + 
                     bruvs:protection_use:logDistP + bruvs:protection_use:Slope + 
                     bruvs:protection_use:poly(logSST, 2) + bruvs:protection_use:logDistSM + 
                     bruvs:protection_use:poly(SST_sd, 2),
                     data = tab_firstmode,  correlation = nlme::corRatio(form=~1),method='ML') 
  
  
  #get aic
  a <- AIC(mod)
  print(a)
  
  #return model
  print("returning model")
  return(mod)
  
}





#' fit fully saturated gls models with secondmode as response variable and bruvs as factor of all covariates
#' with autocorrelation  **ATTENTION need to update with new variables
#'
#' @param data dataframe with predictor variables
#'
#' @return
#' @export
#'

fit_gls_sat_cor_secondmode <- function(data){
  
  
  #autocorrelation
  print("fitting gls with formula: logSecondmode ~ bruvs * protection_use  * (logBathy + logTTM + GovernmentEffectiveness_mean +logDistC + logDistP+ logDistCR+ Slope+
                                             poly(logSST,2)  + poly(logCHL,2) +logDistSM + poly(SST_sd,2))")
  
  mod <- nlme::gls(logSecondmode ~ bruvs * protection_use * (logBathy + logTTM + GovernmentEffectiveness_mean + logDistC+ logDistP + logDistCR + Slope +
                                                            poly(logSST,2) + poly(logCHL,2) +logDistSM + poly(SST_sd,2)), data = tab_secondmode,  correlation = nlme::corRatio(form=~1),method='ML') 
  
  
  #get aic
  a <- AIC(mod)
  print(a)
  
  #return model
  print("returning model")
  return(mod)
  
}



#' fit simplified gls models with secondmode as response variable and bruvs as factor of all covariates
#' with autocorrelation  **ATTENTION need to update with new variables
#'
#' @param data dataframe with predictor variables
#'
#' @return
#' @export
#'

fit_gls_sim_cor_secondmode <- function(data){
  
  
  #autocorrelation
  print("fitting gls with formula: logSecondmode ~ bruvs + protection_use + logBathy + logTTM + 
    GovernmentEffectiveness_mean + logDistC + logDistP + logDistCR + 
    poly(logSST, 2) + poly(logCHL, 2) + logDistSM + bruvs:protection_use + 
    bruvs:logBathy + bruvs:logTTM + bruvs:GovernmentEffectiveness_mean + 
    bruvs:logDistP + bruvs:logDistCR + bruvs:poly(logSST, 2) + 
    bruvs:poly(logCHL, 2) + bruvs:logDistSM + protection_use:logBathy + 
    protection_use:logTTM + protection_use:GovernmentEffectiveness_mean + 
    protection_use:logDistP + protection_use:poly(logSST, 2) + 
    protection_use:poly(logCHL, 2) + bruvs:protection_use:logBathy + 
    bruvs:protection_use:logTTM + bruvs:protection_use:GovernmentEffectiveness_mean + 
    bruvs:protection_use:logDistP + bruvs:protection_use:poly(logSST, 
    2) + bruvs:protection_use:poly(logCHL, 2)")
  
  mod <- nlme::gls(logSecondmode ~ bruvs + protection_use + logBathy + logTTM + 
                     GovernmentEffectiveness_mean + logDistC + logDistP + logDistCR + 
                     poly(logSST, 2) + poly(logCHL, 2) + logDistSM + bruvs:protection_use + 
                     bruvs:logBathy + bruvs:logTTM + bruvs:GovernmentEffectiveness_mean + 
                     bruvs:logDistP + bruvs:logDistCR + bruvs:poly(logSST, 2) + 
                     bruvs:poly(logCHL, 2) + bruvs:logDistSM + protection_use:logBathy + 
                     protection_use:logTTM + protection_use:GovernmentEffectiveness_mean + 
                     protection_use:logDistP + protection_use:poly(logSST, 2) + 
                     protection_use:poly(logCHL, 2) + bruvs:protection_use:logBathy + 
                     bruvs:protection_use:logTTM + bruvs:protection_use:GovernmentEffectiveness_mean + 
                     bruvs:protection_use:logDistP + bruvs:protection_use:poly(logSST, 
                                                                               2) + bruvs:protection_use:poly(logCHL, 2), data = tab_secondmode,  correlation = nlme::corRatio(form=~1),method='ML') 
  
  
  #get aic
  a <- AIC(mod)
  print(a)
  
  #return model
  print("returning model")
  return(mod)
  
}




#' plot predictor variables for each bruvs_protection_use cat
#' @param dat data 
#' @param envar_name envar variables
#' @import ggplot2
#' @return
#' @export
#'


bruvs_protect_var <- function(dat, envar_name) { 
  
  
  dat$interaction_cat <- paste(dat$protection_use, dat$bruvs, sep = "__")
  
  envar_plot <- ggplot(data=dat, aes(x=interaction_cat, y= dat[[envar_name]] )) +
    stat_summary(fun = mean, fun.min = min, fun.max = max) +
    theme(axis.text = element_text(size = 10)) +
    theme(axis.title = element_text(size = 10)) +
    #ylim(0, 6000)+
    coord_flip() +xlab("") +ylab(envar_name)
  
  print(envar_plot)
  invisible(envar_plot)
  
}


#' marginal plots for categorical co_variates with noextra
#'
#' @param response, dat with predictor variables
#' @param mod_name name of model
#' @param dat data
#' @param mod model
#' @param var variable
#' @param var_name variable name
#' @param group group
#' @param group2 second group
#' 
#' @return
#' @export
#'

marg_plot_cat_covar_noextra <- function(response, mod_name, dat, mod, var, var_name, group, group2, condition){
  

  
  #function
  var_to_plot <- dat[var_name]
  group_var <- dat[group]
  group_var2 <-dat[group2]
  
  pred <- as.data.frame(ggeffects::ggpredict(mod, terms= c(var, group, group2), condition = condition)) #between 40 and 469 meter depth"logBathy [1.35:1.85]"
  
  #select empirical range of covariate bruvs
  
  min(var_to_plot[group_var=="pelagic"]) -> min_var_pel 
  max(var_to_plot[group_var=="pelagic"]) -> max_var_pel 
  min(var_to_plot[group_var=="benthic"]) -> min_var_ben
  max(var_to_plot[group_var=="benthic"]) -> max_var_ben
  
  #filter predictions to empirical range of bruvs
  pred %>%    
    dplyr::mutate(x = dplyr::case_when(group=="pelagic" ~ replace(x, x<min_var_pel | x>max_var_pel, NA),
                                       group=="benthic" ~ replace(x, x<min_var_ben | x>max_var_ben, NA))) -> pred
  
  #select empirical range of var per bruvs
  
  min(var_to_plot[group_var=="pelagic"&group_var2 == "protected"]) -> min_var2_pel_pro 
  min(var_to_plot[group_var=="pelagic"&group_var2 == "part_protected"]) -> min_var2_pel_part
  min(var_to_plot[group_var=="pelagic"&group_var2 == "not_protected"]) -> min_var2_pel_not
  min(var_to_plot[group_var=="benthic"&group_var2 == "protected"]) -> min_var2_ben_pro
  min(var_to_plot[group_var=="benthic"&group_var2 == "part_protected"]) -> min_var2_ben_part
  min(var_to_plot[group_var=="benthic"&group_var2 == "not_protected"]) -> min_var2_ben_not
  
  #filter predictions to empirical range of var per bruvs
  
  pred %>%
    dplyr::mutate(x = dplyr::case_when(group=="pelagic"&facet=="protected" ~ replace(x, x<min_var2_pel_pro, NA),
                                       group=="pelagic"&facet=="part_protected" ~ replace(x, x<min_var2_pel_part, NA),
                                       group=="pelagic"&facet=="not_protected" ~ replace(x, x<min_var2_pel_not, NA),
                                       group=="benthic"&facet=="protected"~ replace(x, x<min_var2_ben_pro, NA),
                                       group=="benthic"&facet=="part_protected"~ replace(x, x<min_var2_ben_part, NA),
                                       group=="benthic"&facet=="not_protected"~ replace(x, x<min_var2_ben_not, NA))) -> pred
  
  #select empirical range of var per bruvs
  
  max(var_to_plot[group_var=="pelagic"&group_var2 == "protected"]) -> max_var2_pel_pro 
  max(var_to_plot[group_var=="pelagic"&group_var2 == "part_protected"]) -> max_var2_pel_part
  max(var_to_plot[group_var=="pelagic"&group_var2 == "not_protected"]) -> max_var2_pel_not
  max(var_to_plot[group_var=="benthic"&group_var2 == "protected"]) -> max_var2_ben_pro
  max(var_to_plot[group_var=="benthic"&group_var2 == "part_protected"]) -> max_var2_ben_part
  max(var_to_plot[group_var=="benthic"&group_var2 == "not_protected"]) -> max_var2_ben_not
  
  #filter predictions to empirical range of var per bruvs
  
  pred %>%
    dplyr::mutate(x = dplyr::case_when(group=="pelagic"&facet=="protected" ~ replace(x, x>max_var2_pel_pro, NA),
                                       group=="pelagic"&facet=="part_protected" ~ replace(x, x>max_var2_pel_part, NA),
                                       group=="pelagic"&facet=="not_protected" ~ replace(x, x>max_var2_pel_not, NA),
                                       group=="benthic"&facet=="protected"~ replace(x, x>max_var2_ben_pro, NA),
                                       group=="benthic"&facet=="part_protected"~ replace(x, x>max_var2_ben_part, NA),
                                       group=="benthic"&facet=="not_protected"~ replace(x, x>max_var2_ben_not, NA))) -> pred
  
  pred  %>%
    tidyr::drop_na(x) -> pred
  
  marg <- ggplot(pred, aes(x, predicted, group=group, colour = group, fill=group)) +
    geom_line() + 
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .3, linetype=0) +
    scale_colour_manual(name = "", values = c('orange','#077DAA'), aesthetics = c("colour", "fill"))+
    theme_light()+ labs(x = var_name, y = response) + facet_wrap(~facet)#+ylim(-5.5, 3.5)
  
  print(marg)
  
  ggsave(marg, filename = here::here("outputs", "model_outputs", paste0(mod_name, "_marg_plot_", group2,"_", var_name, ".png")), width = 10, height = 6, units = "in", dpi =300)
  
  invisible(marg)
  
}




#' fit fully saturated gls models with unimode as response variable and bruvs as factor of all covariates
#' with autocorrelation  **ATTENTION need to update with new variables
#'
#' @param data dataframe with predictor variables
#'
#' @return
#' @export
#'

fit_gls_sat_cor_unimode <- function(data){
  
  
  #autocorrelation
  print("fitting gls with formula: logUnimode ~ bruvs * protection_use  * (logBathy + logTTM + GovernmentEffectiveness_mean +logDistC + logDistP+ logDistCR+ Slope+
                                             poly(logSST,2)  + poly(logCHL,2) +logDistSM + poly(SST_sd,2))")
  
  mod <- nlme::gls(logUnimode ~ bruvs * protection_use * (logBathy + logTTM + GovernmentEffectiveness_mean + logDistC+ logDistP + logDistCR + Slope +
                                                               poly(logSST,2) + poly(logCHL,2) +logDistSM + poly(SST_sd,2)), data = tab_unimode,  correlation = nlme::corRatio(form=~1),method='ML') 
  
  
  #get aic
  a <- AIC(mod)
  print(a)
  
  #return model
  print("returning model")
  return(mod)
  
}


#' multi marginal plot across models

#' @param beta_marg marg betaslop
#' @param firstmode_marg marg firstmode
#' @param secondmode_marg marg secondmode
#' @param covariate_name name of covariate
#' 
#' @import ggplot2
#' @import cowplot
#' @return
#' @export
#'
#'

multi_covariate_marg <- function(beta_marg, firstmode_marg, secondmode_marg, covariate_name){
  
  multi_marg <- ggdraw() +
    draw_plot(beta_marg+theme(strip.text.x = element_text(size = 16), 
                              axis.text = element_text(size =16), axis.title = element_text(size =16), legend.text =element_text(size =16),legend.position= c(.9, .8), 
                              legend.background = element_rect(fill = "transparent"))+ labs(x=""),  0,   0.66, 1, 0.33) +
    draw_plot(firstmode_marg+theme(strip.text.x = element_text(size = 16), 
                              axis.text = element_text(size =16),axis.title = element_text(size =16), legend.text =element_text(size =16),legend.position= "none")+ labs(x=""), 0, 0.33, 1, .33) +
    draw_plot(secondmode_marg+theme(strip.text.x = element_text(size = 16), 
                              axis.text = element_text(size =16),axis.title = element_text(size =16), legend.text =element_text(size =16),legend.position= "none"), 0, 0, 1,.33) +
  
    draw_plot_label(c("a", "b", "c"), c(0, 0, 0), c(.99, .66, .33), size = 22)
  
  print(multi_marg)
  
  ggsave(multi_marg, filename = here::here("outputs", "model_outputs", paste0("All_model", covariate_name, ".png")), width = 12, height = 12, units = "in", dpi =300)
  
  invisible(multi_marg)
  
  
}

#' multi marginal plot across models only SST_sd

#' @param beta_marg marg betaslop
#' @param firstmode_marg marg firstmode
#' @param covariate_name name of covariate
#' 
#' @import ggplot2
#' @import cowplot
#' @return
#' @export
#'
#'

multi_covariate_marg_SST_sd <- function(beta_marg, firstmode_marg, covariate_name){
  
  multi_marg <- ggdraw() +
    draw_plot(beta_marg+theme(strip.text.x = element_text(size = 16), 
                              axis.text = element_text(size =16), axis.title = element_text(size =16), legend.text =element_text(size =16),legend.position= c(.9, .9), 
                              legend.background = element_rect(fill = "transparent"))+ labs(x=""),  0,   0.5, 1, 0.5) +
    draw_plot(firstmode_marg+theme(strip.text.x = element_text(size = 16), 
                                   axis.text = element_text(size =16),axis.title = element_text(size =16), legend.text =element_text(size =16),legend.position= "none"), 0, 0, 1, .5)+
    draw_plot_label(c("A", "B"), c(0, 0), c(1, 0.5), size = 16)
  
  print(multi_marg)
  
  ggsave(multi_marg, filename = here::here("outputs", "model_outputs", paste0("All_model", covariate_name, ".png")), width = 12, height = 10, units = "in", dpi =300)
  
  invisible(multi_marg)
  
  
}

#' marginal plots for categorical co_variates with noextra - deLog10 the y response
#'
#' @param response, dat with predictor variables
#' @param mod_name name of model
#' @param dat data
#' @param mod model
#' @param var variable
#' @param var_name variable name
#' @param group group
#' @param group2 second group
#' 
#' @return
#' @export
#'

marg_plot_cat_covar_noextra_deLog <- function(response, mod_name, dat, mod, var, var_name, group, group2, condition){
  
  
  options(scipen=10000)
  
  #function
  var_to_plot <- dat[var_name]
  group_var <- dat[group]
  group_var2 <-dat[group2]
  
  pred <- as.data.frame(ggeffects::ggpredict(mod, terms= c(var, group, group2), condition = condition)) #between 40 and 469 meter depth"logBathy [1.35:1.85]"
  
  #select empirical range of covariate bruvs
  
  min(var_to_plot[group_var=="pelagic"]) -> min_var_pel 
  max(var_to_plot[group_var=="pelagic"]) -> max_var_pel 
  min(var_to_plot[group_var=="benthic"]) -> min_var_ben
  max(var_to_plot[group_var=="benthic"]) -> max_var_ben
  
  #filter predictions to empirical range of bruvs
  pred %>%    
    dplyr::mutate(x = dplyr::case_when(group=="pelagic" ~ replace(x, x<min_var_pel | x>max_var_pel, NA),
                                       group=="benthic" ~ replace(x, x<min_var_ben | x>max_var_ben, NA))) -> pred
  
  #select empirical range of var per bruvs
  
  min(var_to_plot[group_var=="pelagic"&group_var2 == "protected"]) -> min_var2_pel_pro 
  min(var_to_plot[group_var=="pelagic"&group_var2 == "part_protected"]) -> min_var2_pel_part
  min(var_to_plot[group_var=="pelagic"&group_var2 == "not_protected"]) -> min_var2_pel_not
  min(var_to_plot[group_var=="benthic"&group_var2 == "protected"]) -> min_var2_ben_pro
  min(var_to_plot[group_var=="benthic"&group_var2 == "part_protected"]) -> min_var2_ben_part
  min(var_to_plot[group_var=="benthic"&group_var2 == "not_protected"]) -> min_var2_ben_not
  
  #filter predictions to empirical range of var per bruvs
  
  pred %>%
    dplyr::mutate(x = dplyr::case_when(group=="pelagic"&facet=="protected" ~ replace(x, x<min_var2_pel_pro, NA),
                                       group=="pelagic"&facet=="part_protected" ~ replace(x, x<min_var2_pel_part, NA),
                                       group=="pelagic"&facet=="not_protected" ~ replace(x, x<min_var2_pel_not, NA),
                                       group=="benthic"&facet=="protected"~ replace(x, x<min_var2_ben_pro, NA),
                                       group=="benthic"&facet=="part_protected"~ replace(x, x<min_var2_ben_part, NA),
                                       group=="benthic"&facet=="not_protected"~ replace(x, x<min_var2_ben_not, NA))) -> pred
  
  #select empirical range of var per bruvs
  
  max(var_to_plot[group_var=="pelagic"&group_var2 == "protected"]) -> max_var2_pel_pro 
  max(var_to_plot[group_var=="pelagic"&group_var2 == "part_protected"]) -> max_var2_pel_part
  max(var_to_plot[group_var=="pelagic"&group_var2 == "not_protected"]) -> max_var2_pel_not
  max(var_to_plot[group_var=="benthic"&group_var2 == "protected"]) -> max_var2_ben_pro
  max(var_to_plot[group_var=="benthic"&group_var2 == "part_protected"]) -> max_var2_ben_part
  max(var_to_plot[group_var=="benthic"&group_var2 == "not_protected"]) -> max_var2_ben_not
  
  #filter predictions to empirical range of var per bruvs
  
  pred %>%
    dplyr::mutate(x = dplyr::case_when(group=="pelagic"&facet=="protected" ~ replace(x, x>max_var2_pel_pro, NA),
                                       group=="pelagic"&facet=="part_protected" ~ replace(x, x>max_var2_pel_part, NA),
                                       group=="pelagic"&facet=="not_protected" ~ replace(x, x>max_var2_pel_not, NA),
                                       group=="benthic"&facet=="protected"~ replace(x, x>max_var2_ben_pro, NA),
                                       group=="benthic"&facet=="part_protected"~ replace(x, x>max_var2_ben_part, NA),
                                       group=="benthic"&facet=="not_protected"~ replace(x, x>max_var2_ben_not, NA))) -> pred
  
  pred  %>%
    tidyr::drop_na(x) -> pred
  
  marg <- ggplot(pred, aes(x, y=(10^predicted)-1, group=group, colour = group, fill=group)) +
    geom_line() + 
    geom_ribbon(aes(ymin = (10^conf.low)-1, ymax = (10^conf.high)-1), alpha = .3, linetype=0) +
    scale_colour_manual(name = "", values = c('orange','#077DAA'), aesthetics = c("colour", "fill"))+
    theme_light()+ labs(x = var_name, y = response) + scale_y_log10()+facet_wrap(~facet)#+ylim(-5.5, 3.5)
  
  print(marg)
  
  ggsave(marg, filename = here::here("outputs", "model_outputs", paste0(mod_name, "_marg_plot_deLog", group2,"_", var_name, ".png")), width = 10, height = 6, units = "in", dpi =300)
  
  invisible(marg)
  
}



#' @param beta_marg marg betaslop
#' @param firstmode_marg marg firstmode
#' @param secondmode_marg marg secondmode
#' @param covariate_name name of covariate
#' 
#' @import ggplot2
#' @import cowplot
#' @return
#' @export
#'
#'

multi_covariate_marg_cat <- function(beta_marg, firstmode_marg, secondmode_marg, covariate_name){
  
  multi_marg <- ggdraw() +
    draw_plot(beta_marg+theme(strip.text.x = element_text(size = 16), 
                              axis.text = element_text(size =16), axis.title = element_text(size =16), legend.text =element_text(size =16),legend.position= c(.9, .8), 
                              legend.background = element_rect(fill = "transparent"))+ labs(x=""),  0,   0.66, 1, 0.33) +
    draw_plot(firstmode_marg+theme(strip.text.x = element_text(size = 16), 
                                   axis.text = element_text(size =16),axis.title = element_text(size =16), legend.text =element_text(size =16),legend.position= "none")+ labs(x=""), 0, 0.33, 1, .33) +
    draw_plot(secondmode_marg+theme(strip.text.x = element_text(size = 16), 
                                    axis.text = element_text(size =16),axis.title = element_text(size =16), legend.text =element_text(size =16),legend.position= "none"), 0, 0, 1,.33) +
    
    draw_plot_label(c("a", "b", "c"), c(0, 0, 0), c(.99, .66, .33), size = 16)
  
  print(multi_marg)
  
  ggsave(multi_marg, filename = here::here("outputs", "model_outputs", paste0("All_model", covariate_name, ".png")), width = 8, height = 10, units = "in", dpi =300)
  
  invisible(multi_marg)
  
  
}


#' modal analysis scatterplot plus regression line
#'
#' @param data 
#'
#' @import ggplot2 
#' @return
#' @export
#'
#' 

mode_vs_mode_lm <- function(dat){
  options(scipen=5)
  
  mode_mode <- ggplot(data=dat, aes(x=logFirstmode, y=logSecondmode, group=bruvs, colour=bruvs, fill=bruvs))+
    geom_point(size = 1)+
    geom_smooth(method="lm")+theme_light()+
    theme(legend.position = c(0.8, 0.9), legend.title = element_blank(), axis.title.y = element_text(size=16),
          legend.text = element_text(size =16),axis.text.x = element_text(size=16),
          axis.text.y = element_text(size=16), axis.title.x = element_text(size=16))+
    scale_fill_manual(values = c("pelagic" = '#077DAA', 'benthic' = 'orange'))+  
    scale_colour_manual(values = c("pelagic" = '#077DAA', 'benthic' = 'darkorange')) +xlab("First mode (kg), log10(x+1)")+ylab("Second mode (kg), log10(x+1)")+facet_wrap(~protection_use)

  print(mode_mode)
  
  ggsave(mode_mode, filename = here::here("outputs", "mode_vs_mode.png"), width = 12, height =6, units = "in", dpi =300)
  invisible(mode_mode)
  
  #modal regression analysis - to estimate the spacing between the modes in log10 units
  
  mod <- lm(logSecondmode ~ bruvs*logFirstmode,
            data = dat)
  
  return(summary(mod))
  
}

#' modal analysis linear regression
#'
#' @param data 
#'
#' @import ggplot2 
#' @import cowplot
#' @return
#' @export
#'
#' 

mode_lm_res_beta <- function(dat){

  
#get residuals from modal regression
mod <- lm(logSecondmode ~ bruvs*logFirstmode, data = dat)
lm_res <- residuals(mod)
tab_res <- cbind(tab_firstmode, lm_res)

#plot residuals against betaslope to see if negative residuals (ie where the difference between the modes is less than expected) are associated with more negative betaslopes

mode_res <- ggplot(data = tab_res, aes(y= betaslope, x = lm_res, group=bruvs, colour = bruvs, fill = bruvs, size=((10^logTTM)/10000000)^2))+
  geom_point()+
  geom_smooth(method="lm")+
  coord_cartesian(xlim = c(1.5, -1.5), ylim=c(0,-3)) +theme_light()+
  theme(legend.position = c(0.8, 0.9), legend.title = element_blank(), axis.title.y = element_text(size=16),
        legend.text = element_text(size =16),axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16), axis.title.x = element_text(size=16))+
  scale_fill_manual(values = c("pelagic" = '#077DAA', 'benthic' = 'orange'))+  
  scale_colour_manual(values = c("pelagic" = '#077DAA', 'benthic' = 'darkorange')) +xlab("Residuals from modal regression")+facet_wrap(~protection_use)

print(mode_res)

ggsave(mode_res, filename = here::here("outputs", "mode_lm_res_beta.png"), width = 12, height =6, units = "in", dpi =300)

invisible(mode_res)


mode_mod_res <- lm(betaslope ~ bruvs*lm_res, data = dat)

return(summary(mode_mod_res))

}
