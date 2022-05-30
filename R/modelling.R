
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
#' @return
#' @export
#'

clean_data_with_vars_betaslope <- function(dat){

  # transformation of variables
  
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
  protection_use = as.factor(dat$protection)
  exped = as.factor(dat$TERRITORY1)

  # bind
  dat = cbind(dat, exped, logconflicts,logNGO,logTTM,logSAU,logTTP, logDistP, logDistSM,logDistCR,logDistC,logBathy,logPP,logCHL,logSST,bruvs, protection_use)

  # drop nas in some variables
  dat %>%
    tidyr::drop_na(logBathy) %>%
    tidyr::drop_na(betaslope) %>%
    tidyr::drop_na(mean_lat) %>%
    tidyr::drop_na(mean_long) %>%
   
  # turn protection NA into out (no protection) 
    dplyr::mutate(protection_use = tidyr::replace_na(protection_use, "Out")) %>%
  # rename protection levels
    dplyr::mutate(protection_use = dplyr::recode(protection_use, All = "no_take", None = "out", `Not reported` = "partly_protected", Out = "out", Part = "no_take")) -> dat
  
  #turn protection_use into numeric
    dat$protection_use = as.numeric(dat$protection_use)
  
  #no_take = 1, out = 2, partly_protected =3
    
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
  png(here::here("outputs", "model_outputs", paste0("resid_vs_fitted_", model_name, ".png")))
  plot(residuals(model, type = "normalized") ~ fitted(model))
  dev.off()

  #variograms
  png(here::here("outputs",  "model_outputs", paste0("variogram_longlat_", model_name, ".png")))
  print(plot(nlme:::Variogram(model, form = ~ mean_long+mean_lat, resType = "normalized", data = dat)))
  dev.off()

  png(here::here("outputs",  "model_outputs", paste0("variogram_", model_name, ".png"))) ##attention not sur this one makes sense
  print(plot(nlme:::Variogram(model, form = ~1, resType = "normalized", data = dat)))
  dev.off()

  #autocorrelation plot
  png(here::here("outputs",  "model_outputs", paste0("acf_", model_name, ".png")))
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
        ggplot2::geom_line(data = dat_ben, ggplot2::aes(x = 10^var_to_plot, y = fit, colour='darkorange')) + #predictions
        ggplot2::geom_ribbon(data = dat_ben, ggplot2::aes(x = 10^var_to_plot,
                                        ymin = lower,
                                        ymax = upper,
                                        fill='darkorange'),#95% confidence intervals
                    alpha=0.5,  show.legend=T) +
        ggplot2::labs(y = "fit", x = substring(var_to_plot, 4))+
        ggplot2::scale_colour_manual(name = "", values = c('#077DAA', 'darkorange'),
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
          ggplot2::geom_point(data = dat_ben, ggplot2::aes(x = var_to_plot, y = fit, colour='darkorange')) + #predictions
          ggplot2::geom_errorbar(data = dat_ben, ggplot2::aes(x= var_to_plot, ymin=lower, ymax=upper,colour='darkorange')) +#95% confidence intervals
          
          ggplot2::labs(y = "fit", x = var_to_plot)+
          ggplot2::scale_colour_manual(name = "", values = c('#077DAA', 'darkorange'),
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
          ggplot2::geom_line(data = dat_ben, ggplot2::aes(x = var_to_plot, y = fit, colour='darkorange')) + #predictions
          ggplot2::geom_ribbon(data = dat_ben, ggplot2::aes(x = var_to_plot,
                                          ymin = lower,
                                          ymax = upper,
                                          fill='darkorange'),#95% confidence intervals
                      alpha=0.5,  show.legend=T) +
          ggplot2::labs(y = "fit", x = var_to_plot)+
          ggplot2::scale_colour_manual(name = "", values = c('#077DAA', 'darkorange'),
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
  
  mod_coef <- plot_model(model, sort.est = TRUE, show.values = TRUE, value.offset = .3, type = 'std')
  
  print(mod_coef)
  
  ggsave(mod_coef, filename = here::here("outputs", "model_outputs", paste0("coef_plot_", model_name, ".png")), width = 8, height = 6, units = "in", dpi =300)
  
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



#' clean data with predictor variables **ATTENTION need to update with new variables
#'
#' @param dat dataframe with predictor variables
#' @return
#' @export
#'

clean_data_with_vars_median_maxsize <- function(dat){
  
  # transformation of variables
  
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
  protection_use = as.factor(dat$protection)
  
  # bind
  dat = cbind(dat,logconflicts,logNGO,logTTM,logSAU,logTTP, logDistP, logDistSM,logDistCR,logDistC,logBathy,logPP,logCHL,logSST,bruvs, protection_use)
  
  # drop nas in some variables
  dat %>%
    tidyr::drop_na(logBathy) %>%
    tidyr::drop_na(median_maxsize) %>%
    tidyr::drop_na(mean_lat) %>%
    tidyr::drop_na(mean_long) %>%

  # turn protection NA into out (no protection) 
  dplyr::mutate(protection_use = tidyr::replace_na(protection_use, "Out")) %>%
  # rename protection levels
  dplyr::mutate(protection_use = dplyr::recode(protection_use, All = "no_take", None = "out", `Not reported` = "partly_protected", Out = "out", Part = "no_take")) -> dat
  
  #turn protection_use into numeric
  dat$protection_use = as.numeric(dat$protection_use)
  
  #no_take = 1, out = 2, partly_protected =3
  
  
  return(dat)
  
  
}




#' clean data with predictor variables **ATTENTION need to update with new variables
#'
#' @param dat dataframe with predictor variables
#' @return
#' @export
#'

clean_data_with_vars_median <- function(dat){
  
  # transformation of variables
  
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
  protection_use = as.factor(dat$protection)

  # bind
  dat = cbind(dat,logconflicts,logNGO,logTTM,logSAU,logTTP, logDistP, logDistSM,logDistCR,logDistC,logBathy,logPP,logCHL,logSST,bruvs, protection_use)
  
  # drop nas in some variables
  dat %>%
    tidyr::drop_na(logBathy) %>%
    tidyr::drop_na(mean_mediansize) %>%
    tidyr::drop_na(mean_lat) %>%
    tidyr::drop_na(mean_long) %>%
  
  # turn protection NA into out (no protection) 
  dplyr::mutate(protection_use = tidyr::replace_na(protection_use, "Out")) %>%
    # rename protection levels
    dplyr::mutate(protection_use = dplyr::recode(protection_use, All = "no_take", None = "out", `Not reported` = "partly_protected", Out = "out", Part = "no_take")) -> dat
  
  #turn protection_use into numeric
  dat$protection_use = as.numeric(dat$protection_use)
  
  #no_take = 1, out = 2, partly_protected =3
  
  return(dat)
  
  
}


#' clean data with predictor variables **ATTENTION need to update with new variables
#'
#' @param dat dataframe with predictor variables
#' @return
#' @export
#'

clean_data_with_vars_first_mode <- function(dat){
  
  # transformation of variables
  
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
  protection_use = as.factor(dat$protection)
  exped = as.factor(dat$TERRITORY1)
  
  # bind
  dat = cbind(dat, exped, logconflicts,logNGO,logTTM,logSAU,logTTP, logDistP, logDistSM,logDistCR,logDistC,logBathy,logPP,logCHL,logSST,bruvs, protection_use)
  
  # drop nas in some variables
  dat %>%
    tidyr::drop_na(logBathy) %>%
    tidyr::drop_na(first_mode) %>%
    tidyr::drop_na(mean_lat) %>%
    tidyr::drop_na(mean_long) %>%
    
    # turn protection NA into out (no protection) 
    dplyr::mutate(protection_use = tidyr::replace_na(protection_use, "Out")) %>%
    # rename protection levels
    dplyr::mutate(protection_use = dplyr::recode(protection_use, All = "no_take", None = "out", `Not reported` = "partly_protected", Out = "out", Part = "no_take")) -> dat
  
  #turn protection_use into numeric
  dat$protection_use = as.numeric(dat$protection_use)
  
  #no_take = 1, out = 2, partly_protected =3
  
  return(dat)
  
  
}

#' clean data with predictor variables **ATTENTION need to update with new variables
#'
#' @param dat dataframe with predictor variables
#' @return
#' @export
#'

clean_data_with_vars_second_mode <- function(dat){
  
  # transformation of variables
  
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
  protection_use = as.factor(dat$protection)
  exped = as.factor(dat$TERRITORY1)
  
  # bind
  dat = cbind(dat, exped, logconflicts,logNGO,logTTM,logSAU,logTTP, logDistP, logDistSM,logDistCR,logDistC,logBathy,logPP,logCHL,logSST,bruvs, protection_use)
  
  # drop nas in some variables
  dat %>%
    tidyr::drop_na(logBathy) %>%
    tidyr::drop_na(second_mode) %>%
    tidyr::drop_na(mean_lat) %>%
    tidyr::drop_na(mean_long) %>%
    
    # turn protection NA into out (no protection) 
    dplyr::mutate(protection_use = tidyr::replace_na(protection_use, "Out")) %>%
    # rename protection levels
    dplyr::mutate(protection_use = dplyr::recode(protection_use, All = "no_take", None = "out", `Not reported` = "partly_protected", Out = "out", Part = "no_take")) -> dat
  
  #turn protection_use into numeric
  dat$protection_use = as.numeric(dat$protection_use)
  
  #no_take = 1, out = 2, partly_protected =3
  
  return(dat)
  
  
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


#' clean data with predictor variables **ATTENTION need to update with new variables
#'
#' @param dat dataframe with predictor variables
#' @return
#' @export
#'

clean_data_with_vars_mean_maxsize <- function(dat){
  
  # transformation of variables
  
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
  protection_use = as.factor(dat$protection)
  
  # bind
  dat = cbind(dat,logconflicts,logNGO,logTTM,logSAU,logTTP, logDistP, logDistSM,logDistCR,logDistC,logBathy,logPP,logCHL,logSST,bruvs, protection_use)
  
  # drop nas in some variables
  dat %>%
    tidyr::drop_na(logBathy) %>%
    tidyr::drop_na(mean_maxsize) %>%
    tidyr::drop_na(mean_lat) %>%
    tidyr::drop_na(mean_long) %>%
    
    # turn protection NA into out (no protection) 
    dplyr::mutate(protection_use = tidyr::replace_na(protection_use, "Out")) %>%
    # rename protection levels
    dplyr::mutate(protection_use = dplyr::recode(protection_use, All = "no_take", None = "out", `Not reported` = "partly_protected", Out = "out", Part = "no_take")) -> dat
  
  #turn protection_use into numeric
  dat$protection_use = as.numeric(dat$protection_use)
  
  #no_take = 1, out = 2, partly_protected =3
  
  
  return(dat)
}