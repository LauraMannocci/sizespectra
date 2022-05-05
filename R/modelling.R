
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

clean_data_with_vars <- function(dat){

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

  # bind
  dat = cbind(dat,logconflicts,logNGO,logTTM,logSAU,logTTP, logDistP, logDistSM,logDistCR,logDistC,logBathy,logPP,logCHL,logSST,bruvs)

  # drop nas in some variables
  dat %>%
    tidyr::drop_na(logBathy) %>%
    tidyr::drop_na(betaslope) %>%
    tidyr::drop_na(mean_lat) %>%
    tidyr::drop_na(mean_long) -> dat

  # add pelagic/benthic predictors
  dat %>%
    dplyr::mutate(pelagic = ifelse(bruvs == "pelagic", TRUE, FALSE)) %>%
    dplyr::mutate(benthic = ifelse(bruvs == "benthic", TRUE, FALSE)) -> dat

  return(dat)


}






#' make correlogram of predictors ******need to update variables
#'
#' @param dat dataframe with predictor variables
#'
#' @return
#' @export
#'

make_correlogram_vars <- function(dat){

  #need to load library for pnael.cor tp work
  library(corrgram)
  cor <- corrgram::corrgram(dat[,c("logPP","logCHL","logSST","SST_sd","HDI_mean","logNGO","Voice_mean",
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
#' @param model_name
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

get_partial_plot_betaslope <- function(dat, model, model_name, vars){

  plot <- list()

  #loop on model variables
  for (i in 1:length(vars)){

    var_to_plot <- vars[i]
    other_vars <- vars[!vars %in% var_to_plot]

    #get means for other predictors and replace in new dataframe for prediction
    datnew <- dat
    means <- apply(datnew[, other_vars], MARGIN = 2, FUN = function(x) mean(x, na.rm = TRUE))
    datnew[, other_vars] <- rep(means, each = nrow(datnew))

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

    if (var_to_plot %in% c("logBathy", "logDistP", "logDistSM")){

      #log transformation
      plot[[i]] <- ggplot2::ggplot() +
        # pelagic
        ggplot2::geom_line(data = dat_pel, ggplot2::aes(x = 10^var_to_plot, y = fit, colour='#077DAA')) + #predictions
        ggplot2::geom_ribbon(data = dat_pel, ggplot2::aes(x = 10^var_to_plot,
                                        ymin = lower,
                                        ymax = upper,
                                        fill='#077DAA'),#95% confidence intervals
                    alpha=0.2, show.legend=T) +

        # benthic
        ggplot2::geom_line(data = dat_ben, ggplot2::aes(x = 10^var_to_plot, y = fit, colour='darkorange')) + #predictions
        ggplot2::geom_ribbon(data = dat_ben, ggplot2::aes(x = 10^var_to_plot,
                                        ymin = lower,
                                        ymax = upper,
                                        fill='darkorange'),#95% confidence intervals
                    alpha=0.2,  show.legend=T) +
        ggplot2::labs(y = "betaslope", x = substring(var_to_plot, 4))+
        ggplot2::scale_colour_manual(name = "", values = c('#077DAA', 'darkorange'),
                            labels = c("pelagic", "benthic"), aesthetics = c("colour", "fill"))+
        ggplot2::theme_classic()

    }else{

      plot[[i]] <- ggplot2::ggplot() +
        # pelagic
        ggplot2::geom_line(data = dat_pel, ggplot2::aes(x = var_to_plot, y = fit, colour='#077DAA')) + #predictions
        ggplot2::geom_ribbon(data = dat_pel, ggplot2::aes(x = var_to_plot,
                                        ymin = lower,
                                        ymax = upper,
                                        fill='#077DAA'),#95% confidence intervals
                    alpha=0.2, show.legend=T) +

        # benthic
        ggplot2::geom_line(data = dat_ben, ggplot2::aes(x = var_to_plot, y = fit, colour='darkorange')) + #predictions
        ggplot2::geom_ribbon(data = dat_ben, ggplot2::aes(x = var_to_plot,
                                        ymin = lower,
                                        ymax = upper,
                                        fill='darkorange'),#95% confidence intervals
                    alpha=0.2,  show.legend=T) +
        ggplot2::labs(y = "betaslope", x = var_to_plot)+
        ggplot2::scale_colour_manual(name = "", values = c('#077DAA', 'darkorange'),
                            labels = c("pelagic", "benthic"), aesthetics = c("colour", "fill"))+
        ggplot2::theme_classic()
    }

  }

  #save multiplot
  png(here::here("outputs",  "model_outputs", paste0("partial_plot_", model_name, ".png")), width = 960, height = 960)
  Rmisc::multiplot(plotlist = plot, cols = 2)
  dev.off()

}








