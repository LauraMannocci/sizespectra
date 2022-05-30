#load all functions
devtools::document()
devtools::load_all()


# read and clean data ----------------------------


# read data with predictor variables
tab <- read_data_with_vars()


#ggplot2::ggplot(tab_betaslope, aes(exped, CHL, fill=bruvs)) + ggplot2::geom_boxplot()


##### BETASLOPE MODEL -----
# clean data with predictor variables for betaslope
tab_betaslope <- clean_data_with_vars_betaslope(tab)


#make correlogram of predictors
cor <- make_correlogram_vars(tab_betaslope)



# fit gls with betaslope as response -------------------------------------


#fit gls with no autocorrelation
mod_no_cor_betaslope <- fit_gls_no_cor_betaslope(tab_betaslope)


#compare gls models with different basic autocorrelation structures (form=~1)
mod_basic_cor_betaslope <- compare_gls_basic_cor_betaslope(tab_betaslope)


#compare gls models with different basic autocorrelation structures (form=~long+lat)
mod_spatial_cor_betaslope <- compare_gls_spatial_cor_betaslope(tab_betaslope)


#compare aic of gls models
AIC(mod_no_cor_betaslope, mod_basic_cor_betaslope, mod_spatial_cor_betaslope)
#mod_basic_cor is the best model


#get model diagnostics
get_gls_diagnostics(tab_betaslope, mod_basic_cor_betaslope, "mod_basic_cor_betaslope")
get_gls_diagnostics(tab_betaslope, mod_no_cor_betaslope, "mod_no_cor_betaslope")
get_gls_diagnostics(tab_betaslope, mod_spatial_cor_betaslope, "mod_spatial_cor_betaslope")


#get partial plot of best model (mod_basic_cor)
get_partial_plot_betaslope(tab_betaslope, mod_basic_cor_betaslope, "mod_basic_cor", c("logBathy", "Slope",  "logDistP", "logDistSM", "GovernmentEffectiveness_mean"))


#fit fully saturated model with basic autocorrelation structure


mod_sat_betaslope <- nlme::gls(betaslope ~ bruvs*(protection_use + logBathy + Slope + logDistP  + logDistSM + logDistCR+ logTTM+logSST+ SST_sd +logCHL+ GovernmentEffectiveness_mean), data = tab_betaslope, correlation = nlme::corRatio(form=~1))


summary(mod_sat_betaslope)

#model performance

get_adj_r2(mod_sat_betaslope)


#get partial plot of saturated model - need to update with name of actual variables

get_partial_plot(tab_betaslope, mod_sat_betaslope, "mod_sat_betaslope", c("protection_use" ,"logBathy","Slope", "logDistP", "logDistSM","logDistCR","logTTM", "SST_sd", "logCHL", "logSST" ,"GovernmentEffectiveness_mean"))

coef_plot(mod_sat_betaslope, "mod_sat_betaslope")


#fit simplified model----


mod_sim_betaslope <- nlme::gls(betaslope ~ bruvs*(logBathy +logCHL+ Slope + logTTM + SST_sd+ logDistSM + GovernmentEffectiveness_mean), data = tab_betaslope, correlation = nlme::corRatio(form=~1))


summary(mod_sim_betaslope)

# get partial plot of simplified model - need to update with name of actual variables

get_partial_plot(tab_betaslope, mod_sim_betaslope, "mod_sim_betaslope", c("logBathy","Slope","logCHL", "logDistSM","SST_sd", "logTTM", "GovernmentEffectiveness_mean"))


#standardized effect plot

coef_plot(mod_sim_betaslope, "mod_sim_betaslope")

#model performance

get_adj_r2(mod_sim_betaslope)



##### MEDIAN MAXSIZE MODEL -----

# clean data with predictor variables for maxsize

tab_median_maxsize <- clean_data_with_vars_median_maxsize(tab)

#fit fully saturated model with basic autocorrelation structure----

mod_sat_median_maxsize <- nlme::gls(median_maxsize ~ bruvs*(logBathy + Slope + logDistP + logTTM + logDistSM + logDistCR+ logSST+ SST_sd + logCHL + GovernmentEffectiveness_mean+ protection_use), data = tab_median_maxsize, correlation = nlme::corRatio(form=~1))

summary(mod_sat_median_maxsize)
# get partial plot of saturated model - need to update with name of actual variables

get_partial_plot(tab_median_maxsize, mod_sat_median_maxsize, "mod_sat_median_maxsize", c("logBathy","Slope","logDistP","logTTM", "logDistSM","logDistCR", "logSST" ,"SST_sd", "logCHL", "GovernmentEffectiveness_mean","protection_use"))

#standardized effect plot

coef_plot(mod_sat_median_maxsize, "mod_sat_median_maxsize")

#model performance

get_adj_r2(mod_sat_median_maxsize)


#fit simplified model with basic autocorrelation structure----

mod_sim_median_maxsize <- nlme::gls(median_maxsize ~ bruvs*GovernmentEffectiveness_mean, data = tab_median_maxsize, correlation = nlme::corRatio(form=~1))

# get partial plot of simplified model - need to update with name of actual variables

get_partial_plot(tab_median_maxsize, mod_sim_median_maxsize, "mod_sim_median_maxsize", c("GovernmentEffectiveness_mean"))

#standardized effect plot

coef_plot(mod_sim_median_maxsize, "mod_sim_median_maxsize")

#model performance

get_adj_r2(mod_sim_median_maxsize)


##### MEAN MAXSIZE MODEL -----

# clean data with predictor variables for maxsize

tab_mean_maxsize <- clean_data_with_vars_mean_maxsize(tab)

#fit fully saturated model with basic autocorrelation structure----

mod_sat_mean_maxsize <- nlme::gls(mean_maxsize ~ bruvs*(logBathy + Slope + logDistP + logTTM + logDistSM + logDistCR+ logSST+ SST_sd + logCHL+ GovernmentEffectiveness_mean+ protection_use), data = tab_mean_maxsize, correlation = nlme::corRatio(form=~1))

summary(mod_sat_mean_maxsize)
# get partial plot of saturated model - need to update with name of actual variables

get_partial_plot(tab_mean_maxsize, mod_sat_mean_maxsize, "mod_sat_mean_maxsize", c("logBathy","Slope","logDistP", "logDistSM","logDistCR", "logSST" ,"logCHL", "GovernmentEffectiveness_mean","protection_use"))

#standardized effect plot

coef_plot(mod_sat_mean_maxsize, "mod_sat_mean_maxsize")

#model performance

get_adj_r2(mod_sat_mean_maxsize)


#fit simplified model with basic autocorrelation structure----

mod_sim_mean_maxsize <- nlme::gls(mean_maxsize ~ bruvs*GovernmentEffectiveness_mean, data = tab_mean_maxsize, correlation = nlme::corRatio(form=~1))

# get partial plot of saturated model - need to update with name of actual variables

get_partial_plot(tab_mean_maxsize, mod_sim_mean_maxsize, "mod_sim_mean_maxsize", c("GovernmentEffectiveness_mean"))

#standardized effect plot

coef_plot(mod_sim_mean_maxsize, "mod_sim_mean_maxsize")

#model performance

get_adj_r2(mod_sim_mean_maxsize)



##### MEDIAN MODEL -----

# clean data with predictor variables for maxsize

tab_mediansize <- clean_data_with_vars_median(tab)


#fit fully saturated model with basic autocorrelation structure----

mod_sat_mediansize <- nlme::gls(mean_mediansize ~ bruvs*(logBathy + Slope + logDistP + logDistSM + logDistCR+ logSST+ GovernmentEffectiveness_mean), data = tab_mediansize, correlation = nlme::corRatio(form=~1))

# get partial plot of saturated model - need to update with name of actual variables

partial_plot_sat_mediansize <- get_partial_plot(tab_mediansize, mod_sat_mediansize, "mod_sat_mediansize", c("logBathy","Slope","logDistP", "logDistSM","logDistCR", "logSST" ,"GovernmentEffectiveness_mean"))

#standardized effect plot

coef_plot_mediansize <- coef_plot(mod_sat_mediansize, "mod_sat_mediansize")

summary(mod_sat_mediansize)

#model performance

get_adj_r2(mod_sat_mediansize)


#fit simplified model with basic autocorrelation structure----

mod_sim_mediansize <- nlme::gls(mean_mediansize ~ bruvs*(logBathy + logDistP + logSST+ GovernmentEffectiveness_mean), data = tab_mediansize, correlation = nlme::corRatio(form=~1))

# get partial plot of saturated model - need to update with name of actual variables

partial_plot_sim_mediansize <- get_partial_plot(tab_mediansize, mod_sim_mediansize, "mod_sim_mediansize", c("logBathy","logDistP", "logSST","GovernmentEffectiveness_mean"))

#standardized effect plot

coef_plot_mediansize <- coef_plot(mod_sim_mediansize, "mod_sim_mediansize")

#model performance

get_adj_r2(mod_sim_mediansize)

##### FIRST MODE MODEL -----

# clean data with predictor variables for first mode

tab_first_mode <- clean_data_with_vars_first_mode(tab)

#fit fully saturated model with basic autocorrelation structure

mod_sat_first_mode <- nlme::gls(first_mode ~ bruvs*(protection_use + logBathy + Slope + logDistP  + logDistSM + logDistCR+ logTTM+logSST+ SST_sd +logCHL+ GovernmentEffectiveness_mean), data = tab_first_mode, correlation = nlme::corRatio(form=~1))


summary(mod_sat_first_mode)

#model performance

get_adj_r2(mod_sat_first_mode)


#get partial plot of saturated model - need to update with name of actual variables

get_partial_plot(tab_first_mode, mod_sat_first_mode, "mod_sat_first_mode", c("protection_use" ,"logBathy","Slope", "logDistP", "logDistSM","logDistCR","logTTM", "SST_sd", "logCHL", "logSST" ,"GovernmentEffectiveness_mean"))

#standardized effect plot

coef_plot(mod_sat_first_mode, "mod_sat_first_mode")


##### SECOND MODE MODEL -----

# clean data with predictor variables for second mode

tab_second_mode <- clean_data_with_vars_second_mode(tab)

#fit fully saturated model with basic autocorrelation structure

mod_sat_second_mode <- nlme::gls(second_mode ~ bruvs*(protection_use + logBathy + Slope + logDistP  + logDistSM + logDistCR+ logTTM+logSST+ SST_sd +logCHL+ GovernmentEffectiveness_mean), data = tab_second_mode, correlation = nlme::corRatio(form=~1))


summary(mod_sat_second_mode)

#model performance

get_adj_r2(mod_sat_second_mode)


#get partial plot of saturated model - need to update with name of actual variables

get_partial_plot(tab_second_mode, mod_sat_second_mode, "mod_sat_second_mode", c("protection_use" ,"logBathy","Slope", "logDistP", "logDistSM","logDistCR","logTTM", "SST_sd", "logCHL", "logSST" ,"GovernmentEffectiveness_mean"))

#standardized effect plot

coef_plot(mod_sat_second_mode, "mod_sat_second_mode")

