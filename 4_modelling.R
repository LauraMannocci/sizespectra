#load all functions
devtools::load_all()
devtools::document()


# read and clean data ----------------------------


# read data with predictor variables
tab <- read_data_with_vars()


# clean data with predictor variables
tab <- clean_data_with_vars(tab)


#make correlogram of predictors
cor <- make_correlogram_vars(tab)






# fit gls with betaslope as response -------------------------------------


#fit gls with no autocorrelation
mod_no_cor <- fit_gls_no_cor_betaslope(tab)


#compare gls models with different basic autocorrelation structures (form=~1)
mod_basic_cor <- compare_gls_basic_cor_betaslope(tab)


#compare gls models with different basic autocorrelation structures (form=~long+lat)
mod_spatial_cor <- compare_gls_spatial_cor_betaslope(tab)


#compare aic of gls models
AIC(mod_no_cor, mod_basic_cor, mod_spatial_cor)
#mod_basic_cor is the best model


#get model diagnostics
get_gls_diagnostics(tab, mod_basic_cor, "mod_basic_cor")
get_gls_diagnostics(tab, mod_no_cor, "mod_no_cor")
get_gls_diagnostics(tab, mod_spatial_cor, "mod_spatial_cor")


#get partial plot of best model (mod_basic_cor)
get_partial_plot_betaslope(tab, mod_basic_cor, "mod_basic_cor", c("logBathy", "Slope",  "logDistP", "logDistSM", "GovernmentEffectiveness_mean"))



