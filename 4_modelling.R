#load all functions
devtools::document()
devtools::load_all()


# read and clean data ----------------------------


# read data with predictor variables
tab <- read_data_with_vars()


#ggplot2::ggplot(tab_betaslope, aes(exped, CHL, fill=bruvs)) + ggplot2::geom_boxplot()


###DEFINE CONDITION----
#original model
#condition = c(logBathy = 1.7, logDistCR = 5, logSST = 1.4, logDistSM = 4.5, logDistP = 1.2, logCHL=.10, logDistC = 4, SST_sd = 0.7, logTTM = 3, GovernmentEffectiveness_mean = 1.5, Slope =88)

condition = c(logBathy = 1.7, logDistCR = 5, logSST = 1.45, logDistSM = 4.5, logDistP = 1.2, logCHL=.08, logDistC = 4, logTTM = 3, Slope =88)

###BETASLOPE MODEL -----
# clean data with predictor variables for betaslope
tab_betaslope <- clean_data_with_vars(tab, "betaslope")

#truncate to three standard deviation
tab_betaslope <- truncate_data(tab_betaslope, "betaslope", 3)

#envar range to set for marg----

var_logBathy <-  bruvs_var_range(tab_firstmode, "logBathy")
var_logDistCR <- bruvs_var_range(tab_firstmode, "logDistCR")
var_logDistSM <- bruvs_var_range(tab_firstmode, "logDistSM")
var_logDistP <-  bruvs_var_range(tab_firstmode, "logDistP")
var_logDistC <-  bruvs_var_range(tab_firstmode, "logDistC")
var_logTTM <-    bruvs_var_range(tab_firstmode, "logTTM")
var_logCHL <-    bruvs_var_range(tab_firstmode, "logCHL")
var_logSST <-    bruvs_var_range(tab_firstmode, "logSST")
var_SST_sd <-    bruvs_var_range(tab_firstmode, "SST_sd")
var_SST_GEm <-   bruvs_var_range(tab_firstmode, "GovernmentEffectiveness_mean")

#make multiplot
envar_multiplot <- multi_envar_range() 

#envar range interaction to set for marg----

prot_var_logBathy <-  bruvs_protect_var(tab_betaslope, "logBathy", 1.7)  # logBathy = 1.5
prot_var_logDistCR <- bruvs_protect_var(tab_betaslope, "logDistCR", 5) # logDistCR = 4.5
prot_var_logDistSM <- bruvs_protect_var(tab_betaslope, "logDistSM", 4.5) # logDistSM = 5
prot_var_logDistP <-  bruvs_protect_var(tab_betaslope, "logDistP", 1.2)  # logDistP = 1.7
prot_var_logDistC <-  bruvs_protect_var(tab_betaslope, "logDistC", 4)  # logDistC = 4
prot_var_logTTM <-    bruvs_protect_var(tab_betaslope, "logTTM", 3)    # logTTM  = 3
prot_var_logCHL <-    bruvs_protect_var(tab_betaslope, "logCHL",.06)    # logCHL = 0.10 
prot_var_logSST <-    bruvs_protect_var(tab_betaslope, "logSST", 1.4)    # logSST = 1.4
prot_var_Slope  <-    bruvs_protect_var(tab_betaslope, "Slope", 88) # Slbetaslope#make multiplot
envar_multiplot_cat <- multi_envar_range_cat() 

#make correlogram of predictors
cor <- make_correlogram_vars(tab_betaslope)


# fit saturated betaslope mode

#betaslope fully saturated model with basic autocorrelation structure----
mod_sat_betaslope <- fit_gls_sat_cor_betaslope(tab_betaslope)

#variance inflation factor
vif(mod_sat_betaslope) #suggest government effectiveness and SST_sd are problematic at VIF >10

#get model diagnostics
get_gls_diagnostics(tab_betaslope, mod_sat_betaslope, "mod_sat_betaslope")

#model performance
get_adj_r2(mod_sat_betaslope)

#coef plot
coef_plot(mod_sat_betaslope, "mod_sat_betaslope")

coef_plot_signif_terms(mod_sat_betaslope, "mod_sat_betaslope ", 0.05)



##### stepAIC on saturated model CAREFULL takes a long time
mod_sim_betaslope <- MASS::stepAIC(mod_sat_betaslope)

#betaslope simplified model----
##faster, but needs updating if the saturated model form has changed

mod_sim_betaslope <- fit_gls_sim_cor_betaslope(tab_betaslope)

#model performance
get_adj_r2(mod_sim_betaslope) 

#get model diagnostic
get_gls_diagnostics(tab_betaslope, mod_sim_betaslope, "mod_sim_betaslope")


#standardized effect plot
coef_plot(mod_sim_betaslope, "mod_sim_betaslope")

#standardized effect plot with significant terms of interest
coef_plot_signif_terms(mod_sim_betaslope, "GLS size-spectra slope", 0.05)

 #betaslope marginal plot of simplified model----
#mod_sim_betaslope <- mod_sat_betaslope


#bathy marg with zoom
marg_plot_bathy(response = "beta_slope", mod_name = "mod_sim_betaslope", dat = tab_betaslope, mod= mod_sim_betaslope, var = "logBathy [all]", var_name = "logBathy", group = "bruvs")

#log marg
marg_plot_log(response = "beta_slope", mod_name = "mod_sim_betaslope", dat = tab_betaslope, mod= mod_sim_betaslope, var = "logTTM [all]", var_name = "logTTM", group = "bruvs", condition =condition)
marg_plot_log(response = "beta_slope", mod_name = "mod_sim_betaslope", dat = tab_betaslope, mod= mod_sim_betaslope, var = "GovernmentEffectiveness_mean [all]", var_name = "GovernmentEffectiveness_mean", group = "bruvs", condition = condition)
marg_plot_log(response = "beta_slope", mod_name = "mod_sim_betaslope", dat = tab_betaslope, mod= mod_sim_betaslope, var = "SST_sd [all]", var_name = "SST_sd", group = "bruvs", condition =condition)
marg_plot_log(response = "beta_slope", mod_name = "mod_sim_betaslope", dat = tab_betaslope, mod= mod_sim_betaslope, var = "logSST [all]", var_name = "logSST", group = "bruvs", condition =condition)
marg_plot_log(response = "beta_slope", mod_name = "mod_sim_betaslope", dat = tab_betaslope, mod= mod_sim_betaslope, var = "Slope [all]", var_name = "Slope", group = "bruvs", condition =condition)
marg_plot_log(response = "beta_slope", mod_name = "mod_sim_betaslope", dat = tab_betaslope, mod= mod_sim_betaslope, var = "logDistCR [all]", var_name = "logDistCR", group = "bruvs", condition =condition)
marg_plot_log(response = "beta_slope", mod_name = "mod_sim_betaslope", dat = tab_betaslope, mod= mod_sim_betaslope, var = "logDistP [all]", var_name = "logDistP", group = "bruvs", condition =condition)

#nonlog marg (set as "not_protected")
#marg_plot(response = "beta_slope", mod_name = "mod_sim_betaslope", dat = tab_betaslope, mod= mod_sim_betaslope, var = "SST_sd [all]", var_name = "SST_sd", group = "bruvs")

#multiple interaction terms extrapolates
#marg_plot_cat_covar(response = "beta_slope", mod_name = "mod_sim_betaslope", dat = tab_betaslope, mod= mod_sim_betaslope, var = "protection_use [all]", var_name = "protection_use", group = "bruvs", group2 = "protection_use", condition =condition)

#multiple categorical terms
ProtBetaslope <- marg_plot_cat_catvar(response= "beta_slope", mod_name = "mod_sim_betaslope", tab_betaslope, mod_sim_betaslope, "protection_use [all]", "protection_use", "bruvs", condition = condition)

#multiple interaction terms with - no extrapolation

#bathy
marg_plot_cat_covar_noextra(response = "beta_slope", mod_name = "mod_sim_betaslope", dat = tab_betaslope, mod= mod_sim_betaslope, var = "logBathy [all]", var_name = "logBathy", group = "bruvs", group2 = "protection_use", condition = condition)
marg_plot_cat_covar_noextra(response = "beta_slope", mod_name = "mod_sim_betaslope", dat = tab_betaslope, mod= mod_sim_betaslope, var = "logDistC [all]", var_name = "logDistC", group = "bruvs", group2 = "protection_use", condition = condition)
marg_plot_cat_covar_noextra(response = "beta_slope", mod_name = "mod_sim_betaslope", dat = tab_betaslope, mod= mod_sim_betaslope, var = "logDistCR [all]", var_name = "logDistCR", group = "bruvs", group2 = "protection_use", condition = condition)
marg_plot_cat_covar_noextra(response = "beta_slope", mod_name = "mod_sim_betaslope", dat = tab_betaslope, mod= mod_sim_betaslope, var = "Slope [all]", var_name = "Slope", group = "bruvs", group2 = "protection_use", condition = condition)

#envar
marg_plot_cat_covar_noextra(response = "beta_slope", mod_name = "mod_sim_betaslope", dat = tab_betaslope, mod= mod_sim_betaslope, var = "logCHL [all]", var_name = "logCHL", group = "bruvs", group2 = "protection_use", condition = condition)
SSTBetaslope <- marg_plot_cat_covar_noextra(response = "beta_slope", mod_name = "mod_sim_betaslope", dat = tab_betaslope, mod= mod_sim_betaslope, var = "logSST [all]", var_name = "logSST", group = "bruvs", group2 = "protection_use", condition = condition)
SST_sdBetaslope <- marg_plot_cat_covar_noextra(response = "beta_slope", mod_name = "mod_sim_betaslope", dat = tab_betaslope, mod= mod_sim_betaslope, var = "SST_sd [all]", var_name = "SST_sd", group = "bruvs", group2 = "protection_use", condition = condition)

#social
marg_plot_cat_covar_noextra(response = "beta_slope", mod_name = "mod_sim_betaslope", dat = tab_betaslope, mod= mod_sim_betaslope, var = "GovernmentEffectiveness_mean [all]", var_name = "GovernmentEffectiveness_mean", group = "bruvs", group2 = "protection_use", condition = condition)
TTMBetaslope <- marg_plot_cat_covar_noextra(response = "beta_slope", mod_name = "mod_sim_betaslope", dat = tab_betaslope, mod= mod_sim_betaslope, var = "logTTM [all]", var_name = "logTTM", group = "bruvs", group2 = "protection_use", condition = condition)
distPBetaslope <- marg_plot_cat_covar_noextra(response = "beta_slope", mod_name = "mod_sim_betaslope", dat = tab_betaslope, mod= mod_sim_betaslope, var = "logDistP [all]", var_name = "logDistP", group = "bruvs", group2 = "protection_use", condition = condition)





##### FIRST MODE MODEL -----

# clean data with predictor variables for first mode

tab_firstmode <- clean_data_with_vars(tab, "logFirstmode")

#truncate to three standard deviation
tab_firstmode <- truncate_data(tab_firstmode, "logFirstmode", 3)


#firstmode fully saturated model with basic autocorrelation structure----

mod_sat_firstmode <- fit_gls_sat_cor_firstmode(tab_firstmode)

#variance inflation factor
vif(mod_sat_firstmode) #suggest government effectiveness and SST_sd are problematic at VIF >10

#model performance

get_adj_r2(mod_sat_firstmode)

#get model diagnostics
get_gls_diagnostics(tab_firstmode, mod_sat_firstmode, "mod_sat_firstmode")

coef_plot_signif_terms(mod_sat_firstmode, "mod_sat_firstmode ", 0.05)


#coef plot
coef_plot(mod_sat_firstmode, "mod_sat_firstmode")

#firstmode simplified model----

##### stepAIC on saturated model CAREFULL takes a long time
mod_sim_firstmode <- MASS::stepAIC(mod_sat_firstmode)

#or just fit directly

##faster, but needs updating if the saturated model form has changed
mod_sim_firstmode <- fit_gls_sim_cor_firstmode(tab_firstmode)


#model performance

get_adj_r2(mod_sim_firstmode)

#get model diagnostics
get_gls_diagnostics(tab_firstmode, mod_sim_firstmode, "mod_sim_firstmode")

#coef plot
coef_plot(mod_sim_firstmode, "mod_sim_firstmode")

#standardized effect plot with significant terms of interest
coef_plot_signif_terms(mod_sim_firstmode, "GLS body size of relatively small fishes ", 0.05)

#firstmode marginal plot of simplified model----
#mod_sim_firstmode <- mod_sat_firstmode


#firstmode marginal plots of simplified model - need to update with name of actual variables

#simple marginal plot
marg_plot_log(response = "logFirstmode", mod_name = "mod_sim_firstmode", dat = tab_firstmode, mod= mod_sim_firstmode, var = "logBathy [all]", var_name = "logBathy", group = "bruvs", condition =condition)
marg_plot_log(response = "logFirstmode", mod_name = "mod_sim_firstmode", dat = tab_firstmode, mod= mod_sim_firstmode, var = "logDistP [all]", var_name = "logDistP", group = "bruvs", condition =condition)
marg_plot_log(response = "logFirstmode", mod_name = "mod_sim_firstmode", dat = tab_firstmode, mod= mod_sim_firstmode, var = "GovernmentEffectiveness_mean [all]", var_name = "GovernmentEffectiveness_mean", group = "bruvs", condition =condition)


#interaction marginal plot 
logBathyFirstmode <- marg_plot_cat_covar_noextra(response = "logFirstmode", mod_name = "mod_sim_firstmode", dat = tab_firstmode, mod= mod_sim_firstmode, var = "logBathy [all]", var_name = "logBathy", group = "bruvs", group2 = "protection_use", condition = condition)
distPFirstmode <- marg_plot_cat_covar_noextra(response = "logFirstmode", mod_name = "mod_sim_firstmode", dat = tab_firstmode, mod= mod_sim_firstmode, var = "logDistP [all]", var_name = "logDistP", group = "bruvs", group2 = "protection_use", condition = condition)
SSTFirstmode <- marg_plot_cat_covar_noextra(response = "logFirstmode", mod_name = "mod_sim_firstmode", dat = tab_firstmode, mod= mod_sim_firstmode, var = "logSST [all]", var_name = "logSST", group = "bruvs", group2 = "protection_use", condition = condition)
SST_sdFirstmode <- marg_plot_cat_covar_noextra(response = "logFirstmode", mod_name = "mod_sim_firstmode", dat = tab_firstmode, mod= mod_sim_firstmode, var = "SST_sd [all]", var_name = "SST_sd", group = "bruvs", group2 = "protection_use", condition = condition)
CHL_Firstmode <- marg_plot_cat_covar_noextra(response = "logFirstmode", mod_name = "mod_sim_firstmode", dat = tab_firstmode, mod= mod_sim_firstmode, var = "logCHL [all]", var_name = "logCHL", group = "bruvs", group2 = "protection_use", condition = condition)
Gov_Firstmode <- marg_plot_cat_covar_noextra(response = "logFirstmode", mod_name = "mod_sim_firstmode", dat = tab_firstmode, mod= mod_sim_firstmode, var = "GovernmentEffectiveness_mean [all]", var_name = "GovernmentEffectiveness_mean", group = "bruvs", group2 = "protection_use", condition = condition)

#TTMFirstmode_bruvs <- marg_plot_cat_covar_separate_bruvs(response = "logFirstmode", mod_name = "mod_sim_firstmode", dat = tab_firstmode, mod= mod_sim_firstmode, var = "logTTM [all]", var_name = "logTTM", group = "bruvs", group2 = "protection_use", condition = condition)

TTMFirstmode <- marg_plot_cat_covar_noextra(response = "logFirstmode", mod_name = "mod_sim_firstmode", dat = tab_firstmode, mod= mod_sim_firstmode, var = "logTTM [all]", var_name = "logTTM", group = "bruvs", group2 = "protection_use", condition = condition)


#multiple categorical terms
ProtFirstmode <- marg_plot_cat_catvar(response= "logFirstmode", mod_name = "mod_sim_firstmode", tab_firstmode, mod_sim_firstmode, "protection_use [all]", "protection_use", "bruvs", condition = condition)



##### SECOND MODE MODEL -----

# clean data with predictor variables for second mode

tab_secondmode <- clean_data_with_vars(tab, "logSecondmode")

#truncate to three standard deviation

tab_secondmode <- truncate_data(tab_secondmode, "logSecondmode", 3)

#secondmode fully saturated model with basic autocorrelation structure----

mod_sat_secondmode <- fit_gls_sat_cor_secondmode(tab_secondmode)

#variance inflation factor
vif(mod_sat_secondmode) #suggest government effectiveness and SST_sd are problematic at VIF >10


#model performance

get_adj_r2(mod_sat_secondmode)

#standardized effect plot

coef_plot(mod_sat_secondmode, "mod_sat_secondmode")

#get model diagnostics
get_gls_diagnostics(tab_secondmode, mod_sat_secondmode, "mod_sat_secondmode")



#secondmode simplified model----

##### stepAIC on saturated model CAREFULL takes a long time
mod_sim_secondmode <- MASS::stepAIC(mod_sat_secondmode)



# or just fit simplied model directly - faster but needs updating if the saturated model changes

mod_sim_secondmode <- fit_gls_sim_cor_secondmode(tab_secondmode)

#standardized effect plot

coef_plot(mod_sim_secondmode, "mod_sim_secondmode")

#standardized effect plot with significant terms of interest
coef_plot_signif_terms(mod_sim_secondmode, "GLS body size of relatively large fishes", 0.05)

# model diagnostics
get_gls_diagnostics(tab_secondmode, mod_sim_secondmode, "mod_sim_secondmode")


#adjusted r2
get_adj_r2(mod_sim_secondmode)

#secondmode marginal plot of simplified model----
#mod_sim_secondmode <- mod_sat_secondmode

#define conditions

#secondmode marginal plots of simplified model - need to update with name of actual variables

#simple marginal plot
marg_plot_log(response = "logSecondmode", mod_name = "mod_sim_secondmode", dat = tab_secondmode, mod= mod_sim_secondmode, var = "logTTM [all]", var_name = "logTTM", group = "bruvs", condition =condition)
marg_plot_log(response = "logSecondmode", mod_name = "mod_sim_secondmode", dat = tab_secondmode, mod= mod_sim_secondmode, var = "logCHL [all]", var_name = "logCHL", group = "bruvs", condition =condition)
marg_plot_log(response = "logSecondmode", mod_name = "mod_sim_secondmode", dat = tab_secondmode, mod= mod_sim_secondmode, var = "logDistC [all]", var_name = "logDistC", group = "bruvs", condition =condition)
marg_plot_log(response = "logSecondmode", mod_name = "mod_sim_secondmode", dat = tab_secondmode, mod= mod_sim_secondmode, var = "logBathy [all]", var_name = "logBathy", group = "bruvs", condition =condition)
marg_plot_log(response = "logSecondmode", mod_name = "mod_sim_secondmode", dat = tab_secondmode, mod= mod_sim_secondmode, var = "logDistCR [all]", var_name = "logDistCR", group = "bruvs", condition =condition)
marg_plot_log(response = "logSecondmode", mod_name = "mod_sim_secondmode", dat = tab_secondmode, mod= mod_sim_secondmode, var = "logDistP [all]", var_name = "logDistP", group = "bruvs", condition =condition)
marg_plot_log(response = "logSecondmode", mod_name = "mod_sim_secondmode", dat = tab_secondmode, mod= mod_sim_secondmode, var = "logSST [all]", var_name = "logSST", group = "bruvs", condition =condition)
marg_plot_log(response = "logSecondmode", mod_name = "mod_sim_secondmode", dat = tab_secondmode, mod= mod_sim_secondmode, var = "logDistSM [all]", var_name = "logDistSM", group = "bruvs", condition =condition)


#interaction marginal plot 

marg_plot_cat_covar_noextra(response = "logSecondmode", mod_name = "mod_sim_secondmode", dat = tab_secondmode, mod= mod_sim_secondmode, var = "logBathy [all]", var_name = "logBathy", group = "bruvs", group2 = "protection_use", condition = condition)
distPSecondmode <- marg_plot_cat_covar_noextra(response = "logSecondmode", mod_name = "mod_sim_secondmode", dat = tab_secondmode, mod= mod_sim_secondmode, var = "logDistP [all]", var_name = "logDistP", group = "bruvs", group2 = "protection_use", condition = condition)
SSTSecondmode <- marg_plot_cat_covar_noextra(response = "logSecondmode", mod_name = "mod_sim_secondmode", dat = tab_secondmode, mod= mod_sim_secondmode, var = "logSST [all]", var_name = "logSST", group = "bruvs", group2 = "protection_use", condition = condition)
CHLSecondmode <- marg_plot_cat_covar_noextra(response = "logSecondmode", mod_name = "mod_sim_secondmode", dat = tab_secondmode, mod= mod_sim_secondmode, var = "logCHL [all]", var_name = "logCHL", group = "bruvs", group2 = "protection_use", condition = condition)
marg_plot_cat_covar_noextra(response = "logSecondmode", mod_name = "mod_sim_secondmode", dat = tab_secondmode, mod= mod_sim_secondmode, var = "logDistC [all]", var_name = "logDistC", group = "bruvs", group2 = "protection_use", condition = condition)
marg_plot_cat_covar_noextra(response = "logSecondmode", mod_name = "mod_sim_secondmode", dat = tab_secondmode, mod= mod_sim_secondmode, var = "GovernmentEffectiveness_mean [all]", var_name = "GovernmentEffectiveness_mean", group = "bruvs", group2 = "protection_use", condition = condition)
marg_plot_cat_covar_noextra(response = "logSecondmode", mod_name = "mod_sim_secondmode", dat = tab_secondmode, mod= mod_sim_secondmode, var = "SST_sd [all]", var_name = "SST_sd", group = "bruvs", group2 = "protection_use", condition = condition)
marg_plot_cat_covar_noextra(response = "logSecondmode", mod_name = "mod_sim_secondmode", dat = tab_secondmode, mod= mod_sim_secondmode, var = "logDistSM [all]", var_name = "logDistSM", group = "bruvs", group2 = "protection_use", condition = condition)



TTMSecondmode <- marg_plot_cat_covar_noextra(response = "logSecondmode", mod_name = "mod_sim_secondmode", dat = tab_secondmode, mod= mod_sim_secondmode, var = "logTTM [all]", var_name = "logTTM", group = "bruvs", group2 = "protection_use", condition = condition)


#TTMSecondmode_bruvs <- marg_plot_cat_covar_separate_bruvs(response = "logSecondmode", mod_name = "mod_sim_secondmode", dat = tab_secondmode, mod= mod_sim_secondmode, var = "logTTM [all]", var_name = "logTTM", group = "bruvs", group2 = "protection_use", condition = condition)


#multiple categorical terms
ProtSecondmode <- marg_plot_cat_catvar(response= "logSecondmode", mod_name = "mod_sim_secondmode", tab_secondmode, mod_sim_secondmode, "protection_use [all]", "protection_use", "bruvs", condition = condition)

### ALL MODEL COMBINED-----
#TTM
multi_covariate_marg(TTMFirstmode, TTMSecondmode,TTMBetaslope, "TTM")
multi_covariate_marg_bruvs(TTMFirstmode_bruvs, TTMSecondmode_bruvs,TTMBetaslope_bruvs, "TTM")

#TTM with conceptual 
conceptual_marg_mod(TTMFirstmode, TTMSecondmode,TTMBetaslope, "TTM")
conceptual_marg_bruvs(TTMFirstmode_bruvs, TTMSecondmode_bruvs,TTMBetaslope_bruvs, "TTM")



#extract prediction for each models and combine data at typical envar values for benthic and pelagic samples


pel <- subset(tab_firstmode, bruvs=="pelagic")#&protection_use=="not_protected")
ben <- subset(tab_firstmode, bruvs=="benthic")#&protection_use=="not_protected")

#pel = tab_firstmode
#ben = tab_firstmode

# with means specified 

condition_pel = c(logBathy = mean(pel$logBathy), logDistCR = mean(pel$logDistCR), logSST = mean(pel$logSST), logDistSM = mean(pel$logDistSM), logDistP =  1.2, logCHL= mean(pel$logCHL), logDistC = mean(pel$logDistC), Slope =mean(pel$Slope))
condition_ben = c(logBathy = mean(ben$logBathy), logDistCR = mean(ben$logDistCR), logSST = mean(ben$logSST), logDistSM = mean(ben$logDistSM), logDistP =  1.2, logCHL= mean(ben$logCHL), logDistC = mean(ben$logDistC), Slope =mean(ben$Slope))

pred_firstmode <- marg_data_bruvs_prot(response = "logFirstmode", dat = tab_firstmode, mod=mod_sim_firstmode, var = "logTTM [all]", var_name = "logTTM", group = "bruvs", group2 = "protection_use", condition = condition_pel)
pred_firstmode_pel <- subset(pred_firstmode, group=="Pelagic")# subset pelagic predictions
pred_firstmode <- marg_data_bruvs_prot(response = "logFirstmode", dat = tab_firstmode, mod=mod_sim_firstmode, var = "logTTM [all]", var_name = "logTTM", group = "bruvs", group2 = "protection_use", condition = condition_ben)
pred_firstmode_ben <- subset(pred_firstmode, group=="Benthic")# subset benthic predictions
# 
# pel <- subset(tab_secondmode, bruvs=="pelagic")
# ben <- subset(tab_secondmode, bruvs=="benthic")
pred_secondmode <- marg_data_bruvs_prot(response = "logSecondmode", dat = tab_secondmode, mod=mod_sim_secondmode, var = "logTTM [all]", var_name = "logTTM", group = "bruvs", group2 = "protection_use", condition = condition_pel)
pred_secondmode_pel <- subset(pred_secondmode, group=="Pelagic")
pred_secondmode <- marg_data_bruvs_prot(response = "logSecondmode", dat = tab_secondmode, mod=mod_sim_secondmode, var = "logTTM [all]", var_name = "logTTM", group = "bruvs", group2 = "protection_use", condition = condition_ben)
pred_secondmode_ben <- subset(pred_secondmode, group=="Benthic")
# 
# pel <- subset(tab_betaslope, bruvs=="pelagic")
# ben <- subset(tab_betaslope, bruvs=="benthic")
pred_betaslope <- marg_data_bruvs_prot(response = "beta_slope", dat = tab_betaslope, mod=mod_sim_betaslope, var = "logTTM [all]", var_name = "logTTM", group = "bruvs", group2 = "protection_use", condition = condition_pel)
pred_betaslope_pel <- subset(pred_betaslope, group=="Pelagic")
pred_betaslope <- marg_data_bruvs_prot(response = "beta_slope", dat = tab_betaslope, mod=mod_sim_betaslope, var = "logTTM [all]", var_name = "logTTM", group = "bruvs", group2 = "protection_use", condition = condition_ben)
pred_betaslope_ben <- subset(pred_betaslope, group=="Benthic")

pred_all <- rbind(pred_firstmode_pel,pred_firstmode_ben, pred_secondmode_pel,pred_secondmode_ben, pred_betaslope_pel,pred_betaslope_ben)

save_marg_pred_all(pred_all) # save marginal predictions

###plot marginal plot combined by protection level - works really well!

pred_all <- read_marg_pred_all() # read marginal predictions - saves rerunning models
marg_plot_bruvs_prot(pred=pred_all, var_name = "logTTM")






# #distPort
# multi_covariate_marg(distPFirstmode, distPSecondmode, distPBetaslope, "distP")
# #protection
# multi_covariate_marg_cat(ProtFirstmode, ProtSecondmode, ProtBetaslope, "Prot")
# #SST
# multi_covariate_marg(SSTFirstmode+coord_cartesian(xlim =c(1.2, 1.5)), SSTSecondmode+coord_cartesian(xlim=c(1.2, 1.5)), SSTBetaslope+coord_cartesian(xlim=c(1.2, 1.5)), "SST")
# #SST_sd
# multi_covariate_marg_SST_sd(SST_sdFirstmode, SST_sdBetaslope, "SST_sd")
# 



## LINEAR REGRESSION RESPONSE-----

response_vs_response(tab_firstmode)
mode_vs_mode_lm(tab_firstmode)
mode_lm_res_beta(tab_firstmode)

tab_firstmode2 <- subset(tab_firstmode, betaslope > -4,)

tapply(tab_firstmode$logFirstmode, tab_firstmode$bruvs, summary)

mode_lm_res_beta_Secondmode(tab_firstmode2)


ggplot(tab_firstmode)+ geom_point(aes(x =logTTM, y=logDistP, colour = bruvs))+ facet_wrap(~protection_use)


  geom_smooth(aes(x = betaslope, y=logSecondmode), method='lm')#+ 
  #geom_point(aes(x = betaslope, y=logSecondmode), colour = "BLUE")



ggplot(tab_firstmode)+ geom_point(aes(x = logSecondmode, y=logFirstmode))#+
  #geom_smooth(aes(x = betaslope, y=logSecondmode), method='lm')#+ 
#geom_point(aes(x = betaslope, y=logSecondmode), colour = "BLUE")



#Moran's I

moran_i_test(mod_sim_betaslope, tab_betaslope)
moran_i_test(mod_sim_firstmode, tab_firstmode)
moran_i_test(mod_sim_secondmode, tab_secondmode)


#################### SENSIVITY ANALYSIS (ie sub-sampling historical occurrences to sample size of modern occurrences)  -------------------------------------

his_coef_sens_all <- list()
his_metrics_sens_all <- list()
his_pred_all <- raster::stack()

for (i in 1:10){
  
  print(paste("sensitivity step", i))
  
  ######  Random sample of historical occurrences
  
  #tab = data to test
  
  indices <- sample(1:nrow(mod), size = (nrow(tab)*.9), replace = F)
  tab.z <- tab[indices,]
  
  
  
  ######  PREPARATION OF 
  

  
  
  ######  MODEL EVALUATION
  

  # saturated model fitting
  mod_sat_betaslope.x <- fit_gls_sat_cor_betaslope(tab_betaslope)
  
  
  ######  MODEL SELECTION AND OPTIMISATION
  
  # select best model
  mod_sim_betaslope.x <- MASS::stepAIC(mod_sat_betaslope)
  
  # fill in metrics list
  his_metrics_sens_all[[i]] <- his_metrics_sens
  
  # We can select a single model from the ENMevaluation object using the tune.args of our optimal model
  his_mod_sens <- eval.models(SPHis_sens.x)[[his_metrics_sens$tune.args]]
  
  
  
  ######  MODEL COEFFICIENTS AND PARTIAL PLOTS
  
  # Here are the non-zero coefficients in our model.
  his_coef_sens <- tibble::enframe(his_mod_sens$betas)
  his_coef_sens$type <- factor('Historical')
  his_coef_sens$step <- factor(i)
  
  # fill in coef list
  his_coef_sens_all[[i]] <- his_coef_sens
  
  
  
  ######  MODEL PREDICTIONS
  
  # make predictions from selected model
  predHis_sens <- eval.predictions(SPHis_sens.x)[[his_metrics_sens$tune.args]]
  
  # stack prediction rasters
  his_pred_all <- raster::stack(his_pred_all, predHis_sens)
  
}




#### convert lists to dataframes and write csv

his_coef_sens_all <- do.call(rbind.data.frame, his_coef_sens_all)
write.csv(his_coef_sens_all, here::here("outputs", "historical_coefficients_sensitivity.csv"))

his_metrics_sens_all <- do.call(rbind.data.frame, his_metrics_sens_all)
write.csv(his_metrics_sens_all, here::here("outputs", "historical_metrics_best_models_sensitivity.csv"))

mean(his_metrics_sens_all$auc.val.avg) #0.6508068
sd(his_metrics_sens_all$auc.val.avg) #0.009309328




### make coefficient barplot

# preprocessing

his_coef_sens_all %>%
  dplyr::mutate(name = as.factor(name)) %>%
  dplyr::group_by(name) %>%
  dplyr::summarise(mean_value = mean(value),
                   sd = sd(value)) %>%
  dplyr::mutate(type = 'Historical') %>%
  dplyr::mutate(type = as.factor(type)) %>%
  dplyr::rename('value' = mean_value) -> his_coef_sens_all2

coefMod %>%
  dplyr::mutate(name = as.factor(name)) %>%
  dplyr::mutate(sd = 0) %>%
  dplyr::select(-type) %>%
  dplyr::mutate(type = 'Modern') %>%
  dplyr::mutate(type = as.factor(type)) -> coefMod2

coefs <- rbind(his_coef_sens_all2, coefMod2)

make_coefficients_barplot_sensitivity(coefs, c(-0.004, 0.002))






### plot mean and sd historical predictions with mpa and extrap extent

# calculate mean and sd predictions

his_pred_mean <- raster::calc(his_pred_all, fun = mean)
his_pred_sd <- raster::calc(his_pred_all, fun = sd)

# plot

plot_mean_sd_historical_predictions_with_extra_mpas(wio, his_pred_mean, his_pred_sd, df_extraHis, mpa_sf)



