#load all functions
devtools::document()
devtools::load_all()


# read and clean data ----------------------------


# read data with predictor variables
tab <- read_data_with_vars()


#ggplot2::ggplot(tab_betaslope, aes(exped, CHL, fill=bruvs)) + ggplot2::geom_boxplot()


###DEFINE CONDITION----
condition = c(logBathy = 1.7, logDistCR = 5, logSST = 1.4, logDistP = 1, logCHL=.10, logDistC = 4, SST_sd = 0.7, logTTM = 3, GovernmentEffectiveness_mean = 1.5, Slope =88)


###BETASLOPE MODEL -----
# clean data with predictor variables for betaslope
tab_betaslope <- clean_data_with_vars(tab, "betaslope")

#envar range to set for marg----

var_logBathy <- bruvs_var_range(tab_betaslope, "logBathy")
var_logDistCR <- bruvs_var_range(tab_betaslope, "logDistCR")
var_logDistSM <- bruvs_var_range(tab_betaslope, "logDistSM")
var_logDistP <- bruvs_var_range(tab_betaslope, "logDistP")
var_logDistC <- bruvs_var_range(tab_betaslope, "logDistC")
var_logTTM <- bruvs_var_range(tab_betaslope, "logTTM")
var_logCHL <- bruvs_var_range(tab_betaslope, "logCHL")
var_logSST <- bruvs_var_range(tab_betaslope, "logSST")
var_SST_sd <- bruvs_var_range(tab_betaslope, "SST_sd")
var_SST_GEm <- bruvs_var_range(tab_betaslope, "GovernmentEffectiveness_mean")

#make multiplot
envar_multiplot <- multi_envar_range() 

#envar range interaction to set for marg----

prot_var_logBathy <-  bruvs_protect_var(tab_betaslope, "logBathy")  # logBathy = 1.5
prot_var_logDistCR <- bruvs_protect_var(tab_betaslope, "logDistCR") # logDistCR = 4.5
prot_var_logDistSM <- bruvs_protect_var(tab_betaslope, "logDistSM") # logDistSM = 5
prot_var_logDistP <-  bruvs_protect_var(tab_betaslope, "logDistP")  # logDistP = 1.7
prot_var_logDistC <-  bruvs_protect_var(tab_betaslope, "logDistC")  # logDistC = 4
prot_var_logTTM <-    bruvs_protect_var(tab_betaslope, "logTTM")    # logTTM  = 3
prot_var_logCHL <-    bruvs_protect_var(tab_betaslope, "logCHL")    # logCHL = 0.10 
prot_var_logSST <-    bruvs_protect_var(tab_betaslope, "logSST")    # logSST = 1.4
prot_var_SST_sd <-    bruvs_protect_var(tab_betaslope, "SST_sd")    # SST_sd = 0.7
prot_var_GEm    <-    bruvs_protect_var(tab_betaslope, "GovernmentEffectiveness_mean") # GovernmentEffectiveness_mean = 1
prot_var_Slope  <-    bruvs_protect_var(tab_betaslope, "Slope") # Slope = 1

#make multiplot
envar_multiplot_cat <- multi_envar_range_cat() 

#make correlogram of predictors
cor <- make_correlogram_vars(tab_betaslope)

# fit saturated betaslope mode

#betaslope fully saturated model with basic autocorrelation structure----
mod_sat_betaslope <- fit_gls_sat_cor_betaslope(tab_betaslope)

#get model diagnostics
get_gls_diagnostics(tab_betaslope, mod_sat_betaslope, "mod_sat_betaslope")

#model performance
get_adj_r2(mod_sat_betaslope)

#coef plot
coef_plot(mod_sat_betaslope, "mod_sat_betaslope")


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
coef_plot_signif_terms(mod_sim_betaslope, "mod_sim_betaslope", 0.05)

 #betaslope marginal plot of simplified model----
#mod_sim_betaslope <- mod_sim_betaslope


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


TTMBetaslope_bruvs <- marg_plot_cat_covar_separate_bruvs(response = "beta_slope", mod_name = "mod_sim_betaslope", dat = tab_betaslope, mod= mod_sim_betaslope, var = "logTTM [all]", var_name = "logTTM", group = "bruvs", group2 = "protection_use", condition = condition)



##### FIRST MODE MODEL -----

# clean data with predictor variables for first mode

tab_firstmode <- clean_data_with_vars(tab, "logFirstmode")

#firstmode fully saturated model with basic autocorrelation structure----

mod_sat_firstmode <- fit_gls_sat_cor_firstmode(tab_firstmode)

#model performance

get_adj_r2(mod_sat_firstmode)

#get model diagnostics
get_gls_diagnostics(tab_firstmode, mod_sat_firstmode, "mod_sat_firstmode")


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
coef_plot_signif_terms(mod_sim_firstmode, "mod_sim_firstmode", 0.05)

#firstmode marginal plot of simplified model----
#mod_sim_firstmode <- mod_sat_firstmode


#firstmode marginal plots of simplified model - need to update with name of actual variables

#simple marginal plot
marg_plot_log(response = "logFirstmode", mod_name = "mod_sim_firstmode", dat = tab_firstmode, mod= mod_sim_firstmode, var = "logBathy [all]", var_name = "logBathy", group = "bruvs", condition =condition)
marg_plot_log(response = "logFirstmode", mod_name = "mod_sim_firstmode", dat = tab_firstmode, mod= mod_sim_firstmode, var = "logDistP [all]", var_name = "logDistP", group = "bruvs", condition =condition)
marg_plot_log(response = "logFirstmode", mod_name = "mod_sim_firstmode", dat = tab_firstmode, mod= mod_sim_firstmode, var = "logTTM [all]", var_name = "logTTM", group = "bruvs", condition =condition)


#interaction marginal plot 
marg_plot_cat_covar_noextra(response = "logFirstmode", mod_name = "mod_sim_firstmode", dat = tab_firstmode, mod= mod_sim_firstmode, var = "logBathy [all]", var_name = "logBathy", group = "bruvs", group2 = "protection_use", condition = condition)
distPFirstmode <- marg_plot_cat_covar_noextra(response = "logFirstmode", mod_name = "mod_sim_firstmode", dat = tab_firstmode, mod= mod_sim_firstmode, var = "logDistP [all]", var_name = "logDistP", group = "bruvs", group2 = "protection_use", condition = condition)
SSTFirstmode <- marg_plot_cat_covar_noextra(response = "logFirstmode", mod_name = "mod_sim_firstmode", dat = tab_firstmode, mod= mod_sim_firstmode, var = "logSST [all]", var_name = "logSST", group = "bruvs", group2 = "protection_use", condition = condition)
TTMFirstmode <- marg_plot_cat_covar_noextra(response = "logFirstmode", mod_name = "mod_sim_firstmode", dat = tab_firstmode, mod= mod_sim_firstmode, var = "logTTM [all]", var_name = "logTTM", group = "bruvs", group2 = "protection_use", condition = condition)
SST_sdFirstmode <- marg_plot_cat_covar_noextra(response = "logFirstmode", mod_name = "mod_sim_firstmode", dat = tab_firstmode, mod= mod_sim_firstmode, var = "SST_sd [all]", var_name = "SST_sd", group = "bruvs", group2 = "protection_use", condition = condition)

TTMFirstmode_bruvs <- marg_plot_cat_covar_separate_bruvs(response = "logFirstmode", mod_name = "mod_sim_firstmode", dat = tab_firstmode, mod= mod_sim_firstmode, var = "logTTM [all]", var_name = "logTTM", group = "bruvs", group2 = "protection_use", condition = condition)



#multiple categorical terms
ProtFirstmode <- marg_plot_cat_catvar(response= "logFirstmode", mod_name = "mod_sim_firstmode", tab_firstmode, mod_sim_firstmode, "protection_use [all]", "protection_use", "bruvs", condition = condition)



##### SECOND MODE MODEL -----

# clean data with predictor variables for second mode

tab_secondmode <- clean_data_with_vars(tab, "logSecondmode")


#secondmode fully saturated model with basic autocorrelation structure----

mod_sat_secondmode <- fit_gls_sat_cor_secondmode(tab_secondmode)


#model performance

get_adj_r2(mod_sat_secondmode)

#standardized effect plot

coef_plot(mod_sat_secondmode, "mod_sat_secondmode")

#get model diagnostics
get_gls_diagnostics(tab_secondmode, mod_sat_secondmode, "mod_sat_secondmode")



#secondmode simplified model----

##### stepAIC on saturated model CAREFULL takes a long time
mod_sim_secondmode <- MASS::stepAIC(mod_sat_secondmode)


# or just fit simplied model directly - need to update if the saturated model changes

mod_sim_secondmode <- fit_gls_sim_cor_secondmode(tab_secondmode)

#standardized effect plot

coef_plot(mod_sim_secondmode, "mod_sim_secondmode")

#standardized effect plot with significant terms of interest
coef_plot_signif_terms(mod_sim_secondmode, "mod_sim_secondmode", 0.05)

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
marg_plot_log(response = "logSecondmode", mod_name = "mod_sim_secondmode", dat = tab_secondmode, mod= mod_sim_secondmode, var = "GovernmentEffectiveness_mean [all]", var_name = "GovernmentEffectiveness_mean", group = "bruvs", condition =condition)
marg_plot_log(response = "logSecondmode", mod_name = "mod_sim_secondmode", dat = tab_secondmode, mod= mod_sim_secondmode, var = "logCHL [all]", var_name = "logCHL", group = "bruvs", condition =condition)
marg_plot_log(response = "logSecondmode", mod_name = "mod_sim_secondmode", dat = tab_secondmode, mod= mod_sim_secondmode, var = "logDistC [all]", var_name = "logDistC", group = "bruvs", condition =condition)
marg_plot_log(response = "logSecondmode", mod_name = "mod_sim_secondmode", dat = tab_secondmode, mod= mod_sim_secondmode, var = "logBathy [all]", var_name = "logBathy", group = "bruvs", condition =condition)
marg_plot_log(response = "logSecondmode", mod_name = "mod_sim_secondmode", dat = tab_secondmode, mod= mod_sim_secondmode, var = "logDistCR [all]", var_name = "logDistCR", group = "bruvs", condition =condition)
marg_plot_log(response = "logSecondmode", mod_name = "mod_sim_secondmode", dat = tab_secondmode, mod= mod_sim_secondmode, var = "logDistP [all]", var_name = "logDistP", group = "bruvs", condition =condition)
marg_plot_log(response = "logSecondmode", mod_name = "mod_sim_secondmode", dat = tab_secondmode, mod= mod_sim_secondmode, var = "logSST [all]", var_name = "logSST", group = "bruvs", condition =condition)


#interaction marginal plot 

marg_plot_cat_covar_noextra(response = "logSecondmode", mod_name = "mod_sim_secondmode", dat = tab_secondmode, mod= mod_sim_secondmode, var = "logBathy [all]", var_name = "logBathy", group = "bruvs", group2 = "protection_use", condition = condition)
distPSecondmode <- marg_plot_cat_covar_noextra(response = "logSecondmode", mod_name = "mod_sim_secondmode", dat = tab_secondmode, mod= mod_sim_secondmode, var = "logDistP [all]", var_name = "logDistP", group = "bruvs", group2 = "protection_use", condition = condition)
SSTSecondmode <- marg_plot_cat_covar_noextra(response = "logSecondmode", mod_name = "mod_sim_secondmode", dat = tab_secondmode, mod= mod_sim_secondmode, var = "logSST [all]", var_name = "logSST", group = "bruvs", group2 = "protection_use", condition = condition)
CHLSecondmode <- marg_plot_cat_covar_noextra(response = "logSecondmode", mod_name = "mod_sim_secondmode", dat = tab_secondmode, mod= mod_sim_secondmode, var = "logCHL [all]", var_name = "logCHL", group = "bruvs", group2 = "protection_use", condition = condition)
marg_plot_cat_covar_noextra(response = "logSecondmode", mod_name = "mod_sim_secondmode", dat = tab_secondmode, mod= mod_sim_secondmode, var = "logDistC [all]", var_name = "logDistC", group = "bruvs", group2 = "protection_use", condition = condition)
marg_plot_cat_covar_noextra(response = "logSecondmode", mod_name = "mod_sim_secondmode", dat = tab_secondmode, mod= mod_sim_secondmode, var = "GovernmentEffectiveness_mean [all]", var_name = "GovernmentEffectiveness_mean", group = "bruvs", group2 = "protection_use", condition = condition)
TTMSecondmode <- marg_plot_cat_covar_noextra(response = "logSecondmode", mod_name = "mod_sim_secondmode", dat = tab_secondmode, mod= mod_sim_secondmode, var = "logTTM [all]", var_name = "logTTM", group = "bruvs", group2 = "protection_use", condition = condition)


TTMSecondmode_bruvs <- marg_plot_cat_covar_separate_bruvs(response = "logSecondmode", mod_name = "mod_sim_secondmode", dat = tab_secondmode, mod= mod_sim_secondmode, var = "logTTM [all]", var_name = "logTTM", group = "bruvs", group2 = "protection_use", condition = condition)


#multiple categorical terms
ProtSecondmode <- marg_plot_cat_catvar(response= "logSecondmode", mod_name = "mod_sim_secondmode", tab_secondmode, mod_sim_secondmode, "protection_use [all]", "protection_use", "bruvs", condition = condition)


### ALL MODEL COMBINED-----
#TTM
multi_covariate_marg(TTMFirstmode, TTMSecondmode,TTMBetaslope, "TTM")
multi_covariate_marg_bruvs(TTMFirstmode_bruvs, TTMSecondmode_bruvs,TTMBetaslope_bruvs, "TTM")


 #TTM with conceptual 
conceptual_marg_mod(TTMFirstmode, TTMSecondmode,TTMBetaslope, "TTM")
conceptual_marg_bruvs(TTMFirstmode_bruvs, TTMSecondmode_bruvs,TTMBetaslope_bruvs, "TTM")




   #distPort
multi_covariate_marg(distPFirstmode, distPSecondmode, distPBetaslope, "distP")

#protection
multi_covariate_marg_cat(ProtFirstmode, ProtSecondmode, ProtBetaslope, "Prot")

#SST
multi_covariate_marg(SSTFirstmode+coord_cartesian(xlim =c(1.2, 1.5)), SSTSecondmode+coord_cartesian(xlim=c(1.2, 1.5)), SSTBetaslope+coord_cartesian(xlim=c(1.2, 1.5)), "SST")

#SST_sd
multi_covariate_marg_SST_sd(SST_sdFirstmode, SST_sdBetaslope, "SST_sd")




## LINEAR REGRESSION RESPONSE-----

response_vs_response(tab_firstmode)
mode_vs_mode_lm(tab_firstmode)
mode_lm_res_beta(tab_firstmode)

tab_firstmode2 <- subset(tab_firstmode, betaslope > -4,)

tapply(tab_firstmode$logFirstmode, tab_firstmode$bruvs, summary)

mode_lm_res_beta_Secondmode(tab_firstmode2)



