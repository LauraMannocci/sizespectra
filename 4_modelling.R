#load all functions
devtools::document()
devtools::load_all()


# read and clean response variables ----------------------------


# read data with predictor variables
tab <- read_data_with_vars()


#ggplot2::ggplot(tab_betaslope, aes(exped, CHL, fill=bruvs)) + ggplot2::geom_boxplot()

# clean data with predictor variables for betaslope
tab_betaslope <- clean_data_with_vars(tab, "betaslope")

#truncate to three standard deviation
tab_betaslope <- truncate_data(tab_betaslope, "betaslope", 3)


ggplot()+geom_jitter(data = tab_betaslope, aes(logTTM, GovernmentEffectiveness_mean, colour = bruvs, size =betaslope), alpha = .4)+
  scale_colour_manual(values = c("pelagic" = '#077DAA', 'benthic' = 'orange'))+theme_light()

# clean data with predictor variables for first mode
tab_firstmode <- clean_data_with_vars(tab, "logFirstmode")

#truncate to three standard deviation
tab_firstmode <- truncate_data(tab_firstmode, "logFirstmode", 3)


# clean data with predictor variables for second mode
tab_secondmode <- clean_data_with_vars(tab, "logSecondmode")

#truncate to three standard deviation
tab_secondmode <- truncate_data(tab_secondmode, "logSecondmode", 3)


###Fig S4 response variable  ----
response_fig_new(dat=tab_betaslope, tab_first=tab_firstmode, tab_second=tab_secondmode, min_size= 0.001)

#response_fig(fl_pelagic_benthic_meta, tab_betaslope,tab_firstmode, 
             #tab_secondmode, min_size= 0.001, lat_band =15, bandw = 0.2, scale= 30, alpha=0.3)


###Fig S5 environmental variables range  ----

#envar range to set for marg----

var_logBathy <-  bruvs_var_range(tab_firstmode, "logBathy", "Seabed depth, log10(m)")
var_logDistCR <- bruvs_var_range(tab_firstmode, "logDistCR", "Distance to coral reef, log10(m)")
var_logDistSM <- bruvs_var_range(tab_firstmode, "logDistSM", "Distance to seamount, log10(m)")
var_logDistP <-  bruvs_var_range(tab_firstmode, "logDistP", "Distance to port, log10(m)")
var_logDistC <-  bruvs_var_range(tab_firstmode, "logDistC", "Distance to coast, log10(m)")
var_logTTM <-    bruvs_var_range(tab_firstmode, "logTTM", "Travel time to market, log10(min)")
var_logCHL <-    bruvs_var_range(tab_firstmode, "logCHL", "Chlorophyll-a concenctration, log10(mg.m-3)")
var_logSST <-    bruvs_var_range(tab_firstmode, "logSST", "Sea-surface temperature, log10(°C)")
var_slope <-     bruvs_var_range(tab_firstmode, "Slope", "Slope, (°)")

#var_SST_sd <-    bruvs_var_range(tab_firstmode, "SST_sd")
#var_SST_GEm <-   bruvs_var_range(tab_firstmode, "GovernmentEffectiveness_mean")

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
prot_var_SST_sd  <-    bruvs_protect_var(tab_betaslope, "SST_sd", 88) # Slbetaslope#make multiplot


envar_multiplot_cat <- multi_envar_range_cat() 

###BETASLOPE MODEL -----

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
coef_plot_signif_terms(mod_sim_betaslope, "GLS size spectra slope", 0.05)

 #Fig. S6 betaslope marginal plot of simplified model----
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

#Fig. S7 firstmode marginal plot of simplified model----
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

coef_plot_signif_terms(mod_sat_secondmode, "mod_sat_secondmode ", 0.05)


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

#Fig. S8 secondmode marginal plot of simplified model----
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

### Fig. 4  ALL MODEL COMBINED----
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

###plot marginal plot combined by protection level -

pred_all <- read_marg_pred_all() # read marginal predictions - saves rerunning models
marg_plot_bruvs_prot(pred=pred_all, var_name = "logTTM", rug_beta = tab_betaslope)





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


## SENSIVITY ANALYSIS 90% sub-sampling data----


#betaslope sensitivity analysis 90%  -------------------------------------

mod_pred_sens_all <- list()



for (i in 1:10){
  
  print(paste("sensitivity step", i))
  

  #tab_clean = data to test cleaned for size-based indicator
  tab_clean = tab_betaslope
  #var = size-based indicator
  #var_res = betaslope
  var_res_name = "beta_slope"
  
  ######  Random sample of data
  

  indices <- sample(1:nrow(tab_clean), size = (nrow(tab_clean)*.9), replace = F)
  tab_z <- tab_clean[indices,]
  
  
  
  ######  PREPARATION OF MODELS


  # saturated model fitting
  mod_sat.x <- nlme::gls(betaslope ~ bruvs * protection_use * (logTTM +logDistP) + bruvs * (logDistSM+logDistC +logBathy+ logDistCR + Slope +
                                                                                        poly(logSST,2) + poly(logCHL,2)), data = tab_z,  correlation = nlme::corRatio(form=~1),method='ML') 

  ######  MODEL MARGINAL PREDICTIONS
  # make marginal plot predictions from selected model
  # with means specified 
  
  pel <- subset(tab_z, bruvs=="pelagic")#&protection_use=="not_protected")
  ben <- subset(tab_z, bruvs=="benthic")#&protection_use=="not_protected")
  
  condition_pel = c(logBathy = mean(pel$logBathy), logDistCR = mean(pel$logDistCR), logSST = mean(pel$logSST), logDistSM = mean(pel$logDistSM), logDistP =  1.2, logCHL= mean(pel$logCHL), logDistC = mean(pel$logDistC), Slope =mean(pel$Slope))
  condition_ben = c(logBathy = mean(ben$logBathy), logDistCR = mean(ben$logDistCR), logSST = mean(ben$logSST), logDistSM = mean(ben$logDistSM), logDistP =  1.2, logCHL= mean(ben$logCHL), logDistC = mean(ben$logDistC), Slope =mean(ben$Slope))
  
  pred_mod_sens <- marg_data_bruvs_prot(response = var_res_name, dat = tab_z, mod=mod_sat.x, var = "logTTM [all]", var_name = "logTTM", group = "bruvs", group2 = "protection_use", condition = condition_pel)
  pred_mod_sens_pel <- subset(pred_mod_sens, group=="Pelagic")# subset pelagic predictions
  pred_mod_sens <- marg_data_bruvs_prot(response = var_res_name, dat = tab_z, mod=mod_sat.x, var = "logTTM [all]", var_name = "logTTM", group = "bruvs", group2 = "protection_use", condition = condition_ben)
  pred_mod_sens_ben <- subset(pred_mod_sens, group=="Benthic")# subset benthic predictions
  
  pred_mod_sens <- rbind(pred_mod_sens_pel, pred_mod_sens_ben)
  pred_mod_sens$sens_step <- i
  
  # fill in predictions
  
  mod_pred_sens_all[[i]] <- pred_mod_sens[,1:9]
}

#### convert lists to dataframes and write csv
mod_pred_sens_betaslope <- do.call(rbind.data.frame, mod_pred_sens_all)
write.csv(mod_pred_sens_betaslope, here::here("outputs", "model_outputs", "sensitivity_betaslope.csv"))




#################### logfirstmode sensitivity analysis 90%  -------------------------------------

mod_pred_sens_all <- list()



for (i in 1:10){
  
  print(paste("sensitivity step", i))
  
  
  #tab_clean = data to test cleaned for size-based indicator
  tab_clean <- tab_firstmode
  #var = size-based indicator
  #var_res = betaslope
  var_res_name = "logFirstmode"
  
  ######  Random sample of data
  
  indices <- sample(1:nrow(tab_clean), size = (nrow(tab_clean)*.9), replace = F)
  tab_z <- tab_clean[indices,]
  
  
  
  ######  PREPARATION OF MODELS
  
  
  # saturated model fitting
  mod_sat.x <- nlme::gls(logFirstmode ~ bruvs * protection_use * (logTTM +logDistP) + bruvs * (logDistSM+logDistC +logBathy+ logDistCR + Slope +
                                                                                              poly(logSST,2) + poly(logCHL,2)), data = tab_z,  correlation = nlme::corRatio(form=~1),method='ML') 
  
  ######  MODEL MARGINAL PREDICTIONS
  # make marginal plot predictions from selected model
  # with means specified 
  
  pel <- subset(tab_z, bruvs=="pelagic")#&protection_use=="not_protected")
  ben <- subset(tab_z, bruvs=="benthic")#&protection_use=="not_protected")
  
  condition_pel = c(logBathy = mean(pel$logBathy), logDistCR = mean(pel$logDistCR), logSST = mean(pel$logSST), logDistSM = mean(pel$logDistSM), logDistP =  1.2, logCHL= mean(pel$logCHL), logDistC = mean(pel$logDistC), Slope =mean(pel$Slope))
  condition_ben = c(logBathy = mean(ben$logBathy), logDistCR = mean(ben$logDistCR), logSST = mean(ben$logSST), logDistSM = mean(ben$logDistSM), logDistP =  1.2, logCHL= mean(ben$logCHL), logDistC = mean(ben$logDistC), Slope =mean(ben$Slope))
  
  pred_mod_sens <- marg_data_bruvs_prot(response = var_res_name, dat = tab_z, mod=mod_sat.x, var = "logTTM [all]", var_name = "logTTM", group = "bruvs", group2 = "protection_use", condition = condition_pel)
  pred_mod_sens_pel <- subset(pred_mod_sens, group=="Pelagic")# subset pelagic predictions
  pred_mod_sens <- marg_data_bruvs_prot(response = var_res_name, dat = tab_z, mod=mod_sat.x, var = "logTTM [all]", var_name = "logTTM", group = "bruvs", group2 = "protection_use", condition = condition_ben)
  pred_mod_sens_ben <- subset(pred_mod_sens, group=="Benthic")# subset benthic predictions
  
  pred_mod_sens <- rbind(pred_mod_sens_pel, pred_mod_sens_ben)
  pred_mod_sens$sens_step <- i
  
  # fill in predictions
  
  mod_pred_sens_all[[i]] <- pred_mod_sens[,1:9]
}

#### convert lists to dataframes and write csv
mod_pred_sens_firstmode <- do.call(rbind.data.frame, mod_pred_sens_all)
write.csv(mod_pred_sens_firstmode, here::here("outputs", "model_outputs", "sensitivity_firstmode.csv"))



#################### logsecondmode sensitivity analysis 90%  -------------------------------------

mod_pred_sens_all <- list()



for (i in 1:10){
  
  print(paste("sensitivity step", i))
  
  
  #tab_clean = data to test cleaned for size-based indicator
  tab_clean <- tab_secondmode
  #var = size-based indicator
  var_res_name = "logSecondmode"
  
  ######  Random sample of data
  

  indices <- sample(1:nrow(tab_clean), size = (nrow(tab_clean)*.9), replace = F)
  tab_z <- tab_clean[indices,]
  
  
  
  ######  PREPARATION OF MODELS
  
  
  # saturated model fitting
  mod_sat.x <- nlme::gls(logSecondmode ~ bruvs * protection_use * (logTTM +logDistP) + bruvs * (logDistSM+logDistC +logBathy+ logDistCR + Slope +
                                                                                                 poly(logSST,2) + poly(logCHL,2)), data = tab_z,  correlation = nlme::corRatio(form=~1),method='ML') 
  
  ######  MODEL MARGINAL PREDICTIONS
  # make marginal plot predictions from selected model
  # with means specified 
  
  pel <- subset(tab_z, bruvs=="pelagic")#&protection_use=="not_protected")
  ben <- subset(tab_z, bruvs=="benthic")#&protection_use=="not_protected")
  
  condition_pel = c(logBathy = mean(pel$logBathy), logDistCR = mean(pel$logDistCR), logSST = mean(pel$logSST), logDistSM = mean(pel$logDistSM), logDistP =  1.2, logCHL= mean(pel$logCHL), logDistC = mean(pel$logDistC), Slope =mean(pel$Slope))
  condition_ben = c(logBathy = mean(ben$logBathy), logDistCR = mean(ben$logDistCR), logSST = mean(ben$logSST), logDistSM = mean(ben$logDistSM), logDistP =  1.2, logCHL= mean(ben$logCHL), logDistC = mean(ben$logDistC), Slope =mean(ben$Slope))
  
  pred_mod_sens <- marg_data_bruvs_prot(response = var_res_name, dat = tab_z, mod=mod_sat.x, var = "logTTM [all]", var_name = "logTTM", group = "bruvs", group2 = "protection_use", condition = condition_pel)
  pred_mod_sens_pel <- subset(pred_mod_sens, group=="Pelagic")# subset pelagic predictions
  pred_mod_sens <- marg_data_bruvs_prot(response = var_res_name, dat = tab_z, mod=mod_sat.x, var = "logTTM [all]", var_name = "logTTM", group = "bruvs", group2 = "protection_use", condition = condition_ben)
  pred_mod_sens_ben <- subset(pred_mod_sens, group=="Benthic")# subset benthic predictions
  
  pred_mod_sens <- rbind(pred_mod_sens_pel, pred_mod_sens_ben)
  pred_mod_sens$sens_step <- i
  
  # fill in predictions
  
  mod_pred_sens_all[[i]] <- pred_mod_sens[,1:9]
}

#### convert lists to dataframes and write csv
mod_pred_sens_secondmode <- do.call(rbind.data.frame, mod_pred_sens_all)
write.csv(mod_pred_sens_secondmode, here::here("outputs", "model_outputs", "sensitivity_secondmode.csv"))



################## Fig. S9 marginal plot sensitivity  90% -------

#merge
pred_all_sens <- rbind(mod_pred_sens_firstmode,mod_pred_sens_secondmode,mod_pred_sens_betaslope)

save_marg_pred_all_sens(pred_all_sens) # save marginal predictions

###plot marginal plot combined by protection level 

pred_all_sens <- read_marg_pred_all_sens() # read marginal predictions


marg_plot_bruvs_prot_sens(pred=pred_all_sens, var_name = "logTTM")


## SENSIVITY ANALYSIS drop ocean----


#betaslope sensitivity drop ocean -------------------------------------



  ######  PREPARATION OF MODELS
  
  
  # saturated model fitting

###drop atlantic ocean samples------
var_res_name = "betaslope"

  tab_betaslope_atlantic <- subset(tab_betaslope, TERRITORY1!="Ascension" & TERRITORY1!="Azores" & TERRITORY1!="Tristan" & TERRITORY1!="Norway" & TERRITORY1!="Tristan da Cunha")
  mod_sat.atlantic <- nlme::gls(betaslope ~ bruvs * protection_use * (logTTM +logDistP) + bruvs * (logDistSM+logDistC +logBathy+ logDistCR + Slope +
                                                                                             poly(logSST,2) + poly(logCHL,2)), data = tab_betaslope_atlantic,  correlation = nlme::corRatio(form=~1),method='ML') 

  ######  MODEL MARGINAL PREDICTIONS
  # make marginal plot predictions from selected model with means specified 
  pel <- subset(tab_betaslope_atlantic, bruvs=="pelagic")#&protection_use=="not_protected")
  ben <- subset(tab_betaslope_atlantic, bruvs=="benthic")#&protection_use=="not_protected")
  condition_pel = c(logBathy = mean(pel$logBathy), logDistCR = mean(pel$logDistCR), logSST = mean(pel$logSST), logDistSM = mean(pel$logDistSM), logDistP =  1.2, logCHL= mean(pel$logCHL), logDistC = mean(pel$logDistC), Slope =mean(pel$Slope))
  condition_ben = c(logBathy = mean(ben$logBathy), logDistCR = mean(ben$logDistCR), logSST = mean(ben$logSST), logDistSM = mean(ben$logDistSM), logDistP =  1.2, logCHL= mean(ben$logCHL), logDistC = mean(ben$logDistC), Slope =mean(ben$Slope))
  pred_mod_sens <- marg_data_bruvs_prot(response = var_res_name, dat = tab_betaslope_atlantic, mod=mod_sat.atlantic, var = "logTTM [all]", var_name = "logTTM", group = "bruvs", group2 = "protection_use", condition = condition_pel)
  pred_mod_sens_pel <- subset(pred_mod_sens, group=="Pelagic")# subset pelagic predictions
  pred_mod_sens <- marg_data_bruvs_prot(response = var_res_name, dat = tab_betaslope_atlantic, mod=mod_sat.atlantic, var = "logTTM [all]", var_name = "logTTM", group = "bruvs", group2 = "protection_use", condition = condition_ben)
  pred_mod_sens_ben <- subset(pred_mod_sens, group=="Benthic")# subset benthic predictions
  pred_mod_sens_atlantic <- rbind(pred_mod_sens_pel, pred_mod_sens_ben)
  pred_mod_sens_atlantic$sens_ocean <- 'drop_atlantic'
  
###  drop indian ocean samples-----
  
  tab_betaslope_indian_aus <- subset(tab_betaslope, TERRITORY1!="Chagos Archipelago" & TERRITORY1!="Sudan" & TERRITORY1!="Cocos Islands" & TERRITORY1!="Maldives" & TERRITORY1!= "Australia")
  tab_betaslope_indian_FNQ <- subset(tab_betaslope, Exped.=="FNQ_2018" | Exped.=="FNQ_2017_11" | Exped.=="FNQ_2017_06" | Exped.=="FNQ Nov 2017" | Exped.=="FNQ Jun 2017")
  tab_betaslope_indian <- rbind(tab_betaslope_indian_aus, tab_betaslope_indian_FNQ)
  mod_sat.indian <- nlme::gls(betaslope ~ bruvs * protection_use * (logTTM +logDistP) + bruvs * (logDistSM+logDistC +logBathy+ logDistCR + Slope +
                                                                                                     poly(logSST,2) + poly(logCHL,2)), data = tab_betaslope_indian,  correlation = nlme::corRatio(form=~1),method='ML') 
  ######  MODEL MARGINAL PREDICTIONS
  # make marginal plot predictions from selected model with means specified 
  pel <- subset(tab_betaslope_indian, bruvs=="pelagic")
  ben <- subset(tab_betaslope_indian, bruvs=="benthic")
  condition_pel = c(logBathy = mean(pel$logBathy), logDistCR = mean(pel$logDistCR), logSST = mean(pel$logSST), logDistSM = mean(pel$logDistSM), logDistP =  1.2, logCHL= mean(pel$logCHL), logDistC = mean(pel$logDistC), Slope =mean(pel$Slope))
  condition_ben = c(logBathy = mean(ben$logBathy), logDistCR = mean(ben$logDistCR), logSST = mean(ben$logSST), logDistSM = mean(ben$logDistSM), logDistP =  1.2, logCHL= mean(ben$logCHL), logDistC = mean(ben$logDistC), Slope =mean(ben$Slope))
  pred_mod_sens <- marg_data_bruvs_prot(response = var_res_name, dat = tab_betaslope_indian, mod=mod_sat.indian, var = "logTTM [all]", var_name = "logTTM", group = "bruvs", group2 = "protection_use", condition = condition_pel)
  pred_mod_sens_pel <- subset(pred_mod_sens, group=="Pelagic")# subset pelagic predictions
  pred_mod_sens <- marg_data_bruvs_prot(response = var_res_name, dat = tab_betaslope_indian, mod=mod_sat.indian, var = "logTTM [all]", var_name = "logTTM", group = "bruvs", group2 = "protection_use", condition = condition_ben)
  pred_mod_sens_ben <- subset(pred_mod_sens, group=="Benthic")# subset benthic predictions
  pred_mod_sens_indian <- rbind(pred_mod_sens_pel, pred_mod_sens_ben)
  pred_mod_sens_indian$sens_ocean <- 'drop_indian'
  
###  drop pacific ocean samples----
   
  tab_betaslope_pacific_aus <- subset(tab_betaslope, TERRITORY1!="Clipperton Island" & TERRITORY1!="Costa Rica" & TERRITORY1!="Clipperton Island" & TERRITORY1!="Palau" & TERRITORY1!= "French Polynesia" & TERRITORY1!="Fiji" & TERRITORY1!="Niue" & TERRITORY1!= "New Caledonia" & TERRITORY1!= "Galapagos" & TERRITORY1!= "Mexico" & TERRITORY1!= "Columbia")
  tab_betaslope_pacific <- subset(tab_betaslope_pacific_aus, Exped.!="FNQ_2018" & Exped.!="FNQ_2017_11" & Exped.!="FNQ_2017_06" & Exped.!="FNQ Nov 2017" & Exped.!="FNQ Jun 2017")
  mod_sat.pacific <- nlme::gls(betaslope ~ bruvs * protection_use * (logTTM +logDistP) + bruvs * (logDistSM+logDistC +logBathy+ logDistCR + Slope +
                                                                                                     poly(logSST,2) + poly(logCHL,2)), data = tab_betaslope_pacific,  correlation = nlme::corRatio(form=~1),method='ML') 
  
  ######  MODEL MARGINAL PREDICTIONS
  # make marginal plot predictions from selected model with means specified 
  pel <- subset(tab_betaslope_pacific, bruvs=="pelagic")#&protection_use=="not_protected")
  ben <- subset(tab_betaslope_pacific, bruvs=="benthic")#&protection_use=="not_protected")
  condition_pel = c(logBathy = mean(pel$logBathy), logDistCR = mean(pel$logDistCR), logSST = mean(pel$logSST), logDistSM = mean(pel$logDistSM), logDistP =  1.2, logCHL= mean(pel$logCHL), logDistC = mean(pel$logDistC), Slope =mean(pel$Slope))
  condition_ben = c(logBathy = mean(ben$logBathy), logDistCR = mean(ben$logDistCR), logSST = mean(ben$logSST), logDistSM = mean(ben$logDistSM), logDistP =  1.2, logCHL= mean(ben$logCHL), logDistC = mean(ben$logDistC), Slope =mean(ben$Slope))
  pred_mod_sens <- marg_data_bruvs_prot(response = var_res_name, dat = tab_betaslope_pacific, mod=mod_sat.pacific, var = "logTTM [all]", var_name = "logTTM", group = "bruvs", group2 = "protection_use", condition = condition_pel)
  pred_mod_sens_pel <- subset(pred_mod_sens, group=="Pelagic")# subset pelagic predictions
  pred_mod_sens <- marg_data_bruvs_prot(response = var_res_name, dat = tab_betaslope_pacific, mod=mod_sat.pacific, var = "logTTM [all]", var_name = "logTTM", group = "bruvs", group2 = "protection_use", condition = condition_ben)
  pred_mod_sens_ben <- subset(pred_mod_sens, group=="Benthic")# subset benthic predictions
  pred_mod_sens_pacific <- rbind(pred_mod_sens_pel, pred_mod_sens_ben)
  pred_mod_sens_pacific$sens_ocean <- 'drop_pacific'
  

#combined all predictions with dropped oceans
  
  pred_sens_betaslope_oceans <- rbind(pred_mod_sens_indian, pred_mod_sens_atlantic, pred_mod_sens_pacific)
  



# logfirstmode sensitivity drop ocean -------------------------------------


######  PREPARATION OF MODELS


# saturated model fitting
###drop atlantic ocean samples------
var_res_name = "logFirstmode"

tab_firstmode_atlantic <- subset(tab_firstmode, TERRITORY1!="Ascension" & TERRITORY1!="Azores" & TERRITORY1!="Tristan" & TERRITORY1!="Norway" & TERRITORY1!="Tristan da Cunha")
mod_sat.atlantic <- nlme::gls(logFirstmode ~ bruvs * protection_use * (logTTM +logDistP) + bruvs * (logDistSM+logDistC +logBathy+ logDistCR + Slope +
                                                                                                   poly(logSST,2) + poly(logCHL,2)), data = tab_firstmode_atlantic,  correlation = nlme::corRatio(form=~1),method='ML') 

######  MODEL MARGINAL PREDICTIONS
# make marginal plot predictions from selected model with means specified 
pel <- subset(tab_firstmode_atlantic, bruvs=="pelagic")
ben <- subset(tab_firstmode_atlantic, bruvs=="benthic")
condition_pel = c(logBathy = mean(pel$logBathy), logDistCR = mean(pel$logDistCR), logSST = mean(pel$logSST), logDistSM = mean(pel$logDistSM), logDistP =  1.2, logCHL= mean(pel$logCHL), logDistC = mean(pel$logDistC), Slope =mean(pel$Slope))
condition_ben = c(logBathy = mean(ben$logBathy), logDistCR = mean(ben$logDistCR), logSST = mean(ben$logSST), logDistSM = mean(ben$logDistSM), logDistP =  1.2, logCHL= mean(ben$logCHL), logDistC = mean(ben$logDistC), Slope =mean(ben$Slope))
pred_mod_sens <- marg_data_bruvs_prot(response = var_res_name, dat = tab_firstmode_atlantic, mod=mod_sat.atlantic, var = "logTTM [all]", var_name = "logTTM", group = "bruvs", group2 = "protection_use", condition = condition_pel)
pred_mod_sens_pel <- subset(pred_mod_sens, group=="Pelagic")# subset pelagic predictions
pred_mod_sens <- marg_data_bruvs_prot(response = var_res_name, dat = tab_betaslope_atlantic, mod=mod_sat.atlantic, var = "logTTM [all]", var_name = "logTTM", group = "bruvs", group2 = "protection_use", condition = condition_ben)
pred_mod_sens_ben <- subset(pred_mod_sens, group=="Benthic")# subset benthic predictions
pred_mod_sens_atlantic <- rbind(pred_mod_sens_pel, pred_mod_sens_ben)
pred_mod_sens_atlantic$sens_ocean <- 'drop_atlantic'

###  drop indian ocean samples-----

tab_firstmode_indian_aus <- subset(tab_firstmode, TERRITORY1!="Chagos Archipelago" & TERRITORY1!="Sudan" & TERRITORY1!="Cocos Islands" & TERRITORY1!="Maldives" & TERRITORY1!= "Australia")
tab_firstmode_indian_FNQ <- subset(tab_firstmode, Exped.=="FNQ_2018" | Exped.=="FNQ_2017_11" | Exped.=="FNQ_2017_06" | Exped.=="FNQ Nov 2017" | Exped.=="FNQ Jun 2017")
tab_firstmode_indian <- rbind(tab_firstmode_indian_aus, tab_firstmode_indian_FNQ)
mod_sat.indian <- nlme::gls(logFirstmode ~ bruvs * protection_use * (logTTM +logDistP) + bruvs * (logDistSM+logDistC +logBathy+ logDistCR + Slope +
                                                                                                 poly(logSST,2) + poly(logCHL,2)), data = tab_firstmode_indian,  correlation = nlme::corRatio(form=~1),method='ML') 
######  MODEL MARGINAL PREDICTIONS
# make marginal plot predictions from selected model with means specified 
pel <- subset(tab_firstmode_indian, bruvs=="pelagic")#&protection_use=="not_protected")
ben <- subset(tab_firstmode_indian, bruvs=="benthic")#&protection_use=="not_protected")
condition_pel = c(logBathy = mean(pel$logBathy), logDistCR = mean(pel$logDistCR), logSST = mean(pel$logSST), logDistSM = mean(pel$logDistSM), logDistP =  1.2, logCHL= mean(pel$logCHL), logDistC = mean(pel$logDistC), Slope =mean(pel$Slope))
condition_ben = c(logBathy = mean(ben$logBathy), logDistCR = mean(ben$logDistCR), logSST = mean(ben$logSST), logDistSM = mean(ben$logDistSM), logDistP =  1.2, logCHL= mean(ben$logCHL), logDistC = mean(ben$logDistC), Slope =mean(ben$Slope))
pred_mod_sens <- marg_data_bruvs_prot(response = var_res_name, dat = tab_firstmode_indian, mod=mod_sat.indian, var = "logTTM [all]", var_name = "logTTM", group = "bruvs", group2 = "protection_use", condition = condition_pel)
pred_mod_sens_pel <- subset(pred_mod_sens, group=="Pelagic")# subset pelagic predictions
pred_mod_sens <- marg_data_bruvs_prot(response = var_res_name, dat = tab_firstmode_indian, mod=mod_sat.indian, var = "logTTM [all]", var_name = "logTTM", group = "bruvs", group2 = "protection_use", condition = condition_ben)
pred_mod_sens_ben <- subset(pred_mod_sens, group=="Benthic")# subset benthic predictions
pred_mod_sens_indian <- rbind(pred_mod_sens_pel, pred_mod_sens_ben)
pred_mod_sens_indian$sens_ocean <- 'drop_indian'

###  drop pacific ocean samples----

tab_firstmode_pacific_aus <- subset(tab_firstmode, TERRITORY1!="Clipperton Island" & TERRITORY1!="Costa Rica" & TERRITORY1!="Clipperton Island" & TERRITORY1!="Palau" & TERRITORY1!= "French Polynesia" & TERRITORY1!="Fiji" & TERRITORY1!="Niue" & TERRITORY1!= "New Caledonia" & TERRITORY1!= "Galapagos" & TERRITORY1!= "Mexico" & TERRITORY1!= "Columbia")
tab_firstmode_pacific <- subset(tab_firstmode_pacific_aus, Exped.!="FNQ_2018" & Exped.!="FNQ_2017_11" & Exped.!="FNQ_2017_06" & Exped.!="FNQ Nov 2017" & Exped.!="FNQ Jun 2017")
mod_sat.pacific <- nlme::gls(logFirstmode ~ bruvs * protection_use * (logTTM +logDistP) + bruvs * (logDistSM+logDistC +logBathy+ logDistCR + Slope +
                                                                                                  poly(logSST,2) + poly(logCHL,2)), data = tab_firstmode_pacific,  correlation = nlme::corRatio(form=~1),method='ML') 

######  MODEL MARGINAL PREDICTIONS
# make marginal plot predictions from selected model with means specified 
pel <- subset(tab_firstmode_pacific, bruvs=="pelagic")#&protection_use=="not_protected")
ben <- subset(tab_firstmode_pacific, bruvs=="benthic")#&protection_use=="not_protected")
condition_pel = c(logBathy = mean(pel$logBathy), logDistCR = mean(pel$logDistCR), logSST = mean(pel$logSST), logDistSM = mean(pel$logDistSM), logDistP =  1.2, logCHL= mean(pel$logCHL), logDistC = mean(pel$logDistC), Slope =mean(pel$Slope))
condition_ben = c(logBathy = mean(ben$logBathy), logDistCR = mean(ben$logDistCR), logSST = mean(ben$logSST), logDistSM = mean(ben$logDistSM), logDistP =  1.2, logCHL= mean(ben$logCHL), logDistC = mean(ben$logDistC), Slope =mean(ben$Slope))
pred_mod_sens <- marg_data_bruvs_prot(response = var_res_name, dat = tab_firstmode_pacific, mod=mod_sat.pacific, var = "logTTM [all]", var_name = "logTTM", group = "bruvs", group2 = "protection_use", condition = condition_pel)
pred_mod_sens_pel <- subset(pred_mod_sens, group=="Pelagic")# subset pelagic predictions
pred_mod_sens <- marg_data_bruvs_prot(response = var_res_name, dat = tab_firstmode_pacific, mod=mod_sat.pacific, var = "logTTM [all]", var_name = "logTTM", group = "bruvs", group2 = "protection_use", condition = condition_ben)
pred_mod_sens_ben <- subset(pred_mod_sens, group=="Benthic")# subset benthic predictions
pred_mod_sens_pacific <- rbind(pred_mod_sens_pel, pred_mod_sens_ben)
pred_mod_sens_pacific$sens_ocean <- 'drop_pacific'


#combined all predictions with dropped oceans

pred_sens_firstmode_oceans <- rbind(pred_mod_sens_indian, pred_mod_sens_atlantic, pred_mod_sens_pacific)





# logsecond mode sensitivity drop ocean  -------------------------------------
######  PREPARATION OF MODELS
# saturated model fitting

###drop atlantic ocean samples------
var_res_name = "logSecondmode"

tab_secondmode_atlantic <- subset(tab_secondmode, TERRITORY1!="Ascension" & TERRITORY1!="Azores" & TERRITORY1!="Tristan" & TERRITORY1!="Norway" & TERRITORY1!="Tristan da Cunha")
mod_sat.atlantic <- nlme::gls(logSecondmode ~ bruvs * protection_use * (logTTM +logDistP) + bruvs * (logDistSM+logDistC +logBathy+ logDistCR + Slope +
                                                                                                      poly(logSST,2) + poly(logCHL,2)), data = tab_secondmode_atlantic,  correlation = nlme::corRatio(form=~1),method='ML') 

######  MODEL MARGINAL PREDICTIONS
# make marginal plot predictions from selected model with means specified 
pel <- subset(tab_secondmode_atlantic, bruvs=="pelagic")
ben <- subset(tab_secondmode_atlantic, bruvs=="benthic")
condition_pel = c(logBathy = mean(pel$logBathy), logDistCR = mean(pel$logDistCR), logSST = mean(pel$logSST), logDistSM = mean(pel$logDistSM), logDistP =  1.2, logCHL= mean(pel$logCHL), logDistC = mean(pel$logDistC), Slope =mean(pel$Slope))
condition_ben = c(logBathy = mean(ben$logBathy), logDistCR = mean(ben$logDistCR), logSST = mean(ben$logSST), logDistSM = mean(ben$logDistSM), logDistP =  1.2, logCHL= mean(ben$logCHL), logDistC = mean(ben$logDistC), Slope =mean(ben$Slope))
pred_mod_sens <- marg_data_bruvs_prot(response = var_res_name, dat = tab_secondmode_atlantic, mod=mod_sat.atlantic, var = "logTTM [all]", var_name = "logTTM", group = "bruvs", group2 = "protection_use", condition = condition_pel)
pred_mod_sens_pel <- subset(pred_mod_sens, group=="Pelagic")# subset pelagic predictions
pred_mod_sens <- marg_data_bruvs_prot(response = var_res_name, dat = tab_secondmode_atlantic, mod=mod_sat.atlantic, var = "logTTM [all]", var_name = "logTTM", group = "bruvs", group2 = "protection_use", condition = condition_ben)
pred_mod_sens_ben <- subset(pred_mod_sens, group=="Benthic")# subset benthic predictions
pred_mod_sens_atlantic <- rbind(pred_mod_sens_pel, pred_mod_sens_ben)
pred_mod_sens_atlantic$sens_ocean <- 'drop_atlantic'

###  drop indian ocean samples-----

tab_secondmode_indian_aus <- subset(tab_secondmode, TERRITORY1!="Chagos Archipelago" & TERRITORY1!="Sudan" & TERRITORY1!="Cocos Islands" & TERRITORY1!="Maldives" & TERRITORY1!= "Australia")
tab_secondmode_indian_FNQ <- subset(tab_secondmode, Exped.=="FNQ_2018" | Exped.=="FNQ_2017_11" | Exped.=="FNQ_2017_06" | Exped.=="FNQ Nov 2017" | Exped.=="FNQ Jun 2017")
tab_secondmode_indian <- rbind(tab_secondmode_indian_aus, tab_secondmode_indian_FNQ)
mod_sat.indian <- nlme::gls(logSecondmode ~ bruvs * protection_use * (logTTM +logDistP) + bruvs * (logDistSM+logDistC +logBathy+ logDistCR + Slope +
                                                                                                    poly(logSST,2) + poly(logCHL,2)), data = tab_secondmode_indian,  correlation = nlme::corRatio(form=~1),method='ML') 
######  MODEL MARGINAL PREDICTIONS
# make marginal plot predictions from selected model with means specified 
pel <- subset(tab_secondmode_indian, bruvs=="pelagic")#&protection_use=="not_protected")
ben <- subset(tab_secondmode_indian, bruvs=="benthic")#&protection_use=="not_protected")
condition_pel = c(logBathy = mean(pel$logBathy), logDistCR = mean(pel$logDistCR), logSST = mean(pel$logSST), logDistSM = mean(pel$logDistSM), logDistP =  1.2, logCHL= mean(pel$logCHL), logDistC = mean(pel$logDistC), Slope =mean(pel$Slope))
condition_ben = c(logBathy = mean(ben$logBathy), logDistCR = mean(ben$logDistCR), logSST = mean(ben$logSST), logDistSM = mean(ben$logDistSM), logDistP =  1.2, logCHL= mean(ben$logCHL), logDistC = mean(ben$logDistC), Slope =mean(ben$Slope))
pred_mod_sens <- marg_data_bruvs_prot(response = var_res_name, dat = tab_secondmode_indian, mod=mod_sat.indian, var = "logTTM [all]", var_name = "logTTM", group = "bruvs", group2 = "protection_use", condition = condition_pel)
pred_mod_sens_pel <- subset(pred_mod_sens, group=="Pelagic")# subset pelagic predictions
pred_mod_sens <- marg_data_bruvs_prot(response = var_res_name, dat = tab_secondmode_indian, mod=mod_sat.indian, var = "logTTM [all]", var_name = "logTTM", group = "bruvs", group2 = "protection_use", condition = condition_ben)
pred_mod_sens_ben <- subset(pred_mod_sens, group=="Benthic")# subset benthic predictions
pred_mod_sens_indian <- rbind(pred_mod_sens_pel, pred_mod_sens_ben)
pred_mod_sens_indian$sens_ocean <- 'drop_indian'

###  drop pacific ocean samples----

tab_secondmode_pacific_aus <- subset(tab_secondmode, TERRITORY1!="Clipperton Island" & TERRITORY1!="Costa Rica" & TERRITORY1!="Clipperton Island" & TERRITORY1!="Palau" & TERRITORY1!= "French Polynesia" & TERRITORY1!="Fiji" & TERRITORY1!="Niue" & TERRITORY1!= "New Caledonia" & TERRITORY1!= "Galapagos" & TERRITORY1!= "Mexico" & TERRITORY1!= "Columbia")
tab_secondmode_pacific <- subset(tab_secondmode_pacific_aus, Exped.!="FNQ_2018" & Exped.!="FNQ_2017_11" & Exped.!="FNQ_2017_06" & Exped.!="FNQ Nov 2017" & Exped.!="FNQ Jun 2017")
mod_sat.pacific <- nlme::gls(logSecondmode ~ bruvs * protection_use * (logTTM +logDistP) + bruvs * (logDistSM+logDistC +logBathy+ logDistCR + Slope +
                                                                                                     poly(logSST,2) + poly(logCHL,2)), data = tab_secondmode_pacific,  correlation = nlme::corRatio(form=~1),method='ML') 

######  MODEL MARGINAL PREDICTIONS
# make marginal plot predictions from selected model with means specified 
pel <- subset(tab_secondmode_pacific, bruvs=="pelagic")#&protection_use=="not_protected")
ben <- subset(tab_secondmode_pacific, bruvs=="benthic")#&protection_use=="not_protected")
condition_pel = c(logBathy = mean(pel$logBathy), logDistCR = mean(pel$logDistCR), logSST = mean(pel$logSST), logDistSM = mean(pel$logDistSM), logDistP =  1.2, logCHL= mean(pel$logCHL), logDistC = mean(pel$logDistC), Slope =mean(pel$Slope))
condition_ben = c(logBathy = mean(ben$logBathy), logDistCR = mean(ben$logDistCR), logSST = mean(ben$logSST), logDistSM = mean(ben$logDistSM), logDistP =  1.2, logCHL= mean(ben$logCHL), logDistC = mean(ben$logDistC), Slope =mean(ben$Slope))
pred_mod_sens <- marg_data_bruvs_prot(response = var_res_name, dat = tab_betaslope_pacific, mod=mod_sat.pacific, var = "logTTM [all]", var_name = "logTTM", group = "bruvs", group2 = "protection_use", condition = condition_pel)
pred_mod_sens_pel <- subset(pred_mod_sens, group=="Pelagic")# subset pelagic predictions
pred_mod_sens <- marg_data_bruvs_prot(response = var_res_name, dat = tab_betaslope_pacific, mod=mod_sat.pacific, var = "logTTM [all]", var_name = "logTTM", group = "bruvs", group2 = "protection_use", condition = condition_ben)
pred_mod_sens_ben <- subset(pred_mod_sens, group=="Benthic")# subset benthic predictions
pred_mod_sens_pacific <- rbind(pred_mod_sens_pel, pred_mod_sens_ben)
pred_mod_sens_pacific$sens_ocean <- 'drop_pacific'


#combined all predictions with dropped oceans

pred_sens_secondmode_oceans <- rbind(pred_mod_sens_indian, pred_mod_sens_atlantic, pred_mod_sens_pacific)


#Fig. S10 marginal plot drop ocean -------

#merge
pred_all_sens_oceans <- rbind(pred_sens_firstmode_oceans,pred_sens_secondmode_oceans,pred_sens_betaslope_oceans)

save_marg_pred_all_sens_oceans(pred_all_sens_oceans) # save marginal predictions

###plot marginal plot combined by protection level

pred_all_sens_oceans_mod <- read_marg_pred_all_sens_oceans() # read marginal predictions
#summary(pred_all_sens_oceans_mod)

marg_plot_bruvs_prot_sens_oceans(pred=pred_all_sens_oceans_mod, var_name = "logTTM")



