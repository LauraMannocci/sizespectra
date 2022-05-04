####MODELLING OF SIZE SPECTRA

# importation et transformation des donnes



# importation des donnes

tab <- read.table(here::here("outputs", file="benthic_pelagic_response_envar_clean.txt"))

tab <- tidyr::drop_na(tab, betaslope)



summary(tab)
head(tab)
dim(tab)

# creation d'un facteur

x <- factor(LETTERS[1:2])
a=rep(x, dim(tab)[1]/2)

tab=cbind(tab,a)

# spaMM

hist(tab$median_maxsize)
hist(log10(tab$median_maxsize+1))

hist(tab$median_mediansize)
hist(log10(tab$median_mediansize+1))

hist(tab$betaslope)


# change of variables

logconflicts=log10(tab$conflicts+1)
logNGO=log10(tab$NGO+1)
logTTM=log10(tab$TravelTime_market+1)
logSAU=log10(tab$SAU+1)
logTTP=log10(tab$TravelTime_pop+1)
logSAU=log10(tab$SAU+1)
logDistP=log10(tab$distPort+1)
logDistSM=log10(tab$distSeamounts+1)
logDistCR=log10(tab$distCoralReef+1)
logDistC=log10(tab$distCoast+1)
logBathy=log10(-tab$Bathymetry)
logPP=log10(tab$PP+1)
logCHL=log10(tab$CHL+1)
logSST=log10(tab$SST_mean)
bruvs=as.factor(tab$bruvs_type)

tab=cbind(tab,logconflicts,logNGO,logTTM,logSAU,logTTP, logDistP, logDistSM,logDistCR,logDistC,logBathy,logPP,logCHL,logSST,bruvs)

corrgram::corrgram(tab[,c("logPP","logCHL","logSST","SST_sd","HDI_mean","logNGO","Voice_mean",
                "MarineEcosystemDependency","logBathy","logDistC","logDistSM","GovernmentEffectiveness_mean",
                "logDistP","logSAU","logconflicts","logDistCR","logTTM","logTTP")],
         lower.panel=panel.cor, upper.panel=panel.shade)

tab <- tidyr::drop_na(tab, logBathy)

summary(tab)
library(spaMM)
# betaslope

# full model

full.slope=fitme(betaslope~protection+logPP+logSST+SST_sd+HDI_mean+MarineEcosystemDependency+logBathy+logDistC+logDistSM+
                   GovernmentEffectiveness_mean+logDistP+logSAU+logconflicts+logDistCR+logTTM+logTTP+Matern(1|mean_lat+mean_long),nb_cores =2, data=tab)

# with Random Effect
full.slope=fitme(betaslope~logBathy+Slope+logDistP+logDistSM+RuleofLaw+
                   GovernmentEffectiveness_mean+(1|bruvs)+Matern(1|mean_lat+mean_long),nb_cores =2, data=tab)

summary(full.slope)

# terribly long
AIC(mod.slope)

stepAIC(mod.slope, nb_cores =2)

confint(mod.slope,"MarineEcosystemDependency")

confint(mod.slope,"HDI_mean")

plot_effects(mod.slope,"SST_mean")


Anova(full.slope)
performance::r2_nakagawa(full.slope)
r2(full.slope)


#combine fitted with covariates
fit <- as.data.frame(fitted(full.slope))
fit <- cbind(fit, tab)

summary(full.slope)
#partial plot
parplot_bathy <- ggplot2::ggplot(fit, aes(10^logBathy, betaslope, colour = bruvs, fill =bruvs)) + geom_smooth(method = "lm") + theme_bw()
parplot_slope <- ggplot2::ggplot(fit, aes(Slope, betaslope, colour = bruvs, fill=bruvs)) + geom_smooth(method = "lm")+ theme_bw()
parplot_distP <- ggplot2::ggplot(fit, aes(10^logDistP, betaslope, colour = bruvs, fill=bruvs)) + geom_smooth(method = "lm")+ theme_bw()
parplot_SM   <- ggplot2::ggplot(fit, aes(10^logDistSM, betaslope, colour = bruvs, fill =bruvs)) + geom_smooth(method = "lm")+ theme_bw()
parplot_GEM   <- ggplot2::ggplot(fit, aes(GovernmentEffectiveness_mean, betaslope, colour = bruvs, fill =bruvs)) + geom_smooth(method = "lm")+ theme_bw()

parplot_bathy
parplot_slope
parplot_distP
parplot_GEM
parplot_SM

#extract random effects
ran_effect_coef <- VarCorr(full.slope)
colnames(ran_effect_coef) <- c("ran_effect_variable","int", "variance_parameter_estimate", "std_dev")

#extract fixed effects
fixed_effect_coef <- as.data.frame(summary(full.slope)$beta_table)
library(tidyverse)
rownames_to_column(fixed_effect_coef, var="fixed_effect_variable") -> fixed_effect_coef
colnames(fixed_effect_coef) <- c("fixed_effect_var","coef_estimate", "cond_se", "t_value")

ran_effect_plot <- ggplot(ran_effect_coef, aes(y = variance_parameter_estimate, x=ran_effect_variable)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_point(aes(x = ran_effect_variable, y = variance_parameter_estimate)) + 
  geom_linerange(aes(x = ran_effect_variable, ymin = variance_parameter_estimate-std_dev, ymax = variance_parameter_estimate+std_dev),lwd = 1) +
  coord_flip()

fixed_effect_plot <- ggplot(fixed_effect_coef, aes(y = coef_estimate, x=fixed_effect_var)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_point(aes(x = fixed_effect_var, y = coef_estimate)) + 
  geom_linerange(aes(x = fixed_effect_var, ymin = coef_estimate-cond_se, ymax = coef_estimate+cond_se),lwd = 1) +
  coord_flip()

ggpubr::ggarrange(parplot_bathy, parplot_slope, parplot_distP,parplot_GEM, parplot_SM, ran_effect_plot, fixed_effect_plot, ncol = 2, nrow = 4)



