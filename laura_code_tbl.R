library(magrittr); library(spaMM) ; library(ggplot2); library(parallel); library(nlme); library(effectsize);library(visreg);library(corrgram)


#load previous models
save(mod,mod2, mod3, mod4,mod5, mod6, mod7, file = "model.RData")

load("model.RData")

#---------------------------- load and clean data

# importation des donnes

tab <- read.table(here::here("outputs", file="pelagic_benthic_response_envar_clean.txt"))


tab <- tidyr::drop_na(tab, betaslope)


summary(tab)
head(tab)
dim(tab)


ggplot(tab, aes(x=10^logBathy, y=betaslope, colour = bruvs), alpha=.4) +geom_point()

ggplot(tab, aes(x=-Bathymetry, fill =bruvs, alpha=.4)) + geom_density()+scale_x_log10()


    
#remove weird bathy
tab_pel <- tab[ which(tab$bruvs=='pelagic'), ]
sum(tab_pel$Bathymetry >-20)
tab_ben <- tab[ which(tab$bruvs=='benthic'), ]
sum(tab_ben$Bathymetry >-2)



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


#add pelagic/benthic predictors
tab %>%
  dplyr::mutate(pelagic = ifelse(bruvs == "pelagic", TRUE, FALSE)) %>%
  dplyr::mutate(benthic = ifelse(bruvs == "benthic", TRUE, FALSE)) -> tab




#------------------------------------- fit model

# full model (bruvs as interaction term of all predictors ; separate spatial autocorrelation structures for pelagic and benthic data)
mod = fitme(betaslope ~ logBathy*bruvs + Slope*bruvs + logDistP*bruvs + logDistSM*bruvs + GovernmentEffectiveness_mean*bruvs +
            Matern(pelagic | mean_lat + mean_long) + Matern(benthic | mean_lat + mean_long),
            nb_cores = floor(parallel::detectCores()/1.5) ,
            data = tab)



#--------------------------------------- partial plot for logBathy

# build new data with all predictors and lat/lon equal to their mean but logbathy
tab %>%
  dplyr::mutate(Slope = mean(Slope, na.rm = TRUE),
                logDistP = mean(logDistP, na.rm = TRUE),
                logDistSM = mean(logDistSM, na.rm = TRUE),
                GovernmentEffectiveness_mean = mean(GovernmentEffectiveness_mean, na.rm = TRUE),
                mean_long = mean(mean_long, na.rm = TRUE),
                mean_lat = mean(mean_lat, na.rm = TRUE)) -> tabpred


#get predictions and variances on new data
pred <- predict(mod, tabpred, type = "response")
variance <- get_predVar(mod, tabpred, variances = list(predVar=TRUE)) # predVar=TRUE for prediction variance (ie the uncertainty in the linear predictor)

#add predictions and variances to new data
tabpred <- cbind(tabpred, pred, variance)
tabpred <- tabpred %>%
  dplyr::mutate(upper = pred + variance,
                lower = pred - variance)

### try different way 

# ggplot(data=tabpred, aes(x=10^logBathy, y= pred, group = bruvs, colour =bruvs, fill =bruvs))+ geom_smooth(method='lm')+
#    #scale_colour_manual(values = c("Midwater" = '#077DAA', 'Seabed' = 'darkorange'))+
#    scale_fill_manual(values = c("Midwater" = '#077DAA', 'Seabed' = 'darkorange'))
# 
# 

#get subsets
tabpred_pel <- subset(tabpred, bruvs == "pelagic")
tabpred_ben <- subset(tabpred, bruvs == "benthic")

#partial plot with color-coded factor representing pelagic/benthic
options(scipen=3)

p <- ggplot() +
  # pelagic
   geom_line(data = tabpred_pel, aes(x = 10^logBathy, y = pred, colour='#077DAA'))+ #predictions
   geom_ribbon(data = tabpred_pel, aes(x = 10^logBathy,
                                    ymin = lower,
                                    ymax = upper,
                                    fill='#077DAA'),#95% confidence intervals
   alpha=0.2, show.legend=T) +

   #benthic
  geom_line(data = tabpred_ben, aes(x = 10^logBathy, y = pred, colour='darkorange'))+ #predictions
  geom_ribbon(data = tabpred_ben, aes(x=10^logBathy,
                                   ymin = lower,
                                   ymax = upper,
                                   fill='darkorange'),#95% confidence intervals
              alpha=0.2,  show.legend=T) +
  labs(y = "betaslope", x = "Bathy")+
  scale_colour_manual(name = "", values = c('#077DAA', 'darkorange'),
                      labels = c("pelagic", "benthic"),aesthetics = c("colour", "fill"))+
  theme_light()+scale_x_log10()
p
ggsave(here::here("outputs", "partial_plot.png"), p)



### try gls

mod2 <- gls(betaslope ~ bruvs*(logBathy + Slope + logDistP + logDistSM + GovernmentEffectiveness_mean), data = tab)

mod3 <- gls(betaslope ~ bruvs*(logBathy + Slope + logDistP + logDistSM + GovernmentEffectiveness_mean), data = tab, correlation = corAR1(form=~1))
mod4 <- gls(betaslope ~ bruvs*(logBathy + Slope + logDistP + logDistSM + GovernmentEffectiveness_mean), data = tab, correlation = corExp(form=~1))
mod5 <- gls(betaslope ~ bruvs*(logBathy + Slope + logDistP + logDistSM + GovernmentEffectiveness_mean), data = tab, correlation = corGaus(form=~1))
mod6 <- gls(betaslope ~ bruvs*(logBathy + Slope + logDistP + logDistSM + GovernmentEffectiveness_mean), data = tab, correlation = corLin(form=~1))

mod7 <- gls(betaslope ~ bruvs*(logBathy + Slope + logDistP + logDistSM + GovernmentEffectiveness_mean), data = tab, correlation = corGaus(form=~mean_lat+mean_long))




#https://www.r-bloggers.com/2011/10/linear-regression-with-correlated-data/


#https://www.flutterbys.com.au/stats/tut/tut8.4a.html
anova(mod2, mod3, mod4, mod5, mod6, mod7)

effectsize(mod3)

p = visreg(mod5, xvar ='logBathy', by = 'bruvs', overlay = TRUE, partial = FALSE, rug = FALSE,plot=FALSE)


plotp <- ggplot(p$fit, aes(10^logBathy, visregFit, colour= bruvs, fill= bruvs)) +
  geom_ribbon(aes(ymin=visregLwr, ymax=visregUpr, colour = bruvs, fill = bruvs)) +
  geom_line() +
  scale_colour_manual(name = "", values = c('#077DAA', 'darkorange'),aesthetics = c("colour", "fill"))

ggsave(here::here("outputs", "partial_plot.png"), plotp)


save(file = "model.RData")



