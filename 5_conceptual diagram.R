#load all functions ----
devtools::document()
devtools::load_all() ### load packages and functions in R folder



# required packages
library(tidyverse)
library(ggsci)
library(cowplot)
library(data.table)
library(scales)
library(rgdal)
library(ggpubr)
library(lme4)


### Conceptual diagram

#```{r fig1, warning=F, message=F}
# Conceptual diagram of the hypotheses. Made using simulated data
set.seed(1)
# setting parameters for normal distribution plots (for steepening effect)
first_mode <- rlnorm(n = 10, meanlog = 0.25, sdlog = 0.1) + 30
second_mode <- rlnorm(n = 10, meanlog = 2.5, sdlog = 0.1) + 55
first_sd   <- rnorm(n = 10, mean = 8, sd = 1)
second_sd   <- rnorm(n = 10, mean = 10, sd = 1)
# Figure 1A

#modal distribution of sizes
p <-
  tibble(type = "first", spp =1:10, mean = first_mode, sd = first_sd) %>% 
  bind_rows(tibble(type = "second", spp =1:10, mean = second_mode, sd = second_sd)) %>% 
  mutate(vals = map2(mean, sd, .f=~dnorm(0:100, mean=.x, sd=.y))) %>% 
  unnest(cols = "vals") %>% 
  mutate(id = paste0(type, spp)) %>% 
  mutate(x = rep(0:100, 20)) %>% 
  ggplot2::ggplot(aes(x, vals, fill=type, group=id)) +
  ggplot2::geom_area(position = 'identity', alpha=0.1, aes(colour=type)) +
  theme_light() +
  theme(legend.position = c(0.8, 0.8),
        legend.background = element_rect(fill = "transparent"), axis.title.y= element_text(size=16), axis.title.x = element_text(size=16),axis.text.y = element_text(size = 16),axis.text.x = element_text(size = 16), legend.text = element_text(size = 16), legend.title = element_blank()) + labs(x = "Body size (kg)",y = "Probability density") +
  scale_fill_manual(name="Mode", labels = c("First mode", "Second mode"), values =  c("darkgreen", "lightgreen"))+ 
  scale_colour_manual(name="Mode", labels = c("First mode", "Second mode"), values =  c("darkgreen", "lightgreen"))+
  scale_x_continuous(breaks = c(20, 50, 80), labels = c("0.01", "1", "100"))+xlab("Body size (kg")+ylab("")
p

#steepening of size spectra
p1a <-
  tibble(mass = 1:10, fished = (6 + (-1.4*mass)), unfished =  (6 + (-1*mass))) %>% 
  gather(key = "type", value = "abundance", -mass) %>% 
  ggplot2::ggplot(aes(mass, abundance)) +
  ggplot2::geom_ribbon(aes(ymin=(6 + (-1.4*mass)), ymax=(6 + (-1*mass))), alpha=0.3, col="transparent") +
  ggplot2::geom_line(aes(linetype = type), size = 2, colour = "darkgreen", alpha =.5) + 
  #ggplot2::geom_point(aes(fill = type), col="black", shape=21, size=3) + 
  theme_light() +
  theme(legend.position = "none", 
        legend.background = element_rect(fill = "transparent")) + 
  labs(x = "",
       y = "Log(abundance)")+ theme(axis.text.x = element_text(size=16), axis.text.y = element_blank(),axis.title = element_text(size =16), legend.text =element_text(size =16), legend.title = element_blank())+
      scale_linetype_manual(values=c("dotted", "solid"))+scale_x_continuous(breaks = c(2.5, 5, 7.5), labels = c("0.01", "1", "100"))

p1a

#shallowing of size spectra
p1b <-
  tibble(mass = 1:10, fished = (4 + (-0.8*mass)), unfished =  (6 + (-1*mass))) %>% 
  gather(key = "type", value = "abundance", -mass) %>% 
  ggplot2::ggplot(aes(mass, abundance)) +
  ggplot2::geom_ribbon(aes(ymin=(4 + (-0.8*mass)), ymax=(6 + (-1*mass))), alpha=0.3, col="transparent") +
  ggplot2::geom_line(aes(linetype = type), size = 2, colour = "darkgreen", alpha =.5) + 
  #ggplot2::geom_point(aes(fill = type), col="black", shape=21, size=3) + 
  theme_light() +
  theme(legend.position = "none", 
        legend.background = element_rect(fill = "transparent")) + 
  labs(x = "",
       y = "")+ theme(axis.text.x = element_text(size=16), axis.text.y = element_blank(),axis.title = element_text(size =16), legend.text =element_text(size =16), legend.title = element_blank())+
  scale_linetype_manual(values=c("dotted", "solid"))+scale_x_continuous(breaks = c(2.5, 5, 7.5), labels = c("0.01", "1", "100"))

p1b

#stable size spectra - proportional depletion
p1c <-
  tibble(mass = 1:10, fished = (4 + (-1*mass)), unfished =  (6 + (-1*mass))) %>% 
  gather(key = "type", value = "abundance", -mass) %>% 
  ggplot2::ggplot(aes(mass, abundance)) +
  ggplot2::geom_ribbon(aes(ymin=(4 + (-1*mass)), ymax=(6 + (-1*mass))), alpha=0.3, col="transparent") +
  ggplot2::geom_line(aes(linetype = type), size = 2, colour = "darkgreen", alpha =.5) + 
  #ggplot2::geom_point(aes(fill = type), col="black", shape=21, size=3) + 
  theme_light() +
  theme(legend.position = "none", 
        legend.background = element_rect(fill = "transparent")) + 
  labs(x = "",
       y = "")+ theme(axis.text.x = element_text(size=16), axis.text.y = element_blank(),axis.title = element_text(size =16), legend.text =element_text(size =16), legend.title = element_blank())+
  scale_linetype_manual(values=c("dotted", "solid"))+scale_x_continuous(breaks = c(2.5, 5, 7.5), labels = c("0.01", "1", "100"))

p1c

# steepening of size spectra with trophic release
p1d <-
  tibble(mass = 1:10, fished = (8 + (-1.4*mass)), unfished =  (6 + (-1*mass))) %>% 
  gather(key = "type", value = "abundance", -mass) %>% 
  ggplot2::ggplot(aes(mass, abundance)) +
  ggplot2::geom_ribbon(aes(ymin=(8 + (-1.4*mass)), ymax=(6 + (-1*mass))), alpha=0.3, col="transparent") +
  ggplot2::geom_line(aes(linetype = type), size = 2, colour = "darkgreen", alpha =.5) + 
  #ggplot2::geom_point(aes(fill = type), col="black", shape=21, size=3) + 
  theme_light() +
  theme(legend.position = c(0.8, 0.8), 
        legend.background = element_rect(fill = "transparent")) + 
  labs(x = "",
       y = "")+ theme(axis.text.x = element_text(size =16), axis.text = element_blank(),axis.title = element_text(size =16), legend.text =element_text(size =16), legend.title = element_blank())+
  scale_linetype_manual(values=c("dotted", "solid"))+scale_x_continuous(breaks = c(2.5, 5, 7.5), labels = c("0.01", "1", "100"))
p1d


####hypothesised effects of fishing on modes

#second mode smaller
p2 <- p + geom_segment(aes(x = 70, y = 0.02, xend = 50, yend = 0.02),
                  arrow = arrow(length = unit(0.3, "cm")), linetype = "solid")+
  theme(legend.position = "null")+xlab("Body size (kg)")+ylab("Probability density")

#first mode smaller
p3 <- p + geom_segment(aes(x = 30, y = 0.03, xend = 10, yend = 0.03),
                  arrow = arrow(length = unit(0.3, "cm")), linetype = "solid")+
  theme(legend.position = "null")+xlab("Body size (kg)")

#first and second mode smaller

p4 <- p + geom_segment(aes(x = 30, y = 0.03, xend = 10, yend = 0.03),
                  arrow = arrow(length = unit(0.3, "cm")), linetype = "solid")+
    geom_segment(aes(x = 70, y = 0.02, xend = 50, yend = 0.02),
                 arrow = arrow(length = unit(0.3, "cm")), linetype = "solid")+
  theme(legend.position = "null")+xlab("Body size (kg)")


# first mode smaller and second mode bigger
p5 <-p + geom_segment(aes(x = 30, y = 0.03, xend = 50, yend = 0.03),
                  arrow = arrow(length = unit(0.5, "cm")), linetype = "dashed")+ 
  geom_segment(aes(x = 70, y = 0.02, xend = 50, yend = 0.02),
                   arrow = arrow(length = unit(0.3, "cm")), linetype = "solid")+
  theme(legend.position = "none")+
  ##for legend
  geom_segment(aes(x = 40, y = 0.07, xend = 55, yend = 0.07),
                                                arrow = arrow(length = unit(0.3, "cm")), linetype = "solid")+
  geom_segment(aes(x = 40, y = 0.065, xend = 55, yend = 0.065),
               arrow = arrow(length = unit(0.5, "cm")), linetype = "dashed")+
  annotate("text", x=80, y=0.07, label= "Direct effect", size=5)+
  annotate("text", x=80, y=.065, label= "Indirect effect", size =5)+xlab("Body size (kg)")




p5

fig_cptual <-     cowplot::ggdraw()+
                  cowplot::draw_plot(p+ylab("Probability density"), 0.25, 0.66, 0.5, 0.33)+
                  cowplot::draw_plot(p1a, 0, 0.33, 0.25,0.3)+
                  cowplot::draw_plot(p1b, 0.25, 0.33, 0.25,0.3)+
                  cowplot::draw_plot(p1c, 0.50, 0.33, 0.25,0.3)+
                  cowplot::draw_plot(p1d, 0.75, 0.33, 0.25,0.3)+
                  cowplot::draw_plot(p2, 0, 0, 0.25, 0.3)+
                  cowplot::draw_plot(p3, 0.25, 0, 0.25, 0.3)+
                  cowplot::draw_plot(p4, 0.50, 0, 0.25, 0.3)+
                  cowplot::draw_plot(p5, 0.75, 0, 0.25, 0.3)

  
fig_cptual 

ggsave(fig_cptual, filename = here::here("outputs", "fig_cptual.jpeg"), width = 16, height = 14, units = "in", dpi =300)#render cowplots in jpeg less you get seethrough bits




###### NERC bentho-pelagic coupling
# Conceptual diagram of the hypotheses. Made using simulated data
set.seed(2)
# setting parameters for normal distribution plots (for steepening effect)
first_mode <-  rlnorm(n = 1, meanlog = 0.25, sdlog = 0.1) + 10
second_mode <- rlnorm(n = 1, meanlog = 2.5, sdlog = 0.1) + 55
second_b_mode <- rlnorm(n = 1, meanlog = 2, sdlog = 0.1) + 15
third_mode <- rlnorm(n = 1, meanlog = 1, sdlog = 0.1) + 35

first_sd   <-   rnorm(n = 1, mean = 5, sd = 1)
second_sd   <-  rnorm(n = 1, mean = 8, sd = 1)
second_b_sd   <-  rnorm(n = 1, mean = 8, sd = 1)
third_sd   <-  rnorm(n = 1, mean = 10, sd = 10)

#modal distribution of sizes
pb <-
  tibble(type = "first", spp =1:2, mean = first_mode, sd = first_sd) %>% 
  bind_rows(tibble(type = "second", spp =1:2, mean = second_mode, sd = second_sd)) %>% 
  bind_rows(tibble(type = "third", spp =1:2, mean = third_mode, sd = third_sd)) %>% 
  bind_rows(tibble(type = "second_b", spp =1:2, mean = second_b_mode, sd = second_b_sd)) %>% 
  mutate(vals = map2(mean, sd, .f=~dnorm(0:100, mean=.x, sd=.y))) %>% 
  unnest(cols = "vals") %>% 
  mutate(id = paste0(type, spp)) %>% 
  mutate(x = rep(0:100, 8)) %>% 
  ggplot2::ggplot(aes(x, vals, fill=type, group=id)) +
  ggplot2::geom_area(position = 'identity', alpha=0.1, aes(colour=type)) +
  theme_light() +
  theme(legend.position = c(0.8, 0.8),
        legend.background = element_rect(fill = "transparent"), axis.title.y= element_text(size=16), axis.title.x = element_text(size=16),axis.text.y = element_text(size = 16),axis.text.x = element_text(size = 16), legend.text = element_text(size = 16), legend.title = element_blank()) + labs(x = "Body size (kg)",y = "Probability density") +
  scale_fill_manual(name="Mode", labels = c("Pelagic", "", "Benthic"), values =  c("#077DAA", "#077DAA", "orange","orange"))+ 
  scale_colour_manual(name="Mode", labels = c("Pelagic", "","Benthic"), values =  c("#077DAA","#077DAA", "orange","orange"))+
  scale_x_continuous(breaks = c(0, 40, 70), labels = c("0.001", "1", "100"))+xlab("Body size (kg")+ylab("")+ theme(legend.position = "none")
pb


# benthic erosion due to habitat structure loss

set.seed(2)

first_mode <-  rlnorm(n = 1, meanlog = 2.25, sdlog = 0.1) + 35
second_mode <- rlnorm(n = 1, meanlog = 0.5, sdlog = 0.1) + 15


first_sd   <-   rnorm(n = 1, mean = 5, sd = 1)
second_sd   <-  rnorm(n = 1, mean = 8, sd = 1)

#modal distribution of sizes
pb <-
  tibble(type = "first", spp =1:2, mean = first_mode, sd = first_sd) %>% 
  bind_rows(tibble(type = "second", spp =1:2, mean = second_mode, sd = second_sd)) %>% 
  #bind_rows(tibble(type = "third", spp =1:2, mean = third_mode, sd = third_sd)) %>% 
  mutate(vals = map2(mean, sd, .f=~dnorm(0:100, mean=.x, sd=.y))) %>% 
  unnest(cols = "vals") %>% 
  mutate(id = paste0(type, spp)) %>% 
  mutate(x = rep(0:100, 4)) %>% 
  ggplot2::ggplot(aes(x, vals, fill=type, group=id)) +
  ggplot2::geom_area(position = 'identity', alpha=0.1, aes(colour=type)) +
  theme_light() +
  theme(legend.position = c(0.8, 0.8),
        legend.background = element_rect(fill = "transparent"), axis.title.y= element_text(size=16), axis.title.x = element_text(size=16),axis.text.y = element_text(size = 16),axis.text.x = element_text(size = 16), legend.text = element_text(size = 16), legend.title = element_blank()) + labs(x = "Body size (kg)",y = "Probability density") +
  scale_fill_manual(name="Mode", labels = c("Benthic",  "Benthic"), values =  c("orange","orange"))+ 
  scale_colour_manual(name="Mode", labels = c("Benthic", "Benthic"), values =  c("orange","orange"))+
  scale_x_continuous(breaks = c(0, 40, 70), labels = c("0.001", "1", "100"))+xlab("Body size (kg")+ylab("")+ theme(legend.position = "none")
pb

# pelagic fisheries due to fisheries

# Conceptual diagram of the hypotheses. Made using simulated data
set.seed(2)
# setting parameters for normal distribution plots (for steepening effect)
first_mode <-  rlnorm(n = 1, meanlog = 0.25, sdlog = 0.1) + 10
second_mode <- rlnorm(n = 1, meanlog = 2.5, sdlog = 0.1) + 40

first_sd   <-   rnorm(n = 1, mean = 5, sd = 1)
second_sd   <-  rnorm(n = 1, mean = 8, sd = 1)

#modal distribution of sizes
pb <-
  tibble(type = "first", spp =1:2, mean = first_mode, sd = first_sd) %>% 
  bind_rows(tibble(type = "second", spp =1:2, mean = second_mode, sd = second_sd)) %>% 
  mutate(vals = map2(mean, sd, .f=~dnorm(0:100, mean=.x, sd=.y))) %>% 
  unnest(cols = "vals") %>% 
  mutate(id = paste0(type, spp)) %>% 
  mutate(x = rep(0:100, 4)) %>% 
  ggplot2::ggplot(aes(x, vals, fill=type, group=id)) +
  ggplot2::geom_area(position = 'identity', alpha=0.1, aes(colour=type)) +
  theme_light() +
  theme(legend.position = c(0.8, 0.8),
        legend.background = element_rect(fill = "transparent"), axis.title.y= element_text(size=16), axis.title.x = element_text(size=16),axis.text.y = element_text(size = 16),axis.text.x = element_text(size = 16), legend.text = element_text(size = 16), legend.title = element_blank()) + labs(x = "Body size (kg)",y = "Probability density") +
  scale_fill_manual(name="Mode", labels = c("Pelagic", "", "Benthic"), values =  c("#077DAA", "#077DAA", "orange","orange"))+ 
  scale_colour_manual(name="Mode", labels = c("Pelagic", "","Benthic"), values =  c("#077DAA","#077DAA", "orange","orange"))+
  scale_x_continuous(breaks = c(0, 40, 70), labels = c("0.001", "1", "100"))+xlab("Body size (kg")+ylab("")+ theme(legend.position = "none")
pb

#decoupled

set.seed(2)
# setting parameters for normal distribution plots (for steepening effect)
first_mode <-  rlnorm(n = 1, meanlog = 0.25, sdlog = 0.1) + 10
second_mode <- rlnorm(n = 1, meanlog = .5, sdlog = 0.1) + 55
second_b_mode <- rlnorm(n = 1, meanlog = .5, sdlog = 0.1) + 15
third_mode <- rlnorm(n = 1, meanlog = 3, sdlog = 0.1) + 35

first_sd   <-   rnorm(n = 1, mean = 5, sd = 1)
second_sd   <-  rnorm(n = 1, mean = 8, sd = 1)
second_b_sd   <-  rnorm(n = 1, mean = 8, sd = 1)
third_sd   <-  rnorm(n = 1, mean = 10, sd = 10)

#modal distribution of sizes
pb1 <-
  tibble(type = "first", spp =1:2, mean = first_mode, sd = first_sd) %>% 
  bind_rows(tibble(type = "second", spp =1:2, mean = second_mode, sd = second_sd)) %>% 
  bind_rows(tibble(type = "third", spp =1:2, mean = third_mode, sd = third_sd)) %>% 
  bind_rows(tibble(type = "second_b", spp =1:2, mean = second_b_mode, sd = second_b_sd)) %>% 
  mutate(vals = map2(mean, sd, .f=~dnorm(0:100, mean=.x, sd=.y))) %>% 
  unnest(cols = "vals") %>% 
  mutate(id = paste0(type, spp)) %>% 
  mutate(x = rep(0:100, 8)) %>% 
  ggplot2::ggplot(aes(x, vals, fill=type, group=id)) +
  ggplot2::geom_area(position = 'identity', alpha=0.1, aes(colour=type)) +
  theme_light() +
  theme(legend.position = c(0.8, 0.8),
        legend.background = element_rect(fill = "transparent"), axis.title.y= element_text(size=16), axis.title.x = element_text(size=16),axis.text.y = element_text(size = 16),axis.text.x = element_text(size = 16), legend.text = element_text(size = 16), legend.title = element_blank()) + labs(x = "Body size (kg)",y = "Probability density") +
  scale_fill_manual(name="Mode", labels = c("Pelagic", "", "Benthic"), values =  c("#077DAA", "#077DAA", "orange","orange"))+ 
  scale_colour_manual(name="Mode", labels = c("Pelagic", "","Benthic"), values =  c("#077DAA","#077DAA", "orange","orange"))+
  scale_x_continuous(breaks = c(0, 40, 70), labels = c("0.001", "1", "100"))+xlab("Body size (kg")+ylab("")+ theme(legend.position = "none")
pb1
