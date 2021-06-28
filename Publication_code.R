##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
##                                                                ##
## Plots for paper                                                ##
##                                                                ##
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
rm(list = ls())
gc()

library(dplyr)
library(ggplot2)
library(openxlsx)
library(lme4)
library(nlme)

## function for calculating standard error
standard_error <- function(x) sd(x, na.rm = T) / sqrt(length(x))

## color palette for climate
rbPalette <- c("#0072B2", "#D55E00")

## D. carthusianorum == dia
## S. ochroleuca == sca

## dia_dat == original data set
## sca_dat == original data set
## dia_3in == data used for the three way interaction, calculates the mean of viable seeds per individual
##            per season per treatment per climate (climate x season x pollination)
## dia_int == dia interaction (climate x pollination)
## sca_cli == sca aggregated to climate level
## dia_sea == dia aggregated to season level
## sca_sea == sca aggregated to season level

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## read data
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dia_dat <- read.xlsx("C://Users/ma22buky/Documents/PhD/Pollination_experiment/data/Seed_count_R.xlsx", 
                     sheetIndex = 1) %>% subset(. ,status == "ripe_capsule" & sample != "group")

sca_dat <- read.xlsx("C://Users/ma22buky/Documents/PhD/Pollination_experiment/data/Seed_count_R.xlsx", 
                     sheetIndex = 2)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## add month, season and ind id to the data frame
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# month
dia_dat$month <- as.Date(dia_dat$date, "%d.%m.%y") %>% format("%m") %>% as.numeric()
sca_dat$month <- as.Date(sca_dat$date, "%d.%m.%y") %>% format("%m") %>% as.numeric()

# season
dia_dat$season <- ifelse(dia_dat$month >= 6 & dia_dat$month <= 8, "summer", 
                         ifelse(dia_dat$month >= 9 & dia_dat$month <= 11, "fall", "kick"))
sca_dat$season <- ifelse(sca_dat$month >= 6 & sca_dat$month <= 8, "summer", 
                         ifelse(sca_dat$month >= 9 & sca_dat$month <= 11, "fall", "kick"))

# ind id
dia_dat$ind_id <- paste(dia_dat$plot, dia_dat$individual, sep = "_")
sca_dat$ind_id <- paste(sca_dat$plot, sca_dat$individual, sep = "_")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## calculate sample size
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
count_(dia_dat, vars = c("climate", "season"))
count_(sca_dat, vars = c("climate", "season"))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Model for three way interaction
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dia_3in <- aggregate(dia_dat$viable, by = list(dia_dat$ind_id, dia_dat$climate, 
                                               dia_dat$season, dia_dat$treatment, dia_dat$plot),
                     FUN = mean, na.rm = T) %>% rename(ind_id = Group.1, climate = Group.2, season = Group.3,
                                                       treatment = Group.4, plot = Group.5, viable = x)

sca_3in <- aggregate(sca_dat$viable, by = list(sca_dat$ind_id, sca_dat$climate, 
                                               sca_dat$season, sca_dat$treatment, sca_dat$plot),
                     FUN = mean, na.rm = T) %>% rename(ind_id = Group.1, climate = Group.2, season = Group.3,
                                                       treatment = Group.4, plot = Group.5, viable = x)

## check how many individuals we have over all and per season
length(unique(dia_3in$ind_id))
group_by(dia_3in, season) %>% summarise(count = n_distinct(ind_id))

length(unique(sca_3in$ind_id))
group_by(sca_3in, season) %>% summarise(count = n_distinct(ind_id))

dia_mod <- lmer(viable ~ climate * season * treatment + (1|ind_id/plot/climate), 
                data = dia_3in)
sca_mod <- lmer(viable ~ climate * season * treatment + (1|ind_id/plot/climate), 
                data = sca_3in)

car::Anova(dia_mod, test.statistic = "F")
car::Anova(sca_mod, test.statistic = "F")

anova(lme(viable ~ climate * season * treatment, random = ~1|ind_id/plot/climate, 
          correlation = corAR1(form=~1|ind_id/plot/climate),
          data = dia_3in))
anova(lme(viable ~ climate * season * treatment, random = ~1|ind_id/plot/climate, 
          data = sca_3in))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  climate x pollination dia only
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dia_int <- aggregate(dia_3in$viable, by = list(dia_3in$climate, dia_3in$treatment),
                     FUN = mean, na.rm = T) %>% rename(climate = Group.1, pollination = Group.2, viable = x)

dia_int$se <- aggregate(dia_3in$viable, by = list(dia_3in$climate, dia_3in$treatment),
                        FUN = standard_error)$x 


dia_cli_pol <- ggplot(dia_int, aes(x = pollination, y = viable, fill = climate)) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = viable - se, ymax = viable + se), width = .5, position = position_dodge(0.85)) + 
  theme_bw() + 
  geom_point(dia_3in, mapping = aes(x = treatment, y = viable, shape = climate), position = position_dodge(0.85)) + 
  scale_fill_manual(values = rbPalette) + 
  labs(title = expression(paste(italic("D. carthusianorum"))), x = "", y = "Viable seeds per seed capsule") + 
  guides(fill = guide_legend(title = "Climate"), shape = guide_legend(title = "Climate"))

ggsave("C://Users/ma22buky/Documents/PhD/Pollination_experiment/Paper/Publication_figs/climxpoll_dia.jpeg", 
       dpi = 300, plot = dia_cli_pol)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Climate for just sca
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sca_cli <- aggregate(sca_3in$viable, by = list(sca_3in$climate), 
                     FUN = mean, na.rm = T) %>% rename(climate = Group.1, viable = x)
sca_cli$se <- aggregate(sca_3in$viable, by = list(sca_3in$climate), 
                        FUN = standard_error)$x

sca_cli_pl <- ggplot(sca_cli, aes(x = climate, y = viable, fill = climate)) + geom_bar(stat = "identity") + theme_bw() +
  scale_fill_manual(values = rbPalette) + 
  geom_errorbar(aes(ymin = viable - se, ymax = viable + se), width = 0.5) + 
  geom_point(sca_3in, mapping = aes(x = climate, y = viable, shape = climate)) +
  labs(title = expression(paste(italic("S.ochroleuca"))), x = "", y = "Viable seeds per seed head") + 
  guides(fill = guide_legend(title = "Climate"), shape = guide_legend(title = "Climate"))

ggsave("C://Users/ma22buky/Documents/PhD/Pollination_experiment/Paper/Publication_figs/sca_cli.jpeg",
       dpi = 300, plot = sca_cli_pl)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  season for dia and sca
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dia_sea <- aggregate(dia_3in$viable, by = list(dia_3in$season), 
                     FUN = mean, na.rm = T)  %>% rename(season = Group.1, viable = x) 
dia_sea$se <- aggregate(dia_3in$viable, by = list(dia_3in$season), 
                        FUN = standard_error)$x

sca_sea <- aggregate(sca_3in$viable, by = list(sca_3in$season), 
                     FUN = mean, na.rm = T) %>% rename(season = Group.1, viable = x)
sca_sea$se <- aggregate(sca_3in$viable, by = list(sca_3in$season), 
                        FUN = standard_error)$x

#make season factor so i can change the order in the graph
dia_sea$season <- factor(dia_sea$season, levels = c("summer", "fall"))
sca_sea$season <- factor(sca_sea$season, levels = c("summer", "fall"))

dia_sea_pl <- ggplot(dia_sea, aes(x = season, y = viable)) + geom_bar(stat = "identity") + theme_bw() + 
  geom_errorbar(aes(ymin = viable - se, ymax = viable + se), width = 0.5) + 
  geom_point(dia_3in, mapping = aes(x = season, y = viable)) + 
  labs(title = expression(paste(italic("D. carthusianorum"))), x = "", y = "Viable seeds per seed capsule")

sca_sea_pl <- ggplot(sca_sea, aes(x = season, y = viable)) + geom_bar(stat = "identity") + theme_bw() + 
  geom_errorbar(aes(ymin = viable - se, max = viable + se), width = 0.5) + 
  geom_point(sca_3in, mapping = aes(x = season, y = viable)) + 
  labs(title = expression(paste(italic("S. ochroleuca"))), x = "", y = "Viable seeds per seed head")


ggpubr::ggarrange(dia_sea_pl, sca_sea_pl, ncol = 2)

ggsave("C://Users/ma22buky/Documents/PhD/Pollination_experiment/Paper/Publication_figs/both_sea.jpeg",  
       dpi = 300, plot = last_plot())
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## supplemental plots
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## read data
dia_bag <- read.xlsx("C://Users/ma22buky/Documents/PhD/Pollination_experiment/data/seed_count_R_bagged.xlsx", 
                     sheetIndex = 2) %>% subset(., status == "ripe_capsule" & sample == "single")
sca_bag <- read.xlsx("C://Users/ma22buky/Documents/PhD/Pollination_experiment/data/seed_count_R_bagged.xlsx", 
                     sheetIndex = 1)
dia_size <- read.xlsx("C://Users/ma22buky/Documents/PhD/Pollination_experiment/data/Original_data/Phenology.xlsx")

## add month, season and individual id to the data frame
dia_bag$month <- as.Date(dia_bag$date, "%d.%m.%Y") %>% format("%m") %>% as.numeric()
sca_bag$month <- as.Date(sca_bag$date, "%d.%m.%Y") %>% format("%m") %>% as.numeric()

dia_bag$season <- ifelse(dia_bag$month >= 6 & dia_bag$month <= 8, "summer", 
                         ifelse(dia_bag$month >= 9 & dia_bag$month <= 11, "fall", "kick"))
sca_bag$season <- ifelse(sca_bag$month >= 6 & sca_bag$month <= 8, "summer", 
                         ifelse(sca_bag$month >= 9 & sca_bag$month <= 11, "fall", "kick"))

dia_bag$ind_id <- paste(dia_bag$individual, dia_bag$plot, sep = "_")
sca_bag$ind_id <- paste(sca_bag$individual, sca_bag$plot, sep = "_")

## run the model
dia_3in_bag <- aggregate(dia_bag$viable, by = list(dia_bag$ind_id, dia_bag$climate, 
                                                   dia_bag$season, dia_bag$treatment, dia_bag$plot),
                         FUN = mean, na.rm = T) %>% rename(ind_id = Group.1, climate = Group.2, season = Group.3,
                                                           treatment = Group.4, plot = Group.5, viable = x)
sca_3in_bag <- aggregate(sca_bag$viable, by = list(sca_bag$ind_id, sca_bag$climate, 
                                                   sca_bag$season, sca_bag$treatment, sca_bag$plot),
                         FUN = mean, na.rm = T) %>% rename(ind_id = Group.1, climate = Group.2, season = Group.3,
                                                           treatment = Group.4, plot = Group.5, viable = x)

sca_3in_bag <- subset(sca_3in_bag, !is.nan(sca_3in_bag$viable))

dia_mod_bag <- lmer(viable ~ climate * season * treatment + (1|ind_id/plot/climate), 
                    data = dia_3in_bag)
sca_mod_bag <- lmer(viable ~ climate * season * treatment + (1|ind_id/plot/climate), 
                    data = sca_3in_bag)

car::Anova(dia_mod_bag)
car::Anova(sca_mod_bag)

anova(lme(viable ~ climate * season * treatment, random =  ~1|ind_id/plot/climate, 
          data = dia_3in_bag))
anova(lme(viable ~ climate * season * treatment, random =  ~1|ind_id/plot/climate, 
          data = sca_3in_bag))
## plot the three-way interaction
## change season to factor (makes it easy to change order of season for facet wrap)
dia_3in_bag$season <- factor(dia_3in_bag$season, levels = c("summer", "fall"))

dia_pl_bag <- aggregate(dia_3in_bag$viable, by = list(dia_3in_bag$climate, dia_3in_bag$treatment, dia_3in_bag$season), 
                        FUN = mean, na.rm = T) %>% rename(climate = Group.1, treatment = Group.2, season = Group.3,
                                                          viable = x)
sca_pl_bag <- aggregate(sca_3in_bag$viable, by = list(sca_3in_bag$climate, sca_3in_bag$treatment, sca_3in_bag$season), 
                        FUN = mean, na.rm = T) %>% rename(climate = Group.1, treatment = Group.2, season = Group.3, 
                                                          viable = x)

dia_pl_bag$se <- aggregate(dia_3in_bag$viable, by = list(dia_3in_bag$climate, dia_3in_bag$treatment, dia_3in_bag$season), 
                           FUN = standard_error)$x
sca_pl_bag$se <- aggregate(sca_3in_bag$viable, by = list(sca_3in_bag$climate, sca_3in_bag$treatment, sca_3in_bag$season), 
                           FUN = standard_error)$x

dia_bag_pl <- ggplot(dia_pl_bag, aes(x = treatment, y = viable, fill = climate)) +
  geom_bar(stat = "identity", position = "dodge") + 
  theme_bw() + scale_fill_manual(values = rbPalette) + facet_wrap(~ season) + 
  geom_errorbar(aes(ymin = viable - se, ymax = viable + se), width = 0.5, position = position_dodge(0.85)) + 
  geom_point(dia_3in_bag, mapping = aes(x = treatment, y = viable, shape = climate), position = position_dodge(0.85)) + 
  labs(title = expression(paste(italic("D. carthusianorum"))), x = "", y = "Viable seeds per seed capsule") + 
  guides(fill = guide_legend(title = "Climate"), shape = guide_legend(title = "Climate"))

sca_bag_pl <- ggplot(sca_pl_bag, aes(x = treatment, y = viable, fill = climate)) + geom_bar(stat = "identity", position = "dodge") + 
  theme_bw() + scale_fill_manual(values = rbPalette) + facet_wrap(~ season) + 
  geom_errorbar(aes(ymin = viable - se, ymax = viable + se), width = 0.5, position = position_dodge(0.85)) + 
  geom_point(sca_3in_bag, mapping = aes(x = treatment, y = viable, shape = climate), position = position_dodge(0.85)) + 
  labs(title = expression(paste(italic("S. ochroleuca"))), x = "", y = "Viable seeds per seed head") + 
  guides(fill = guide_legend(title = "Climate"), shape = guide_legend(title = "Climate"))

ggpubr::ggarrange(dia_bag_pl, sca_bag_pl, nrow = 2)

ggsave("C://Users/ma22buky/Documents/PhD/Pollination_experiment/Paper/Publication_figs/both_sea.jpeg", 
       dpi = 300, plot = last_plot())
