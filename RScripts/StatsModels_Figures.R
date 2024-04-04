# Script for survival analyses, survival figures, and behavioral shift figures

# KDO and FER
# Last updated September 2022 by FER

# Load packages using function ----
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Packages that I want 
packages <- c("remotes",
              "tidyr",
              "tidyverse",
              "reshape2",
              "lme4",
              "rgl",
              "ggplot2",
              "modelr",
              "shinystan",
              "survival", ## The survivor package!!
              "survminer", ## for plotting in ggplot
              "gridExtra",
              "dplyr",
              "gtsummary",
              "scales",
              "viridisLite",
              "gghalves",
              "ggdist")

#Run the ipak loop
ipak(packages)



# load data ----
oneyr <- read.csv("Data/NCvsC_1year_TSH_Nov29.csv") %>%
  mutate(ModelType = 1)

oneyr_null <- read.csv("Data/NCvsC_Nullyear_TSH_Nov19.csv") %>%
  mutate(ModelType = 0)

fiveyr <- read.csv("Data/NCvsC_5year_TSH_Nov29.csv")%>%
  mutate(ModelType = 1)

fiveyr_null <- read.csv("Data/NCvsC_NULL_5year_TSH_Nov29.csv")  %>%
  mutate(ModelType = 0)


###### Reformat 1 yr data for plotting
oneyr_use <- oneyr %>%
  mutate(propHabitat = White.Table/(Black.Table + White.Table)) %>%
  mutate(propSafeSpace = Domain.Prey/(Domain.Prey + DomainOverlap)) %>%
  mutate(propPredFree = rowSums(.[28:39])/rowSums(.[16:39]))%>%
  mutate(interactions = rowSums(.[41:63])) %>%
  mutate(Pred.Prey_Domain =interaction(Pred.Start.Con, Prey.Start.Con))

oneyr_use$Pred.Strat <- as.factor(oneyr_use$Pred.Strat)
oneyr_use$Pred.Start.Con <- as.factor(oneyr_use$Pred.Start.Con)
oneyr_use$Prey.Start.Con <- as.factor(oneyr_use$Prey.Start.Con)

# one yr data for models ----
OneYearNullandTrue <- rbind(oneyr, oneyr_null)

OneYearNullandTrue <- OneYearNullandTrue  %>%
  mutate(propHabitat = White.Table/(Black.Table + White.Table)) %>%
  mutate(propSafeSpace = Domain.Prey/(Domain.Prey + DomainOverlap)) %>%
  mutate(propPredFree = rowSums(.[28:39])/rowSums(.[16:39]))%>%
  mutate(interactions = rowSums(.[41:63])) %>%
  mutate(Pred.Prey_Domain =interaction(Pred.Start.Con, Prey.Start.Con))

# make a dataframe with all 5 year data
OneYearNullandTrue2 <- OneYearNullandTrue %>%
  mutate(status = ifelse (ticks == 43800, 0, 1)) %>%
  mutate(year = ticks/365/24)

OneYearNullandTrue2$Pred.Strat <- as.factor(OneYearNullandTrue2$Pred.Strat)
OneYearNullandTrue2$Pred.Start.Con <- as.factor(OneYearNullandTrue2$Pred.Start.Con)
OneYearNullandTrue2$Prey.Start.Con <- as.factor(OneYearNullandTrue2$Prey.Start.Con)

###### 5 year data for models ----

# bind CE only and CE + NCE model results
FiveYearNullandTrue <- rbind(fiveyr, fiveyr_null)

# calculate prop safe space, prop habitat, and prop time shifts
FiveYearNullandTrue <- FiveYearNullandTrue  %>%
  mutate(propHabitat = White.Table/(Black.Table + White.Table)) %>%
  mutate(propSafeSpace = Domain.Prey/(Domain.Prey + DomainOverlap)) %>%
  mutate(propPredFree = rowSums(.[28:39])/rowSums(.[16:39]))%>%
  mutate(interactions = rowSums(.[41:63])) %>%
  mutate(Pred.Prey_Domain =interaction(Pred.Start.Con, Prey.Start.Con))

# make a dataframe and convert ticks to 'years' and set alive vs. dead status
# prepping the data for the 'survivor' package - need a column called status which is binary
# to determine if the 'event' (dying) occurred or not
FiveYearNullandTrue2 <- FiveYearNullandTrue %>%
  mutate(status = ifelse (ticks == 43800, 0, 1)) %>%
  mutate(year = ticks/365/24)

# make sure starting conditions are factors
FiveYearNullandTrue2$Pred.Strat <- as.factor(FiveYearNullandTrue2$Pred.Strat)
FiveYearNullandTrue2$Pred.Start.Con <- as.factor(FiveYearNullandTrue2$Pred.Start.Con)
FiveYearNullandTrue2$Prey.Start.Con <- as.factor(FiveYearNullandTrue2$Prey.Start.Con)




#######################SURIVAL PACKAGE ----

# CE + NCE effects
FiveYearTrue <-
  FiveYearNullandTrue2 %>%
  subset(ModelType == 1)

# only CE
FiveYearCE <-
  FiveYearNullandTrue2 %>%
  subset(ModelType == 0)


## Make Hazard Ratio tables ----
FiveYearNCE <-
  coxph(Surv(year, status) ~ propHabitat + propPredFree + propSafeSpace,
        data = FiveYearTrue)
haz.table <- FiveYearNCE %>%
  gtsummary::tbl_regression(exp = TRUE) 


# behavioral changes for CE only
FiveYearCEmod <-
  coxph(Surv(year, status) ~ Pred.Strat + Pred.Start.Con*Prey.Start.Con,
        data = FiveYearCE)
haz.table <- FiveYearCEmod %>%
  gtsummary::tbl_regression(exp = TRUE) 


## Look at hazard ratio of behavioral changes by predator hunting mode

## Active predators
Active5 <- FiveYearNullandTrue2 %>%
  subset(Pred.Strat == "Active")

# Small/Small
Active5SS <- Active5 %>%
  subset(Pred.Prey_Domain == "Small.Small")
ActiveFiveYearNCE <-
  coxph(Surv(year, status) ~ propHabitat + propPredFree + propSafeSpace,
        data = Active5SS)
exp(confint(ActiveFiveYearNCE))
haz.table <- ActiveFiveYearNCE %>%
  gtsummary::tbl_regression(exp = TRUE) 

# Small/Large
Active5SL <- Active5 %>%
  subset(Pred.Prey_Domain == "Small.Large")
ActiveFiveYearNCE <-
  coxph(Surv(year, status) ~ propHabitat + propPredFree + propSafeSpace,
        data = Active5SL)
exp(confint(ActiveFiveYearNCE))
haz.table <- ActiveFiveYearNCE %>%
  gtsummary::tbl_regression(exp = TRUE) 

# Large/Small
Active5LS <- Active5 %>%
  subset(Pred.Prey_Domain == "Large.Small")
ActiveFiveYearNCE <-
  coxph(Surv(year, status) ~ propHabitat + propPredFree + propSafeSpace,
        data = Active5LS)
exp(confint(ActiveFiveYearNCE))
haz.table <- ActiveFiveYearNCE %>%
  gtsummary::tbl_regression(exp = TRUE) 


# Large/Large
Active5LL <- Active5 %>%
  subset(Pred.Prey_Domain == "Large.Large")
ActiveFiveYearNCE <-
  coxph(Surv(year, status) ~ propHabitat + propPredFree + propSafeSpace,
        data = Active5LL)
exp(confint(ActiveFiveYearNCE))
haz.table <- ActiveFiveYearNCE %>%
  gtsummary::tbl_regression(exp = TRUE) 



## Sit-and-Pursue predators
SP5 <- FiveYearTrue %>%
  subset(Pred.Strat == "Sit-and-Pursue")

# Small/Small
SP5SS <- SP5 %>%
  subset(Pred.Prey_Domain == "Small.Small")
SPFiveYearNCE <-
  coxph(Surv(year, status) ~ propHabitat + propPredFree + propSafeSpace,
        data = SP5SS)
exp(confint(SPFiveYearNCE))
haz.table <- SPFiveYearNCE %>%
  gtsummary::tbl_regression(exp = TRUE) 

# Small/Large
SP5SL <- SP5 %>%
  subset(Pred.Prey_Domain == "Small.Large")
SPFiveYearNCE <-
  coxph(Surv(year, status) ~ propHabitat + propPredFree + propSafeSpace,
        data = SP5SL)
exp(confint(SPFiveYearNCE))
haz.table <- SPFiveYearNCE %>%
  gtsummary::tbl_regression(exp = TRUE) 

# Large/Small
SP5LS <- SP5 %>%
  subset(Pred.Prey_Domain == "Large.Small")
SPFiveYearNCE <-
  coxph(Surv(year, status) ~ propHabitat + propPredFree + propSafeSpace,
        data = SP5LS)
exp(confint(SPFiveYearNCE))
haz.table <- SPFiveYearNCE %>%
  gtsummary::tbl_regression(exp = TRUE) 

# Large/Large
SP5LL <- SP5 %>%
  subset(Pred.Prey_Domain == "Large.Large")
SPFiveYearNCE <-
  coxph(Surv(year, status) ~ propHabitat + propPredFree + propSafeSpace,
        data = SP5LL)
exp(confint(SPFiveYearNCE)) # get HR 95% CI
haz.table <- SPFiveYearNCE %>%
  gtsummary::tbl_regression(exp = TRUE) 


## Sit-and-Wait predators
SW5 <- FiveYearTrue %>%
  subset(Pred.Strat == "Sit-and-Wait")

# Small/Small
SW5SS <- SW5 %>%
  subset(Pred.Prey_Domain == "Small.Small")
SWFiveYearNCE <-
  coxph(Surv(year, status) ~ propHabitat + propPredFree + propSafeSpace,
        data = SW5SS)
exp(confint(SWFiveYearNCE)) # get HR 95% CI
haz.table <- SWFiveYearNCE %>%
  gtsummary::tbl_regression(exp = TRUE) 

# Small/Large
SW5SL <- SW5 %>%
  subset(Pred.Prey_Domain == "Small.Large")
SWFiveYearNCE <-
  coxph(Surv(year, status) ~ propHabitat + propPredFree + propSafeSpace,
        data = SW5SL)
exp(confint(SWFiveYearNCE)) # get HR 95% CI
haz.table <- SWFiveYearNCE %>%
  gtsummary::tbl_regression(exp = TRUE) 

# Large/Small
SW5LS <- SW5 %>%
  subset(Pred.Prey_Domain == "Large.Small")
SWFiveYearNCE <-
  coxph(Surv(year, status) ~ propHabitat + propPredFree + propSafeSpace,
        data = SP5LS)
exp(confint(SWFiveYearNCE)) # get HR 95% CI
haz.table <- SWFiveYearNCE %>%
  gtsummary::tbl_regression(exp = TRUE) 

# Large/Large
SW5LL <- SW5 %>%
  subset(Pred.Prey_Domain == "Large.Large")
SWFiveYearNCE <-
  coxph(Surv(year, status) ~ propHabitat + propPredFree + propSafeSpace,
        data = SW5LL)
exp(confint(SWFiveYearNCE)) # get HR 95% CI
haz.table <- SWFiveYearNCE %>%
  gtsummary::tbl_regression(exp = TRUE) 






## Plot the hazard ratio data ----
# load summary file
HR_summ <- read.csv("Data/HazardRatioSummary_Feb2022.csv", header = TRUE, fileEncoding="UTF-8-BOM")

# I know there is a better way to do this, but I can't remember it. Ha.
HR_summ$Pred.Strat <- as.factor(HR_summ$Pred.Strat)
HR_summ$Pred.Start.Con <- as.factor(HR_summ$Pred.Start.Con)
HR_summ$Prey.Start.Con <- as.factor(HR_summ$Prey.Start.Con)
HR_summ$HR.Type <- as.factor(HR_summ$HR.Type)

# reorder
HRsumm_new <- HR_summ
HRsumm_new$Pred.Start.Con <- factor(HRsumm_new$Pred.Start.Con, 
                                       levels = c("Small", "Large"))
HRsumm_new$Prey.Start.Con <- factor(HRsumm_new$Prey.Start.Con, 
                                    levels = c("Small", "Large"))

# create new labels for the facets
new_labels <- c("Small" = "Predator Small Domain", "Large" = "Predator Large Domain")
new_labels2 <- c("Small" = "Prey Small Domain", "Large" = "Prey Large Domain")

# Removal of points that had consumptive effects
HRsumm_new1<- HRsumm_new %>%
  mutate(CE_or_NCE = case_when(Pred.Strat == "Active" & Pred.Prey_Domain =="Small.Small" ~ "CE",
                               Pred.Strat == "Active" & Pred.Prey_Domain == "Large.Small" ~ "CE",
                               Pred.Strat == "Sit-and-Pursue" & Pred.Prey_Domain == "Large.Large" ~ "CE",
                               Pred.Strat == "Sit-and-Wait" & Pred.Prey_Domain == "Large.Large" ~ "CE")) %>%
  mutate(CE_or_NCE = case_when(is.na(CE_or_NCE) == TRUE ~"NCE",
                               is.na(CE_or_NCE) != TRUE ~"CE"))

#HRsumm_new1<-HRsumm_new1 %>%
 # subset(CE_or_NCE == "NCE")

 # plot of hazard ratios of only non-consumptive effects
# Plot of hazard ratios
HR_plot <- ggplot(HRsumm_new1,
                  aes(
                    x = HR.Type,
                    y = HazardRatio,
                    fill = Pred.Strat,
                    group = Pred.Strat
                  )) +
  geom_errorbar(
    aes(ymin = Lower95CI, ymax = Upper95CI, color = Pred.Strat),
    width = 0.3,
    position = "dodge"
  ) +
  geom_point(pch = 21,
             size = 4,
             position = position_dodge(width = 0.3)) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  scale_fill_viridis_d(begin = 0, end = 0.9, name = "Hunting Mode") +
  scale_color_viridis_d(begin = 0, end = 0.9, name = "Hunting Mode") +
  ylab("Hazard Ratio") +
  xlab("Type of Shift") +
  geom_hline(yintercept = 1, linetype = "dotted") +
  scale_y_continuous(
    trans = log10_trans(),
    breaks = trans_breaks("log10", function(x)
      10 ^ x),
    labels = trans_format("log10", math_format(10 ^ .x))
  ) +
  scale_x_discrete(labels=c("Habitat" = "Habitat", "PredFree" = "Time",
                            "SafeSpace" = "Space")) +
  facet_grid(Pred.Start.Con ~ Prey.Start.Con,
             labeller = labeller(Pred.Start.Con = new_labels,
                                 Prey.Start.Con = new_labels2))


print(HR_plot)

ggsave(HR_plot, filename = "Output_Figures/HazardRatiosPlot_NCEonly.png", dpi = 300, width = 8, height = 5)


# Plot of hazard ratios of all (CE and NCE)
HR_plot <- ggplot(HRsumm_new1,
                  aes(
                    x = HR.Type,
                    y = HazardRatio,
                    fill = Pred.Strat,
                   # alpha = CE_or_NCE,
                    shape = CE_or_NCE,
                    group = Pred.Strat
                  )) +
  geom_errorbar(
    aes(ymin = Lower95CI, ymax = Upper95CI, color = Pred.Strat),
    width = 0.3,
    position = "dodge"
  ) +
  geom_point(
             aes(shape = CE_or_NCE, pch = 21,
                 size = 4),
             position = position_dodge(width = 0.3)) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  scale_fill_viridis_d(begin = 0, end = 0.9, name = "Hunting Mode") +
  scale_color_viridis_d(begin = 0, end = 0.9, name = "Hunting Mode") +
 # scale_alpha_discrete(name = "Dominant Effect") +

  ylab("Hazard Ratio") +
  xlab("Type of Shift") +
  geom_hline(yintercept = 1, linetype = "dotted") +
  scale_y_continuous(
    trans = log10_trans(),
    breaks = trans_breaks("log10", function(x)
      10 ^ x),
    labels = trans_format("log10", math_format(10 ^ .x))
  ) +
  scale_x_discrete(labels=c("Habitat" = "Habitat", "PredFree" = "Time",
                            "SafeSpace" = "Space")) +
  scale_shape_manual(values = c(23,21), name = "Dominant Effect") +
  guides(size = "none")+
  facet_grid(Pred.Start.Con ~ Prey.Start.Con,
             labeller = labeller(Pred.Start.Con = new_labels,
                                Prey.Start.Con = new_labels2))


print(HR_plot)

ggsave(HR_plot, filename = "Output_Figures/HazardRatiosPlot_CEvsNCE.png", dpi = 300, width = 8, height = 5)









## make time plots for yr = 0-1 and yr = 0-5 to look at shifts ----

##### One yr data

# reorder
oneyr_new <- oneyr_use
oneyr_new$Pred.Start.Con <- factor(oneyr_new$Pred.Start.Con, 
                                    levels = c("Small", "Large"))
oneyr_new$Prey.Start.Con <- factor(oneyr_new$Prey.Start.Con, 
                                    levels = c("Small", "Large"))

# create new labels for the facets
new_labels <- c("Small" = "Predator Small Domain", "Large" = "Predator Large Domain")
new_labels2 <- c("Small" = "Prey Small Domain", "Large" = "Prey Large Domain")

## melt the dataframe for extra column for shift
# add unique row id
oneyr_new$index <- 1:nrow(oneyr_new)
head(oneyr_new)
molted <- melt(value.name = "BehaviorShift", oneyr_new[,c(65:67, 70)],id.vars=c("index"))
molted

oneyrshifts <- left_join(molted, oneyr_new[,c(1:4,69:70)], by = "index")
summary(oneyrshifts)

# reorder behavior shifts
oneyrshifts$variable <- factor(oneyrshifts$variable, 
                                   levels = c("propHabitat", "propPredFree", "propSafeSpace"))

# make zeros into NA for space shifts
oneyrshifts$BehaviorShift <- ifelse(oneyrshifts$BehaviorShift == 0, "NA", oneyrshifts$BehaviorShift)
oneyrshifts$BehaviorShift <- as.numeric(as.character(oneyrshifts$BehaviorShift))
summary(oneyrshifts)


# Plot of behavioral shifts
oneyrshift_plot <- ggplot(oneyrshifts,
                  aes(
                    x = variable,
                    y = BehaviorShift,
                    fill = Pred.Strat,
                    group = Pred.Strat
                  )) +
  geom_hline(yintercept = 0.5, linetype = "dotted") +
  ggdist::stat_halfeye(
    adjust = 1,
    normalize = "groups",
    position = position_dodge(width = 0.5),
    # width = 1, 
    # height = 1,
    ## set slab interval to show IQR and 95% data range
    .width = c(.5, .95),
    aes(fill = Pred.Strat),
    slab_alpha = 0.7
  ) + 
  theme_bw(base_size = 14) +
  ylim(0,1) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  scale_fill_viridis_d(begin = 0., end = 0.9, name = "Hunting Mode") +
  scale_color_viridis_d(begin = 0., end = 0.9, name = "Hunting Mode") +
  ylab("Prey Proportional Shift to Safety") +
  xlab("Type of Shift") +
  scale_x_discrete(labels=c("propHabitat" = "Habitat", "propSafeSpace" = "Space",
                            "propPredFree" = "Time")) +
  facet_grid(Pred.Start.Con ~ Prey.Start.Con,
             labeller = labeller(Pred.Start.Con = new_labels,
                                 Prey.Start.Con = new_labels2)) +
  ggtitle("Shifts at one year")


print(oneyrshift_plot)

ggsave(oneyrshift_plot, filename = "Output_Figures/OneYrShifts.png", dpi = 300, width = 8, height = 5)







#### Five year data behavioral shifts

# reorder
fiveyr_new <- FiveYearTrue
fiveyr_new$Pred.Start.Con <- factor(fiveyr_new$Pred.Start.Con, 
                                   levels = c("Small", "Large"))
fiveyr_new$Prey.Start.Con <- factor(fiveyr_new$Prey.Start.Con, 
                                   levels = c("Small", "Large"))

# create new labels for the facets
new_labels <- c("Small" = "Predator Small Domain", "Large" = "Predator Large Domain")
new_labels2 <- c("Small" = "Prey Small Domain", "Large" = "Prey Large Domain")

## melt the dataframe for extra column for shift
# add unique row id
fiveyr_new$index <- 1:nrow(fiveyr_new)
head(fiveyr_new)
molted <- melt(value.name = "BehaviorShift", fiveyr_new[,c(65:67, 72)],id.vars=c("index"))
molted

fiveyrshifts <- left_join(molted, fiveyr_new[,c(2:4,69:70, 72)], by = "index")
summary(fiveyrshifts)

# reorder behavior shifts
fiveyrshifts$variable <- factor(fiveyrshifts$variable, 
                               levels = c("propHabitat", "propPredFree", "propSafeSpace"))

fiveyrshifts$BehaviorShift <- ifelse(fiveyrshifts$BehaviorShift == 0, "NA", fiveyrshifts$BehaviorShift)
fiveyrshifts$BehaviorShift <- as.numeric(as.character(fiveyrshifts$BehaviorShift))
summary(fiveyrshifts)


## removing Consumptive effect
fiveyrshifts1<- fiveyrshifts %>%
  mutate(CE_or_NCE = case_when(Pred.Strat == "Active" & Pred.Prey_Domain =="Small.Small" ~ "CE",
                               Pred.Strat == "Active" & Pred.Prey_Domain == "Large.Small" ~ "CE",
                               Pred.Strat == "Sit-and-Pursue" & Pred.Prey_Domain == "Large.Large" ~ "CE",
                               Pred.Strat == "Sit-and-Wait" & Pred.Prey_Domain == "Large.Large" ~ "CE")) %>%
  mutate(CE_or_NCE = case_when(is.na(CE_or_NCE) == TRUE ~"NCE",
                               is.na(CE_or_NCE) != TRUE ~"CE"))
#fiveyrshifts1<-fiveyrshifts1 %>%
#  subset(CE_or_NCE == "NCE")

## Plot highlighting consumptive vs non consumptive effects
fiveyrshift_plot <- ggplot(fiveyrshifts1,
                           aes(
                             x = variable,
                             y = BehaviorShift,
                             fill = Pred.Strat,
                             shape = CE_or_NCE,
                             group = Pred.Strat
                           )) +
  geom_hline(yintercept = 0.5, linetype = "dotted") +
  ggdist::stat_halfeye(
    adjust = 1,
    normalize = "groups",
    position = position_dodge(width = 0.5),
    ## set slab interval to show IQR and 95% data range
    .width = c(.5, .95),
    aes(fill = Pred.Strat),
    slab_alpha = 0.7) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  ylim(0, 1) +
  scale_fill_viridis_d(begin = 0., end = 0.9, name = "Hunting Mode") +
  scale_color_viridis_d(begin = 0., end = 0.9, name = "Hunting Mode") +
  ylab("Prey Proportional Shift to Safety") +
  xlab("Type of Shift") +
  scale_x_discrete(labels=c("propHabitat" = "Habitat", "propPredFree" = "Time", 
                            "propSafeSpace" = "Space")) +
  scale_shape_manual(values = c(23,16), name = "Dominant Effect") +
  ## here ^
  facet_grid(Pred.Start.Con ~ Prey.Start.Con,
             labeller = labeller(Pred.Start.Con = new_labels,
                                 Prey.Start.Con = new_labels2)) +
  ggtitle("Shifts at five years")



print(fiveyrshift_plot)

ggsave(fiveyrshift_plot, filename = "Output_Figures/FiveYrShifts_NCEvsCE.png", dpi = 300, width = 8, height = 5)


###


### Plot of behavioral shifts
fiveyrshift_plot <- ggplot(fiveyrshifts1,
                          aes(
                            x = variable,
                            y = BehaviorShift,
                            fill = Pred.Strat,
                          
                            group = Pred.Strat
                          )) +
  geom_hline(yintercept = 0.5, linetype = "dotted") +
  ggdist::stat_halfeye(
    adjust = 1,
    normalize = "groups",
    position = position_dodge(width = 0.5),
    ## set slab interval to show IQR and 95% data range
    .width = c(.5, .95),
    aes(fill = Pred.Strat),
    slab_alpha = 0.7) +
theme_bw(base_size = 14) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  ylim(0, 1) +
  scale_fill_viridis_d(begin = 0., end = 0.9, name = "Hunting Mode") +
  scale_color_viridis_d(begin = 0., end = 0.9, name = "Hunting Mode") +
  ylab("Prey Proportional Shift to Safety") +
  xlab("Type of Shift") +
  scale_x_discrete(labels=c("propHabitat" = "Habitat", "propPredFree" = "Time", 
                            "propSafeSpace" = "Space")) +

  facet_grid(Pred.Start.Con ~ Prey.Start.Con,
             labeller = labeller(Pred.Start.Con = new_labels,
                                 Prey.Start.Con = new_labels2)) +
  ggtitle("Shifts at five years")


print(fiveyrshift_plot)

ggsave(fiveyrshift_plot, filename = "Output_Figures/FiveYrShifts.png", dpi = 300, width = 8, height = 5)



#################### Prepping separate hunting modes and looking at survival plots across all of the data

## overall models
## all
FiveYearNullandTrue <- FiveYearNullandTrue %>%
  mutate(Pred.Prey_Domain =interaction(Pred.Start.Con, Prey.Start.Con))

FiveYearNullandTrue_surv <- FiveYearNullandTrue %>%
  mutate(status = ifelse (ticks == 43800, 0, 1)) %>%
  mutate(year = ticks/365/24)

allmod <- survfit(Surv(year, status) ~ ModelType, data = FiveYearNullandTrue_surv)



## to get colors to match the other plot, I am using viridisLite to get the hex codes
# > viridis(3, alpha = 1, begin = 0, end = 0.9, direction = 1, option = "D")
# [1] "#440154FF" "#25848EFF" "#BBDF27FF"




####### Comparing consumptive vs. non-consumptive effects ----

# > viridis(11, alpha = 1, begin = 0, end = 1, direction = 1, option = "D")
# [1] "#440154FF" "#482576FF" "#414487FF" "#35608DFF" "#2A788EFF" "#21908CFF" "#22A884FF"
# [8] "#43BF71FF" "#7AD151FF" "#BBDF27FF" "#FDE725FF"

## Active survival models ----

OneYearNullandTrue.A <- OneYearNullandTrue %>%
  subset(Pred.Strat == "Active") %>%
  mutate(Pred.Prey_Domain =interaction(Pred.Start.Con, Prey.Start.Con))

OneYearNullandTrue.A_surv <- OneYearNullandTrue.A %>%
  mutate(status = ifelse (ticks == 43800, 0, 1)) %>%
  mutate(year = ticks/365/24)

FiveYearNullandTrue.A <- FiveYearNullandTrue %>%
  subset(Pred.Strat == "Active") %>%
  mutate(Pred.Prey_Domain =interaction(Pred.Start.Con, Prey.Start.Con))

FiveYearNullandTrue.A_surv <- FiveYearNullandTrue.A %>%
  mutate(status = ifelse (ticks == 43800, 0, 1)) %>%
  mutate(year = ticks/365/24)

# reorder
OneYearNullandTrue.A_surv$Pred.Start.Con <- factor(OneYearNullandTrue.A_surv$Pred.Start.Con, 
                                                    levels = c("Small", "Large"))
OneYearNullandTrue.A_surv$Prey.Start.Con <- factor(OneYearNullandTrue.A_surv$Prey.Start.Con, 
                                                    levels = c("Small", "Large"))

FiveYearNullandTrue.A_surv$Pred.Start.Con <- factor(FiveYearNullandTrue.A_surv$Pred.Start.Con, 
                                    levels = c("Small", "Large"))
FiveYearNullandTrue.A_surv$Prey.Start.Con <- factor(FiveYearNullandTrue.A_surv$Prey.Start.Con, 
                                    levels = c("Small", "Large"))



## Facet plot for active predators
fitA1 <- survfit( Surv(year, status) ~ ModelType, data = OneYearNullandTrue.A_surv)
fitA <- survfit( Surv(year, status) ~ ModelType, data = FiveYearNullandTrue.A_surv)
ActiveMulti <- ggsurvplot_facet(fitA, 
                                FiveYearNullandTrue.A_surv, 
                                conf.int = TRUE,
                                facet.by = c("Pred.Start.Con", "Prey.Start.Con"),
                                palette = c("gray", "#440154FF"),
                                surv.median.line = "v", # add median survival
                                pval = TRUE,
                                pval.coord = c(3.5, 0.8),
                                ggtheme = theme_bw(base_size = 16),
                                short.panel.labs = TRUE,
                                panel.labs = list(Prey.Start.Con = c("Prey Small Domain", "Prey Large Domain"),
                                                  Pred.Start.Con = c("Predator Small Domain", "Predator Large Domain")),
                                legend.labs = c("CE", "CE + NCE"),
                                size = 1,
                                title = "(a) Active Hunting Mode") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
ActiveMultiOne <- ggsurvplot_facet(fitA1, 
                                OneYearNullandTrue.A_surv, 
                                conf.int = TRUE,
                                facet.by = c("Pred.Start.Con", "Prey.Start.Con"),
                                palette = c("gray", "#440154FF"),
                                surv.median.line = "v", # add median survival
                                pval = TRUE,
                                pval.coord = c(0.02, 0.2),
                                ggtheme = theme_bw(base_size = 16),
                                short.panel.labs = TRUE,
                                panel.labs = list(Prey.Start.Con = c("Prey Small Domain", "Prey Large Domain"),
                                                  Pred.Start.Con = c("Predator Small Domain", "Predator Large Domain")),
                                legend.labs = c("CE", "CE + NCE"),
                                size = 1,
                                title = "(a) Active Hunting Mode") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
print(ActiveMultiOne)
ggsave("Output_Figures/OneYr_ActivePredSurv.png", dpi = 300, height = 6, width = 7)

print(ActiveMulti)
ggsave(plot = ActiveMulti, "Output_Figures/FiveYr_ActivePredSurv.png", dpi = 300, height = 6, width = 7)





## Sit-and-wait survival models ----

OneYearNullandTrue.SW <- OneYearNullandTrue %>%
  subset(Pred.Strat == "Sit-and-Wait") %>%
  mutate(Pred.Prey_Domain =interaction(Pred.Start.Con, Prey.Start.Con))

OneYearNullandTrue.SW_surv <- OneYearNullandTrue.SW %>%
  mutate(status = ifelse (ticks == 43800, 0, 1)) %>%
  mutate(year = ticks/365/24)

FiveYearNullandTrue.SW <- FiveYearNullandTrue %>%
  subset(Pred.Strat == "Sit-and-Wait") %>%
  mutate(Pred.Prey_Domain =interaction(Pred.Start.Con, Prey.Start.Con))

FiveYearNullandTrue.SW_surv <- FiveYearNullandTrue.SW %>%
  mutate(status = ifelse (ticks == 43800, 0, 1)) %>%
  mutate(year = ticks/365/24)

# reorder
OneYearNullandTrue.SW_surv$Pred.Start.Con <- factor(OneYearNullandTrue.SW_surv$Pred.Start.Con, 
                                                     levels = c("Small", "Large"))
OneYearNullandTrue.SW_surv$Prey.Start.Con <- factor(OneYearNullandTrue.SW_surv$Prey.Start.Con, 
                                                     levels = c("Small", "Large"))

FiveYearNullandTrue.SW_surv$Pred.Start.Con <- factor(FiveYearNullandTrue.SW_surv$Pred.Start.Con, 
                                                    levels = c("Small", "Large"))
FiveYearNullandTrue.SW_surv$Prey.Start.Con <- factor(FiveYearNullandTrue.SW_surv$Prey.Start.Con, 
                                                    levels = c("Small", "Large"))


# SW facet plot
fitSW1 <- survfit( Surv(year, status) ~ ModelType, data = OneYearNullandTrue.SW_surv)
fitSW <- survfit( Surv(year, status) ~ ModelType, data = FiveYearNullandTrue.SW_surv)
SWMulti <- ggsurvplot_facet(fitSW, 
                                FiveYearNullandTrue.SW_surv, 
                                conf.int = TRUE,
                                facet.by = c("Pred.Start.Con", "Prey.Start.Con"),
                                palette = c("gray", "#BBDF27FF"),
                                surv.median.line = "v", # add median survival
                                pval = TRUE,
                                pval.coord = c(0.1, 0.2),
                                ggtheme = theme_bw(base_size = 16),
                                short.panel.labs = TRUE,
                                panel.labs = list(Prey.Start.Con = c("Prey Small Domain", "Prey Large Domain"),
                                                 Pred.Start.Con = c("Predator Small Domain", "Predator Large Domain")),
                                legend.labs = c("CE", "CE + NCE"),
                                size = 1,
                                title = "(c) Sit-and-Wait Hunting Mode") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
SWMulti1 <- ggsurvplot_facet(fitSW1,
                            OneYearNullandTrue.SW_surv,
                            conf.int = TRUE,
                            facet.by = c("Pred.Start.Con", "Prey.Start.Con"),
                            palette = c("gray", "#BBDF27FF"),
                            surv.median.line = "v", # add median survival
                            pval = TRUE,
                            pval.coord = c(0.1, 0.2),
                            ggtheme = theme_bw(base_size = 16),
                            short.panel.labs = TRUE,
                            panel.labs = list(Prey.Start.Con = c("Prey Small Domain", "Prey Large Domain"),
                                              Pred.Start.Con = c("Predator Small Domain", "Predator Large Domain")),
                            legend.labs = c("CE", "CE + NCE"),
                            size = 1,
                            title = "(c) Sit-and-Wait Hunting Mode") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
print(SWMulti1)
print(SWMulti)
ggsave(SWMulti1, filename = "Output_Figures/OneYr_SWPredSurv.png", dpi = 300, height = 6, width = 7)
ggsave(SWMulti, filename = "Output_Figures/FiveYr_SWPredSurv.png", dpi = 300, height = 6, width = 7)

# SW small/small
SW_SS <- FiveYearNullandTrue.SW_surv %>%
  subset(Pred.Prey_Domain == "Small.Small")
# Get regression median and p-value of Null vs. NCE
SWSSmod <- survfit(Surv(year, status) ~ ModelType, data = SW_SS)
SWSSmod

# SW small/large
SW_SL <- FiveYearNullandTrue.SW_surv %>%
  subset(Pred.Prey_Domain == "Small.Large")
# Get regression median and p-value of Null vs. NCE
SWSLmod <- survfit(Surv(year, status) ~ ModelType, data = SW_SL)
SWSLmod

# SW large/small
SW_LS <- FiveYearNullandTrue.SW_surv %>%
  subset(Pred.Prey_Domain == "Large.Small")
# Get regression median and p-value of Null vs. NCE
SWLSmod <- survfit(Surv(year, status) ~ ModelType, data = SW_LS)
SWLSmod

# SW large/large
SW_LL <- FiveYearNullandTrue.SW_surv %>%
  subset(Pred.Prey_Domain == "Large.Large")
# Get regression median and p-value of Null vs. NCE
SWLLmod <- survfit(Surv(year, status) ~ ModelType, data = SW_LL)
SWLLmod




## Sit-and-Pursue survival models ----
OneYearNullandTrue.SP <- OneYearNullandTrue %>%
  subset(Pred.Strat == "Sit-and-Pursue") %>%
  mutate(Pred.Prey_Domain =interaction(Pred.Start.Con, Prey.Start.Con))

OneYearNullandTrue.SP_surv <- OneYearNullandTrue.SP %>%
  mutate(status = ifelse (ticks == 43800, 0, 1)) %>%
  mutate(year = ticks/365/24)

FiveYearNullandTrue.SP <- FiveYearNullandTrue %>%
  subset(Pred.Strat == "Sit-and-Pursue") %>%
  mutate(Pred.Prey_Domain =interaction(Pred.Start.Con, Prey.Start.Con))

FiveYearNullandTrue.SP_surv <- FiveYearNullandTrue.SP %>%
  mutate(status = ifelse (ticks == 43800, 0, 1)) %>%
  mutate(year = ticks/365/24)

# reorder
OneYearNullandTrue.SP_surv$Pred.Start.Con <- factor(OneYearNullandTrue.SP_surv$Pred.Start.Con, 
                                                     levels = c("Small", "Large"))
OneYearNullandTrue.SP_surv$Prey.Start.Con <- factor(OneYearNullandTrue.SP_surv$Prey.Start.Con, 
                                                     levels = c("Small", "Large"))

FiveYearNullandTrue.SP_surv$Pred.Start.Con <- factor(FiveYearNullandTrue.SP_surv$Pred.Start.Con, 
                                                     levels = c("Small", "Large"))
FiveYearNullandTrue.SP_surv$Prey.Start.Con <- factor(FiveYearNullandTrue.SP_surv$Prey.Start.Con, 
                                                     levels = c("Small", "Large"))

# SP facet plot
fitSP1 <- survfit( Surv(year, status) ~ ModelType, data = OneYearNullandTrue.SP_surv)
fitSP <- survfit( Surv(year, status) ~ ModelType, data = FiveYearNullandTrue.SP_surv)
SPMulti <- ggsurvplot_facet(fitSP, 
                            FiveYearNullandTrue.SP_surv, 
                            conf.int = TRUE,
                            facet.by = c("Pred.Start.Con", "Prey.Start.Con"),
                            palette = c("gray", "#25848EFF"),
                            surv.median.line = "v", # add median survival
                            pval = TRUE,
                            pval.coord = c(3.5, 0.8),
                            ggtheme = theme_bw(base_size = 16),
                            short.panel.labs = TRUE,
                            panel.labs = list(Prey.Start.Con = c("Prey Small Domain", "Prey Large Domain"),
                                              Pred.Start.Con = c("Predator Small Domain", "Predator Large Domain")),
                            legend.labs = c("CE", "CE + NCE"),
                            size = 1,
                            title = "(b) Sit-and-Pursue Hunting Mode") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
print(SPMulti)
ggsave("Output_Figures/FiveYr_SPPredSurv.png", dpi = 300, height = 6, width = 7)

SPMulti1 <- ggsurvplot_facet(fitSP1, 
                            OneYearNullandTrue.SP_surv, 
                            conf.int = TRUE,
                            facet.by = c("Pred.Start.Con", "Prey.Start.Con"),
                            palette = c("gray", "#25848EFF"),
                            surv.median.line = "v", # add median survival
                            pval = TRUE,
                            pval.coord = c(0.025, 0.2),
                            ggtheme = theme_bw(base_size = 16),
                            short.panel.labs = TRUE,
                            panel.labs = list(Prey.Start.Con = c("Prey Small Domain", "Prey Large Domain"),
                                              Pred.Start.Con = c("Predator Small Domain", "Predator Large Domain")),
                            legend.labs = c("CE", "CE + NCE"),
                            size = 1,
                            title = "(b) Sit-and-Pursue Hunting Mode") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
print(SPMulti1)
ggsave("Output_Figures/OneYr_SPPredSurv.png", dpi = 300, height = 6, width = 7)

# SP small/small
SP_SS <- FiveYearNullandTrue.SP_surv %>%
  subset(Pred.Prey_Domain == "Small.Small")
# Get regression median and p-value of Null vs. NCE
SPSSmod <- survfit(Surv(year, status) ~ ModelType, data = SP_SS)
SPSSmod

# SP small/large
SP_SL <- FiveYearNullandTrue.SP_surv %>%
  subset(Pred.Prey_Domain == "Small.Large")
# Get regression median and p-value of Null vs. NCE
SPSLmod <- survfit(Surv(year, status) ~ ModelType, data = SP_SL)
SPSLmod

# SP large/small
SP_LS <- FiveYearNullandTrue.SP_surv %>%
  subset(Pred.Prey_Domain == "Large.Small")
# Get regression median and p-value of Null vs. NCE
SPLSmod <- survfit(Surv(year, status) ~ ModelType, data = SP_LS)
SPLSmod

# SP large/large
SP_LL <- FiveYearNullandTrue.SP_surv %>%
  subset(Pred.Prey_Domain == "Large.Large")
# Get regression median and p-value of Null vs. NCE
SPLLmod <- survfit(Surv(year, status) ~ ModelType, data = SP_LL)
SPLLmod



