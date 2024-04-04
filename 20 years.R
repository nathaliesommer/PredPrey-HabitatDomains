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


twentyyr <- read.csv("Data/NCvsC_20year_TSH_Sept252023.csv")%>%
  mutate(ModelType = 1)

twentyyr_null <- read.csv("Data/NCvsC_NULL_20year_TSH_Sept252023.csv")  %>%
  mutate(ModelType = 0)


###### 5 year data for models ----

# bind CE only and CE + NCE model results
TwentyYearNullandTrue <- rbind(twentyyr, twentyyr_null)

# calculate prop safe space, prop habitat, and prop time shifts
TwentyYearNullandTrue <- TwentyYearNullandTrue  %>%
  mutate(propHabitat = White.Table/(Black.Table + White.Table)) %>%
  mutate(propSafeSpace = Domain.Prey/(Domain.Prey + DomainOverlap)) %>%
  mutate(propPredFree = rowSums(.[28:39])/rowSums(.[16:39]))%>%
  mutate(interactions = rowSums(.[41:63])) %>%
  mutate(Pred.Prey_Domain =interaction(Pred.Start.Con, Prey.Start.Con))

# make a dataframe and convert ticks to 'years' and set alive vs. dead status
# prepping the data for the 'survivor' package - need a column called status which is binary
# to determine if the 'event' (dying) occurred or not
TwentyYearNullandTrue2 <- TwentyYearNullandTrue %>%
  mutate(status = ifelse (ticks == 175200, 0, 1)) %>%
  mutate(year = ticks/365/24)

# make sure starting conditions are factors
TwentyYearNullandTrue2$Pred.Strat <- as.factor(TwentyYearNullandTrue2$Pred.Strat)
TwentyYearNullandTrue2$Pred.Start.Con <- as.factor(TwentyYearNullandTrue2$Pred.Start.Con)
TwentyYearNullandTrue2$Prey.Start.Con <- as.factor(TwentyYearNullandTrue2$Prey.Start.Con)




#######################SURIVAL PACKAGE ----


# CE + NCE effects
TwentyYearTrue <-
  TwentyYearNullandTrue2 %>%
  subset(ModelType == 1)

# only CE
TwentyYearCE <-
  TwentyYearNullandTrue2 %>%
  subset(ModelType == 0)


## Make Hazard Ratio tables ----
TwentyYearNCE <-
  coxph(Surv(year, status) ~ propHabitat + propPredFree + propSafeSpace,
        data = TwentyYearTrue)
haz.table <- TwentyYearNCE %>%
  gtsummary::tbl_regression(exp = TRUE) 


# behavioral changes for CE only
TwentyYearCEmod <-
  coxph(Surv(year, status) ~ Pred.Strat + Pred.Start.Con*Prey.Start.Con,
        data = TwentyYearCE)
haz.table <- TwentyYearCEmod %>%
  gtsummary::tbl_regression(exp = TRUE) 


### KO SKIPPED HERE
## Look at hazard ratio of behavioral changes by predator hunting mode

## Active predators
Active10 <- TwentyYearNullandTrue2 %>%
  subset(Pred.Strat == "Active")

# Small/Small
Active10SS <- Active10 %>%
  subset(Pred.Prey_Domain == "Small.Small")
ActiveTwentyYearNCE <-
  coxph(Surv(year, status) ~ propHabitat + propPredFree + propSafeSpace,
        data = Active10SS)
#summary(ActiveTwentyYearNCE) # Sig - not the same
predict(ActiveTwentyYearNCE)
exp(confint(ActiveTwentyYearNCE))
haz.table <- ActiveTwentyYearNCE %>%
  gtsummary::tbl_regression(exp = TRUE) 

# Small/Large
Active10SL <- Active10 %>%
  subset(Pred.Prey_Domain == "Small.Large")
ActiveTwentyYearNCE <-
  coxph(Surv(year, status) ~ propHabitat + propPredFree + propSafeSpace,
        data = Active10SL)
summary(ActiveTwentyYearNCE) #no sig/ same
exp(confint(ActiveTwentyYearNCE))
haz.table <- ActiveTwentyYearNCE %>%
  gtsummary::tbl_regression(exp = TRUE) 

# Large/Small
Active10LS <- Active10 %>%
  subset(Pred.Prey_Domain == "Large.Small")
ActiveTwentyYearNCE <-
  coxph(Surv(year, status) ~ propHabitat + propPredFree + propSafeSpace,
        data = Active10LS)
exp(confint(ActiveTwentyYearNCE))
haz.table <- ActiveTwentyYearNCE %>%
  gtsummary::tbl_regression(exp = TRUE) 


# Large/Large
Active10LL <- Active10 %>%
  subset(Pred.Prey_Domain == "Large.Large")
ActiveTwentyYearNCE <-
  coxph(Surv(year, status) ~ propHabitat + propPredFree + propSafeSpace,
        data = Active10LL)
exp(confint(ActiveTwentyYearNCE))
haz.table <- ActiveTwentyYearNCE %>%
  gtsummary::tbl_regression(exp = TRUE) 



## Sit-and-Pursue predators
SP10 <- TwentyYearTrue %>%
  subset(Pred.Strat == "Sit-and-Pursue")

# Small/Small
SP10SS <- SP10 %>%
  subset(Pred.Prey_Domain == "Small.Small")
SPTwentyYearNCE <-
  coxph(Surv(year, status) ~ propHabitat + propPredFree + propSafeSpace,
        data = SP10SS)
exp(confint(SPTwentyYearNCE))
haz.table <- SPTwentyYearNCE %>%
  gtsummary::tbl_regression(exp = TRUE) 

# Small/Large
SP10SL <- SP10 %>%
  subset(Pred.Prey_Domain == "Small.Large")
SPTwentyYearNCE <-
  coxph(Surv(year, status) ~ propHabitat + propPredFree + propSafeSpace,
        data = SP10SL)
exp(confint(SPTwentyYearNCE))
haz.table <- SPTwentyYearNCE %>%
  gtsummary::tbl_regression(exp = TRUE) 

# Large/Small
SP10LS <- SP10 %>%
  subset(Pred.Prey_Domain == "Large.Small")
SPTwentyYearNCE <-
  coxph(Surv(year, status) ~ propHabitat + propPredFree + propSafeSpace,
        data = SP10LS)
exp(confint(SPTwentyYearNCE))
haz.table <- SPTwentyYearNCE %>%
  gtsummary::tbl_regression(exp = TRUE) 

# Large/Large
SP10LL <- SP10 %>%
  subset(Pred.Prey_Domain == "Large.Large")
SPTwentyYearNCE <-
  coxph(Surv(year, status) ~ propHabitat + propPredFree + propSafeSpace,
        data = SP10LL)
exp(confint(SPTwentyYearNCE)) # get HR 910% CI
haz.table <- SPTwentyYearNCE %>%
  gtsummary::tbl_regression(exp = TRUE) 


## Sit-and-Wait predators
SW10 <- TwentyYearTrue %>%
  subset(Pred.Strat == "Sit-and-Wait")

# Small/Small
SW10SS <- SW10 %>%
  subset(Pred.Prey_Domain == "Small.Small")
SWTwentyYearNCE <-
  coxph(Surv(year, status) ~ propHabitat + propPredFree + propSafeSpace,
        data = SW10SS)
exp(confint(SWTwentyYearNCE)) # get HR 910% CI
haz.table <- SWTwentyYearNCE %>%
  gtsummary::tbl_regression(exp = TRUE) 

# Small/Large
SW10SL <- SW10 %>%
  subset(Pred.Prey_Domain == "Small.Large")
SWTwentyYearNCE <-
  coxph(Surv(year, status) ~ propHabitat + propPredFree + propSafeSpace,
        data = SW10SL)
exp(confint(SWTwentyYearNCE)) # get HR 910% CI
haz.table <- SWTwentyYearNCE %>%
  gtsummary::tbl_regression(exp = TRUE) 

# Large/Small
SW10LS <- SW10 %>%
  subset(Pred.Prey_Domain == "Large.Small")
SWTwentyYearNCE <-
  coxph(Surv(year, status) ~ propHabitat + propPredFree + propSafeSpace,
        data = SP10LS)
exp(confint(SWTwentyYearNCE)) # get HR 910% CI
haz.table <- SWTwentyYearNCE %>%
  gtsummary::tbl_regression(exp = TRUE) 

# Large/Large
SW10LL <- SW10 %>%
  subset(Pred.Prey_Domain == "Large.Large")
SWTwentyYearNCE <-
  coxph(Surv(year, status) ~ propHabitat + propPredFree + propSafeSpace,
        data = SW10LL)
exp(confint(SWTwentyYearNCE)) # get HR 910% CI
haz.table <- SWTwentyYearNCE %>%
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
                    alpha = CE_or_NCE,
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
  scale_alpha_discrete( name = "Dominant Effect") +
  
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

ggsave(HR_plot, filename = "Output_Figures/HazardRatiosPlot_CEvsNCE.png", dpi = 300, width = 8, height = 5)









## make time plots for yr = 0-1 and yr = 0-5 to look at shifts ----



#### Twentyyear data behavioral shifts

# reorder
twentyyr_new <- TwentyYearTrue
twentyyr_new$Pred.Start.Con <- factor(twentyyr_new$Pred.Start.Con, 
                                   levels = c("Small", "Large"))
twentyyr_new$Prey.Start.Con <- factor(twentyyr_new$Prey.Start.Con, 
                                   levels = c("Small", "Large"))

# create new labels for the facets
new_labels <- c("Small" = "Predator Small Domain", "Large" = "Predator Large Domain")
new_labels2 <- c("Small" = "Prey Small Domain", "Large" = "Prey Large Domain")

## melt the dataframe for extra column for shift
# add unique row id
twentyyr_new$index <- 1:nrow(twentyyr_new)
head(twentyyr_new)
molted <- melt(value.name = "BehaviorShift", twentyyr_new[,c(65:67, 72)],id.vars=c("index"))
molted

twentyyrshifts <- left_join(molted, twentyyr_new[,c(2:4,69:70, 72)], by = "index")
summary(twentyyrshifts)

# reorder behavior shifts
twentyyrshifts$variable <- factor(twentyyrshifts$variable, 
                               levels = c("propHabitat", "propPredFree", "propSafeSpace"))

twentyyrshifts$BehaviorShift <- ifelse(twentyyrshifts$BehaviorShift == 0, "NA", twentyyrshifts$BehaviorShift)
twentyyrshifts$BehaviorShift <- as.numeric(as.character(twentyyrshifts$BehaviorShift))
summary(twentyyrshifts)


## removing Consumptive effect
twentyyrshifts1<- twentyyrshifts %>%
  mutate(CE_or_NCE = case_when(Pred.Strat == "Active" & Pred.Prey_Domain =="Small.Small" ~ "CE",
                               Pred.Strat == "Active" & Pred.Prey_Domain == "Large.Small" ~ "CE",
                               Pred.Strat == "Sit-and-Pursue" & Pred.Prey_Domain == "Large.Large" ~ "CE",
                               Pred.Strat == "Sit-and-Wait" & Pred.Prey_Domain == "Large.Large" ~ "CE")) %>%
  mutate(CE_or_NCE = case_when(is.na(CE_or_NCE) == TRUE ~"NCE",
                               is.na(CE_or_NCE) != TRUE ~"CE"))
#twentyyrshifts1<-twentyyrshifts1 %>%
#  subset(CE_or_NCE == "NCE")

## Plot highlighting consumptive vs non consumptive effects

twentyyrshift_plot <- ggplot(twentyyrshifts1,
                          aes(
                            x = variable,
                            y = BehaviorShift,
                            color = CE_or_NCE,
                            fill = Pred.Strat,
                            group = Pred.Strat,
                            #slab_alpha = as.factor(CE_or_NCE)
                          )) +
  geom_hline(yintercept = 0.5, linetype = "dotted") +
  ggdist::stat_halfeye(
    adjust = 1,
    normalize = "groups",
    position = position_dodge(width = 0.5),
    ## set slab interval to show IQR and 95% data range
    .width = c(.5, .95),
    aes(fill = Pred.Strat, slab_alpha = CE_or_NCE)
  ) +
  # scale_alpha_discrete(range = c(0.35, 0.9)) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  ylim(0, 1) +
  scale_fill_viridis_d(begin = 0., end = 0.9, name = "Hunting Mode") +
  scale_color_manual(values=c("#9d9d9d", "#000000")) +
  scale_slab_alpha_discrete(range = c(.3, 0.75), guide = "none") +
  #scale_alpha_manual(values=c(0.7, 0.7))+
  #scale_slab_alpha_discrete(values=c(0.1, 0.7)) +
  
  ylab("Prey Proportional Shift to Safety") +
  xlab("Type of Shift") +
  labs(color = "Dominant Effect\n") +
  scale_x_discrete(labels=c("propHabitat" = "Habitat", "propPredFree" = "Time", 
                            "propSafeSpace" = "Space")) +
  facet_grid(Pred.Start.Con ~ Prey.Start.Con,
             labeller = labeller(Pred.Start.Con = new_labels,
                                 Prey.Start.Con = new_labels2)) +
  ggtitle("Shifts at twenty years")


print(twentyyrshift_plot)
ggsave(twentyyrshift_plot, filename = "Output_Figures/TwentyYrShifts_NCEvsCE.png", dpi = 300, width = 8, height = 5)


###


### Plot of behavioral shifts
twentyyrshift_plot <- ggplot(twentyyrshifts1,
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
  ggtitle("Shifts at twenty years")


print(twentyyrshift_plot)

ggsave(twentyyrshift_plot, filename = "Output_Figures/TwentyYrShifts.png", dpi = 300, width = 8, height = 5)



#################### Prepping separate hunting modes and looking at survival plots across all of the data

## overall models
## all
TwentyYearNullandTrue <- TwentyYearNullandTrue %>%
  mutate(Pred.Prey_Domain =interaction(Pred.Start.Con, Prey.Start.Con))

TwentyYearNullandTrue_surv <- TwentyYearNullandTrue %>%
  mutate(status = ifelse (ticks == 175200, 0, 1)) %>%
  mutate(year = ticks/365/24)

allmod <- survfit(Surv(year, status) ~ ModelType, data = TwentyYearNullandTrue_surv)



## to get colors to match the other plot, I am using viridisLite to get the hex codes
# > viridis(3, alpha = 1, begin = 0, end = 0.9, direction = 1, option = "D")
# [1] "#440154FF" "#25848EFF" "#BBDF27FF"




####### Comparing consumptive vs. non-consumptive effects ----

# > viridis(11, alpha = 1, begin = 0, end = 1, direction = 1, option = "D")
# [1] "#440154FF" "#482576FF" "#414487FF" "#35608DFF" "#2A788EFF" "#21908CFF" "#22A884FF"
# [8] "#43BF71FF" "#7AD151FF" "#BBDF27FF" "#FDE725FF"

## Active survival models ----



TwentyYearNullandTrue.A <- TwentyYearNullandTrue %>%
  subset(Pred.Strat == "Active") %>%
  mutate(Pred.Prey_Domain =interaction(Pred.Start.Con, Prey.Start.Con))

TwentyYearNullandTrue.A_surv <- TwentyYearNullandTrue.A %>%
  mutate(status = ifelse (ticks == 175200, 0, 1)) %>%
  mutate(year = ticks/365/24)

# reorder

TwentyYearNullandTrue.A_surv$Pred.Start.Con <- factor(TwentyYearNullandTrue.A_surv$Pred.Start.Con, 
                                                   levels = c("Small", "Large"))
TwentyYearNullandTrue.A_surv$Prey.Start.Con <- factor(TwentyYearNullandTrue.A_surv$Prey.Start.Con, 
                                                   levels = c("Small", "Large"))



## Facet plot for active predators
?ggsurvplot_facet
fitA <- survfit( Surv(year, status) ~ ModelType, data = TwentyYearNullandTrue.A_surv)
ActiveMulti <- ggsurvplot_facet(fitA, 
                                TwentyYearNullandTrue.A_surv, 
                                conf.int = TRUE,
                                facet.by = c("Pred.Start.Con", "Prey.Start.Con"),
                                palette = c("gray", "#440154FF"),
                                surv.median.line = "v", # add median survival
                                pval = TRUE,
                                pval.coord = c(3.5, 0.8),
                                ggtheme = theme_bw(base_size = 16),
                                short.panel.labs = TRUE,
                                scales = "free",
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

print(ActiveMulti)
ggsave(plot = ActiveMulti, "Output_Figures/TwentyYr_ActivePredSurv.png", dpi = 300, height = 6, width = 7)





## Sit-and-wait survival models ----


TwentyYearNullandTrue.SW <- TwentyYearNullandTrue %>%
  subset(Pred.Strat == "Sit-and-Wait") %>%
  mutate(Pred.Prey_Domain =interaction(Pred.Start.Con, Prey.Start.Con))

TwentyYearNullandTrue.SW_surv <- TwentyYearNullandTrue.SW %>%
  mutate(status = ifelse (ticks == 175200, 0, 1)) %>%
  mutate(year = ticks/365/24)

# reorder

TwentyYearNullandTrue.SW_surv$Pred.Start.Con <- factor(TwentyYearNullandTrue.SW_surv$Pred.Start.Con, 
                                                    levels = c("Small", "Large"))
TwentyYearNullandTrue.SW_surv$Prey.Start.Con <- factor(TwentyYearNullandTrue.SW_surv$Prey.Start.Con, 
                                                    levels = c("Small", "Large"))


# SW facet plot
fitSW <- survfit( Surv(year, status) ~ ModelType, data = TwentyYearNullandTrue.SW_surv)
SWMulti <- ggsurvplot_facet(fitSW, 
                            TwentyYearNullandTrue.SW_surv, 
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

print(SWMulti)
ggsave(SWMulti, filename = "Output_Figures/TwentyYr_SWPredSurv.png", dpi = 300, height = 6, width = 7)

# SW small/small
SW_SS <- TwentyYearNullandTrue.SW_surv %>%
  subset(Pred.Prey_Domain == "Small.Small")
# Get regression median and p-value of Null vs. NCE
SWSSmod <- survfit(Surv(year, status) ~ ModelType, data = SW_SS)
SWSSmod

# SW small/large
SW_SL <- TwentyYearNullandTrue.SW_surv %>%
  subset(Pred.Prey_Domain == "Small.Large")
# Get regression median and p-value of Null vs. NCE
SWSLmod <- survfit(Surv(year, status) ~ ModelType, data = SW_SL)
SWSLmod

# SW large/small
SW_LS <- TwentyYearNullandTrue.SW_surv %>%
  subset(Pred.Prey_Domain == "Large.Small")
# Get regression median and p-value of Null vs. NCE
SWLSmod <- survfit(Surv(year, status) ~ ModelType, data = SW_LS)
SWLSmod

# SW large/large
SW_LL <- TwentyYearNullandTrue.SW_surv %>%
  subset(Pred.Prey_Domain == "Large.Large")
# Get regression median and p-value of Null vs. NCE
SWLLmod <- survfit(Surv(year, status) ~ ModelType, data = SW_LL)
SWLLmod




## Sit-and-Pursue survival models ----


TwentyYearNullandTrue.SP <- TwentyYearNullandTrue %>%
  subset(Pred.Strat == "Sit-and-Pursue") %>%
  mutate(Pred.Prey_Domain =interaction(Pred.Start.Con, Prey.Start.Con))

TwentyYearNullandTrue.SP_surv <- TwentyYearNullandTrue.SP %>%
  mutate(status = ifelse (ticks == 175200, 0, 1)) %>%
  mutate(year = ticks/365/24)

# reorder
TwentyYearNullandTrue.SP_surv$Pred.Start.Con <- factor(TwentyYearNullandTrue.SP_surv$Pred.Start.Con, 
                                                    levels = c("Small", "Large"))
TwentyYearNullandTrue.SP_surv$Prey.Start.Con <- factor(TwentyYearNullandTrue.SP_surv$Prey.Start.Con, 
                                                    levels = c("Small", "Large"))

# SP facet plot
fitSP <- survfit( Surv(year, status) ~ ModelType, data = TwentyYearNullandTrue.SP_surv)
SPMulti <- ggsurvplot_facet(fitSP, 
                            TwentyYearNullandTrue.SP_surv, 
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
ggsave("Output_Figures/TwentyYr_SPPredSurv.png", dpi = 300, height = 6, width = 7)

# SP small/small
SP_SS <- TwentyYearNullandTrue.SP_surv %>%
  subset(Pred.Prey_Domain == "Small.Small")
# Get regression median and p-value of Null vs. NCE
SPSSmod <- survfit(Surv(year, status) ~ ModelType, data = SP_SS)
SPSSmod

# SP small/large
SP_SL <- TwentyYearNullandTrue.SP_surv %>%
  subset(Pred.Prey_Domain == "Small.Large")
# Get regression median and p-value of Null vs. NCE
SPSLmod <- survfit(Surv(year, status) ~ ModelType, data = SP_SL)
SPSLmod

# SP large/small
SP_LS <- TwentyYearNullandTrue.SP_surv %>%
  subset(Pred.Prey_Domain == "Large.Small")
# Get regression median and p-value of Null vs. NCE
SPLSmod <- survfit(Surv(year, status) ~ ModelType, data = SP_LS)
SPLSmod

# SP large/large
SP_LL <- TwentyYearNullandTrue.SP_surv %>%
  subset(Pred.Prey_Domain == "Large.Large")
# Get regression median and p-value of Null vs. NCE
SPLLmod <- survfit(Surv(year, status) ~ ModelType, data = SP_LL)
SPLLmod



