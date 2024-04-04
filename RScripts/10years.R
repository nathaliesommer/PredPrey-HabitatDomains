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


tenyr <- read.csv("Data/Prepped_data/NCvsC_10year_TSH_Sept252023.csv")%>%
  mutate(ModelType = 1)

tenyr_null <- read.csv("Data/Prepped_data/NCvsC_NULL_10year_TSH_Sept252023.csv")  %>%
  mutate(ModelType = 0)



# make a dataframe with all 10 year data

###### 10 year data for models ----

# bind CE only and CE + NCE model results
TenYearNullandTrue <- rbind(tenyr, tenyr_null)

# calculate prop safe space, prop habitat, and prop time shifts
TenYearNullandTrue <- TenYearNullandTrue  %>%
  mutate(propHabitat = White.Table/(Black.Table + White.Table)) %>%
  mutate(propSafeSpace = Domain.Prey/(Domain.Prey + DomainOverlap)) %>%
  mutate(propPredFree = rowSums(.[28:39])/rowSums(.[16:39]))%>%
  mutate(interactions = rowSums(.[41:63])) %>%
  mutate(Pred.Prey_Domain =interaction(Pred.Start.Con, Prey.Start.Con))

# make a dataframe and convert ticks to 'years' and set alive vs. dead status
# prepping the data for the 'survivor' package - need a column called status which is binary
# to determine if the 'event' (dying) occurred or not
TenYearNullandTrue2 <- TenYearNullandTrue %>%
  mutate(status = ifelse (ticks == 87600, 0, 1)) %>%
  mutate(year = ticks/365/24)

# make sure starting conditions are factors
TenYearNullandTrue2$Pred.Strat <- as.factor(TenYearNullandTrue2$Pred.Strat)
TenYearNullandTrue2$Pred.Start.Con <- as.factor(TenYearNullandTrue2$Pred.Start.Con)
TenYearNullandTrue2$Prey.Start.Con <- as.factor(TenYearNullandTrue2$Prey.Start.Con)




#######################SURIVAL PACKAGE ----


# CE + NCE effects
TenYearTrue <-
  TenYearNullandTrue2 %>%
  subset(ModelType == 1)

# only CE
TenYearCE <-
  TenYearNullandTrue2 %>%
  subset(ModelType == 0)


## Make Hazard Ratio tables ----
TenYearNCE <-
  coxph(Surv(year, status) ~ propHabitat + propPredFree + propSafeSpace,
        data = TenYearTrue)
haz.table <- TenYearNCE %>%
  gtsummary::tbl_regression(exp = TRUE) 


# behavioral changes for CE only
TenYearCEmod <-
  coxph(Surv(year, status) ~ Pred.Strat + Pred.Start.Con*Prey.Start.Con,
        data = TenYearCE)
haz.table <- TenYearCEmod %>%
  gtsummary::tbl_regression(exp = TRUE) 


### KO SKIPPED HERE
## Look at hazard ratio of behavioral changes by predator hunting mode

## Active predators
Active10 <- TenYearNullandTrue2 %>%
  subset(Pred.Strat == "Active")

# Small/Small
Active10SS <- Active10 %>%
  subset(Pred.Prey_Domain == "Small.Small")
ActiveTenYearNCE <-
  coxph(Surv(year, status) ~ propHabitat + propPredFree + propSafeSpace,
        data = Active10SS)
#summary(ActiveTenYearNCE) # Sig - not the same
predict(ActiveTenYearNCE)
exp(confint(ActiveTenYearNCE))
haz.table <- ActiveTenYearNCE %>%
  gtsummary::tbl_regression(exp = TRUE) 

# Small/Large
Active10SL <- Active10 %>%
  subset(Pred.Prey_Domain == "Small.Large")
ActiveTenYearNCE <-
  coxph(Surv(year, status) ~ propHabitat + propPredFree + propSafeSpace,
        data = Active10SL)
summary(ActiveTenYearNCE) #no sig/ same
exp(confint(ActiveTenYearNCE))
haz.table <- ActiveTenYearNCE %>%
  gtsummary::tbl_regression(exp = TRUE) 

# Large/Small
Active10LS <- Active10 %>%
  subset(Pred.Prey_Domain == "Large.Small")
ActiveTenYearNCE <-
  coxph(Surv(year, status) ~ propHabitat + propPredFree + propSafeSpace,
        data = Active10LS)
exp(confint(ActiveTenYearNCE))
haz.table <- ActiveTenYearNCE %>%
  gtsummary::tbl_regression(exp = TRUE) 


# Large/Large
Active10LL <- Active10 %>%
  subset(Pred.Prey_Domain == "Large.Large")
ActiveTenYearNCE <-
  coxph(Surv(year, status) ~ propHabitat + propPredFree + propSafeSpace,
        data = Active10LL)
exp(confint(ActiveTenYearNCE))
haz.table <- ActiveTenYearNCE %>%
  gtsummary::tbl_regression(exp = TRUE) 



## Sit-and-Pursue predators
SP10 <- TenYearTrue %>%
  subset(Pred.Strat == "Sit-and-Pursue")

# Small/Small
SP10SS <- SP10 %>%
  subset(Pred.Prey_Domain == "Small.Small")
SPTenYearNCE <-
  coxph(Surv(year, status) ~ propHabitat + propPredFree + propSafeSpace,
        data = SP10SS)
exp(confint(SPTenYearNCE))
haz.table <- SPTenYearNCE %>%
  gtsummary::tbl_regression(exp = TRUE) 

# Small/Large
SP10SL <- SP10 %>%
  subset(Pred.Prey_Domain == "Small.Large")
SPTenYearNCE <-
  coxph(Surv(year, status) ~ propHabitat + propPredFree + propSafeSpace,
        data = SP10SL)
exp(confint(SPTenYearNCE))
haz.table <- SPTenYearNCE %>%
  gtsummary::tbl_regression(exp = TRUE) 

# Large/Small
SP10LS <- SP10 %>%
  subset(Pred.Prey_Domain == "Large.Small")
SPTenYearNCE <-
  coxph(Surv(year, status) ~ propHabitat + propPredFree + propSafeSpace,
        data = SP10LS)
exp(confint(SPTenYearNCE))
haz.table <- SPTenYearNCE %>%
  gtsummary::tbl_regression(exp = TRUE) 

# Large/Large
SP10LL <- SP10 %>%
  subset(Pred.Prey_Domain == "Large.Large")
SPTenYearNCE <-
  coxph(Surv(year, status) ~ propHabitat + propPredFree + propSafeSpace,
        data = SP10LL)
exp(confint(SPTenYearNCE)) # get HR 910% CI
haz.table <- SPTenYearNCE %>%
  gtsummary::tbl_regression(exp = TRUE) 


## Sit-and-Wait predators
SW10 <- TenYearTrue %>%
  subset(Pred.Strat == "Sit-and-Wait")

# Small/Small
SW10SS <- SW10 %>%
  subset(Pred.Prey_Domain == "Small.Small")
SWTenYearNCE <-
  coxph(Surv(year, status) ~ propHabitat + propPredFree + propSafeSpace,
        data = SW10SS)
exp(confint(SWTenYearNCE)) # get HR 910% CI
haz.table <- SWTenYearNCE %>%
  gtsummary::tbl_regression(exp = TRUE) 

# Small/Large
SW10SL <- SW10 %>%
  subset(Pred.Prey_Domain == "Small.Large")
SWTenYearNCE <-
  coxph(Surv(year, status) ~ propHabitat + propPredFree + propSafeSpace,
        data = SW10SL)
exp(confint(SWTenYearNCE)) # get HR 910% CI
haz.table <- SWTenYearNCE %>%
  gtsummary::tbl_regression(exp = TRUE) 

# Large/Small
SW10LS <- SW10 %>%
  subset(Pred.Prey_Domain == "Large.Small")
SWTenYearNCE <-
  coxph(Surv(year, status) ~ propHabitat + propPredFree + propSafeSpace,
        data = SP10LS)
exp(confint(SWTenYearNCE)) # get HR 910% CI
haz.table <- SWTenYearNCE %>%
  gtsummary::tbl_regression(exp = TRUE) 

# Large/Large
SW10LL <- SW10 %>%
  subset(Pred.Prey_Domain == "Large.Large")
SWTenYearNCE <-
  coxph(Surv(year, status) ~ propHabitat + propPredFree + propSafeSpace,
        data = SW10LL)
exp(confint(SWTenYearNCE)) # get HR 910% CI
haz.table <- SWTenYearNCE %>%
  gtsummary::tbl_regression(exp = TRUE) 




#### Tenyear data behavioral shifts

# reorder
tenyr_new <- TenYearTrue
tenyr_new$Pred.Start.Con <- factor(tenyr_new$Pred.Start.Con, 
                                    levels = c("Small", "Large"))
tenyr_new$Prey.Start.Con <- factor(tenyr_new$Prey.Start.Con, 
                                    levels = c("Small", "Large"))

# create new labels for the facets
new_labels <- c("Small" = "Predator Small Domain", "Large" = "Predator Large Domain")
new_labels2 <- c("Small" = "Prey Small Domain", "Large" = "Prey Large Domain")

## melt the dataframe for extra column for shift
# add unique row id
tenyr_new$index <- 1:nrow(tenyr_new)
head(tenyr_new)
molted <- melt(value.name = "BehaviorShift", tenyr_new[,c(65:67, 72)],id.vars=c("index"))
molted

tenyrshifts <- left_join(molted, tenyr_new[,c(2:4,69:70, 72)], by = "index")
summary(tenyrshifts)

# reorder behavior shifts
tenyrshifts$variable <- factor(tenyrshifts$variable, 
                                levels = c("propHabitat", "propPredFree", "propSafeSpace"))

tenyrshifts$BehaviorShift <- ifelse(tenyrshifts$BehaviorShift == 0, "NA", tenyrshifts$BehaviorShift)
tenyrshifts$BehaviorShift <- as.numeric(as.character(tenyrshifts$BehaviorShift))
summary(tenyrshifts)


## removing Consumptive effect
tenyrshifts1<- tenyrshifts %>%
  mutate(CE_or_NCE = case_when(Pred.Strat == "Active" & Pred.Prey_Domain =="Small.Small" ~ "CE",
                               Pred.Strat == "Active" & Pred.Prey_Domain == "Large.Small" ~ "CE",
                               Pred.Strat == "Sit-and-Pursue" & Pred.Prey_Domain == "Large.Large" ~ "CE",
                               Pred.Strat == "Sit-and-Wait" & Pred.Prey_Domain == "Large.Large" ~ "CE")) %>%
  mutate(CE_or_NCE = case_when(is.na(CE_or_NCE) == TRUE ~"NCE",
                               is.na(CE_or_NCE) != TRUE ~"CE"))
#tenyrshifts1<-tenyrshifts1 %>%
#  subset(CE_or_NCE == "NCE")

## Plot highlighting consumptive vs non consumptive effects

tenyrshift_plot <- ggplot(tenyrshifts1,
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
  ggtitle("Shifts at ten years")


print(tenyrshift_plot)
ggsave(tenyrshift_plot, filename = "Output_Figures/TenYrShifts_NCEvsCE.png", dpi = 300, width = 8, height = 5)


###


### Plot of behavioral shifts
tenyrshift_plot <- ggplot(tenyrshifts1,
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
  ggtitle("Shifts at ten years")


print(tenyrshift_plot)

ggsave(tenyrshift_plot, filename = "Output_Figures/TenYrShifts.png", dpi = 300, width = 8, height = 5)



#################### Prepping separate hunting modes and looking at survival plots across all of the data

## overall models
## all
TenYearNullandTrue <- TenYearNullandTrue %>%
  mutate(Pred.Prey_Domain =interaction(Pred.Start.Con, Prey.Start.Con))

TenYearNullandTrue_surv <- TenYearNullandTrue %>%
  mutate(status = ifelse (ticks == 87600, 0, 1)) %>%
  mutate(year = ticks/365/24)

allmod <- survfit(Surv(year, status) ~ ModelType, data = TenYearNullandTrue_surv)



## to get colors to match the other plot, I am using viridisLite to get the hex codes
# > viridis(3, alpha = 1, begin = 0, end = 0.9, direction = 1, option = "D")
# [1] "#440154FF" "#25848EFF" "#BBDF27FF"




####### Comparing consumptive vs. non-consumptive effects ----

# > viridis(11, alpha = 1, begin = 0, end = 1, direction = 1, option = "D")
# [1] "#440154FF" "#482576FF" "#414487FF" "#35608DFF" "#2A788EFF" "#21908CFF" "#22A884FF"
# [8] "#43BF71FF" "#7AD151FF" "#BBDF27FF" "#FDE725FF"

## Active survival models ----



TenYearNullandTrue.A <- TenYearNullandTrue %>%
  subset(Pred.Strat == "Active") %>%
  mutate(Pred.Prey_Domain =interaction(Pred.Start.Con, Prey.Start.Con))

TenYearNullandTrue.A_surv <- TenYearNullandTrue.A %>%
  mutate(status = ifelse (ticks == 87600, 0, 1)) %>%
  mutate(year = ticks/365/24)

# reorder

TenYearNullandTrue.A_surv$Pred.Start.Con <- factor(TenYearNullandTrue.A_surv$Pred.Start.Con, 
                                                    levels = c("Small", "Large"))
TenYearNullandTrue.A_surv$Prey.Start.Con <- factor(TenYearNullandTrue.A_surv$Prey.Start.Con, 
                                                    levels = c("Small", "Large"))



## Facet plot for active predators
?ggsurvplot_facet
fitA <- survfit( Surv(year, status) ~ ModelType, data = TenYearNullandTrue.A_surv)
ActiveMulti <- ggsurvplot_facet(fitA, 
                                TenYearNullandTrue.A_surv, 
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
ggsave(plot = ActiveMulti, "Output_Figures/TenYr_ActivePredSurv.png", dpi = 300, height = 6, width = 7)





## Sit-and-wait survival models ----


TenYearNullandTrue.SW <- TenYearNullandTrue %>%
  subset(Pred.Strat == "Sit-and-Wait") %>%
  mutate(Pred.Prey_Domain =interaction(Pred.Start.Con, Prey.Start.Con))

TenYearNullandTrue.SW_surv <- TenYearNullandTrue.SW %>%
  mutate(status = ifelse (ticks == 87600, 0, 1)) %>%
  mutate(year = ticks/365/24)

# reorder

TenYearNullandTrue.SW_surv$Pred.Start.Con <- factor(TenYearNullandTrue.SW_surv$Pred.Start.Con, 
                                                     levels = c("Small", "Large"))
TenYearNullandTrue.SW_surv$Prey.Start.Con <- factor(TenYearNullandTrue.SW_surv$Prey.Start.Con, 
                                                     levels = c("Small", "Large"))


# SW facet plot
fitSW <- survfit( Surv(year, status) ~ ModelType, data = TenYearNullandTrue.SW_surv)
SWMulti <- ggsurvplot_facet(fitSW, 
                            TenYearNullandTrue.SW_surv, 
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
ggsave(SWMulti, filename = "Output_Figures/TenYr_SWPredSurv.png", dpi = 300, height = 6, width = 7)

# SW small/small
SW_SS <- TenYearNullandTrue.SW_surv %>%
  subset(Pred.Prey_Domain == "Small.Small")
# Get regression median and p-value of Null vs. NCE
SWSSmod <- survfit(Surv(year, status) ~ ModelType, data = SW_SS)
SWSSmod

# SW small/large
SW_SL <- TenYearNullandTrue.SW_surv %>%
  subset(Pred.Prey_Domain == "Small.Large")
# Get regression median and p-value of Null vs. NCE
SWSLmod <- survfit(Surv(year, status) ~ ModelType, data = SW_SL)
SWSLmod

# SW large/small
SW_LS <- TenYearNullandTrue.SW_surv %>%
  subset(Pred.Prey_Domain == "Large.Small")
# Get regression median and p-value of Null vs. NCE
SWLSmod <- survfit(Surv(year, status) ~ ModelType, data = SW_LS)
SWLSmod

# SW large/large
SW_LL <- TenYearNullandTrue.SW_surv %>%
  subset(Pred.Prey_Domain == "Large.Large")
# Get regression median and p-value of Null vs. NCE
SWLLmod <- survfit(Surv(year, status) ~ ModelType, data = SW_LL)
SWLLmod




## Sit-and-Pursue survival models ----


TenYearNullandTrue.SP <- TenYearNullandTrue %>%
  subset(Pred.Strat == "Sit-and-Pursue") %>%
  mutate(Pred.Prey_Domain =interaction(Pred.Start.Con, Prey.Start.Con))

TenYearNullandTrue.SP_surv <- TenYearNullandTrue.SP %>%
  mutate(status = ifelse (ticks == 87600, 0, 1)) %>%
  mutate(year = ticks/365/24)

# reorder
TenYearNullandTrue.SP_surv$Pred.Start.Con <- factor(TenYearNullandTrue.SP_surv$Pred.Start.Con, 
                                                     levels = c("Small", "Large"))
TenYearNullandTrue.SP_surv$Prey.Start.Con <- factor(TenYearNullandTrue.SP_surv$Prey.Start.Con, 
                                                     levels = c("Small", "Large"))

# SP facet plot
fitSP <- survfit( Surv(year, status) ~ ModelType, data = TenYearNullandTrue.SP_surv)
SPMulti <- ggsurvplot_facet(fitSP, 
                            TenYearNullandTrue.SP_surv, 
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
ggsave("Output_Figures/TenYr_SPPredSurv.png", dpi = 300, height = 6, width = 7)

# SP small/small
SP_SS <- TenYearNullandTrue.SP_surv %>%
  subset(Pred.Prey_Domain == "Small.Small")
# Get regression median and p-value of Null vs. NCE
SPSSmod <- survfit(Surv(year, status) ~ ModelType, data = SP_SS)
SPSSmod

# SP small/large
SP_SL <- TenYearNullandTrue.SP_surv %>%
  subset(Pred.Prey_Domain == "Small.Large")
# Get regression median and p-value of Null vs. NCE
SPSLmod <- survfit(Surv(year, status) ~ ModelType, data = SP_SL)
SPSLmod

# SP large/small
SP_LS <- TenYearNullandTrue.SP_surv %>%
  subset(Pred.Prey_Domain == "Large.Small")
# Get regression median and p-value of Null vs. NCE
SPLSmod <- survfit(Surv(year, status) ~ ModelType, data = SP_LS)
SPLSmod

# SP large/large
SP_LL <- TenYearNullandTrue.SP_surv %>%
  subset(Pred.Prey_Domain == "Large.Large")
# Get regression median and p-value of Null vs. NCE
SPLLmod <- survfit(Surv(year, status) ~ ModelType, data = SP_LL)
SPLLmod



