# Script for survival analyses

# Adapted from KDO script Jan 2022
# KDO and FER
# Last updated April 2022 by FER


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
              "ggridges",
              "lme4",
              "rstan",
              "rstanarm",
              "rgl",
              "shinystan",
              "bayesplot",
              "loo",
              "bayestestR",
              "ggplot2",
              "tidybayes",
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
# 
# oneyr_null <- read.csv("Data/NCvsC_Nullyear_TSH_Nov19.csv") %>%
#   mutate(ModelType = 0)

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

###### Just looking at 5 years for now

# load data ----


FiveYearNullandTrue <- rbind(fiveyr, fiveyr_null)

FiveYearNullandTrue <- FiveYearNullandTrue  %>%
  mutate(propHabitat = White.Table/(Black.Table + White.Table)) %>%
  mutate(propSafeSpace = Domain.Prey/(Domain.Prey + DomainOverlap)) %>%
  mutate(propPredFree = rowSums(.[28:39])/rowSums(.[16:39]))%>%
  mutate(interactions = rowSums(.[41:63])) %>%
  mutate(Pred.Prey_Domain =interaction(Pred.Start.Con, Prey.Start.Con))

# make a dataframe with all 5 year data
FiveYearNullandTrue2 <- FiveYearNullandTrue %>%
  mutate(status = ifelse (ticks == 43800, 0, 1)) %>%
  mutate(year = ticks/365/24)

FiveYearNullandTrue2$Pred.Strat <- as.factor(FiveYearNullandTrue2$Pred.Strat)
FiveYearNullandTrue2$Pred.Start.Con <- as.factor(FiveYearNullandTrue2$Pred.Start.Con)
FiveYearNullandTrue2$Prey.Start.Con <- as.factor(FiveYearNullandTrue2$Prey.Start.Con)


### Need to look at each predator hunting type, and each habitat domain size combination separately, so first separate Active.Large.Large

active5.Large.Large <-  subset(FiveYearNullandTrue, Pred.Strat == "Active") %>%
  mutate(Pred.Prey_Domain =interaction(Pred.Start.Con, Prey.Start.Con)) %>%
  subset(Pred.Prey_Domain == "Large.Large")

#######################SURIVAL PACKAGE
## prepping the data for the 'survivor' package - need a column called status which is binary
#to determine if the 'event' (dying) occurred or not
FiveYearNullandTrue_surv <-   active5.Large.Large %>%
  mutate(status = ifelse (ticks == 43800, 0, 1)) %>%
  mutate(year = ticks/365/24)

#create the survival obejct:
survobj<- with(FiveYearNullandTrue_surv, Surv(year, status))

# Looking at the data (both null and NCE model together)
# fit0 <- survfit(survobj~1, data=FiveYearNullandTrue_surv)
# summary(fit0)
# plot(fit0, xlab="Survival Time in Years", 
#      ylab="% Surviving", yscale=100,
#      main="Survival Distribution (Overall)") 

######## Compare the survival distributions of null and true (what we really want)
survobj<- with(FiveYearNullandTrue, Surv(year,status)) # same as above

fit1.5.A.S.S <- survfit(survobj~ModelType, data=FiveYearNullandTrue)


# Plot of 5 year null vs. modeled survival
# ggsurvplot(
#   fit1.5.A.S.S,
#   data = FiveYearNullandTrue_surv,
#   size = 1,                 # change line size
#   palette =
#     c("#E7B800", "#2E9FDF"),# custom color palettes
#   conf.int = TRUE,          # Add confidence interval
#   pval = TRUE,              # Add p-value
#   risk.table = TRUE,        # Add risk table
#   risk.table.col = "strata",# Risk table color by groups
#   surv.median.line = "hv",  # add the median survival pointer.
#   title = "5 yr Active Small/Small",
#   legend.labs =
#     c("Null", "NCE"),    # Change legend labels
#   risk.table.height = 0.25, # Useful to change when you have multiple groups
#   ggtheme = theme_bw()      # Change ggplot2 theme
# )


# test for difference between Null and NCE
# survival curves (logrank test) 


Allmod <- survdiff(Surv(year, status) ~ ModelType + Pred.Strat, data=FiveYearNullandTrue2)

# look for differences based on shifts
FiveYearTrue <-
  FiveYearNullandTrue2 %>%
  subset(ModelType == 1)


## Make Hazard Ratio tables ----
# prey behavioral changes
# FER thinks we should use this one
FiveYearNCE <-
  coxph(Surv(year, status) ~ propHabitat + propPredFree + propSafeSpace,
        data = FiveYearTrue)
haz.table <- FiveYearNCE %>%
  gtsummary::tbl_regression(exp = TRUE) 


# kitchen sink of starting conditions
# FER thinks we should use this one
FiveYearNCE <-
  coxph(Surv(year, status) ~ Pred.Strat + Pred.Start.Con*Prey.Start.Con,
        data = FiveYearTrue)
haz.table <- FiveYearNCE %>%
  gtsummary::tbl_regression(exp = TRUE) 


## Look at hazard ratio of behavioral changes by predator strategy

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


## Plot the data
# made a summary file
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

# Plot of hazard ratios
HR_plot <- ggplot(HRsumm_new,
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
  scale_fill_viridis_d(begin = 0.2, end = 0.8, name = "Predator Strategy") +
  scale_color_viridis_d(begin = 0.2, end = 0.8, name = "Predator Strategy") +
  ylab("Hazard Ratio") +
  xlab("Behavior Shift") +
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

# ggsave(HR_plot, filename = "Output_Figures/HazardRatiosPlot.png", width = 8, height = 5)









## make time plots for yr = 1, 2.5, and 5 to look at shifts

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
  # gghalves::geom_half_point(
  #   ## draw jitter on the left
  #   side = "l", 
  #   ## control range of jitter
  #   range_scale = .1, 
  #   ## add some transparency
  #   alpha = .3
  # geom_point(
  #   size = 0.5,
  #   postition = position_dodge(width = 0.5), 
  #   alpha = .3,
  #     position = position_dodge(width = 0.3)
  #   ) +
  theme_bw(base_size = 14) +
  ylim(0,1) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  scale_fill_viridis_d(begin = 0.2, end = 0.8, name = "Predator Strategy") +
  scale_color_viridis_d(begin = 0.2, end = 0.8, name = "Predator Strategy") +
  ylab("Prey proportional low-risk space or time") +
  xlab("Behavior Shift") +
  scale_x_discrete(labels=c("propHabitat" = "Habitat", "propSafeSpace" = "Space",
                            "propPredFree" = "Time")) +
  facet_grid(Pred.Start.Con ~ Prey.Start.Con,
             labeller = labeller(Pred.Start.Con = new_labels,
                                 Prey.Start.Con = new_labels2)) +
  ggtitle("Shifts at one year")


print(oneyrshift_plot)

ggsave(oneyrshift_plot, filename = "Output_Figures/OneYrShifts.png", width = 8, height = 5)







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

### Plot of behavioral shifts
fiveyrshift_plot <- ggplot(fiveyrshifts,
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
  # gghalves::geom_half_point(
  #   ## draw jitter on the left
  #   side = "l", 
  #   ## control range of jitter
  #   range_scale = .1, 
  #   ## add some transparency
  #   alpha = .3
  # geom_point(
  #   size = 0.5,
  #   postition = position_dodge(width = 0.5), 
  #   alpha = .3,
#     position = position_dodge(width = 0.3)
#   ) +
theme_bw(base_size = 14) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  ylim(0, 1) +
  scale_fill_viridis_d(begin = 0.2, end = 0.8, name = "Predator Strategy") +
  scale_color_viridis_d(begin = 0.2, end = 0.8, name = "Predator Strategy") +
  ylab("Prey proportional low-risk space or time") +
  xlab("Behavior Shift") +
  scale_x_discrete(labels=c("propHabitat" = "Habitat", "propPredFree" = "Time", 
                            "propSafeSpace" = "Space")) +
  facet_grid(Pred.Start.Con ~ Prey.Start.Con,
             labeller = labeller(Pred.Start.Con = new_labels,
                                 Prey.Start.Con = new_labels2)) +
  ggtitle("Shifts at five years")


print(fiveyrshift_plot)

ggsave(fiveyrshift_plot, filename = "Output_Figures/FiveYrShifts.png", width = 8, height = 5)



#################### Prepping separate huntings and looking at surivial plots across all of the data

## to get colors to match the other plot, I am using viridisLite to get the hex codes
# > viridis(11, alpha = 1, begin = 0, end = 1, direction = 1, option = "D")
# [1] "#440154FF" "#482576FF" "#414487FF" "#35608DFF" "#2A788EFF" "#21908CFF" "#22A884FF"
# [8] "#43BF71FF" "#7AD151FF" "#BBDF27FF" "#FDE725FF"


## Active 
FiveYearNullandTrue.Active <- FiveYearNullandTrue %>%
  subset(Pred.Strat == "Active") %>%
  mutate(Pred.Prey_Domain =interaction(Pred.Start.Con, Prey.Start.Con))


FiveYearNullandTrue.Active_surv <- FiveYearNullandTrue.Active %>%
  mutate(status = ifelse (ticks == 43800, 0, 1)) %>%
  mutate(year = ticks/365/24)




## Sit-and-Wait ----

FiveYearNullandTrue.SW <- FiveYearNullandTrue %>%
  subset(Pred.Strat == "Sit-and-Wait") %>%
  mutate(Pred.Prey_Domain =interaction(Pred.Start.Con, Prey.Start.Con))


FiveYearNullandTrue.SW_surv <- FiveYearNullandTrue.SW %>%
  mutate(status = ifelse (ticks == 43800, 0, 1)) %>%
  mutate(year = ticks/365/24)

## use facet within the survminer package
# trying to use this method for facet https://rpkgs.datanovia.com/survminer/reference/ggsurvplot_facet.html
# convert to factor
FiveYearNullandTrue.SW_surv$Prey.Start.Con <- as.factor(FiveYearNullandTrue.SW_surv$Prey.Start.Con)
FiveYearNullandTrue.SW_surv$Pred.Start.Con <- as.factor(FiveYearNullandTrue.SW_surv$Pred.Start.Con)



## Comparing consumptive vs. non-consumptive effects ----
# Figures for manuscript?


# > viridis(11, alpha = 1, begin = 0, end = 1, direction = 1, option = "D")
# [1] "#440154FF" "#482576FF" "#414487FF" "#35608DFF" "#2A788EFF" "#21908CFF" "#22A884FF"
# [8] "#43BF71FF" "#7AD151FF" "#BBDF27FF" "#FDE725FF"

## Active survival models

FiveYearNullandTrue.A <- FiveYearNullandTrue %>%
  subset(Pred.Strat == "Active") %>%
  mutate(Pred.Prey_Domain =interaction(Pred.Start.Con, Prey.Start.Con))

FiveYearNullandTrue.A_surv <- FiveYearNullandTrue.A %>%
  mutate(status = ifelse (ticks == 43800, 0, 1)) %>%
  mutate(year = ticks/365/24)

# reorder
FiveYearNullandTrue.A_surv$Pred.Start.Con <- factor(FiveYearNullandTrue.A_surv$Pred.Start.Con, 
                                    levels = c("Small", "Large"))
FiveYearNullandTrue.A_surv$Prey.Start.Con <- factor(FiveYearNullandTrue.A_surv$Prey.Start.Con, 
                                    levels = c("Small", "Large"))

## Facet plot for active predators
fitA <- survfit( Surv(year, status) ~ ModelType, data = FiveYearNullandTrue.A_surv)
ActiveMulti <- ggsurvplot_facet(fitA, 
                                FiveYearNullandTrue.A_surv, 
                                conf.int = TRUE,
                                facet.by = c("Pred.Start.Con", "Prey.Start.Con"),
                                palette = c("gray", "#414487FF"),
                                surv.median.line = "v", # add median survival
                                pval = TRUE,
                                pval.coord = c(3.5, 0.8),
                                ggtheme = theme_bw(base_size = 12),
                                short.panel.labs = TRUE,
                                panel.labs = list(Prey.Start.Con = c("Prey Small Domain", "Prey Large Domain"),
                                                  Pred.Start.Con = c("Predator Small Domain", "Predator Large Domain")),
                                legend.labs = c("Consumptive", "Non-consumptive"),
                                size = 1,
                                title = "Active Predator Survival Curves") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
print(ActiveMulti)
ggsave("Output_Figures/ActivePredSurv.png", dpi = 300, height = 6, width = 7)

# A small/small
A_SS <- FiveYearNullandTrue.A_surv %>%
  subset(Pred.Prey_Domain == "Small.Small")
# Get regression median and p-value of Null vs. NCE
ASSmod <- survfit(Surv(year, status) ~ ModelType, data = A_SS)
ASSmod

ASSplot <- 
  ggsurvplot(
    ASSmod,
    conf.int = TRUE, # add 95% confidence intervals
    size = 1,        # change line size
    palette = c("gray", "#73D055FF"), # custom color palettes
    pval = TRUE, # add p-value
    surv.median.line = "hv", # add median survival
    legend.labs = c("Consumptive", "Non-consumptive"),
    ggtheme = theme_bw(base_size = 12)
  )

# A small/large
A_SL <- FiveYearNullandTrue.A_surv %>%
  subset(Pred.Prey_Domain == "Small.Large")
# Get regression median and p-value of Null vs. NCE
ASLmod <- survfit(Surv(year, status) ~ ModelType, data = A_SL)
ASLmod

ASLplot <- 
  ggsurvplot(
    ASLmod,
    conf.int = TRUE, # add 95% confidence intervals
    size = 1,        # change line size
    palette = c("#1F968BFF", "#73D055FF"), # custom color palettes
    pval = TRUE, # add p-value
    surv.median.line = "hv", # add median survival
    legend.labs = c("Consumptive", "Non-consumptive"),
    ggtheme = theme_bw(base_size = 12)
  )

# A large/small
A_LS <- FiveYearNullandTrue.A_surv %>%
  subset(Pred.Prey_Domain == "Large.Small")
# Get regression median and p-value of Null vs. NCE
ALSmod <- survfit(Surv(year, status) ~ ModelType, data = A_LS)
ALSmod

ALSplot <- 
  ggsurvplot(
    ALSmod,
    conf.int = TRUE, # add 95% confidence intervals
    size = 1,        # change line size
    palette = c("#1F968BFF", "#73D055FF"), # custom color palettes
    pval = TRUE, # add p-value
    surv.median.line = "hv", # add median survival
    legend.labs = c("Consumptive", "Non-consumptive"),
    ggtheme = theme_bw(base_size = 12)
  )

# A large/large
A_LL <- FiveYearNullandTrue.A_surv %>%
  subset(Pred.Prey_Domain == "Large.Large")
# Get regression median and p-value of Null vs. NCE
ALLmod <- survfit(Surv(year, status) ~ ModelType, data = A_LL)
ALLmod

ALLplot <- 
  ggsurvplot(
    ALLmod,
    conf.int = TRUE, # add 95% confidence intervals
    size = 1,        # change line size
    palette = c("#1F968BFF", "#73D055FF"), # custom color palettes
    pval = TRUE, # add p-value
    surv.median.line = "hv", # add median survival
    legend.labs = c("Consumptive", "Non-consumptive"),
    ggtheme = theme_bw(base_size = 12)
  )

## multipanel
# this isn't working. Need to try ggsurvplot_facet
splots <- list(ASSplot, ASLplot, ALSplot, ALLplot)

all_active <- 
  arrange_ggsurvplots(
  splots,
  nrow = 2, ncol = 2,
  title = "Active predators",
  labels = "auto")



## Sit-and-wait survival models

FiveYearNullandTrue.SW <- FiveYearNullandTrue %>%
  subset(Pred.Strat == "Sit-and-Wait") %>%
  mutate(Pred.Prey_Domain =interaction(Pred.Start.Con, Prey.Start.Con))

FiveYearNullandTrue.SW_surv <- FiveYearNullandTrue.SW %>%
  mutate(status = ifelse (ticks == 43800, 0, 1)) %>%
  mutate(year = ticks/365/24)

# reorder
FiveYearNullandTrue.SW_surv$Pred.Start.Con <- factor(FiveYearNullandTrue.SW_surv$Pred.Start.Con, 
                                                    levels = c("Small", "Large"))
FiveYearNullandTrue.SW_surv$Prey.Start.Con <- factor(FiveYearNullandTrue.SW_surv$Prey.Start.Con, 
                                                    levels = c("Small", "Large"))

# SW facet plot
fitSW <- survfit( Surv(year, status) ~ ModelType, data = FiveYearNullandTrue.SW_surv)
SWMulti <- ggsurvplot_facet(fitSW, 
                                FiveYearNullandTrue.SW_surv, 
                                conf.int = TRUE,
                                facet.by = c("Pred.Start.Con", "Prey.Start.Con"),
                                palette = c("gray", "#7AD151FF"),
                                surv.median.line = "v", # add median survival
                                pval = TRUE,
                                pval.coord = c(0.1, 0.2),
                                ggtheme = theme_bw(base_size = 12),
                                short.panel.labs = TRUE,
                                panel.labs = list(Prey.Start.Con = c("Prey Small Domain", "Prey Large Domain"),
                                                 Pred.Start.Con = c("Predator Small Domain", "Predator Large Domain")),
                                legend.labs = c("Consumptive", "Non-consumptive"),
                                size = 1,
                                title = "Sit-and-Wait Predator Survival Curves") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
print(SWMulti)
ggsave("Output_Figures/SWPredSurv.png", dpi = 300, height = 6, width = 7)

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

## Sit-and-Pursue survival models

FiveYearNullandTrue.SP <- FiveYearNullandTrue %>%
  subset(Pred.Strat == "Sit-and-Pursue") %>%
  mutate(Pred.Prey_Domain =interaction(Pred.Start.Con, Prey.Start.Con))

FiveYearNullandTrue.SP_surv <- FiveYearNullandTrue.SP %>%
  mutate(status = ifelse (ticks == 43800, 0, 1)) %>%
  mutate(year = ticks/365/24)

# reorder
FiveYearNullandTrue.SP_surv$Pred.Start.Con <- factor(FiveYearNullandTrue.SP_surv$Pred.Start.Con, 
                                                     levels = c("Small", "Large"))
FiveYearNullandTrue.SP_surv$Prey.Start.Con <- factor(FiveYearNullandTrue.SP_surv$Prey.Start.Con, 
                                                     levels = c("Small", "Large"))

# SP facet plot

fitSP <- survfit( Surv(year, status) ~ ModelType, data = FiveYearNullandTrue.SP_surv)
SPMulti <- ggsurvplot_facet(fitSP, 
                            FiveYearNullandTrue.SP_surv, 
                            conf.int = TRUE,
                            facet.by = c("Pred.Start.Con", "Prey.Start.Con"),
                            palette = c("gray", "#21908CFF"),
                            surv.median.line = "v", # add median survival
                            pval = TRUE,
                            pval.coord = c(3.5, 0.8),
                            ggtheme = theme_bw(base_size = 12),
                            short.panel.labs = TRUE,
                            panel.labs = list(Prey.Start.Con = c("Prey Small Domain", "Prey Large Domain"),
                                              Pred.Start.Con = c("Predator Small Domain", "Predator Large Domain")),
                            legend.labs = c("Consumptive", "Non-consumptive"),
                            size = 1,
                            title = "Sit-and-Pursue Predator Survival Curves") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
print(SPMulti)
ggsave("Output_Figures/SPPredSurv.png", dpi = 300, height = 6, width = 7)

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

# load saved summary data for survival

NCEvCE <- read.csv("Data/NCEvNullSummary.csv", header = TRUE, fileEncoding="UTF-8-BOM")

survival_plot <- ggplot(NCEvCE,
                  aes(
                    x = Pred.Strat,
                    y = MedSurv,
                    fill = p.value,
                    group = Pred.Strat
                  )) +
  geom_line(aes(group = Pred.Strat)) +
  # geom_errorbar(
  #   aes(ymin = LCL95, ymax = UCL95, color = Model),
  #   width = 0.3,
  #   position = "dodge"
  # ) +
  geom_point(pch = 21,
             size = 4,
             position = position_dodge(width = 0.3)) +
  theme_bw(base_size = 14) +
  scale_fill_viridis_c() +
  ylab("Median survival time (yr)") +
  xlab("Predator strategy") +
  facet_grid(Pred.Start.Con ~ Prey.Start.Con)

ggsave(survival_plot, filename = "Output_Figures/UglyMedSurvPlot.png", width = 8, height = 5)




## survival plots ----

Predator.Prey<- unique(FiveYearNullandTrue.SP_surv$Pred.Prey_Domain)

splots <- list()
for (i in Predator.Prey) {
  subset1 <- FiveYearNullandTrue.SP_surv %>%
    subset(Pred.Prey_Domain == i)
  survobj <- with(subset1, Surv(year, status))
  ThePlot <- survfit(survobj ~ ModelType, data = subset1)
  splots[[i]] <-
    ggsurvplot(
      ThePlot,
      conf.int = TRUE, # add 95% confidence intervals
      size = 1,        # change line size
      palette = c("#E7B800", "#2E9FDF"), # custom color palettes
      pval = TRUE, # add p-value
      surv.median.line = "hv", # add median survival
      legend.labs = c("Null", "NCE"),
      ggtheme = theme_bw()
    ) + 
    ggtitle(paste0(i, " (Predator.Prey)"))
  #assign(paste("ThePlot", i, sep = ""), plot)
}
SPPlots <- arrange_ggsurvplots(
  splots,
  print = TRUE,
  ncol = 2,
  nrow = 2,
  title = "Survival Distributions for Sit-and-Pursue Hunting Strategy"
)

ggsave(SPPlots, filename = "Output_Figures/SPPredSurv.png", dpi = 300, width = 9, height = 7)

