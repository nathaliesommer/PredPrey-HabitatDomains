# KO R-script playing around

# Stats models for proportional shifts
# KDO
# Last update Jan 2022

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
              "ggridges",
              "tidybayes",
              "modelr",
              "shinystan",
              "dplyr")

#Run the ipak loop
ipak(packages)



# load data ----
oneyr <- read_csv("Data/NCvsC_1year_TSH_Nov29.csv") %>%
  mutate(ModelType = 1)
fiveyr <- read_csv("Data/NCvsC_5year_TSH_Nov29.csv")
oneyr_null <- read_csv("Data/NCvsC_Nullyear_TSH_Nov19.csv") %>%
  mutate(ModelType = 0)

fiveyr_null <- read_csv("Data/NCvsC_NULL_5year_TSH_Nov29.csv")
# use df data from NCvsC_5years_Space_Time_Habitat

oneYearNullandTrue <- rbind(oneyr, oneyr_null)

oneYearNullandTrue <- oneYearNullandTrue  %>%
  mutate(propHabitat = White.Table/(Black.Table + White.Table)) %>%
  mutate(propSafeSpace = Domain.Prey/(Domain.Prey + DomainOverlap)) %>%
  mutate(propPredFree = rowSums(.[28:39])/rowSums(.[16:39]))
  
testing<- glm(ModelType ~ ticks + propHabitat + propSafeSpace + propPredFree + Pred.Prey_Domain, family = binomial, data = oneYearNullandTrue)
summary(testing)

# separate predator strategies
active1 <- subset(oneYearNullandTrue, Pred.Strat == "Active") %>%
  mutate(Pred.Prey_Domain =interaction(Pred.Start.Con, Prey.Start.Con)) 
  select( Pred.Prey_Domain, propW, propPredFree, propSafeSpace, ticks)

active1.Small.Small <- subset(oneYearNullandTrue, Pred.Strat == "Active") %>%
  mutate(Pred.Prey_Domain =interaction(Pred.Start.Con, Prey.Start.Con)) %>%
  subset(Pred.Prey_Domain == "Small.Small")
  
  testing<- glm(ModelType ~ ticks + propHabitat + propSafeSpace + propPredFree, family = binomial, data = active1.Small.Small)
  summary(testing)


###### Now for 5 years
  
  # load data ----

  fiveyr <- read_csv("Data/NCvsC_5year_TSH_Nov29.csv")%>%
    mutate(ModelType = 1)

  fiveyr_null <- read_csv("Data/NCvsC_NULL_5year_TSH_Nov29.csv")  %>%
    mutate(ModelType = 0)

FiveYearNullandTrue <- rbind(fiveyr, fiveyr_null)
  
FiveYearNullandTrue <- FiveYearNullandTrue  %>%
    mutate(propHabitat = White.Table/(Black.Table + White.Table)) %>%
    mutate(propSafeSpace = Domain.Prey/(Domain.Prey + DomainOverlap)) %>%
    mutate(propPredFree = rowSums(.[28:39])/rowSums(.[16:39]))
  
  testing<- glm(ModelType ~ ticks + propHabitat + propSafeSpace + propPredFree + Pred.Prey_Domain, family = binomial, data = oneYearNullandTrue)
  summary(testing)
  
  # separate predator strategies
  active5 <- subset(FiveYearNullandTrue, Pred.Strat == "Active") %>%
    mutate(Pred.Prey_Domain =interaction(Pred.Start.Con, Prey.Start.Con)) 
  
  active5.Small.Small <- subset(FiveYearNullandTrue, Pred.Strat == "Active") %>%
    mutate(Pred.Prey_Domain =interaction(Pred.Start.Con, Prey.Start.Con)) %>%
    subset(Pred.Prey_Domain == "Small.Small")
  
  active5.Large.Small <-  subset(FiveYearNullandTrue, Pred.Strat == "Active") %>%
    mutate(Pred.Prey_Domain =interaction(Pred.Start.Con, Prey.Start.Con)) %>%
    subset(Pred.Prey_Domain == "Large.Small")
  
SW5.Large.Small <-  subset(FiveYearNullandTrue, Pred.Strat == "Sit-and-Wait") %>%
    mutate(Pred.Prey_Domain =interaction(Pred.Start.Con, Prey.Start.Con)) %>%
    subset(Pred.Prey_Domain == "Large.Small")
  
  testing5<- glm(ModelType ~ ticks + propHabitat + propSafeSpace + propPredFree, family = binomial, data = active5.Small.Small)
  summary(testing5)
  
  active5.Large.Small.glm<- glm(ModelType ~ ticks + propHabitat + propSafeSpace + propPredFree, family = binomial, data = active5.Large.Small)
  summary(active5.Large.Small.glm)
  
  SWe5.Large.Small.glm<- glm(ModelType ~ ticks + propHabitat + propSafeSpace + propPredFree, family = binomial, data = SW5.Large.Small)
  summary(SWe5.Large.Small.glm)
  

####### 
  library(survival)
help(lung)  
View(lung)

survobj<- with(lung, Surv(time,status))
fit0 <- survfit(survobj~1, data=lung)
summary(fit0)
plot(fit0, xlab="Survival Time in Days", 
     ylab="% Surviving", yscale=100,
     main="Survival Distribution (Overall)") 

# Compare the survival distributions of men and women 
fit1 <- survfit(survobj~sex,data=lung)

# plot the survival distributions by sex 
plot(fit1, xlab="Survival Time in Days", 
     ylab="% Surviving", yscale=100, col=c("red","blue"),
     main="Survival Distributions by Gender") 
legend("topright", title="Gender", c("Male", "Female"),
       fill=c("red", "blue"))

# test for difference between male and female 
# survival curves (logrank test) 
survdiff(survobj~sex, data=lung) 

# predict male survival from age and medical scores 
MaleMod <- coxph(survobj~age+ph.ecog+ph.karno+pat.karno,
                 data=lung, subset=sex==1)

# display results 
MaleMod

# evaluate the proportional hazards assumption 
cox.zph(MaleMod)

#3 need status to be 1 or 0 

FiveYearNullandTrue_surv <-   active5.Small.Small %>%
  mutate(status = ifelse (ticks == 43800, 0, 1)) %>%
  mutate(year = ticks/365/24)

survobj<- with(FiveYearNullandTrue_surv, Surv(year,status))
fit0 <- survfit(survobj~1, data=FiveYearNullandTrue_surv)
summary(fit0)
plot(fit0, xlab="Survival Time in Hours", 
     ylab="% Surviving", yscale=100,
     main="Survival Distribution (Overall)") 

# Compare the survival distributions of null and true
survobj<- with(FiveYearNullandTrue_surv, Surv(year,status))
fit1.5.A.S.S <- survfit(survobj~ModelType, data=FiveYearNullandTrue_surv)
ggsurvplot(fit1.5.A.S.S, conf.int = TRUE, ggtheme = theme_minimal())



# plot the survival distributions by sex 
plot(fit1.5.A.S.S, xlab="Survival Time in Years", 
     ylab="% Surviving", yscale=100, col=c("red","blue"),
     main="Survival Distributions Null vs NCE \nfor Active Hunting Strategy \nfor Small Pred, Small Prey")
legend("topright", title="Model Type", c("Null", "NCE"),
       fill=c("red", "blue"))

# test for difference between male and female 
# survival curves (logrank test) 
survdiff(survobj~ModelType, data=FiveYearNullandTrue_surv) 

# predict male survival from age and medical scores 
MaleMod <- coxph(survobj~ propHabitat + propSafeSpace + propPredFree,
                 data=FiveYearNullandTrue_surv)

# display results 
MaleMod


# evaluate the proportional hazards assumption 
cox.zph(MaleMod)

#################### All others

## Active 
FiveYearNullandTrue.Active <- FiveYearNullandTrue %>%
  subset(Pred.Strat == "Active") %>%
mutate(Pred.Prey_Domain =interaction(Pred.Start.Con, Prey.Start.Con))

  
FiveYearNullandTrue.Active_surv <- FiveYearNullandTrue.Active %>%
  mutate(status = ifelse (ticks == 43800, 0, 1)) %>%
  mutate(year = ticks/365/24)

Predator.Prey<- unique(FiveYearNullandTrue.Active_surv$Pred.Prey_Domain)

splots<- list()
for (i in Predator.Prey) {
  print(i)
   subset1<- FiveYearNullandTrue.Active_surv %>%
    subset(Pred.Prey_Domain == i)
  survobj<- with(subset1, Surv(year,status))
 ThePlot<- survfit(survobj~ModelType, data=subset1)
 splots[[i]]<-ggsurvplot(ThePlot, conf.int = TRUE, legend.labs=c("Model = Null", "Model =NCE"), ggtheme = theme_minimal()) +
   ggtitle(paste0(i, " (Predator.Prey)"))
 #assign(paste("ThePlot", i, sep = ""), plot)
}
ActivePlots<- arrange_ggsurvplots(splots, print = TRUE,
                    ncol = 2, nrow = 2, title = "Survival Distributions Null (red) vs NCE (blue) for Active Hunting Strategy")

## not used

par(mfrow=c(2,2))

for (i in Predator.Prey) {
  subset<- FiveYearNullandTrue.Active_surv %>%
    subset(Pred.Prey_Domain == i)
  survobj <- with(subset, Surv(year, status))
  # Compare the survival distributions of men and women 
  fit1<- survfit(survobj~ModelType, data=subset)
  
  # plot the survival distributions by sex 
  plot(fit1, xlab="Survival Time in Years", 
       ylab="% Surviving", yscale=100, col=c("red","blue"),
       main= paste0( i, " (Predator.Prey)"))
  #legend("topright", title="Model Type", c("Null", "NCE"),
  #       fill=c("red", "blue"))
  mtext("Survival Distributions Null (red) vs NCE (blue) for Active Hunting Strategy", side = 3, line = -1, outer = TRUE)
}


library(gridExtra)
library(ggplot2)




## Sit-and-Wait

FiveYearNullandTrue.SW <- FiveYearNullandTrue %>%
  subset(Pred.Strat == "Sit-and-Wait") %>%
  mutate(Pred.Prey_Domain =interaction(Pred.Start.Con, Prey.Start.Con))


FiveYearNullandTrue.SW_surv <- FiveYearNullandTrue.SW %>%
  mutate(status = ifelse (ticks == 43800, 0, 1)) %>%
  mutate(year = ticks/365/24)

Predator.Prey<- unique(FiveYearNullandTrue.SW_surv$Pred.Prey_Domain)

splots<- list()
for (i in Predator.Prey) {
  subset1<- FiveYearNullandTrue.SW_surv %>%
    subset(Pred.Prey_Domain == i)
  survobj<- with(subset1, Surv(year,status))
  ThePlot<- survfit(survobj~ModelType, data=subset1)
  splots[[i]]<-ggsurvplot(ThePlot, conf.int = TRUE, legend.labs=c("Model = Null", "Model = NCE"), ggtheme = theme_minimal()) +
    ggtitle(paste0(i, " (Predator.Prey)"))
  #assign(paste("ThePlot", i, sep = ""), plot)
}
SWPlots<- arrange_ggsurvplots(splots, print = TRUE,
                    ncol = 2, nrow = 2, title = "Survival Distributions Null (red) vs NCE (blue) for Sit-and-Wait Hunting Strategy")


## Sit-and-Pursue


FiveYearNullandTrue.SP <- FiveYearNullandTrue %>%
  subset(Pred.Strat == "Sit-and-Pursue") %>%
  mutate(Pred.Prey_Domain =interaction(Pred.Start.Con, Prey.Start.Con))


FiveYearNullandTrue.SP_surv <- FiveYearNullandTrue.SP %>%
  mutate(status = ifelse (ticks == 43800, 0, 1)) %>%
  mutate(year = ticks/365/24)

Predator.Prey<- unique(FiveYearNullandTrue.SP_surv$Pred.Prey_Domain)

splots<-list()
for (i in Predator.Prey) {
  subset<- FiveYearNullandTrue.SP_surv %>%
    subset(Pred.Prey_Domain == i)
  survobj<- with(subset1, Surv(year,status))
  ThePlot<- survfit(survobj~ModelType, data=subset)
  splots[[i]]<-ggsurvplot(ThePlot, conf.int = TRUE, legend.labs=c("Model = Null", "Model = NCE"), ggtheme = theme_minimal()) +
    ggtitle(paste0(i, " (Predator.Prey)"))
  #assign(paste("ThePlot", i, sep = ""), plot)
}
SPPlots<- arrange_ggsurvplots(splots, print = TRUE,
                              ncol = 2, nrow = 2, title = "Survival Distributions Null (red) vs NCE (blue) for Sit-and-Pursue Hunting Strategy")



### Test this out later
install.packages(c("survival", "survminer"))

library("survival")
library("survminer")


survobj<- with(FiveYearNullandTrue.Active_surv, Surv(year,status))
fit0 <- survfit(survobj~1, data=FiveYearNullandTrue.Active_surv )
summary(fit0)
plot(fit0, xlab="Survival Time in Hours", 
     ylab="% Surviving", yscale=100,
     main="Survival Distribution (Overall)") 

# Compare the survival distributions of men and women 
fit1.5.A.S.S <- survfit(survobj~ModelType, data=FiveYearNullandTrue.Active_surv )




# convert to factors ----
# hunting mode and starting conditions as factors
oneyr$Pred.Strat <- as.factor(oneyr$Pred.Strat)
oneyr$Prey.Start.Con <- as.factor(oneyr$Prey.Start.Con)
oneyr$Pred.Start.Con <- as.factor(oneyr$Pred.Start.Con)

oneyr_null$Pred.Strat <- as.factor(oneyr_null$Pred.Strat)
oneyr_null$Prey.Start.Con <- as.factor(oneyr_null$Prey.Start.Con)
oneyr_null$Pred.Start.Con <- as.factor(oneyr_null$Pred.Start.Con)

fiveyr$Pred.Strat <- as.factor(fiveyr$Pred.Strat)
fiveyr$Prey.Start.Con <- as.factor(fiveyr$Prey.Start.Con)
fiveyr$Pred.Start.Con <- as.factor(fiveyr$Pred.Start.Con)

fiveyr_null$Pred.Strat <- as.factor(fiveyr_null$Pred.Strat)
fiveyr_null$Prey.Start.Con <- as.factor(fiveyr_null$Prey.Start.Con)
fiveyr_null$Pred.Start.Con <- as.factor(fiveyr_null$Pred.Start.Con)


# calculate shifts ----
# Habitat shifts - White.Table has lower detectability
oneyr$propW <- oneyr$White.Table/(oneyr$Black.Table + oneyr$White.Table)
oneyr_null$propW <- oneyr_null$White.Table/(oneyr_null$Black.Table + oneyr_null$White.Table)
fiveyr$propW <- fiveyr$White.Table/(fiveyr$Black.Table + fiveyr$White.Table)
fiveyr_null$propW <- fiveyr_null$White.Table/(fiveyr_null$Black.Table + fiveyr_null$White.Table)

# Space shift: overlap with predator in space
# Domain.Prey is where only prey are found (no predators)
oneyr$propSafeSpace <- oneyr$Domain.Prey/(oneyr$DomainOverlap + oneyr$Domain.Prey)
oneyr_null$propSafeSpace <- oneyr_null$Domain.Prey/(oneyr_null$DomainOverlap + oneyr_null$Domain.Prey)
fiveyr$propSafeSpace <- fiveyr$Domain.Prey/(fiveyr$DomainOverlap + fiveyr$Domain.Prey)
fiveyr_null$propSafeSpace <- fiveyr_null$Domain.Prey/(fiveyr_null$DomainOverlap + fiveyr_null$Domain.Prey)

# time shifts
# proportion of time spent in predator free hrs 0-11
# sum all time active
oneyr$SumTime <- apply(oneyr[,c(16:39)], 1, sum)
oneyr$PredOverlap <- apply(oneyr[,c(16:27)], 1, sum)
oneyr$propPredFree <- 1 - oneyr$PredOverlap/oneyr$SumTime

oneyr_null$SumTime <- apply(oneyr_null[,c(16:39)], 1, sum)
oneyr_null$PredOverlap <- apply(oneyr_null[,c(16:27)], 1, sum)
oneyr_null$propPredFree <- 1 - oneyr_null$PredOverlap/oneyr_null$SumTime

fiveyr$SumTime <- apply(fiveyr[,c(16:39)], 1, sum)
fiveyr$PredOverlap <- apply(fiveyr[,c(16:27)], 1, sum)
fiveyr$propPredFree <- 1 - fiveyr$PredOverlap/fiveyr$SumTime

fiveyr_null$SumTime <- apply(fiveyr_null[,c(16:39)], 1, sum)
fiveyr_null$PredOverlap <- apply(fiveyr_null[,c(16:27)], 1, sum)
fiveyr_null$propPredFree <- 1 - fiveyr_null$PredOverlap/fiveyr_null$SumTime

## the proportions of proportions
# let's go!
oneyr$shifts <- oneyr$propSafeSpace/oneyr$propPredFree
oneyr_null$shifts <- oneyr_null$propSafeSpace/oneyr_null$propPredFree
fiveyr$shifts <- fiveyr$propSafeSpace/fiveyr$propPredFree
fiveyr_null$shifts <- fiveyr_null$propSafeSpace/fiveyr_null$propPredFree


# separate predator strategies
active1 <- subset(oneyr, Pred.Strat == "Active") %>%
  mutate(Pred.Prey_Domain =interaction(Pred.Start.Con, Prey.Start.Con)) %>%
  select( Pred.Prey_Domain, propW, propPredFree, propSafeSpace, ticks)
#
active1_null <- subset(oneyr_null, Pred.Strat == "Active") %>%
  mutate(Pred.Prey_Domain =interaction(Pred.Start.Con, Prey.Start.Con)) %>%
  select( Pred.Prey_Domain, propW, propPredFree, propSafeSpace, ticks)
#

sitwait1 <- subset(oneyr, Pred.Strat == "Sit-and-Wait")
pursue1 <- subset(oneyr, Pred.Strat == "Sit-and-Pursue")

active5 <- subset(fiveyr, Pred.Strat == "Active")
sitwait5 <- subset(fiveyr, Pred.Strat == "Sit-and-Wait")
pursue5 <- subset(fiveyr, Pred.Strat == "Sit-and-Pursue")

####### Principal component analysis ####### 
results<-prcomp(active1, scale = TRUE) 
#reverse signs
results$rotation<- -1*results$rotation

results$rotation

biplot(results, scale = 0)

PCA_active_SS <- active1 %>%
  subset(Pred.Prey_Domain == "Small.Small") %>%
  select(ticks, propW, propPredFree, propSafeSpace)
row.names(PCA_active_SS)<- PCA_active_SS$ticks

PCA_active_SS <- PCA_active_SS[,-1]

results<-prcomp(PCA_active_SS, scale = TRUE) 


####### 3d scatter plot ####### 
install.packages("rgl")
library(rgl)
palette(rainbow(4))

rgl::plot3d(x = active1$propW, y = active1$propPredFree, z = active1$propSafeSpace, col = as.integer(active1$Pred.Prey_Domain)) 
legend3d("topright", legend = levels(active1$Pred.Prey_Domain), col = rainbow(4), pch = 19)


rgl::plot3d(x = active1_null $propW, y = active1_null $propPredFree, z = active1_null $propSafeSpace, col = as.integer(active1_null $Pred.Prey_Domain)) 
legend3d("topright", legend = levels(active1_null $Pred.Prey_Domain), col = rainbow(4), pch = 19)


#######
active1.2<- oneyr %>%
  subset(Pred.Start.Con == "Small") %>%
  subset(Prey.Start.Con == "Small")
  
test_of_death <- glm(ticks ~ propPredFree + propSafeSpace + propW, data = active1.2)
summary(test_of_death)
