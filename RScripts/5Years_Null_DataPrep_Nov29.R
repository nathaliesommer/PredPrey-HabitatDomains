

#Import Data

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
              "dplyr") #additional color palettes

#Run the ipak loop
ipak(packages)


setwd("/Users/kaggieorrick/Desktop/OneDrive - Yale University/NetLogo/Pred-Prey/Pred_PreyHabitatDomains/PredPrey-HabitatDomains/RScripts")
FILE_TO_READ <- "../Data/YEAR5_NULL_8x12_death_Space_Time_Habitat_Nov29Final YEAR5_MODELS-table.csv"

data<- read.csv(FILE_TO_READ, skip = 6, stringsAsFactors = FALSE, col.names=c("run Number", "Pred-Strat", "Prey-Start-Con",  "Pred-Start-Con", "Detect-Black",  "Detect-White", "Step", "ticks", "nearby", "mapAware-table", "Hour-Map",  "Black-Table", "White-Table", "DomainOverlap", "Domain-Prey"))

# KO manipulating data
list_of_list<- as.list(as.list(data$Hour.Map))

#Removing of the brackets
shortened<- str_split(list_of_list, "] ", simplify = TRUE ) # spliting by the "]" bracket (can't do it the other way because R thinks you are doing something)
shortened1 <- substring(shortened, 2) #removing the open bracket "["
shortened2<- as.data.frame(shortened1) #making into a dataframe
shortened2$V1<-substring(shortened2$V1, 2) #"manually" removing the extra bracket in the first column
shortened2$V24<-substring(shortened2$V24, 1, nchar(shortened2$V24) - 2) #"manually" removing the last 2 brackets in the last column

#I seriously am unsure if all are needed but this puts the table together
transposed<- t(shortened2) %>% #first transpose the list, so it's
  str_split(" ")

df <- as.data.frame(transposed) #turn it into a datframe


finished<- as.data.frame(t(df))


newdata<- finished[,1:2]

newdata2<- newdata %>%
  mutate(hours = as.numeric(V1)) %>%
  mutate(amount_used = as.numeric(V2))

finished2<- newdata2[,3:4]

hour0s <- finished2 %>%
  filter(hours == 0)


hour1s <- finished2 %>%
  filter(hours == 1)


hour2s <- finished2 %>%
  filter(hours == 2) 


hour3s <- finished2 %>%
  filter(hours == 3) 

hour4s <- finished2 %>%
  filter(hours == 4) 

hour5s <- finished2 %>%
  filter(hours == 5) 

hour6s <- finished2 %>%
  filter(hours == 6) 

hour7s <- finished2 %>%
  filter(hours == 7)

hour8s <- finished2 %>%
  filter(hours == 8)

hour9s <- finished2 %>%
  filter(hours == 9) 

hour10s <- finished2 %>%
  filter(hours == 10) 

hour11s <- finished2 %>%
  filter(hours == 11) 

hour12s <- finished2 %>%
  filter(hours == 12)

hour13s <- finished2 %>%
  filter(hours == 13) 

hour14s <- finished2 %>%
  filter(hours == 14)

hour15s <- finished2 %>%
  filter(hours == 15) 

hour16s <- finished2 %>%
  filter(hours == 16) 

hour17s <- finished2 %>%
  filter(hours == 17) 

hour18s <- finished2 %>%
  filter(hours == 18) 

hour19s <- finished2 %>%
  filter(hours == 19) 

hour20s <- finished2 %>%
  filter(hours == 20) 

hour21s <- finished2 %>%
  filter(hours == 21)

hour22s <- finished2 %>%
  filter(hours == 22) 

hour23s <- finished2 %>%
  filter(hours == 23) 


full_df<- cbind.data.frame(hour0s[,2], hour1s[,2],hour2s[,2], hour3s[,2], hour4s[,2], hour5s[,2], hour6s[,2], hour7s[,2], hour8s[,2], hour9s[,2], hour10s[,2], hour11s[,2],hour12s[,2], hour13s[,2], hour14s[,2], hour15s[,2], hour16s[,2], hour17s[,2], hour18s[,2], hour19s[,2], hour20s[,2], hour21s[,2],hour22s[,2], hour23s[,2])


colnames(full_df)<- c("hour0", "hour1", "hour2","hour3","hour4","hour5", "hour6", "hour7", "hour8","hour9","hour10","hour11", "hour12","hour13","hour14","hour15", "hour16", "hour17", "hour18","hour19","hour20","hour21", "hour22","hour23")



final<- cbind(data, full_df)

list_of_list<- as.list(as.list(data$mapAware.table))

#Removing of the brackets
shortened<- str_split(list_of_list, "] ", simplify = TRUE ) # spliting by the "]" bracket (can't do it the other way because R thinks you are doing something)
shortened1 <- substring(shortened, 2) #removing the open bracket "["
shortened2<- as.data.frame(shortened1) #making into a dataframe
shortened2$V1<-substring(shortened2$V1, 2) #"manually" removing the extra bracket in the first column
shortened2$V24<-substring(shortened2$V24, 1, nchar(shortened2$V24) - 2) #"manually" removing the last 2 brackets in the last column

#I seriously am unsure if all are needed but this puts the table together
transposed<- t(shortened2) %>% #first transpose the list, so it's
  str_split(" ")

df <- as.data.frame(transposed) #turn it into a datframe


finished<- as.data.frame(t(df))


newdata<- finished[,1:2]

newdata2<- newdata %>%
  mutate(hours = as.numeric(V1)) %>%
  mutate(amount_used = as.numeric(V2))

finished2<- newdata2[,3:4]

hour0s <- finished2 %>%
  filter(hours == 0)


hour1s <- finished2 %>%
  filter(hours == 1)


hour2s <- finished2 %>%
  filter(hours == 2) 


hour3s <- finished2 %>%
  filter(hours == 3) 

hour4s <- finished2 %>%
  filter(hours == 4) 

hour5s <- finished2 %>%
  filter(hours == 5) 

hour6s <- finished2 %>%
  filter(hours == 6) 

hour7s <- finished2 %>%
  filter(hours == 7)

hour8s <- finished2 %>%
  filter(hours == 8)

hour9s <- finished2 %>%
  filter(hours == 9) 

hour10s <- finished2 %>%
  filter(hours == 10) 

hour11s <- finished2 %>%
  filter(hours == 11) 

hour12s <- finished2 %>%
  filter(hours == 12)

hour13s <- finished2 %>%
  filter(hours == 13) 

hour14s <- finished2 %>%
  filter(hours == 14)

hour15s <- finished2 %>%
  filter(hours == 15) 

hour16s <- finished2 %>%
  filter(hours == 16) 

hour17s <- finished2 %>%
  filter(hours == 17) 

hour18s <- finished2 %>%
  filter(hours == 18) 

hour19s <- finished2 %>%
  filter(hours == 19) 

hour20s <- finished2 %>%
  filter(hours == 20) 

hour21s <- finished2 %>%
  filter(hours == 21)

hour22s <- finished2 %>%
  filter(hours == 22) 

hour23s <- finished2 %>%
  filter(hours == 23) 


full_df<- cbind.data.frame(hour0s[,2], hour1s[,2],hour2s[,2], hour3s[,2], hour4s[,2], hour5s[,2], hour6s[,2], hour7s[,2], hour8s[,2], hour9s[,2], hour10s[,2], hour11s[,2],hour12s[,2], hour13s[,2], hour14s[,2], hour15s[,2], hour16s[,2], hour17s[,2], hour18s[,2], hour19s[,2], hour20s[,2], hour21s[,2],hour22s[,2], hour23s[,2])


colnames(full_df)<- c("ihour0", "ihour1", "ihour2","ihour3","ihour4","ihour5", "ihour6", "ihour7", "ihour8","ihour9","ihour10","ihour11", "ihour12","ihour13","ihour14","ihour15", "ihour16", "ihour17", "ihour18","ihour19","ihour20","ihour21", "ihour22","ihour23")

rm(list=ls()[ls() %in% c("hour0s", "hour1s", "hour2s","hour3s","hour4s","hour5s", "hour6s", "hour7s", "hour8s","hour9s","hour10s","hour11s", "hour12s","hour13s","hour14s","hour15s", "hour16s", "hour17s", "hour18s","hour19s","hour20s","hour21s", "hour22s","hour23s","transposed", "shortened", "shortened1", "shortened2", "finished", "finished2")])


final2<- cbind(final, full_df)

data <- final2 %>%
  mutate(nearby = gsub("\\[|\\]", "", data$nearby))




NAME_OF_CLEAN_DATA <- "/Users/kaggieorrick/Desktop/OneDrive - Yale University/NetLogo/Pred-Prey/Pred_PreyHabitatDomains/PredPrey-HabitatDomains/Data/NCvsC_NULL_5year_TSH_Nov29.csv"

write.csv(data, NAME_OF_CLEAN_DATA, row.names = FALSE)
getwd()

