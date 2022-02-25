#Senestivity Analysis of Jan 2021 SA 



#===load libraries======
library(ggplot2)
#library(plyr)
library(dplyr)
library(tidyverse)
library(readr)
library(ggrepel)
library(gridExtra)
library(ggrepel)

setwd("~/My Drive/Scholarship/Yale/Projects/PredPreyHabitatDomains/PredPreyHabitatDomain/Sensitivity Analysis Models")

#=====Attack======
Attack.SA <- read.csv("Active-SA2-LOF.csv")%>%
  mutate_if(is.logical, as.character)%>%
  group_by(parameter, index)%>%
  summarize(value = mean(value))%>%
  pivot_wider(names_from = index, values_from = value)

Attack.SA.plot<-ggplot(Attack.SA, aes(x=mustar, y=sigma)) + 
  geom_point(shape=22, fill = "black", size=2)+
  ggtitle("Attack")+
  xlab("mustar") +
  ylab("sigma")+
  #  ylim(0, 50)+
  #  xlim(0, 1100)+
  #geom_text(aes(label=parameter), size=3, hjust=-0.1, vjust=0)+
  geom_text_repel(aes(label = parameter),
                  box.padding   = 0.35, 
                  point.padding = 0.15,
                  segment.color = 'grey50',
                  size = 3) +
  theme(axis.text.x = element_text(size=12,  colour = "black"),
        axis.text.y = element_text(size=12,  colour = "black"),
        axis.ticks = element_line(colour = "black"),
        panel.background = element_rect(fill= "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black", size = 1)) 





#=====Sit And Wait======
Sit_And_Wait.SA <- read.csv("Sit-And-Wait-SA2-LOF.csv")%>%
  mutate_if(is.logical, as.character)%>%
  group_by(parameter, index)%>%
  summarize(value = mean(value))%>%
  pivot_wider(names_from = index, values_from = value)

Sit_And_Wait.SA.plot<-ggplot(Sit_And_Wait.SA, aes(x=mustar, y=sigma)) + 
  geom_point(shape=22, fill = "black", size=2)+
  ggtitle("Sit and Wait")+
  xlab("mustar") +
  ylab("sigma")+
  #  ylim(0, 50)+
  #  xlim(0, 1100)+
  #geom_text(aes(label=parameter), size=3, hjust=-0.1, vjust=0)+
  geom_text_repel(aes(label = parameter),
                  box.padding   = 0.35, 
                  point.padding = 0.15,
                  segment.color = 'grey50',
                  size = 3) +
  theme(axis.text.x = element_text(size=12,  colour = "black"),
        axis.text.y = element_text(size=12,  colour = "black"),
        axis.ticks = element_line(colour = "black"),
        panel.background = element_rect(fill= "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black", size = 1)) 




#=====Sit And Pursue======
Sit_And_Pursue.SA <- read.csv("Sit-And-Persue-SA2-LOF.csv")%>%
  mutate_if(is.logical, as.character)%>%
  group_by(parameter, index)%>%
  summarize(value = mean(value))%>%
  pivot_wider(names_from = index, values_from = value)

Sit_And_Pursue.SA.plot<-ggplot(Sit_And_Pursue.SA, aes(x=mustar, y=sigma)) + 
  geom_point(shape=22, fill = "black", size=2)+
  ggtitle("Sit and Pursue")+
  xlab("mustar") +
  ylab("sigma")+
  # ylim(0, 50)+
  #  xlim(0, 1100)+
  #geom_text(aes(label=parameter), size=3, hjust=-0.1, vjust=0)+
  geom_text_repel(aes(label = parameter),
                  box.padding   = 0.35, 
                  point.padding = 0.15,
                  segment.color = 'grey50',
                  size = 3) +
  theme(axis.text.x = element_text(size=12,  colour = "black"),
        axis.text.y = element_text(size=12,  colour = "black"),
        axis.ticks = element_line(colour = "black"),
        panel.background = element_rect(fill= "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black", size = 1)) 




#facet wrap====
Attack.SA$model<-"Attack"
Sit_And_Pursue.SA$model<-"Sit and Persue"
Sit_And_Wait.SA$model<-"Sit and Wait"


all.data<-rbind(Attack.SA, Sit_And_Pursue.SA, Sit_And_Wait.SA )  

quartz()
all.data%>%
  ggplot(aes(x=mustar, y=sigma))+
  geom_point(shape=22, fill = "black", size=2)+
  facet_wrap( ~ model , labeller = label_wrap_gen(2, multi_line=FALSE), scales = "fixed")+
  xlab("mustar") +
  ylab("sigma")+
  theme(axis.text.x=element_text(colour= "black", size = 12),
        axis.text.y = element_text(colour= "black", size = 12),
        axis.line = element_line(colour = "black", size = 1),
        axis.line.y = element_line(colour = "black", size =1),
        axis.ticks.x = element_line(colour = "black", size = 1),
        axis.ticks.y = element_line(colour = "black", size = 1),
        axis.ticks.length = unit(.2, "cm"),
        axis.title = element_text(size = 14, color = "black", face = "bold"),
        # panel.background = element_rect(fill= "white"),
        legend.position = "right",
        legend.title = element_text(color = "black", size = 12, face = "bold"),
        strip.text.x = element_text(size=14, color = "black", face = "bold"),
        strip.background = element_rect(color = "black", fill = "white", size = 1.5, linetype = "solid")) +
  geom_text_repel(aes(label = parameter),
                  segment.color = 'grey50',
                  size = 3) 




#all plots together====
allplots<-grid.arrange(Attack.SA.plot, Sit_And_Wait.SA.plot, Sit_And_Pursue.SA.plot, ncol=2, nrow=2) 

quartz()
allplots
