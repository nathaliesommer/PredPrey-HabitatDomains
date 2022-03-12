#Caribou movement on nutrient distribution 
#Plotting of sensitivity analysis results

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


#=====Active======
Active.SA <- read.csv("Active-SA2-LOF.csv")%>%
  mutate_if(is.logical, as.character)%>% 
  group_by(parameter, index)%>%  
  summarize(value = mean(value))%>%
  pivot_wider(names_from = index, values_from = value)

Active.SA.plot<-ggplot(Active.SA, aes(x=mustar, y=sigma)) + 
  geom_point(shape=22, fill = "black", size=2)+
  ggtitle("Active")+  xlab("mustar") +  ylab("sigma")+ #  ylim(0, 50)+  xlim(0, 1100)+
  geom_text_repel(aes(label = parameter),  box.padding   = 0.35, point.padding = 0.15, segment.color = 'grey50', size = 3) +
  theme(axis.text.x = element_text(size=12,  colour = "black"), axis.text.y = element_text(size=12,  colour = "black"), axis.ticks = element_line(colour = "black"), panel.background = element_rect(fill= "white"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_rect(fill = NA, colour = "black", size = 1)) 


Active.SA2 <- read.csv("Active-SA2-LOF2.csv")%>%
  mutate_if(is.logical, as.character)%>% 
  group_by(parameter, index)%>%  
  summarize(value = mean(value))%>%
  pivot_wider(names_from = index, values_from = value)

Active.SA2.plot<-ggplot(Active.SA2, aes(x=mustar, y=sigma)) + 
  geom_point(shape=22, fill = "black", size=2)+
  ggtitle("Active")+  xlab("mustar") +  ylab("sigma")+ #  ylim(0, 50)+  xlim(0, 1100)+
  geom_text_repel(aes(label = parameter),  box.padding   = 0.35, point.padding = 0.15, segment.color = 'grey50', size = 3) +
  theme(axis.text.x = element_text(size=12,  colour = "black"), axis.text.y = element_text(size=12,  colour = "black"), axis.ticks = element_line(colour = "black"), panel.background = element_rect(fill= "white"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_rect(fill = NA, colour = "black", size = 1)) 


Active.SA3 <- read.csv("Active-SA2-LOF3.csv")%>%
  mutate_if(is.logical, as.character)%>% 
  group_by(parameter, index)%>%  
  summarize(value = mean(value))%>%
  pivot_wider(names_from = index, values_from = value)

Active.SA3.plot<-ggplot(Active.SA3, aes(x=mustar, y=sigma)) + 
  geom_point(shape=22, fill = "black", size=2)+
  ggtitle("Active")+  xlab("mustar") +  ylab("sigma")+ #  ylim(0, 50)+  xlim(0, 1100)+
  geom_text_repel(aes(label = parameter),  box.padding   = 0.35, point.padding = 0.15, segment.color = 'grey50', size = 3) +
  theme(axis.text.x = element_text(size=12,  colour = "black"), axis.text.y = element_text(size=12,  colour = "black"), axis.ticks = element_line(colour = "black"), panel.background = element_rect(fill= "white"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_rect(fill = NA, colour = "black", size = 1)) 




Active.SA4 <- read.csv("Active-SA2-LOF4.csv")%>%
  mutate_if(is.logical, as.character)%>% 
  group_by(parameter, index)%>%  
  summarize(value = mean(value))%>%
  pivot_wider(names_from = index, values_from = value)

Active.SA4.plot<-ggplot(Active.SA4, aes(x=mustar, y=sigma)) + 
  geom_point(shape=22, fill = "black", size=2)+
  ggtitle("Active")+  xlab("mustar") +  ylab("sigma")+ #  ylim(0, 50)+  xlim(0, 1100)+
  geom_text_repel(aes(label = parameter),  box.padding   = 0.35, point.padding = 0.15, segment.color = 'grey50', size = 3) +
  theme(axis.text.x = element_text(size=12,  colour = "black"), axis.text.y = element_text(size=12,  colour = "black"), axis.ticks = element_line(colour = "black"), panel.background = element_rect(fill= "white"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_rect(fill = NA, colour = "black", size = 1)) 



Active.SA5 <- read.csv("Active-SA2-LOF5.csv")%>%
  mutate_if(is.logical, as.character)%>% 
  group_by(parameter, index)%>%  
  summarize(value = mean(value))%>%
  pivot_wider(names_from = index, values_from = value)

Active.SA5.plot<-ggplot(Active.SA5, aes(x=mustar, y=sigma)) + 
  geom_point(shape=22, fill = "black", size=2)+
  ggtitle("Active")+  xlab("mustar") +  ylab("sigma")+ #  ylim(0, 50)+  xlim(0, 1100)+
  geom_text_repel(aes(label = parameter),  box.padding   = 0.35, point.padding = 0.15, segment.color = 'grey50', size = 3) +
  theme(axis.text.x = element_text(size=12,  colour = "black"), axis.text.y = element_text(size=12,  colour = "black"), axis.ticks = element_line(colour = "black"), panel.background = element_rect(fill= "white"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_rect(fill = NA, colour = "black", size = 1)) 




quartz()
allplots<-grid.arrange(Active.SA.plot, Active.SA2.plot, Active.SA3.plot, Active.SA4.plot, Active.SA5.plot, ncol=2, nrow=3) 

#= = = = 
#Acitve with more runs ====
Active.SA6 <- read.csv("Active-SA2-LOF6.csv")%>%
  mutate_if(is.logical, as.character)%>% 
  group_by(parameter, index)%>%  
  summarize(value = mean(value))%>%
  pivot_wider(names_from = index, values_from = value)

Active.SA6.plot<-ggplot(Active.SA6, aes(x=mustar, y=sigma)) + 
  geom_point(shape=22, fill = "black", size=2)+
  ggtitle("Active")+  xlab("mustar") +  ylab("sigma")+ #  ylim(0, 50)+  xlim(0, 1100)+
  geom_text_repel(aes(label = parameter),  box.padding   = 0.35, point.padding = 0.15, segment.color = 'grey50', size = 3) +
  theme(axis.text.x = element_text(size=12,  colour = "black"), axis.text.y = element_text(size=12,  colour = "black"), axis.ticks = element_line(colour = "black"), panel.background = element_rect(fill= "white"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_rect(fill = NA, colour = "black", size = 1)) 


Active.SA7 <- read.csv("Active-SA2-LOF7.csv")%>%
  mutate_if(is.logical, as.character)%>% 
  group_by(parameter, index)%>%  
  summarize(value = mean(value))%>%
  pivot_wider(names_from = index, values_from = value)

Active.SA7.plot<-ggplot(Active.SA7, aes(x=mustar, y=sigma)) + 
  geom_point(shape=22, fill = "black", size=2)+
  ggtitle("Active")+  xlab("mustar") +  ylab("sigma")+ #  ylim(0, 50)+  xlim(0, 1100)+
  geom_text_repel(aes(label = parameter),  box.padding   = 0.35, point.padding = 0.15, segment.color = 'grey50', size = 3) +
  theme(axis.text.x = element_text(size=12,  colour = "black"), axis.text.y = element_text(size=12,  colour = "black"), axis.ticks = element_line(colour = "black"), panel.background = element_rect(fill= "white"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_rect(fill = NA, colour = "black", size = 1)) 


Active.SA8 <- read.csv("Active-SA2-LOF8.csv")%>%
  mutate_if(is.logical, as.character)%>% 
  group_by(parameter, index)%>%  
  summarize(value = mean(value))%>%
  pivot_wider(names_from = index, values_from = value)

Active.SA8.plot<-ggplot(Active.SA8, aes(x=mustar, y=sigma)) + 
  geom_point(shape=22, fill = "black", size=2)+
  ggtitle("Active")+  xlab("mustar") +  ylab("sigma")+ #  ylim(0, 50)+  xlim(0, 1100)+
  geom_text_repel(aes(label = parameter),  box.padding   = 0.35, point.padding = 0.15, segment.color = 'grey50', size = 3) +
  theme(axis.text.x = element_text(size=12,  colour = "black"), axis.text.y = element_text(size=12,  colour = "black"), axis.ticks = element_line(colour = "black"), panel.background = element_rect(fill= "white"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_rect(fill = NA, colour = "black", size = 1)) 




Active.SA9 <- read.csv("Active-SA2-LOF9.csv")%>%
  mutate_if(is.logical, as.character)%>% 
  group_by(parameter, index)%>%  
  summarize(value = mean(value))%>%
  pivot_wider(names_from = index, values_from = value)

Active.SA9.plot<-ggplot(Active.SA9, aes(x=mustar, y=sigma)) + 
  geom_point(shape=22, fill = "black", size=2)+
  ggtitle("Active")+  xlab("mustar") +  ylab("sigma")+ #  ylim(0, 50)+  xlim(0, 1100)+
  geom_text_repel(aes(label = parameter),  box.padding   = 0.35, point.padding = 0.15, segment.color = 'grey50', size = 3) +
  theme(axis.text.x = element_text(size=12,  colour = "black"), axis.text.y = element_text(size=12,  colour = "black"), axis.ticks = element_line(colour = "black"), panel.background = element_rect(fill= "white"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_rect(fill = NA, colour = "black", size = 1)) 



Active.SA10 <- read.csv("Active-SA2-LOF10.csv")%>%
  mutate_if(is.logical, as.character)%>% 
  group_by(parameter, index)%>%  
  summarize(value = mean(value))%>%
  pivot_wider(names_from = index, values_from = value)

Active.SA10.plot<-ggplot(Active.SA10, aes(x=mustar, y=sigma)) + 
  geom_point(shape=22, fill = "black", size=2)+
  ggtitle("Active")+  xlab("mustar") +  ylab("sigma")+ #  ylim(0, 50)+  xlim(0, 1100)+
  geom_text_repel(aes(label = parameter),  box.padding   = 0.35, point.padding = 0.15, segment.color = 'grey50', size = 3) +
  theme(axis.text.x = element_text(size=12,  colour = "black"), axis.text.y = element_text(size=12,  colour = "black"), axis.ticks = element_line(colour = "black"), panel.background = element_rect(fill= "white"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_rect(fill = NA, colour = "black", size = 1)) 




quartz()
allplots<-grid.arrange(Active.SA6.plot, Active.SA7.plot, Active.SA8.plot, Active.SA9.plot, Active.SA10.plot, ncol=2, nrow=3) 





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
Sit_And_Pursue.SA <- read.csv("Sit-and-Pursue-SA2-LOF.csv")%>%
  mutate_if(is.logical, as.character)%>% 
  group_by(parameter, index)%>%  
  summarize(value = mean(value))%>%
  pivot_wider(names_from = index, values_from = value)

Sit_And_Pursue.SA.plot<-ggplot(Sit_And_Pursue.SA, aes(x=mustar, y=sigma)) + 
  geom_point(shape=22, fill = "black", size=2)+
  ggtitle("Active")+  xlab("mustar") +  ylab("sigma")+   ylim(70, 130)+  xlim(70, 130) +
  geom_text_repel(aes(label = parameter),  box.padding   = 0.35, point.padding = 0.15, segment.color = 'grey50', size = 3) +
  theme(axis.text.x = element_text(size=12,  colour = "black"), axis.text.y = element_text(size=12,  colour = "black"), axis.ticks = element_line(colour = "black"), panel.background = element_rect(fill= "white"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_rect(fill = NA, colour = "black", size = 1)) 


Sit_And_Pursue.SA2 <- read.csv("Sit-and-Pursue-SA2-LOF2.csv")%>%
  mutate_if(is.logical, as.character)%>% 
  group_by(parameter, index)%>%  
  summarize(value = mean(value))%>%
  pivot_wider(names_from = index, values_from = value)

Sit_And_Pursue.SA2.plot<-ggplot(Sit_And_Pursue.SA2, aes(x=mustar, y=sigma)) + 
  geom_point(shape=22, fill = "black", size=2)+
  ggtitle("Active")+  xlab("mustar") +  ylab("sigma")+ ylim(70, 130)+  xlim(70, 130) +
  geom_text_repel(aes(label = parameter),  box.padding   = 0.35, point.padding = 0.15, segment.color = 'grey50', size = 3) +
  theme(axis.text.x = element_text(size=12,  colour = "black"), axis.text.y = element_text(size=12,  colour = "black"), axis.ticks = element_line(colour = "black"), panel.background = element_rect(fill= "white"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_rect(fill = NA, colour = "black", size = 1)) 


Sit_And_Pursue.SA3 <- read.csv("Sit-and-Pursue-SA2-LOF3.csv")%>%
  mutate_if(is.logical, as.character)%>% 
  group_by(parameter, index)%>%  
  summarize(value = mean(value))%>%
  pivot_wider(names_from = index, values_from = value)

Sit_And_Pursue.SA3.plot<-ggplot(Sit_And_Pursue.SA3, aes(x=mustar, y=sigma)) + 
  geom_point(shape=22, fill = "black", size=2)+
  ggtitle("Active")+  xlab("mustar") +  ylab("sigma")+ ylim(70, 130)+  xlim(70, 130)+
  geom_text_repel(aes(label = parameter),  box.padding   = 0.35, point.padding = 0.15, segment.color = 'grey50', size = 3) +
  theme(axis.text.x = element_text(size=12,  colour = "black"), axis.text.y = element_text(size=12,  colour = "black"), axis.ticks = element_line(colour = "black"), panel.background = element_rect(fill= "white"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_rect(fill = NA, colour = "black", size = 1)) 

quartz()
allplots<-grid.arrange(Sit_And_Pursue.SA.plot, Sit_And_Pursue.SA2.plot, Sit_And_Pursue.SA3.plot, ncol=2, nrow=2) 




#facet wrap====
Active.SA$model<-"Active"
Sit_And_Pursue.SA$model<-"Sit and Persue"
Sit_And_Wait.SA$model<-"Sit and Wait"


all.data<-rbind(Active.SA, Sit_And_Pursue.SA, Sit_And_Wait.SA )  

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
quartz()
allplots<-grid.arrange(Active.SA.plot, Sit_And_Wait.SA.plot, Sit_And_Pursue.SA.plot, ncol=2, nrow=2) 

quartz()
allplots


#Null Models -----
#=====Active======
Active.SA.Null <- read.csv("Active-SA2-NULL.csv")%>%
  mutate_if(is.logical, as.character)%>%
  group_by(parameter, index)%>%
  summarize(value = mean(value))%>%
  pivot_wider(names_from = index, values_from = value)

Active.SA.NULL.plot<-ggplot(Active.SA.Null, aes(x=mustar, y=sigma)) + 
  geom_point(shape=22, fill = "black", size=2)+
  ggtitle("Active")+
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


Active.SA2.Null <- read.csv("Active-SA2-NULL2.csv")%>%
  mutate_if(is.logical, as.character)%>%
  group_by(parameter, index)%>%
  summarize(value = mean(value))%>%
  pivot_wider(names_from = index, values_from = value)

Active.SA2.NULL.plot<-ggplot(Active.SA2.Null, aes(x=mustar, y=sigma)) + 
  geom_point(shape=22, fill = "black", size=2)+
  ggtitle("Active")+
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



Active.SA3.Null <- read.csv("Active-SA2-NULL3.csv")%>%
  mutate_if(is.logical, as.character)%>%
  group_by(parameter, index)%>%
  summarize(value = mean(value))%>%
  pivot_wider(names_from = index, values_from = value)

Active.SA3.NULL.plot<-ggplot(Active.SA3.Null, aes(x=mustar, y=sigma)) + 
  geom_point(shape=22, fill = "black", size=2)+
  ggtitle("Active")+
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




quartz()
allplots<-grid.arrange(Active.SA.NULL.plot, Active.SA2.NULL.plot, Active.SA3.NULL.plot, ncol=2, nrow=2) 




#=====Sit And Wait======
Sit_And_Wait.SA.NULL <- read.csv("Sit-And-Wait-SA2-NULL.csv")%>%
  mutate_if(is.logical, as.character)%>%
  group_by(parameter, index)%>%
  summarize(value = mean(value))%>%
  pivot_wider(names_from = index, values_from = value)

Sit_And_Wait.SA.NULL.plot<-ggplot(Sit_And_Wait.SA.NULL, aes(x=mustar, y=sigma)) + 
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
Sit_And_Pursue.SA.NULL <- read.csv("Sit-And-Persue-SA2-NULL.csv")%>%
  mutate_if(is.logical, as.character)%>%
  group_by(parameter, index)%>%
  summarize(value = mean(value))%>%
  pivot_wider(names_from = index, values_from = value)

Sit_And_Pursue.SA.NULL.plot<-ggplot(Sit_And_Pursue.SA.NULL, aes(x=mustar, y=sigma)) + 
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
Active.SA.Null$model<-"Active"
Sit_And_Pursue.SA.NULL$model<-"Sit and Persue"
Sit_And_Wait.SA.NULL$model<-"Sit and Wait"


all.data<-rbind(Active.SA.Null, Sit_And_Pursue.SA.NULL, Sit_And_Wait.SA.NULL )  

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
quartz()
allplots<-grid.arrange(Active.SA.NULL.plot, Sit_And_Wait.SA.NULL.plot, Sit_And_Pursue.SA.NULL.plot, ncol=2, nrow=2) 

quartz()
allplots




#Plots to use ---- 
Active.SA3 <- read.csv("Active-SA2-LOF3.csv")%>%
  mutate_if(is.logical, as.character)%>% 
  group_by(parameter, index)%>%  
  summarize(value = mean(value))%>%
  pivot_wider(names_from = index, values_from = value)

Active.SA3.plot<-ggplot(Active.SA3, aes(x=mustar, y=sigma)) + 
  geom_point(shape=22, fill = "black", size=2)+
  ggtitle("Active")+  xlab("mustar") +  ylab("sigma")+ #  ylim(0, 50)+  xlim(0, 1100)+
  geom_text_repel(aes(label = parameter),  box.padding   = 0.35, point.padding = 0.15, segment.color = 'grey50', size = 3) +
  theme(axis.text.x = element_text(size=12,  colour = "black"), axis.text.y = element_text(size=12,  colour = "black"), axis.ticks = element_line(colour = "black"), panel.background = element_rect(fill= "white"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_rect(fill = NA, colour = "black", size = 1)) 

Active.SA3.plot
