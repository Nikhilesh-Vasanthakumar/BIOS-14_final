#Code used for analyzing mountain goat hunting data.
#13-01-2022 BIOS-14
data = read.table("exam2022_part2.txt", header=T)#Reading thr data

library(dplyr)#Used to filter the data
library(car)#Used for TukeyHSD
library(ggpubr)#Used to plot II-dimensional Anova Plots.

library(ggplot2)#Used to plot linear model

hornavg=(data$hornL+data$hornR)/2 #Finding the average length of the goat horns

#Model1
m1=lm(mass~age,data=data)
ggplot(data, aes(x = age, y = mass)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red") #Linear Regression Plot for identifying relationship
#Filtering data based on sex
M=filter(data,data$sex=="M")
F=filter(data,data$sex=="F")


#Model2
#ANOVA for bodymass and sex of the mountain goats
m2=lm(log(mass)~sex,data=data)
#Boxplot to visualize ANOVA results
boxplot(M$mass,F$mass,names=c("Male","Female"),ylab="Bodymass in Kg",las=1,col=c("red","sienna"),main="Mean difference in Bodymass")
##ANOVA for Average Hornlength and sex of the mountain goats
m3=lm(log(hornavg)~sex,data=data)
#Boxplot to visualize ANOVA results
boxplot((M$hornL+M$hornR)/2,(F$hornL+F$hornR)/2,names=c("Male","Female"),ylab="Length in mm",las=1,col=c("red","sienna"),main="Mean difference in Hornlength")


density1=as.factor(data$density)#Initializing Population density as a Factor
#Two way ANOVA to analyse the effect of sex and density on mass.
m4=aov(mass~sex*density,data=data)
ggline(data,x="sex",y="mass",col="density",
       add=c("mean_se","jitter"),
       ylab="Bodymass in Kg",xlab="Gender",
       legend.title="Population Density",legend="right")
TukeyHSD(m4) #To get the mean differences from the model

season1=as.factor(data$season)#Initializing the Hunting season as factors
#ANOVA to analyse effect of sex and season on average horn length
m5=aov(hornavg~sex+season1,data=data) 
ggline(data,x="sex",y="hornL",col="season",
       add=c("mean_se","jitter"),
       ylab="Hornlength in mm",xlab="Gender",
       legend.title="Year",legend="right")
TukeyHSD(m5)
#ANOVA to analyse effect of sex and season on Body mass
m6=aov(mass~sex+season1,data=data)
ggline(data,x="sex",y="mass",col="season",
       add=c("mean_se","jitter"),
       ylab="Bodymass in Kg",xlab="Gender",
       legend.title="Year",legend="right")
TukeyHSD(m6)