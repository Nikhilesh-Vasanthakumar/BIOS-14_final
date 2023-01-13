#Code used for analyzing blossom data of two species of Dalechampia S and L
#13-01-2022 BIOS-14

library(dplyr)#Used to filter data.
library(car)#Used for TukeyHSD.
library(ggpubr) #Used to plot II-dimensional Anova Plots.



data=read.csv("exam2022_part1.csv")
#Filtering given Dataset by species.
S=filter(data,data$sp =='S')  
L=filter(data,data$sp =='L')
#Boxplots used to identify differences of GA and UBW in between species
boxplot(S$GA,L$GA,names=c("Species S","Species L"),ylab="Area in mm^2",las=1,col=c("red","sienna"),main="Box-Plot of the Gland Area ")
boxplot(S$UBW,L$UBW,names=c("Species S","Species L"),ylab="Length in mm",las=1,col=c("red","sienna"),main="Box-Plot of the Upper Bract Width")

#Modelling starts
species=as.factor(data$sp)#Selecting the species as factors.
#Model-Difference in dimensions between Species.
m5=lm(log(UBW)+log(UBL)+log(LBL)+log(LBW)+log(GA)+log(GSD)+log(GAD)~species,data=data)
#Boxplot for the model
boxplot(log(UBW)+log(UBL)+log(LBL)+log(LBW)+log(GA)+log(GSD)+log(GAD)~species,data=data,las=1,col=c("red","sienna"),xlab="Species",ylab="Formula",main="Difference in overall dimension of the two species")


#Check the effects of wet and dry greenhouse
#Initializing them as factors
conditionS=as.factor(S$treat)
conditionL=as.factor(L$treat)

#Model 2-Effect of Treatment on dimension
#For Species S
m6=lm(log(UBW)+log(UBL)+log(LBL)+log(LBW)+log(GA)+log(GSD)+log(GAD)~conditionS,data=S)
#For Species L
m7=lm(log(UBW)+log(UBL)+log(LBL)+log(LBW)+log(GA)+log(GSD)+log(GAD)~conditionL,data=L)

#2Way ANOVA to find the effects of treatment and species on GA and LBW

m8=aov(GA~sp*treat,data=data) #Model for GA
summary(m8)
TukeyHSD(m8)
#Plot for Model
ggline(data,x="sp",y="GA",col="treat",
       add=c("mean_se","jitter"),
       ylab="Gland Area in mm2",xlab="Species",
       legend.title="Treatment",legend="right",
       main="         The effects of Treatment on the Gland Area(GA) of the two species")

m9=aov(UBW~sp*treat,data=data)#Model for UBW
summary(m9)
TukeyHSD(m9)
#plot for the Model
ggline(data,x="sp",y="UBW",col="treat",
       add=c("mean_se","jitter"),
       ylab="Upper Bract Width(in mm)",xlab="Species",
       legend.title="Treatment",legend="right",
       main="The effects of Treatment on the Upper Bract Width(UBW) two species")

