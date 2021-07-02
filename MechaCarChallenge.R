library(dplyr)
library(tidyverse)
MechaCar <- read.csv('../Desktop/DataBootcamp/R_Analysis/MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)
mechalin <- lm(mpg~vehicle_weight+vehicle_length+spoiler_angle+ground_clearance+AWD,data=MechaCar)
summary(mechalin)

SuspenCoil <- read.csv('../Desktop/DataBootcamp/R_Analysis/Suspension_Coil.csv',check.names=F,stringsAsFactors = F)
total_summary <- SuspenCoil %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI)) 
lot_summary <- SuspenCoil %>% group_by(Manufacturing_Lot)%>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI))

t.test(SuspenCoil$PSI,mu = 1500)
t.test(subset(SuspenCoil,Manufacturing_Lot=="Lot1")$PSI,mu = 1500)
t.test(subset(SuspenCoil,Manufacturing_Lot=="Lot2")$PSI,mu = 1500)
t.test(subset(SuspenCoil,Manufacturing_Lot=="Lot3")$PSI,mu = 1500)