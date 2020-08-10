rm(list=ls())
#klaispeets; minugithubipassword
user = "riina"
if(user == "riina"){setwd("~/work/Heli/heli herring")}
load("consolidated_herring.RData")

#Kasutame alguses ainult neid ridu kus koik muutujad olemas
d = data[which(data$complete == "yes"),]

plot(R~year, data = d)
library(mgcv)

#Kas toidu arengustaadiumid on omavahel seotud?
pairs(d[,c(5:8)])
pairs(d[,c(9:12)]) #Avaosas enam mitte nii väga
