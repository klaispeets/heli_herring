rm(list=ls())
#klaispeets; minugihubipassword
user = "riina"
if(user == "riina"){setwd("~/work/Heli/heli herring")}
load("consolidated_herring.RData")

#Kasutame alguses ainult neid ridu kus koik muutujad olemas
d = herring[which(herring$complete == "yes"),]


plot(R~year, data = d)
library(mgcv)

full_m = gam(R ~ s(SSB1, k = 4)
             + s(SSB0, k = 4)
             + s(sun0, k = 4)
             + s(E1_0, k = 4)
             + s(E2_0, k = 4)
             + s(E3_0, k = 4)
             + s(L_0, k = 4)
             + s(wa_prev_0, k = 4)
             + s(wa_1, k = 4)
             + s(may_0, k = 4)
             + s(summer_0, k = 4),
             data = d)

summary(full_m)

par(mfrow=c(3,4), mar = c(2.5,2.5,1,1), mgp = c(1.5,0.5,0))
plot(full_m)

