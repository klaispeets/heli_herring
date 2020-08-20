rm(list = ls())
#klaispeets; minugithubipassword
user = "riina" # Kirjuta siia "heli" asemele, siis võtab sul õige kausta lahti
if(user == "riina"){setwd("~/work/Heli/heli herring")}else{setwd("~/Desktop/Herring_data/heli_herring-master")}
load("consolidated_herring.RData")

#Kasutame alguses ainult neid ridu kus koik muutujad olemas
d = data[which(data$complete == "yes"),]

#Kas toidu arengustaadiumid on omavahel seotud?
pairs(d[,c(5:8)])
pairs(d[,c(9:12)]) #Avaosas enam mitte nii väga

d = d[which(d$year %in% c(1957:2015)),]

d$N[which(d$year == 2002)] = mean(d$N[which(d$year %in% c(2001,2003))])
d$E2[which(d$year == 1986)] = mean(d$E2[which(d$year %in% c(1985,1986,1987))])
d$E1[which(d$year == 1986)] = mean(d$E1[which(d$year %in% c(1985,1986, 1987))])

#Aegread

par(mfrow = c(3, 2), tck=-0.02,mar=c(2.5,2.5,2.5,2.5), mgp = c(1.3,0.3,0))

plot(d$E1 ~ d$year, ylab=expression(paste("Abudance (log ind. ",m**-2,")")), xlab="Year", pch = 16)
lines(d$E1 ~ d$year)
mtext("E1", side=3, adj=0, cex= 0.6)

plot(d$E2 ~ d$year, ylab=expression(paste("Abudance (log ind. ",m**-2,")")), xlab="Year", pch = 16)
lines(d$E2 ~ d$year)
mtext("E2", side=3, adj=0, cex= 0.6)

plot(d$E3 ~ d$year, ylab=expression(paste("Abudance (log ind. ",m**-2,")")), xlab="Year", pch = 16)
lines(d$E3 ~ d$year)
mtext("E3", side=3, adj=0, cex= 0.6)

plot(d$N ~ d$year, ylab=expression(paste("Abudance (log ind. ",m**-2,")")), xlab="Year", pch = 16)
lines(d$N ~ d$year)
mtext("N", side=3, adj=0, cex= 0.6)

plot(d$wa ~ d$year, ylab="winter severity", xlab="Year", pch = 16)
lines(d$wa ~ d$year)
mtext("wa", side=3, adj=0, cex= 0.6)


#Let's try the GAM models with interaction (tensor product)

library(mgcv)
summary(gam(R ~ te(N, SSB0, k = 4), data = d))#R2adj: 0.29
summary(gam(R ~ te(E1, SSB0, k = 4), data = d))#0.245
summary(gam(R ~ te(E2, SSB0, k = 4), data = d))#0.23
summary(gam(R ~ te(E3, SSB0, k = 4), data = d))#0.23
summary(gam(R ~ te(wa, SSB0, k = 4), data = d))#0.39# best one!

#Step2
summary(gam(R ~ te(wa, SSB0, k = 4) + te(N, SSB0, k = 4), data = d))#0.42, marignally sign.
summary(gam(R ~ te(wa, SSB0, k = 4) + te(E1, SSB0, k = 4), data = d))#NS
summary(gam(R ~ te(wa, SSB0, k = 4) + te(E3, SSB0, k = 4), data = d))#NS

#Step3: food in interaction with wa
summary(gam(R ~ te(wa, SSB0, k = 4) + te(N, wa, k = 4), data = d))#NA
summary(gam(R ~ te(wa, SSB0, k = 4) + te(E1, wa, k = 4), data = d))#NS
summary(gam(R ~ te(wa, SSB0, k = 4) + te(E3, wa, k = 4), data = d))#Rsq adj 0.49 (from 0.39), but te(E3,wa) is only marginally sign

#Step 4: interaction between N and E3
summary(gam(R ~ te(wa, SSB0, k = 4) + te(E3, N, k = 4), data = d))#Rsq adj 0.49 (from 0.39), but te(E3,N) is only marginally significant (0.07)

#Out of sample prediction ability and influential years
#Aim of next step is to identify which model is the best in out of sample prediction category, and whether there are some years that seem to affect the model most

newdata = subset(d, select = c("year","R", "SSB0", "wa", "N","E1","E2","E3"))
#2 model candidates
m1 = gam(R ~ te(SSB0, wa, k = 4), data = newdata)
m2 = gam(R ~ te(SSB0, wa, k = 4) +  te(N, E3, k = 4) , data = newdata)

newdata$pred_m1 = predict(m1)
newdata$pred_m2 = predict(m2)
newdata$oos_R1 = NA
newdata$oos_R2 = NA

newdata$corr1 = NA
newdata$corr2 = NA

for(i in 1:55){
  
  # train new models leaving out year i
  m1 = gam(R ~ te(SSB0, wa, k = 4), data = newdata[-i,]) 
  m2 = gam(R ~ te(SSB0, wa, k = 4) +  te(N, E3, k = 4) , data = newdata[-i,])
  
  #Predict R for all years, incl the year that was left out ("out of sample prediction")
  pred1 = predict(m1, newdata = newdata)  
  pred2 = predict(m2, newdata = newdata)
  
  newdata$oos_R1[i] = pred1[i]
  newdata$oos_R2[i] = pred2[i]
  
  newdata$corr1[i] = cor(newdata$pred_m1, pred1)
  newdata$corr2[i] = cor(newdata$pred_m2, pred2)
  }

range(newdata$corr1)
range(newdata$corr2)

plot(corr1 ~ year, data = newdata)
plot(corr2 ~ year, data = newdata)

#Influential years:
newdata$year[which(newdata$corr2 < 0.9)]
#1982, 2006

#Which model had better out of sample predictionn skill:
summary(lm(R~oos_R1, data = newdata))#0.55
summary(lm(R~oos_R2, data = newdata))#0.16

par(mfrow = c(3,1))
plot(R ~ year, data = newdata, pch = 16)
lines(R~year, data = newdata)
lines(oos_R1~year, data = newdata, col = 2)
lines(oos_R2~year, data= newdata, col = 3)

newdata$corr1[which(newdata$year %in% c(1982,2006))] # these 2 years seemed to be influential only when model included te(N, E3) term



#Test whether the correlation between R and SSB0 depends on the mean level of SSB0
if(T){type = "chronological"} # alternative: "chronological" #lisasin siia } juurde, muidu jookseb kinni (Heli)

if(type=="SSB0"){newdata = newdata[order(newdata$SSB0),]}else{newdata = newdata[order(newdata$year),]}
if(type=="SSB0"){xlabel = "Mean SSB"}else{xlabel = "Middle year"}
  meanSSB = rep(NA,41)
 slope = rep(NA, 41)
 p = rep(NA, 41)
for(i in 1:41){
  idx = c(i: (i+14))
  m = lm(R ~ SSB0, data = newdata[idx,])
  slope[i] = summary(m)$coefficients[2,1]
  p[i] = summary(m)$coefficients[2,4]
  if(type=="SSB0"){meanSSB[i] = round(mean(newdata$SSB0[idx]), digits=0)}else{meanSSB[i] = round(mean(newdata$year[idx]), digits=0)}
}


par(mfrow = c(3, 2), tck=-0.02,mar=c(2.5,2.5,2.5,2.8), mgp = c(1.3,0.3,0))
plot(slope ~ meanSSB, xlab = xlabel, ylab = "Slope of lm(R ~ SSB)")
idx = which(p<0.05)
points(slope[idx]~meanSSB[idx], pch = 16)
abline(h = 0, lwd = 0.5)
mtext(" a)", side = 3, adj = 0, line = -1.5, cex = 0.9)

#WA
slope = rep(NA, 41)
p = rep(NA, 41)
meanWA = rep(NA,41)
for(i in 1:41){
  idx = c(i: (i+14))
  m = lm(R ~ wa, data = newdata[idx,])
  slope[i] = summary(m)$coefficients[2,1]
  p[i] = summary(m)$coefficients[2,4]
  meanWA[i] = round(mean(newdata$wa[idx]), digits=2)}

plot(slope ~ meanSSB, xlab = xlabel, ylab = "Slope of lm(R ~ wa)")
idx = which(p<0.05)
points(slope[idx]~meanSSB[idx], pch = 16)
abline(h = 0, lwd = 0.5)
mtext(" b)", side = 3, adj = 0, line = -1.5, cex = 0.9)
par(new=T)
plot(meanWA ~ meanSSB, type = "line", col = grey(0.5), axes = F, ylab = "", xlab = "")
axis(4);mtext("Mean wa", side = 4,  line = 1.5, cex = 0.8)


#N
slope = rep(NA, 41)
p = rep(NA, 41)
meanN = rep(NA,41)
for(i in 1:41){
  idx = c(i: (i+14))
  m = lm(R ~ N, data = newdata[idx,])
  slope[i] = summary(m)$coefficients[2,1]
  p[i] = summary(m)$coefficients[2,4]
  meanN[i] = round(mean(newdata$N[idx]), digits=2)}

plot(slope ~ meanSSB, xlab = xlabel, ylab = "Slope of lm(R ~ N)")
idx = which(p<0.05)
points(slope[idx]~meanSSB[idx], pch = 16)
abline(h = 0, lwd = 0.5)
mtext(" c)", side = 3, adj = 0, line = -1.5, cex = 0.9)
par(new=T)
plot(meanN ~ meanSSB, type = "line", col = grey(0.5), axes = F, ylab = "", xlab = "")
axis(4);mtext("Mean N", side = 4,  line = 1.5, cex = 0.8)

#E1
slope = rep(NA, 41)
p = rep(NA, 41)
meanE1 = rep(NA,41)
for(i in 1:41){
  idx = c(i: (i+14))
  m = lm(R ~ E1, data = newdata[idx,])
  slope[i] = summary(m)$coefficients[2,1]
  p[i] = summary(m)$coefficients[2,4]
  meanE1[i] = round(mean(newdata$E1[idx]), digits=2)}

plot(slope ~ meanSSB, xlab = xlabel, ylab = "Slope of lm(R ~ E1)")
idx = which(p<0.05)
points(slope[idx]~meanSSB[idx], pch = 16)
abline(h = 0, lwd = 0.5)
mtext(" d)", side = 3, adj = 0, line = -1.5, cex = 0.9)
par(new=T)
plot(meanN ~ meanSSB, type = "line", col = grey(0.5), axes = F, ylab = "", xlab = "")
axis(4);mtext("Mean E1", side = 4,  line = 1.5, cex = 0.8)

#E2
slope = rep(NA, 41)
p = rep(NA, 41)
meanE2 = rep(NA,41)
for(i in 1:41){
  idx = c(i: (i+14))
  m = lm(R ~ E2, data = newdata[idx,])
  slope[i] = summary(m)$coefficients[2,1]
  p[i] = summary(m)$coefficients[2,4]
  meanE2[i] = round(mean(newdata$E2[idx]), digits=2)}

plot(slope ~ meanSSB, xlab = xlabel, ylab = "Slope of lm(R ~ E2)")
idx = which(p<0.05)
points(slope[idx]~meanSSB[idx], pch = 16)
abline(h = 0, lwd = 0.5)
mtext(" e)", side = 3, adj = 0, line = -1.5, cex = 0.9)
par(new=T)
plot(meanE2 ~ meanSSB, type = "line", col = grey(0.5), axes = F, ylab = "", xlab = "")
axis(4);mtext("Mean E2", side = 4,  line = 1.5, cex = 0.8)


#E3
slope = rep(NA, 41)
p = rep(NA, 41)
meanE3 = rep(NA,41)
for(i in 1:41){
  idx = c(i: (i+14))
  m = lm(R ~ E3, data = newdata[idx,])
  slope[i] = summary(m)$coefficients[2,1]
  p[i] = summary(m)$coefficients[2,4]
  meanE3[i] = round(mean(newdata$E3[idx]), digits=2)}

plot(slope ~ meanSSB, xlab = xlabel, ylab = "Slope of lm(R ~ E3)")
idx = which(p<0.05)
points(slope[idx]~meanSSB[idx], pch = 16)
abline(h = 0, lwd = 0.5)
mtext(" f)", side = 3, adj = 0, line = -1.5, cex = 0.9)
par(new=T)
plot(meanE3 ~ meanSSB, type = "line", col = grey(0.5), axes = F, ylab = "", xlab = "")
axis(4);mtext("Mean E3", side = 4,  line = 1.5, cex = 0.8)
#} üleliigne?
