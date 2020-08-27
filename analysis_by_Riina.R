rm(list = ls())
#klaispeets; minugithubipassword
user = "riina" # Kirjuta siia "heli" asemele, siis võtab sul õige kausta lahti
if(user == "riina"){setwd("~/work/Heli/heli herring")}else{setwd("~/Desktop/Herring_data/heli_herring-master")}
load("consolidated_herring.RData")

#Kasutame alguses ainult neid ridu kus koik muutujad olemas
d = data[which(data$complete == "yes"),]
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

#Create 2D image of the model
library(colorRamps)
var = d$R
SSB0 = d$SSB0
wa = d$wa
m = gam(var ~ te(SSB0, wa))
SSB0 = seq(min(d$SSB0), max(d$SSB0), length.out = 100)
wa = seq(min(d$wa), max(d$wa), length.out = 100)
predicted = matrix(ncol=length(SSB0),nrow = length(wa))
for(i in 1:length(wa)){ 
  new.data = data.frame(SSB0,wa[i])
  names(new.data)<-c("SSB0","wa")
  pred = predict.gam(m, newdata = new.data)
  predicted[,i] = pred}

par(mfrow = c(1,1))
image(predicted,col=matlab.like(20),axes=F , xlab="Winter harshness", ylab = "SSB")
contour(predicted,levels = c(1000,1500,2000,2500,3000,3500),add=T)

labels_wa = as.numeric(c(round(quantile(wa, c(0.2,0.4,0.6,0.8)), digits = 0)))
labels_SSB = as.numeric(c(round(quantile(SSB0, c(0.2,0.4,0.6,0.8)), digits = 0)))

axis(1, at = c(0.2,0.4,0.6,0.8), labels = labels_wa)
axis(2, at = c(0.2,0.4,0.6,0.8), labels = labels_SSB)




#Step2
summary(gam(R ~ te(wa, SSB0, k = 4) + te(N, SSB0, k = 4), data = d))#0.42, marignally sign.
summary(gam(R ~ te(wa, SSB0, k = 4) + te(E1, SSB0, k = 4), data = d))#NS
summary(gam(R ~ te(wa, SSB0, k = 4) + te(E2, SSB0, k = 4), data = d))#NS
summary(gam(R ~ te(wa, SSB0, k = 4) + te(E3, SSB0, k = 4), data = d))#NS

#Step3: food in interaction with wa
summary(gam(R ~ te(wa, SSB0, k = 4) + te(N, wa, k = 4), data = d))#NS
summary(gam(R ~ te(wa, SSB0, k = 4) + te(E1, wa, k = 4), data = d))#NS
summary(gam(R ~ te(wa, SSB0, k = 4) + te(E2, wa, k = 4), data = d))#NS
summary(gam(R ~ te(wa, SSB0, k = 4) + te(E3, wa, k = 4), data = d))#NS

#Step 4: interaction between N and E3
summary(gam(R ~ te(wa, SSB0, k = 4) + te(N, E3, k = 4), data = d))#Rsq adj 0.49 (from 0.39), but te(E3,N) is only marginally significant (0.07)

#Create 2D images of te(wa, SSB0) and te(N, E3)
var = d$R
SSB0 = d$SSB0
wa = d$wa
N = d$N
E3 = d$E3
m = gam(var ~ te(SSB0, wa) + te(N, E3))
SSB0 = seq(min(SSB0), max(SSB0), length.out = 100)
wa = seq(min(wa), max(wa), length.out = 100)
predicted = matrix(ncol=length(SSB0),nrow = length(wa))

for(i in 1:length(wa)){ 
  new.data = data.frame(SSB0,wa[i], mean(N), mean(E3))
  names(new.data)<-c("SSB0","wa", "N", "E3")
  pred = predict.gam(m, newdata = new.data)
  predicted[,i] = pred}

par(mfrow = c(2,1))
image(predicted,col=matlab.like(20),axes=F , xlab="Winter harshness", ylab = "SSB")
contour(predicted,levels = c(1000,1500,2000,2500,3000,3500),add=T)
labels_wa = as.numeric(c(round(quantile(wa, c(0.2,0.4,0.6,0.8)), digits = 0)))
labels_SSB = as.numeric(c(round(quantile(SSB0, c(0.2,0.4,0.6,0.8)), digits = 0)))
axis(1, at = c(0.2,0.4,0.6,0.8), labels = labels_wa)
axis(2, at = c(0.2,0.4,0.6,0.8), labels = labels_SSB)

var = d$R
SSB0 = d$SSB0
wa = d$wa
N = d$N
E3 = d$E3
m = gam(var ~ te(SSB0, wa) + te(N, E3))
N = seq(min(N), max(N), length.out = 100)
E3 = seq(min(E3), max(E3), length.out = 100)
predicted = matrix(ncol=length(N),nrow = length(E3))

for(i in 1:length(E3)){ 
  new.data = data.frame(N,E3[i], mean(SSB0), mean(wa))
  names(new.data)<-c("N", "E3","SSB0","wa")
  pred = predict.gam(m, newdata = new.data)
  predicted[,i] = pred}

image(predicted,col=matlab.like(20),axes=F , xlab="E3", ylab = "N")
contour(predicted,levels = c(1000,1500,2000,2500,3000,3500),add=T)
labels_N = as.numeric(c(round(quantile(N, c(0.2,0.4,0.6,0.8)), digits = 0)))
labels_E3 = as.numeric(c(round(quantile(E3, c(0.2,0.4,0.6,0.8)), digits = 0)))
axis(1, at = c(0.2,0.4,0.6,0.8), labels = labels_E3)
axis(2, at = c(0.2,0.4,0.6,0.8), labels = labels_N)



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

#Which model had better out of sample predictionn skill:
cor(newdata$R, newdata$oos_R1)
cor(newdata$R, newdata$oos_R2)

range(newdata$corr1)
range(newdata$corr2)

plot(corr1 ~ year, ylim = c(0.82,1), data = newdata, main = "Model 1", xlab = "Year", ylab = "Correlation with full data model")
plot(corr2 ~ year,ylim = c(0.82,1), data = newdata, main = "Model 2", xlab = "Year", ylab = "Correlation with full data model")

#Influential years:
newdata$year[which(newdata$corr2 < 0.9)]
#1982, 2006

par(mfrow = c(1,1))
plot(R ~ year, data = newdata, pch = 16)
lines(R~year, data = newdata)
lines(oos_R1~year, data = newdata, col = 2)
lines(oos_R2~year, data= newdata, col = 3)
text(c(1960,1960,1960), c(7000,6300,5600), labels = c("Observed", "Model 1", "Model 2"), col = c(1,2,3), pos = 4)


#Test whether the correlation between R and SSB0 depends on the mean level of SSB0
type = "chronological" # alternative: "chronological" #lisasin siia } juurde, muidu jookseb kinni (Heli)
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
par(new=T)
plot(newdata$SSB0 ~ newdata$year, type = "line", col = grey(0.5), axes = F, ylab = "", xlab = "")
axis(4);mtext("Mean SSB", side = 4,  line = 1.5, cex = 0.8, col = grey(0.5))

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
