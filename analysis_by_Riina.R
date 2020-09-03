rm(list = ls())
#klaispeets; minugithubipassword
user = "riina" # Kirjuta siia "heli" asemele, siis võtab sul õige kausta lahti
if(user == "riina"){setwd("~/work/Heli/heli herring")}else{setwd("~/Desktop/Herring_data/heli_herring-master")}
load("consolidated_herring.RData")

library(colorRamps)
library(mgcv)

#Kasutame alguses ainult neid ridu kus koik muutujad olemas
d = final[which(final$complete == "yes"),]

#Selle rea pidin lisama, kuna ma tegin veidi data_prep skripti ümber; ja nüüd osad tulbanimed olid teised kui edasises skriptis
names(d) = c("year","R","SSB","sun","N","E1","E2","E3","N_2","E1_2", "E2_2", "E3_2", "open_N",   "open_E1",  "open_E2",  "open_E3",  "wa","may_june", "complete")

d$N[which(d$year == 2001)] = mean(d$N[which(d$year %in% c(2000,2002))])
d$N_2[which(d$year == 2001)] = mean(d$N_2[which(d$year %in% c(2000,2002))])
d$E2_2[which(d$year == 1985)] = mean(d$E2_2[which(d$year %in% c(1984,1985,1986))])

#Aegread (siin ei muutnud midagi)

par(mfrow = c(3, 3), tck=-0.02,mar=c(2.5,2.5,2.5,2.5), mgp = c(1.3,0.3,0))
plot(d$E1 ~ d$year, ylab=expression(paste("Abudance (log ind. ",m**-2,")")), xlab="Year", pch = 16)
lines(d$E1 ~ d$year)
mtext("E1", side=3, adj=0, cex= 0.6)
plot(d$E2 ~ d$year, ylab=expression(paste("Abudance (log ind. ",m**-2,")")), xlab="Year", pch = 16)
lines(d$E2 ~ d$year)
mtext("E2", side=3, adj=0, cex= 0.6)
plot(d$E3 ~ d$year, ylab=expression(paste("Abudance (log ind. ",m**-2,")")), xlab="Year", pch = 16)
lines(d$E3 ~ d$year)
mtext("E3", side=3, adj=0, cex= 0.6)
plot(d$open_E1 ~ d$year, ylab=expression(paste("Abudance (log ind. ",m**-2,")")), xlab="Year", pch = 16)
lines(d$open_E1 ~ d$year)
mtext("open_E1", side=3, adj=0, cex= 0.6)
plot(d$open_E2 ~ d$year, ylab=expression(paste("Abudance (log ind. ",m**-2,")")), xlab="Year", pch = 16)
lines(d$open_E2 ~ d$year)
mtext("open_E2", side=3, adj=0, cex= 0.6)
plot(d$open_E3 ~ d$year, ylab=expression(paste("Abudance (log ind. ",m**-2,")")), xlab="Year", pch = 16)
lines(d$open_E3 ~ d$year)
mtext("open_E3", side=3, adj=0, cex= 0.6)
plot(d$N ~ d$year, ylab=expression(paste("Abudance (log ind. ",m**-2,")")), xlab="Year", pch = 16)
lines(d$N ~ d$year)
mtext("N", side=3, adj=0, cex= 0.6)
plot(d$open_N ~ d$year, ylab=expression(paste("Abudance (log ind. ",m**-2,")")), xlab="Year", pch = 16)
lines(d$open_N ~ d$year)
mtext("open_N", side=3, adj=0, cex= 0.6)

par(mfrow = c(1, 3), tck=-0.02,mar=c(2.5,2.5,2.5,2.5), mgp = c(1.3,0.3,0))
plot(d$wa ~ d$year, ylab="winter severity", xlab="Year", pch = 16)
lines(d$wa ~ d$year)
mtext("wa", side=3, adj=0, cex= 0.6)
plot(d$sun ~ d$year, ylab="sun hours", xlab="Year", pch = 16)
lines(d$sun ~ d$year)
mtext("sun", side=3, adj=0, cex= 0.6)
plot(d$may_june ~ d$year, ylab="temperature", xlab="Year", pch = 16)
lines(d$may_june ~ d$year)
mtext("may_june temp", side=3, adj=0, cex= 0.6)


#Siin ma nüüd testi ükshaaval läbi kõik erinevad toidukategooriad, sh rannikumere toit 2 variandis: mai juuli perioodi peale arvutatud
#(nii nagu seni); või mai-juuni peale (lühem periood, tähistatud kui N_2, E1_2, jne)
#lisaks proovin kõiki toiduobjekte log skaalal (nii nagu seni), ja naturaalsel skaala (exponenti võttes)
#Panin kõigi katsetuste tulemused siia:
#https://docs.google.com/spreadsheets/d/1iDOtbrB-3WCmL4VLl3T0OUsVn2o2LrAl9Otki7g89kI/edit#gid=0

library(mgcv)
summary(gam(R ~ te((N), SSB, k = 4), data = d))$r.sq
summary(gam(R ~ te(exp(N), SSB, k = 4), data = d))$r.sq  # Natural scale
summary(gam(R ~ te((N_2), SSB, k = 4), data = d))$r.sq
summary(gam(R ~ te(exp(N_2), SSB, k = 4), data = d))$r.sq

summary(gam(R ~ te((E1), SSB, k = 4), data = d))$r.sq
summary(gam(R ~ te(exp(E1), SSB, k = 4), data = d))$r.sq
summary(gam(R ~ te((E1_2), SSB, k = 4), data = d))$r.sq
summary(gam(R ~ te(exp(E1_2), SSB, k = 4), data = d))$r.sq


summary(gam(R ~ te((E2), SSB, k = 4), data = d))$r.sq
summary(gam(R ~ te(exp(E2), SSB, k = 4), data = d))$r.sq
summary(gam(R ~ te((E2_2), SSB, k = 4), data = d))$r.sq
summary(gam(R ~ te(exp(E2_2), SSB, k = 4), data = d))$r.sq


summary(gam(R ~ te((E3), SSB, k = 4), data = d))$r.sq
summary(gam(R ~ te(exp(E3), SSB, k = 4), data = d))$r.sq
summary(gam(R ~ te((E3_2), SSB, k = 4), data = d))$r.sq
summary(gam(R ~ te(exp(E3_2), SSB, k = 4), data = d))$r.sq

summary(gam(R ~ te((open_N), SSB, k = 4), data = d))$r.sq
summary(gam(R ~ te(exp(open_N), SSB, k = 4), data = d))$r.sq

summary(gam(R ~ te((open_E1), SSB, k = 4), data = d))$r.sq
summary(gam(R ~ te(exp(open_E1), SSB, k = 4), data = d))$r.sq

summary(gam(R ~ te((open_E2), SSB, k = 4), data = d))$r.sq
summary(gam(R ~ te(exp(open_E2), SSB, k = 4), data = d))$r.sq

summary(gam(R ~ te((open_E3), SSB, k = 4), data = d))$r.sq
summary(gam(R ~ te(exp(open_E3), SSB, k = 4), data = d))$r.sq

#Nagu sa ise õigesti märkasid, andis parima tulemuse tegelikult open_E2 (log skaalal).
#Seega on korrektne jätkata seda esimesena lisades.
#Kõigepealt joonis vaatamaks kuidas see 2D space välja näeb kus meil on SSB ja open_E2 interaktsioonis:


var = d$R
SSB = d$SSB
open_E2 = d$open_E2
m = gam(var ~ te(SSB, open_E2))
SSB = seq(min(d$SSB), max(d$SSB), length.out = 100)
open_E2 = seq(min(d$open_E2), max(d$open_E2), length.out = 100)
predicted = matrix(ncol=length(SSB),nrow = length(open_E2))
for(i in 1:length(open_E2)){ 
  new.data = data.frame(SSB,open_E2[i])
  names(new.data)<-c("SSB","open_E2")
  pred = predict.gam(m, newdata = new.data)
  predicted[,i] = pred}
par(mfrow = c(1,1))
image(predicted,col=matlab.like(20),axes=F , xlab="open E2 (log-scale)", ylab = "SSB")
contour(predicted,levels = c(1000,1500,2000,2500,3000,3500),add=T)
labels_open_E2 = as.numeric(c(round(quantile(open_E2, c(0.2,0.4,0.6,0.8)), digits = 0)))
labels_SSB = as.numeric(c(round(quantile(SSB, c(0.2,0.4,0.6,0.8)), digits = 0)))
axis(1, at = c(0.2,0.4,0.6,0.8), labels = labels_open_E2)
axis(2, at = c(0.2,0.4,0.6,0.8), labels = labels_SSB)

#Proovime kohe ka kui hea on selle mudeli out of sample prediction skill:

newdata = subset(d, select = c("year","R", "SSB", "open_E2"))
m1 = gam(R ~ te(SSB, open_E2, k = 4), data = newdata)
newdata$pred_m1 = predict(m1)
newdata$oos_R1 = NA
newdata$corr1 = NA
for(i in 1:53){
  # train new model leaving out the i'th year
  m1 = gam(R ~ te(SSB, open_E2, k = 4), data = newdata[-i,]) 
  # Predict R for all years, including for the year that was left out ("out of sample prediction")
  pred1 = predict(m1, newdata = newdata)  
  newdata$oos_R1[i] = pred1[i]
  newdata$corr1[i] = cor(newdata$pred_m1, pred1)
}

plot(R~year, data = newdata, xlab ="Year", ylab = "R", pch = 16)
lines(R~year, data = newdata)
points(oos_R1~year, data = newdata, col = 2, pch = 16)
lines(oos_R1~year, data = newdata, col = 2, pch = 16)
plot(R ~ open_E2, data = newdata)
summary(gam(R ~ s(open_E2, k = 4), data = newdata))
#Päris hea kusjuures

#Step2 - siin peaks jätkama sama loogikaga nagu varasemalt - proovima kõigepealt milline teine muutuja interaktsioonis SSB-ga võiks järgmisena lisatud saada
summary(gam(R ~ te(open_E2, SSB, k = 4) + te(wa, SSB, k = 4), data = d))#NS
summary(gam(R ~ te(open_E2, SSB, k = 4) + te(sun, SSB, k = 4), data = d)) # 0.56
summary(gam(R ~ te(open_E2, SSB, k = 4) + te(may_june, SSB, k = 4), data = d)) # NS

#Proovime ka parimat varianti rannikumere toidust, milleks oli mai-juuni E3 naturaalsel skaalal
summary(gam(R ~ te(open_E2, SSB, k = 4) + te(exp(E3_2), SSB, k = 4), data = d)) # 0.63

#Tundub nagu siseneks rannikumere toit järgmisena (enne päikest, talvekarmust jms)

#Create 2D images of te(open_E2, SSB0) and te(exp(E3_2), SSB)
var = d$R
SSB = d$SSB
open_E2 = d$open_E2
E3_2 = exp(d$E3_2)
m = gam(var ~ te(open_E2, SSB) + te(E3_2, SSB))
SSB = seq(min(SSB), max(SSB), length.out = 100)
open_E2 = seq(min(open_E2), max(open_E2), length.out = 100)
predicted = matrix(ncol=length(SSB),nrow = length(open_E2))

for(i in 1:length(open_E2)){ 
  new.data = data.frame(SSB,open_E2[i], mean(E3_2))
  names(new.data)<-c("SSB","open_E2", "E3_2")
  pred = predict.gam(m, newdata = new.data)
  predicted[,i] = pred}

par(mfrow = c(2,1))
image(predicted,col=matlab.like(20),axes=F , xlab="Open E2", ylab = "SSB")
contour(predicted,levels = c(1000,1500,2000,2500,3000,3500),add=T)
labels_open_E2 = as.numeric(c(round(quantile(open_E2, c(0.2,0.4,0.6,0.8)), digits = 0)))
labels_SSB = as.numeric(c(round(quantile(SSB, c(0.2,0.4,0.6,0.8)), digits = 0)))
axis(1, at = c(0.2,0.4,0.6,0.8), labels = labels_open_E2)
axis(2, at = c(0.2,0.4,0.6,0.8), labels = labels_SSB)

var = d$R
SSB = d$SSB
E3_2 = exp(d$E3_2)
open_E2 = d$open_E2
m = gam(var ~ te(open_E2, SSB) + te(E3_2,SSB))
E3_2 = seq(min(E3_2), max(E3_2), length.out = 100)
SSB = seq(min(SSB), max(SSB), length.out = 100)
predicted = matrix(ncol=length(E3_2),nrow = length(SSB))

for(i in 1:length(E3_2)){ 
  new.data = data.frame(SSB,E3_2[i], mean(open_E2))
  names(new.data)<-c("SSB", "E3_2","open_E2")
  pred = predict.gam(m, newdata = new.data)
  predicted[,i] = pred}

image(predicted,col=matlab.like(20),axes=F , xlab="Natural scale E3, may-june", ylab = "SSB")
contour(predicted,levels = c(1000,1500,2000,2500,3000,3500),add=T)
labels_E3 = as.numeric(c(round(quantile(E3_2, c(0.2,0.4,0.6,0.8)), digits = 0)))
labels_SSB = as.numeric(c(round(quantile(SSB, c(0.2,0.4,0.6,0.8)), digits = 0)))
axis(1, at = c(0.2,0.4,0.6,0.8), labels = labels_E3)
axis(2, at = c(0.2,0.4,0.6,0.8), labels = labels_SSB)


newdata = subset(d, select = c("year","R", "SSB", "open_E2", "E3_2"))
newdata$E3_2 = exp(newdata$E3_2)
m1 = gam(R ~ te(SSB, open_E2, k = 4)+te(E3_2, SSB, k = 4), data = newdata)
newdata$pred_m1 = predict(m1)
newdata$oos_R1 = NA
newdata$corr1 = NA

for(i in 1:53){
  # train new model leaving out the i'th year
  m1 = gam(R ~ te(SSB, open_E2, k = 4) + te(E3_2, SSB, k = 4), data = newdata[-i,]) 
  # Predict R for all years, including for the year that was left out ("out of sample prediction")
  pred1 = predict(m1, newdata = newdata)  
  newdata$oos_R1[i] = pred1[i]
  newdata$corr1[i] = cor(newdata$pred_m1, pred1)
}

range(newdata$corr1)
summary(lm(R~oos_R1, data = newdata))#Ainult 0.29 - mmis tähendab et kuigi mudel nagu seletaks hästi, ei ole out of sample prediction ja stability kuigi head enam.


#Siit edasi on juba vana skript. Põhimõtteliselt võiks järgmise sammuna jätta E3_2 kõrvale (kuna out of sample prediction test ütles et see pole väga hea muutuja),
#ja proovida mõne muu muutujaga, nt te(sun, SSB), või hoopis te(sun, wa).





#####
#####




#Step3: food in interaction with wa
#summary(gam(R ~ te(wa, SSB, k = 4) + te(N, wa, k = 4), data = d))#NS
#summary(gam(R ~ te(wa, SSB, k = 4) + te(E1, wa, k = 4), data = d))#NS
#summary(gam(R ~ te(wa, SSB, k = 4) + te(E2, wa, k = 4), data = d))#NS
#summary(gam(R ~ te(wa, SSB, k = 4) + te(E3, wa, k = 4), data = d))#NS
summary(gam(R ~ te(wa, SSB, k = 4) + te(open_N, wa, k = 4), data = d))#NS
summary(gam(R ~ te(wa, SSB, k = 4) + te(open_E1, wa, k = 4), data = d))#NS
summary(gam(R ~ te(wa, SSB, k = 4) + te(open_E2, wa, k = 4), data = d))#S 0.499
summary(gam(R ~ te(wa, SSB, k = 4) + te(open_E3, wa, k = 4), data = d))#NS
summary(gam(R ~ te(wa, SSB, k = 4) + te(sun, wa, k = 4), data = d))#S 0.51
summary(gam(R ~ te(wa, SSB, k = 4) + te(may_june, wa, k = 4), data = d))#NS


#Step 4: interaction between N and E3
#summary(gam(R ~ te(wa, SSB, k = 4) + te(N, E3, k = 4), data = d))#Rsq adj 0.49 (from 0.39), but te(E3,N) is only marginally significant (0.07)
summary(gam(R ~ te(wa, SSB, k = 4) + te(open_N, open_E3, k = 4), data = d))#NS
summary(gam(R ~ te(wa, SSB, k = 4) + te(open_E2, open_N, k = 4), data = d))# S 0.537
summary(gam(R ~ te(wa, SSB, k = 4) + te(open_E2, sun, k = 4), data = d)) #S 0.563
summary(gam(R ~ te(wa, SSB, k = 4) + te(open_E2, may_june, k = 4), data = d))

#Create 2D images of te(wa, SSB0) and te(sun, open_E2)
var = d$R
SSB = d$SSB
wa = d$wa
sun = d$sun
E2 = d$open_E2
m = gam(var ~ te(SSB, wa) + te(sun, E2))
SSB = seq(min(SSB), max(SSB), length.out = 100)
wa = seq(min(wa), max(wa), length.out = 100)
predicted = matrix(ncol=length(SSB),nrow = length(wa))

for(i in 1:length(wa)){ 
  new.data = data.frame(SSB,wa[i], mean(sun), mean(E2))
  names(new.data)<-c("SSB","wa", "sun", "E2")
  pred = predict.gam(m, newdata = new.data)
  predicted[,i] = pred}

par(mfrow = c(2,1))
image(predicted,col=matlab.like(20),axes=F , xlab="Winter harshness", ylab = "SSB")
contour(predicted,levels = c(1000,1500,2000,2500,3000,3500),add=T)
labels_wa = as.numeric(c(round(quantile(wa, c(0.2,0.4,0.6,0.8)), digits = 0)))
labels_SSB = as.numeric(c(round(quantile(SSB, c(0.2,0.4,0.6,0.8)), digits = 0)))
axis(1, at = c(0.2,0.4,0.6,0.8), labels = labels_wa)
axis(2, at = c(0.2,0.4,0.6,0.8), labels = labels_SSB)

var = d$R
SSB = d$SSB
wa = d$wa
sun = d$sun
E2 = d$open_E2
m = gam(var ~ te(SSB, wa) + te(sun, E2))
sun = seq(min(sun), max(sun), length.out = 100)
E2 = seq(min(E2), max(E2), length.out = 100)
predicted = matrix(ncol=length(sun),nrow = length(E2))

for(i in 1:length(E2)){ 
  new.data = data.frame(sun,E2[i], mean(SSB), mean(wa))
  names(new.data)<-c("sun", "E2","SSB","wa")
  pred = predict.gam(m, newdata = new.data)
  predicted[,i] = pred}

image(predicted,col=matlab.like(20),axes=F , xlab="open_E2", ylab = "sun")
contour(predicted,levels = c(1000,1500,2000,2500,3000,3500),add=T)
labels_sun = as.numeric(c(round(quantile(sun, c(0.2,0.4,0.6,0.8)), digits = 0)))
labels_E2 = as.numeric(c(round(quantile(E2, c(0.2,0.4,0.6,0.8)), digits = 0)))
axis(1, at = c(0.2,0.4,0.6,0.8), labels = labels_E2)
axis(2, at = c(0.2,0.4,0.6,0.8), labels = labels_sun)


#Create 2D images of te(wa, SSB0) and te(N, E3)
var = d$R
SSB = d$SSB
wa = d$wa
N = d$N
E3 = d$E3
m = gam(var ~ te(SSB, wa) + te(N, E3))
SSB = seq(min(SSB), max(SSB), length.out = 100)
wa = seq(min(wa), max(wa), length.out = 100)
predicted = matrix(ncol=length(SSB),nrow = length(wa))

for(i in 1:length(wa)){ 
  new.data = data.frame(SSB,wa[i], mean(N), mean(E3))
  names(new.data)<-c("SSB","wa", "N", "E3")
  pred = predict.gam(m, newdata = new.data)
  predicted[,i] = pred}

par(mfrow = c(2,1))
image(predicted,col=matlab.like(20),axes=F , xlab="Winter harshness", ylab = "SSB")
contour(predicted,levels = c(1000,1500,2000,2500,3000,3500),add=T)
labels_wa = as.numeric(c(round(quantile(wa, c(0.2,0.4,0.6,0.8)), digits = 0)))
labels_SSB = as.numeric(c(round(quantile(SSB, c(0.2,0.4,0.6,0.8)), digits = 0)))
axis(1, at = c(0.2,0.4,0.6,0.8), labels = labels_wa)
axis(2, at = c(0.2,0.4,0.6,0.8), labels = labels_SSB)

var = d$R
SSB = d$SSB
wa = d$wa
N = d$N
E3 = d$E3
m = gam(var ~ te(SSB, wa) + te(N, E3))
N = seq(min(N), max(N), length.out = 100)
E3 = seq(min(E3), max(E3), length.out = 100)
predicted = matrix(ncol=length(N),nrow = length(E3))

for(i in 1:length(E3)){ 
  new.data = data.frame(N,E3[i], mean(SSB), mean(wa))
  names(new.data)<-c("N", "E3","SSB","wa")
  pred = predict.gam(m, newdata = new.data)
  predicted[,i] = pred}

image(predicted,col=matlab.like(20),axes=F , xlab="E3", ylab = "N")
contour(predicted,levels = c(1000,1500,2000,2500,3000,3500),add=T)
labels_N = as.numeric(c(round(quantile(N, c(0.2,0.4,0.6,0.8)), digits = 0)))
labels_E3 = as.numeric(c(round(quantile(E3, c(0.2,0.4,0.6,0.8)), digits = 0)))
axis(1, at = c(0.2,0.4,0.6,0.8), labels = labels_E3)
axis(2, at = c(0.2,0.4,0.6,0.8), labels = labels_N)

#New_steps with open_E2

#open_E2
var = d$R
SSB = d$SSB
open_E2 = d$open_E2
m = gam(var ~ te(SSB, open_E2))
SSB = seq(min(d$SSB), max(d$SSB), length.out = 100)
open_E2 = seq(min(d$open_E2), max(d$open_E2), length.out = 100)
predicted = matrix(ncol=length(SSB),nrow = length(open_E2))
for(i in 1:length(open_E2)){ 
  new.data = data.frame(SSB,open_E2[i])
  names(new.data)<-c("SSB","open_E2")
  pred = predict.gam(m, newdata = new.data)
  predicted[,i] = pred}

par(mfrow = c(1,1))
image(predicted,col=matlab.like(20),axes=F , xlab="open_E2", ylab = "SSB")
contour(predicted,levels = c(1000,1500,2000,2500,3000,3500),add=T)

labels_open_E2 = as.numeric(c(round(quantile(open_E2, c(0.2,0.4,0.6,0.8)), digits = 0)))
labels_SSB = as.numeric(c(round(quantile(SSB, c(0.2,0.4,0.6,0.8)), digits = 0)))

axis(1, at = c(0.2,0.4,0.6,0.8), labels = labels_open_E2)
axis(2, at = c(0.2,0.4,0.6,0.8), labels = labels_SSB)




#Out of sample prediction ability and influential years
#Aim of next step is to identify which model is the best in out of sample prediction category, and whether there are some years that seem to affect the model most

newdata = subset(d, select = c("year","R", "SSB", "wa", "N","E1","E2","E3"))
#2 model candidates
m1 = gam(R ~ te(SSB, wa, k = 4), data = newdata)
m2 = gam(R ~ te(SSB, wa, k = 4) +  te(N, E3, k = 4) , data = newdata)

newdata$pred_m1 = predict(m1)
newdata$pred_m2 = predict(m2)
newdata$oos_R1 = NA
newdata$oos_R2 = NA
newdata$corr1 = NA
newdata$corr2 = NA

for(i in 1:53){
  # train new models leaving out year i
  m1 = gam(R ~ te(SSB, wa, k = 4), data = newdata[-i,]) 
  m2 = gam(R ~ te(SSB, wa, k = 4) +  te(N, E3, k = 4) , data = newdata[-i,])
  
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

if(TRUE){
  #Test whether the correlation between R and SSB depends on the mean level of SSB
  type = "chronological" # alternative: "chronological" #lisasin siia } juurde, muidu jookseb kinni (Heli)
  if(type=="SSB"){newdata = newdata[order(newdata$SSB),]}else{newdata = newdata[order(newdata$year),]}
  if(type=="SSB"){xlabel = "Mean SSB"}else{xlabel = "Middle year"}
  meanSSB = rep(NA,39)
  slope = rep(NA, 39)
  p = rep(NA, 39)
  for(i in 1:39){
    idx = c(i: (i+14))
    m = lm(R ~ SSB, data = newdata[idx,])
    slope[i] = summary(m)$coefficients[2,1]
    p[i] = summary(m)$coefficients[2,4]
    if(type=="SSB"){meanSSB[i] = round(mean(newdata$SSB[idx]), digits=0)}else{meanSSB[i] = round(mean(newdata$year[idx]), digits=0)}
  }
  
  
  par(mfrow = c(3, 2), tck=-0.02,mar=c(2.5,2.5,2.5,2.8), mgp = c(1.3,0.3,0))
  plot(slope ~ meanSSB, xlab = xlabel, ylab = "Slope of lm(R ~ SSB)")
  idx = which(p<0.05)
  points(slope[idx]~meanSSB[idx], pch = 16)
  abline(h = 0, lwd = 0.5)
  mtext(" a)", side = 3, adj = 0, line = -1.5, cex = 0.9)
  par(new=T)
  plot(newdata$SSB ~ newdata$year, type = "line", col = grey(0.5), axes = F, ylab = "", xlab = "")
  axis(4);mtext("Mean SSB", side = 4,  line = 1.5, cex = 0.8, col = grey(0.5))
  
  #WA
  slope = rep(NA, 39)
  p = rep(NA, 39)
  meanWA = rep(NA,39)
  for(i in 1:39){
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
  slope = rep(NA, 39)
  p = rep(NA, 39)
  meanN = rep(NA,39)
  for(i in 1:39){
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
  slope = rep(NA, 39)
  p = rep(NA, 39)
  meanE1 = rep(NA,39)
  for(i in 1:39){
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
  plot(meanE1 ~ meanSSB, type = "line", col = grey(0.5), axes = F, ylab = "", xlab = "")
  axis(4);mtext("Mean E1", side = 4,  line = 1.5, cex = 0.8)
  
  #E2
  slope = rep(NA, 39)
  p = rep(NA, 39)
  meanE2 = rep(NA,39)
  for(i in 1:39){
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
  slope = rep(NA, 39)
  p = rep(NA, 39)
  meanE3 = rep(NA,39)
  for(i in 1:39){
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
}


##Open
newdata = subset(d, select = c("year","R", "SSB", "wa", "open_N","open_E1","open_E2","open_E3", "sun", "may_june"))


if(TRUE){
  #Test whether the correlation between R and SSB depends on the mean level of SSB
  type = "SSB" # alternative: "chronological" #lisasin siia } juurde, muidu jookseb kinni (Heli)
  if(type=="SSB"){newdata = newdata[order(newdata$SSB),]}else{newdata = newdata[order(newdata$year),]}
  if(type=="SSB"){xlabel = "Mean SSB"}else{xlabel = "Middle year"}
  meanSSB = rep(NA,39)
  slope = rep(NA, 39)
  p = rep(NA, 39)
  for(i in 1:39){
    idx = c(i: (i+14))
    m = lm(R ~ SSB, data = newdata[idx,])
    slope[i] = summary(m)$coefficients[2,1]
    p[i] = summary(m)$coefficients[2,4]
    if(type=="SSB"){meanSSB[i] = round(mean(newdata$SSB[idx]), digits=0)}else{meanSSB[i] = round(mean(newdata$year[idx]), digits=0)}
  }
  
  
  par(mfrow = c(3, 3), tck=-0.02,mar=c(2.5,2.5,2.5,2.8), mgp = c(1.3,0.3,0))
  plot(slope ~ meanSSB, xlab = xlabel, ylab = "Slope of lm(R ~ SSB)")
  idx = which(p<0.05)
  points(slope[idx]~meanSSB[idx], pch = 16)
  abline(h = 0, lwd = 0.5)
  mtext(" a)", side = 3, adj = 0, line = -1.5, cex = 0.9)
  #par(new=T)
  #plot(newdata$SSB ~ newdata$year, type = "line", col = grey(0.5), axes = F, ylab = "", xlab = "")
  #axis(4);mtext("Mean SSB", side = 4,  line = 1.5, cex = 0.8, col = grey(0.5))
  
  
  #N
  slope = rep(NA, 39)
  p = rep(NA, 39)
  meanN = rep(NA,39)
  for(i in 1:39){
    idx = c(i: (i+14))
    m = lm(R ~ open_N, data = newdata[idx,])
    slope[i] = summary(m)$coefficients[2,1]
    p[i] = summary(m)$coefficients[2,4]
    meanN[i] = round(mean(newdata$open_N[idx]), digits=2)}
  
  plot(slope ~ meanSSB, xlab = xlabel, ylab = "Slope of lm(R ~ O_N)")
  idx = which(p<0.05)
  points(slope[idx]~meanSSB[idx], pch = 16)
  abline(h = 0, lwd = 0.5)
  mtext(" b)", side = 3, adj = 0, line = -1.5, cex = 0.9)
  par(new=T)
  plot(meanN ~ meanSSB, type = "line", col = grey(0.5), axes = F, ylab = "", xlab = "")
  axis(4);mtext("Mean N", side = 4,  line = 1.5, cex = 0.8)
  
  #E1
  slope = rep(NA, 39)
  p = rep(NA, 39)
  meanE1 = rep(NA,39)
  for(i in 1:39){
    idx = c(i: (i+14))
    m = lm(R ~ open_E1, data = newdata[idx,])
    slope[i] = summary(m)$coefficients[2,1]
    p[i] = summary(m)$coefficients[2,4]
    meanE1[i] = round(mean(newdata$open_E1[idx]), digits=2)}
  
  plot(slope ~ meanSSB, xlab = xlabel, ylab = "Slope of lm(R ~ O_E1)")
  idx = which(p<0.05)
  points(slope[idx]~meanSSB[idx], pch = 16)
  abline(h = 0, lwd = 0.5)
  mtext(" c)", side = 3, adj = 0, line = -1.5, cex = 0.9)
  par(new=T)
  plot(meanE1 ~ meanSSB, type = "line", col = grey(0.5), axes = F, ylab = "", xlab = "")
  axis(4);mtext("Mean E1", side = 4,  line = 1.5, cex = 0.8)
  
  #E2
  slope = rep(NA, 39)
  p = rep(NA, 39)
  meanE2 = rep(NA,39)
  for(i in 1:39){
    idx = c(i: (i+14))
    m = lm(R ~ open_E2, data = newdata[idx,])
    slope[i] = summary(m)$coefficients[2,1]
    p[i] = summary(m)$coefficients[2,4]
    meanE2[i] = round(mean(newdata$open_E2[idx]), digits=2)}
  
  plot(slope ~ meanSSB, xlab = xlabel, ylab = "Slope of lm(R ~ O_E2)")
  idx = which(p<0.05)
  points(slope[idx]~meanSSB[idx], pch = 16)
  abline(h = 0, lwd = 0.5)
  mtext(" d)", side = 3, adj = 0, line = -1.5, cex = 0.9)
  par(new=T)
  plot(meanE2 ~ meanSSB, type = "line", col = grey(0.5), axes = F, ylab = "", xlab = "")
  axis(4);mtext("Mean E2", side = 4,  line = 1.5, cex = 0.8)
  
  
  #E3
  slope = rep(NA, 39)
  p = rep(NA, 39)
  meanE3 = rep(NA,39)
  for(i in 1:39){
    idx = c(i: (i+14))
    m = lm(R ~ open_E3, data = newdata[idx,])
    slope[i] = summary(m)$coefficients[2,1]
    p[i] = summary(m)$coefficients[2,4]
    meanE3[i] = round(mean(newdata$open_E3[idx]), digits=2)}
  
  plot(slope ~ meanSSB, xlab = xlabel, ylab = "Slope of lm(R ~ O_E3)")
  idx = which(p<0.05)
  points(slope[idx]~meanSSB[idx], pch = 16)
  abline(h = 0, lwd = 0.5)
  mtext(" e)", side = 3, adj = 0, line = -1.5, cex = 0.9)
  par(new=T)
  plot(meanE3 ~ meanSSB, type = "line", col = grey(0.5), axes = F, ylab = "", xlab = "")
  axis(4);mtext("Mean E3", side = 4,  line = 1.5, cex = 0.8)
  
  #sun
  slope = rep(NA, 39)
  p = rep(NA, 39)
  mean_sun = rep(NA,39)
  for(i in 1:39){
    idx = c(i: (i+14))
    m = lm(R ~ sun, data = newdata[idx,])
    slope[i] = summary(m)$coefficients[2,1]
    p[i] = summary(m)$coefficients[2,4]
    mean_sun[i] = round(mean(newdata$sun[idx]), digits=2)}
  
  plot(slope ~ meanSSB, xlab = xlabel, ylab = "Slope of lm(R ~ sun)")
  idx = which(p<0.05)
  points(slope[idx]~meanSSB[idx], pch = 16)
  abline(h = 0, lwd = 0.5)
  mtext(" f)", side = 3, adj = 0, line = -1.5, cex = 0.9)
  par(new=T)
  plot(mean_sun ~ meanSSB, type = "line", col = grey(0.5), axes = F, ylab = "", xlab = "")
  axis(4);mtext("Mean sun", side = 4,  line = 1.5, cex = 0.8)
  
  #may_june
  slope = rep(NA, 39)
  p = rep(NA, 39)
  mean_summer = rep(NA,39)
  for(i in 1:39){
    idx = c(i: (i+14))
    m = lm(R ~ may_june, data = newdata[idx,])
    slope[i] = summary(m)$coefficients[2,1]
    p[i] = summary(m)$coefficients[2,4]
    mean_summer[i] = round(mean(newdata$may_june[idx]), digits=2)}
  
  plot(slope ~ meanSSB, xlab = xlabel, ylab = "Slope of lm(R ~ may_june)")
  idx = which(p<0.05)
  points(slope[idx]~meanSSB[idx], pch = 16)
  abline(h = 0, lwd = 0.5)
  mtext(" g)", side = 3, adj = 0, line = -1.5, cex = 0.9)
  par(new=T)
  plot(mean_summer ~ meanSSB, type = "line", col = grey(0.5), axes = F, ylab = "", xlab = "")
  axis(4);mtext("Mean temperature", side = 4,  line = 1.5, cex = 0.8)
  
  #WA
  slope = rep(NA, 39)
  p = rep(NA, 39)
  meanWA = rep(NA,39)
  for(i in 1:39){
    idx = c(i: (i+14))
    m = lm(R ~ wa, data = newdata[idx,])
    slope[i] = summary(m)$coefficients[2,1]
    p[i] = summary(m)$coefficients[2,4]
    meanWA[i] = round(mean(newdata$wa[idx]), digits=2)}
  
  plot(slope ~ meanSSB, xlab = xlabel, ylab = "Slope of lm(R ~ wa)")
  idx = which(p<0.05)
  points(slope[idx]~meanSSB[idx], pch = 16)
  abline(h = 0, lwd = 0.5)
  mtext(" h)", side = 3, adj = 0, line = -1.5, cex = 0.9)
  par(new=T)
  plot(meanWA ~ meanSSB, type = "line", col = grey(0.5), axes = F, ylab = "", xlab = "")
  axis(4);mtext("Mean wa", side = 4,  line = 1.5, cex = 0.8)
}
