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

#SW
#Kronoloogiline või piki SSB-d?
type = "kronoloogiline"
if(type == "SSB"){newdata = d[order(d$SSB),]}else{newdata = d[order(d$year),]}

cor1 = rep(NA,41) # E1
cor2 = rep(NA,41) # E2
cor3 = rep(NA,41) # E3
cor4 = rep(NA,41) # N
cor5 = rep(NA,41) #open_E1
cor6 = rep(NA,41) #open_E2
cor7 = rep(NA,41) #open_E3
cor8 = rep(NA,41) #open_N
cor9 = rep(NA,41) #wa

mean_R = rep(NA,41)
mean_E1 = rep(NA,41)
mean_E2 = rep(NA,41)
mean_E3 = rep(NA,41)
mean_N = rep(NA,41)
mean_open_E1 = rep(NA,41)
mean_open_E2 = rep(NA,41)
mean_open_E3 = rep(NA,41)
mean_open_N = rep(NA,41)
mean_wa = rep(NA,41)
cy = rep(NA, 41)


for(i in 1:41){
  idx = c(i: (i+14))
  cor1[i] = cor(newdata$E1[idx], newdata$R[idx])
  cor2[i] = cor(newdata$E2[idx], newdata$R[idx])
  cor3[i] = cor(newdata$E3[idx], newdata$R[idx])
  cor4[i] = cor(newdata$N[idx], newdata$R[idx])
  cor5[i] = cor(newdata$open_E1[idx], newdata$R[idx])
  cor6[i] = cor(newdata$open_E2[idx], newdata$R[idx])
  cor7[i] = cor(newdata$open_E3[idx], newdata$R[idx])
  cor8[i] = cor(newdata$open_N[idx], newdata$R[idx])
  cor9[i] = cor(newdata$wa[idx], newdata$R[idx])
  mean_R[i] = mean(newdata$R[idx])
  mean_E1[i] = mean(newdata$E1[idx])
  mean_E2[i] = mean(newdata$E2[idx])
  mean_E3[i] = mean(newdata$E3[idx])
  mean_N[i] = mean(newdata$N[idx])
  mean_open_E1[i] = mean(newdata$open_E1[idx])
  mean_open_E2[i] = mean(newdata$open_E2[idx])
  mean_open_E3[i] = mean(newdata$open_E3[idx])
  mean_open_N[i] = mean(newdata$open_N[idx])
  mean_wa[i] = mean(newdata$wa[idx])
  mean_wa[i] = mean(newdata$wa[idx])
  if(type == "SSB"){cy[i] = round(mean(newdata$SSB[idx]), digits=0)}else{cy[i] = round(mean(newdata$year[idx]), digits=0)}
}

par(mfrow = c(3, 3), tck=-0.02,mar=c(2.5,2.5,2.5,2.5), mgp = c(1.3,0.3,0))

xlabel = ifelse(type =="SSB", "Mean SSB", "Middle year")

if(TRUE){
  plot(cor1 ~ cy, ylab="Pearsons' r", xlab = xlabel, pch = 16)
  lines(cor1 ~ cy)
  abline(h=0)
  abline(v=c(40, 60, 80, 100), lwd =.5, col=rgb(0,0,0,alpha = 0.5))
  par(new = T)
  plot(mean_E1 ~ cy, axes = F, type = "n", ylab = "", xlab ="")
  lines(mean_E1 ~ cy, col = grey(0.5), lwd = 2)
  axis(4)
  mtext("Mean E1 of the window", side = 4, line = 1.3, cex = 0.5, col = grey(0.5))
  mtext("R vs E1", side=3, adj=0, cex= 0.6)
  
  plot(cor2 ~ cy, ylab="Pearsons' r", xlab = xlabel, pch = 16)
  lines(cor2 ~ cy)
  abline(h=0)
  abline(v=c(40, 60, 80, 100), lwd =.5, col=rgb(0,0,0,alpha = 0.5))
  par(new = T)
  plot(mean_E2 ~ cy, axes = F, type = "n", ylab = "", xlab ="")
  lines(mean_E2 ~ cy, col = grey(0.5), lwd = 2)
  axis(4)
  mtext("Mean E2 of the window", side = 4, line = 1.3, cex = 0.5, col = grey(0.5))
  mtext("R vs E2", side=3, adj=0, cex= 0.6)
  
  plot(cor3 ~ cy, ylab="Pearsons' r", xlab = xlabel, pch = 16)
  lines(cor3 ~ cy)
  abline(h=0)
  abline(v=c(40, 60, 80, 100), lwd =.5, col=rgb(0,0,0,alpha = 0.5))
  par(new = T)
  plot(mean_E3 ~ cy, axes = F, type = "n", ylab = "", xlab ="")
  lines(mean_E3 ~ cy, col = grey(0.5), lwd = 2)
  axis(4)
  mtext("Mean E3 of the window", side = 4, line = 1.3, cex = 0.5, col = grey(0.5))
  mtext("R vs E3", side=3, adj=0, cex= 0.6)
  
  plot(cor4 ~ cy, ylab="Pearsons' r", xlab = xlabel, pch = 16)
  lines(cor4 ~ cy)
  abline(h=0)
  abline(v=c(40, 60, 80, 100), lwd =.5, col=rgb(0,0,0,alpha = 0.5))
  par(new = T)
  plot(mean_N ~ cy, axes = F, type = "n", ylab = "", xlab ="")
  lines(mean_N ~ cy, col = grey(0.5), lwd = 2)
  axis(4)
  mtext("Mean N of the window", side = 4, line = 1.3, cex = 0.5, col = grey(0.5))
  mtext("R vs N", side=3, adj=0, cex= 0.6)
  
  plot(cor5 ~ cy, ylab="Pearsons' r", xlab = xlabel, pch = 16)
  lines(cor5 ~ cy)
  abline(h=0)
  abline(v=c(40, 60, 80, 100), lwd =.5, col=rgb(0,0,0,alpha = 0.5))
  par(new = T)
  plot(mean_open_E1 ~ cy, axes = F, type = "n", ylab = "", xlab ="")
  lines(mean_open_E1 ~ cy, col = grey(0.5), lwd = 2)
  axis(4)
  mtext("Mean open_E1 of the window", side = 4, line = 1.3, cex = 0.5, col = grey(0.5))
  mtext("R vs open_E1", side=3, adj=0, cex= 0.6)
  
  plot(cor6 ~ cy, ylab="Pearsons' r", xlab = xlabel, pch = 16)
  lines(cor6 ~ cy)
  abline(h=0)
  abline(v=c(40, 60, 80, 100), lwd =.5, col=rgb(0,0,0,alpha = 0.5))
  par(new = T)
  plot(mean_open_E2 ~ cy, axes = F, type = "n", ylab = "", xlab ="")
  lines(mean_open_E2 ~ cy, col = grey(0.5), lwd = 2)
  axis(4)
  mtext("Mean open_E2 of the window", side = 4, line = 1.3, cex = 0.5, col = grey(0.5))
  mtext("R vs open_E2", side=3, adj=0, cex= 0.6)
  
  plot(cor7 ~ cy, ylab="Pearsons' r", xlab = xlabel, pch = 16)
  lines(cor7 ~ cy)
  abline(h=0)
  abline(v=c(40, 60, 80, 100), lwd =.5, col=rgb(0,0,0,alpha = 0.5))
  par(new = T)
  plot(mean_open_E3 ~ cy, axes = F, type = "n", ylab = "", xlab ="")
  lines(mean_open_E3 ~ cy, col = grey(0.5), lwd = 2)
  axis(4)
  mtext("Mean open_E3 of the window", side = 4, line = 1.3, cex = 0.5, col = grey(0.5))
  mtext("R vs open_E3", side=3, adj=0, cex= 0.6)
  
  plot(cor8 ~ cy, ylab="Pearsons' r", xlab = xlabel, pch = 16)
  lines(cor8 ~ cy)
  abline(h=0)
  abline(v=c(40, 60, 80, 100), lwd =.5, col=rgb(0,0,0,alpha = 0.5))
  par(new = T)
  plot(mean_open_N ~ cy, axes = F, type = "n", ylab = "", xlab ="")
  lines(mean_open_N ~ cy, col = grey(0.5), lwd = 2)
  axis(4)
  mtext("Mean open_N of the window", side = 4, line = 1.3, cex = 0.5, col = grey(0.5))
  mtext("R vs open_N", side=3, adj=0, cex= 0.6)
  
  plot(cor9 ~ cy, ylab="Pearsons' r", xlab = xlabel, pch = 16)
  lines(cor9 ~ cy)
  abline(h=0)
  abline(v=c(40, 60, 80, 100), lwd =.5, col=rgb(0,0,0,alpha = 0.5))
  par(new = T)
  plot(mean_wa ~ cy, axes = F, type = "n", ylab = "", xlab ="")
  lines(mean_wa ~ cy, col = grey(0.5), lwd = 2)
  axis(4)
  mtext("Mean wa of the window", side = 4, line = 1.3, cex = 0.5, col = grey(0.5))
  mtext("R vs wa", side=3, adj=0, cex= 0.6)
}
