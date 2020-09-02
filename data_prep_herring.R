rm(list=ls())
#Kui võtta nihkeks 1988, siis 2015-1988=27 a; võtta andmed 1960-1987 ja 1988-2015

if(TRUE){
  user = "riina" # sina kirjuta siia "heli", siis laeb sama skript sul ka õigest kohast andmed
  
  if(user == "heli"){load("~/Desktop/HeliWork/raim/GOR herring R analyses/revision_1/script_and_data/ices.RData")}else{load("/Users/riina82/work/Heli/heli herring/ices.RData")}
  
  #Andmed drives https://drive.google.com/drive/folders/1bzRMKy5LHeTGspjqNMaGid1yPtI7Garz?usp=sharing
  
  ices = ices[which(ices$year %in% c(1957:2016)),]
  idx = which(ices$year %in% c(1977:2016))
  
  #andmete ühtlustamine
  ices$R[idx]=ices$R[idx]/1000
  ices$SSB[idx]=ices$SSB[idx]/1000
  
  #SUN
  if(user=="riina"){load("/Users/riina82/work/Heli/heli herring/sun.RData")}else{load("~/Desktop/HeliWork/raim/GOR herring R analyses/revision_1/script_and_data/sun.RData")} #Monthly sun hours data from 1961-2013, table "data"
  if(user=="riina"){load("/Users/riina82/work/Heli/heli herring/daily_sun_hours.RData")}else{load("~/Desktop/HeliWork/raim/GOR herring R analyses/revision_1/script_and_data/daily_sun_hours.RData")}
  
  # table "sun", newer daily data from 1999 - 2016 - recalculate to monthly sums, and then combine with earlier data
  #sun andmed kõige lähedasemad on Pärnu, teised on Saaremaa andmed. 
  
  y = unique(sun$year)
  for (i in 1:length(y)){
    set = sun[which(sun$year == y[i] & sun$location == "parnu"),c(3:14)]
    x=apply(set, 2, sum, na.rm = T)
    if(i == 1){monthly = t(data.frame(x))}else{monthly = rbind(monthly, t(data.frame(x)))}
  }
  
  monthly=data.frame(monthly)
  monthly$annual=as.numeric(apply(monthly,1,sum))
  monthly=data.frame(y,monthly)
  names(monthly)=names(data)
  data=rbind(data,monthly[c(16:18),])
  data$June[which(is.na(data$June))]=mean(data$June[which(!is.na(data$June))])
  data$July[which(is.na(data$July))]=mean(data$July[which(!is.na(data$July))])
  annual=as.numeric(apply(data[,c(2:13)],1,sum))
  data$annual=annual
  sun=data$annual[match(ices$year,data$year)]
  ices$sun = sun
  
  final = ices
  final$Rl = NULL
  ####
  if(user=="riina"){load("/Users/riina82/work/Heli/heli herring/00zoopl_data_V4.RData")}else{load("~/Desktop/HeliWork/raim/GOR herring R analyses/revision_1/script_and_data/00_zooplankton_data_V4.RData")}
  
  #E.affinis ja naupliused pärnu lahest
  
  s = SAMPLE[SAMPLE$subbasin == "Gulf of Riga" & !is.na(SAMPLE$vol) & !is.na(SAMPLE$lat) & SAMPLE$provider == "est",]
  s$month = as.numeric(format(s$Date, "%m"))
  
  
  c = COUNT[COUNT$sampleID %in% s$sampleID & COUNT$legalSp %in% c("Eurytemora affinis", "Copepoda"),]
  c = c[c$stage != "T",]
  c = c[which(c$stage != "parasite"),]
  idx = which(c$legalSp == "Copepoda" & c$stage != "N")
  if(length(idx) > 0){c = c[-idx,]}
  s$jul = as.numeric(format(s$Date, '%j'))
  c$jul = s$jul[match(c$sampleID, s$sampleID)]
  c$vol = s$vol[match(c$sampleID, s$sampleID)]
  c$abund2 = c$vol * c$abund
  xtab = tapply(c$abund2, list(c$sampleID, c$stage), sum)
  xtab[which(is.na(xtab))] = 0
  set = data.frame(rownames(xtab))
  names(set) = "sampleID"
  set$E1 = xtab[,2]+xtab[,3]+xtab[,4] #I-III
  set$E2 = xtab[,5]+xtab[,7] #IV-V
  set$E3 = xtab[,1] #adult 
  set$N = xtab[,6]
  mch = match(set$sampleID,s$sampleID)
  env = subset(s, select = c("lat", "lon", "year", "jul", "month"))
  set = cbind(set, env[mch, ])
  set1 = set[which(set$month %in% c(5,6,7)),]
  set2 = set[which(set$month %in% c(5,6)),]
  
  library(mgcv)
  new.data = data.frame(unique(set1$year))
  names(new.data)="year"
  new.data$jul = mean(set1$jul)
  new.data$lat = mean(set1$lat)
  new.data$lon = mean(set1$lon)
  m = gam(log1p(N) ~ te(lat, lon) + s(jul, k = 4) + as.factor(year), data = set1)
  new.data$predN = predict(m, newdata = new.data)
  m = gam(log1p(E1) ~ te(lat, lon) + s(jul, k = 4) + as.factor(year), data = set1)
  new.data$predE1 = predict(m, newdata = new.data)
  m = gam(log1p(E2) ~ te(lat, lon) + s(jul, k = 4) + as.factor(year), data = set1)
  new.data$predE2 = predict(m, newdata = new.data)
  m = gam(log1p(E3) ~ te(lat, lon) + s(jul, k = 4) + as.factor(year), data = set1)
  new.data$predE3 = predict(m, newdata = new.data)

  new.data$jul = mean(set2$jul)
  new.data$lat = mean(set2$lat)
  new.data$lon = mean(set2$lon)
  m = gam(log1p(N) ~ te(lat, lon) + s(jul, k = 4) + as.factor(year), data = set2)
  new.data$predN_2 = predict(m, newdata = new.data)
  m = gam(log1p(E1) ~ te(lat, lon) + s(jul, k = 4) + as.factor(year), data = set2)
  new.data$predE1_2 = predict(m, newdata = new.data)
  m = gam(log1p(E2) ~ te(lat, lon) + s(jul, k = 4) + as.factor(year), data = set2)
  new.data$predE2_2 = predict(m, newdata = new.data)
  m = gam(log1p(E3) ~ te(lat, lon) + s(jul, k = 4) + as.factor(year), data = set2)
  new.data$predE3_2 = predict(m, newdata = new.data)

  mch = match(final$year, new.data$year)
  final = cbind(final, new.data[mch,c(5:12)])

  par(mfrow=c(2,2))
  plot(log1p(N) ~  year, data = set1, main = "Nauplii: observed vs calculated annual means", col = rgb(0,0,0,alpha = 0.1), pch = 16)
  points(predN~year, data = new.data, col = 2, pch = 16)
  points(predN_2~year, data = new.data, col = 3, pch = 16)
  text(c(1955,1955), c(20,18), labels = c("Based on May-July", "Based on May-June"), col = c(2,3), pos = 4, cex = .8)

  plot(log1p(E1) ~  year, data = set1, main = "E1: observed vs calculated annual means", col = rgb(0,0,0,alpha = 0.1), pch = 16)
  points(predE1~year, data = new.data, col = 2, pch = 16)
  points(predE1_2~year, data = new.data, col = 3, pch = 16)
  text(c(1955,1955), c(20,18), labels = c("Based on May-July", "Based on May-June"), col = c(2,3), pos = 4, cex = .8)
  
  plot(log1p(E2) ~  year, data = set1, main = "E2: observed vs calculated annual means", col = rgb(0,0,0,alpha = 0.1), pch = 16)
  points(predE2~year, data = new.data, col = 2, pch = 16)
  points(predE2_2~year, data = new.data, col = 3, pch = 16)
  text(c(1955,1955), c(19.5,18), labels = c("Based on May-July", "Based on May-June"), col = c(2,3), pos = 4, cex = .8)
  

  plot(log1p(E3) ~  year, data = set1, main = "E3: observed vs calculated annual means", col = rgb(0,0,0,alpha = 0.1), pch = 16)
  points(predE3~year, data = new.data, col = 2, pch = 16)
  points(predE3_2~year, data = new.data, col = 3, pch = 16)
  text(c(1955,1955), c(19.5,18), labels = c("Based on May-July", "Based on May-June"), col = c(2,3), pos = 4, cex = .8)
  
  #Eurytemora affinis from open gulf (Gunta)
  s = SAMPLE[SAMPLE$subbasin == "Gulf of Riga" & !is.na(SAMPLE$vol) & !is.na(SAMPLE$lat) & SAMPLE$provider == "Gunta Rubene",]
  s$month = as.numeric(format(s$Date, "%m"))
  
  c = COUNT[which(COUNT$sampleID %in% s$sampleID),]
  c = c[c$stage != "T",]
  c = c[c$stage != "t",]
  idx = which(c$legalSp == "Eurytemora affinis" | c$stage == "N")
  c = c[idx,]
  
  s$jul = as.numeric(format(s$Date, '%j'))
  c$jul = s$jul[match(c$sampleID, s$sampleID)]
  c$vol = s$vol[match(c$sampleID, s$sampleID)]
  c$abund2 = c$vol * c$abund
  xtab = tapply(c$abund2, list(c$sampleID, c$stage), sum)
  xtab[which(is.na(xtab))] = 0
  set = data.frame(rownames(xtab))
  names(set) = "sampleID"
  set$E1 = xtab[,2]+xtab[,3]+xtab[,4] #I-III
  set$E2 = xtab[,5]+xtab[,7] #IV-V
  set$E3 = xtab[,1] #adult 
  set$N = xtab[,6]
  
  mch = match(set$sampleID,s$sampleID)
  env = subset(s, select = c("lat", "lon", "year", "jul", "month"))
  set = cbind(set, env[mch, ])
  set = set[which(set$month %in% c(5)),] #ainult mai kuu 
  
  ######predicted mean abundance in May & June

  new.data = data.frame(unique(set$year))
  names(new.data)="year"
  new.data$jul = mean(set$jul)
  new.data$lat = mean(set$lat)
  new.data$lon = mean(set$lon)
  
  m = gam(log1p(N) ~ te(lat, lon) + s(jul, k = 4) + as.factor(year), data = set)
  new.data$open_N = predict(m, newdata = new.data)

  m = gam(log1p(E1) ~ te(lat, lon) + s(jul, k = 4) + as.factor(year), data = set)
  new.data$open_E1 = predict(m, newdata = new.data)

  m = gam(log1p(E2) ~ te(lat, lon) + s(jul, k = 4) + as.factor(year), data = set)
  new.data$open_E2 = predict(m, newdata = new.data)

  m = gam(log1p(E3) ~ te(lat, lon) + s(jul, k = 4) + as.factor(year), data = set)
  new.data$open_E3 = predict(m, newdata = new.data)

  #####
  mch = match(final$year, new.data$year)
  final = cbind(final, new.data[mch, c(5:8)])
  
  
  ##################
  #WA ja temperatuur
  if(user=="riina"){load("/Users/riina82/work/Heli/heli herring/Air_temp_Liivi laht.RData")}else{load("~/Desktop/HeliWork/raim/GOR herring R analyses/revision_1/script_and_data/Air_temp_Liivi laht.RData")}
  for(i in 3:14){
    set = data[,c(1,2,i,15)] 
    names(set) = c("year","day","air_temp","station")
    set = subset(set, !is.na(air_temp)) 
    set$month = i-2
    set$Date = as.Date(paste(set$year, set$day, set$month, sep = "-"), "%Y-%d-%m")
    if(i == 3){SET = set} else {SET = rbind(SET, set)}}
  
  SET$year[which(SET$month %in% c(11,12))] = SET$year[which(SET$month %in% c(11,12))] + 1
  SET = SET[which(SET$station %in% c("kihnu") & SET$month %in% c(11,12,1:4)),]
  SET = SET[which(SET$air_temp < 0),]
  idx=which(SET$year %in% c(1957:2016))
  wa = tapply(SET$air_temp[idx], as.factor(SET$year[idx]), sum)
  final$wa=wa[match(final$year, names(wa))]
  if(TRUE){
    if(user=="riina"){load("/Users/riina82/work/Heli/heli herring/Air_temp_Liivi laht.RData")}else{load("~/Desktop/HeliWork/raim/GOR herring R analyses/revision_1/script_and_data/Air_temp_Liivi laht.RData")}
    for(i in 3:14){
      set = data[,c(1,2,i,15)] 
      names(set) = c("year","day","air_temp","station")
      set = subset(set, !is.na(air_temp)) 
      set$month = i-2
      set$Date = as.Date(paste(set$year, set$day, set$month, sep = "-"), "%Y-%d-%m")
      if(i == 3){SET = set} else {SET = rbind(SET, set)}}
    
    SET = SET[which(SET$station %in% c("kihnu") & SET$month %in% c(5, 6)),]
    idx=which(SET$month==5 & SET$year %in% c(1957:2016))
    may_june = tapply(SET$air_temp[idx], as.factor(SET$year[idx]), mean, na.rm=T)
    #idx=which(SET$month %in% c(6,7,8) & SET$year %in% c(1957:2016))
    #summer = tapply(SET$air_temp[idx], as.factor(SET$year[idx]), mean)
    final$may_june = may_june[match(final$year, names(may_june))]
  }
}

#Shift R one year earlier, so that the value of one year olds would be assigned to the year of actual birth
final$R = c(final$R[2:nrow(final)], NA)
final$complete = apply(is.na(final[,c(2:ncol(final)),]), 1, sum)
final$complete = ifelse(final$complete==0, "yes", "no")

save(final, file = "consolidated_herring.RData")
