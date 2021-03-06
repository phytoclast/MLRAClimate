MLRAname <- 'mlra'
MLRA <- '98A'
selectClim <- subset(Norms2010,
                     Latitude >= -90 &
                       Latitude <= 90 &
                       Longitude >= -180 &
                       Longitude <= 180 &
                       LRU %in% MLRA)

#savedselect <- StationMeans
#saveRDS(savedselect, 'data/savedselect.RDS')
selecty <- mean(selectClim$Latitude)
selectx <- mean(selectClim$Longitude)
selectz <- mean(selectClim$Elevation)
selectClim2 <- subset(Norms2010,
                      Latitude >= selecty - 5 &
                        Latitude <= selecty + 5 &
                        Longitude >= selectx - 7 &
                        Longitude <= selectx + 7
                      )
selectClim2$wts <- 1/(((selectClim2$Latitude - selecty)^2 + (selectClim2$Longitude - selectx)^2)^0.5+5)
selectClim2$LRU <- 'other'
selectClim$wts <- 1
selectClim <- rbind(selectClim, selectClim2)
rm(selectClim2)
rm(savedselect)
#Make Monthly Rows. Added criteria to exclude external data.
#Jan
selectMonthly <- selectClim[selectClim$wts >=1,c("LRU","Station_ID","State","Latitude","Longitude","Elevation","t01","tl01","pp01")]
colnames(selectMonthly) <- c("LRU","Station_ID","State","Latitude","Longitude","Elevation","t","tl","p")
selectMonthly$Month<- 1

#Feb-Dec
for (i in 1:11){
  
  selectMonthlyA <- selectClim[selectClim$wts >=1,c("LRU","Station_ID","State","Latitude","Longitude","Elevation",
                                  colnames(selectClim)[which(colnames(selectClim)=='t01')+i],
                                  colnames(selectClim)[which(colnames(selectClim)=='tl01')+i],
                                  colnames(selectClim)[which(colnames(selectClim)=='pp01')+i])]
  colnames(selectMonthlyA)<- c("LRU","Station_ID","State","Latitude","Longitude","Elevation","t","tl","p")
  selectMonthlyA$Month<- i+1
  selectMonthly <- rbind(selectMonthly, selectMonthlyA)
}
rm(selectMonthlyA)
sumMonthly <- ddply(selectMonthly, "Month", summarise,Lat = mean(Latitude),Lon = mean(Longitude),Elev = mean(Elevation),
                    t25 = quantile(t, 0.25), t75 = quantile(t, 0.75), t = mean(t),
                    p25 = quantile(p, 0.25), p75 = quantile(p, 0.75), p = mean(p), 
                    tl = mean(tl))
sumMonthly$th <- sumMonthly$t*2 - sumMonthly$tl
sumMonthly <- merge(sumMonthly, DaysMonth, by.x='Month',by.y='Month_')

sumMonthly$hs = acos(pmin(pmax(-tan(sumMonthly$Lat/360*2*3.141592) * tan(sumMonthly$declination),-1),1))


sumMonthly$Ra  = 
  117.5 * (sumMonthly$hs*sin(sumMonthly$Lat/360*2*3.141592)*sin(sumMonthly$declination) +
             cos(sumMonthly$Lat/360*2*3.141592)*cos(sumMonthly$declination)*sin(sumMonthly$hs)) / 3.141592

sumMonthly$e <- 0.008404*
  216.7*exp(17.26939*
              sumMonthly$t/(sumMonthly$t+237.3))/
  (sumMonthly$t+273.3)*
  (sumMonthly$Ra)*sumMonthly$Days*
  abs((sumMonthly$th - sumMonthly$tl))^0.5 + 0.001
sumMonthly <- subset(sumMonthly, select = -c(Day_, Days, hs, Ra, declination))
sumMonthly$a <- pmin(sumMonthly$e, sumMonthly$p)
sumMonthly$b <- (sumMonthly$t > 0)*sumMonthly$t 
peakAET <- max(sumMonthly$a)
AET <- sum(sumMonthly$a)
PET <- sum(sumMonthly$e)
MAP <- sum(sumMonthly$p)
Surplus <- max(MAP - AET, 0)
Deficit <- max(PET - AET, 0)
PPETRatio<-MAP/(PET+0.0001)
MAAT <- mean(sumMonthly$t)
SummerBioT <- max(mean(sumMonthly[c(1:4,11:12),'b']), mean(sumMonthly[c(5:10),'b']))
Tc <- min(sumMonthly$t)
Tcl <- min(sumMonthly$tl)
Tw <- max(sumMonthly$t)
Twh <- max(sumMonthly$th)
Lat <- mean(sumMonthly$Lat)
Lon <- mean(sumMonthly$Lon)
Elev <- mean(sumMonthly$Elev)

#calculate extreme winter low----
pacificsouth <- 1/((((Lat - -22.7)/13)^2 + ((Lon - -82.3)/14)^2)^2+1)
amazon2 <- 1/((((Lat - -10.2)/5)^2 + ((Lon - -59.9)/10)^2)^2+1)
amazon1 <- 1/((((Lat - -2.8)/14)^2 + ((Lon - -61.3)/19)^2)^2+1)
pacificcent <- 1/((((Lat - 4.1)/21)^2 + ((Lon - -122.4)/41)^2)^2+1)
mexico <- 1/((((Lat - 26)/6)^2 + ((Lon - -98.4)/12)^2)^2+1)
florida <- 1/((((Lat - 27.5)/4)^2 + ((Lon - -81.1)/8)^2)^2+1)
pacificnorth <- 1/((((Lat - 32.9)/26)^2 + ((Lon - -145)/27)^2)^2+1)
oklahoma <- 1/((((Lat - 33.6)/4)^2 + ((Lon - -98.4)/8)^2)^2+1)
arizona <- 1/((((Lat - 34)/12)^2 + ((Lon - -113.1)/8)^2)^2+1)
atlantic <- 1/((((Lat - 34)/15)^2 + ((Lon - -60.7)/19)^2)^2+1)
himalayas <- 1/((((Lat - 35.3)/6)^2 + ((Lon - 91.3)/13)^2)^2+1)
kentucky <- 1/((((Lat - 38.5)/3)^2 + ((Lon - -87.6)/9)^2)^2+1)
detroit <- 1/((((Lat - 41.8)/3)^2 + ((Lon - -82.6)/4)^2)^2+1)
ontario <- 1/((((Lat - 44.6)/2)^2 + ((Lon - -79.2)/6)^2)^2+1)
montana <- 1/((((Lat - 45.4)/5)^2 + ((Lon - -111.8)/10)^2)^2+1)
minn <- 1/((((Lat - 47.6)/6)^2 + ((Lon - -92.6)/12)^2)^2+1)
hudson <- 1/((((Lat - 60)/7)^2 + ((Lon - -87)/34)^2)^2+1)
siberia <- 1/((((Lat - 61.2)/20)^2 + ((Lon - 105.7)/39)^2)^2+1)
california <- 1/((((Lat - 34.8)/9)^2 + ((Lon - -128.2)/9)^2)^2+1)
washington <- 1/((((Lat - 46)/5)^2 + ((Lon - -126.6)/5)^2)^2+1)
colorado <- 1/((((Lat - 38.3)/2)^2 + ((Lon - -108.8)/3)^2)^2+1)
hawaii <- 1/((((Lat - 21.3)/7)^2 + ((Lon - -157.5)/11)^2)^2+1)
chess <- 1/((((Lat - 37)/3)^2 + ((Lon - -74)/3)^2)^2+1)

Tclx<-	-9.171	+
  Tcl *	1.202	+
  Lat *	-0.04149	+
  Elev *	0.0008691	+
  Lat * Elev *	-0.00002455	+
  pacificsouth *	-1.792	+
  amazon2 *	2.573	+
  amazon1 *	-1.014	+
  pacificcent *	-0.749	+
  mexico *	-0.8227	+
  florida *	-3.557	+
  pacificnorth *	-1.246	+
  oklahoma *	0.1758	+
  arizona *	2.605	+
  chess *	0.8347	+
  atlantic *	0.2967	+
  himalayas *	-1.814	+
  kentucky *	-2.644	+
  detroit *	0	+
  ontario *	-2.314	+
  montana *	-4.415	+
  minn *	1.136	+
  hudson *	-5.154	+
  siberia *	-3.797	+
  california *	4.48	+
  washington *	3.597	+
  colorado *	1.458	+
  hawaii *	6.673	

rm(pacificsouth,amazon1,amazon2, pacificcent, mexico, florida, pacificnorth, oklahoma, arizona, atlantic, himalayas, kentucky, detroit, ontario, montana, minn, hudson, siberia, california, washington, colorado, chess, hawaii)



####-------------
StationMeans <- aggregate(selectClim[,c("t01", "t02", "t03", "t04", "t05", "t06",
                                        "t07", "t08", "t09", "t10", "t11", "t12",
                                        "tl01", "tl02", "tl03", "tl04", "tl05", "tl06",
                                        "tl07", "tl08", "tl09", "tl10", "tl11", "tl12",
                                        "pp01", "pp02", "pp03", "pp04", "pp05", "pp06",
                                        "pp07", "pp08", "pp09", "pp10", "pp11","pp12","wts")],
                          by=list(selectClim$LRU, selectClim$Station_ID, selectClim$Station_Name, selectClim$State, selectClim$Latitude, selectClim$Longitude, selectClim$Elevation), FUN='mean')
colnames(StationMeans)[1:7] <- c('LRU', 'Station_ID', 'Station_Name', 'State', 'Latitude', 'Longitude', 'Elevation')
StationMeans$b01 <- 0
StationMeans$b02 <- 0
StationMeans$b03 <- 0
StationMeans$b04 <- 0
StationMeans$b05 <- 0
StationMeans$b06 <- 0
StationMeans$b07 <- 0
StationMeans$b08 <- 0
StationMeans$b09 <- 0
StationMeans$b10 <- 0
StationMeans$b11 <- 0
StationMeans$b12 <- 0
for (i in 0:11){
  StationMeans[,which(colnames(StationMeans)=='b01')+i]  <- StationMeans[,which(colnames(StationMeans)=='t01')+i]*
    (StationMeans[,which(colnames(StationMeans)=='t01')+i]>0)*1
}




StationMeans$th01 <- StationMeans$t01*2-StationMeans$tl01
StationMeans$th02 <- StationMeans$t02*2-StationMeans$tl02
StationMeans$th03 <- StationMeans$t03*2-StationMeans$tl03
StationMeans$th04 <- StationMeans$t04*2-StationMeans$tl04
StationMeans$th05 <- StationMeans$t05*2-StationMeans$tl05
StationMeans$th06 <- StationMeans$t06*2-StationMeans$tl06
StationMeans$th07 <- StationMeans$t07*2-StationMeans$tl07
StationMeans$th08 <- StationMeans$t08*2-StationMeans$tl08
StationMeans$th09 <- StationMeans$t09*2-StationMeans$tl09
StationMeans$th10 <- StationMeans$t10*2-StationMeans$tl10
StationMeans$th11 <- StationMeans$t11*2-StationMeans$tl11
StationMeans$th12 <- StationMeans$t12*2-StationMeans$tl12

#calculate extreme winter low----
StationMeans$pacificsouth <- 1/((((StationMeans$Latitude - -22.7)/13)^2 + ((StationMeans$Longitude - -82.3)/14)^2)^2+1)
StationMeans$amazon2 <- 1/((((StationMeans$Latitude - -10.2)/5)^2 + ((StationMeans$Longitude - -59.9)/10)^2)^2+1)
StationMeans$amazon1 <- 1/((((StationMeans$Latitude - -2.8)/14)^2 + ((StationMeans$Longitude - -61.3)/19)^2)^2+1)
StationMeans$pacificcent <- 1/((((StationMeans$Latitude - 4.1)/21)^2 + ((StationMeans$Longitude - -122.4)/41)^2)^2+1)
StationMeans$mexico <- 1/((((StationMeans$Latitude - 26)/6)^2 + ((StationMeans$Longitude - -98.4)/12)^2)^2+1)
StationMeans$florida <- 1/((((StationMeans$Latitude - 27.5)/4)^2 + ((StationMeans$Longitude - -81.1)/8)^2)^2+1)
StationMeans$pacificnorth <- 1/((((StationMeans$Latitude - 32.9)/26)^2 + ((StationMeans$Longitude - -145)/27)^2)^2+1)
StationMeans$oklahoma <- 1/((((StationMeans$Latitude - 33.6)/4)^2 + ((StationMeans$Longitude - -98.4)/8)^2)^2+1)
StationMeans$arizona <- 1/((((StationMeans$Latitude - 34)/12)^2 + ((StationMeans$Longitude - -113.1)/8)^2)^2+1)
StationMeans$atlantic <- 1/((((StationMeans$Latitude - 34)/15)^2 + ((StationMeans$Longitude - -60.7)/19)^2)^2+1)
StationMeans$himalayas <- 1/((((StationMeans$Latitude - 35.3)/6)^2 + ((StationMeans$Longitude - 91.3)/13)^2)^2+1)
StationMeans$kentucky <- 1/((((StationMeans$Latitude - 38.5)/3)^2 + ((StationMeans$Longitude - -87.6)/9)^2)^2+1)
StationMeans$detroit <- 1/((((StationMeans$Latitude - 41.8)/3)^2 + ((StationMeans$Longitude - -82.6)/4)^2)^2+1)
StationMeans$ontario <- 1/((((StationMeans$Latitude - 44.6)/2)^2 + ((StationMeans$Longitude - -79.2)/6)^2)^2+1)
StationMeans$montana <- 1/((((StationMeans$Latitude - 45.4)/5)^2 + ((StationMeans$Longitude - -111.8)/10)^2)^2+1)
StationMeans$minn <- 1/((((StationMeans$Latitude - 47.6)/6)^2 + ((StationMeans$Longitude - -92.6)/12)^2)^2+1)
StationMeans$hudson <- 1/((((StationMeans$Latitude - 60)/7)^2 + ((StationMeans$Longitude - -87)/34)^2)^2+1)
StationMeans$siberia <- 1/((((StationMeans$Latitude - 61.2)/20)^2 + ((StationMeans$Longitude - 105.7)/39)^2)^2+1)
StationMeans$california <- 1/((((StationMeans$Latitude - 34.8)/9)^2 + ((StationMeans$Longitude - -128.2)/9)^2)^2+1)
StationMeans$washington <- 1/((((StationMeans$Latitude - 46)/5)^2 + ((StationMeans$Longitude - -126.6)/5)^2)^2+1)
StationMeans$colorado <- 1/((((StationMeans$Latitude - 38.3)/2)^2 + ((StationMeans$Longitude - -108.8)/3)^2)^2+1)
StationMeans$hawaii <- 1/((((StationMeans$Latitude - 21.3)/7)^2 + ((StationMeans$Longitude - -157.5)/11)^2)^2+1)
StationMeans$chess <- 1/((((StationMeans$Latitude - 37)/3)^2 + ((StationMeans$Longitude - -74)/3)^2)^2+1)

StationMeans$Tg <- pmax(apply(StationMeans[,c('b05','b06','b07','b08','b09','b10')], 1, FUN = mean), apply(StationMeans[,c('b11','b12','b01','b02','b03','b04')], 1, FUN = mean))/1
StationMeans$Tc <- apply(StationMeans[,c('t01', 't02', 't04', 't04', 't05', 't06', 't07', 't08', 't09', 't10', 't11', 't12')], 1, FUN = min)
StationMeans$Tcl <- apply(StationMeans[,c('tl01', 'tl02', 'tl04', 'tl04', 'tl05', 'tl06', 'tl07', 'tl08', 'tl09', 'tl10', 'tl11', 'tl12')], 1, FUN = min)
StationMeans$Tw <- apply(StationMeans[,c('t01', 't02', 't04', 't04', 't05', 't06', 't07', 't08', 't09', 't10', 't11', 't12')], 1, FUN = max)
StationMeans$Twh <- apply(StationMeans[,c('th01', 'th02', 'th04', 'th04', 'th05', 'th06', 'th07', 'th08', 'th09', 'th10', 'th11', 'th12')], 1, FUN = max)


StationMeans$Tclx<-	-9.171	+
  StationMeans$Tcl *	1.202	+
  StationMeans$Latitude *	-0.04149	+
  StationMeans$Elevation *	0.0008691	+
  StationMeans$Latitude * StationMeans$Elevation *	-0.00002455	+
  StationMeans$pacificsouth *	-1.792	+
  StationMeans$amazon2 *	2.573	+
  StationMeans$amazon1 *	-1.014	+
  StationMeans$pacificcent *	-0.749	+
  StationMeans$mexico *	-0.8227	+
  StationMeans$florida *	-3.557	+
  StationMeans$pacificnorth *	-1.246	+
  StationMeans$oklahoma *	0.1758	+
  StationMeans$arizona *	2.605	+
  StationMeans$chess *	0.8347	+
  StationMeans$atlantic *	0.2967	+
  StationMeans$himalayas *	-1.814	+
  StationMeans$kentucky *	-2.644	+
  StationMeans$detroit *	0	+
  StationMeans$ontario *	-2.314	+
  StationMeans$montana *	-4.415	+
  StationMeans$minn *	1.136	+
  StationMeans$hudson *	-5.154	+
  StationMeans$siberia *	-3.797	+
  StationMeans$california *	4.48	+
  StationMeans$washington *	3.597	+
  StationMeans$colorado *	1.458	+
  StationMeans$hawaii *	6.673	

StationMeans <- StationMeans[!is.na(StationMeans$Tclx),]
StationMeans <- subset(StationMeans, select = -c(pacificsouth,amazon1,amazon2, pacificcent, mexico, florida,                                               pacificnorth, oklahoma, arizona, atlantic, himalayas, kentucky, 
                                                 detroit, ontario, montana, minn, hudson, siberia, california, washington, colorado, chess, hawaii))

#daylength


StationMeans$e01 <- 0
StationMeans$e02 <- 0
StationMeans$e03 <- 0
StationMeans$e04 <- 0
StationMeans$e05 <- 0
StationMeans$e06 <- 0
StationMeans$e07 <- 0
StationMeans$e08 <- 0
StationMeans$e09 <- 0
StationMeans$e10 <- 0
StationMeans$e11 <- 0
StationMeans$e12 <- 0



#----

for (i in 0:11){
  StationMeans$hs = acos(pmin(pmax(-tan(StationMeans$Latitude/360*2*3.141592) * tan(DaysMonth[i+1,]$declination),-1),1))
  StationMeans$Ra = 
    117.5 * (StationMeans$hs*sin(StationMeans$Latitude/360*2*3.141592)*sin(DaysMonth[i+1,]$declination) +
               cos(StationMeans$Latitude/360*2*3.141592)*cos(DaysMonth[i+1,]$declination)*sin(StationMeans$hs)) / 3.141592
  
  StationMeans[,which(colnames(StationMeans)=='e01')+i] <- 0.008404*
    216.7*exp(17.26939*
                StationMeans[,which(colnames(StationMeans)=='t01')+i]/(StationMeans[,which(colnames(StationMeans)=='t01')+i]+237.3))/
    (StationMeans[,which(colnames(StationMeans)=='t01')+i]+273.3)*
    (StationMeans$Ra)*DaysMonth[1+i,c('Days')]*
    abs((StationMeans[,which(colnames(StationMeans)=='t01')+i] - StationMeans[,which(colnames(StationMeans)=='tl01')+i])*2)^0.5 + 0.001
}


StationMeans <- subset(StationMeans, 
                       select = -c(hs,Ra))

StationMeans$a01 <- 0
StationMeans$a02 <- 0
StationMeans$a03 <- 0
StationMeans$a04 <- 0
StationMeans$a05 <- 0
StationMeans$a06 <- 0
StationMeans$a07 <- 0
StationMeans$a08 <- 0
StationMeans$a09 <- 0
StationMeans$a10 <- 0
StationMeans$a11 <- 0
StationMeans$a12 <- 0
for (i in 0:11){
  StationMeans[,which(colnames(StationMeans)=='a01')+i] <- 
    pmin(StationMeans[,which(colnames(StationMeans)=='e01')+i], StationMeans[,which(colnames(StationMeans)=='pp01')+i])
}


StationMeans$pAET <- apply(StationMeans[,which(colnames(StationMeans)=='a01'):which(colnames(StationMeans)=='a12')], MARGIN = 1, FUN='max')

StationMeans$PET <- apply(StationMeans[,which(colnames(StationMeans)=='e01'):which(colnames(StationMeans)=='e12')], MARGIN = 1, FUN='sum')

StationMeans$MAP <- apply(StationMeans[,which(colnames(StationMeans)=='pp01'):which(colnames(StationMeans)=='pp12')], MARGIN = 1, FUN='sum')

StationMeans$AET <- apply(StationMeans[,which(colnames(StationMeans)=='a01'):which(colnames(StationMeans)=='a12')], MARGIN = 1, FUN='sum')
StationMeans$MAAT <- apply(StationMeans[,which(colnames(StationMeans)=='t01'):which(colnames(StationMeans)=='t12')], MARGIN = 1, FUN='mean')

StationMeans$Deficit <- pmax(StationMeans$PET - StationMeans$AET, 0)
StationMeans$Surplus <- pmax(StationMeans$MAP - StationMeans$AET, 0)


StationMeans <- 
  StationMeans[,c("LRU","Station_ID","Station_Name","State",
                  "Latitude","Longitude","Elevation","Tg","Tc","Tcl","Tw","Twh","Tclx","MAAT","MAP","PET","AET","pAET",
                  "Deficit","Surplus","wts")]#must include wts term.

StationMeans <- merge(StationMeans, NormCoordTrans, by='Station_ID')
StationMeans$PPETRatio <- StationMeans$MAP/(StationMeans$PET +0.0001)
StationMeans$Mindex <- StationMeans$PPETRatio/(StationMeans$PPETRatio+1)

graphymax = input$lat[2]#max(selectClim$Latitude)+10# 
graphymin = input$lat[1]#min(selectClim$Latitude)-10# 
graphxmax = input$lon[2]#max(selectClim$Longitude)+10# 
graphxmin = input$lon[1]#min(selectClim$Longitude)-10# 


StationMeans$SP1 <- round(ifelse(StationMeans$PPETRatio < 0.5 & StationMeans$Surplus < 25 & StationMeans$pAET < 75, pmax(StationMeans$Surplus/25, StationMeans$pAET/75)  ,1),15)
StationMeans$SP2 <- round(ifelse(StationMeans$SP1 >= 1, ifelse(StationMeans$pAET < 75 & (StationMeans$Deficit >= 150 | StationMeans$PPETRatio < 1), pmax(StationMeans$pAET/75, 150/(StationMeans$Deficit+150)),1),0),15)
StationMeans$SP3 <- round(ifelse(StationMeans$SP2 >= 1, ifelse(StationMeans$Deficit >= 150 | StationMeans$PPETRatio < 1, pmax(150/(StationMeans$Deficit+150)),1),0),15)
StationMeans$SP4 <- round(ifelse(StationMeans$SP3 >= 1, pmin(1-StationMeans$Deficit/150),0),15)
StationMeans$SPindex <- StationMeans$SP1 + StationMeans$SP2 + StationMeans$SP3 + StationMeans$SP4 + 1 #Seasonal precipitation index
StationMeans$Cindex <- pmin(StationMeans$Tclx+15, StationMeans$Tc) #Cold index
StationMeans$Dindex <- StationMeans$Deficit/(StationMeans$Deficit + 100)
StationMeans$Sindex <- StationMeans$Surplus/(StationMeans$Surplus + 100)
StationMeans$Aindex <- StationMeans$pAET/(StationMeans$pAET + 100)
#Key to climate type_____________________________________________________


Seasonalilty <- ifelse(Deficit < 150 & PPETRatio>=1, "Isopluvial",
                       ifelse(Surplus < 25 & PPETRatio < 0.5 & peakAET < 75, "Isoxeric",
                              ifelse(peakAET < 75,"Xerothermic","Pluviothermic")))







MRegime <- ifelse(PPETRatio>=2,"Perhumid",
                  ifelse(PPETRatio>=1.414,"Moist-Humid",
                         ifelse(PPETRatio>=1,"Dry-Humid",
                                ifelse(PPETRatio>=0.707,"Moist-Subhumid",
                                       ifelse(PPETRatio>=0.5,"Dry-Subhumid",
                                              ifelse(PPETRatio>=0.25,"Semiarid",
                                                     ifelse(PPETRatio>=0.125,"Arid","Perarid"
                                                     )))))))


BioTemperatureC <- 
  ifelse(Tc >= 20 & Tclx >=5,"Meso-Tropical",
         ifelse(Tc >= 15 & Tclx >=0,"Cryo-Tropical",
                ifelse(Tc >= 10 & Tclx >=-5,"Thermo-Sutropical",
                       ifelse(Tc >= 5 & Tclx >=-10,"Meso-Subtropical",
                              ifelse(Tc >= 0 & Tclx >=-15,"Cryo-Subtropical",
                                     ifelse(Tc >= -5 & Tclx >=-20,"Thermo-Temperate",
                                            ifelse(Tc >= -10 & Tclx >=-25,"Meso-Temperate",
                                                   ifelse(Tc >= -25 & Tclx >=-40,"Cryo-Temperate","Polar"
                                                   ))))))))

BioTemperatureW <- ifelse(SummerBioT >= 24,"Hot (Lowland)",
                          ifelse(SummerBioT >= 18,"Warm (Premontane)",
                                 ifelse(SummerBioT >= 15,"Warm-Mild (Lower-Montane)",
                                        ifelse(SummerBioT >= 12,"Cool-Mild (Upper-Montane)",
                                               ifelse(SummerBioT >= 6,"Cool (Subalpine)","Cold (Alpine)"
                                               )))))
Climatetext<-paste(BioTemperatureW," ",BioTemperatureC,", ",MRegime," ",Seasonalilty, sep="" )
#Swap out the external data to a seperate data frame and retain internal data for all but elevation graph.
StationMeans2<- StationMeans
StationMeans <- StationMeans[StationMeans$wts >=1,]


#Supplemental Graph1----
a1=data.frame(x=c(-50,-50,0,0), y=c(0,6,6,0))
a2=data.frame(x=c(-50,-50,0,0), y=c(6,12,12,6))
a3=data.frame(x=c(-50,-50,0,0), y=c(12,36,36,12))
a4=data.frame(x=c(0,0,6,0), y=c(0,6,6,0))
a5=data.frame(x=c(0,0,18,6), y=c(6,18,18,6))
a6=data.frame(x=c(0,0,15,15), y=c(18,36,36,18))
a7=data.frame(x=c(15,15,36,18), y=c(18,36,36,18))

ll1 <- data.frame(x=c(-50,6), y=c(6,6))
ll2 <- data.frame(x=c(0,0), y=c(0,36))
ll3 <- data.frame(x=c(15,15), y=c(18,36))
l1 <- data.frame(x=c(-50,12), y=c(12,12))
l2 <- data.frame(x=c(-50,0), y=c(15,15))
l3 <- data.frame(x=c(-50,18), y=c(18,18))
l4 <- data.frame(x=c(0,24), y=c(24,24))
l5 <- data.frame(x=c(-10,-10), y=c(0,36))
l6 <- data.frame(x=c(-25,-25), y=c(0,36))

climplot2 <-  ggplot() +
  geom_polygon(data=a1, mapping=aes(x=x, y=y, fill='alpine'),alpha = 0.5)+
  geom_polygon(data=a2, mapping=aes(x=x, y=y, fill='boreal'),alpha = 0.5)+
  geom_polygon(data=a3, mapping=aes(x=x, y=y, fill='temperate'),alpha = 0.5)+
  geom_polygon(data=a4, mapping=aes(x=x, y=y, fill='andean'),alpha = 0.5)+
  geom_polygon(data=a5, mapping=aes(x=x, y=y, fill='oceanic'),alpha = 0.5)+
  geom_polygon(data=a6, mapping=aes(x=x, y=y, fill='subtropical'),alpha = 0.5)+
  geom_polygon(data=a7, mapping=aes(x=x, y=y, fill='tropical'),alpha = 0.5)+
  
  geom_line(data=ll1, mapping=aes(x=x, y=y),alpha = 0.3, color='black', linetype='solid')+
  geom_line(data=ll2, mapping=aes(x=x, y=y),alpha = 0.3, color='black', linetype='solid')+
  geom_line(data=ll3, mapping=aes(x=x, y=y),alpha = 0.3, color='black', linetype='solid')+
  geom_line(data=l1, mapping=aes(x=x, y=y),alpha = 0.3, color='black', linetype='solid')+
  geom_line(data=l2, mapping=aes(x=x, y=y),alpha = 0.3, color='black', linetype='solid')+
  geom_line(data=l3, mapping=aes(x=x, y=y),alpha = 0.3, color='black', linetype='solid')+
  geom_line(data=l4, mapping=aes(x=x, y=y),alpha = 0.3, color='black', linetype='solid')+
  geom_line(data=l5, mapping=aes(x=x, y=y),alpha = 0.3, color='black', linetype='solid')+
  geom_line(data=l6, mapping=aes(x=x, y=y),alpha = 0.3, color='black', linetype='solid')+
  geom_point(data=StationMeans, mapping=aes(x=Cindex, y=Tg, color = 'Current MLRA'), size=0.5)+
  geom_density2d(data=StationMeans, mapping=aes(x=Cindex, y=Tg), color = 'black',alpha = 0.25)+
  geom_point(data=savedselect, mapping=aes(x=Cindex, y=Tg, color = 'Saved MLRA'), size=0.5)+
  geom_density2d(data=savedselect, mapping=aes(x=Cindex, y=Tg),color = 'red',alpha = 0.25)+
  scale_fill_manual("Legend", values = c("alpine" = "pink",
                                         "boreal" = "darkgreen",
                                         "temperate" = "greenyellow",
                                         "andean" = "lightblue",
                                         "oceanic" = "darkcyan",
                                         "subtropical" = "orange",
                                         "tropical" = "darkred"
                                         
  ))+
  scale_color_manual("",values = c("Current MLRA" = "black","Saved MLRA" = "red"))+
  scale_x_continuous(name= "Coldest Month (Annual Extreme Minimum)", 
                     breaks=c(-45,-40, -35, -30, -25, -20,-15, -10,-5, 0,5, 10,15, 20,25,30),
                     labels=c('-45 (-60)','-40 (-55)', '-35 (-50)','-30 (-45)', '-25 (-40)','-20 (-35)','-15 (-30)','-10 (-25)',
                              '-5 (-20)','0 (-15)','5 (-10)','10 (-5)','15 (0)','20 (5)','25 (10)','30 (15)'))+
  scale_y_continuous(name= "Growing Season", breaks=c(0,6,12,18,24,30))+
  coord_fixed(ratio = 1/1,xlim = c(-45,30), ylim = c(0, 33))+
  labs(title = paste("Climate of ",StationMeans[1,]$LRU, ": ", MLRAname, sep=""))+
  theme_bw()+
  theme(legend.position='right',axis.text.x = element_text(angle = 90, vjust = 0, hjust = 0),
        panel.grid.major = element_line(), panel.grid.minor = element_blank())


climplot2




















#Model temperature elevation curve using inputs controlling for latitude and longitude.
wmod <- lm(Tg ~ Elevation + Latitude + Longitude, weight = StationMeans2$wts,data = StationMeans2)
cmod <- lm(Cindex ~ Elevation + Latitude + Longitude, weight = StationMeans2$wts, data = StationMeans2)
df <- StationMeans2
df$wfit <- predict.lm(wmod, df)
df$cfit <- predict.lm(cmod, df)
df$Latitude <- selecty
df$Longitude <-  selectx
df$wfit2 <- predict.lm(wmod, df)
df$cfit2 <- predict.lm(cmod, df)
df$Tg <-  df$Tg + (df$wfit2 - df$wfit)
df$Cindex <-  df$Cindex + (df$cfit2 - df$cfit)
climelev <-  ggplot() +
  geom_point(mapping=aes(y=c(-1000,8000), x=c(-1000,8000)), size=0)+#increase range of graph for extrapolation
  stat_smooth(data=df, mapping=aes(y=Tg, x=Elevation, weight = df$wts, color='Growing Season'), method='lm', formula='y~x', fullrange = TRUE, size=0.5)+
  stat_smooth(data=df, mapping=aes(y=Cindex, x=Elevation, weight = df$wts, color='Winter'), method='lm', formula='y~x', fullrange = TRUE, size=0.5)+
  
  geom_point(data=StationMeans, mapping=aes(y=Tg, x=Elevation, shape='Growing Season', color='Growing Season'), size=1.5)+
  geom_point(data=StationMeans, mapping=aes(y=Cindex, x=Elevation, shape='Winter', color='Winter'), size=1.5)+
  scale_x_continuous(name= "Elevation", 
                     breaks=c(-500,0, 500,1000,1500,2000,2500,3000,3500,4000,4500,5000,6000,8000))+
  scale_y_continuous(name= "Temperature", breaks=c(-25,-10,0,6,12,18,24,30,36))+
  coord_fixed(ratio = 1000/15,xlim = c(-250,4500), ylim = c(-30, 33))+
  labs(title = paste("Climate of ",StationMeans[1,]$LRU, ": ", MLRAname, sep=""))+
  theme_bw()+
  scale_shape_manual("",values = c("Winter" = 6, "Growing Season"=2))+
  scale_color_manual("",values = c("Winter" = 'blue', "Growing Season"='red'))+
  theme(legend.position="bottom",axis.text.x = element_text(angle = 90, vjust = 0, hjust = 0),
        panel.grid.major = element_line( colour = 'black', size = 0.1), panel.grid.minor = element_blank())
climelev