library(rpart)
library(rpart.plot)
Biomeclimate <- readRDS(file='C:/workspace2/BiomeClimate/data/Biomeclimate.RDS')
#classification tree packages
biomesummary<-read.delim("C:/workspace/biomesummary.txt")

#Classification Tree
#selectBiome <- Biomeclimate[Biomeclimate$BIOME %in% c('1','4','7','8') & Biomeclimate$Norm == '1990',]
selectBiome <- Biomeclimate[Biomeclimate$Latitude >= 33 & Biomeclimate$Latitude <= 50 & Biomeclimate$Longitude <= -85 & Biomeclimate$Latitude >= -106 & Biomeclimate$Norm == '1990',]
selectBiome <- Biomeclimate[Biomeclimate$Norm == '1990',]
selectBiomecount<-aggregate(selectBiome[,c("biomname")], by=list(selectBiome$biomname),FUN=length)
colnames(selectBiomecount)<-c("biomname","x")
selectBiomecount$wt<-1000/(selectBiomecount$x+1000)
#selectBiome <- subset(selectBiome, select = -c(wt) )
selectBiome<-merge(selectBiome,selectBiomecount,by="biomname")
colnames(selectBiome)
selectBiome$P <- apply(selectBiome[,which(colnames(selectBiome) == 'p01') : which(colnames(selectBiome) == 'p12')], MARGIN = 1, FUN = 'sum')
selectBiome$E <- apply(selectBiome[,which(colnames(selectBiome) == 'e01') : which(colnames(selectBiome) == 'e12')], MARGIN = 1, FUN = 'sum')
mean(selectBiome$E)

#####
selectBiome$M <- selectBiome$P/(selectBiome$E + 0.0000001)
biomeclass <- rpart(biomname ~  Tg + Tc + Tclx + M + Deficit + Surplus + pAET, data = selectBiome,weights=selectBiome$wt, method="class", control = list(maxdepth = 5, cp=0.0005, minsplit=1000))

biomeclass <- rpart(biomname ~  Tclx + M, data = selectBiome,weights=selectBiome$wt, method="class", control = list(maxdepth = 5, cp=0.0005, minsplit=1000))
# Make plot
png(filename="biomeclass.png",width = 25, height = 4, units = 'in', res = 600)
rpart.plot(biomeclass, extra=108) # Make plot
dev.off()
#
library(shiny)
library(ggplot2)
#calculate percentiles
library(plyr)
Norms2010 <- readRDS(file='data/Norms2010.RDS')
DaysMonth <- readRDS(file='data/DaysMonth.RDS')

MLRA <- readRDS(file='data/LRU.RDS')
NormCoordTrans <- readRDS(file='data/NormCoordTrans.RDS')
Norms2010<-merge(Norms2010,MLRA, by="Station_ID")


DaysMonth$declination <- 0.409*sin(2*3.141592*DaysMonth$Day_/365-1.39)
StationMeans <- aggregate(Norms2010[,c("t01", "t02", "t03", "t04", "t05", "t06",
                                       "t07", "t08", "t09", "t10", "t11", "t12",
                                       "tl01", "tl02", "tl03", "tl04", "tl05", "tl06",
                                       "tl07", "tl08", "tl09", "tl10", "tl11", "tl12",
                                       "pp01", "pp02", "pp03", "pp04", "pp05", "pp06",
                                       "pp07", "pp08", "pp09", "pp10", "pp11","pp12")],
by=list(Norms2010$LRU, Norms2010$Station_ID, Norms2010$Latitude, Norms2010$Longitude, Norms2010$Elevation), FUN='mean')
colnames(StationMeans)[1:5] <- c('LRU', 'Station_ID', 'Latitude', 'Longitude', 'Elevation')


#Make Monthly Rows
selectClim <- Norms2010[Norms2010$LRU %in% '98A',]
#Jan
selectMonthly <- selectClim[,c("LRU","Station_ID","State","Latitude","Longitude","Elevation","t01","tl01","pp01")]
colnames(selectMonthly) <- c("LRU","Station_ID","State","Latitude","Longitude","Elevation","t","tl","p")
selectMonthly$Month<- 1

#Feb-Dec
for (i in 1:11){
  
  selectMonthlyA <- selectClim[,c("LRU","Station_ID","State","Latitude","Longitude","Elevation",
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
     abs((sumMonthly$th - sumMonthly$tl)*2)^0.5 + 0.001
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
   MAAT <- sum(sumMonthly$t)
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
   StationMeans <- aggregate(Norms2010[,c("t01", "t02", "t03", "t04", "t05", "t06",
                                          "t07", "t08", "t09", "t10", "t11", "t12",
                                          "tl01", "tl02", "tl03", "tl04", "tl05", "tl06",
                                          "tl07", "tl08", "tl09", "tl10", "tl11", "tl12",
                                          "pp01", "pp02", "pp03", "pp04", "pp05", "pp06",
                                          "pp07", "pp08", "pp09", "pp10", "pp11","pp12")],
                             by=list(Norms2010$LRU, Norms2010$Station_ID, Norms2010$Station_Name, Norms2010$State, Norms2010$Latitude, Norms2010$Longitude, Norms2010$Elevation), FUN='mean')
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
   
   StationMeans$Deficit <- pmax(StationMeans$PET - StationMeans$AET, 0)
   StationMeans$Surplus <- pmax(StationMeans$MAP - StationMeans$AET, 0)
   
   
   
   
   
   
###-----------   
   
   
Norms2010<-merge(Norms2010,MLRA, by="Station_ID")
mlraagg <- aggregate(MLRA[,c('Station_ID')], by=list(MLRA$LRU), FUN=length)
write.csv(MLRA, 'MLRA.csv')
LRU <- read.csv('MLRA.csv')
saveRDS(LRU,'data/LRU.RDS')
LRU2 <- readRDS('data/LRU.RDS')
require(sp)
require(raster)
require(rgdal)
require(rms)
require(RODBC)
require(ggplot2)
require(broom)
LRU.shp <- readOGR("data/gis/MLRA_2018.shp")
LRU.shp2 <- spTransform(LRU.shp, crs("+init=epsg:2163"))
LRU.shptidy <- tidy(LRU.shp2, region ='LRU')
LRUNames <- unique(as.data.frame(LRU.shp[, c('MLRA', 'LRU','MLRA_NAME')]))
LRUNames <- LRUNames[!is.na(LRUNames$MLRA_NAME),]

saveRDS(LRUNames, 'LRUNames.RDS')
saveRDS(LRU.shptidy, 'LRU.shptidy.RDS')
crs(LRU.shp)

Norms2010 <- readRDS('data/Norms2010.RDS')
names(Norms2010)
NormCoord <- unique(Norms2010[,c('Station_ID', 'Latitude', 'Longitude')])

xy <- NormCoord[,c('Longitude','Latitude')]
pts <- SpatialPointsDataFrame(coords = xy, data = NormCoord,
                              proj4string = CRS("+init=epsg:4269"))
pts2 <- spTransform(pts, crs("+init=epsg:2163"))
pts3 <- tidy(pts2, region='Station_ID')
pts3 <- cbind(Station_ID = as.character(pts2$Station_ID),coordinates(pts2))
saveRDS(pts3, 'NormCoordTrans.RDS')

CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0 "))

pts <- 
selectMLRA.shp <- LRU.shptidy[LRU.shptidy$id %in% '98A', ]
ggplot() +
 geom_polygon(data = selectMLRA.shp, aes(x = long, y = lat, group=group))
              