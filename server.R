#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)
library(ggplot2)
#calculate percentiles
library(plyr)
Norms2010 <- readRDS(file='data/Norms2010.RDS')
MLRA <- readRDS(file='data/MLRA.RDS')
DaysMonth <- readRDS(file='data/DaysMonth.RDS')
Norms2010<-merge(Norms2010,MLRA, by="Station_ID")

######
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$climplot <- renderPlot({ 
  
  
  selection<-input$mlra
  #selection<-c("98A","98A1","98A3","98A4","98A5")
  #selection<-c("97A","97A1","97A2")
  #selection<-c("110","110XY")
  #selection<-c("111C","111CY")
  
  #selectedNorm <- Norms2010[Norms2010$LRU %in% selection,]
  #selectedNorm <- selectedNorm[selectedNorm$Elevation > 600,]
  #selectedNorm <- selectedNorm[selectedNorm$Elevation < 600,]
  #selectedNorm <- Norms2010[Norms2010$State %in% c("MI"),]
  #selectedNorm <- selectedNorm[selectedNorm$Station_Name %in% c("JORNADA EXP RANGE","KNOXVILLE EXP STN", "GRAND RAPIDS"),]
  selectedNorm <- Norms2010[Norms2010$LRU %in% selection,]
  
  
  meanbystation <- aggregate(selectedNorm[,4:43], by=list(selectedNorm$Station_ID,selectedNorm$Station_Name,selectedNorm$State ), FUN='mean')
  colnames(meanbystation) <-  colnames(selectedNorm[,1:43])
  
  meanbystation$b01 <- meanbystation$t01*(0)
  meanbystation$b02 <- meanbystation$t01*(0)
  meanbystation$b03 <- meanbystation$t01*(0)
  meanbystation$b04 <- meanbystation$t01*(0)
  meanbystation$b05 <- meanbystation$t01*(0)
  meanbystation$b06 <- meanbystation$t01*(0)
  meanbystation$b07 <- meanbystation$t01*(0)
  meanbystation$b08 <- meanbystation$t01*(0)
  meanbystation$b09 <- meanbystation$t01*(0)
  meanbystation$b10 <- meanbystation$t01*(0)
  meanbystation$b11 <- meanbystation$t01*(0)
  meanbystation$b12 <- meanbystation$t01*(0)
  for (i in 1:12){
    meanbystation[,i+43] <- meanbystation[,i+7]*(meanbystation[,i+7]>0)*1
  }
  
  meanbystation$Bt6 <- pmax(apply(meanbystation[,c('b05','b06','b07','b08','b09','b10')], 1, FUN = mean), apply(meanbystation[,c('b11','b12','b01','b02','b03','b04')], 1, FUN = mean))/1
  meanbystation$tlo <- pmax(apply(meanbystation[,8:19], 1, FUN = min),apply(meanbystation[,20:31], 1, FUN = min)+6,apply(meanbystation[,8:19], 1, FUN = min) * 2 + 6 - meanbystation$Bt6)
  
  meanbystation$e01 <- meanbystation$t01*(0)
  meanbystation$e02 <- meanbystation$t01*(0)
  meanbystation$e03 <- meanbystation$t01*(0)
  meanbystation$e04 <- meanbystation$t01*(0)
  meanbystation$e05 <- meanbystation$t01*(0)
  meanbystation$e06 <- meanbystation$t01*(0)
  meanbystation$e07 <- meanbystation$t01*(0)
  meanbystation$e08 <- meanbystation$t01*(0)
  meanbystation$e09 <- meanbystation$t01*(0)
  meanbystation$e10 <- meanbystation$t01*(0)
  meanbystation$e11 <- meanbystation$t01*(0)
  meanbystation$e12 <- meanbystation$t01*(0)
  meanbystation[1:4,58:69]
  
  DaysMonth$declination <- 0.409*sin(2*3.141592*DaysMonth$Day_/365-1.39)
  
  Daylength <- merge(unique(selectedNorm[,1:6]), DaysMonth)
  
  Daylength$Daylength <- ifelse(Daylength$Latitude + Daylength$declination*360/2/3.141592 >= 90, 24, ifelse(Daylength$Latitude - Daylength$declination*360/2/3.141592 >= 90, 0, (atan(-((sin(-0.83/360*2*3.141592)-sin(Daylength$declination)*sin(Daylength$Latitude/360*2*3.141592))/(cos(Daylength$declination)*cos(Daylength$Latitude/360*2*3.141592)))/(-((sin(-0.83/360*2*3.141592)-sin(Daylength$declination)*sin(Daylength$Latitude/360*2*3.141592))/(cos(Daylength$declination)*cos(Daylength$Latitude/360*2*3.141592)))*((sin(-0.83/360*2*3.141592)-sin(Daylength$declination)*sin(Daylength$Latitude/360*2*3.141592))/(cos(Daylength$declination)*cos(Daylength$Latitude/360*2*3.141592)))+1)^0.5)+2*atan(1))/3.141592*24))
  meanbystation[1:2,20:23]
  for (i in 1:12){   
    
    #add daylength to dataset
    meanbystation <- merge(meanbystation,Daylength[Daylength$Month_ %in% i,c("Station_ID","Days","Daylength")],by=("Station_ID"))
    #meanbystation <-na.omit(meanbystation)
    #PET
    meanbystation[,i+57] <- 216.7*6.108*exp(17.26939*meanbystation[,i+7]/(meanbystation[,i+7]+237.3))/(meanbystation[,i+7]+273.3)*0.1651*(meanbystation$Daylength/12)*meanbystation$Days*0.2606*abs((meanbystation[,i+7]-meanbystation[,i+19])*2)^0.5 + 0.001
    meanbystation <- subset(meanbystation, select = -c(Days,Daylength))
  }
  
  meanbystation$a01 <- meanbystation$t01*(0)
  meanbystation$a02 <- meanbystation$t01*(0)
  meanbystation$a03 <- meanbystation$t01*(0)
  meanbystation$a04 <- meanbystation$t01*(0)
  meanbystation$a05 <- meanbystation$t01*(0)
  meanbystation$a06 <- meanbystation$t01*(0)
  meanbystation$a07 <- meanbystation$t01*(0)
  meanbystation$a08 <- meanbystation$t01*(0)
  meanbystation$a09 <- meanbystation$t01*(0)
  meanbystation$a10 <- meanbystation$t01*(0)
  meanbystation$a11 <- meanbystation$t01*(0)
  meanbystation$a12 <- meanbystation$t01*(0)
  meanbystation[1:3,70:81]
  #prec = [,i+31]; pet = [,i+57]; temp = meanbystation[,i+7]
  for (i in 1:12){
    meanbystation[,i+69] <- pmin(meanbystation[,i+57],meanbystation[,i+31])
  }
  
  meanbystation$g01 <- meanbystation$t01*(0)
  meanbystation$g02 <- meanbystation$t01*(0)
  meanbystation$g03 <- meanbystation$t01*(0)
  meanbystation$g04 <- meanbystation$t01*(0)
  meanbystation$g05 <- meanbystation$t01*(0)
  meanbystation$g06 <- meanbystation$t01*(0)
  meanbystation$g07 <- meanbystation$t01*(0)
  meanbystation$g08 <- meanbystation$t01*(0)
  meanbystation$g09 <- meanbystation$t01*(0)
  meanbystation$g10 <- meanbystation$t01*(0)
  meanbystation$g11 <- meanbystation$t01*(0)
  meanbystation$g12 <- meanbystation$t01*(0)
  meanbystation[1:3,58:68]
  #prec = [,i+31]; pet = [,i+57]; temp = meanbystation[,i+7]
  for (i in 1:12){
    meanbystation[,i+81] <- ifelse(meanbystation[,i+57] > meanbystation[,i+31], meanbystation[,i+31]/(meanbystation[,i+57] + 0.001)*
                                     ((meanbystation[,i+7]-5)*(meanbystation[,i+7]>5)*0.8 + (meanbystation[,i+7])*(meanbystation[,i+7]>0)*0.2), 
                                   ((meanbystation[,i+7]-5)*(meanbystation[,i+7]>5))*0.8 + (meanbystation[,i+7])*(meanbystation[,i+7]>0)*0.2)
  }
  meanbystation$th01 <- meanbystation$t01*2-meanbystation$tl01
  meanbystation$th02 <- meanbystation$t02*2-meanbystation$tl02
  meanbystation$th03 <- meanbystation$t03*2-meanbystation$tl03
  meanbystation$th04 <- meanbystation$t04*2-meanbystation$tl04
  meanbystation$th05 <- meanbystation$t05*2-meanbystation$tl05
  meanbystation$th06 <- meanbystation$t06*2-meanbystation$tl06
  meanbystation$th07 <- meanbystation$t07*2-meanbystation$tl07
  meanbystation$th08 <- meanbystation$t08*2-meanbystation$tl08
  meanbystation$th09 <- meanbystation$t09*2-meanbystation$tl09
  meanbystation$th10 <- meanbystation$t10*2-meanbystation$tl10
  meanbystation$th11 <- meanbystation$t11*2-meanbystation$tl11
  meanbystation$th12 <- meanbystation$t12*2-meanbystation$tl12
  
  #calculate extreme winter low----
  meanbystation$pacificsouth <- 1/((((meanbystation$Latitude - -22.7)/13)^2 + ((meanbystation$Longitude - -82.3)/14)^2)^2+1)
  meanbystation$amazon2 <- 1/((((meanbystation$Latitude - -10.2)/5)^2 + ((meanbystation$Longitude - -59.9)/10)^2)^2+1)
  meanbystation$amazon1 <- 1/((((meanbystation$Latitude - -2.8)/14)^2 + ((meanbystation$Longitude - -61.3)/19)^2)^2+1)
  meanbystation$pacificcent <- 1/((((meanbystation$Latitude - 4.1)/21)^2 + ((meanbystation$Longitude - -122.4)/41)^2)^2+1)
  meanbystation$mexico <- 1/((((meanbystation$Latitude - 26)/6)^2 + ((meanbystation$Longitude - -98.4)/12)^2)^2+1)
  meanbystation$florida <- 1/((((meanbystation$Latitude - 27.5)/4)^2 + ((meanbystation$Longitude - -81.1)/8)^2)^2+1)
  meanbystation$pacificnorth <- 1/((((meanbystation$Latitude - 32.9)/26)^2 + ((meanbystation$Longitude - -145)/27)^2)^2+1)
  meanbystation$oklahoma <- 1/((((meanbystation$Latitude - 33.6)/4)^2 + ((meanbystation$Longitude - -98.4)/8)^2)^2+1)
  meanbystation$arizona <- 1/((((meanbystation$Latitude - 34)/12)^2 + ((meanbystation$Longitude - -113.1)/8)^2)^2+1)
  meanbystation$atlantic <- 1/((((meanbystation$Latitude - 34)/15)^2 + ((meanbystation$Longitude - -60.7)/19)^2)^2+1)
  meanbystation$himalayas <- 1/((((meanbystation$Latitude - 35.3)/6)^2 + ((meanbystation$Longitude - 91.3)/13)^2)^2+1)
  meanbystation$kentucky <- 1/((((meanbystation$Latitude - 38.5)/3)^2 + ((meanbystation$Longitude - -87.6)/9)^2)^2+1)
  meanbystation$detroit <- 1/((((meanbystation$Latitude - 41.8)/3)^2 + ((meanbystation$Longitude - -82.6)/4)^2)^2+1)
  meanbystation$ontario <- 1/((((meanbystation$Latitude - 44.6)/2)^2 + ((meanbystation$Longitude - -79.2)/6)^2)^2+1)
  meanbystation$montana <- 1/((((meanbystation$Latitude - 45.4)/5)^2 + ((meanbystation$Longitude - -111.8)/10)^2)^2+1)
  meanbystation$minn <- 1/((((meanbystation$Latitude - 47.6)/6)^2 + ((meanbystation$Longitude - -92.6)/12)^2)^2+1)
  meanbystation$hudson <- 1/((((meanbystation$Latitude - 60)/7)^2 + ((meanbystation$Longitude - -87)/34)^2)^2+1)
  meanbystation$siberia <- 1/((((meanbystation$Latitude - 61.2)/20)^2 + ((meanbystation$Longitude - 105.7)/39)^2)^2+1)
  meanbystation$california <- 1/((((meanbystation$Latitude - 34.8)/9)^2 + ((meanbystation$Longitude - -128.2)/9)^2)^2+1)
  meanbystation$washington <- 1/((((meanbystation$Latitude - 46)/5)^2 + ((meanbystation$Longitude - -126.6)/5)^2)^2+1)
  meanbystation$colorado <- 1/((((meanbystation$Latitude - 38.3)/2)^2 + ((meanbystation$Longitude - -108.8)/3)^2)^2+1)
  meanbystation$hawaii <- 1/((((meanbystation$Latitude - 21.3)/7)^2 + ((meanbystation$Longitude - -157.5)/11)^2)^2+1)
  meanbystation$chess <- 1/((((meanbystation$Latitude - 37)/3)^2 + ((meanbystation$Longitude - -74)/3)^2)^2+1)
  meanbystation$Tc <- apply(meanbystation[,c('t01', 't02', 't04', 't04', 't05', 't06', 't07', 't08', 't09', 't10', 't11', 't12')], 1, FUN = min)
  meanbystation$Tcl <- apply(meanbystation[,c('tl01', 'tl02', 'tl04', 'tl04', 'tl05', 'tl06', 'tl07', 'tl08', 'tl09', 'tl10', 'tl11', 'tl12')], 1, FUN = min)
  
  meanbystation$Tclx<-	-9.171	+
    meanbystation$Tcl *	1.202	+
    meanbystation$Latitude *	-0.04149	+
    meanbystation$Elevation *	0.0008691	+
    meanbystation$Latitude * meanbystation$Elevation *	-0.00002455	+
    meanbystation$pacificsouth *	-1.792	+
    meanbystation$amazon2 *	2.573	+
    meanbystation$amazon1 *	-1.014	+
    meanbystation$pacificcent *	-0.749	+
    meanbystation$mexico *	-0.8227	+
    meanbystation$florida *	-3.557	+
    meanbystation$pacificnorth *	-1.246	+
    meanbystation$oklahoma *	0.1758	+
    meanbystation$arizona *	2.605	+
    meanbystation$chess *	0.8347	+
    meanbystation$atlantic *	0.2967	+
    meanbystation$himalayas *	-1.814	+
    meanbystation$kentucky *	-2.644	+
    meanbystation$detroit *	0	+
    meanbystation$ontario *	-2.314	+
    meanbystation$montana *	-4.415	+
    meanbystation$minn *	1.136	+
    meanbystation$hudson *	-5.154	+
    meanbystation$siberia *	-3.797	+
    meanbystation$california *	4.48	+
    meanbystation$washington *	3.597	+
    meanbystation$colorado *	1.458	+
    meanbystation$hawaii *	6.673	
  
  meanbystation <- subset(meanbystation, select = -c(pacificsouth,amazon1,amazon2, pacificcent, mexico, florida,                                               pacificnorth, oklahoma, arizona, atlantic, himalayas, kentucky, 
                                                     detroit, ontario, montana, minn, hudson, siberia, california, washington, colorado, chess, hawaii))
  #Make Monthly Rows
  #Jan
  NormsMonthly2010 <- selectedNorm[,c("Station_ID","Station_Name","State","Latitude","Longitude","Elevation","tl01","t01","pp01")]
  NormsMonthly2010$Month<- 1
  colnames(NormsMonthly2010)[colnames(NormsMonthly2010) == 'tl01'] <- 'tlow'
  colnames(NormsMonthly2010)[colnames(NormsMonthly2010) == 't01'] <- 't'
  colnames(NormsMonthly2010)[colnames(NormsMonthly2010) == 'pp01'] <- 'pp'
  #Feb
  NormsMonthly2010A <- selectedNorm[,c("Station_ID","Station_Name","State","Latitude","Longitude","Elevation","tl02","t02","pp02")]
  colnames(NormsMonthly2010A)<-c("Station_ID","Station_Name","State","Latitude","Longitude","Elevation","tlow","t","pp")
  NormsMonthly2010A$Month<- 2
  NormsMonthly2010 <- rbind(NormsMonthly2010, NormsMonthly2010A)
  #Mar
  NormsMonthly2010A <- selectedNorm[,c("Station_ID","Station_Name","State","Latitude","Longitude","Elevation","tl03","t03","pp03")]
  colnames(NormsMonthly2010A)<-c("Station_ID","Station_Name","State","Latitude","Longitude","Elevation","tlow","t","pp")
  NormsMonthly2010A$Month<- 3
  NormsMonthly2010 <- rbind(NormsMonthly2010, NormsMonthly2010A)
  #Apr
  NormsMonthly2010A <- selectedNorm[,c("Station_ID","Station_Name","State","Latitude","Longitude","Elevation","tl04","t04","pp04")]
  colnames(NormsMonthly2010A)<-c("Station_ID","Station_Name","State","Latitude","Longitude","Elevation","tlow","t","pp")
  NormsMonthly2010A$Month<- 4
  NormsMonthly2010 <- rbind(NormsMonthly2010, NormsMonthly2010A)
  #May
  NormsMonthly2010A <- selectedNorm[,c("Station_ID","Station_Name","State","Latitude","Longitude","Elevation","tl05","t05","pp05")]
  colnames(NormsMonthly2010A)<-c("Station_ID","Station_Name","State","Latitude","Longitude","Elevation","tlow","t","pp")
  NormsMonthly2010A$Month<- 5
  NormsMonthly2010 <- rbind(NormsMonthly2010, NormsMonthly2010A)
  #Jun
  NormsMonthly2010A <- selectedNorm[,c("Station_ID","Station_Name","State","Latitude","Longitude","Elevation","tl06","t06","pp06")]
  colnames(NormsMonthly2010A)<-c("Station_ID","Station_Name","State","Latitude","Longitude","Elevation","tlow","t","pp")
  NormsMonthly2010A$Month<- 6
  NormsMonthly2010 <- rbind(NormsMonthly2010, NormsMonthly2010A)
  #Jul
  NormsMonthly2010A <- selectedNorm[,c("Station_ID","Station_Name","State","Latitude","Longitude","Elevation","tl07","t07","pp07")]
  colnames(NormsMonthly2010A)<-c("Station_ID","Station_Name","State","Latitude","Longitude","Elevation","tlow","t","pp")
  NormsMonthly2010A$Month<- 7
  NormsMonthly2010 <- rbind(NormsMonthly2010, NormsMonthly2010A)
  #Aug
  NormsMonthly2010A <- selectedNorm[,c("Station_ID","Station_Name","State","Latitude","Longitude","Elevation","tl08","t08","pp08")]
  colnames(NormsMonthly2010A)<-c("Station_ID","Station_Name","State","Latitude","Longitude","Elevation","tlow","t","pp")
  NormsMonthly2010A$Month<- 8
  NormsMonthly2010 <- rbind(NormsMonthly2010, NormsMonthly2010A)
  #Sep
  NormsMonthly2010A <- selectedNorm[,c("Station_ID","Station_Name","State","Latitude","Longitude","Elevation","tl09","t09","pp09")]
  colnames(NormsMonthly2010A)<-c("Station_ID","Station_Name","State","Latitude","Longitude","Elevation","tlow","t","pp")
  NormsMonthly2010A$Month<- 9
  NormsMonthly2010 <- rbind(NormsMonthly2010, NormsMonthly2010A)
  #Oct
  NormsMonthly2010A <- selectedNorm[,c("Station_ID","Station_Name","State","Latitude","Longitude","Elevation","tl10","t10","pp10")]
  colnames(NormsMonthly2010A)<-c("Station_ID","Station_Name","State","Latitude","Longitude","Elevation","tlow","t","pp")
  NormsMonthly2010A$Month<- 10
  NormsMonthly2010 <- rbind(NormsMonthly2010, NormsMonthly2010A)
  #Nov
  NormsMonthly2010A <- selectedNorm[,c("Station_ID","Station_Name","State","Latitude","Longitude","Elevation","tl11","t11","pp11")]
  colnames(NormsMonthly2010A)<-c("Station_ID","Station_Name","State","Latitude","Longitude","Elevation","tlow","t","pp")
  NormsMonthly2010A$Month<- 11
  NormsMonthly2010 <- rbind(NormsMonthly2010, NormsMonthly2010A)
  #Dec
  NormsMonthly2010A <- selectedNorm[,c("Station_ID","Station_Name","State","Latitude","Longitude","Elevation","tl12","t12","pp12")]
  colnames(NormsMonthly2010A)<-c("Station_ID","Station_Name","State","Latitude","Longitude","Elevation","tlow","t","pp")
  NormsMonthly2010A$Month<- 12
  NormsMonthly2010 <- rbind(NormsMonthly2010, NormsMonthly2010A)
  #Reorder Columns
  NormsMonthly2010<-NormsMonthly2010[,c("Station_ID","Station_Name","State","Latitude","Longitude","Elevation","Month","tlow","t","pp")]
  #biotemperature
  NormsMonthly2010$BioT <- (NormsMonthly2010$t>0)*NormsMonthly2010$t
  #Growing Degree Temperature
  NormsMonthly2010$GDT <- (NormsMonthly2010$t-5>0)*(NormsMonthly2010$t-5)+5
  #biotemperature*precipitation
  #NormsMonthly2010$BP <- NormsMonthly2010$p*NormsMonthly2010$BioT
  
  
  #convert cm to mm for consistency -obsolete- #NormsMonthly2010$pp<-NormsMonthly2010$pp*10
  
  #daylength
  
  DaysMonth$declination <- 0.409*sin(2*3.141592*DaysMonth$Day_/365-1.39)
  
  Daylength <- merge(unique(selectedNorm[,1:6]), DaysMonth)
  
  Daylength$Daylength <- ifelse(Daylength$Latitude + Daylength$declination*360/2/3.141592 >= 90, 24, ifelse(Daylength$Latitude - Daylength$declination*360/2/3.141592 >= 90, 0, (atan(-((sin(-0.83/360*2*3.141592)-sin(Daylength$declination)*sin(Daylength$Latitude/360*2*3.141592))/(cos(Daylength$declination)*cos(Daylength$Latitude/360*2*3.141592)))/(-((sin(-0.83/360*2*3.141592)-sin(Daylength$declination)*sin(Daylength$Latitude/360*2*3.141592))/(cos(Daylength$declination)*cos(Daylength$Latitude/360*2*3.141592)))*((sin(-0.83/360*2*3.141592)-sin(Daylength$declination)*sin(Daylength$Latitude/360*2*3.141592))/(cos(Daylength$declination)*cos(Daylength$Latitude/360*2*3.141592)))+1)^0.5)+2*atan(1))/3.141592*24))
  
  #add daylength to dataset
  NormsMonthly2010 <- merge(NormsMonthly2010,Daylength[,c("Station_ID","Station_Name","Month_","Days","Daylength")],by.x=c("Station_ID","Station_Name","Month"),by.y=c("Station_ID","Station_Name","Month_"))
  NormsMonthly2010 <-na.omit(NormsMonthly2010)
  #PET
  NormsMonthly2010$PET <- 216.7*6.108*exp(17.26939*NormsMonthly2010$t/(NormsMonthly2010$t+237.3))/(NormsMonthly2010$t+273.3)*0.1651*(NormsMonthly2010$Daylength/12)*NormsMonthly2010$Days*0.2606*abs((NormsMonthly2010$t-NormsMonthly2010$tlow)*2)^0.5 + 0.001
  #Growindex
  NormsMonthly2010$Gindex <-pmin(1,NormsMonthly2010$p/NormsMonthly2010$PET)*(pmax(0,NormsMonthly2010$t-5)*0.8+pmax(0,NormsMonthly2010$t)*0.2)
  #monthly relative deficit
  NormsMonthly2010$D<- 1-pmin(NormsMonthly2010$PET,NormsMonthly2010$p)/NormsMonthly2010$PET
  #Growth weighted temperature
  NormsMonthly2010$GwtT<-NormsMonthly2010$Gindex*NormsMonthly2010$t
  #monthly AET
  NormsMonthly2010$AET <- pmin(NormsMonthly2010$PET,NormsMonthly2010$p)
  
  #Make One Column
  NormMonthT <- NormsMonthly2010[,c("Station_ID","Station_Name","State","Latitude","Longitude","Elevation","Month","t")]
  NormMonthT$param<-"t"
  colnames(NormMonthT)<-c("Station_ID","Station_Name","State","Latitude","Longitude","Elevation","Month","v","param")
  NormMonthT$Month<-factor(NormMonthT$Month)
  AggrNormMonthT <- ddply(NormMonthT, "Month", summarise, v000 = quantile(v, .0),v005 = quantile(v, .05),v025 = quantile(v, .25),v050 = mean(v),v075 = quantile(v, .75), v095 = quantile(v, .95),v100 = quantile(v, 1))
  rm(NormMonthT)
  
  NormMonthTl <- NormsMonthly2010[,c("Station_ID","Station_Name","State","Latitude","Longitude","Elevation","Month","tlow","t")]
  NormMonthTl$thigh <- NormMonthTl$t * 2 - NormMonthTl$tlow
  NormMonthTl$Month<-factor(NormMonthTl$Month)
  AggrNormMonthTl <- ddply(NormMonthTl, "Month", summarise, v050 = mean(tlow) ,th050 = mean(thigh))
  rm(NormMonthTl)
  
  NormMonthP <- NormsMonthly2010[,c("Station_ID","Station_Name","State","Latitude","Longitude","Elevation","Month","pp")]
  NormMonthP$param<-"p"
  colnames(NormMonthP)<-c("Station_ID","Station_Name","State","Latitude","Longitude","Elevation","Month","v","param")
  NormMonthP$v <-NormMonthP$v*0.2
  NormMonthP$Month<-factor(NormMonthP$Month)
  AggrNormMonthP <- ddply(NormMonthP, "Month", summarise, v000 = quantile(v, .0),v005 = quantile(v, .05),v025 = quantile(v, .25), v050 = mean(v), v075 = quantile(v, .75), v095 = quantile(v, .95),v100 = quantile(v, 1))
  rm(NormMonthP)
  
  NormMonthE <- NormsMonthly2010[,c("Station_ID","Station_Name","State","Latitude","Longitude","Elevation","Month","PET")]
  NormMonthE$param<-"E"
  colnames(NormMonthE)<-c("Station_ID","Station_Name","State","Latitude","Longitude","Elevation","Month","v","param")
  NormMonthE$v <-NormMonthE$v*0.2
  NormMonthE$Month<-factor(NormMonthE$Month)
  AggrNormMonthE <- ddply(NormMonthE, "Month", summarise, v000 = quantile(v, .0),v005 = quantile(v, .05),v025 = quantile(v, .25), v050 = mean(v), v075 = quantile(v, .75), v095 = quantile(v, .95),v100 = quantile(v, 1))
  rm(NormMonthE)
  
  NormMonthB <- NormsMonthly2010[,c("Station_ID","Station_Name","State","Latitude","Longitude","Elevation","Month","BioT")]
  NormMonthB$param<-"B"
  colnames(NormMonthB)<-c("Station_ID","Station_Name","State","Latitude","Longitude","Elevation","Month","v","param")
  NormMonthB$Month<-factor(NormMonthB$Month)
  AggrNormMonthB <- ddply(NormMonthB, "Month", summarise, v000 = quantile(v, .0),v005 = quantile(v, .05),v025 = quantile(v, .25), v050 = mean(v), v075 = quantile(v, .75), v095 = quantile(v, .95),v100 = quantile(v, 1))
  rm(NormMonthB)
  
  NormMonthGindex <- NormsMonthly2010[,c("Station_ID","Station_Name","State","Latitude","Longitude","Elevation","Month","Gindex")]
  NormMonthGindex$param<-"Gindex"
  colnames(NormMonthGindex)<-c("Station_ID","Station_Name","State","Latitude","Longitude","Elevation","Month","v","param")
  NormMonthGindex$Month<-factor(NormMonthGindex$Month)
  AggrNormMonthGindex <- ddply(NormMonthGindex, "Month", summarise, v000 = quantile(v, .0),v005 = quantile(v, .05),v025 = quantile(v, .25), v050 = mean(v), v075 = quantile(v, .75), v095 = quantile(v, .95),v100 = quantile(v, 1))
  rm(NormMonthGindex)
  
  
  #station means
  NormsMonthly2010mean <-aggregate(NormsMonthly2010[,c("tlow","t","pp","BioT","GDT","Days","Daylength","PET","Gindex","GwtT")], by=list(NormsMonthly2010$Station_ID,NormsMonthly2010$Station_Name,NormsMonthly2010$State,NormsMonthly2010$Latitude,NormsMonthly2010$Longitude,NormsMonthly2010$Elevation,NormsMonthly2010$Month),FUN=mean)
  colnames(NormsMonthly2010mean)<-c("Station_ID","Station_Name","State","Latitude","Longitude","Elevation","Month","tlow","t","pp","BioT","GDT","Days","Daylength","PET","Gindex","GwtT")
  NormsMonthly2010mean$D <- 1-pmin(NormsMonthly2010mean$PET,NormsMonthly2010mean$pp)/NormsMonthly2010mean$PET
  #biotemperature*precipitation
  #NormsMonthly2010mean$BP <- NormsMonthly2010mean$pp * NormsMonthly2010mean$BioT
  Warmth<- meanbystation[,c( 'Station_ID', 'Bt6')]
  Cold<- meanbystation[,c( 'Station_ID', 'tlo')]
  #sumBP<-aggregate(NormsMonthly2010$BP, by=list(NormsMonthly2010$Station_ID),FUN=mean)
  #colnames(sumBP)<- c("Station_ID","BP")
  sumB<- cbind.data.frame(meanbystation$Station_ID, apply(meanbystation[,c('b01','b02','b03','b04','b05','b06','b07','b08','b09','b10','b11','b12')], 1, mean))
  colnames(sumB)<- c("Station_ID","BioT")
  meanWinterLow<-aggregate(NormsMonthly2010[,c("tlow","t")], by=list(NormsMonthly2010$Station_ID),FUN=min)
  colnames(meanWinterLow)<- c("Station_ID","tlowmin","tlow")
  meanSummerGDT<-aggregate(NormsMonthly2010$GDT, by=list(NormsMonthly2010$Station_ID),FUN=mean)
  colnames(meanSummerGDT)<- c("Station_ID","GDT")
  sumGindex<- cbind.data.frame(meanbystation$Station_ID, apply(meanbystation[,c('g01','g02','g03','g04','g05','g06','g07','g08','g09','g10','g11','g12')], 1, sum))
  colnames(sumGindex)<- c("Station_ID","GDTsum")
  maxGindex<- cbind.data.frame(meanbystation$Station_ID, apply(meanbystation[,c('g01','g02','g03','g04','g05','g06','g07','g08','g09','g10','g11','g12')], 1, max))
  colnames(maxGindex)<- c("Station_ID","GDTmax")
  sumPET<- cbind.data.frame(meanbystation$Station_ID, apply(meanbystation[,c('e01','e02','e03','e04','e05','e06','e07','e08','e09','e10','e11','e12')], 1, sum))
  colnames(sumPET)<- c("Station_ID","PET")
  sumP<- cbind.data.frame(meanbystation$Station_ID, apply(meanbystation[,c('pp01','pp02','pp03','pp04','pp05','pp06','pp07','pp08','pp09','pp10','pp11','pp12')], 1, sum))
  colnames(sumP)<- c("Station_ID","p")
  sumAET<- cbind.data.frame(meanbystation$Station_ID, apply(meanbystation[,c('a01','a02','a03','a04','a05','a06','a07','a08','a09','a10','a11','a12')], 1, sum))
  colnames(sumAET)<- c("Station_ID","AET")
  maxAET<- cbind.data.frame(meanbystation$Station_ID, apply(meanbystation[,c('a01','a02','a03','a04','a05','a06','a07','a08','a09','a10','a11','a12')], 1, max))
  colnames(maxAET)<- c("Station_ID","maxAET")
  meanT<- cbind.data.frame(meanbystation$Station_ID, apply(meanbystation[,c('t01','t02','t03','t04','t05','t06','t07','t08','t09','t10','t11','t12')], 1, mean))
  colnames(meanT)<- c("Station_ID","t")
  meanMaxT<- cbind.data.frame(meanbystation$Station_ID, apply(meanbystation[,c('t01','t02','t03','t04','t05','t06','t07','t08','t09','t10','t11','t12')], 1, max))
  colnames(meanMaxT)<- c("Station_ID","tx")
  meanMaxTHigh<- cbind.data.frame(meanbystation$Station_ID, apply(meanbystation[,c('th01','th02','th03','th04','th05','th06','th07','th08','th09','th10','th11','th12')], 1, max))
  colnames(meanMaxTHigh)<- c("Station_ID","txh")
  meanMinT<- cbind.data.frame(meanbystation$Station_ID, apply(meanbystation[,c('t01','t02','t03','t04','t05','t06','t07','t08','t09','t10','t11','t12')], 1, min))
  colnames(meanMinT)<- c("Station_ID","tn")
  meanMinTLow<- cbind.data.frame(meanbystation$Station_ID, apply(meanbystation[,c('tl01','tl02','tl03','tl04','tl05','tl06','tl07','tl08','tl09','tl10','tl11','tl12')], 1, min))
  colnames(meanMinTLow)<- c("Station_ID","tnl")
  DSI<-aggregate(NormsMonthly2010mean$D, by=list(NormsMonthly2010mean$Station_ID), FUN=sd)
  colnames(DSI)<- c("Station_ID","dsD")
  DSI$DSI<-DSI$dsD*sqrt(11/12)*2
  DSIi<-aggregate(NormsMonthly2010$D, by=list(NormsMonthly2010$Station_ID), FUN=sd)
  colnames(DSIi)<- c("Station_ID","dsD")
  DSIi$DSIi<-DSIi$dsD*sqrt((12*30-1)/(12*30))*2
  sumGwtT<-aggregate(NormsMonthly2010$GwtT, by=list(NormsMonthly2010$Station_ID),FUN=sum)
  colnames(sumGwtT)<- c("Station_ID","GwtTsum")
  GwtTotal<-merge(sumGindex,sumGwtT,by="Station_ID")
  GwtTotal<-merge(maxGindex, GwtTotal,by="Station_ID")
  GwtTotal$GwtTT<-GwtTotal$GwtT/GwtTotal$GDTsum
  #warmrain<-aggregate(NormsMonthly2010$p*(NormsMonthly2010$t>=15),by=list(NormsMonthly2010$Station_ID),FUN=sum)
  #colnames(warmrain)<- c("Station_ID","warmrain")
  #coolrain<-aggregate(NormsMonthly2010$p*(NormsMonthly2010$t<15),by=list(NormsMonthly2010$Station_ID),FUN=sum)
  #colnames(coolrain)<- c("Station_ID","coolrain")
  meanStation<-merge(Warmth,meanSummerGDT,by="Station_ID")
  meanStation<-merge(meanStation,Cold,by="Station_ID")
  #meanStation<-merge(meanStation,sumBP,by="Station_ID")
  meanStation<-merge(meanStation,sumB,by="Station_ID")
  meanStation<-merge(meanStation,sumP,by="Station_ID")
  meanStation<-merge(meanStation,sumAET,by="Station_ID")
  meanStation<-merge(meanStation,maxAET,by="Station_ID")
  meanStation<-merge(meanStation,meanT,by="Station_ID")
  meanStation<-merge(meanStation,sumPET,by="Station_ID")
  meanStation<-merge(meanStation,meanWinterLow,by="Station_ID")
  meanStation<-merge(meanStation,GwtTotal[,c(1,2,5)],by="Station_ID")
  meanStation<-merge(meanStation,meanMaxTHigh,by="Station_ID")
  meanStation<-merge(meanStation,meanMaxT,by="Station_ID")
  meanStation<-merge(meanStation,meanMinT,by="Station_ID")
  meanStation<-merge(meanStation,meanMinTLow,by="Station_ID")
  meanStation$M<-meanStation$p/meanStation$PET
  meanStation<-merge(meanStation,DSI[,c("Station_ID","DSI")],by="Station_ID")
  meanStation$AETR<- meanStation$AET/(meanStation$PET+0.001)
  meanStation$Surplus <-meanStation$p-meanStation$AET
  meanStation$Deficit <-meanStation$PET-meanStation$AET
  meanStation<-merge(meanStation,DSIi[,c("Station_ID","DSIi")],by="Station_ID")
  rm(Warmth, Cold, meanWinterLow,sumGwtT,meanT,maxGindex,sumGindex,GwtTotal,sumP, sumB,sumAET, maxAET, sumPET,DSI,DSIi,meanMaxTHigh,meanMinT,meanMaxT,meanMinTLow)
  #rm(Warmth, Cold, sumBP, meanWinterLow,sumGwtT,meanT,maxGindex,sumGindex,GwtTotal,sumP,sumAET, sumPET,coolrain,warmrain,DSI,DSIi)
  
  
  
  #Quarterly Indices_merged_________________________________________________________________
  Eq01<-aggregate(NormsMonthly2010mean[NormsMonthly2010mean$Month %in% c("11","12","1"),]$PET, by=list(NormsMonthly2010mean[NormsMonthly2010mean$Month %in% c("11","12","1"),]$Station_ID),FUN=sum)
  colnames(Eq01)<- c("ID","Eq01")
  Eq02<-aggregate(NormsMonthly2010mean[NormsMonthly2010mean$Month %in% c("12","1","2"),]$PET, by=list(NormsMonthly2010mean[NormsMonthly2010mean$Month %in% c("12","1","2"),]$Station_ID),FUN=sum)
  colnames(Eq02)<- c("ID","Eq02")
  Eq03<-aggregate(NormsMonthly2010mean[NormsMonthly2010mean$Month %in% c("1","2","3"),]$PET, by=list(NormsMonthly2010mean[NormsMonthly2010mean$Month %in% c("1","2","3"),]$Station_ID),FUN=sum)
  colnames(Eq03)<- c("ID","Eq03")
  Eq04<-aggregate(NormsMonthly2010mean[NormsMonthly2010mean$Month %in% c("2","3","4"),]$PET, by=list(NormsMonthly2010mean[NormsMonthly2010mean$Month %in% c("2","3","4"),]$Station_ID),FUN=sum)
  colnames(Eq04)<- c("ID","Eq04")
  Eq05<-aggregate(NormsMonthly2010mean[NormsMonthly2010mean$Month %in% c("3","4","5"),]$PET, by=list(NormsMonthly2010mean[NormsMonthly2010mean$Month %in% c("3","4","5"),]$Station_ID),FUN=sum)
  colnames(Eq05)<- c("ID","Eq05")
  Eq06<-aggregate(NormsMonthly2010mean[NormsMonthly2010mean$Month %in% c("4","5","6"),]$PET, by=list(NormsMonthly2010mean[NormsMonthly2010mean$Month %in% c("4","5","6"),]$Station_ID),FUN=sum)
  colnames(Eq06)<- c("ID","Eq06")
  Eq07<-aggregate(NormsMonthly2010mean[NormsMonthly2010mean$Month %in% c("5","6","7"),]$PET, by=list(NormsMonthly2010mean[NormsMonthly2010mean$Month %in% c("5","6","7"),]$Station_ID),FUN=sum)
  colnames(Eq07)<- c("ID","Eq07")
  Eq08<-aggregate(NormsMonthly2010mean[NormsMonthly2010mean$Month %in% c("6","7","8"),]$PET, by=list(NormsMonthly2010mean[NormsMonthly2010mean$Month %in% c("6","7","8"),]$Station_ID),FUN=sum)
  colnames(Eq08)<- c("ID","Eq08")
  Eq09<-aggregate(NormsMonthly2010mean[NormsMonthly2010mean$Month %in% c("7","8","9"),]$PET, by=list(NormsMonthly2010mean[NormsMonthly2010mean$Month %in% c("7","8","9"),]$Station_ID),FUN=sum)
  colnames(Eq09)<- c("ID","Eq09")
  Eq10<-aggregate(NormsMonthly2010mean[NormsMonthly2010mean$Month %in% c("8","9","10"),]$PET, by=list(NormsMonthly2010mean[NormsMonthly2010mean$Month %in% c("8","9","10"),]$Station_ID),FUN=sum)
  colnames(Eq10)<- c("ID","Eq10")
  Eq11<-aggregate(NormsMonthly2010mean[NormsMonthly2010mean$Month %in% c("9","10","11"),]$PET, by=list(NormsMonthly2010mean[NormsMonthly2010mean$Month %in% c("9","10","11"),]$Station_ID),FUN=sum)
  colnames(Eq11)<- c("ID","Eq11")
  Eq12<-aggregate(NormsMonthly2010mean[NormsMonthly2010mean$Month %in% c("10","11","12"),]$PET, by=list(NormsMonthly2010mean[NormsMonthly2010mean$Month %in% c("10","11","12"),]$Station_ID),FUN=sum)
  colnames(Eq12)<- c("ID","Eq12")
  #
  Pq01<-aggregate(NormsMonthly2010mean[NormsMonthly2010mean$Month %in% c("11","12","1"),]$p, by=list(NormsMonthly2010mean[NormsMonthly2010mean$Month %in% c("11","12","1"),]$Station_ID),FUN=sum)
  colnames(Pq01)<- c("ID","Pq01")
  Pq02<-aggregate(NormsMonthly2010mean[NormsMonthly2010mean$Month %in% c("12","1","2"),]$p, by=list(NormsMonthly2010mean[NormsMonthly2010mean$Month %in% c("12","1","2"),]$Station_ID),FUN=sum)
  colnames(Pq02)<- c("ID","Pq02")
  Pq03<-aggregate(NormsMonthly2010mean[NormsMonthly2010mean$Month %in% c("1","2","3"),]$p, by=list(NormsMonthly2010mean[NormsMonthly2010mean$Month %in% c("1","2","3"),]$Station_ID),FUN=sum)
  colnames(Pq03)<- c("ID","Pq03")
  Pq04<-aggregate(NormsMonthly2010mean[NormsMonthly2010mean$Month %in% c("2","3","4"),]$p, by=list(NormsMonthly2010mean[NormsMonthly2010mean$Month %in% c("2","3","4"),]$Station_ID),FUN=sum)
  colnames(Pq04)<- c("ID","Pq04")
  Pq05<-aggregate(NormsMonthly2010mean[NormsMonthly2010mean$Month %in% c("3","4","5"),]$p, by=list(NormsMonthly2010mean[NormsMonthly2010mean$Month %in% c("3","4","5"),]$Station_ID),FUN=sum)
  colnames(Pq05)<- c("ID","Pq05")
  Pq06<-aggregate(NormsMonthly2010mean[NormsMonthly2010mean$Month %in% c("4","5","6"),]$p, by=list(NormsMonthly2010mean[NormsMonthly2010mean$Month %in% c("4","5","6"),]$Station_ID),FUN=sum)
  colnames(Pq06)<- c("ID","Pq06")
  Pq07<-aggregate(NormsMonthly2010mean[NormsMonthly2010mean$Month %in% c("5","6","7"),]$p, by=list(NormsMonthly2010mean[NormsMonthly2010mean$Month %in% c("5","6","7"),]$Station_ID),FUN=sum)
  colnames(Pq07)<- c("ID","Pq07")
  Pq08<-aggregate(NormsMonthly2010mean[NormsMonthly2010mean$Month %in% c("6","7","8"),]$p, by=list(NormsMonthly2010mean[NormsMonthly2010mean$Month %in% c("6","7","8"),]$Station_ID),FUN=sum)
  colnames(Pq08)<- c("ID","Pq08")
  Pq09<-aggregate(NormsMonthly2010mean[NormsMonthly2010mean$Month %in% c("7","8","9"),]$p, by=list(NormsMonthly2010mean[NormsMonthly2010mean$Month %in% c("7","8","9"),]$Station_ID),FUN=sum)
  colnames(Pq09)<- c("ID","Pq09")
  Pq10<-aggregate(NormsMonthly2010mean[NormsMonthly2010mean$Month %in% c("8","9","10"),]$p, by=list(NormsMonthly2010mean[NormsMonthly2010mean$Month %in% c("8","9","10"),]$Station_ID),FUN=sum)
  colnames(Pq10)<- c("ID","Pq10")
  Pq11<-aggregate(NormsMonthly2010mean[NormsMonthly2010mean$Month %in% c("9","10","11"),]$p, by=list(NormsMonthly2010mean[NormsMonthly2010mean$Month %in% c("9","10","11"),]$Station_ID),FUN=sum)
  colnames(Pq11)<- c("ID","Pq11")
  Pq12<-aggregate(NormsMonthly2010mean[NormsMonthly2010mean$Month %in% c("10","11","12"),]$p, by=list(NormsMonthly2010mean[NormsMonthly2010mean$Month %in% c("10","11","12"),]$Station_ID),FUN=sum)
  colnames(Pq12)<- c("ID","Pq12")
  
  
  QuarterlyNormals<-merge(Eq01,Pq01,by="ID")
  QuarterlyNormals<-merge(QuarterlyNormals,Eq02,by="ID")
  QuarterlyNormals<-merge(QuarterlyNormals,Pq02,by="ID")
  QuarterlyNormals<-merge(QuarterlyNormals,Eq03,by="ID")
  QuarterlyNormals<-merge(QuarterlyNormals,Pq03,by="ID")
  QuarterlyNormals<-merge(QuarterlyNormals,Eq04,by="ID")
  QuarterlyNormals<-merge(QuarterlyNormals,Pq04,by="ID")
  QuarterlyNormals<-merge(QuarterlyNormals,Eq05,by="ID")
  QuarterlyNormals<-merge(QuarterlyNormals,Pq05,by="ID")
  QuarterlyNormals<-merge(QuarterlyNormals,Eq06,by="ID")
  QuarterlyNormals<-merge(QuarterlyNormals,Pq06,by="ID")
  QuarterlyNormals<-merge(QuarterlyNormals,Eq07,by="ID")
  QuarterlyNormals<-merge(QuarterlyNormals,Pq07,by="ID")
  QuarterlyNormals<-merge(QuarterlyNormals,Eq08,by="ID")
  QuarterlyNormals<-merge(QuarterlyNormals,Pq08,by="ID")
  QuarterlyNormals<-merge(QuarterlyNormals,Eq09,by="ID")
  QuarterlyNormals<-merge(QuarterlyNormals,Pq09,by="ID")
  QuarterlyNormals<-merge(QuarterlyNormals,Eq10,by="ID")
  QuarterlyNormals<-merge(QuarterlyNormals,Pq10,by="ID")
  QuarterlyNormals<-merge(QuarterlyNormals,Eq11,by="ID")
  QuarterlyNormals<-merge(QuarterlyNormals,Pq11,by="ID")
  QuarterlyNormals<-merge(QuarterlyNormals,Eq12,by="ID")
  QuarterlyNormals<-merge(QuarterlyNormals,Pq12,by="ID")
  #aggregate all stations
  QuarterlyNormals$all<-"all"
  QuarterlyNormalsAgg<-aggregate(QuarterlyNormals[,c(2:25)], by=list(QuarterlyNormals$all), FUN=mean)
  QuarterlyNormalsAgg$Aq01<-apply(QuarterlyNormalsAgg[,c("Eq01","Pq01")],1,min)
  QuarterlyNormalsAgg$Aq02<-apply(QuarterlyNormalsAgg[,c("Eq02","Pq02")],1,min)
  QuarterlyNormalsAgg$Aq03<-apply(QuarterlyNormalsAgg[,c("Eq03","Pq03")],1,min)
  QuarterlyNormalsAgg$Aq04<-apply(QuarterlyNormalsAgg[,c("Eq04","Pq04")],1,min)
  QuarterlyNormalsAgg$Aq05<-apply(QuarterlyNormalsAgg[,c("Eq05","Pq05")],1,min)
  QuarterlyNormalsAgg$Aq06<-apply(QuarterlyNormalsAgg[,c("Eq06","Pq06")],1,min)
  QuarterlyNormalsAgg$Aq07<-apply(QuarterlyNormalsAgg[,c("Eq07","Pq07")],1,min)
  QuarterlyNormalsAgg$Aq08<-apply(QuarterlyNormalsAgg[,c("Eq08","Pq08")],1,min)
  QuarterlyNormalsAgg$Aq09<-apply(QuarterlyNormalsAgg[,c("Eq09","Pq09")],1,min)
  QuarterlyNormalsAgg$Aq10<-apply(QuarterlyNormalsAgg[,c("Eq10","Pq10")],1,min)
  QuarterlyNormalsAgg$Aq11<-apply(QuarterlyNormalsAgg[,c("Eq11","Pq11")],1,min)
  QuarterlyNormalsAgg$Aq12<-apply(QuarterlyNormalsAgg[,c("Eq12","Pq12")],1,min)
  QuarterlyNormalsAgg$Emax<-apply(QuarterlyNormalsAgg[,c("Eq01","Eq02","Eq03","Eq04","Eq05","Eq06","Eq07","Eq08","Eq09","Eq10","Eq11","Eq12")],1,max)
  QuarterlyNormalsAgg$Pmax<-apply(QuarterlyNormalsAgg[,c("Pq01","Pq02","Pq03","Pq04","Pq05","Pq06","Pq07","Pq08","Pq09","Pq10","Pq11","Pq12")],1,max)
  QuarterlyNormalsAgg$Pmin<-apply(QuarterlyNormalsAgg[,c("Pq01","Pq02","Pq03","Pq04","Pq05","Pq06","Pq07","Pq08","Pq09","Pq10","Pq11","Pq12")],1,min)
  QuarterlyNormalsAgg$Amax<-apply(QuarterlyNormalsAgg[,c("Aq01","Aq02","Aq03","Aq04","Aq05","Aq06","Aq07","Aq08","Aq09","Aq10","Aq11","Aq12")],1,max)
  
  QuarterlyNormalsAgg$Mq01<-QuarterlyNormalsAgg$Aq01/QuarterlyNormalsAgg$Eq01
  QuarterlyNormalsAgg$Mq02<-QuarterlyNormalsAgg$Aq02/QuarterlyNormalsAgg$Eq02
  QuarterlyNormalsAgg$Mq03<-QuarterlyNormalsAgg$Aq03/QuarterlyNormalsAgg$Eq03
  QuarterlyNormalsAgg$Mq04<-QuarterlyNormalsAgg$Aq04/QuarterlyNormalsAgg$Eq04
  QuarterlyNormalsAgg$Mq05<-QuarterlyNormalsAgg$Aq05/QuarterlyNormalsAgg$Eq05
  QuarterlyNormalsAgg$Mq06<-QuarterlyNormalsAgg$Aq06/QuarterlyNormalsAgg$Eq06
  QuarterlyNormalsAgg$Mq07<-QuarterlyNormalsAgg$Aq07/QuarterlyNormalsAgg$Eq07
  QuarterlyNormalsAgg$Mq08<-QuarterlyNormalsAgg$Aq08/QuarterlyNormalsAgg$Eq08
  QuarterlyNormalsAgg$Mq09<-QuarterlyNormalsAgg$Aq09/QuarterlyNormalsAgg$Eq09
  QuarterlyNormalsAgg$Mq10<-QuarterlyNormalsAgg$Aq10/QuarterlyNormalsAgg$Eq10
  QuarterlyNormalsAgg$Mq11<-QuarterlyNormalsAgg$Aq11/QuarterlyNormalsAgg$Eq11
  QuarterlyNormalsAgg$Mq12<-QuarterlyNormalsAgg$Aq12/QuarterlyNormalsAgg$Eq12
  
  QuarterlyNormalsAgg$rEq01<-QuarterlyNormalsAgg$Eq01/QuarterlyNormalsAgg$Emax
  QuarterlyNormalsAgg$rEq02<-QuarterlyNormalsAgg$Eq02/QuarterlyNormalsAgg$Emax
  QuarterlyNormalsAgg$rEq03<-QuarterlyNormalsAgg$Eq03/QuarterlyNormalsAgg$Emax
  QuarterlyNormalsAgg$rEq04<-QuarterlyNormalsAgg$Eq04/QuarterlyNormalsAgg$Emax
  QuarterlyNormalsAgg$rEq05<-QuarterlyNormalsAgg$Eq05/QuarterlyNormalsAgg$Emax
  QuarterlyNormalsAgg$rEq06<-QuarterlyNormalsAgg$Eq06/QuarterlyNormalsAgg$Emax
  QuarterlyNormalsAgg$rEq07<-QuarterlyNormalsAgg$Eq07/QuarterlyNormalsAgg$Emax
  QuarterlyNormalsAgg$rEq08<-QuarterlyNormalsAgg$Eq08/QuarterlyNormalsAgg$Emax
  QuarterlyNormalsAgg$rEq09<-QuarterlyNormalsAgg$Eq09/QuarterlyNormalsAgg$Emax
  QuarterlyNormalsAgg$rEq10<-QuarterlyNormalsAgg$Eq10/QuarterlyNormalsAgg$Emax
  QuarterlyNormalsAgg$rEq11<-QuarterlyNormalsAgg$Eq11/QuarterlyNormalsAgg$Emax
  QuarterlyNormalsAgg$rEq12<-QuarterlyNormalsAgg$Eq12/QuarterlyNormalsAgg$Emax
  
  QuarterlyNormalsAgg$rPq01<-QuarterlyNormalsAgg$Pq01/QuarterlyNormalsAgg$Pmax
  QuarterlyNormalsAgg$rPq02<-QuarterlyNormalsAgg$Pq02/QuarterlyNormalsAgg$Pmax
  QuarterlyNormalsAgg$rPq03<-QuarterlyNormalsAgg$Pq03/QuarterlyNormalsAgg$Pmax
  QuarterlyNormalsAgg$rPq04<-QuarterlyNormalsAgg$Pq04/QuarterlyNormalsAgg$Pmax
  QuarterlyNormalsAgg$rPq05<-QuarterlyNormalsAgg$Pq05/QuarterlyNormalsAgg$Pmax
  QuarterlyNormalsAgg$rPq06<-QuarterlyNormalsAgg$Pq06/QuarterlyNormalsAgg$Pmax
  QuarterlyNormalsAgg$rPq07<-QuarterlyNormalsAgg$Pq07/QuarterlyNormalsAgg$Pmax
  QuarterlyNormalsAgg$rPq08<-QuarterlyNormalsAgg$Pq08/QuarterlyNormalsAgg$Pmax
  QuarterlyNormalsAgg$rPq09<-QuarterlyNormalsAgg$Pq09/QuarterlyNormalsAgg$Pmax
  QuarterlyNormalsAgg$rPq10<-QuarterlyNormalsAgg$Pq10/QuarterlyNormalsAgg$Pmax
  QuarterlyNormalsAgg$rPq11<-QuarterlyNormalsAgg$Pq11/QuarterlyNormalsAgg$Pmax
  QuarterlyNormalsAgg$rPq12<-QuarterlyNormalsAgg$Pq12/QuarterlyNormalsAgg$Pmax
  
  QuarterlyNormalsAgg$rAq01<-apply(QuarterlyNormalsAgg[,c("rEq01","rPq01")],1,min)
  QuarterlyNormalsAgg$rAq02<-apply(QuarterlyNormalsAgg[,c("rEq02","rPq02")],1,min)
  QuarterlyNormalsAgg$rAq03<-apply(QuarterlyNormalsAgg[,c("rEq03","rPq03")],1,min)
  QuarterlyNormalsAgg$rAq04<-apply(QuarterlyNormalsAgg[,c("rEq04","rPq04")],1,min)
  QuarterlyNormalsAgg$rAq05<-apply(QuarterlyNormalsAgg[,c("rEq05","rPq05")],1,min)
  QuarterlyNormalsAgg$rAq06<-apply(QuarterlyNormalsAgg[,c("rEq06","rPq06")],1,min)
  QuarterlyNormalsAgg$rAq07<-apply(QuarterlyNormalsAgg[,c("rEq07","rPq07")],1,min)
  QuarterlyNormalsAgg$rAq08<-apply(QuarterlyNormalsAgg[,c("rEq08","rPq08")],1,min)
  QuarterlyNormalsAgg$rAq09<-apply(QuarterlyNormalsAgg[,c("rEq09","rPq09")],1,min)
  QuarterlyNormalsAgg$rAq10<-apply(QuarterlyNormalsAgg[,c("rEq10","rPq10")],1,min)
  QuarterlyNormalsAgg$rAq11<-apply(QuarterlyNormalsAgg[,c("rEq11","rPq11")],1,min)
  QuarterlyNormalsAgg$rAq12<-apply(QuarterlyNormalsAgg[,c("rEq12","rPq12")],1,min)
  
  QuarterlyNormalsAgg$pratio<-QuarterlyNormalsAgg$Pmin/QuarterlyNormalsAgg$Pmax
  
  QuarterlyNormalsAgg$rAmax<-apply(QuarterlyNormalsAgg[,c("rAq01","rAq02","rAq03","rAq04","rAq05","rAq06","rAq07","rAq08","rAq09","rAq10","rAq11","rAq12")],1,max)
  QuarterlyNormalsAgg$Mmax<-apply(QuarterlyNormalsAgg[,c("Mq01","Mq02","Mq03","Mq04","Mq05","Mq06","Mq07","Mq08","Mq09","Mq10","Mq11","Mq12")],1,max)
  QuarterlyNormalsAgg$Mmin<-apply(QuarterlyNormalsAgg[,c("Mq01","Mq02","Mq03","Mq04","Mq05","Mq06","Mq07","Mq08","Mq09","Mq10","Mq11","Mq12")],1,min)
  rm(Eq01,Eq02,Eq03,Eq04,Eq05,Eq06,Eq07,Eq08,Eq09,Eq10,Eq11,Eq12,Pq01,Pq02,Pq03,Pq04,Pq05,Pq06,Pq07,Pq08,Pq09,Pq10,Pq11,Pq12)
  
  #summarize title
  MAAT<-mean(meanStation$t, na.rm=TRUE)
  Elev<-mean(Daylength$Elevation, na.rm=TRUE)
  Lat<-mean(Daylength$Latitude, na.rm=TRUE)
  Lon<-mean(Daylength$Longitude, na.rm=TRUE)
  PET<-mean(meanStation$PET, na.rm=TRUE)
  MAP<-mean(meanStation$p, na.rm=TRUE)
  GDT<-mean(meanStation$GDT, na.rm=TRUE)
  peakGDM<-mean(meanStation$GDTmax, na.rm=TRUE)
  pptratio<-QuarterlyNormalsAgg$pratio
  #sumWarmrain<-sum(meanStation$warmrain, na.rm=TRUE)
  #sumcoolrain<-sum(meanStation$coolrain, na.rm=TRUE)
  #warmrainindex<-mean(meanStation$WRI, na.rm=TRUE)
  GrowthWtTemperature<-mean(meanStation$GwtTT, na.rm=TRUE)
  Qdef<-QuarterlyNormalsAgg$Mmax-QuarterlyNormalsAgg$Mmin
  DSImean<-mean(meanStation$DSI,na.rm=TRUE)
  DSIimean<-mean(meanStation$DSIi,na.rm=TRUE)
  PPETRatio<-mean(meanStation$M,na.rm=TRUE)
  #SummerBioT<-mean(meanStation$Warmth,na.rm=TRUE)
  #WinterLow<-mean(meanStation$tlowmin,na.rm=TRUE)
  #PWBTmean<-mean(meanStation$PWBT,na.rm=TRUE)
  BioTmean<-mean(meanStation$BioT,na.rm=TRUE)
  #PWBTdiffmean<-mean(meanStation$PWBTdif,na.rm=TRUE)
  AETR<-mean(meanStation$AETR,na.rm=TRUE)
  Surplus <- mean(meanStation$Surplus,na.rm=TRUE)
  Deficit <- mean(meanStation$Deficit,na.rm=TRUE)
  Drought <- 1-AETR
  SummerBioT<-mean(meanStation$Bt6,na.rm=TRUE)
  WinterLow<-mean(meanStation$tlo,na.rm=TRUE)
  peakAET <- mean(meanStation$maxAET, na.rm = T)
  Tw<-mean(meanStation$tx)
  Twh<-mean(meanStation$txh)
  Tc<-mean(meanStation$tn)
  Tcl<-mean(meanStation$tnl)
  Tclx <- mean(meanbystation$Tclx)
  
  #Key to climate type_____________________________________________________
  
  
  Seasonalilty <- ifelse(Deficit < 150 & PPETRatio>=1, "Isopluvial",
                         ifelse(Surplus < 25 & PPETRatio < 0.5 & peakAET < 75, "Isoxeric",
                                ifelse(peakAET < 75,"Xerothermic","Pluviothermic")))
  
  
  
  
  
  
  ifelse(QuarterlyNormalsAgg$rAmax>0.6,SeasonalityB<-"Monsoonal",SeasonalityB<-"Mediterranean")
  
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
  
  #assemble supplemental summary
  
  my_text <- paste("Lat: ",round(Lat,digits=2),"  Lon:", round(Lon,digits=2),"  Elev: ",round(Elev,digits=0)," m  MAAT: ",round(MAAT,digits=1),"°C  ","MAP: ", round(MAP,0)," mm  ",
                   "Maximum (High): ",round(Tw,1),"°C (",round(Twh,1),"°C) ", "Minimum (Low): ",round(Tc,1),"°C (",round(Tcl,1),"°C)","\n",
                   "Growing Season Temperature: ",round(SummerBioT,digits=1),"; Annual Extreme Low: ", round(Tclx,1),
                   "; Moisture Index: ", round(PPETRatio,2),"; Surplus: ", round(Surplus,0),"; Deficit: ", round(Deficit,0),"; Peak AET: ", round(peakAET,0), "\n", Climatetext,sep="")
  #aggregate graph
  climplot <- ggplot(NULL, aes(x=Month, y=v050)) + 
    geom_bar(stat="identity",aes(fill="Precipitation", y=v050), data = AggrNormMonthP, alpha = 0.85,  color="blue") +
    geom_bar(stat="identity", aes(fill="PET", y=v050), data = AggrNormMonthE, alpha = 0.60,  color="red") +
    #geom_bar(stat="identity", aes(fill="Temperature", y=v050), data = AggrNormMonthT, alpha = 0.80,  color="black",width=0.1) +
    geom_line(stat="identity",  aes(color="Temperature", y=v050),data = AggrNormMonthT, alpha = 1, group= "Temperature") +
    geom_line(stat="identity",  aes(color="Growth", y=v050),data = AggrNormMonthGindex, alpha = 1, group= "Gindex") +
    #geom_line(stat="identity",  aes(color="Low", y=v050),data = AggrNormMonthTl, alpha = 1, group= "Low") +
    #geom_line(stat="identity",  aes(color="High", y=th050),data = AggrNormMonthTl, alpha = 1, group= "High") +
    geom_point(aes(color="Temperature", shape="Temperature", y=v050),data = AggrNormMonthT, shape=19) +
    geom_point(aes(color="Temperature", shape = "Low", y=v050),data = AggrNormMonthTl, shape=6) +
    geom_point(aes(color="Temperature", shape = "High", y=th050),data = AggrNormMonthTl, shape=2) +
    geom_errorbar(aes(ymin=v025, ymax=v075), data = AggrNormMonthP, width=.2,position=position_dodge(-0.9), color="blue") +
    geom_errorbar(aes(ymin=v025, ymax=v075), data = AggrNormMonthT, width=.2,position=position_dodge(0.9), color="red") +
    #geom_errorbar(aes(ymin=v050, ymax=th050), data = AggrNormMonthTl, width=.5,position=position_dodge(0.9), color="red") +
    #ylab ("Temperature, Celsius (Precipitation, PET, mm)") +
    ylab ("Temperature (°C) | Precipitation, PET (mm)") +
    labs(title = paste("Climate of LRU ",selection, sep=""))+
    
    scale_y_continuous(breaks=c(-20,-15,-10,-5,0,5,10,15,20,25,30,35,40), labels=c('-20°C (-4°F)', '-15°C (5°F)', '-10°C (14°F)', '-5°C (23°F)', '0 mm (0 in)   0°C (32°F)', '25 mm (1 in)   5°C (41°F)', '50 mm (2 in)   10°C (50°F)', '75 mm (3 in)   15°C (59°F)', '100 mm (4 in)   20°C (68°F)', '125 mm (5 in)   25°C (77°F)', '150 mm (6 in)   30°C (86°F)', '175 mm (7 in)   35°C (95°F)', '200 mm (8 in)   40°C (104°F)'))+
    #scale_y_continuous(breaks=c(-20,-15,-10,-5,0,5,10,15,20,25,30,35,40), labels=c("-20","-15","-10","-5","0","5 (25)","10 (50)","15 (75)","20 (100)","25 (125)","30 (150)","35 (175)","40 (200)"))+
    coord_cartesian(ylim = c(-20, 40))+
    scale_fill_manual("Legend", values = c("Precipitation" = "cyan", "PET" = "yellow", "Temperature"="black","Low"="red","High"="red"))+
    scale_color_manual("",values = c("Temperature" = "red", "Low" = "red", "High"="red","Growth"="darkgreen"))+
    scale_shape_manual("",values = c("Temperature" = 19, "Low" = 6, "High"=2))+
    labs(subtitle=my_text)
  #library(grid)
  #my_grob = grid.text(my_text, x=0.7,  y=0.8, gp=gpar(col="firebrick", fontsize=14, fontface="bold"))
  #climplot + annotation_custom(my_grob)
  
  #ggsave(paste('Climate_',selection,'.png',sep=""), plot = climplot, device = "png", path = "C:/workspace/graphs/", scale = 2, width = 15, height = 10, units = "cm", dpi = 100, limitsize = TRUE)
  
  climplot})
  
})
