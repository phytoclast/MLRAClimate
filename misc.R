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
MLRA <- readRDS(file='data/MLRA.RDS')
DaysMonth <- readRDS(file='data/DaysMonth.RDS')
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
              