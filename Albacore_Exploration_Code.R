getwd()


install.packages('rworldmap')
install.packages('spatialEco')

library(rworldmap)
library('spatialEco')

rm(list=ls())


require(sp)
data(meuse)
coordinates(meuse) = ~x+y
wt.copper <- wt.centroid(albacore2, 'copper', sp=TRUE)
wt.zinc <- wt.centroid(meuse, 'zinc', sp=TRUE)
plot(albacore2, pch=20, cex=0.75, main='Weighted centroid(s)')
points(wt.copper, pch=19, col='red', cex=1.5)
points(wt.zinc, pch=19, col='blue', cex=1.5)
box()
legend('topleft', legend=c('all','copper', 'zinc'),
       pch=c(20,19,19),col=c('black','red','blue')

coordinates(albacore2) = ~LONGITUDE + LATITUDE
wt.2005 <- wt.centroid(albacore2, 'KEPT', sp=TRUE)



albacore<-read.csv('albacore_troll_95to2015.csv')
albacore$DATE_FISHED<-as.Date(albacore$DATE_FISHED, format='%m/%d/%y')
albacore$YEAR <- as.numeric(format(albacore$DATE_FISHED,'%Y'))
albacore$MONTH <- as.numeric(format(albacore$DATE_FISHED,'%m'))
albacore<-albacore[c(-1,-3,-4,-5,-6,-9,-11)]
albacore<-albacore[complete.cases(albacore),]
albacore2<-albacore[which(albacore$YEAR=='2013'),]

coordinates(albacore2) = ~LONGITUDE + LATITUDE

newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(-180, -110), ylim = c(30, 55), asp = 1)

points(albacore2, pch=20, cex=0.25, main='Weighted centroid(s)')

wt.2000 <- wt.centroid(albacore2, 'KEPT', sp=TRUE)
points(wt.2000, pch=19, col='red', cex=1.5)



coords= cbind(albacore$LONGITUDE, albacore$LATITUDE)
sp=SpatialPoints(coords)
data=as.data.frame(cbind(albacore$VESSELID,albacore$KEPT, albacore$YEAR, albacore$MONTH))
spdf=SpatialPointsDataFrame(coords, data)





?SpatialPoints


coordinates(albacore) = ~


albacore1<-read.csv('albacoreupdate.csv')
albacore2<-albacore[which(albacore$VESSELID=='500057'),]

install.packages('rworldmap')
library(rworldmap)
newmap <- getMap(resolution = "low")

albacore2<-albacore[which(albacore$VESSELID=='1228801'),]
albacore2<-albacore[which(albacore$year=='2005'),]


plot(newmap, xlim = c(-170, -120), ylim = c(15, 55), asp = 1)
points(albacore2$LONGITUDE, albacore2$LATITUDE, col=albacore2$VESSELID)

str(albacore)
