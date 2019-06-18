getwd()

rm(list=ls())

install.packages('rworldmap')
install.packages('spatialEco')
install.packages('RColorBrewer')

library(rworldmap)
library('spatialEco')
library('RColorBrewer')

newmap <- getMap(resolution = "low")
albacore<-read.csv('albacore_troll_complete_longtweak.csv')


albacore$DATE_FISHED<-as.Date(albacore$DATE_FISHED, format='%m/%d/%y')
albacore$YEAR <- as.numeric(format(albacore$DATE_FISHED,'%Y'))
albacore$MONTH <- as.numeric(format(albacore$DATE_FISHED,'%m'))
albacore<-albacore[c(-1,-3,-4,-5,-6,-9,-11)] ## Take out -4 if you want CPUE
albacore<-subset(albacore, YEAR!= 1991)
albacore<-subset(albacore, KEPT!= -99)
albacore<-subset(albacore, HOURS!= 0) ## Only for CPUE
albacore$FishHour<-albacore$KEPT/albacore$HOURS ## Only for CPUE
albacore<-albacore[complete.cases(albacore),]


Specs<-read.csv('CompleteMatch_Cleaned.csv')
Tuna<-merge(x=albacore, y=Specs, by="Vessel_ID", all.x=TRUE)
Tuna<-Tuna[which(Tuna$LONGITUDE > -210),]
Tuna<-Tuna[which(Tuna$LONGITUDE < -115 ),]

###Vessel Size Filter
Tuna<-Tuna[which(Tuna$Gross_Ton.mt. < 50),]

###Select by Differnt State Port Groupings

Tuna<-Tuna[which(Tuna$Port_of_Registration_State.x=='OR'
                |Tuna$Port_of_Registration_State.x=='WA'),]

Tuna<-Tuna[which(Tuna$Port_of_Registration_State.x=='HI'),]

Tuna<-Tuna[which(Tuna$Port_of_Registration_State.x=='AK'),]

###Filter By Year
Tuna<-Tuna[which(Tuna$YEAR=='1998'),]


###Full Earth 
plot(newmap, xlim = c(-180, 180), ylim = c(30, 55), asp = 1)

###California
plot(newmap, xlim = c(-145, -110), ylim = c(35, 50), asp = 1)

###East Pacific
plot(newmap, xlim = c(-180, -120), ylim = c(20, 60), asp = 1)

###West Pacific
plot(newmap, xlim = c(130, 180), ylim = c(30, 55), asp = 1)

## Plot All Points & Single Centroid



TunaYear<-Tuna[which(Tuna$YEAR=='2006'),]

coordinates(TunaYear) = ~LONGITUDE + LATITUDE
points(TunaYear, pch=20, cex=0.25)
wt.2000 <- wt.centroid(TunaYear, 'KEPT', sp=TRUE)
points(wt.2000, pch=19, col='red', cex=1.5)


###Change 'KEPT' in this Function to FishHour if CPUE is desired

load.file <- function(filename) {
  coordinates(filename) = ~LONGITUDE + LATITUDE
  wt.2000 <- wt.centroid(filename, 'KEPT', sp=TRUE)
  x<-as.data.frame(wt.2000)
  names(x)<-c("LONGITUDE", "LATITUDE")
  y<-as.data.frame(aggregate(KEPT~YEAR, data=filename, FUN=sum)) ### Change from sum to mean if CPUE is desired
  Value<-as.data.frame(c(x,y))
}

Tuna.split<-split(Tuna, Tuna$YEAR)

data <- lapply(Tuna.split, load.file)
output <- matrix(unlist(data), ncol = 4, byrow = TRUE)
Centroids<-as.data.frame(output)
names(Centroids)<-c("LONGITUDE", "LATITUDE", "YEAR", "FISH")

plot(newmap, xlim = c(-150, -124), ylim = c(43, 47), asp = 1)
points(Centroids$LONGITUDE, Centroids$LATITUDE, pch=21, cex=Centroids$FISH/300000, col=terrain.colors(25))
text(LATITUDE~LONGITUDE, labels=YEAR, data=Centroids, cex=.4, pos=1)



