###
### 1/31/2018
### Alison Ketz
### Movement data preprocessing using adehabitatLT package
###


###
### Preliminaries
###

rm(list=ls())

library(geosphere)
library(lubridate)
library(Hmisc)
library(zoo)
library(stringr)
library(ggplot2)
library(dplyr)
library(ggmap)
library(adehabitatHR)
library(adehabitatLT)
library(maptools)
library(changepoint)
library(sp)
library(spatstat)#for "duplicate" function
library(readr)
library(RColorBrewer)

setwd("~/Documents/Parturition/180510_parturition")

###
### Load GPS data
###

datalist_temp = list.files(pattern="*.csv")
myfiles = lapply(datalist_temp, read.csv,header=TRUE)
nameHeader = tolower(gsub('[[:punct:]]',"",names(myfiles[[1]])))

#clean data
datafiles=lapply(myfiles,function(x){
    names(x)<-nameHeader #fix column names, remove punctuation
    x[,1]<-trimws(x[,1]) #trimwhitespace in deviceid column
    lowtag=rep(substr(x[1,1], 1, 4),dim(x)[1])
    x=data.frame(lowtag,x)
    names(x)=c('lowtag', 'devname', 'devid' ,'datetimegmt', 'datetimelocal', 'latitude', 'longitude', 'altitude', 'fixstatus', 'dop', 'temp', 'main','back')
    x[,6]=trimws(x[,6])
    x[,7]=trimws(x[,7])
  x})

#get list of all ids
# datafiles=lapply(datafiles,function(x){lowtag=rep(substr(x[1,1], 1, 4),dim(x)[1]);x=data.frame(lowtag,x,stringsAsFactors = FALSE);x})
ids=unlist(lapply(datafiles,function(x){x[1,1]}))

datafiles=lapply(datafiles,function(x){
    #julian day
    x$julian=yday(trimws(x$datetimegmt))
  x})


###
### Individual 1
###

d=datafiles[[1]]
d[which(d[,6]==head(d)[6,6]),6]='0'
d[which(d[,7]==head(d)[6,7]),6]='0'
d$longitude[6]='0'
d$longitude=as.numeric(d$longitude)
d$latitude=as.numeric(d$latitude)
d[which(d$longitude==0),6:7]=NA
d=d[!is.na(d$longitude),]
d$indx=1:dim(d)[1]

d$julian=yday(paste("2018-05-",substr(trimws(d$datetimegmt),4,5),sep=""))

hday=c(yday("2018-05-04"),yday("2018-05-07"),128,129)

d$hitday=as.factor(as.numeric(d$julian %in% hday))
d$hitday
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Make a bounding box for data
box <- make_bbox(longitude,latitude,data = d)

calc_zoom(box)
map=get_map(location=c(mean(d$longitude),mean(d$latitude)),source="google",zoom=16,maptype="satellite",crop=box)
#Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=43.069755,-90.280971&zoom=14&size=640x640&scale=2&maptype=satellite&language=en-EN&sensor=false

map1=ggmap(map)+geom_path(aes(x = longitude, y = latitude,color=hitday),data = d, size = 1)+geom_point(aes(x = longitude, y = latitude,color=hitday),data = d,size =4)+geom_text(aes(x=longitude,y=latitude,label=indx),data=d,size=2.5)+ggtitle(d$lowtag[1])+xlab("Longitude")+ylab("Latitude")+scale_color_manual(values=c(cbPalette[5:6]))

pdf("5177_map.pdf")
print(map1)
dev.off()


##############################################################################################################################################################
#5728
d=datafiles[[2]]

d$longitude=as.numeric(d$longitude)
d$latitude=as.numeric(d$latitude)
d[which(d$longitude==0),6:7]=NA
d=d[!is.na(d$longitude),]
d$indx=1:dim(d)[1]
d$julian=yday(paste("2018-05-",substr(trimws(d$datetimegmt),4,5),sep=""))

hday=c(yday("2018-05-01"),yday("2018-05-02"),123,126)

d$hitday=as.factor(as.numeric(d$julian %in% hday))
d$hitday
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Make a bounding box for data
box <- make_bbox(longitude,latitude,data = d)

calc_zoom(box)
map=get_map(location=c(mean(d$longitude),mean(d$latitude)),source="google",zoom=16,maptype="satellite",crop=box)
#Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=43.069755,-90.280971&zoom=14&size=640x640&scale=2&maptype=satellite&language=en-EN&sensor=false

map2=ggmap(map)+geom_path(aes(x = longitude, y = latitude,color=hitday),data = d, size = 1)+geom_point(aes(x = longitude, y = latitude,color=hitday),data = d,size =4)+geom_text(aes(x=longitude,y=latitude,label=indx),data=d,size=2.5)+ggtitle(d$lowtag[1])+xlab("Longitude")+ylab("Latitude")+scale_color_manual(values=c(cbPalette[5:6]),labels=c("Out","Hit"))

map2

pdf("5728_map.pdf")
print(map2)
dev.off()





##############################################################################################################################################################
#5728
d=datafiles[[3]]
d[9,6]='0'

d[which(d[,6]==head(d)[6,6]),6]='0'
d[which(d[,7]==head(d)[6,7]),6]='0'
d$longitude[6]='0'


d$longitude=as.numeric(d$longitude)
d$latitude=as.numeric(d$latitude)
d[which(d$longitude==0),6:7]=NA
d=d[!is.na(d$longitude),]
d$indx=1:dim(d)[1]
d$julian=yday(paste("2018-05-",substr(trimws(d$datetimegmt),4,5),sep=""))

hday=124:127
d$hitday=as.factor(as.numeric(d$julian %in% hday))
d$hitday
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Make a bounding box for data
box <- make_bbox(longitude,latitude,data = d)

calc_zoom(box)
map=get_map(location=c(mean(d$longitude),mean(d$latitude)),source="google",zoom=16,maptype="satellite",crop=box)
#Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=43.069755,-90.280971&zoom=14&size=640x640&scale=2&maptype=satellite&language=en-EN&sensor=false

map3=ggmap(map)+geom_path(aes(x = longitude, y = latitude,color=hitday),data = d, size = 1)+geom_point(aes(x = longitude, y = latitude,color=hitday),data = d,size =4)+geom_text(aes(x=longitude,y=latitude,label=indx),data=d,size=2.5)+ggtitle(d$lowtag[1])+xlab("Longitude")+ylab("Latitude")+scale_color_manual(values=c(cbPalette[5:6]),labels=c("Out","Hit"))

map3

pdf("5880_map.pdf")
print(map3)
dev.off()
