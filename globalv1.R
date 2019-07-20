library(dplyr)
library(leaflet)
library(sf)
library(rgdal)
library(classInt)
library(ggplot2)
library(reshape2)
#setwd("C:/Users/Inheed/Documents/R/AquaC")
aquaD<-read.csv("aquaC_data.csv")
states <- readOGR("STATES1.shp")
statesNA <- readOGR("STATES1_na.shp")
subregions<-readOGR("Subregions.shp")
atlantic<-readOGR("All_Atlantic.shp")
states@data$STATE<-c(10,5,4,1,7,11,13,8,12,2,9,3,14,6)

CONSUMPTION<-read.csv("CONSUMPTION.csv")
PREFERENCE<-read.csv("PREFERENCE.csv")
AVAILABILITY<-read.csv("AVAILABILITY.csv")
BUY<-read.csv("BUY.csv")
STORE<-read.csv("STORE.csv")
REST<-read.csv("RESTAURANT.csv")

STdata2<-read.csv("EXPENDITURE.csv")
STdata<-read.csv("StateData.csv")


#FINAL DATA
#states
STdata<-STdata[,-1]
PREFERENCE<-PREFERENCE[,-1]
CONSUMPTION<-CONSUMPTION[,-1]
AVAILABILITY<-AVAILABILITY[,-1]

STdata$STATE<-as.numeric(STdata[,1])
PREFERENCE$STATE<-as.numeric(PREFERENCE[,1])
CONSUMPTION$STATE<-as.numeric(CONSUMPTION[,1])
AVAILABILITY$STATE<-as.numeric(AVAILABILITY[,1])
BUY$STATE<-as.numeric(BUY[,1])
STORE$STATE<-as.numeric(STORE[,1])
REST$STATE<-as.numeric(REST[,1])
row.names(PREFERENCE)=c(1:17)
row.names(CONSUMPTION)=c(1:17)
row.names(AVAILABILITY)=c(1:17)
row.names(BUY)=c(1:17)
row.names(STORE)=c(1:17)
row.names(REST)=c(1:17)
row.names(STdata)=c(1:14)

STdata<-merge(STdata, STdata2[1:14,]) #combine with expenditure data
STdata
MAP_ST<-states
MAP_ST<-merge(MAP_ST[,-4], STdata)
MAP_ST@data

#subregions
NE<-round(colMeans(STdata[1:6,-1]),2)
MA<-round(colMeans(STdata[7:10,-1]),2)
SE<-round(colMeans(STdata[11:14,-1]),2)
SRdata<-as.data.frame(rbind(NE,MA,SE))
SRdata
SRdata$SUBREGION<-c("NORTHEAST","MIDATLANTIC","SOUTHEAST")
MAP_SR<-subregions
MAP_SR@data$STATE_NAME<-as.character(MAP_SR@data$STATE_NAME)
MAP_SR<-merge(MAP_SR, SRdata, by.x="STATE_NAME", by.y="SUBREGION")
MAP_SR$STATE_ABBR<-c(15,16,17)
colnames(MAP_SR@data)[5]<-"STATE" #add field called STATE for $layerID var to click like states
MAP_SR@data
MAP_ST@data


#map colors

C.pal <- colorRampPalette(c("#09EB5C","#000000"),space="rgb", bias=0.2)(2)
A.pal <- colorRampPalette(c("#EBB110","#000000"),space="rgb", bias=0.2)(2)
P.pal <- colorRampPalette(c("#584DEB","#000000"),space="rgb", bias=0.2)(2)
S.pal <- colorRampPalette(c("#7CDEDC","#000000"),space="rgb", bias=0.2)(2)
R.pal <- colorRampPalette(c("#9CA0E6","#000000"),space="rgb", bias=0.2)(2)
B.pal <- colorRampPalette(c("#F7BCE0","#000000"),space="rgb", bias=0.1)(2)
#NAMES for title
titleNames<-data.frame(STATE=c(1:17),NAME=c("Maine","New Hampshire","Massachusetts","Rhode Island",
                            "Connecticut","New York","New Jersey","Delaware","Maryland",
                            "Virginia","North Carolina","South Carolina","Georgia","Florida",
                            "Northeast Region","Mid-Atlantic Region", "Southeast Region"
                            ))
titleNames

