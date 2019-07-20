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
OAtlantic<-readOGR("All_Atlantic.shp")
states@data$STATE<-c(10,5,4,1,7,11,13,8,12,2,9,3,14,6)

#read in Map 1 data
FARM<-read.csv("M1_farmraised.csv")
LOCATION<-read.csv("M1_location.csv")
LOCATIONA<-read.csv("M1_locationaware.csv")
PAYMORE<-read.csv("M1_paymore.csv")
UNMET<-read.csv("M2_additional.csv")

#map 1 data prep
FARM$STATE<-as.numeric(FARM[,1])
LOCATION$STATE<-as.numeric(LOCATION[,1])
LOCATIONA$STATE<-as.numeric(LOCATIONA[,1])
PAYMORE$STATE<-as.numeric(PAYMORE[,1])
UNMET$STATE<-as.numeric(UNMET[,1])

row.names(FARM)=c(1:18)
row.names(LOCATION)=c(1:18)
row.names(LOCATIONA)=c(1:18)
row.names(PAYMORE)=c(1:18)
row.names(UNMET)=c(1:18)


UNMET$TOTAL<-round(rowSums(UNMET[,-1]),2) #total unmet demand
UNMET1<-UNMET[,-21]
UNMET
FARM_fin<-round(rowMeans(subset(FARM, select = c(3, 6,9))),2) #Average all farmed fish types
ST_data1<-cbind(STATE=FARM$STATE,
                as.numeric(FARM_fin),
                PAYMORE=as.numeric(PAYMORE[,6]),
                LOCATION=as.numeric(LOCATION[,4]), 
                LOCATIONA=as.numeric(LOCATIONA[,2]),
                UNMET=as.numeric(UNMET[,21]))
ST_data1<-as.data.frame(ST_data1)

#configure Map1 map data
MAP_ST1<-merge(states[,-4], ST_data1)

#M1 subregions
SRdata1<-ST_data1[15:17,]
SRdata1$SUBREGION<-c("NORTHEAST","MIDATLANTIC","SOUTHEAST")
MAP_SR1<-merge(subregions[,-4], SRdata1,by.x="STATE_NAME", by.y="SUBREGION")
MAP_SR1$STATE_ABBR<-MAP_SR1$STATE 
row.names(MAP_SR1@data)<-MAP_SR1@data$STATE


#M1 Overall Atlantic
Odata1<-ST_data1[18,]
MAP_O1<-OAtlantic
MAP_O1@data$STATE_NAME<-"Atlantic States Overall"
MAP_O1@data$STATE<-18
MAP_O1<-merge(MAP_O1[,-4], Odata1)
row.names(MAP_O1@data)<-MAP_O1@data$STATE


#read in Map 2 data
CONSUMPTION<-read.csv("M2_consumption.csv")
PREFERENCE<-read.csv("M2_preference.csv")
AVAILABILITY<-read.csv("M2_availability.csv")
BUY<-read.csv("M2_additional.csv")
STORE<-read.csv("M2_store.csv")
REST<-read.csv("M2_restaurant.csv")
ST_means<-read.csv("M2_statemeans.csv")


#FINAL DATA
#states
ST_means$STATE<-as.numeric(ST_means[,1])
PREFERENCE$STATE<-as.numeric(PREFERENCE[,1])
CONSUMPTION$STATE<-as.numeric(CONSUMPTION[,1])
AVAILABILITY$STATE<-as.numeric(AVAILABILITY[,1])
BUY$STATE<-as.numeric(BUY[,1])
STORE$STATE<-as.numeric(STORE[,1])
REST$STATE<-as.numeric(REST[,1])
row.names(PREFERENCE)=c(1:18)
row.names(CONSUMPTION)=c(1:18)
row.names(AVAILABILITY)=c(1:18)
row.names(BUY)=c(1:18)
row.names(STORE)=c(1:18)
row.names(REST)=c(1:18)
row.names(ST_means)=c(1:18)

MAP_ST<-states
MAP_ST<-merge(MAP_ST[,-4], ST_means)

#subregions
SRdata<-ST_means[15:17,]
SRdata$SUBREGION<-c("NORTHEAST","MIDATLANTIC","SOUTHEAST")
MAP_SR<-subregions
MAP_SR@data$STATE_NAME<-as.character(MAP_SR@data$STATE_NAME)
MAP_SR<-merge(MAP_SR[,-4], SRdata, by.x="STATE_NAME", by.y="SUBREGION")
MAP_SR$STATE_ABBR<-MAP_SR$STATE  
row.names(MAP_SR@data)<-MAP_SR@data$STATE

#Overall Atlantic
Odata<-ST_means[18,]
MAP_O<-OAtlantic
MAP_O<-merge(MAP_O[,-4], Odata)
MAP_O@data$STATE_NAME<-"Atlantic States Overall"
MAP_O@data$STATE<-18
row.names(MAP_O@data)<-MAP_O@data$STATE

#map colors
C.pal <- colorRampPalette(c("#09EB5C","#000000"),space="rgb", bias=0.2)(2)
P.pal <- colorRampPalette(c("#EBB110","#000000"),space="rgb", bias=0.2)(2)
A.pal <- colorRampPalette(c("#584DEB","#000000"),space="rgb", bias=0.2)(2)
S.pal <- colorRampPalette(c("#7CDEDC","#000000"),space="rgb", bias=0.2)(2)
R.pal <- colorRampPalette(c("#FCFF4B","#000000"),space="rgb", bias=0.2)(2)
B.pal <- colorRampPalette(c("#F7BCE0","#000000"),space="rgb", bias=0.1)(2)

W.pal <- colorRampPalette(c("#FFBA08","#000000"),space="rgb", bias=0.1)(2)
Q.pal <- colorRampPalette(c("#E4572E","#000000"),space="rgb", bias=0.1)(2)
D.pal <- colorRampPalette(c("#A8C256","#000000"),space="rgb", bias=0.1)(2)
O.pal <- colorRampPalette(c("#AA767C","#000000"),space="rgb", bias=0.1)(2)

#chartcolors
color_table<-c("#0E1116","#9CB380","#676F54","#BDC696")
color_table2<-c("#0E1116","#9CB380","#676F54","#BDC696", "#A9A9A9")
#NAMES for title
titleNames<-data.frame(STATE=c(1:18),NAME=c("Maine","New Hampshire","Massachusetts","Rhode Island",
                            "Connecticut","New York","New Jersey","Delaware","Maryland",
                            "Virginia","North Carolina","South Carolina","Georgia","Florida",
                            "Northeast Region","Mid-Atlantic Region", "Southeast Region", "Overall"
                            ))



