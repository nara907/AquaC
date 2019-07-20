library(dplyr)
library(leaflet)
library(sf)
library(rgdal)
library(classInt)
library(ggplot2)
library(reshape2)
setwd("C:/Users/Inheed/Documents/R/AquaC")
aquaD<-read.csv("aquaC_data.csv")

#CONSUMPTION_______________________________________________________________________
#FLOUNDER
CaquaD<-filter(aquaD,EAT_SEAFOOD==1)

CON_F<-dplyr::select(CaquaD, "FINAL_WEIGHT","STATE","REGIONS",
                     "CONSUMED_BASS","CONSUMED_COBIA","CONSUMED_DRUM","CONSUMED_FLOUNDER",
                     "CONSUMED_HALIBUT", "CONSUMED_A_SALMON","CONSUMED_P_SALMON","CONSUMED_STURGEON","CONSUMED_OTHER_FIN")
#group and create column for total average
CON_F_s<-CON_F%>%
  group_by(STATE) %>%
  summarize(BASS=(sum(CONSUMED_BASS*FINAL_WEIGHT)/sum(FINAL_WEIGHT)),
            COBIA=(sum(CONSUMED_COBIA*FINAL_WEIGHT)/sum(FINAL_WEIGHT)),
            DRUM=(sum(CONSUMED_DRUM*FINAL_WEIGHT)/sum(FINAL_WEIGHT)),
            FLOUNDER=(sum(CONSUMED_FLOUNDER*FINAL_WEIGHT)/sum(FINAL_WEIGHT)),
            HALIBUT=(sum(CONSUMED_HALIBUT*FINAL_WEIGHT)/sum(FINAL_WEIGHT)), 
            ASALMON=(sum(CONSUMED_A_SALMON*FINAL_WEIGHT)/sum(FINAL_WEIGHT)),
            PSALMON=(sum(CONSUMED_P_SALMON*FINAL_WEIGHT)/sum(FINAL_WEIGHT)), 
            STURGEON=(sum(CONSUMED_STURGEON*FINAL_WEIGHT)/sum(FINAL_WEIGHT)), 
            OTHER_F=(sum(CONSUMED_OTHER_FIN*FINAL_WEIGHT)/sum(FINAL_WEIGHT)))
CON_F_s<-as.data.frame(CON_F_s)
CON_F_s$TOTAL_F<-(CON_F_s[,2]+CON_F_s[,3]+CON_F_s[,4]+CON_F_s[,5]+CON_F_s[,6]+CON_F_s[,7]+CON_F_s[,8]+CON_F_s[,9]+CON_F_s[,10])/9


#CRUSTACEANS
CON_C<-dplyr::select(CaquaD,"FINAL_WEIGHT", "STATE","REGIONS",
                     "CONSUMED_CRABS","CONSUMED_LOBSTER","CONSUMED_SHRIMP","CONSUMED_CRUSTACEANS")
#group and create column for total average
CON_C_s<-CON_C%>%
  group_by(STATE) %>%
  summarize(CRABS=(sum(CONSUMED_CRABS*FINAL_WEIGHT)/sum(FINAL_WEIGHT)),
            LOBSTERS=(sum(CONSUMED_LOBSTER*FINAL_WEIGHT)/sum(FINAL_WEIGHT)),
            SHRIMP=(sum(CONSUMED_SHRIMP*FINAL_WEIGHT)/sum(FINAL_WEIGHT)),
            OTHER_C=(sum(CONSUMED_CRUSTACEANS*FINAL_WEIGHT)/sum(FINAL_WEIGHT)))
CON_C_s<-as.data.frame(CON_C_s)
CON_C_s$TOTAL_C<-(CON_C_s[,2]+CON_F_s[,3]+CON_F_s[,4]+CON_F_s[,5])/4


#MOLLUSKS
CON_M<-dplyr::select(CaquaD,"FINAL_WEIGHT", "STATE","REGIONS",
                     "CONSUMED_ABALONE","CONSUMED_CLAMS","CONSUMED_MUSSELS","CONSUMED_OYSTERS","CONSUMED_SCALLOPS",
                     "CONSUMED_MOLLUSKS")
#group and create column for total average
CON_M_s<-CON_M%>%
  group_by(STATE) %>%
  summarize(ABALONE=(sum(CONSUMED_ABALONE*FINAL_WEIGHT)/sum(FINAL_WEIGHT)),
            CLAMS=(sum(CONSUMED_CLAMS*FINAL_WEIGHT)/sum(FINAL_WEIGHT)),
            MUSSELS=(sum(CONSUMED_MUSSELS*FINAL_WEIGHT)/sum(FINAL_WEIGHT)),
            OYSTERS=(sum(CONSUMED_OYSTERS*FINAL_WEIGHT)/sum(FINAL_WEIGHT)),
            SCALLOPS=(sum(CONSUMED_SCALLOPS*FINAL_WEIGHT)/sum(FINAL_WEIGHT)),
            OTHER_M=(sum(CONSUMED_MOLLUSKS*FINAL_WEIGHT)/sum(FINAL_WEIGHT)))
CON_M_s<-as.data.frame(CON_M_s)
CON_M_s$TOTAL_M<-(CON_M_s[,2]+CON_M_s[,3]+CON_M_s[,4]+CON_M_s[,5]+CON_M_s[,6]+CON_M_s[,7])/6
CON_M_s


#SEA VEGETABLES
CON_V<-dplyr::select(CaquaD, "FINAL_WEIGHT","STATE","REGIONS",
                     "CONSUMED_SEA_V")
#group and create column for total average

CON_V_s<-CON_V%>%
  group_by(STATE) %>%
  summarize(SEAV=(sum(CONSUMED_SEA_V*FINAL_WEIGHT)/sum(FINAL_WEIGHT)))
CON_V_s<-as.data.frame(CON_V_s)
CON_V_s

#MERGE ALL CATEGORIES INTO ONE DATA FRAME
CComb<-merge(CON_F_s,CON_C_s, by="STATE",all=TRUE)
CComb<-merge(CComb,CON_M_s, by="STATE",all=TRUE)
CComb<-merge(CComb,CON_V_s, by="STATE",all=TRUE)
#change some column names 
colnames(CComb)[7]<-("ATLANTIC SALMON")
colnames(CComb)[8]<-("PACIFIC SALMON")
colnames(CComb)[24]<-("SEA VEGETABLES")
CComb<-cbind(STATE=CComb[,1],round(CComb[,2:24]*100,2)) #make values into percentage, round

CONSUMPTION<-dplyr::select(CComb,1:9,12:14,17:21,24) #select al but "OTHER" and "TOTAL"

CON<-dplyr::select(CComb,1,C_F=11,C_C=16,C_M=23,C_S=24) #select totals only

#Calculate subregion averages
NEc<-colMeans(CComb[1:6,-1])
MAc<-colMeans(CComb[7:10,-1])
SEc<-colMeans(CComb[11:14,-1])
SRcon<-as.data.frame(rbind(NEc,MAc,SEc))
SRcon$STATE_NAME<-c("NORTHEAST","MIDATLANTIC","SOUTHEAST")
row.names(SRcon)=c(15,16,17)
SEc
colnames(SRcon)[24] <- "STATE"
SRcon<-dplyr::select(SRcon,1:8,11:13,16:20,23:24) #select all but "other" and "total"
CONSUMPTION<-rbind(CONSUMPTION,SRcon) #combines state and subregion for graph table
CONSUMPTION
CONSUMPTION[15:17,1]<-c(15,16,17)#change NE to 15, MA to 16 and SE to 17 for click id
CONSUMPTION<-cbind(STATE=CONSUMPTION[,1],round(CONSUMPTION[,2:18],2)) #ROUND
CONSUMPTION



#PREFERENCE_________________________________________________________________________
#FLOUNDER
PREF_F<-dplyr::select(CaquaD, "FINAL_WEIGHT","STATE","REGIONS",
                      "LIKE_BASS","LIKE_COBIA","LIKE_DRUM","LIKE_FLOUNDER","LIKE_HALIBUT",
                      "LIKE_A_SALMON","LIKE_P_SALMON","LIKE_STURGEON","LIKE_OTHER_FIN")
#group and create column for total average
PREF_F_s<-PREF_F%>%
  group_by(STATE) %>%
  summarize(BASS=sum(LIKE_BASS*FINAL_WEIGHT/sum(LIKE_BASS*FINAL_WEIGHT/LIKE_BASS,na.rm=TRUE),na.rm = TRUE),
            COBIA=sum(LIKE_COBIA*FINAL_WEIGHT/sum(LIKE_COBIA*FINAL_WEIGHT/LIKE_COBIA,na.rm=TRUE),na.rm = TRUE),
            DRUM=sum(LIKE_DRUM*FINAL_WEIGHT/sum(LIKE_DRUM*FINAL_WEIGHT/LIKE_DRUM,na.rm=TRUE),na.rm = TRUE),
            FLOUNDER=sum(LIKE_FLOUNDER*FINAL_WEIGHT/sum(LIKE_FLOUNDER*FINAL_WEIGHT/LIKE_FLOUNDER,na.rm=TRUE),na.rm = TRUE),
            HALIBUT=sum(LIKE_HALIBUT*FINAL_WEIGHT/sum(LIKE_HALIBUT*FINAL_WEIGHT/LIKE_HALIBUT,na.rm=TRUE),na.rm = TRUE),
            ASALMON=sum(LIKE_A_SALMON*FINAL_WEIGHT/sum(LIKE_A_SALMON*FINAL_WEIGHT/LIKE_A_SALMON,na.rm=TRUE),na.rm = TRUE),
            PSALMON=sum(LIKE_P_SALMON*FINAL_WEIGHT/sum(LIKE_P_SALMON*FINAL_WEIGHT/LIKE_P_SALMON,na.rm=TRUE),na.rm = TRUE),
            STURGEON=sum(LIKE_STURGEON*FINAL_WEIGHT/sum(LIKE_STURGEON*FINAL_WEIGHT/LIKE_STURGEON,na.rm=TRUE),na.rm = TRUE), 
            OTHER_F=sum(LIKE_OTHER_FIN*FINAL_WEIGHT/sum(LIKE_OTHER_FIN*FINAL_WEIGHT/LIKE_OTHER_FIN,na.rm=TRUE),na.rm = TRUE))
PREF_F_s<-as.data.frame(PREF_F_s)
PREF_F_s$TOTAL_F<-(PREF_F_s[,2]+PREF_F_s[,3]+PREF_F_s[,4]+PREF_F_s[,5]+PREF_F_s[,6]+PREF_F_s[,7]+PREF_F_s[,8]+PREF_F_s[,9]+PREF_F_s[,10])/9
PREF_F_s

#CRUSTACEANS
PREF_C<-dplyr::select(CaquaD, "FINAL_WEIGHT","STATE","REGIONS",
                      "LIKE_CRABS","LIKE_LOBSTER","LIKE_SHRIMP","LIKE_CRUSTACEANS")
#group and create column for total average
PREF_C_s<-PREF_C%>%
  group_by(STATE) %>%
  summarize(CRABS=sum(LIKE_CRABS*FINAL_WEIGHT/sum(LIKE_CRABS*FINAL_WEIGHT/LIKE_CRABS,na.rm=TRUE),na.rm = TRUE),
            LOBSTER=sum(LIKE_LOBSTER*FINAL_WEIGHT/sum(LIKE_LOBSTER*FINAL_WEIGHT/LIKE_LOBSTER,na.rm=TRUE),na.rm = TRUE),
            SHRIMP=sum(LIKE_SHRIMP*FINAL_WEIGHT/sum(LIKE_SHRIMP*FINAL_WEIGHT/LIKE_SHRIMP,na.rm=TRUE),na.rm = TRUE),
            OTHER_C=sum(LIKE_CRUSTACEANS*FINAL_WEIGHT/sum(LIKE_CRUSTACEANS*FINAL_WEIGHT/LIKE_CRUSTACEANS,na.rm=TRUE),na.rm = TRUE))
PREF_C_s<-as.data.frame(PREF_C_s)
PREF_C_s$TOTAL_C<-(PREF_C_s[,2]+PREF_C_s[,3]+PREF_C_s[,4]+PREF_C_s[,5])/4
PREF_C_s

#MOLLUSKS
PREF_M<-dplyr::select(CaquaD,"FINAL_WEIGHT", "STATE","REGIONS",
                      "LIKE_ABALONE","LIKE_CLAMS","LIKE_MUSSELS","LIKE_OYSTERS","LIKE_SCALLOPS",
                      "LIKE_MOLLUSKS")
#group and create column for total average
PREF_M_s<-PREF_M%>%
  group_by(STATE) %>%
  summarize(ABALONE=sum(LIKE_ABALONE*FINAL_WEIGHT/sum(LIKE_ABALONE*FINAL_WEIGHT/LIKE_ABALONE,na.rm=TRUE),na.rm = TRUE),
            CLAMS=sum(LIKE_CLAMS*FINAL_WEIGHT/sum(LIKE_CLAMS*FINAL_WEIGHT/LIKE_CLAMS,na.rm=TRUE),na.rm = TRUE),
            MUSSELS=sum(LIKE_MUSSELS*FINAL_WEIGHT/sum(LIKE_MUSSELS*FINAL_WEIGHT/LIKE_MUSSELS,na.rm=TRUE),na.rm = TRUE),
            OYSTERS=sum(LIKE_OYSTERS*FINAL_WEIGHT/sum(LIKE_OYSTERS*FINAL_WEIGHT/LIKE_OYSTERS,na.rm=TRUE),na.rm = TRUE),
            SCALLOPS=sum(LIKE_SCALLOPS*FINAL_WEIGHT/sum(LIKE_SCALLOPS*FINAL_WEIGHT/LIKE_SCALLOPS,na.rm=TRUE),na.rm = TRUE),
            OTHER_M=sum(LIKE_MOLLUSKS*FINAL_WEIGHT/sum(LIKE_MOLLUSKS*FINAL_WEIGHT/LIKE_MOLLUSKS,na.rm=TRUE),na.rm = TRUE))
PREF_M_s<-as.data.frame(PREF_M_s)
PREF_M_s$TOTAL_M<-(PREF_M_s[,2]+PREF_M_s[,3]+PREF_M_s[,4]+PREF_M_s[,5]+PREF_M_s[,6]+PREF_M_s[,7])/6
PREF_M_s

#SEA VEGETABLES
PREF_V<-dplyr::select(CaquaD,"FINAL_WEIGHT", "STATE","REGIONS",
                      "LIKE_SEA_V")
#group and create column for total average
PREF_V_s<-PREF_V%>%
  group_by(STATE) %>%
  summarize(SEAV=sum(LIKE_SEA_V*FINAL_WEIGHT/sum(LIKE_SEA_V*FINAL_WEIGHT/LIKE_SEA_V,na.rm=TRUE),na.rm = TRUE))
PREF_V_s<-as.data.frame(PREF_V_s)
PREF_V_s

#MERGE ALL CATEGORIES INTO ONE DATA FRAME
PComb<-merge(PREF_F_s,PREF_C_s, by="STATE",all=TRUE)
PComb<-merge(PComb,PREF_M_s, by="STATE",all=TRUE)
PComb<-merge(PComb,PREF_V_s, by="STATE",all=TRUE)
colnames(PComb)[7]<-("ATLANTIC SALMON")
colnames(PComb)[8]<-("PACIFIC SALMON")
colnames(PComb)[24]<-("SEA VEGETABLES")
PComb

PREFERENCE<-dplyr::select(PComb,1:9,12:14,17:21,24) #select all but "OTHER" and "TOTAL"
PREF<-dplyr::select(PComb,1,P_F=11,P_C=16,P_M=23,P_S=24)#select only "total's

#calculate subregion averages
NEp<-colMeans(PComb[1:6,-1])
MAp<-colMeans(PComb[7:10,-1])
SEp<-colMeans(PComb[11:14,-1])
SRpref<-as.data.frame(rbind(NEp,MAp,SEp))
SRpref$STATE_NAME<-c("NORTHEAST","MIDATLANTIC","SOUTHEAST")
row.names(SRpref)=c(15,16,17)
colnames(SRpref)[24] <- "STATE"
SRpref<-dplyr::select(SRpref,1:8,11:13,16:20,23:24) #select all except 'total" and "other"
PREFERENCE<-rbind(PREFERENCE,SRpref) #combines state and subregion for graph tables
PREFERENCE[15:17,1]<-c(15,16,17)#change NE to 15, MA to 16 and SE to 17 for click id
PREFERENCE<-cbind(STATE=PREFERENCE[,1],round(PREFERENCE[,2:18],2)) #ROUND
PREFERENCE



#AVAILAILABILITY____________________________________________________________________
#FLOUNDER
AVAI_F<-dplyr::select(CaquaD, "FINAL_WEIGHT","STATE","REGIONS",
                      "AVAILABLE_BASS","AVAILABLE_COBIA","AVAILABLE_DRUM","AVAILABLE_FLOUNDER",
                      "AVAILABLE_A_SALMON","AVAILABLE_P_SALMON","AVAILABLE_STURGEON","AVAILABLE_OTHER_FIN")
#group and create column for total average
AVAI_F_s<-AVAI_F%>%
  group_by(STATE) %>%
  summarize(BASS=sum(AVAILABLE_BASS*FINAL_WEIGHT/sum(AVAILABLE_BASS*FINAL_WEIGHT/AVAILABLE_BASS,na.rm=TRUE),na.rm = TRUE),
            COBIA=sum(AVAILABLE_COBIA*FINAL_WEIGHT/sum(AVAILABLE_COBIA*FINAL_WEIGHT/AVAILABLE_COBIA,na.rm=TRUE),na.rm = TRUE),
            DRUM=sum(AVAILABLE_DRUM*FINAL_WEIGHT/sum(AVAILABLE_DRUM*FINAL_WEIGHT/AVAILABLE_DRUM,na.rm=TRUE),na.rm = TRUE),
            FLOUNDER=sum(AVAILABLE_FLOUNDER*FINAL_WEIGHT/sum(AVAILABLE_FLOUNDER*FINAL_WEIGHT/AVAILABLE_FLOUNDER,na.rm=TRUE),na.rm = TRUE),
            ASALMON=sum(AVAILABLE_A_SALMON*FINAL_WEIGHT/sum(AVAILABLE_A_SALMON*FINAL_WEIGHT/AVAILABLE_A_SALMON,na.rm=TRUE),na.rm = TRUE),
            PSALMON=sum(AVAILABLE_P_SALMON*FINAL_WEIGHT/sum(AVAILABLE_P_SALMON*FINAL_WEIGHT/AVAILABLE_P_SALMON,na.rm=TRUE),na.rm = TRUE),
            STURGEON=sum(AVAILABLE_STURGEON*FINAL_WEIGHT/sum(AVAILABLE_STURGEON*FINAL_WEIGHT/AVAILABLE_STURGEON,na.rm=TRUE),na.rm = TRUE), 
            OTHER_F=sum(AVAILABLE_OTHER_FIN*FINAL_WEIGHT/sum(AVAILABLE_OTHER_FIN*FINAL_WEIGHT/AVAILABLE_OTHER_FIN,na.rm=TRUE),na.rm = TRUE))
AVAI_F_s<-as.data.frame(AVAI_F_s)
AVAI_F_s$TOTAL_F<-(AVAI_F_s[,2]+AVAI_F_s[,3]+AVAI_F_s[,4]+AVAI_F_s[,5]+AVAI_F_s[,6]+AVAI_F_s[,7]+AVAI_F_s[,8]+AVAI_F_s[,9])/8


#CRUSTACEANS
AVAI_C<-dplyr::select(CaquaD, "FINAL_WEIGHT","STATE","REGIONS",
                      "AVAILABLE_CRABS","AVAILABLE_LOBSTER","AVAILABLE_SHRIMP","AVAILABLE_CRUSTACEANS")
#group and create column for total average
AVAI_C_s<-AVAI_C%>%
  group_by(STATE) %>%
  summarize(CRABS=sum(AVAILABLE_CRABS*FINAL_WEIGHT/sum(AVAILABLE_CRABS*FINAL_WEIGHT/AVAILABLE_CRABS,na.rm=TRUE),na.rm = TRUE),
            LOBSTER=sum(AVAILABLE_LOBSTER*FINAL_WEIGHT/sum(AVAILABLE_LOBSTER*FINAL_WEIGHT/AVAILABLE_LOBSTER,na.rm=TRUE),na.rm = TRUE),
            SHRIMP=sum(AVAILABLE_SHRIMP*FINAL_WEIGHT/sum(AVAILABLE_SHRIMP*FINAL_WEIGHT/AVAILABLE_SHRIMP,na.rm=TRUE),na.rm = TRUE),
            OTHER_C=sum(AVAILABLE_CRUSTACEANS*FINAL_WEIGHT/sum(AVAILABLE_CRUSTACEANS*FINAL_WEIGHT/AVAILABLE_CRUSTACEANS,na.rm=TRUE),na.rm = TRUE))
AVAI_C_s<-as.data.frame(AVAI_C_s)
AVAI_C_s$TOTAL_C<-(AVAI_C_s[,2]+AVAI_C_s[,3]+AVAI_C_s[,4]+AVAI_C_s[,5])/4


#MOLLUSKS
AVAI_M<-dplyr::select(CaquaD, "FINAL_WEIGHT","STATE","REGIONS",
                      "AVAILABLE_ABALONE","AVAILABLE_CLAMS","AVAILABLE_MUSSELS","AVAILABLE_OYSTERS",
                      "AVAILABLE_SCALLOPS","AVAILABLE_MOLLUSKS")
#group and create column for total average
AVAI_M_s<-AVAI_M%>%
  group_by(STATE) %>%
  summarize(ABALONE=sum(AVAILABLE_ABALONE*FINAL_WEIGHT/sum(AVAILABLE_ABALONE*FINAL_WEIGHT/AVAILABLE_ABALONE,na.rm=TRUE),na.rm = TRUE),
            CLAMS=sum(AVAILABLE_CLAMS*FINAL_WEIGHT/sum(AVAILABLE_CLAMS*FINAL_WEIGHT/AVAILABLE_CLAMS,na.rm=TRUE),na.rm = TRUE),
            MUSSELS=sum(AVAILABLE_MUSSELS*FINAL_WEIGHT/sum(AVAILABLE_MUSSELS*FINAL_WEIGHT/AVAILABLE_MUSSELS,na.rm=TRUE),na.rm = TRUE),
            OYSTERS=sum(AVAILABLE_OYSTERS*FINAL_WEIGHT/sum(AVAILABLE_OYSTERS*FINAL_WEIGHT/AVAILABLE_OYSTERS,na.rm=TRUE),na.rm = TRUE),
            SCALLOPS=sum(AVAILABLE_SCALLOPS*FINAL_WEIGHT/sum(AVAILABLE_SCALLOPS*FINAL_WEIGHT/AVAILABLE_SCALLOPS,na.rm=TRUE),na.rm = TRUE),
            OTHER_M=sum(AVAILABLE_MOLLUSKS*FINAL_WEIGHT/sum(AVAILABLE_MOLLUSKS*FINAL_WEIGHT/AVAILABLE_MOLLUSKS,na.rm=TRUE),na.rm = TRUE))
AVAI_M_s<-as.data.frame(AVAI_M_s)
AVAI_M_s$TOTAL_M<-(AVAI_M_s[,2]+AVAI_M_s[,3]+AVAI_M_s[,4]+AVAI_M_s[,5]+AVAI_M_s[,6]+AVAI_M_s[,7])/6


#SEA VEGETABLES
AVAI_V<-dplyr::select(CaquaD, "FINAL_WEIGHT","STATE","REGIONS",
                      "AVAILABLE_SEA_V")
#group and create column for total average
AVAI_V_s<-AVAI_V%>%
  group_by(STATE) %>%
  summarize(SEAV=sum(AVAILABLE_SEA_V*FINAL_WEIGHT/sum(AVAILABLE_SEA_V*FINAL_WEIGHT/AVAILABLE_SEA_V,na.rm=TRUE),na.rm = TRUE))
AVAI_V_s<-as.data.frame(AVAI_V_s)
AVAI_V_s

#MERGE .combine categories
AComb<-merge(AVAI_F_s,AVAI_C_s, by="STATE",all=TRUE)
AComb<-merge(AComb,AVAI_M_s, by="STATE",all=TRUE)
AComb<-merge(AComb,AVAI_V_s, by="STATE",all=TRUE)
colnames(AComb)[6]<-("ATLANTIC SALMON")
colnames(AComb)[7]<-("PACIFIC SALMON")
colnames(AComb)[23]<-("SEA VEGETABLES")

AVAILABILITY<-dplyr::select(AComb,1:8,11:13,16:20,23)#select all except for "other" and "total"
AVAI<-dplyr::select(AComb,1,A_F=10,A_C=15,A_M=22,A_S=23)#select only total

NEa<-colMeans(AComb[1:6,-1])
MAa<-colMeans(AComb[7:10,-1])
SEa<-colMeans(AComb[11:14,-1])

SRava<-as.data.frame(rbind(NEa,MAa,SEa))
SRava$STATE_NAME<-c("NORTHEAST","MIDATLANTIC","SOUTHEAST")
row.names(SRava)=c(15,16,17)
colnames(SRava)[23] <- "STATE"
SRava<-dplyr::select(SRava,1:7,10:12,15:19,22:23)#select all but "other" and "total"
AVAILABILITY<-rbind(AVAILABILITY,SRava)#combines state and subregion for chart graph
AVAILABILITY[15:17,1]<-c(15,16,17)#change NE to 15, MA to 16 and SE to 17 for click id

AVAILABILITY<-cbind(STATE=AVAILABILITY[,1],round(AVAILABILITY[,2:17],2)) #ROUND
AVAILABILITY



#STATE SHAPEFILE MERGE________________________________________________________________________________________-
STdata<-merge(CON,PREF, by="STATE",all=TRUE)
STdata<-merge(STdata,AVAI, by="STATE",all=TRUE)
STdata<-cbind(STATE=STdata[,1],round(STdata[,2:13],2))
STdata


#Write Files
write.csv(CONSUMPTION,"CONSUMPTION.csv")
write.csv(PREFERENCE,"PREFERENCE.csv")
write.csv(AVAILABILITY,"AVAILABILITY.csv")
write.csv(STdata,"StateData.csv")
