CONSUMPTION@data
CartoDB.DarkMatterNoLabels
Esri.WorldGrayCanvas
CartoDB.PositronNoLabels
mypal = colorNumeric( palette="Blues", domain=CONSUMPTION$TOTAL_M)
mytext=paste("Country: ", CONSUMPTION$STATE_NAME,"<br/>", "Area: ", CONSUMPTION$STATE, "<br/>", "Population: ", CONSUMPTION$TOTAL_M, sep="") %>%
  lapply(htmltools::HTML)

ts<-CONSUMPTION@data[,7]
mypal<-colorNumeric( palette="Blues", domain=ts)

numIDP<- leaflet() %>%
    addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
    setView(-70, 34, zoom = 4)%>%
  addPolygons(data= CONSUMPTION,    # NUMBER OF IDPS
              color = "black",
              weight = 0.7,
              opacity = 0.5,
              label= CONSUMPTION$STATE_NAME,
              popup = mytext,
              smoothFactor = 0.5,
              fill = TRUE,
              fillColor = ~mypal(CONSUMPTION@data[,27]),
              fillOpacity = .9)
numIDP

nn<-match(5, CONSUMPTION[,1])

which(CONSUMPTION[,1]==5)
CONSUMPTION
nn<-CONSUMPTION[nn,]
nn
ccomb<-melt(nn,id.vars="STATE")
ccomb
c1 <- ccomb$variable %in% c("BASS" , "COBIA","FLOUNDER","HALIBUT","ATLANTIC.SALMON","PACIFIC.SALMON","STURGEON")
c2<-ccomb$variable %in% c("CRABS" , "LOBSTERS","SHRIMP")
c3<-ccomb$variable %in% c("ABALONE","CLAMS","MUSSELS","SCALLOPS")
c4<-ccomb$variable %in% c("SEA.VEGETABLES")
ccomb$Type[c1] <- "Finfish"
ccomb$Type[c2] <- "Crustaceans"
ccomb$Type[c3] <- "Mollusks"
ccomb$Type[c4] <- "Sea Vegetables"
ggplot(ccomb, aes(x=variable,y=value, fill=type1))+
  geom_bar(stat="identity")+ 
  coord_flip()+
  geom_text(aes(y = value + 9,    # nudge above top of bar
                label = paste0(round(value))),    # prettify
            position = position_dodge(width = .9), 
            size = 3)+ 
  scale_fill_manual(values = color_table) +
  theme(axis.line=element_blank(),
                             axis.text.x=element_blank(),
                             axis.ticks=element_blank(),
                             axis.title.x=element_blank(),
                             axis.title.y=element_blank(),legend.position="bottom",
                             panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                             panel.grid.minor=element_blank(),plot.background=element_blank())
PREFERENCE
CONSUMPTION
#create popup
#add description about out of 5 rating
#add disclaimer
#add contact information to get detailed survey findings
#add details about date and survey specifics (n, data scientists) in a new tab
#change seav to sea vegetables
#fix state bar on chart and .x values???
