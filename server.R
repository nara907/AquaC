##SERVER

library(dplyr)
library(leaflet)
library(sf)
library(rgdal)
library(classInt)
library(ggplot2)
library(reshape2)
library(leaflet.extras)
function(input, output, session) {
  #INSIGHTS MAP
  output$map1 <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 4.5)) %>%
      addProviderTiles(providers$OpenStreetMap.HOT) %>%
      setView(-66.5, 37.4, zoom = 4.5) %>%
      setMaxBounds( lng1 = -66.9
                    , lat1 = 37
                    , lng2 = -66.1
                    , lat2 = 37.8 )
  })
  observe({
    Level <- input$level1
    Category <- input$category1
    
    if (Level == "state1"|Level == "select1") {
      dataM1 <- MAP_ST1 #state shapefile
    } 
    if (Level == "subregion1") {
      dataM1 <- MAP_SR1 #subregion shapefile
    }
    if (Level == "overall1") {
      dataM1 <- MAP_O1 #subregion shapefile
    }
    
    if (Category == "Farm-Raised Preference") {
      color <- W.pal
      dataM<-dataM1[,1:6] #subset consumption columns
      mypal<-colorNumeric( palette=color, domain=dataM@data[,6])
      pLa<-"% Preferring Farm-Raised Seafood: "
      en<-" "
      unitA="%"
    } 
    if (Category == "Willingness to Pay More") {
      color <- O.pal
      dataM<-dataM1[,c(1:5,7)] #subset avilability columns
      mypal<-colorNumeric( palette=color, domain=dataM@data[,6])
      pLa<-"% Willing to Pay More for Local Seafood: "
      en<-" "
      unitA="%"
    } 
    if (Category == "Locality Preference") {
      color <- Q.pal
      dataM<-dataM1[,c(1:5,8)] #subset preference columns
      mypal<-colorNumeric( palette=color, domain=dataM@data[,6])
      pLa<-"% Desiring State Detail about Seafood Origins: "
      en<-" "
      unitA="%"
    } 
    if (Category == "Locality Awareness") {
      color <- D.pal
      dataM<-dataM1[,c(1:5,9)] #subset preference columns
      mypal<-colorNumeric( palette=color, domain=dataM@data[,6])
      pLa<-"% Awareness of Origins of Seafood Consumed: "
      en<-" "
      unitA="%"
    } 
    if (Category == "Unmet Consumer Demand") {
      color <- B.pal
      dataM<-dataM1[,c(1:5,10)] #subset preference columns
      mypal<-colorNumeric( palette=color, domain=dataM@data[,6])
      pLa<-"Total Unmet Consumer Demand: $"
      en<-" $ per household/year"
      unitA=" $"
    } 
    
    map1<-leafletProxy("map1") %>%
      clearShapes() %>%
      clearControls()%>%
      addPolygons(data= statesNA,    
                  color = "grey",
                  weight = 1,
                  opacity = 0.7,
                  smoothFactor = 0.5,
                  fill = TRUE,
                  fillColor = "white",
                  fillOpacity = .1)%>%
      addPolygons(data= dataM,    
                  color = "black",
                  weight = 1.3,
                  opacity = 0.6,
                  highlightOptions = highlightOptions(color = "black", weight = 2,
                                                      bringToFront = TRUE, sendToBack = TRUE),
                  label= paste(dataM$STATE_NAME," (", pLa, dataM@data[,6],en, ")"),
                  labelOptions = labelOptions(noHide = F,
                                              style = list(
                                                "font-weight" = "bold",
                                                "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                "font-size" = "14px"
                                              )),
                  smoothFactor = 0.5,
                  fill = TRUE,
                  layerId = ~STATE,
                  popup=paste0(dataM$STATE_NAME, "<br>",'<h7 style="color:black;">',  
                               pLa, "<b>", dataM@data[,6],en, "</b>", '</h7>'),
                  fillColor = ~mypal(dataM@data[,6]),
                  fillOpacity = .5)

    map1 %>% clearControls()
    if (input$level1 == "state1" |input$level1 == "subregion1") {
      map1 %>% addLegend("topleft", pal = mypal, values = dataM@data[,6],
                         labFormat=labelFormat(suffix=unitA),
                         title = "Legend",
                         opacity = 0.6)
    }

  })
  
#INSIGHTS MAP GRAPH
  graphData1<-reactive({
    if (input$category1 == "Farm-Raised Preference")
      r<-FARM
    if (input$category1 == "Willingness to Pay More")
      r<-PAYMORE
    if (input$category1 == "Locality Preference")
      r<-LOCATION
    if (input$category1 == "Locality Awareness")
      r<-LOCATIONA
    if (input$category1 == "Unmet Consumer Demand")
      r<-UNMET1
    r
  })
  symbolS1<-reactive({
    if (input$category1 == "Farm-Raised Preference")
      r<-"Farm-Raised vs. Wild Caught Seafood: %"
    if (input$category1 == "Willingness to Pay More")
      r<-"Willingness to Pay More: %"
    if (input$category1 == "Locality Preference")
      r<-"Geographical Detail Desired: %"
    if (input$category1 == "Locality Awareness")
      r<-"Frequency of Geographical Awareness: %"
    if (input$category1 == "Unmet Consumer Demand")
      r<-"Unmet Consumer Demand: $(USD) per household"
    r
  })
  gheight<-reactive({
    if (input$category1 == "Farm-Raised Preference")
      r<-200
    if (input$category1 == "Willingness to Pay More")
      r<-170
    if (input$category1 == "Locality Preference")
      r<-150
    if (input$category1 == "Locality Awareness")
      r<-100
    if (input$category1 == "Unmet Consumer Demand")
      r<-330
    r
  })
  captionT<-reactive({
    if (input$category1 == "Farm-Raised Preference")
      r<-"Survey Question: Assuming the same pricing, which of the following would \n you prefer to purchase? (choose one for each item)"
    if (input$category1 == "Willingness to Pay More")
      r<-"Survey Question: How much more would you be willing to pay, if anything, for \n the following categories of farm-raised seafood?"
    if (input$category1 == "Locality Preference")
      r<-"Survey Question: How much detail do you desire about the \n geographic origin of your seafood? (choose one)"
    if (input$category1 == "Locality Awareness")
      r<-"Survey Question: How frequently are you aware of where seafood \n is grown prior to purchasing?"
    if (input$category1 == "Unmet Consumer Demand")
      r<-"Survey Question: If the following farm-raised seafood was more readily available \n for you to purchase in your area, approximately how much more would you purchase annually?"
    r
  })
  output$plot1<-renderPlot({
    event <- (input$map1_shape_click) #Critical Line!!!
    
    validate(need(event$id != "",
                  "Please click on area of interest for details"))
    #title of graph
    nn<-match(event$id, titleNames$STATE)
    titleN <- paste0(as.character(titleNames[nn,2]),"\n", symbolS1())
    
    n<-match(event$id, graphData1()[,1])
    state_data<-graphData1()[n,]
    state_data<-melt(state_data, id.vars="STATE")
    
    c1 <- state_data$variable %in% c("STRIPED.BASS" , "COBIA","DRUM","FLOUNDER","HALIBUT","ATLANTIC.SALMON","PACIFIC.SALMON",
                                     "STURGEON","OTHER.FINFISH")
    c2<-state_data$variable %in% c("CRABS" ,"LOBSTERS","LOBSTER","SHRIMP", "OTHER.CRUSTACEANS" )
    c3<-state_data$variable %in% c("ABALONE","OYSTERS", "CLAMS","MUSSELS","SCALLOPS", "OTHER.MOLLUSKS")
    c4<-state_data$variable %in% c("SEA.VEGETABLES")
    c5<-state_data$variable %in% c("WILD.FINFISH","FARMED.FINFISH","NO.FINFISH.PREFERENCE", "WILD.SHELLFISH", "FARMED.SHELLFISH",
                                   "NO.SHELLFISH.PREFERENCE", "WILD.SEAVEGETABLES", "FARMED.SEAVEGETABLES","NO.SEAVEGETABLES.PREFERENCE",
                                   "FROZEN","FRESH", "LOCAL","ORGANIC", "NON.GMO", "SUSTAINABLE", "DOMESTIC", "TOWN.LEVEL", "COUNTY.LEVEL",
                                   "COUNTRY.LEVEL", "STATE.LEVEL", "NONE", "LOCATION.AWARENESS", "TOTAL")
    state_data$Type[c1] <- "Finfish"
    state_data$Type[c2] <- "Crustaceans"
    state_data$Type[c3] <- "Mollusks"
    state_data$Type[c4] <- "Sea Vegetables"
    state_data$Type[c5] <- ""
    
    ggplot(state_data, aes(x=variable,y=value, fill=Type))+
      geom_bar(stat="identity",width=0.75)+ 
      scale_fill_manual(values = color_table2) +
      coord_flip()+
      ggtitle(titleN)+
      labs(caption=captionT())+
      geom_text(aes(y = 0-(max(state_data$value)*0.07),    # nudge above top of bar
                    label = paste0(value)),    
                position = position_dodge(width = .9), 
                size = 3)+ 
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y = element_text(margin = margin(r =5)),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.position="bottom",
            legend.margin=margin(0,0,0,0),
            legend.box.margin=margin(-10,-10,2,-10),
            panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_blank())
    
  },
  height = gheight,
  width = 440)
  
  #GENERAL FINDINGS MAP
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 4.5)) %>%
      addProviderTiles(providers$OpenStreetMap.HOT) %>%
      setView(-66.5, 37.4, zoom = 4.5) %>%
      setMaxBounds( lng1 = -66.9
                    , lat1 = 37
                    , lng2 = -66.1
                    , lat2 = 37.8 )
  })
  observe({
    Level <- input$level
    Category <- input$category
    Type<- input$type
    
    if (Level == "state" |Level == "select") {
      dataM <- MAP_ST #state shapefile
      } 
    if (Level == "subregion") {
      dataM <- MAP_SR #subregion shapefile
    }
    if (Level == "overall") {
      dataM <- MAP_O #subregion shapefile
    }
    
    if (Category == "Consumption") {
      color <- C.pal
      dataM<-dataM[,1:9] #subset consumption columns
      pLa<-"% Consuming: "
      en<-" "
      unitA="%"
    } 
    if (Category == "Availability") {
      color <- A.pal
      dataM<-dataM[,c(1:5,10:13)] #subset avilability columns
      pLa<-"Availability Rating: "
      en<-" /5"
      unitA=" /5"
    } 
    if (Category == "Preference") {
      color <- P.pal
      dataM<-dataM[,c(1:5,14:17)] #subset preference columns
      pLa<-"Preference Rating: "
      en<-" /5"
      unitA=" /5"
    } 
    if (Category == "Store Expenditure") {
      color <- S.pal
      dataM<-dataM[,c(1:5,22:25)] #subset preference columns
      pLa<-"Store Expenditure: $"
      en<-" per year"
      unitA=" $"
    } 
    if (Category == "Restaurant Expenditure") {
      color <- R.pal
      dataM<-dataM[,c(1:5,18:21)] #subset preference columns
      pLa<-"Restaurant Expenditure: $"
      en<-" per year"
      unitA=" $"
    } 
    if (Category == "Additional Expenditure") {
      color <- B.pal
      dataM<-dataM[,c(1:5,26:29)] #subset preference columns
      pLa<-"Additional Expenditure: $"
      en<-" per year"
      unitA=" $"
    } 
    if (Type == "Finfish") {
      tselect<-dataM@data[,6]
      mypal<-colorNumeric( palette=color, domain=tselect) #create breaks based on data
      typeN="Finfish"
    } 
    if (Type == "Crustaceans") {
      tselect<-dataM@data[,7]
      mypal<-colorNumeric( palette=color, domain=tselect) #create breaks based on data
      typeN="Crustaceans"
    } 
    if (Type == "Mollusks") {
      tselect<-dataM@data[,8]
      mypal<-colorNumeric( palette=color, domain=tselect) #create breaks based on data
      typeN="Mollusks"
    } 
    if (Type == "Sea Vegetables") {
      tselect<-dataM@data[,9]
      mypal<-colorNumeric( palette=color, domain=tselect) #create breaks based on data
      typeN="Sea Vegetables"
    } 
    map<-leafletProxy("map") %>%
      clearShapes() %>%
      clearControls()%>%
      addPolygons(data= statesNA,    
                  color = "grey",
                  weight = 1,
                  opacity = 0.7,
                  smoothFactor = 0.5,
                  fill = TRUE,
                  fillColor = "white",
                  fillOpacity = .1)%>%
      addPolygons(data= dataM,    
                  color = "black",
                  weight = 1.3,
                  opacity = 0.6,
                  highlightOptions = highlightOptions(color = "black", weight = 2,
                                                      bringToFront = TRUE, sendToBack = TRUE),
                  label= paste(dataM$STATE_NAME," (", pLa, tselect,en, ")"),
                  labelOptions = labelOptions(noHide = F,
                                              style = list(
                                                "font-weight" = "bold",
                                                "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                "font-size" = "14px"
                                              )),
                  smoothFactor = 0.5,
                  fill = TRUE,
                  layerId = ~STATE,
                  popup=paste0(dataM$STATE_NAME, "<br>",'<h7 style="color:black;">',  
                               pLa, "<b>", tselect,en, "</b>", '</h7>', "<br>","Aquaculture Type: ", typeN),
                  fillColor = ~mypal(tselect),
                  fillOpacity = .5)

    map %>% clearControls()
    if (input$level == "state" | input$level == "subregion") {
      map %>% addLegend("topleft", pal = mypal, values = tselect,
                         labFormat=labelFormat(suffix=unitA),
                         title = "Legend",
                         opacity = 0.6)
    }

  })
  #GENERAL FINDINGS MAP GRAPH
#need to create object which stores graphData/graphD values based on category selected
  graphData<-reactive({
    if (input$category == "Consumption")
      r<-CONSUMPTION
    if (input$category == "Availability")
      r<-AVAILABILITY
    if (input$category == "Preference")
      r<-PREFERENCE
    if (input$category == "Store Expenditure")
      r<-STORE
    if (input$category == "Restaurant Expenditure")
      r<-REST
    if (input$category == "Additional Expenditure")
      r<-BUY
    r
  })
  symbolS<-reactive({
    if (input$category == "Consumption")
      r<-"Consumption: % of Households"
    if (input$category == "Availability")
      r<-"Availability: Rating out of 5"
    if (input$category == "Preference")
      r<-"Preference: Rating out of 5"
    if (input$category == "Store Expenditure")
      r<-"Store Expenditure: USD($) per Year"
    if (input$category == "Restaurant Expenditure")
      r<-"Restaurant Expenditure: USD($) per Year"
    if (input$category == "Additional Expenditure")
      r<-"Additional Expenditure: USD($) per Year"
    r
  })
  captionT1<-reactive({
    if (input$category == "Consumption")
      r<-"Survey Prompt: Please identify the following seafood you have consumed."
    if (input$category == "Availability")
      r<-"Survey Question: How easy is it for you to purchase the following seafood in your \n immediate area: (1 = never available to 5 = always available)?"
    if (input$category == "Preference")
      r<-"Survey Prompt: Please rate the following seafood you are familiar with: \n (1 = extremely dislike to 5 = extremely like)"
    if (input$category == "Store Expenditure")
      r<-"Survey Question: How much do you spend annually on the following seafood in food stores?"
    if (input$category == "Restaurant Expenditure")
      r<-"Surey Question: How much do you spend annually on the following seafood in \n restaurants/takeout?"
    if (input$category == "Additional Expenditure")
      r<-"Survey Question: If the following farm-raised seafood was more readily available \n for you to purchase in your area, approximately how much more would you purchase annually?"
    r
  })
  output$plot<-renderPlot({
    event <- (input$map_shape_click) #Critical Line!!!

    validate(need(event$id != "",
                  "Please click on area of interest for details"))
    #title of graph
    nn<-match(event$id, titleNames$STATE)
    titleN <- paste0(as.character(titleNames[nn,2]),"\n", symbolS())
    
    n<-match(event$id, graphData()[,1])
    state_data<-graphData()[n,]
    state_data<-melt(state_data, id.vars="STATE")
    c1 <- state_data$variable %in% c("STRIPED.BASS" , "COBIA","DRUM","FLOUNDER","HALIBUT","ATLANTIC.SALMON","PACIFIC.SALMON","STURGEON","OTHER.FINFISH")
    c2<-state_data$variable %in% c("CRABS" ,"LOBSTERS","LOBSTER","SHRIMP", "OTHER.CRUSTACEANS" )
    c3<-state_data$variable %in% c("ABALONE","OYSTERS", "CLAMS","MUSSELS","SCALLOPS", "OTHER.MOLLUSKS")
    c4<-state_data$variable %in% c("SEA.VEGETABLES")
    state_data$Type[c1] <- "Finfish"
    state_data$Type[c2] <- "Crustaceans"
    state_data$Type[c3] <- "Mollusks"
    state_data$Type[c4] <- "Sea Vegetables"
    ggplot(state_data, aes(x=variable,y=value, fill=Type))+
      geom_bar(stat="identity",width=0.75)+ 
      scale_fill_manual(values = color_table) +
      coord_flip()+
      labs(caption=captionT1())+
      ggtitle(titleN)+
      geom_text(aes(y = 0-(max(state_data$value)*0.07),    # nudge 
                    label = paste0(value)),    
                position = position_dodge(width = .9), 
                size = 3.5)+ 
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y = element_text(margin = margin(r =10)),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),legend.position="bottom",
            legend.margin=margin(5,0,8,0),
            legend.box.margin=margin(-10,-10,-5,-10),
            panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_blank())
    
    },
  height = 300,
  width = 440)
}


