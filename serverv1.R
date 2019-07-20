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
    
    if (Level == "state") {
      dataM <- MAP_ST #state shapefile
      
    } 
    if (Level == "subregion") {
      dataM <- MAP_SR #subregion shapefile
      
    }
    
    if (Category == "Consumption") {
      color <- C.pal
      dataM<-dataM[,1:9] #subset consumption columns
      pLa<-"% Consuming: "
      en<-" "
    } 
    if (Category == "Availability") {
      color <- A.pal
      dataM<-dataM[,c(1:5,14:17)] #subset avilability columns
      pLa<-"Availability Rating: "
      en<-" /5"
    } 
    if (Category == "Preference") {
      color <- P.pal
      dataM<-dataM[,c(1:5,10:13)] #subset preference columns
      pLa<-"Preference Rating: "
      en<-" /5"
    } 
    if (Category == "Store Expenditure") {
      color <- S.pal
      dataM<-dataM[,c(1:5,26:29)] #subset preference columns
      pLa<-"Store Expenditure: $"
      en<-" per year"
    } 
    if (Category == "Restaurant Expenditure") {
      color <- R.pal
      dataM<-dataM[,c(1:5,22:25)] #subset preference columns
      pLa<-"Restaurant Expenditure: $"
      en<-" per year"
    } 
    if (Category == "Additional Expenditure") {
      color <- B.pal
      dataM<-dataM[,c(1:5,18:21)] #subset preference columns
      pLa<-"Additional Expenditure: $"
      en<-" per year"
    } 
    if (Type == "Finfish") {
      tselect<-dataM@data[,6]
      mypal<-colorNumeric( palette=color, domain=tselect) #create breaks based on data
    } 
    if (Type == "Crustaceans") {
      tselect<-dataM@data[,7]
      mypal<-colorNumeric( palette=color, domain=tselect) #create breaks based on data
    } 
    if (Type == "Mollusks") {
      tselect<-dataM@data[,8]
      mypal<-colorNumeric( palette=color, domain=tselect) #create breaks based on data
    } 
    if (Type == "Sea Vegetables") {
      tselect<-dataM@data[,9]
      mypal<-colorNumeric( palette=color, domain=tselect) #create breaks based on data
    } 
    leafletProxy("map") %>%
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
                  popup=paste0(dataM$STATE_NAME, "<br>",'<h7 style="color:black;">',  pLa, "<b>", tselect,en, "</b>", '</h7>'),
                  fillColor = ~mypal(tselect),
                  fillOpacity = .5)%>%
      addLegend("topleft", pal = mypal, values = tselect,
                title = "Legend",
                opacity = 0.6)
})
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
  output$plot<-renderPlot({
    event <- (input$map_shape_click) #Critical Line!!!

    validate(need(event$id != "",
                  "Please click on area of interest for details"))
    #title of graph
    nn<-match(event$id, titleNames$STATE)
    titleN <- paste0(as.character(titleNames[nn,2]),"\n", symbolS()) #try matching with any shapefiledata or a new dataframe
    
    n<-match(event$id, graphData()[,1])
    state_data<-graphData()[n,]
    state_data<-melt(state_data, id.vars="STATE")
    ggplot(state_data, aes(x=variable,y=value))+
      geom_bar(stat="identity", fill="black",width=0.75)+ 
      coord_flip()+
      ggtitle(titleN)+
      geom_text(aes(y = 0-(max(state_data$value)*0.07),    # nudge above top of bar
                    label = paste0(value)),    # prettify
                position = position_dodge(width = .9), 
                size = 3.5)+ 
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y = element_text(margin = margin(r =10)),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),legend.position="none",
            panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_blank())
    
    },
  height = 300,
  width = 440)
}


