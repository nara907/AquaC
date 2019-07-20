###UI
varsL <- c(
  "State" = "state",
  "Sub-Region" = "subregion"
)
varsC <- c(
  "Consumption" = "Consumption",
  "Availability" = "Availability",
  "Preference" = "Preference",
  "Store Expenditure" = "Store Expenditure",
  "Restaurant Expenditure" = "Restaurant Expenditure",
  "Additional Expenditure" = "Additional Expenditure"
)
varsT <- c(
  "Finfish" = "Finfish",
  "Crustaceans" = "Crustaceans",
  "Mollusks" = "Mollusks",
  "Sea Vegetables" = "Sea Vegetables"
)
# bootstrapPage(
#   tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
#   leafletOutput("map", width = "100%", height = "100%"),
#   absolutePanel(top = 10, right = 10,
#                 selectInput("level", "Level", varsL, selected="state"),
#                 selectInput("category", "Category", varsC, selected = "Consumption"),
#                 selectInput("type", "Type", varsT, selected="Finfish"),
#                 style = "opacity: 0.65; z-index: 1000;"
#   )
# )

navbarPage("Aquaculture Map: 2018 Survey Results", id="nav",

           tabPanel("Interactive map",
                    div(class="outer",

                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css")
                        ),
                        leafletOutput("map", width="100%", height="100%"),
                        tags$head(tags$style(".leaflet-control-zoom { display: none; }")),
                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 480, height = "auto",

                                      h2("Atlantic States Aqualculture Survey"),
                                      

                                      selectInput("level", "Level", varsL, selected="state"),
                                      selectInput("category", "Category", varsC, selected = "Consumption"),
                                      selectInput("type", "Type", varsT, selected="Finfish"),
                                      style = "opacity: 0.65; z-index: 1000;",
                                      h4("All Types Breakdown"),
                                      plotOutput("plot")
                        ),

                        tags$div(id="cite",
                                 a(img(src='Atlantic-cmyk.jpg', width= "200px"), 
                                   target="_blank", 
                                   href="https://www.atlanticcorp.us/"),
                                 style = "opacity: 0.65; z-index: 1000;"
                        )
                    )
           ),
           tabPanel("About", fixedRow(
             column(8, 
                    tags$div(id="cite2",
                             a(img(src='Atlantic-cmyk.jpg', width= "200px"), target="_blank", href="https://www.atlanticcorp.us/"),
                             style = "z-index: 1000;"
                    ),
                    h4("__"),
                    h3("Aquaculture Survey 2018"),
                    h4("Survey"),
                    p("The aquaculture survey was conducted by Qualtrics®, 
                      a market research company, for Atlantic Corporation.  
                      Survey participants were members of a Qualtrics® survey 
                      panel located in one of the fifteen states bordering the 
                      Atlantic.  The survey was conducted online and contained 
                      approximately 40 questions, some multi-part.  A total of", 
                      em(6021), "records were collected."),
                    h4("Categories"),
                    h5("The following categories of aquaculture were covered in the survey:"),
                    p(code("Finfish:"), " Flounder,Striped Bass,Drum (Red or Black), 
                      Atlantic Salmon, Pacific/Alaskan Salmon,Cobia, Sturgeon"),
                    p(code("Crustaceans:"), " Shrimp, Lobster, Crabs"),
                    p(code("Mollusks:"), " Scallops, Clams, Oysters, Mussels, Abalone"),
                    p(code("Sea Vegetables"))
                    
                    ),
             column(8, offset=3,tags$div(id="cite",'Data compiled for ', tags$em('Atlantic Corporation'), ' by Nara McCray'))
             )),


           conditionalPanel("false", icon("crosshair"))
)