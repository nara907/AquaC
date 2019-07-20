###UI
varsL <- c(
  "[SELECT]"="select",
  "State" = "state",
  "Sub-Region" = "subregion",
  "Region"="overall"
)
varsL1 <- c(
  "[SELECT]"="select1",
  "State" = "state1",
  "Sub-Region" = "subregion1",
  "Region"="overall1"
)
varsC <- c(
  "Consumption" = "Consumption",
  "Availability" = "Availability",
  "Preference" = "Preference",
  "Store Expenditure" = "Store Expenditure",
  "Restaurant Expenditure" = "Restaurant Expenditure"
 # "Additional Expenditure" = "Additional Expenditure"
)
varsM1 <- c(
  "Unmet Consumer Demand" = "Unmet Consumer Demand",
  "Farm-Raised Preference" = "Farm-Raised Preference",
  "Willingness to Pay More" = "Willingness to Pay More",
  "Demand for Geographic Detail of Source" = "Locality Preference",
  "Source Location Awareness" = "Locality Awareness"
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
           tabPanel("About", fixedRow(
             column(9, 
                    tags$div(id="cite2",
                             a(img(src='atllogo.png', width= "290px"), target="_blank", href="https://www.atlanticcorp.us/"),
                             style = "opacity: 0.55;z-index: 1000;"
                    ),
                    #h4("__"),
                    h3(tags$div(id="headT","Atlantic States Seafood Consumer Survey 2018")),
                    h4(tags$strong("Survey")),
                    p("The 36-question Atlantic States Seafood Consumer Survey was developed by the extended Atlantic team with 
                      final editorial suggestions provided by three marine fisheries and aquaculture experts, two from the Mid-Atlantic
                      region and one from the Southeast. It was hosted and administered online during the summer of 2018 by Qualtrics®,
                      a leading international business and market research company, on behalf of Atlantic. Resulting data were reviewed,
                      analyzed and summarized by the Maine Statistical Analysis Center (MSAC). A total of", 
                      em(6021), "records were collected.The data presented in this tool and in the final report is comprised of averages
                      calculated from the seafood consuming subset of the sample (92% of the respondents)."),
        
                    tags$hr(style="border-color: gray;"),
                    h4(tags$strong("Spatial Scope")),
                    h5(tags$strong("State Level:")),  
                    h5(" Maine, New Hampshire, Massachusetts, Rhode Island,
                       Connecticut, New York, New Jersey, Delaware, Maryland,
                       Virginia, North Carolina, South Carolina, Georgia, Florida"),
                    h5(tags$strong("Sub-Region:")),
                    h5(" Northeast", 
                       em("(Maine, New Hampshire, Massachusetts, Rhode Island,
                                                                   Connecticut, New York)")),
                       h5("Mid-Atlantic", 
                       em("(New Jersey, Delaware, Maryland,
                       Virginia)")),
                       h5("Southeast",
                       em("(North Carolina, South Carolina, Georgia, Florida)")),
                    
                    
                    h5(tags$strong("Region:")),
                    h5("Atlantic States"),
                    tags$hr(style="border-color: gray;"),
                    h4(tags$strong("Categories")),
                    h5("The following categories of aquaculture were covered in the survey:"),
                    p(tags$strong("Finfish:"), " Flounder, Striped Bass, Drum (Red or Black), 
                      Atlantic Salmon, Pacific/Alaskan Salmon, Cobia, Sturgeon"),
                    p(tags$strong("Shellfish--Crustaceans:"), " Shrimp, Lobster, Crabs"),
                    p(tags$strong("Shellfish--Mollusks:"), " Scallops, Clams, Oysters, Mussels, Abalone"),
                    p(tags$strong("Sea Vegetables")),
                    tags$hr(style="border-color: gray;"),
                    h4(tags$strong("Contributors")),
                    p("Randy Labbe,", tags$sup(1), em("Project Director")),
                    p("Ray Bernier,", tags$sup(1), em("Co-Project Director")),
                    p("Nara McCray,", tags$sup(1), em("GIS Analyst")),
                    p("Dylan Bouchard,", tags$sup(1), em("Agricultural Economics Consultant")),
                    p("George Shaler,", tags$sup(2), em("Data Consultant")),
                    p("Robyn Dumont,", tags$sup(2),em("Data Consultant")),
                    p("Chris Davis,", tags$sup(3),em("Research Consultant")),
                    p("Mary Ellen Camire,", tags$sup(4), em("Research Consultant")),
                    p(tags$sup("1 Atlantic Corporation, 44 Main Street – Suite 205, Waterville, ME 04901")),
                    p(tags$sup("2 Maine Statistical Analysis Center, University of Southern Maine, 34 Bedford Street, Portland, ME 04101")),
                    p(tags$sup("3 Maine Aquaculture Innovation Center, 193 Clarks Cove Road, Walpole, ME 04573")),
                    p(tags$sup("4 University of Maine, School of Food and Agriculture, 5735 Hitchner Hall, Orono, ME 04469")),
                    tags$hr(style="border-color: gray;"),
                    h4(tags$strong("Acknowledgement")),
                    p("This report was prepared by Atlantic Corporation under award number NAJ4NMF4740362 from
                      the National Oceanic and Atmospheric Administration, U.S. Department of Commerce. The 
                      statements, findings, conclusions, and recommendations are those of the author(s) and do
                      not necessarily reflect the views of the National Oceanic and Atmospheric Administration, 
                      the Department of Commerce, or the Atlantic States Maine Fisheries Commission."),
                    tags$hr(style="border-color: gray;"),
                    h4(tags$strong("Contact:"))
                    ),
             column(8,HTML('<p style="font-size: 13.5px;text-align:left">For questions about this survey, please <a href="https://www.atlanticcorp.us/contact">contact Atlantic Corp</a>.</p>')
             )
                    )),
           
           tabPanel("Insights Map",
                    div(class="outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css")
                        ),
                        leafletOutput("map1", width="100%", height="100%"),
                        tags$head(tags$style(".leaflet-control-zoom { display: none; }")),
                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controlsM1", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 480, height = "auto",
                                      
                                      h4("Atlantic States Seafood Consumer Survey 2018 Insights"),
                                      
                                      selectInput("level1", "Level", varsL1),
                                      selectInput("category1", "Category", varsM1, selected = "Unmet Consumer Demand"),
                                      style = "opacity: 0.65; z-index: 1000;",
                                      h5("Data Breakdown"),
                                      plotOutput("plot1")
                        ),
                        
                        tags$div(id="cite",
                                 a(img(src='atllogo.png', width= "200px"), 
                                   target="_blank", 
                                   href="https://www.atlanticcorp.us/"),
                                 style = "opacity: 0.85; z-index: 1000;"
                        )
                    )
           ),

           tabPanel("General Findings Map",
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

                                      h4("Atlantic States Seafood Consumer Survey 2018 General Findings"),
                                      

                                      selectInput("level", "Level", varsL),
                                      selectInput("category", "Category", varsC, selected = "Consumption"),
                                      selectInput("type", "Type", varsT, selected="Finfish"),
                                      style = "opacity: 0.65; z-index: 1000;",
                                      h5("All Types Breakdown"),
                                      plotOutput("plot")
                        ),

                        tags$div(id="cite",
                                 a(img(src='atllogo.png', width= "200px"), 
                                   target="_blank", 
                                   href="https://www.atlanticcorp.us/"),
                                 style = "opacity: 0.85; z-index: 1000;"
                        )
                    )
           ),
           tabPanel("User Guide",
                    tags$head(
                      # Include our custom CSS
                      includeCSS("styles.css")
                    ),
                    
                    h2("About this application"),
                    HTML(
                      '<p style="text-align:left">This interactive web tool displays results of the 2018 Atlantic States Seafood Consumer Survey conducted in the U.S. Atlantic coastal states.
                      The spatial format allows one to assess the size and characteristics of specific target markets by interactively viewing details and statistics by state, sub-region and region.
                      The "Insights" and "General Findings" maps show spatially coordinated data from the Atlantic States Seafood Consumer Survey 2018.</p>'
                    ),
                    wellPanel(
                      p(strong("Map Usage:")),
                      p("Select LEVEL (state, sub-region, region), CATEGORY (category of data  displayed, ex: Consumption) and TYPE (class of seafood)."),
                      p("The resulting map displays averaged figures. Explore insights relating to shellfish, finfish and sea vegetable market opportunities by state, sub-regions, or the entire Atlantic region 
                        by selecting the area of interest on the map."),
                      p("A graph relating to the selected geographical area will display sub-category details in the sidepanel.")
                      ),
                    h3("Insights Map"),
                    h5(tags$strong("Unmet Consumer Demand:")),
                    HTML('<p style="font-size: 12px;text-align:left">Results from the survey question- "If the following farm-raised seafood was more readily available for you to purchase in your area, approximately how much more would you purchase annually?"</p>
                         <p style="font-size: 12px;text-align:left">Pop-up shows the total annual demand of all seafood categories combined. Graph displays
                         the breakdown of spending on different varieties within all categories.</p>'),
                    h5(tags$strong("Farm-Raised Preference:")),
                    HTML('<p style="font-size: 12px;text-align:left">Results from the survey question- "Assuming the same pricing, which of the following would you prefer to purchase? (choose one for each item)"</p>
                         <p style="font-size: 12px;text-align:left">Pop-up shows percent of those preferring farm-raised seafood overall. Graph displays a breakdown of preference for farm-raised, wild-caught, and no preference in the categories of
                         finfish, shellfish and sea vegetables</p>'),
                    h5(tags$strong("Willingness to Pay More:")),
                    HTML('<p style="font-size: 12px;text-align:left">Results from the survey question- "How much more would you be willing to pay, if anything, for the following categories of farm-raised seafood?"</p>
                         <p style="font-size: 12px;text-align:left">Pop-up shows percent of those willing to pay more for locally harvested seafood. Graph displays a breakdown of how much the population is willing to pay for domestic, sustainable, 
                         non-GMO, organic, local, fresh, frozen seafood.</p>'),
                    h5(tags$strong("Demand for Geographic Location:")),
                    HTML('<p style="font-size: 12px;text-align:left">Results from the survey question- "How much detail do you desire about the geographic origin of your seafood? (choose one)"</p>
                         <p style="font-size: 12px;text-align:left">Pop-up shows percent of those desiring state-level detail about the origins of their seafood. Graph displays a breakdown of those desiring detail at the country, county, state, town, 
                         and no particular level.</p>'),
                    h5(tags$strong("Source Location Awareness:")),
                    HTML('<p style="font-size: 12px;text-align:left">Results from the survey question- "How frequently are you aware of where seafood is grown prior to purchasing?"</p>
                         <p style="font-size: 12px;text-align:left">Pop-up and graph shows frequency of awareness of the origins of seafood purchased.</p>'),
                    h3("General Findngs Map"),
                    
                    
                    h5(tags$strong("Consumption")),
                    HTML('<p style="font-size: 12px;text-align:left">Results from the survey prompt- "Please identify the following seafood you have consumed."</p>
                         <p style="font-size: 12px;text-align:left">Pop-up shows percentage of consumption of the selected category: finfish, shellfish, mollusks, or sea vegetables. Graph displays
                         the breakdown of consumption of different varieties within all categories.</p>'),
                    h5(tags$strong("Availability")),
                    HTML('<p style="font-size: 12px;text-align:left">Results from the survey question- "How easy is it for you to purchase the following seafood in your immediate area: (1 = never available to 5 = always available)?"</p>
                         <p style="font-size: 12px;text-align:left">Pop-up shows rating of seafood availability in the selected category: finfish, shellfish, mollusks, or sea vegetables. Graph displays
                         the breakdown of the availability of different varieties of all categories.</p>'),
                    h5(tags$strong("Preference")),
                    HTML('<p style="font-size: 12px;text-align:left">Results from the survey prompt- "Please rate the following seafood you are familiar with: (1 = extremely dislike to 5 = extremely like)"</p>
                         <p style="font-size: 12px;text-align:left">Pop-up shows preference rating of the selected category: finfish, shellfish, mollusks, or sea vegetables. Graph displays
                         the breakdown of the preference of different varieties within all categories.</p>'),
                    h5(tags$strong("Store Expenditure")),
                    HTML('<p style="font-size: 12px;text-align:left">Results from the survey question- "How much do you spend annually on the following seafood in food stores?"</p>
                         <p style="font-size: 12px;text-align:left">Pop-up shows average annual spending on the selected category: finfish, shellfish, mollusks, or sea vegetables. Graph displays
                         the breakdown of spending on different varieties within all categories.</p>'),
                    h5(tags$strong("Restaurant Expenditure")),
                    HTML('<p style="font-size: 12px;text-align:left">Results from the survey question- "How much do you spend annually on the following seafood in restaurants/takeout?"</p>
                         <p style="font-size: 12px;text-align:left">Pop-up shows average annual spending on the selected category: finfish, shellfish, mollusks, or sea vegetables. Graph displays
                         the breakdown of spending on different varieties within all categories.</p>'),
                    tags$div(
                             a(img(src='atllogo.png', width= "125px"), 
                               target="_blank", 
                               href="https://www.atlanticcorp.us/"),
                             style = "opacity: 0.85; z-index: 1000;text-align:center;"
                    ),
                    HTML('<p style="font-size: 11px;text-align:center">For questions about this survey, please <a href="https://www.atlanticcorp.us/contact">contact Atlantic Corp</a>.</p>')),
           


           conditionalPanel("false", icon("crosshair"))
)