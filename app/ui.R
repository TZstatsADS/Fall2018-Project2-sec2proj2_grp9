################################
vars <- c(
  "Columbia University" = "Columbia University",
  "Time Square" = "Time Squares",
  "Empire State Building" = "Empire State Building",
  "New York University" = "New York University",
  "Columbus Circle" = "Columbus Circle"
)


vars3 <- c("True" = "True",
           "Flase" = "False")

vars4 <- c("1" = "1","2" = "2","3" = "3", "4" ="4", "5" = "5")

library(leaflet)
library(ggmap)
library(geosphere)
#install.packages("rjson")
library(rjson)
library(devtools)
#install_github("dkahle/ggmap")
#install.packages("shiny")
library(shiny)
#install.packages("shinydashboard")
#install_github("r-spatial/sf")
#install.packages("stars")
#install.packages("mapview")
library(shinydashboard)
library(dplyr)
library(ggplot2)
#install.packages("formattable")
#install.packages("shinyWidgets")
library(shinyWidgets)
#library(mapview)
#setwd("/Users/somno/Desktop/nyc yellow cab app")
register_google("AIzaSyDF7L0gqBYv1GLyxgBlfLtYGN7ugQogSeg")

#################################
title <- tags$a(
  tags$img(src="yellow.jpg",height = '50',width = '50'),
'NYC Yellow Taxi')

ui <- dashboardPage(
  
  skin = "yellow",
  dashboardHeader(title = title, titleWidth = 250),
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      
      menuItem("Data Exploration", tabName = "DataExploration", icon = icon("area-chart")),
      menuItem("See Your Neighbourhood", tabName = "SeeYourNeighbourhood", icon = icon("car")),
      menuItem("Interactive Map", tabName = "map", icon = icon("map")),
      menuItem("Interesting Tips", tabName = "Tips", icon = icon("dollar")),
      menuItem("About", tabName = "About", icon = icon("book"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      
      tabItem(tabName = "DataExploration"
               ,fluidRow(
                 tabBox(width = 12,
                        tabPanel("Pickup", width = 12, leafletOutput("Pickup",height = 700)
                        ),
                        tabPanel("Dropoff",width = 12, leafletOutput("Dropoff",height = 700)),
                        tabPanel("Trip Duration & Distance",
                                 valueBoxOutput("mean"),
                                 valueBoxOutput("median"),
                                 
                                 box(title = "Trip Duration", "", solidHeader = TRUE,
                                     collapsible = TRUE,
                                     plotOutput("plot3", height = 600, width = 600)),
                                 
                                 box(title = "Trip Distance", "", solidHeader = TRUE,
                                     collapsible = TRUE,
                                     plotOutput("plot4", height = 600, width = 600)),
                                 
                                 absolutePanel(right = 250, top = 100, width = 200,
                                               selectInput("work","Worktime",vars3)),
                                 
                                 
                                 absolutePanel(right = 50, top = 100, width = 200,
                                               selectInput("passenger_count", "Num of Passengers", vars4)),
                                 
                                 
                                 valueBoxOutput("mean2"),
                                 valueBoxOutput("median2")
                        )),
                 
                 
                 tabBox(width = 12,
                        tabPanel(title="Pickup Location", width = 12, img(src = "pickloc.jpg",height = 350)),
                        tabPanel(title = "Dropoff Location", width = 12, img(src = "droploc.jpg",height = 350)),
                        tabPanel(title = "  ", width = 12))
               ))
      
      ,tabItem(tabName = "SeeYourNeighbourhood"
               ,fluidRow(
                 div(class="outer",
                     
                     tags$head(
                       # Include our custom CSS
                       includeCSS("styles.css"),
                       includeScript("gomap.js")
                     ),
                     
                     leafletOutput("map1", width="100%", height="100%"),
                     
                     # Shiny versions prior to 0.11 should use class="modal" instead.
                     absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                   draggable = F, top = 60, left = "auto", right = 0, bottom = "auto",
                                   width = 160, height = 180,
                                   radioButtons("CF", label = "Layers",
                                                choices = list("Count Number" = "count", "Fare Per Distance" = "FPD","Cash Paying Percentage" = "cash"), 
                                                selected = "count")
                     ),
                     absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                   draggable = TRUE, top = 60, left = 0, right = "auto", bottom = "auto",
                                   width = 330, height = "auto",
                                   
                                   h3("Panel"),
                                   
                                   
                                   selectInput("days", "Days", c("All Day", "Business Day", "Not Business Day"),selected = "All Day"),
                                   
                                   
                                   checkboxInput(inputId = "showhr",
                                                 label = strong("Show hours"),
                                                 value = FALSE),
                                   
                                   conditionalPanel(condition = "input.showhr == false"
                                                    
                                   ),
                                   
                                   
                                   conditionalPanel(condition = "input.showhr == true",
                                                    sliderInput(inputId = "hr_adjust",
                                                                label = "Choose the time of the day:",
                                                                min = 0, max = 23, value = NULL, step = 1)
                                   ),
                                   
                                   
                                   
                                   
                                   checkboxInput("top15count", "Top 5 Count", FALSE),
                                   checkboxInput("top15FPD", "Top 5 FPD", FALSE),
                                   
                                   
                                   checkboxInput(inputId = "showbr",
                                                 label = strong("Show Borough for Top 5 counts/FPD"),
                                                 value = FALSE),
                                   
                                   conditionalPanel(condition = "input.showbr == true",
                                                    selectInput("boroSelect", "Borough for Top 5 counts/FPD", 
                                                                c("Manhattan", "Bronx", "Brooklyn", "Queens", "Staten Island", "All"), 
                                                                selected = "All")
                                   ),
                                   plotOutput("districttimeplot", height = 280)
                     )
                                   
               
        
      )))
      ,tabItem(tabName = "map"
               ,tags$style(HTML(".box.box-solid.box-danger>.box-header {
                            background:#006400;
                                }
                                .box.box-solid.box-danger{
                                border-bottom-color:#006400;
                                border-left-color:#006400;
                                border-right-color:#006400;
                                border-top-color:#006400;
                                }"))
               
               ,fluidRow(
                   tabPanel(
                     title="Interactive Map", 
                     div(class="outer",
                         tags$head(
                           # Include our custom CSS
                           includeCSS("styles.css"),
                           includeScript("gomap.js")
                         )
                         ,leafletOutput("map2", width = "100%", height = "100%")
                         ,absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                        width = 300, height = "auto",
                                        h2("It's your time!"),
                                        selectInput("From", "From", vars),
                                        selectInput("To", "To", vars, selected = "adultpop")
                                        
                                        
                         ))
                   )
                   ,fluidRow(
                     tabPanel(
                       infoBoxOutput("box1"),
                       infoBoxOutput("box2"),
                       infoBoxOutput("box3")))
               
               )
               
               )
               
      ,tabItem(tabName = "Tips",
               fluidRow(
               tabBox(width = 12,
                 tabPanel(title = "Tips by Carriers",
                        setBackgroundImage(src = "http://wallpics4k.com/wp-content/uploads/2014/07/470318.jpg"),
                        h3("Tips Percentage Comparison between Yellow cabs and Green cabs",style="color: black",align="center"),
                        fluidRow( wellPanel(style = "overflow-y:scroll;  height: 500px; opacity: 0.9; background-color: #ffffff;",
                                            column(width = 9, plotOutput("tipPlot")
                                            ),
                                            column(width = 3, checkboxGroupInput("cabs", "Choose a Cab Carrier:",
                                                                                 choices = c("Yellow","Green"),
                                                                                 selected = "Yellow"
                                                                                 
                                            ))))),
              
                 tabPanel("Tips by Days",
                        h3("Tips Percentage Comparison between Weekdays and Weekends",style="color:	black",align="center"),
                        fluidRow( wellPanel(style = "overflow-y:scroll;  height: 500px; opacity: 0.9; background-color: #ffffff;",
                                            column(width = 9, plotOutput("tipPlot_week")
                                            ),
                                            column(width = 3, checkboxGroupInput("week", "Choose a day of week:",
                                                                                 choices = c("Weekday","Weekend"),
                                                                                 selected = "Weekday"
                                                                                 
                                            ))))),
               
                 tabPanel("Tips by Destination",
                        h3("Tips Percentage Comparison between Different Destination",style="color: black",align="center"),
                        fluidRow( wellPanel(style = "overflow-y:scroll;  height: 500px; opacity: 0.9; background-color: #ffffff;",
                                            column(width = 9, plotOutput("tipPlot_place")
                                            ),
                                            column(width = 3, checkboxGroupInput("place", "Choose a Destination:",
                                                                                 choices = c("Standard","Airport","Nassau or Westchester"),
                                                                                 selected = "Standard"                            
                                            ))))),
               
              
                 tabPanel("Tips by Time",
                        h3("Tips Percentage given by different Time of Day",style="color: black",align="center"),
                        fluidRow( wellPanel(style = "overflow-y:scroll;  height: 500px; opacity: 0.9; background-color: #ffffff;",
                                            plotOutput("tipPlot_time")
                        )))))
               
               
               
      ),
      
      tabItem(tabName = "About",
              mainPanel(
                h3("User Manual: ", a("Click Here", href=
                                        "https://github.com/TZstatsADS/Fall2018-Project2-sec2proj2_grp9")),
                br(),
                h3("Data Source"),
                p("Source: ",a("NYC Open Data.",href=
                                 "http://www.nyc.gov/html/tlc/html/about/trip_record_data.shtml")),
                p("Description: ","The yellow taxi trip records were collected and provided to the NYC Taxi and Limousine Commission (TLC) by technology providers authorized under the Taxicab & Livery Passenger Enhancement Programs (TPEP/LPEP)."),
                p("Usage: ","Original dataset was downloaded on 28/09/2018, 
                  which basically contains records from 01-01-2016 to 12-31-2016. 
                  Because of the loading speed concern, this app uses only some sample random records 
                  from the original dataset."),
                br(),
                h3("Author Information"),
                p("Peilu Zhang || pz2233@columbia.edu"),
                p("Lingyi Zhao || lz2570@columbia.edu"),
                p("Nannan Wang || nw2387@columbia.edu"),
                p("Min Jin     || mj2824@columbia.edu"),
                p("Sheng Wang  || sw3224@columbia.edu"),
                
                br(),
                h3("Website"),
                p("Github:", a("https://github.com/TZstatsADS/Fall2018-Project2-sec2proj2_grp9",
                               href="https://github.com/TZstatsADS/Fall2018-Project2-sec2proj2_grp9")
                )
                
                ))
        
      )
              
    )
  )
  
  
  
  