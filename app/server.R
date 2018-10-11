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
library(shinydashboard)
library(dplyr)
library(ggplot2)
#install.packages("mapview")
#install.packages("units")
#install.packages("gdalUtils")
#install.packages("data.table")
#install.packages("rgeos")
library(rgeos)
library(sp)
library(rgdal)
library(htmlwidgets)
library(data.table)
#install.packages("ggthemes")
library(ggthemes)
library("RColorBrewer")
library("stringr")
library("ggplot2")
library("reshape2")
library("geosphere")
library("ggthemes")
library("formattable")
library("base64enc")
library("plotly")
library('shinyWidgets')

setwd(".../data")
register_google("AIzaSyDF7L0gqBYv1GLyxgBlfLtYGN7ugQogSeg")
pick <- read.csv("pick.csv")
drop <- read.csv("drop.csv")
part3 <- read.csv("yong.csv")
yellow_cab<-read.csv("yellow_tripdata_2016-01.csv")

#seeyournh
group1 = "<span style='color: #7f0000; font-size: 11pt'><strong>count</strong></span>"
group2 = "<span style='color: #7f0000; font-size: 11pt'><strong>FPD</strong></span>"
group3 = "<span style='color: #7f0000; font-size: 11pt'><strong>Percentage Cash Paying:</strong></span>"
color = list(color1 = c('#F2D7D5','#D98880', '#CD6155', '#C0392B', '#922B21','#641E16'),
             color2 = c('#e6f5ff','#abdcff', '#70c4ff', '#0087e6', '#005998','#00365d','#1B4F72'),
             color3 = c("#F7FCF5","#74C476", "#005A32"))
bin = list(bin1 = c(0,100,1000,10000,100000,1000000,10000000), bin2 = c(0,1,2,3,4,5,6,7))
label = list(label1 = c("<100","100-1000","1000~10,000","10,000~100,000","100,000~1,000,000","1,000,000~10,000,000"),
             label2 = c("0-1","2-3","3-4","4-5","5-6","6","7+"),
             label3 = c("<0.4","0.4~0.6",">0.6"))
title = list(t1 = "Pick Up Numbers", t2 = "Fare Per Distance",t3  = "PercentagePayingCash")


load('myShape1.RData')
subdat<-spTransform(myShape1, CRS("+init=epsg:4326"))
dynamicdata = fread("pickupDropoff date_hour.csv", header = TRUE, stringsAsFactors=F)
load('count_seperated.RData')
load('FPD_seperated.RData')
rownames(count_result) = subdat@data$NTACode
payper = read.csv("Data_frame_of_summary.csv")


#interactive map
price<-function(From, To){
  From<-as.character(From)
  To<-as.character(To)
  start_location<-round(geocode(From),2)
  drop_location<-round(geocode(To),2)
  yellow_cab_sub<-yellow_cab[,-c(8,9,12,13,14,15,17,18)]
  yellow_cab_sub_new<-round(yellow_cab_sub[,c(6,7,8,9,11)],2)
  yellow_cab_sub_new<-yellow_cab_sub_new[yellow_cab_sub_new$pickup_longitude==start_location$lon,]
  yellow_cab_sub_new<-yellow_cab_sub_new[yellow_cab_sub_new$pickup_latitude==start_location$lat,]
  yellow_cab_sub_new<-yellow_cab_sub_new[yellow_cab_sub_new$dropoff_longitude==drop_location$lon,]
  yellow_cab_sub_new<-yellow_cab_sub_new[yellow_cab_sub_new$dropoff_latitude==drop_location$lat,]
  price<-mean(yellow_cab_sub_new$total_amount)
  return(round(price,3))
}

miles<-function(From, To){
  From<-as.character(From)
  To<-as.character(To)
  dis<-mapdist(From,To,mode = "driving")
  return(round(dis$miles,3))
}

mins<-function(From, To){
  From<-as.character(From)
  To<-as.character(To)
  dis<-mapdist(From,To, mode = "driving")
  return(round(dis$minutes,3))
}

## Tips
# Total Tips Data
tip_percent <- read.csv("tip_combination.csv")
# Tips percentage by Week
tip_week_percent <- read.csv("tip_day_combination.csv")
# Tips percentage by Time
yg_sample <- read.csv("tip_time_combination.csv")
# Tips percentage by Place
tip_place_percent <- read.csv("tip_place_combination.csv")


server <- shinyServer(function(input, output, session) {
  
  set.seed(1234)
  foo <- sample_n(pick, 8e3)
  doo <- sample_n(drop, 8e3)
  
  output$Pickup <- renderLeaflet({
    leaflet(data = foo) %>% setView(lng = -73.97, lat = 40.75, zoom = 13) %>%
      addProviderTiles("Esri.NatGeoWorldMap") %>%
      addCircleMarkers(~ pickup_longitude, ~pickup_latitude, radius = 0.1,
                       color = "darkslateblue", fillOpacity = 0.3)
  })
  
  
  output$Dropoff <-  renderLeaflet({
    leaflet(data = doo) %>% setView(lng = -73.97, lat = 40.75, zoom = 13) %>%
      addProviderTiles("Esri.NatGeoWorldMap") %>%
      addCircleMarkers(~ dropoff_longitude, ~dropoff_latitude, radius = 0.1,
                       color = "#CD5B45", fillOpacity = 0.3)
  }) 
  
  
  
  output$mean <- renderValueBox({
    valueBox(
      round(colMeans(part3 %>% filter(work == as.logical(input$work)) %>%
                       filter(passenger_count == input$passenger_count) %>% select(trip_duration))/60,2),
      "Mean Duration (minutes)", icon = icon("list"),
      color = "purple"
    )
  })
  
  output$median <- renderValueBox({
    valueBox(
      round(apply(part3 %>% filter(work == as.logical(input$work)) %>%
                    filter(passenger_count == input$passenger_count) %>% select(trip_duration),2,median)/60,2)
      , "Median Duration (minutes)", icon = icon("list"),
      color = "yellow"
    )
  })
  
  output$mean2 <- renderValueBox({
    valueBox(
      round(colMeans(part3 %>% filter(work == as.logical(input$work)) %>%
                       filter(passenger_count == input$passenger_count) %>% select(distance))/1609.344,3)
      , "Mean Distance (miles)", icon = icon("list"),
      color = "purple"
    )
  })
  
  output$median2 <- renderValueBox({
    valueBox(
      round(apply(part3 %>% filter(work == as.logical(input$work)) %>%
                    filter(passenger_count == input$passenger_count) %>% select(distance),2,median)/1609.344,3)
      , "Median Distance (miles)", icon = icon("list"),
      color = "yellow"
    )
  })
  
  output$plot3 <- renderPlot(
    part3 %>% filter(trip_duration < 5000) %>% filter(work == as.logical(input$work)) %>%
      filter(passenger_count == input$passenger_count) %>%
      ggplot(aes(trip_duration)) +
      geom_histogram(fill = "darkslateblue",bins = 20,aes(y=..density..))+
      geom_density(alpha = 0.2, fill = "orange") +
      theme_set(theme_tufte())
  )
  
  output$plot4 <- renderPlot(
    part3 %>% filter(distance < 20000) %>% filter(work == as.logical(input$work)) %>%
      filter(passenger_count == input$passenger_count) %>%
      ggplot(aes(distance)) +
      geom_histogram(fill = "orange",bins = 20,aes(y=..density..))+
      geom_density(alpha = 0.2, fill = "darkslateblue") +
      theme_set(theme_tufte())
  )
  
  output$map1 <- renderLeaflet({
  
    if (input$days == "All day"){
      count_intermediate = count_result %>% apply(c(1,2), sum)
      FPD_intermediate = FPD_result %>% apply(c(1,2), mean, na.rm = T)
    }else{
      count_intermediate = count_result[ , , (input$days == "Not Business Day") + 1]
      FPD_intermediate = FPD_result[ , , (input$days == "Not Business Day") + 1]
    }
    if (!input$showhr){
      subdat@data$count = count_intermediate %>% apply(1, sum)
      subdat@data$FPD = FPD_intermediate %>% apply(1, mean, na.rm = T)
    }else{
      subdat@data$count = count_intermediate[, input$hr_adjust+1]
      subdat@data$FPD = FPD_intermediate[, input$hr_adjust+1]
    }
    
    ######
    
    blocks_coord = data.frame(center_lng = rep(NA, 195), center_lat = rep(NA, 195)) # Combine borough coord for future marking purpose
    for (i in 1:195){ blocks_coord[i,] = subdat@polygons[[i]]@labpt }    # One more update: add long/lat permanently into myShape@data as
    
    subdat_top5_intermediate = cbind(subdat@data, blocks_coord)
    
    if (input$boroSelect == "All"){ # filter borough
      subdat_top5 = subdat_top5_intermediate
    } else {
      subdat_top5 = subdat_top5_intermediate[subdat@data$BoroName == input$boroSelect, ]
    }
    if (!input$showbr){
      subdat_top5 = subdat_top5_intermediate
    }
    
    subdat_top5 = subdat_top5 %>% 
      subset(select = c("NTACode", "NTAName", "count", "FPD", "center_lng", "center_lat"))
    
    top5count = subdat_top5[order(subdat_top5$count, decreasing = T),
                            c("NTAName", "count", "center_lng", "center_lat")] %>% head(5) # fetch top-5-rows with the most counts/FPD
    top5FPD = subdat_top5[order(subdat_top5$FPD, decreasing = T),
                          c("NTAName", "FPD", "center_lng", "center_lat")] %>% head(5)

    
    subdat_data=subdat@data[,c("NTACode", "NTAName", "count", "FPD")]
    subdat<-SpatialPolygonsDataFrame(subdat, data=subdat_data)
    
    # print leaflet
    pal = colorBin(color[[1]], bins = bin[[1]])
    pal_FPD = colorBin(color[[2]], bins = bin[[2]])
    pal2 = colorBin(c("#882E72", "#B178A6", "#D6C1DE", "#1965B0", "#5289C7", "#7BAFDE", "#4EB265", "#90C987", "#CAE0AB", "#F7EE55", "#F6C141", "#F1932D", "#E8601C", "#DC050C"), 1:10)
    pal3 = colorBin(c("#005A32", "#74C476", "#F7FCF5"), 0:0.125:1)
    
    
    popup1 = paste0('<strong>Neighborhood: </strong><br>', subdat_data$NTAName, 
                    '<br><strong>Count of pick-ups: </strong><br>', subdat_data$count)
    popup2 = paste0('<strong>Neighborhood: </strong><br>', subdat_data$NTAName, 
                    '<br><strong>Fare Per Distance: </strong><br>', subdat_data$FPD)
    popup3 = paste0('<strong>Neighborhood: </strong><br>', subdat_data$NTAName)
    popup4 = paste0('<strong>Neighborhood: </strong><br>', subdat_data$NTAName, 
                    '<br><strong>Percentage Paying Cash: </strong><br>', payper$PercentagePaying)
    
    
    greenLeafIcon <- makeIcon(
      iconUrl = "https://cdn1.iconfinder.com/data/icons/weather-19/32/fire-512.png",
      iconWidth = 38, iconHeight = 38,
      iconAnchorX = 0, iconAnchorY = 0
    )
    redLeafIcon <- makeIcon(
      iconUrl = "https://maxcdn.icons8.com/Share/icon/Finance//usd1600.png",
      iconWidth = 38, iconHeight = 38,
      iconAnchorX = 0, iconAnchorY = 0
    )
    
  
    pic1<-leaflet(subdat) %>%
      setView(lat=40.7128, lng=-74.0059, zoom=10) %>%
      addProviderTiles('CartoDB.Positron') 
    
    
    if (input$CF == "count"){
      pic1<-pic1 %>%
        addPolygons(fillColor = ~pal(count), color = 'grey', weight = 1, 
                    popup = popup1, fillOpacity = .6, group = group1) %>%
        addLegend(position = "bottomright",
                  colors = color[[1]],
                  labels = label[[1]],
                  opacity = 0.6,
                  title = title[[1]])
    }
    else if (input$CF == "FPD"){
      pic1<-pic1 %>%
        addPolygons(fillColor = ~pal_FPD(FPD), color = 'grey', weight = 1, 
                    popup = popup2, fillOpacity = .6, group = group2) %>%
        addLegend(position = 'bottomright',
                  colors = color[[2]],
                  labels = label[[2]], ## legend labels (only min and max)
                  opacity = 0.6,      ##transparency again
                  title = title[[2]])
    }
    else if (input$CF == "cash"){
      pic1<-pic1 %>%
        addPolygons(fillColor =  ~pal3(payper$PercentagePayingCash), color = 'grey', weight = 1, 
                    popup = popup4, fillOpacity = .6, group = group3) %>%
        addLegend(position = 'bottomright',
                  colors = color[[3]],
                  labels = label[[3]], ## legend labels (only min and max)
                  opacity = 0.6,      ##transparency again
                  title = title[[3]])
    }
    
    
    ###TOP5    
    if (input$top15count == TRUE){
      pic1<-pic1 %>%
        addMarkers(~top5count$center_lng, ~top5count$center_lat, icon = greenLeafIcon)
    }
    
    else{
      pic1
    }
    
    if (input$top15FPD == TRUE){
      pic1<-pic1 %>%
        addMarkers(~top5FPD$center_lng, ~top5FPD$center_lat, icon = redLeafIcon)
    }
    
    else{
      pic1
    }
   
  })
  
  observe({
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    dattest = data.frame(Longitude = event$lng, Latitude = event$lat)
    coordinates(dattest) <- ~ Longitude + Latitude
    proj4string(dattest) <- CRS("+proj=longlat")
    dattest <- spTransform(dattest, proj4string(myShape1))
    rtest = over(dattest, myShape1)
    
    output$districttimeplot <- renderPlot({
      if (nrow(rtest) == 0) {
        return(NULL)
      }
      if (input$days == "All Day"){
        count_resultNTA = count_result[which(rownames(count_result) == rtest$NTACode),,]
        count_resultNTA = apply(count_resultNTA, 1, sum)
        index <- c(0:23)
        dfcount_resultNTA <- data.frame(index, count_resultNTA)
        ggplot(data=dfcount_resultNTA, aes(x=index, y=count_resultNTA)) + geom_bar(stat="identity") + 
          labs(x = "hour") + labs(y = "count per hour")+ggtitle("pick up count flow trend")+geom_smooth(formula = y~x)
      }
      else if (input$days == "Business Day"){
        count_resultNTA = count_result[which(rownames(count_result) == rtest$NTACode),,1]
        index <- c(0:23)
        dfcount_resultNTA <- data.frame(index, count_resultNTA)
        ggplot(data=dfcount_resultNTA, aes(x=index, y=count_resultNTA)) + geom_bar(stat="identity") + 
          labs(x = "hour") + labs(y = "count per hour")+ggtitle("pick up count flow trend")+geom_smooth(formula = y~x)
      }
      else if (input$days == "Not Business Day") {
        count_resultNTA = count_result[which(rownames(count_result) == rtest$NTACode),,2]
        index <- c(0:23)
        dfcount_resultNTA <- data.frame(index, count_resultNTA)
        ggplot(data=dfcount_resultNTA, aes(x=index, y=count_resultNTA)) + geom_bar(stat="identity") + 
          labs(x = "hour") + labs(y = "count per hour")+ggtitle("pick up count flow trend")+geom_smooth(formula = y~x)
      }
      
    })
  })
  
  output$map2 <- renderLeaflet({
    
    register_google("AIzaSyDF7L0gqBYv1GLyxgBlfLtYGN7ugQogSeg")
    route_all<-route(from = input$From, to=input$To)
    
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>% 
      setView(lng = -73.98513, lat = 40.7589, zoom = 12)%>%
      addPolylines(route_all$startLon, route_all$startLat)%>%
      addMarkers(lng=route_all$startLon[c(1,nrow(route_all))], lat=route_all$startLat[c(1, nrow(route_all))], 
                 popup = c(input$From, input$To))%>%
      setView(lng = -73.97, lat = 40.75, zoom = 13)
  })
  
  #dashboard
  
    #distance
    output$box1 <- renderInfoBox({
      infoBox("Est Distance:(miles) ", miles(input$From, input$To) , icon = icon("arrows-h"), fill = TRUE)
    })
    
    #est price
    output$box2 <- renderInfoBox({
      infoBox("Est Price:(dollars) ", price(input$From, input$To), icon = icon("dollar"), fill = TRUE)
    })
    
    #time
    output$box3 <- renderInfoBox({
      infoBox("Est Time:(minutes) ", mins(input$From, input$To) , icon = icon("car"), fill = TRUE)
    })
    
    ## For total percentage of tips given by customers (only customers paied by credit card)
    output$tipPlot <- renderPlot({
      if(length(input$cabs == 0)){
        plot(0,xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='')
      }
      if(length(input$cabs > 0)){
        # generate an rnorm distribution and plot it
        carr_name <- input$cabs
        carr_row <- tip_percent$CABS %in% carr_name
        carr_data <- tip_percent[carr_row, c(3,5,7,9,11,13,15,17,19)]
        colnames(carr_data)[2:9] <- c("0%", "0-5%", "5-10%", "10-15%", "15-20%","20-25%","25-30%","30% +")
        data_melt <- reshape2::melt(carr_data, id = "CABS")
        ggplot(data=data_melt, aes(x = variable, y= value , fill = CABS)) +
          geom_bar(stat="identity", position=position_dodge()) + 
          theme_wsj()+
          guides(fill=guide_legend(title=NULL))+
          theme(axis.title = element_blank(),legend.position = 'top') + 
          labs(x = "Tips Percentage",y = "People Percentage")
      }
    })
    
    ## For tips given by weekdays and weekends
    output$tipPlot_week <- renderPlot({
      if(length(input$week == 0)){
        plot(0,xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='')
      }
      if(length(input$week > 0)){
        # generate an rnorm distribution and plot it
        carr_name <- input$week
        carr_row <- tip_week_percent$Days %in% carr_name
        carr_data <- tip_week_percent[carr_row, c(3,4,5,6,7,8,9,10,11)]
        colnames(carr_data)[2:9] <- c("0%", "0-5%", "5-10%", "10-15%", "15-20%","20-25%","25-30%","30% +")
        data_melt <- reshape2::melt(carr_data, id = "Days")
        ggplot(data=data_melt, aes(x = variable, y= value , fill = Days)) +
          geom_bar(stat="identity", position=position_dodge()) + 
          theme_wsj()+
          guides(fill=guide_legend(title=NULL))+
          theme(axis.title = element_blank(),legend.position = 'top')  
      }
    })  
    
    ## For tips given by times of day
    output$tipPlot_place <- renderPlot({
      if(length(input$place == 0)){
        plot(0,xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='')
      }
      if(length(input$place > 0)){
        # generate an rnorm distribution and plot it
        carr_name <- input$place
        carr_row <- tip_place_percent$Place %in% carr_name
        carr_data <- tip_place_percent[carr_row, c(3,4,5,6,7,8,9,10,11)]
        colnames(carr_data)[2:9] <- c("0%", "0-5%", "5-10%", "10-15%", "15-20%","20-25%","25-30%","30% +")
        data_melt <- reshape2::melt(carr_data, id = "Place")
        ggplot(data=data_melt, aes(x = variable, y= value , fill = Place)) +
          geom_bar(stat="identity", position=position_dodge()) + 
          theme_wsj()+
          guides(fill=guide_legend(title=NULL))+
          theme(axis.title = element_blank(),legend.position = 'top')  
      }
    })  
    
    # It seems that during lunch time the InterQuartile range is smaller,
    # making people more consistent in the amount of tips they leave.
    output$tipPlot_time <- renderPlot({
      
      plot(as.factor(yg_sample$hour), yg_sample$tip_percent, xlab = "hour of day", ylab="Tips, %")
      
    })
  })
  
  
  



