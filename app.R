#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(tidyverse)
library(lubridate)

crime <- read.csv('Crimes_2020.csv')
summary(crime)

#Removing Lat-Long outliers
crime <- crime[-which(crime$Longitude < -88 | crime$Longitude > -86),]

#Removing rows with NA Lat-Long
crime <- crime[-which(is.na(crime$Latitude)),]

#Converting char date to POSIXct DateTime format
crime$Date <- as.POSIXct(crime$Date,format = "%m/%d/%Y %H:%M")

#Removing Wards that has 7 - NAs
crime <- crime[-which(is.na(crime$Ward)),]

#Removing Date NAs
crime <- crime[-which(is.na(crime$Date)),]

#Creating separate date and month columns from datetime (existing Date) column

crime$month <- lubridate::month(crime$Date)
crime$hour <- lubridate::hour(crime$Date)
crime$Day <- lubridate::day(crime$Date)
crime$Day_Week <- wday(crime$Date, label=TRUE)

#There are two occurrences of Criminal sexual assault - Crim sexual assualt. Replacing one of them. 
crime$Primary.Type <- gsub("CRIM SEXUAL ASSAULT","CRIMINAL SEXUAL ASSAULT",crime$Primary.Type)
crime$Primary.Type <- as.factor(crime$Primary.Type)


ui <- dashboardPage(
    dashboardHeader(title = "Dashboard"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("CrimeMonths",tabName = "bar", icon = icon("chart-bar")),
            menuItem("CrimeRegions",tabName = "map", icon = icon("map-pin")),
            menuItem("CrimeHeatmap", tabName = "heat", icon = icon("chess-board")),
            menuItem("CrimeLine", tabName = "loc", icon = icon("chart-line"))
        )),
    dashboardBody(
        tabItems(
            tabItem("bar",
                    fluidPage(

                      titlePanel("Frequency of Crime by Month and Crime Type"),
                        plotOutput('barplot')
                    )),
            tabItem("map",
                    fluidPage(
                      titlePanel("Location of Crimes by Date. Hover for details"),
                      
                        sidebarLayout(
                            sidebarPanel(
                            sliderInput("month","Choose month Jan(1) - Sept(9)", 1,9,value = 2, step =1),
                            sliderInput("day","Choose date of the month", 1,31,value = 2, step = 1)
                        ),
                        mainPanel(
                        leafletOutput('mapplot')
                    ))
                    )),
            tabItem("heat",
                    fluidPage(
                      titlePanel("Heat map of Crime type and Hour of the day"),
                      
                        plotOutput('heatmap')
                    )),
            tabItem("loc",
                    fluidPage(
                      titlePanel("Distribution of Crime types  by Day of Week and Month "),
                      
                        sidebarLayout(
                            sidebarPanel(
                                selectInput("Crime_Type", "Choose one Crime Type",crime$Primary.Type,
                                            selected =  "ASSAULT",multiple = FALSE)), 
                        mainPanel(
                            plotOutput('locbar')
                        ))              
                    ))
            ))) 

server <- function(input,output){
    
#Bar graphs of crime type by month
    
    #including all month - type combination values in the dataset
    barplot_data <- as.data.frame(crime %>% group_by(Primary.Type,month) %>% summarise(count = n()))
    barplot_data$month <- as.factor(barplot_data$month)
    output$barplot <- renderPlot(barplot_data %>% 
                    ggplot(mapping = aes(x=month,y=count, fill=month)) + 
                       geom_col()  + facet_wrap(~ Primary.Type, scales = "free_y") + labs(x="Month",y="Total crimes"))
 

#Map of crimes by date
    data.map <- reactive({
        crime %>% dplyr::filter(month == input$month & Day == input$day) %>%
        mutate(lab = paste("CrimeType", Primary.Type,",","Location",Location.Description, ",","Block",Block))
                             })
    output$mapplot <- renderLeaflet(
        leaflet(data.map()) %>% addTiles() %>% addCircleMarkers(lng = ~Longitude, lat = ~Latitude, radius = 2,
                                                           color = "Purple", label = ~lab))
    
    
#Heat-map of average crimes across crime type and hour  
        
    #including all hour - crime type combination values in the dataset. 
    summary <- as.data.frame(crime %>% 
            group_by(Primary.Type,month, hour) %>% summarise(count = n()) %>% 
            group_by(Primary.Type,hour) %>% summarise(avg = mean(count)))
        
        output$heatmap <- renderPlot(summary %>%
                    ggplot(mapping = aes(x=hour,y=Primary.Type, fill = avg)) + 
                        scale_fill_gradient(name = "Average crime rate", low = "Pink", high = "Navy", na.value = NA) +
                        geom_tile() + labs(x = "Hour of the Day", y = "Crime Type"))

        
#Crimes occurring on day of the week 
        
 data.week <- reactive({ as.data.frame(crime %>% dplyr::filter(Primary.Type == input$Crime_Type)  %>% 
     group_by(month,Day_Week) %>%
     summarise(count = n()))
   })
  
  output$locbar <- renderPlot(data.week() %>%
                       ggplot(mapping = aes(x=month, y=count, colour = Day_Week)) + 
                         geom_line() +
                         labs(x = "Month of crime",y= "Total number of crimes"))
     
}    

shinyApp(ui = ui,server = server)