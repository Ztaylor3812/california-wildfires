library(ggplot2)
library(tidyverse)
library(usmap)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(rvest)

# --------------
# Preparing Data
# --------------

# Retrieving Data
wildfires <- read.csv("C:/Users/Taylor/Downloads/California_Fire_Incidents.csv")

# Clean Dates
wildfires$Extinguished <- as.Date(wildfires$Extinguished)
wildfires$Started <- as.Date(wildfires$Started)
for (i in 1:length(wildfires$Extinguished)){
  if (is.na(wildfires$Extinguished[i]) || wildfires$Extinguished[i] < as.Date("2013-01-01")){
    wildfires$Extinguished[i]<-NA
  }
  if (is.na(wildfires$Started[i]) || wildfires$Started[i] < as.Date("2013-01-01")){
    wildfires$Started[i]<-NA
  }
}

# Remove Duplicates
wildfires <- wildfires[!duplicated(wildfires$UniqueId),]

# Creating Data frame that includes map data for each county and adding
# number of wildfires and total acres burned for each county
mapdata <- map_data("county", "california")
subregion <- unique(mapdata$subregion)
count <- numeric(length(subregion))
total_acres <- numeric(length(subregion))

# Scrape wikipedia for coordinates of centers of California counties
url <- "https://en.wikipedia.org/wiki/User:Michael_J/County_table"
sample <- read_html(url)                  
tables <- html_table(sample, fill = TRUE)[[1]]
tables <- tables[tables$State=="CA",]

# Clean coordinates
center_long <- (gsub("°", "",tables$Longitude))
center_long <- as.numeric(gsub("–","-", center_long))
center_lat<- (gsub("°", "",tables$Latitude))
center_lat <- as.numeric(gsub("–","-", center_lat))

for (i in 1:length(subregion)) {
  for (j in 1:length(wildfires$Counties)){
    if (str_to_title(subregion[i])==wildfires$Counties[j]) {
      count[i] <- count[i] + 1;
      total_acres[i] <- sum(c(total_acres[i],wildfires$AcresBurned[j]), na.rm = TRUE)
    }
  }
}


# Data frame including county name, fire count, acres burned, and center location
counties <- data.frame(subregion, count, total_acres, center_long, center_lat)

# Data frame including counties and border coordinates
mapdata <- left_join(mapdata, counties, by = "subregion")


# -------------
# Creating Maps
# -------------

# Map Template
mapTemp <- ggplot() + theme(axis.text.x = element_blank(),
                            axis.text.y = element_blank(),
                            axis.ticks = element_blank(),
                            axis.title.x = element_blank(),
                            axis.title.y = element_blank(),
                            rect = element_blank(),
                            plot.title = element_text(size = 16))


# ---------------
# Shiny Dashboard
# ---------------

# UI
ui <- dashboardPage(skin = "green",
  dashboardHeader(title = "California Wildfires"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Description", tabName = "Description", icon = icon("file-text")),
      menuItem("Maps", tabName = "Maps", icon = icon("map-marker-alt"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Description",
        box(p("The dataset used is a set containing a list of wildfires that 
              have occured from 2013 to 2020. Each wildfire reported includes
              the latitude, longitude, date, and acres burned by the incident."),
            p("The data was from the California Fire website, 
              https://www.fire.ca.gov/, and was collected and sorted by Kaggle
              user, ananthu017."),
            p("The dashboard I have created visualizes this data through the use
              of maps and graphs to provide insight into how location, season,
              and year all factor into the frequency and severity of wildfires."),
            width = 12),
        valueBox(1636, "Wildfires Included", icon = icon("fire"), 
                 width = 4, color = "red"),
        valueBox(7, "Years of Data", icon = icon("clock"), 
                 width = 4, color = "navy"),
        valueBox("7.5 Million", "Total Acres Burned", icon = icon("tree"), 
                 width = 4, color = "green")
        ),
      tabItem(tabName = "Maps",
              box(plotOutput("map", click = "map_click"), width = 7),
              box(selectInput("features", "Choose a Variable to Display: ", 
                              c("Wildfire Locations","Wildfire Count","Acres Burned")),
                  h4(textOutput("label")), 
                  verbatimTextOutput("click_info"),
                  setSliderColor("Green", 1),
                  sliderInput("date_range","Dates:",
                              min = as.Date("2013-01-01","%Y-%m-%d"),
                              max = as.Date("2019-12-01","%Y-%m-%d"),
                              value=c(as.Date("2013-01-01"),as.Date("2019-12-01")),
                              timeFormat="%b %Y"),
                  width = 5),
              )
    )
  )
)

# Server
server <- function(input, output) {
  global <- reactiveValues(counties2 = counties)
  output$map <- renderPlot({
    wildfires2 <- wildfires[wildfires$Started > input$date_range[1] & 
                              wildfires$Started < input$date_range[2],]
    wildfires2 <- wildfires2[!is.na(wildfires2$Counties),]
    if (input$features=="Wildfire Locations") {
      return(
        mapTemp + geom_polygon(data = mapdata, aes(x=long, y = lat, group=group), 
                               fill = "white", color = "black")+
          geom_point(data = wildfires2, 
             aes(x = Longitude, y = Latitude, size = AcresBurned), 
             color = "red", alpha = 0.5) +
          xlim(min(mapdata$long), max(mapdata$long))+
          ylim(min(mapdata$lat), max(mapdata$lat)) +
          scale_size_continuous(name = "Acres Burned", 
                        labels = c("0", "200,000", "400,000"), 
                        breaks = c(0, 200000, 400000)) +
          labs(title = paste("Map of Major Wildfires in California from \n",
                             format(as.Date(input$date_range[1]), "%b %Y"), " to ",
                             format(as.Date(input$date_range[2]), "%b %Y"), sep = ""))
      )
    } else {
        count2 <- numeric(length(subregion))
        total_acres2 <- numeric(length(subregion))
        for (i in 1:length(subregion)) {
          for (j in 1:length(wildfires2$Counties)){
            if (str_to_title(subregion[i]) == wildfires2$Counties[j]) {
              count2[i] <- count2[i] + 1;
              total_acres2[i] <- sum(c(total_acres2[i],wildfires2$AcresBurned[j]), na.rm = TRUE)
            }
          }
        }
        global$counties2 <- data.frame(subregion, count2, total_acres2)
        mapdata2 <- left_join(mapdata, global$counties2, by = "subregion")
        if (input$features=="Wildfire Count") {
          return(
            mapTemp +
              geom_polygon(data = mapdata2, aes(x=long, y = lat, group=group, fill = count2),
                           color = "black")+
              scale_fill_gradientn(name = "Number of Wildfires",
                                   colors = c("white", "yellow","orange","red")) +
              labs(title = paste("Total Number of Major Wildfires per County from \n",
                                 format(as.Date(input$date_range[1]), "%b %Y"), " to ",
                                 format(as.Date(input$date_range[2]), "%b %Y"), sep = ""))
          )
        } else {
          return(
            mapTemp +
              geom_polygon(data = mapdata2,
                           aes(x=long, y = lat, group=group, fill = total_acres2),
                           color = "black")+
              scale_fill_gradientn(name = "Total Acres Burned",
                                   colors = c("white", "yellow","orange","red")) +
              labs(title = paste("Total Number of Acres Burned per County from \n",
                                 format(as.Date(input$date_range[1]), "%b %Y"), " to ",
                                 format(as.Date(input$date_range[2]), "%b %Y"), sep = ""))
          )
        }
      }
  })
  
  output$label <- reactive({
    if (input$features=="Wildfire Locations") {
      return("Fire Information")
    } else {
      return("County Information")
    }
  })
  
  output$click_info <- reactive({
    if (input$features == "Wildfire Locations") {
      fire_id <- nearPoints(wildfires, input$map_click, addDist = TRUE,xvar = "Longitude", 
                         yvar = "Latitude", maxpoints = 1)$UniqueId
      if (length(fire_id) > 0) {
        index <- which(wildfires$UniqueId == fire_id)
        return(paste("Fire Name: ", wildfires$Name[index],
                     "\nAcres Burned: ", wildfires$AcresBurned[index], 
                     "\nDate Started: ", wildfires$Started[index], sep = ""))
      } else {
        return("Please Select a Fire from the Map\n \n ")
      }
    } else {
      county_name <- nearPoints(counties, input$map_click, addDist = TRUE, threshold = 30,
                                xvar = "center_long", yvar = "center_lat", maxpoints = 1,)$subregion
      if (length(county_name) > 0) {
        index <- which(counties$subregion == county_name)
        return(paste("County: ", county_name,
                     "\nTotal Acres Burned: ", global$counties2$total_acres[index],
                     "\nTotal Number of Wildfires: ", global$counties2$count[index], sep = ""))
      } else {
        return("Please Select a County from the Map\n \n ")
      }
    }
  })
  
}

# Run App
shinyApp(ui, server)


  