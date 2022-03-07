library(ggplot2)
library(gganimate)
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

temp_date <- seq(as.Date("2013-01-01"), length = 84, by = "months")
temp_county <- unique(str_to_title(mapdata$subregion))

date <- sort(rep(seq(as.Date("2013-01-01"), length = 84, by = "months"),58))
county <- rep(temp_county,84)
count <- numeric(84*58)
total_count <- numeric(84*58)

index <- 1
for (i in 1:length(temp_date)-1) {
  month_data <- wildfires[wildfires$Started > temp_date[i] & wildfires$Started < temp_date[i+1],]
  for(j in 1:length(temp_county)) {
    count[index] <- length(month_data[month_data$Counties == temp_county[j],]$Counties)
    index <- index + 1
  }
}

for (i in 1:length(count)) {
  if (i<=58) {
    total_count[i] <- count[i]
  } else {
    total_count[i] <- count[i] + total_count[i-58]
  }
}

monthly_values <- data.frame(date, county, total_count)
monthly_values <- monthly_values[order(monthly_values$date, -monthly_values$total_count),]

rank <- c()
i <- 1
while (i <= length(monthly_values$total_count)) {
  tempDate <- monthly_values$date[i]
  tempRank <- 1
  while (i <= length(monthly_values$total_count) && 
         monthly_values$date[i] == tempDate) {
    rank[i] <- tempRank
    i <- i+1
    tempRank <- tempRank + 1
  }
}

monthly_values['rank'] <- rank
monthly_values <- monthly_values[which(rank<=10),]
View(monthly_values)

graph2 <- monthly_values %>% 
  ggplot(aes(rank, group = county, fill = as.factor(county),color = as.factor(county)))+
  geom_tile(aes(y = total_count/2, height = total_count, width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(county, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y= total_count,label = as.character(total_count), hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse()+
  guides(color = "none", fill = "none") +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm"))

graph2.animation <- graph2 +
  transition_states(date, transition_length = 8, state_length = 2, wrap = FALSE)+
  labs(title = "Number of Major Wildfires per County",
    subtitle = "from 2013-01-01 to {closest_state}")+
  view_follow(fixed_x = TRUE)
graph2.animation

animate(graph2.animation, height = 500, width = 800, fps = 30, duration = 30,
        end_pause = 120, res = 100, renderer = gifski_renderer("monthy_count_animation.gif"))
