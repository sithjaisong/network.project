#==============
library(dplyr) # arrange data structure
library(reshape2)
library(lubridate)
#================
library(ggplot2)  # plotting
library(gridExtra)
library(scales)
library(cowplot)
#===============
library(bioDist) # Co-ocurrance analysis
library(vegan) # Co-ocurrance analysis
#==============
library(igraph) # Network analysis package

#==============

library(RCurl) # run this package for load the data form the website 

file <- getURL("https://docs.google.com/spreadsheets/d/1zB7gNdI7Nk7SuHuPWcjzaKnjuwkvL6sOVMo0zMfuV-c/pub?gid=558862364&single=true&output=csv") # load data from the google drive

data <- read.csv(text = file) # read data which is formated as the csv

mark.data <- data[, names(data) %in% c("Fno", "Country", "Lat", "Long")]

#ind <- mark.data[ mark.data$Country == "IND", ]

#plot(ind$Long, ind$Lat)

mark.data <- mark.data[!(mark.data$Country == "IND" & mark.data$Long > 78 ), ]
mark.data <- mark.data[!(mark.data$Country == "IND" & mark.data$Long < 76.5), ]
mark.data <- mark.data[!(mark.data$Country == "THA" & mark.data$Long > 100.2), ]
mark.data <- mark.data[!(mark.data$Country == "THA" & mark.data$Lat > 16.5), ]
mark.data <- mark.data[!(mark.data$Country == "VNM" & mark.data$Long > 108), ]

mapgoogle <- get_map(location = c(lon = mean(mark.data$Long), lat = mean(mark.data$Lat)), 
                      zoom = 4, maptype = "satellite", scale = 2)

ggmap(mapgoogle) + geom_point(data = mark.data, aes(x = Long, y = Lat, 
                                                     fill = "red", alpha = 0.8), size = 3, shape = 21) + guides(fill = FALSE,  alpha = FALSE, size = FALSE)

fjsdhfjkdsh