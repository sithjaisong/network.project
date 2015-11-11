#
#
#
#
library(RCurl) # run this package for load the data form the website 

file <- getURL("https://docs.google.com/spreadsheets/d/1zB7gNdI7Nk7SuHuPWcjzaKnjuwkvL6sOVMo0zMfuV-c/pub?gid=558862364&single=true&output=csv") # load data from the google drive

data <- read.csv(text = file) # read data which is formated as the csv
