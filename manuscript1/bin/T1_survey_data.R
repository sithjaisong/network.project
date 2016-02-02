#############################################################
#
# R script for data analysis as presented in Table1: Survey data table
#
# files needed: 
# - data from do.R
# 
#############################################################


data %>% select(Country, Year, Season) %>%
  group_by(Country, Year, Season) %>%
  summarise(Count = n())
  