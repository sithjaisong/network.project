#============SKEP= ========#
#1.load the SKEP1 data
#
#
#
load(file = "~/Google Drive/surveySKEP1/FORM2.RData")

# load libraries 

library(dplyr) # pipe function
library(tidyr) # seperate function


FORM2 <- FORM2 %>%
  separate(filename, c("SKEP", "SURVEY", "Country", "Year", "Season", "Fieldno"), "-") %>%
  transform(Fieldno = gsub(pattern = "\\.xls$", replacement = "", .$Fieldno)) %>%
  select(-c(Fieldno, SKEP, SURVEY))

head(FORM2)
dim(FORM2) # there are 27 variables, but there should be 21 variables (WA, WB, DH, GM, WM, LF, DEF, BPH, WBP, GLH, BLB, LB, BS, NBS, SHB, SHR, NB, FSM, DP)

# reamane the variables of injuries
#' WA = weed above
#' WB = weed below
#' SNL = snal
#' DH = deadheart
#' RT = rat
#' GM = gall midge
#' RB = rice bug
#' WH = white head
#' DP = dirty panicle
#' FSM = false smut
#' NB = neck blast
#' SHB = sheath blight
#' SHR = sheath rot
#' SR = stem rot
#' BPH = brown plant hopper
#' WPH = white backed plant hopper
#' AW = army worm
#' LF = leaffolder
#' WM = whorl maggot
#' BLB = bacterial leaf blight
#' BLS = bacterial leaf streak
#' BS = brown spot
#' LB = leaf blast
#' NBS = narrow brown spot
#' RS = red stripe

names(FORM2) <- c("country", "year", "season", "WA", "WB", "SNL", "DH", "RT",  "GM", "RB", "WH", "DP",  "FSM",  "NB",  "SHB", "SHR",
                  "SR", "BPH", "WPH", "AW",   "LF",  "WM",  "BLB",  "BLS",   "BS",   "LB",  "NBS",    "RS")

#load(file = "~/Google Drive/surveySKEP1/SKEP2database.RData")
# This file is the analysed data.
#file <- getURL("https://docs.google.com/spreadsheets/d/1zB7gNdI7Nk7SuHuPWcjzaKnjuwkvL6sOVMo0zMfuV-c/pub?gid=558862364&single=true&output=csv") # load data from the google drive
#data <- read_csv(file, na = c(" ", "NA", "na", "-")) # read data which is formated as the csv

#=== load the library====#
#library(dplyr)
#library(tidyr)

#head(FORM1)

#load(file = "~/Documents/Github/surveySKEP1/FULL.RData")
#eos