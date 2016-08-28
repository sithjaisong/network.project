##############################################################################
# title         : do.R;
# purpose       : extract the injuries and diseases from the form2visit1 and form2visit2 in excel file
# producer      : prepared by S. jaiosong;
# last update   : in Los Banos, IRRI, 21 January 2016;
# inputs        : crop health survey form 2 (excel);
# outputs       : injuries and disease data of SKEP Phase I from all locations
##############################################################################


# ==== Load libraries
library(plyr)
library(dplyr)
library(tidyr)
library(reshape)
library(reshape2)

# ===== Load function
source("~/Documents/Github/network.project/chapter3/R/functions/injury_analysis.R")
source("~/Documents/Github/network.project/chapter3/R/functions/weed_analysis.R")
source("~/Documents/Github/network.project/chapter3/R/functions/sweep_analysis.R")

#=== Load the FORM2 data
load(file = "~/Google Drive/surveySKEP1/FULLdatabase1.RData")

# names(FORM2)
# corract the type of varibale
FORM2[is.na(FORM2)] <- 0

data <- FORM2 %>% transform(index = as.factor(as.character(index)), Country = as.character(Country), 
                                Year = as.character(Year), Season = as.character(Season), Fieldno = as.character(Fieldno), 
                                visit = as.numeric(visit), Q = as.numeric(Q), DVS = as.numeric(DVS), 
                                Nt = as.numeric(Nt), Np = as.numeric(Np), Nl = as.numeric(Nl), SNL = as.numeric(SNL), 
                                DH = as.numeric(DH), WH = as.numeric(WH), GM = as.numeric(GM), RT = as.numeric(RT), 
                                WM = as.numeric(WM), LF = as.numeric(LF), def = as.numeric(def), BPH = as.numeric(BPH), 
                                WPH = as.numeric(WPH), AW = as.numeric(AW), RB = as.numeric(RB), BLB = as.numeric(BLB), 
                                LB = as.numeric(LB), BS = as.numeric(BS), BLS = as.numeric(BLS), NBS = as.numeric(NBS), 
                                RS = as.numeric(RS), SHB = as.numeric(SHB), SHR = as.numeric(SHR), 
                                SR = as.numeric(SR), FS = as.numeric(FS), NB = as.numeric(NB), DP = as.numeric(DP), 
                                Area = as.numeric(Area), weed.above = as.numeric(weed.above), 
                                weed.below = as.numeric(weed.below), S.rank = as.numeric(S.rank), BD.rank = as.numeric(BD.rank), 
                                G.rank = as.numeric(G.rank), SD.rank = as.numeric(SD.rank), Sweep.no = as.numeric(Sweep.no), 
                                GLH.sweep = as.numeric(GLH.sweep), BPH.sweep = as.numeric(BPH.sweep), 
                                WPH.sweep = as.numeric(WPH.sweep), RC.sweep = as.numeric(RC.sweep)
                                )


# the data NA is 0 because the the person sho input the data keep blank when data actualy are 0. And they means really 0, not NA, which is mean did not observe.

data[is.na(data)] <- 0 

analyzed.injury <- injury_analysis(data) # if it error, you need to reload dplyr package


#==== Select: Weed infastration

weed <- data %>% dplyr::select(index, Country, Year, Season, Fieldno, visit, DVS, Area, weed.above, weed.below)

weed[is.na(weed)] <- 0

analyzed.weed <- weed_analysis(weed)

#==== Select sweep insect

GLH <- data %>% dplyr::select(index, Country, Year, Season, Fieldno, visit, DVS, Sweep.no, GLH.sweep)

GLH[is.na(GLH)] <- 0

analyzed.GLH <- sweep_anlaysis(GLH)

# == combine the dataset

injury.profiles <- left_join(analyzed.injury, analyzed.GLH)

injury.profiles$Fieldno <- NULL

names(injury.profiles) <- c("index", "country", "year", "season", "SNL", "DH", "RT", "GM", "RB", "WH", "DP", "FS", "NB", "SHB", "SHR", "SR", 
                            "BPH", "WPH", "AW", "LF", "WM", "BLB", "BLS", "BS", "LB", "NBS", "RS", "GLH")

injury.profiles <- as.data.frame(injury.profiles)
#save(injury.profiles, analyzed.GLH, analyzed.weed, analyzed.injury, file = "~/Google Drive/surveySKEP1/injury_data.RData")


colnames(FORM1)[names(FORM1) %in% c("Country" , "Season")] <- c("country", "season")


# yield with injures
yield.data <- FORM1 %>% dplyr::select(index, country, season, yld.area1, yld.area2, yld.area3) %>% 
  gather(area, yield, yld.area1:yld.area3) %>% 
  mutate(area = gsub("yld.area", "", area)) %>% 
  group_by(index, country, season) %>%
  summarise(mean.yield = mean(yield, rm.na = FALSE))

IPwithyield <- left_join(injury.profiles, yield.data)

IPwithyield <- IPwithyield[!(is.na(IPwithyield$mean.yield) == TRUE),]


IPwithyield$yield.level <- ifelse(IPwithyield$mean.yield < 400, "low",
                                  ifelse(IPwithyield$mean.yield >= 400 & IPwithyield$mean.yield < 600, "medium", "high"
                                  ))

#save(IPwithyield, file =  "~/Google Drive/surveySKEP1/IPwithyield.RData")
#save(injury.profiles, IPwithyield, country.season.dataset, country.season.cor.mat, country.season.net, file = "~/Google Drive/surveySKEP1/chapter3netdata.RData")


