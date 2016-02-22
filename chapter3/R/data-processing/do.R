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
library(reshape2)

# ===== Load function
source("~/Documents/Github/network.project/chapter3/R/functions/injury_analysis.R")
source("~/Documents/Github/network.project/chapter3/R/functions/weed_analysis.R")
source("~/Documents/Github/network.project/chapter3/R/functions/sweep_analysis.R")

#=== Load the FORM2 data
load(file = "~/Google Drive/surveySKEP1/FULLdatabase1.RData")

# names(FORM2)
# corract the type of varibale


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

#==== Select: Pest injuries and disease part

# select out the unreliable varibales

injury.data <- data %>% select(index, Country, Year, Season, Fieldno, visit, DVS, Q, Nt ,Np, Nl, 
                               SNL, DH, WH, GM, RT, WM, LF, BPH, WPH , AW, RB , BLB  ,LB, BS, BLS ,NBS, RS, SHB, SHR, SR, FS, NB, DP)

# the data NA is 0 because the the person sho input the data keep blank when data actualy are 0. And they means really 0, not NA, which is mean did not observe.

injury.data[is.na(injury.data)] <- 0 

analyzed.injury <- injury_analysis(injury.data)


#==== Select: Weed infastration

weed <- data %>% select(index, Country, Year, Season, Fieldno, visit, DVS, Area, weed.above, weed.below)

weed[is.na(weed)] <- 0

analyzed.weed <- weed_analysis(weed)

#==== Select sweep insect

GLH <- data %>% select(index, Country, Year, Season, Fieldno, visit, DVS, Sweep.no, GLH.sweep)

GLH[is.na(GLH)] <- 0

analyzed.GLH <- sweep_anlaysis(GLH)

# == combine the dataset

injury.profiles <- left_join(analyzed.injury, analyzed.GLH)

injury.profiles$Fieldno <- NULL

names(injury.profiles) <- c("index", "country", "year", "season", "SNL", "DH", "RT", "GM", "RB", "WH", "DP", "FS", "NB", "SHB", "SHR", "SR", 
                            "BPH", "WPH", "AW", "LF", "WM", "BLB", "BLS", "BS", "LB", "NBS", "RS", "GLH")

save(injury.profiles, analyzed.GLH, analyzed.weed, analyzed.injury, file = "~/Google Drive/surveySKEP1/injury_data.RData")
