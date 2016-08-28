# Load function
##############################################################################
# title         : weed_analysis.R;
# purpose       : compute weed data recoreded in the excel file;
# producer      : prepared by S. jaiosong;
# last update   : in Los Banos, IRRI, 21 January 2016;
# inputs        : crop health survey form 1 (excel);
# outputs       : injuries and disease data of SKEP Phase I from all locations;
##############################################################################

source("~/Documents/Github/network.project/chapter3/R/functions/function_audpc.R")

weed_analysis <- function(form2.weed) {
    # fill the na with 0
    ##### ----- Analysis sheet 3 weed infastration-----
    ##### Calcuation for Weed sheet3, especially WA and WB tranform from
    ##### scale to percent
    #' weed class 0 is 0 percent
    #' weed class 1 is up to 10 percent, 
    #' weed class 2 is 10 to 30 percent
    #' weed class 3 is 30 to 60 percent
    #' weed class 4 is 60 to 100 percent 
    ########################################################## 
    
    form2.weed["weed.above"][form2.weed["weed.above"] == "0"] <- 0
    form2.weed["weed.above"][form2.weed["weed.above"] == "1"] <- 5
    form2.weed["weed.above"][form2.weed["weed.above"] == "2"] <- 20
    form2.weed["weed.above"][form2.weed["weed.above"] == "3"] <- 45
    form2.weed["weed.above"][form2.weed["weed.above"] == "4"] <- 80
    
    form2.weed["weed.below"][form2.weed["weed.below"] == "0"] <- 0
    form2.weed["weed.below"][form2.weed["weed.below"] == "1"] <- 5
    form2.weed["weed.below"][form2.weed["weed.below"] == "2"] <- 20
    form2.weed["weed.below"][form2.weed["weed.below"] == "3"] <- 45
    form2.weed["weed.below"][form2.weed["weed.below"] == "4"] <- 80
    
    ### 
    
    output <- form2.weed %>% dplyr::group_by(index, Country, Year, Season, Fieldno, visit, DVS) %>% # find mean
    dplyr::mutate(m.WA = mean(weed.above), m.WB = mean(weed.below)) %>% 
    dplyr::group_by(index, Country, Year, Season, Fieldno) %>% 
      dplyr::summarise(x.WA = audpc(m.WA, 
        DVS), x.WB = audpc(m.WB, DVS))
    
    return(output)
} 
