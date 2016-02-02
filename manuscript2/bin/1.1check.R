#Load and run the script 1.load.R first 
library(dplyr)
library(psych)

# change the name of variables to small letter
names(data) <- tolower(names(data))

# remove some columns numnessary

data <-  data %>% select(-c(phase,identifier, fa, fn, fp,
                   lfm, ced, cedjul, hd, hdjul,
                   cvr, varcoded, fymcoded, mu, nplsqm)
                )

data <- data %>% transform(
                  fno = as.numeric(fno),
                  country = as.factor(country),
                  year = as.factor(year),
                  season = as.factor(season),
                  lat = as.numeric(lat),
                  long = as.numeric(long),
                  village = as.character(village),
                  pc = as.factor(pc),
                  cem = as.factor(cem),     
                  ast = as.factor(ast),       
                  ccd = as.numeric(ccd),
                  vartype = as.factor(vartype),
                  fym = as.character(fym),
                  n = as.numeric(n),
                  p = as.numeric(p) ,
                  k = as.numeric(k),
                  mf = as.numeric(mf),        
                  wcp = as.factor(wcp),      
                  iu = as.numeric(iu),     
                  hu = as.numeric(hu),      
                  fu = as.numeric(fu),      
                  cs  = as.factor(cs),      
                  ldg  =  as.numeric(ldg),  
                  yield = as.numeric(yield) ,
                  dscum = as.factor(dscum),   
                  wecum = as.factor(wecum),   
                  ntmax = as.numeric(ntmax), 
                  npmax = as.numeric(npmax),    
                  nltmax = as.numeric(nltmax),  
                  nlhmax = as.numeric(nltmax),  
                  waa = as.numeric(waa),      
                  wba = as.numeric(wba) ,   
                  dhx =  as.numeric(dhx),  
                  whx =  as.numeric(whx),     
                  ssx  = as.numeric(ssx),  
                  wma = as.numeric(wma), 
                  lfa = as.numeric(lfa),
                  lma = as.numeric(lma),   
                  rha  = as.numeric(rha) ,
                  thrx = as.numeric(thrx),    
                  pmx = as.numeric(pmx),    
                  defa  = as.numeric(defa),
                  bphx = as.numeric(bphx),   
                  wbpx = as.numeric(wbpx),    
                  awx  = as.numeric(awx), 
                  rbx =as.numeric(rbx),   
                  rbbx = as.numeric(rbbx),  
                  glhx  = as.numeric(glhx), 
                  stbx=as.numeric(stbx),    
                  hbx= as.numeric(hbx),
                  bbx = as.numeric(bbx),    
                  blba = as.numeric(blba),    
                  lba = as.numeric(lba),    
                  bsa = as.numeric(bsa),    
                  blsa = as.numeric(blsa),  
                  nbsa = as.numeric(nbsa),  
                  rsa  = as.numeric(rsa),   
                  lsa = as.numeric(lsa),    
                  shbx = as.numeric(shbx) ,  
                  shrx = as.numeric(shrx),    
                  srx= as.numeric(srx),    
                  fsmx = as.numeric(fsmx),   
                  nbx =  as.numeric(nbx),   
                  dpx = as.numeric(dpx),    
                  rtdx  = as.numeric(rtdx),  
                  rsdx  = as.numeric(rsdx),
                  gsdx  =as.numeric(gsdx),   
                  rtx = as.numeric(rtx)
) 

#======================================================================
#=================== corract the variable type ========================
#======================================================================

# Now data are in the right format and ready to further analysis, but there are some variables needed to code as the number not character
# Before proforming cluster analysis which is the further analysis, I need to code the character to number

##### recode the previous crop

#if previosu crop data are rice, they will be coded as 1, but others, not rice, they will be coded as 0.

data$pc <- ifelse(data$pc == "rice", 1, 0)
data$pc <- as.factor(data$pc)

##### recode the crop establisment mothods


#Crop establisment method
levels(data$cem)[levels(data$cem) == "trp"] <- 1
levels(data$cem)[levels(data$cem) == "TPR"] <- 1
levels(data$cem)[levels(data$cem) == "DSR"] <- 2
levels(data$cem)[levels(data$cem) == "dsr"] <- 2

##### fym



data$fym <- ifelse(data$fym == "no", 0, 
                   ifelse(data$fym == "0", 0, 1
                   )
)
data$fym <- as.factor(data$fym)

##### Vartype


data$vartype <- ifelse(data$vartype == "tv", 1,
                       ifelse(data$vartype == "mv", 2,
                              ifelse(data$vartype == "hyb", 3, NA
                              )
                       )
)
data$vartype <- as.factor(data$vartype)

##### Weed control practices

# wcp weed control management
levels(data$wcp)[levels(data$wcp) == "hand"] <- 1

levels(data$wcp)[levels(data$wcp) == "herb"] <- 2

levels(data$wcp)[levels(data$wcp) == "herb-hand"] <- 3


# Crop Status
levels(data$cs)[levels(data$cs) == "very poor"] <- 1

levels(data$cs)[levels(data$cs) == "poor"] <- 2

levels(data$cs)[levels(data$cs) == "average"] <- 3

levels(data$cs)[levels(data$cs) == "good"] <- 4

levels(data$cs)[levels(data$cs) == "very good"] <- 5

## end of corraction

data <- as.tbl(data)

idn <- data %>% filter(country == "IDN")

ind <- data %>% filter(country == "IND")

phi <- data %>% filter(country == "PHL")

tha <- data %>% filter(country == "THA")

vnm <- data %>% filter(country == "VNM")

<<<<<<< Updated upstream
##############################################################################
############### Map ##########################################################
#
#
#
############################################################################
# geo data
#idn.geo <- idn %>% select(fno:village)

#idn.geo$village

# library(XML)
# theurl <- "https://en.wikipedia.org/wiki/List_of_regencies_and_cities_of_Indonesia#West_Java"
# webpage <- getURL(theurl)
# 
# pagetree <- htmlTreeParse(webpage, error=function(...){}, useInternalNodes = TRUE)
# 
# # Extract table header and contents
# tablehead <- xpathSApply(pagetree, "//*/table[@class='wikitable sortable']/tr/th", xmlValue)
# results <- xpathSApply(pagetree, "//*/table[@class='wikitable sortable']/tr/td", xmlValue)

# Here are the dictrict lists:Cikempak, Pamanukan, 
###############################################################################


## exploratory analysis for indonesia
# for the production situation and injuries data 
idn.new <- idn %>% select(pc:rtx)
=======
## exploratory analysis

phi.new <- phi %>% select(pc:rtx)
>>>>>>> Stashed changes

str(phi.new)
describe(idn.new)

phi <- phi[ , apply(phi[, -c(1,2)], 2, var, na.rm = TRUE) != 0] # exclude the column with variation = 0

phi <- phi[complete.cases(phi), ] # exclude row which cantain NA
