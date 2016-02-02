
# remove the variables that are not included for analysis
data$phase <- NULL # there is only one type yype of phase in the survey
data$identifier <- NULL # this variable is not included in the analysis
data$village <- NULL # remove name of village
data$fa <- NULL # field area is not include in the analysis
data$fn <- NULL # farmer name can not be included in this survey analysis
data$fp <- NULL # I do not know what is fp
data$lfm <- NULL # there is only one type of land form in this survey
data$ced <- NULL # Date data can not be included in the network analysis
data$cedjul <- NULL # remove crop establisment julian date data
data$hd <- NULL # Date data can not be included in the network analysis
data$hdjul <- NULL # remove harvest julian date
data$cvr <- NULL # reove crop varieties
data$varcoded <- NULL # I will recode them 
data$fymcoded <- NULL # remove code data of fym
data$mu <- NULL # no record of mullucicide data
data$nplsqm <- NULL # remove number of plant per square meter

#======================================================================
#=================== corract the variable type ========================
#======================================================================

data <- transform(data, 
                  country = as.factor(country),
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

##### Vartype


data$vartype <- ifelse(data$vartype == "tv", 1,
                       ifelse(data$vartype == "mv", 2,
                              ifelse(data$vartype == "hyb", 3, NA
                              )
                       )
)


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


### Pre-process before performing cluster analysis

#clean the data
num.data <- apply(data[, -c(1,2)], 2, as.numeric) # create dataframe to store the numerical transformation of raw data excluded fno and country

num.data <- as.data.frame(as.matrix(num.data)) # convert from vector to matrix

data <- cbind(data[ , c("fno", "country")], num.data)

data <- data[ , apply(data[, -c(1,2)], 2, var, na.rm = TRUE) != 0] # exclude the column with variation = 0

data <- data[complete.cases(data), ] # exclude row which cantain NA