# this script used for runing  check the functions from qgraqph to create the partial correlation and corrected p-value by holm or even the fdrlocal

# the result these methods are not working because the output giving the p-value nonsignificant they are all pvalue is 1


library(RCurl) # run this package for load the data form the website 

file <- getURL("https://docs.google.com/spreadsheets/d/1zB7gNdI7Nk7SuHuPWcjzaKnjuwkvL6sOVMo0zMfuV-c/pub?gid=558862364&single=true&output=csv") # load data from the google drive

data <- read.csv(text = file) # read data which is formated as the csv

data[data == "-"] <- NA # replace '-' with NA

data[data == ""] <- NA # replace 'missing data' with NA

#==== to lower variable names ====
names(data) <- tolower(names(data)) # for more consistancy

data$phase <- NULL # there is only one type yype of phase in the survey
data$identifier <- NULL # this variable is not included in the analysis
data$ccs <- NULL
data$ccd <- NULL
data$village <- NULL # remove name of village
data$year <- NULL # remove year data
data$season <- NULL # remove season data
data$lat <- NULL # remove latitude data
data$long <- NULL # remove longitude data
data$fa <- NULL # field area is not include in the analysis
data$fn <- NULL # farmer name can not be included in this survey analysis
data$fp <- NULL # I do not know what is fp
data$pc <- NULL
data$cem <- NULL
data$ast <- NULL
data$vartype <- NULL
data$fym <- NULL
data$n <- NULL
data$p <- NULL
data$k <- NULL
data$mf <- NULL
data$wcp <- NULL
data$iu <- NULL
data$hu <- NULL
data$fu <- NULL
data$cs <- NULL
data$ldg <- NULL
data$yield <- NULL
data$dscum <- NULL
data$wecum <- NULL
data$ntmax <- NULL
data$npmax <- NULL
data$nltmax <- NULL
data$nlhmax <- NULL
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
data$rbpx <- NULL # no record of rice bug p

#======================================================================
#=================== corract the variable type ========================
#======================================================================

data <- transform(data, 
                  country = as.factor(country),
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


num.data <- apply(data[, -c(1,2)], 2, as.numeric) # create dataframe to store the numerical transformation of raw data excluded fno and country

num.data <- as.data.frame(as.matrix(num.data)) # convert from vector to matrix

data <- cbind(data[ , c("fno", "country")], num.data)

data <- data[ , apply(data[, -c(1,2)], 2, var, na.rm = TRUE) != 0] # exclude the column with variation = 0

data <- data[complete.cases(data), ] # exclude row which cantain NA

start.IP <- "dhx" # set to read the data from column named "dhx"

end.IP <- "rtx" # set to read the data from column named "rtx"

start.col.IP <- match(start.IP, names(data)) # match function for check which column of the data mactch the column named following the conditons above

end.col.IP <- match(end.IP, names(data)) # match function for check which column of the data mactch the column named following the conditons above

IP.data <- data[start.col.IP:end.col.IP] # select the columns of raw data which are following the condition above


IP.data <- IP.data[ ,apply(IP.data, 2, var, na.rm = TRUE) != 0] # exclude the column (variables) with variation = 0

#
country <- data$country  #combine two cloumn names country and PS

IP.data <- cbind(country, IP.data)

IP.data[is.na(IP.data)] <- 0

name.country <- as.vector(unique(IP.data$country))

#======= Experiment 1 ======= #

PHL <- IP.data %>% filter(country == "PHL")
PHL <- PHL[-1][ ,apply(PHL[-1], 2, var, na.rm = TRUE) != 0] # exclude the column (variables) with variation = 0

PHLCors <- cor(PHL, method = "spearman")

corGraph <- qgraph(PHLCors, layout = corGraph$layout, graph = "pcor", threshold = "bonferroni", sampleSize = nrow(PHL))

optGraph <- findGraph(as.data.frame(PHLCors), nrow(PHL), type = "cor")
