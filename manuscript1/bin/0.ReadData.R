#=====
# file name : 0.read.data.R
#=====

#==== Loading the data at the first time
# library(RCurl)
# file <- getURL("https://docs.google.com/spreadsheets/d/1zB7gNdI7Nk7SuHuPWcjzaKnjuwkvL6sOVMo0zMfuV-c/pub?gid=558862364&single=true&output=csv")
# data <- read.csv(text = file)
# save(data, file = "manuscript1/data/skep1data.RData")
#===

#====load the meta data ====
load(file = "manuscript1/data/skep1data.RData")

#==== Loading the libraries ====
library(ggplot2)
require(dplyr)
require(reshape)
require(reshape2)
require(qgraph)
library(gridExtra)
library(cowplot)
library(lubridate)
library(XLConnect)
library(qgraph)
library(IsingFit)
library(doBy)
library(cluster)



#==== remove the NA ====
data[data == "-"] <- NA # replace '-' with NA
data[data == ""] <- NA # replace 'missing data' with NA

#==== to lower variable names ====
names(data) <- tolower(names(data))

#==== Delete the unnessary variables variables without data (NA) ====

data$phase <- NULL # there is only one type yype of phase in the survey
data$identifier <- NULL # this variable is not included in the analysis
data$village <- NULL
data$year <- NULL
data$season <- NULL
data$lat <- NULL
data$long <- NULL
data$fa <- NULL # field area is not include in the analysis
data$fn <- NULL # farmer name can not be included in this survey analysis
data$fp <- NULL # I do not know what is fp
data$lfm <- NULL # there is only one type of land form in this survey
data$ced <- NULL # Date data can not be included in the network analysis
data$cedjul <- NULL
data$hd <- NULL # Date data can not be included in the network analysis
data$hdjul <- NULL
data$cvr <- NULL
data$varcoded <- NULL # I will recode them 
data$fymcoded <- NULL
data$mu <- NULL # no record
data$nplsqm <- NULL

#==== corract the variable type =====
data <- transform(data, 
                  country = as.factor(country),
                  year = as.factor(year),
                  season  = as.factor(season),   
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
                  defa  = as.numeric(defa) ,
                  bphx = as.numeric(bphx),   
                  wbpx = as.numeric(wbpx),    
                  awx  = as.numeric(awx), 
                  rbx =as.numeric(rbx),   
                  rbbx = as.numeric(rbbx),  
                  glhx  = as.numeric(glhx), 
                  stbx=as.numeric(stbx),    
                  rbpx = as.numeric(rbpx), 
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

#== recode the catagory data
 levels(data$country)[levels(data$country) == "IDN"] <- 1
 levels(data$country)[levels(data$country) == "IND"] <- 2
 levels(data$country)[levels(data$country) == "PHL"] <- 3
 levels(data$country)[levels(data$country) == "THA"] <- 4
 levels(data$country)[levels(data$country) == "VNM"] <- 5

# Previous crop

data$pc <- ifelse(data$pc == "rice", 1, 0)

# fym there are two type 0 and 1, raw data are recorded as no, yes, and value, if the value is 0 which mean 0 and if the value more than 0 which means 1 

data$fym <- ifelse(data$fym == "no", 0, 
                   ifelse(data$fym == "0", 0, 1
                   )
)

# vartype there are three type treditional varieties, modern varities and hybrid
data$vartype <- ifelse(data$vartype == "tv", 1,
                       ifelse(data$vartype == "mv", 2,
                              ifelse(data$vartype == "hyb", 3, NA
                              )
                       )
)

#Crop establisment method
levels(data$cem)[levels(data$cem) == "trp"] <- 1
levels(data$cem)[levels(data$cem) == "TPR"] <- 1
levels(data$cem)[levels(data$cem) == "DSR"] <- 2
levels(data$cem)[levels(data$cem) == "dsr"] <- 2


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


#==== cluster analysis of the production sitatuon of the survey data ====
# within data the rows no. 306 to 320

#data <- data[-c(155, 306:320), ]

start.PS <- "pc"
end.PS <- "fu"
start.col.PS <- match(start.PS, names(data))
end.col.PS <- match(end.PS, names(data))

PS.data <- data[start.col.PS:end.col.PS]

PS.data <- apply(PS.data, 2, as.numeric)
PS.data <- as.data.frame(as.matrix(PS.data))


PS.data <- na.omit(PS.data) # listwise deletion of missing
#mydata <- scale(mydata) # standardize variables

wss <- (nrow(PS.data)-1)* sum(apply(PS.data, 2, var))
for (i in 2:15) wss[i] <- sum(kmeans(PS.data, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")


#distance matrix
dist.PS <- daisy(PS.data)

cluster.PS <- hclust(dist.PS, method = "average")

dendro.PS <- as.dendrogram(cluster.PS)
plot(dendro.PS, center = T, nodePar = list(lab.cex = 0.6,
                                           lab.col = "black", pch = NA),
     main = "Dendrogram for Production situation")

# draw retangles
rect.hclust(tree = cluster.PS, k=4, border = c("red", "blue", "green", "purple"))

#number of members in each cluster
clusterno.PS <- cutree(cluster.PS, k = 4)

# cophenitic correlation
rcluster.PS <- cophenetic(cluster.PS)
cor(dist.PS, rcluster.PS)

data <- cbind(data[,-(306:320)], clusterno.PS)

#==================================================
# #subset the Indonesia data
idn <- data %>% 
  filter(country == 1) %>%
  select(-country)

#subset India data
ind <- data %>% 
  filter(country == 2) %>%
  select(-country)

#subset Philippines data
phl <- data %>% 
  filter(country == 3) %>%
  select(-country)

#subset Thailand data
tha <- data %>% 
  filter(country == 4) %>%
  select(-country)

#subset Vietnam data
vnm <- data %>% 
  filter(country == 5) %>%
  select(-country)

# #=== check PSno in each country ====
source("manuscript1/bin/multiple.ggplot.R")
p1 <- qplot(idn$clusterno.PS, geom="histogram")
p2 <- qplot(ind$clusterno.PS, geom="histogram")
p3 <- qplot(phl$clusterno.PS, geom="histogram")
p4 <- qplot(tha$clusterno.PS, geom="histogram")
p5 <- qplot(vnm$clusterno.PS, geom="histogram")
multiplot(p1, p2, p3, p4, p5, cols = 2)
