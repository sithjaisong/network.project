load(file = "manuscript1/data/Cluster.data.RData")

#head(clust.data)

clust.data$groups <- paste(clust.data$country, clust.data$clusterno.PS, sep="_")

start.IP <- "dhx"
end.IP <- "rtx"
start.col.IP <- match(start.IP, names(clust.data))
end.col.IP <- match(end.IP, names(clust.data))

IP.data <- clust.data[start.col.IP:end.col.IP]
IP.data <- apply(IP.data, 2, as.numeric)
IP.data <- as.data.frame(as.matrix(IP.data))

IP.data <- IP.data[ ,apply(IP.data, 2, var, na.rm = TRUE) != 0] # exclude the column with variation = 0
groups <- clust.data$groups

cluster.IP.data <- cbind(groups, IP.data)
cluster.IP.data[is.na(cluster.IP.data)] <- 0

# use data 1 
trts <- as.vector(unique(cluster.IP.data$groups))

#=====co_occurrence_pairwise.R====
results <- matrix(nrow = 0, ncol = 7)
options(warnings = -1)
for(a in 1:length(trts)){
  #pull the first element from the vector of treatments
  trt.temp <- trts[a]
  #subset the dataset for those treatments
  temp <- subset(cluster.IP.data, groups == trt.temp)
  
  #in this case the community data started at column 6, so the loop for co-occurrence has to start at that point
  for(b in 2:(dim(temp)[2]-1)){
    #every species will be compared to every other species, so there has to be another loop that iterates down the rest of the columns
    for(c in (b+1):(dim(temp)[2])){
      
      #summing the abundances of species of the columns that will be compared
      species1.ab <- sum(temp[,b])
      species2.ab <- sum(temp[,c])
      #if the column is all 0's no co-occurrence will be performed
      if(species1.ab >1 & species2.ab >1){
        test <- cor.test(temp[,b],temp[,c],method="spearman", na.action=na.rm, exact = FALSE)
        rho <- test$estimate
        p.value <- test$p.value
      }
      
      if(species1.ab <=1 | species2.ab <= 1){
        rho<-0
        p.value<-1
      }	
      
      new.row <- c(trts[a], names(temp)[b], names(temp)[c], rho, p.value, species1.ab, species2.ab)
      results <- rbind(results, new.row)			
      
    }
    
  }
  
  
  print(a/length(trts))
  
}
head(results)
results<-data.frame(data.matrix(results))
names(results)<-c("trt","taxa1","taxa2","rho","p.value","ab1","ab2")

#making sure certain variables are factors
results$trt <- as.factor(results$trt)
results$taxa1 <- as.character(as.factor(results$taxa1))
results$taxa2 <- as.character(as.factor(results$taxa2))
results$rho <- as.numeric(as.character(results$rho))
results$p.value <- as.numeric(as.character(results$p.value))
results$ab1 <- as.numeric(as.character(results$ab1))
results$ab2 <- as.numeric(as.character(results$ab2))

str(results)
head(results)
