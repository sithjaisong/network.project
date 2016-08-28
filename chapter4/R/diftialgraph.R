
area <- c("TMN", "WJV", "LAG", "SUP", "MKD")
for(i in 1:5){
  mypath <- paste("/Users/sithjaisong/Documents/Github/network.project/chapter4/", "difgraph", area[i], ".png", sep ="")
png(file = mypath, width = 500, height = 500)
  E(difnet[[i]])$weight <- abs(E(difnet[[i]])$weight)
  difnet[[i]] <- delete.edges(difnet[[i]], which(E(difnet[[i]])$weight == 0))
  difnet[[i]]<- delete.vertices(difnet[[i]], which(igraph::degree(difnet[[i]]) == 0))
  plot(difnet[[i]], main = paste("Differential network of survey data in", area[i], sep = " "))
dev.off()
}

for(i in 1:5){
  mypath <- paste("/Users/sithjaisong/Documents/Github/network.project/chapter4/", "central_difgraph", area[i], ".png", sep ="")
  plot.node.centrality(difnet[[i]])
  ggsave(file = mypath)
}

