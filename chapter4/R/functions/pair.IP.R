# create the list of all pair injury variables
start.IP <- "DH"
end.IP <- "GLH"
start.col.IP <- match(start.IP, names(injury.profiles))
end.col.IP <- match(end.IP, names(injury.profiles))

IP.data <- injury.profiles[start.col.IP:end.col.IP]


pair.IP.list <- matrix(nrow = 0, ncol = 2)

for(b in 2:(dim(IP.data)[2]-1)){
  for(c in (b+1):(dim(IP.data)[2])){
    new.row <- c(names(IP.data)[b], names(IP.data)[c])
    pair.IP.list <- rbind(pair.IP.list, new.row)          
  }
}
pair.IP.list <- data.frame(data.matrix(pair.IP.list))

names(pair.IP.list)<-c("IPvar1", "IPvar2")
