load(file = "manuscript1/data/ClusterSubsetData.RData")

# check the PS.cluster
temp1 <- idn %>% filter(clusterno.PS == 2)

start.PS <- "pc"
end.PS <- "fu"
start.col.PS <- match(start.PS, names(temp1))
end.col.PS <- match(end.PS, names(temp1))
PS.data <- temp1[start.col.PS:end.col.PS]
PS.data <- apply(PS.data, 2, as.numeric)
PS.data <- as.data.frame(as.matrix(PS.data))

m.all <- melt(PS.data)
varnames <- colnames(PS.data)

i <- 1
out <- NULL
for(i in 1:length(varnames)) {
  gdata <- m.all %>% filter(variable == varnames[i])
  p <- ggplot(gdata, aes(x = value)) + 
    geom_histogram(stat = "bin") + ggtitle(paste("Histogram of", varnames[i], sep = " "))
  dev.new()
  print(p) 
  out[[i]] <- p
}

grid.arrange(out[[1]],
             out[[2]],
             out[[3]],
             out[[4]],
             out[[5]],
             out[[6]],
             out[[7]],
             out[[8]],
             out[[9]],
             out[[10]],
             out[[11]],
             out[[12]],
             out[[13]],
             out[[14]],
             nrow = 4
)
