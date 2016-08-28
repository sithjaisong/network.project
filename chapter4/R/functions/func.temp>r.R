```{r}
pair.IP.list <- matrix(nrow = 0, ncol = 2)
for(b in 2:(dim(IP.data)[2]-1)){
  for(c in (b+1):(dim(IP.data)[2])){
    new.row <- c(names(IP.data)[b], names(IP.data)[c])
    pair.IP.list <- rbind(pair.IP.list, new.row)			
  }
}
pair.IP.list <- data.frame(data.matrix(pair.IP.list))
names(pair.IP.list)<-c("IPvar1","IPvar2")
```

### Calculation of differential correlations
Fisher's z-test was used to identify significant differences between 2 correlations, based on its stringency test and its provision of conservative estimates of true differential correlations among molecules between 2 experimental conditions in the omics data. To test whether the 2 correlation coefficients were significantly different, we first transformed correlation coefficients for each of the 2 conditions, rA and rB, into $Z_A$ and $Z_B$, respectively. The Fisher's transformation of coefficient $r_A$ is defined by: 
  $Z = \frac{1}{2}\log{\frac{1+r_A}{1-r_A}}}$ 
  
  Similarly,

Fisher's z-transformation of r is defined as

$Z = \frac{Z_A - Z_B}{\sqrt{\frac{1}{n_A} + \frac{1}{n_B}}}$

we transform coefficient rB into ZB. Differences between the two correlations can be tested using the following Eq. (1)nA and nB represent the sample size for each of the conditions for each biomolecule pair (Fukushima et al., 2011; Fukushima et al., 2012; Morgenthal et al., 2006). The Z value has an approximately Gaussian distribution under the null hypothesis that the population correlations are equal. Controlling the FDR described by Benjamini and Hochberg (1995) is a stringent and practical method in multiple testing problems. However, while it assumes all tests to be independent, this is not the case for correlation tests. We therefore used the local false-discovery rate (fdr) derived from the fdrtool package (Strimmer, 2008).

```{r differtial network of Indonesia }
#cortest.normal(adj.mat[[1]], adj.mat[[2]], n1=1000, n2=1000)
diff.corr   <- function(data1,n1, data2, n2, N){
cc1 <- data1
cc2 <- data2
ccc1 <- as.vector(cc1[lower.tri(cc1)])
ccc2 <- as.vector(cc2[lower.tri(cc2)])
n1 <- n1
n2 <- n2
n <- N
N <- n * (n - 1)/2
p1 <- rep(1, N)
p2 <- rep(1, N)
pdiff <- rep(1, N)
diff <- rep(1, N)
mol.names <- rownames(adj.mat[[1]])
p1 <- cor2.test(n1, ccc1)
p2 <- cor2.test(n2, ccc2)
pdiff <- compcorr(n1, ccc1, n2, ccc2)$pval
diff <- ccc1 - ccc2
myindex <- which((lower.tri(cc1)) == TRUE, arr.ind = TRUE)
mol.names1 <- mol.names[myindex[, 2]]
mol.names2 <- mol.names[myindex[, 1]]
fin.ind <- pdiff < 0.05
res <- cbind(mol.names1[fin.ind], mol.names2[fin.ind], ccc1[fin.ind], p1[fin.ind], ccc2[fin.ind], p2[fin.ind], pdiff[fin.ind], diff[fin.ind])
res <- as.data.frame(res)
names(res) <- c("var1", "var2", "r1", "p1", "r2", "p2", "p.difference", "difr")
str(res)
res$var1 <- as.character(res$var1)
res$var2 <- as.character(res$var2)
res$r1 <- as.numeric(as.character(res$r1))
res$p1 <- as.numeric(as.character(res$p1))
res$r2 <- as.numeric(as.character(res$r2))
res$p2  <- as.numeric(as.character(res$p2))
res$p.difference <- as.numeric(as.character(res$p.difference))
res$difr <- as.numeric(as.character(res$difr))
return(res)
}
```

## Differential Network analysis of Indonesia
### The pairs of  injury profiles that are different significantly between PS1 and PS2 of Indonesia

```{r}
res <- diff.corr(data1 = adj.mat.IDN_1, 24, data2 = adj.mat.IDN_2, 24, 100)
diff_comb <- left_join(pair.IP.list, res[, -c(3:7)], by = c("IPvar1" = "var1", "IPvar2" = "var2"))
diff_comb[is.na(diff_comb)] <- 0
res
```


```{r fig.align='center', fig.width=15, fig.height=5}
par(mfrow = c(1,3))
com_g <- graph.edgelist(as.matrix(diff_comb[1:2]), directed = FALSE)
#== adjust vertices

V(com_g)$color <- "tomato"
V(com_g)$frame.color <- "gray40"
V(com_g)$shape <- "circle"
V(com_g)$size <- 25
V(com_g)$label.color <- "white"
V(com_g)$label.font <- 2
V(com_g)$label.family <- "Helvetica"
V(com_g)$label.cex <- 0.7

#== adjust the edge
E(com_g)$weight <- as.matrix(diff_comb[, 3])
E(com_g)$width <- 1 + E(com_g)$weight*5

col <- c("firebrick3", "forestgreen")
colc <- cut(diff_comb$difr, breaks = c(-1, 0, 1), include.lowest = TRUE)
#levels(colc)[1] <- "0"
E(com_g)$color <- col[colc]

com_g <- delete_edges(com_g, which(E(com_g)$weight == 0))
plot(g[[1]], layout = layout.circle, main = "Indonesia at PS1")
plot(com_g, layout = layout.circle, main = "Differential network \n between PS1 and PS2 ")
plot(g[[2]], layout = layout.circle, main = "Indonesia at PS2")

```

## Differential Network analysis of India
### The pairs of  injury profiles that are different significantly between PS1 and PS2 of India

```{r}
res2 <- diff.corr(data1 = adj.mat.IND_1, 24, data2 = adj.mat.IND_2, 24, 104)
diff_comb <- left_join(pair.IP.list, res2[, -c(3:7)], by = c("IPvar1" = "var1", "IPvar2" = "var2"))
diff_comb[is.na(diff_comb)] <- 0
res2
```


```{r diff net of India, fig.align='center', fig.width=15, fig.height=5}
par(mfrow = c(1,3))
com_g <- graph.edgelist(as.matrix(diff_comb[1:2]), directed = FALSE)
#== adjust vertices

V(com_g)$color <- "tomato"
V(com_g)$frame.color <- "gray40"
V(com_g)$shape <- "circle"
V(com_g)$size <- 25
V(com_g)$label.color <- "white"
V(com_g)$label.font <- 2
V(com_g)$label.family <- "Helvetica"
V(com_g)$label.cex <- 0.7

#== adjust the edge
E(com_g)$weight <- as.matrix(diff_comb[, 3])
E(com_g)$width <- 1 + E(com_g)$weight*5

col <- c("firebrick3", "forestgreen")
colc <- cut(diff_comb$difr, breaks = c(-1, 0, 1), include.lowest = TRUE)
#levels(colc)[1] <- "0"
E(com_g)$color <- col[colc]

com_g <- delete_edges(com_g, which(E(com_g)$weight == 0))
plot(g[[3]], layout = layout.circle, main = "India at PS1")
plot(com_g, layout = layout.circle, main = "Differential network \n between PS1 and PS2 ")
plot(g[[4]], layout = layout.circle, main = "INdia at PS2")
```

## Differential Network analysis of Thailand
### The pairs of  injury profiles that are different significantly between PS1 and PS2 of Thaialnd

```{r}
res2 <- diff.corr(data1 = adj.mat.THA_1, 24, data2 = adj.mat.THA_2, 24, 105)
diff_comb <- left_join(pair.IP.list, res2[, -c(3:7)], by = c("IPvar1" = "var1", "IPvar2" = "var2"))
diff_comb[is.na(diff_comb)] <- 0
res2
```


```{r diff net of Thaialnd, fig.align='center', fig.width=15, fig.height=5}
par(mfrow = c(1,3))
com_g <- graph.edgelist(as.matrix(diff_comb[1:2]), directed = FALSE)
#== adjust vertices

V(com_g)$color <- "tomato"
V(com_g)$frame.color <- "gray40"
V(com_g)$shape <- "circle"
V(com_g)$size <- 25
V(com_g)$label.color <- "white"
V(com_g)$label.font <- 2
V(com_g)$label.family <- "Helvetica"
V(com_g)$label.cex <- 0.7

#== adjust the edge
E(com_g)$weight <- as.matrix(diff_comb[, 3])
E(com_g)$width <- 1 + E(com_g)$weight*5

col <- c("firebrick3", "forestgreen")
colc <- cut(diff_comb$difr, breaks = c(-1, 0, 1), include.lowest = TRUE)
#levels(colc)[1] <- "0"
E(com_g)$color <- col[colc]

com_g <- delete_edges(com_g, which(E(com_g)$weight == 0))
plot(g[[5]], layout = layout.circle, main = "Thaialnd at PS1")
plot(com_g, layout = layout.circle, main = "Differential network \n between PS1 and PS2")
plot(g[[6]], layout = layout.circle, main = " Thailand at PS2")
```

## Differential Network analysis of Vietnam
### The pairs of  injury profiles that are different significantly between PS1 and PS2 of Vitnam

```{r}
res2 <- diff.corr(data1 = adj.mat.VNM_1, 24, data2 = adj.mat.VNM_2, 24, 105)
diff_comb <- left_join(pair.IP.list, res2[, -c(3:7)], by = c("IPvar1" = "var1", "IPvar2" = "var2"))
diff_comb[is.na(diff_comb)] <- 0
res2
```



```{r diff net of Vietnam, fig.align='center', fig.width=15, fig.height=5}
par(mfrow = c(1,3))
com_g <- graph.edgelist(as.matrix(diff_comb[1:2]), directed = FALSE)
#== adjust vertices

V(com_g)$color <- "tomato"
V(com_g)$frame.color <- "gray40"
V(com_g)$shape <- "circle"
V(com_g)$size <- 25
V(com_g)$label.color <- "white"
V(com_g)$label.font <- 2
V(com_g)$label.family <- "Helvetica"
V(com_g)$label.cex <- 0.7

#== adjust the edge
E(com_g)$weight <- as.matrix(diff_comb[, 3])
E(com_g)$width <- 1 + E(com_g)$weight*5

col <- c("firebrick3", "forestgreen")
colc <- cut(diff_comb$difr, breaks = c(-1, 0, 1), include.lowest = TRUE)
#levels(colc)[1] <- "0"
E(com_g)$color <- col[colc]

com_g <- delete_edges(com_g, which(E(com_g)$weight == 0))

plot(g[[7]], layout = layout.circle , main = "Vietnam at PS1")
plot(com_g, layout = layout.circle, main = "Differential network \n between PS1 and PS2 ")
plot(g[[8]], layout = layout.circle, main = "Vietnam at PS2")
```


See how the yield data are distributed 

```{r}
#head(data)
# select only the variables related to the injury profiles
start.IP <- "dhx"
end.IP <- "rtx"
start.col.IP <- match(start.IP, names(data))
end.col.IP <- match(end.IP, names(data))

IP.data <- data[start.col.IP:end.col.IP]

IP.data <- IP.data[ ,apply(IP.data, 2, var, na.rm = TRUE) != 0] # exclude the column with variation = 0
yield <- data$yield

IP.Y.data <- cbind(yield, IP.data)

IP.Y.data[is.na(IP.Y.data)] <- 0

```

```{r}
#hist(IP.Y.data$yield)
# from this histogram 
# I would group the data into six groups of yields.
ylevel <- ifelse(IP.Y.data$yield <= 3, "Y3", 
ifelse( 3 < IP.Y.data$yield & IP.Y.data$yield <= 4, "Y4",
ifelse(4 < IP.Y.data$yield & IP.Y.data$yield <= 5, "Y5",
ifelse(5 < IP.Y.data$yield & IP.Y.data$yield <= 6, "Y6",
ifelse(6 < IP.Y.data$yield & IP.Y.data$yield <= 7, "Y7", "Y8"
)))))
# y3 <- subset(IP.Y.data, yield <= 3)
# y4 <- subset(IP.Y.data, 3 < yield | yield >= 4 )
# y5 <- subset(IP.Y.data, 4 < yield | yield >= 5 )
# y6 <- subset(IP.Y.data, 5 < yield | yield >= 6 )
# y7 <- subset(IP.Y.data, 6 < yield | yield >= 7 )
# y8 <- subset(IP.Y.data, 7 < yield | yield >= 8 )
IP.Y.data <- cbind(ylevel, IP.Y.data)
IP.Y.data$ylevel <- as.factor(IP.Y.data$ylevel)
ylevels <- levels(IP.Y.data$ylevel)
IP.Y.data$activity.level <- rowSums(IP.Y.data[-1])

ggplot(IP.Y.data, aes(x= as.factor(ylevel), y= activity.level)) + 
geom_boxplot() +
stat_summary(fun.y = median, geom = "line", aes(group = 1)) +
stat_summary(func.y = median, geom = "point")

ggplot(IP.Y.data, aes(x= yield, y= activity.level)) + geom_point()

IP.Y.data.new <- IP.Y.data[, !names(IP.Y.data) %in% c("yield", "activity.level")]
```

```{r, echo = TRUE, warning = FALSE, message = FALSE}
results <- matrix(nrow = 0, ncol = 7)
options(warnings = -1)
for(a in 1:length(ylevels)){
#pull the first element from the vector of treatments
y.temp <- ylevels[a]
#subset the dataset for those treatments
temp <- subset(IP.Y.data.new, ylevel == y.temp)

#in this case the community data started at column 6, so the loop for co-occurrence has to start at that point
for(b in 2:(dim(temp)[2]-1)){
#every species will be compared to every other species, so there has to be another loop that iterates down the rest of the columns
for(c in (b+1):(dim(temp)[2])){

#summing the abundances of species of the columns that will be compared
species1.ab <- sum(temp[, b])
species2.ab <- sum(temp[, c])
#if the column is all 0's no co-occurrence will be performed
if(species1.ab >1 & species2.ab >1){
  test <- cor.test(temp[,b], temp[,c], method = "spearman", na.action = na.rm, exact = FALSE)
  # There are warnings when setting exact = TRUE because of ties from the output of Spearman's correlation
  # stackoverflow.com/questions/10711395/spear-man-correlation and ties
  # It would be still valid if the data is not normally distributed.
  rho <- test$estimate
  p.value <- test$p.value
}

if(species1.ab <=1 | species2.ab <= 1){
  rho<-0
  p.value<-1
}	

new.row <- c(ylevels[a], names(temp)[b], names(temp)[c], rho, p.value, species1.ab, species2.ab)
results <- rbind(results, new.row)			

}

}
print(a/length(ylevels))

}

results <- data.frame(data.matrix(results))
names(results) <- c("ylevel","taxa1","taxa2","rho","p.value","ab1","ab2")

#making sure certain variables are factors
results$ylevel <- as.factor(results$ylevel)
results$taxa1 <- as.character(as.factor(results$taxa1))
results$taxa2 <- as.character(as.factor(results$taxa2))
results$rho <- as.numeric(as.character(results$rho))
results$p.value <- as.numeric(as.character(results$p.value))
results$ab1 <- as.numeric(as.character(results$ab1))
results$ab2 <- as.numeric(as.character(results$ab2))

str(results)
head(results)
```

```{r}
# sub_y.results <- subset(results, as.numeric(as.character(abs(rho))) > 0.25)
sub_y.results <- results %>% filter(p.value < 0.05)

sub_ylevels.results <- list()

for(i in 1: length(ylevels)){
  
  sub_ylevels.results[[i]] <- subset(sub_y.results, ylevel == ylevels[i])
}
```


```{r fig.width=15, fig.height=10, fig.align='center'}
# head(results_sub.by.group[[1]][,2:3]) # get the list
par(mfrow = c(2, 3))

ynet  <- list()

for(i in 1:length(ylevels)){
  
  ynet[[i]] <- graph.edgelist(as.matrix(sub_ylevels.results[[i]][, 2:3]), directed = FALSE)
  #== adjust layout
  
  l <- layout.circle(ynet[[i]])
  #== adjust vertices
  V(ynet[[i]])$color <- "tomato"
  V(ynet[[i]])$frame.color <- "gray40"
  V(ynet[[i]])$shape <- "circle"
  V(ynet[[i]])$size <- 25
  V(ynet[[i]])$label.color <- "white"
  V(ynet[[i]])$label.font <- 2
  V(ynet[[i]])$label.family <- "Helvetica"
  V(ynet[[i]])$label.cex <- 0.7
  
  #== adjust the edge
  E(ynet[[i]])$weight <- as.matrix(sub_ylevels.results[[i]][, 4])
  
  E(ynet[[i]])$width <- 1 + E(ynet[[i]])$weight*5
  
  col <- c("firebrick3", "forestgreen")
  
  colc <- cut(sub_ylevels.results[[i]]$rho, breaks = c(-1, 0, 1), include.lowest = TRUE)
  
  E(ynet[[i]])$color <- col[colc]
  
  #== plot network model
  plot(ynet[[i]], layout = l * 1.0, main = paste( "network model of", ylevels[i]))
}
```

```{r}
network.value <- list()
i <-1

for(i in 1:length(ynet)){
  
  E(ynet[[i]])$weight <- abs(as.matrix(sub_ylevels.results[[i]][, 4]))
  
  network.value[[i]] <- data.frame(
    ylevel = ylevels[i],
    node = V(ynet[[i]])$name,
    strength = graph.strength(ynet[[i]]),
    betweenness = betweenness(ynet[[i]]),
    closeness = closeness(ynet[[i]]),
    eigenvector = evcent(ynet[[i]])$vector,
    clusterCoef = transitivity(ynet[[i]], type=c("local"))
  )
}
network.value <- as.data.frame(do.call("rbind", network.value))
row.names(network.value ) <- NULL
```

```{r}
p1 <- ggplot(network.value, aes(x = ylevel, y = closeness)) + geom_boxplot()
p2 <- ggplot(network.value, aes(x = ylevel, y = betweenness)) + geom_boxplot()
p3 <- ggplot(network.value, aes(x = ylevel, y = strength)) + geom_boxplot()
p4 <- ggplot(network.value, aes(x = ylevel, y = eigenvector)) + geom_boxplot()
p5 <- ggplot(network.value, aes(x = ylevel, y = clusterCoef)) + geom_boxplot()

p6 <- ggplot(IP.Y.data, aes(x= as.factor(ylevel), y= activity.level)) + 
  # geom_boxplot() +
  stat_summary(fun.y = median, geom = "line", aes(group = 1)) 
#stat_summary(func.y = median, geom = "point")

grid.arrange(p1, p2, p3, p4, p5, p6)

```


```{r}
IPvar_id <- names(IP.data)

rawVertex <- data.frame(start_time = 3, end_time = 9, vertex_id = 1:length(IPvar_id) ,vertex_var = IPvar_id)

```


```{r}
sub_IPdata <- results %>% filter(p.value < 0.05)

sub_IPdata$start_time <- ifelse(sub_IPdata$ylevel == "Y3", 3,
                                ifelse(sub_IPdata$ylevel == "Y4", 4,
                                       ifelse(sub_IPdata$ylevel == "Y5", 5,
                                              ifelse(sub_IPdata$ylevel == "Y6", 6,
                                                     ifelse(sub_IPdata$ylevel == "Y7", 7,
                                                            ifelse(sub_IPdata$ylevel == "Y8", 8, 0
                                                            ))))))
sub_IPdata$end_time <- ifelse(sub_IPdata$ylevel == "Y3", 3,
                              ifelse(sub_IPdata$ylevel == "Y4", 4,
                                     ifelse(sub_IPdata$ylevel == "Y5", 5,
                                            ifelse(sub_IPdata$ylevel == "Y6", 6,
                                                   ifelse(sub_IPdata$ylevel == "Y7", 7,
                                                          ifelse(sub_IPdata$ylevel == "Y8", 8, 0
                                                          ))))))
sub_IPdata$weight <-abs(sub_IPdata$rho)

sub_IPdata$type <- ifelse(sub_IPdata$rho < 0, "negative", "positive")

sub_IPdata$ylevel <- NULL
sub_IPdata$rho <- NULL
sub_IPdata$p.value <- NULL
sub_IPdata$ab1 <- NULL
sub_IPdata$ab2 <- NULL
names(sub_IPdata) <- c("from_var", "to_var", "start_time", "end_time", "weight", "type")
rawEdge <- sub_IPdata

list.IP.var <- rawVertex[, c(3,4)]

rawEdge$from_id <- list.IP.var$vertex_id[match(rawEdge$from_var, list.IP.var$vertex_var)]
rawEdge$to_id <- list.IP.var$vertex_id[match(rawEdge$to_var, list.IP.var$vertex_var)]

```

```{r}
dyn.net <- networkDynamic(vertex.spells = rawVertex[, c(1,2,3)], edge.spells = rawEdge[, c(3,4,7,8)], usearrows = FALSE)
plot(dyn.net)
#compute.animation(dyn.net, animation.mode = "kamadakawai", slice.par = list(start = 1, end = 6, interval = 1, aggregate.dur = 0, rule = 'any'), usearrows = FALSE)
```

Here is the network graph of injury profiles from different yield levels varying from 
3 is yields less than 3 ton per ha

4 is yield more than 3 to 4 ton per ha

5 is yield more than 4 to 5 ton per ha

6 is yield more than 5 to 6 ton per ha

7 is yield more than 6 to 7 ton per ha

8 is yield more than 7 ton per ha

```{r}
filmstrip(dyn.net, displaylabels = FALSE, mfrow = c(2, 3), slice.par = list(start = 3, end = 8, interval = 1, aggregate.dur = 0, rule = 'any'), usearrows = FALSE)
render.animation(dyn.net)
timeline(dyn.net, plot.vertex.spells = FALSE)
proximity.timeline(dyn.net, mode = 'sammon',
                   default.dist = 10,
                   labels.at = c(3, 4, 5, 6, 8),
                   llabel.cex = 0.7,
                   vertex.col = c(rep('gray', 24)),
                   start = 3, end = 8)
```

```{r}
proximity.timeline(dyn.net, mode = 'sammon',
                   default.dist = 10,
                   labels.at = c(3, 4, 5, 6, 8),
                   llabel.cex = 0.7,
                   vertex.col = c(rep('gray', 24)),
                   start = 3, end = 8,
                   spline.style = "inactive.ignore")
plot(1,2)
```