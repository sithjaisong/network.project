library(RCurl) # run this package for load the data form the website 

file <- getURL("https://docs.google.com/spreadsheets/d/1zB7gNdI7Nk7SuHuPWcjzaKnjuwkvL6sOVMo0zMfuV-c/pub?gid=558862364&single=true&output=csv") # load data from the google drive

data <- read.csv(text = file) # read data which is formated as the csv

#####==== get the map ====####

library(ggmap)
mark.data <- data[, names(data) %in% c("Fno", "Lat", "Long")]
mapgilbert <- get_map(location = c(lon = mean(mark.data$Long), lat = mean(mark.data$Lat)), 
                      zoom = 4, maptype = "satellite", scale = 2)

ggmap(mapgilbert) + geom_point(data = mark.data, aes(x = Long, y = Lat, 
                                                     fill = "red", alpha = 0.8), size = 3, shape = 21) + guides(fill = FALSE, 
                                                                                                                alpha = FALSE, size = FALSE)
# library(rworldmap) names(data) mark.data <- data[, names(data) %in%
# c('Fno', 'Lat', 'Long')] map <- get_map(location = 'India and
# Southeast Asia', zoom = 4) newmap <- getMap(resolution = 'low')
# plot(newmap, xlim = c(70, 120), ylim = c(-10, 20), asp = 1)
# points(mark.data$Long, mark.data$Lat, col = 'red', cex = .6)


####==== Loading the packages ====####
library(dplyr)  # arrange data structure
library(plyr)
library(reshape)
library(reshape2)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(scales)
library(cowplot)
library(bioDist)  # Co-ocurrance analysis
library(vegan)  # Co-ocurrance analysis
library(WGCNA)
library(igraph)  # Network analysis package
library(qgraph)


####==== tidy the data ====####
data[data == "-"] <- NA  # replace '-' with NA

data[data == ""] <- NA  # replace 'missing data' with NA

names(data) <- tolower(names(data))  # for more consistancy


data <- data[, !names(data) %in% c("phase", "identifier", "village","css", "ccd", 
                                   "year", "season", "lat", "long", "fa", "fn",
                                   "fp", "pc", "cem", "ast", "vartype", "varcoded", 
                                   "fym", "fymcoded", "n", "p", "k", "mf", "wcp", "iu", "hu",
                                   "fu", "cs", "ldg", "yield", "dscum", "wecum", 
                                   "nplsqm", "npmax", "nltmax", "nlhmax", "lfm", 
                                   "ced", "cedjul", "hd", "hdjul", "cvr", "varcode", 
                                   "fymcode", "mu", "nplsm", "rbpx")
             ]

data <- transform(data, country = as.factor(country), waa = as.numeric(waa), 
                  wba = as.numeric(wba), dhx = as.numeric(dhx), whx = as.numeric(whx), 
                  ssx = as.numeric(ssx), wma = as.numeric(wma), lfa = as.numeric(lfa), 
                  lma = as.numeric(lma), rha = as.numeric(rha), thrx = as.numeric(thrx), 
                  pmx = as.numeric(pmx), defa = as.numeric(defa), bphx = as.numeric(bphx), 
                  wbpx = as.numeric(wbpx), awx = as.numeric(awx), rbx = as.numeric(rbx), 
                  rbbx = as.numeric(rbbx), glhx = as.numeric(glhx), stbx = as.numeric(stbx), 
                  hbx = as.numeric(hbx), bbx = as.numeric(bbx), blba = as.numeric(blba), 
                  lba = as.numeric(lba), bsa = as.numeric(bsa), blsa = as.numeric(blsa), 
                  nbsa = as.numeric(nbsa), rsa = as.numeric(rsa), lsa = as.numeric(lsa), 
                  shbx = as.numeric(shbx), shrx = as.numeric(shrx), srx = as.numeric(srx), 
                  fsmx = as.numeric(fsmx), nbx = as.numeric(nbx), dpx = as.numeric(dpx), 
                  rtdx = as.numeric(rtdx), rsdx = as.numeric(rsdx), gsdx = as.numeric(gsdx), 
                  rtx = as.numeric(rtx))


num.data <- apply(data[, -c(1, 2)], 2, as.numeric)  # create dataframe to store the numerical transformation of raw data excluded fno and country

num.data <- as.data.frame(as.matrix(num.data))  # convert from vector to matrix

data <- cbind(data[, c("fno", "country")], num.data)

data <- data[, apply(data[, -c(1, 2)], 2, var, na.rm = TRUE) != 0]  # exclude the column with variation = 0

data <- data[complete.cases(data), ]  # exclude row which cantain NA



####==== exploring graph of the survey data ====#### 
m.data <- melt(data[, !names(data) %in% c("fno", "country")])

varnames <- colnames(data[, !names(data) %in% c("fno", "country")])

p <- list()

for (i in 1:length(varnames)) {
  
  gdata <- m.data %>% filter(variable == varnames[i])
  p[[i]] <- ggplot(gdata, aes(x = value)) + geom_histogram(stat = "bin") + 
    ggtitle(paste("Histogram of", varnames[i], sep = " "))
  
}

plot_grid(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], p[[6]], p[[7]], p[[8]], 
          p[[9]], p[[10]], p[[11]], p[[12]], p[[13]], p[[14]], p[[15]], p[[16]], 
          p[[17]], p[[18]], p[[19]], p[[20]], p[[21]], p[[22]], p[[23]], p[[24]], 
          p[[25]], p[[26]], p[[27]], p[[28]], p[[29]], p[[30]], p[[31]], ncol = 3, 
          align = "v")




### Co-occurence Analysis
start.IP <- "dhx"  # set to read the data from column named 'dhx'

end.IP <- "rtx"  # set to read the data from column named 'rtx'

start.col.IP <- match(start.IP, names(data))  # match function for check which column of the data mactch the column named following the conditons above

end.col.IP <- match(end.IP, names(data))  # match function for check which column of the data mactch the column named following the conditons above

IP.data <- data[start.col.IP:end.col.IP]  # select the columns of raw data which are following the condition above

IP.data <- IP.data[, apply(IP.data, 2, var, na.rm = TRUE) != 0]  # exclude the column (variables) with variation = 0

# 
sampleTree <- hclust(dist(IP.data[-1]), method = "average")

plot(sampleTree, main = "Sample clus")

# Plot a line to show the cut
abline(h = 265, col = "red")

country <- data$country  #combine two cloumn names country and PS

IP.data <- cbind(country, IP.data)

IP.data[is.na(IP.data)] <- 0

name.country <- as.vector(unique(IP.data$country))

# =====co_occurrence_pairwise.R====
country.results <- matrix(nrow = 0, ncol = 7)  # create results to store the outputs

options(warnings = -1)  # setting not to show the massages as the warnings

for (a in 1:length(name.country)) {
  # subset the raw data by groups of countries and cluster of production
  # situation
  country.temp <- name.country[a]
  # subset the dataset for those treatments
  temp <- subset(IP.data, country == country.temp)
  
  # in this case the community data started at column 6, so the loop for
  # co-occurrence has to start at that point
  for (b in 2:(dim(temp)[2] - 1)) {
    # every species will be compared to every other species, so there has
    # to be another loop that iterates down the rest of the columns
    for (c in (b + 1):(dim(temp)[2])) {
      
      # summing the abundances of species of the columns that will be
      # compared
      species1.ab <- sum(temp[, b])
      
      species2.ab <- sum(temp[, c])
      # if the column is all 0's no co-occurrence will be performed
      if (species1.ab > 1 & species2.ab > 1) {
        
        test <- cor.test(temp[, b], temp[, c], method = "spearman", 
                         na.action = na.rm, exact = FALSE)
        # There are warnings when setting exact = TRUE because of ties from the
        # output of Spearman's correlation
        # stackoverflow.com/questions/10711395/spear-man-correlation and ties
        # It would be still valid if the data is not normally distributed.
        
        rho <- test$estimate
        
        p.value <- test$p.value
      }
      
      if (species1.ab <= 1 | species2.ab <= 1) {
        
        rho <- 0
        
        p.value <- 1
      }
      
      new.row <- c(name.country[a], names(temp)[b], names(temp)[c], 
                   rho, p.value, species1.ab, species2.ab)
      
      country.results <- rbind(country.results, new.row)
      
    }
    
  }
  
}

country.results <- data.frame(data.matrix(country.results))

names(country.results) <- c("country", "var1", "var2", "rho", "p.value", 
                            "ab1", "ab2")

# making sure certain variables are factors
country.results$country <- as.factor(country.results$country)
country.results$var1 <- as.character(as.factor(country.results$var1))
country.results$var2 <- as.character(as.factor(country.results$var2))
country.results$rho <- as.numeric(as.character(country.results$rho))
country.results$p.value <- as.numeric(as.character(country.results$p.value))
country.results$ab1 <- as.numeric(as.character(country.results$ab1))
country.results$ab2 <- as.numeric(as.character(country.results$ab2))

####==== thresholding ====####
sub_country.results <- subset(country.results, as.numeric(as.character(p.value)) < 0.05)

results_sub.by.country <- list()

for(i in 1: length(name.country)){
  
  results_sub.by.country[[i]] <- subset(sub_country.results, country == name.country[i])
}

### Network analysis

# head(results_sub.by.group[[1]][,2:3]) # get the list
# layout(matrix(c(1:14), 9, 2, byrow = TRUE))
png("Netcountry.png", units="px", width=2400, height=3600, res=300)
layout(rbind(c(1,2), c(3,4), c(5,6)))


cnet <- list()

for (i in 1:length(name.country)) {
  
  cnet[[i]] <- graph.edgelist(as.matrix(results_sub.by.country[[i]][, 2:3]), directed = FALSE)
  
  l <- layout.circle(cnet[[i]])
  
  V(cnet[[i]])$color <- adjustcolor("slateblue1", alpha.f = .6)
  V(cnet[[i]])$frame.color <- adjustcolor("slateblue1", alpha.f = .6)
  V(cnet[[i]])$shape <- "circle"
  V(cnet[[i]])$size <- 20
  V(cnet[[i]])$label.color <- "white"
  V(cnet[[i]])$label.font <- 2
  V(cnet[[i]])$label.family <- "Helvetica"
  V(cnet[[i]])$label.cex <- 0.7
  
  # == adjust the edge
  E(cnet[[i]])$weight <- as.matrix(results_sub.by.country[[i]][, 4])
  
  E(cnet[[i]])$width <- abs(E(cnet[[i]])$weight) * 10
  
  col <- c("salmon", "forestgreen")
  
  colc <- cut(results_sub.by.country[[i]]$rho, breaks = c(-1, 0, 1), 
              include.lowest = TRUE)
  
  E(cnet[[i]])$color <- col[colc]
  
  # == plot network model
  plot(cnet[[i]], layout = l * 1, margin = -0.2, main = paste("network model of", name.country[i]))
}

dev.off()


network.value <- list()

for(i in 1:length(g)){
  
  E(g[[i]])$weight <- as.matrix(abs(results_sub.by.group[[i]][, 4]))
  
  network.value[[i]] <- data.frame(
    id = V(g[[i]])$name, 
    deg = degree(g[[i]]), # degree
    bet = betweenness(g[[i]]), # betweenness
    clo = closeness(g[[i]]), # closeness
    eig = evcent(g[[i]])$vector,# egin.cent
    cor = graph.coreness(g[[i]]), # coreness
    tra = transitivity(g[[i]], type = c("local")) # cluster coefficients
  )
  
  network.value[[i]]$res <- as.vector(lm(eig ~ bet, data = network.value[[i]])$residuals)
  network.value[[i]]$group <- name.groups[i]
}

net.vertice.data <- as.data.frame(do.call("rbind", network.value))
row.names(net.vertice.data) <- NULL


net.vertice.data$id <- as.factor(net.vertice.data$id)
net.vertice.data$group <- as.factor(net.vertice.data$group)
m.net.vertice.data <- melt(net.vertice.data)

# compare degree

m.net.vertice.data %>% filter(variable == "deg") %>% 
  ggplot(aes(x= value, y = id)) + 
  geom_point(size = 3, color ="blue") +
  facet_wrap(~group) +
  theme_bw() +
  theme(panel.grid.major.x =  element_blank(),
        panel.grid.minor.x =  element_blank(),
        panel.grid.major.y = element_line(color = "grey", linetype = 3)) +
  ggtitle("Degree of each node in the injury profile network")

m.net.vertice.data %>% filter(variable == "bet") %>% 
  ggplot(aes(x= value, y = id)) + 
  geom_point(size = 3, color ="blue") +
  facet_wrap(~group) +
  theme_bw() +
  theme(panel.grid.major.x =  element_blank(),
        panel.grid.minor.x =  element_blank(),
        panel.grid.major.y = element_line(color = "grey", linetype = 3)) +
  ggtitle("Betweenness of each node in the injury profile network")

m.net.vertice.data %>% filter(variable == "bet") %>% 
  ggplot(aes(x= value, y = id)) + 
  geom_point(size = 3, color ="blue") +
  facet_wrap(~group) +
  theme_bw() +
  theme(panel.grid.major.x =  element_blank(),
        panel.grid.minor.x =  element_blank(),
        panel.grid.major.y = element_line(color = "grey", linetype = 3)) +
  ggtitle("Betweenness of each node in the injury profile network")

m.net.vertice.data %>% filter(variable == "clo") %>% 
  ggplot(aes(x= value, y = id)) + 
  geom_point(size = 3, color ="blue") +
  facet_wrap(~group) +
  theme_bw() +
  theme(panel.grid.major.x =  element_blank(),
        panel.grid.minor.x =  element_blank(),
        panel.grid.major.y = element_line(color = "grey", linetype = 3)) +
  ggtitle("Closeness of each node in the injury profile network")

m.net.vertice.data %>% filter(variable == "eig", group == "IDN_1") %>% 
  ggplot(aes(x= value, y = reorder(id, value))) + 
  geom_point(size = 3, color ="blue") +
  facet_wrap(~group) +
  theme_bw() +
  theme(
    panel.grid.major.x =  element_blank(),
    panel.grid.minor.x =  element_blank(),
    panel.grid.major.y = element_line(color = "grey", linetype = 3)
  ) +
  ggtitle("Eigenvector of each node in the injury profile network")

m.net.vertice.data %>% filter(variable == "cor", group == "IDN_1") %>% 
  ggplot(aes(x= value, y = reorder(id, value))) + 
  geom_point(size = 3, color ="blue") +
  facet_wrap(~group) +
  theme_bw() +
  theme(
    panel.grid.major.x =  element_blank(),
    panel.grid.minor.x =  element_blank(),
    panel.grid.major.y = element_line(color = "grey", linetype = 3)
  ) +
  ggtitle("Eigenvector of each node in the injury profile network")



# One method is to plot / regress eigenvector centrality on betweenness and examine the residue.

# - A variable or node with high betweenness and low eigenvector centrality may be an importanct gatkeeper to a central actor
# - A variable or node with low betweeness and hight eigenvector centrality may have unique access to central actor

for(i in 1:length(network.value)){
  plot <- ggplot(
    network.value[[i]], aes(x = bet, y = eig,
                            label = rownames(network.value[[i]]),
                            color = res,
                            size = abs(res))) +
    xlab("Betweenness Centrality") +
    ylab("Eigencvector Centrality") +
    geom_text() +
    ggtitle(paste("Key Actor Analysis for Injuiry Profiles of", name.groups[i]))
  
  print(plot) 
}

# ===================== The ideas ===========
# dd <- degree.distribution(g[[1]], cumulative = T, mode = "all")
# plot(dd, pch = 19, cex = 1, col = "orange", xlab = "Degree", ylab = "Cumulative Frequesncy")
# distances(g[[1]])
# #shortest_paths(g[[1]], 5)
# all_shortest_paths(g[[1]], 1, 6:8)
# mean_distance(g[[1]])
# hist(degree(g[[1]]),  col = "lightblue", xlim = c(0, 10), xlab = "Vertex Degree", ylab = "Frequency", main = "")
# hist(graph.strength(g[[1]]),  col = "pink", xlim = c(0, 5), xlab = "Vertex strength", ylab = "Frequency", main = "")

#global.prop <- NULL
#i <-1
#for(i in 1:length(g)){
#adj.mat <- as.matrix(get.adjacency(g[[i]], attr = "weight"))
#new.global.prop <- as.data.frame(fundamentalNetworkConcepts(adj.mat))
#names(new.global.prop) <- name.groups[i]
#global.prop <- c(global.prop, new.global.prop)
#net.result <- fundamentalNetworkConcepts(mat[[i]])
#conformityBasedNetworkConcepts(mat)
#}
#sum.global <- as.data.frame(do.call("cbind", global.prop))
#rownames(sum.global) <- c("Density", "Centralization", "Heterogeneity", "Mean ClusterCoef", "Mean Connectivity")



# cleate all the possible pair of the variables
# IP.list <- NULL
#   for(i in 2:(dim(IP.data)[2]-1)){
#      for(j in (i+1):(dim(IP.data)[2])){
#       new.row <- c(names(IP.data)[i], names(IP.data)[2 + j])
#       IP.list <- rbind(IP.list, new.row)			
#     }
#   }
# 
# IP.list <- as.data.frame(IP.list)
# names(IP.list) <- c("var1", "var2")
# IP.list$rho <- 0


# results$newrho <- ifelse(abs(results$rho) > 0.25, results$rho, 0)
#  
# IDN1 <- results %>% filter(trt == "IDN_1")
#  IDN2 <- results %>% filter(trt == "IDN_2")
#  IND1 <- results %>% filter(trt == "IND_1")
#  IND2 <- results %>% filter(trt == "IND_2")
#  THA1 <- results %>% filter(trt == "THA_1")
#  THA2 <- results %>% filter(trt == "THA_2")
#  VNM1 <- results %>% filter(trt == "VNM_1")
#  VNM2 <- results %>% filter(trt == "VNM_1")
#  PHL1 <- results %>% filter(trt == "PHL_1")
