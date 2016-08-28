# continue from the chapter 3
library(igraph)
library(dplyr)
library(DiffCorr)
library(graph)
library(QuACN)
library(reshape2)
library(ggplot2)

load(file = "~/Google Drive/surveySKEP1/IPwithyield.RData")

IPwithyield <- subset(IPwithyield, season == "DS")

# = group = #
low.yield.IP <- subset(IPwithyield, yield.level == "low")
medium.yield.IP <- subset(IPwithyield, yield.level == "medium")
high.yield.IP <- subset(IPwithyield, yield.level == "high")

level.yield.IP.list <- list(low.yield.IP, medium.yield.IP, high.yield.IP)



# =====
source("~/Documents/Github/network.project/chapter3/R/functions/function_cooc_table.R")
level.yield.IP.cor.mat <- list()

for (i in 1:length(level.yield.IP.list)) {
  
  # select out the country and season varible
  temp <- level.yield.IP.list[[i]][!names(level.yield.IP.list[[i]]) %in% c("index", "country", "year", "season", "SNL", "AW", "mean.yield", "yield.level")]
  
  temp <- as.data.frame(temp)
  # generate the corrlation matrix 
  
  level.yield.IP.cor.mat[[i]] <- cooc_table(temp)
}

##### Generate the network object  #####
source("~/Documents/Github/network.project/chapter3/R/functions/function_plot_network.R")
level.yield.IP.net <- list()

for (i in 1:length(level.yield.IP.cor.mat)) {
  
  # keep the correlation coefficient at p.value < 0.05
  cut.table <- level.yield.IP.cor.mat[[i]] 
  #%>% filter(p.value < 0.05)
  
  # construct the netwotk object 
  level.yield.IP.net[[i]] <- plot_network(cut.table)
  
}

 plot(level.yield.IP.net[[1]])
 plot(level.yield.IP.net[[2]])
 plot(level.yield.IP.net[[3]])
 
 # ===== for contruct the Venn diagram
pair.yield.all <- level.yield.IP.cor.mat[[1]] %>% dplyr::select(var1, var2) %>% mutate(pair = paste(var1, var2, sep = "_"))

pair.yield.low <- level.yield.IP.cor.mat[[1]] %>% filter(p.value < 0.05) %>% dplyr::select(var1, var2) %>% mutate(pair = paste(var1, var2, sep = "_"))

pair.yield.medium <- level.yield.IP.cor.mat[[2]] %>% filter(p.value < 0.05) %>% dplyr::select(var1, var2) %>% mutate(pair = paste(var1, var2, sep = "_"))

pair.yield.high <- level.yield.IP.cor.mat[[3]] %>% filter(p.value < 0.05) %>% dplyr::select(var1, var2) %>% mutate(pair = paste(var1, var2, sep = "_"))


intersect <- cbind(pair.low, pair.medium, pair.high)

# == End of construc the Ven diagrame

source("~/Documents/Github/network.project/chapter4/R/functions/function_plot.network.R")
source("~/Documents/Github/network.project/chapter4/R/functions/function_plot.difnetwork.R")

source("~/Documents/Github/network.project/chapter4/R/functions/function_compcorr.R")
source("~/Documents/Github/network.project/chapter4/R/functions/function_diff.corr.R")


# ==================================================================================
a <- 2
b <- 3

dif <- matrix(nrow = 0, ncol = 11)

for (a in 1:(length(level.yield.IP.cor.mat) - 1)){

  for (b in (a + 1):length(level.yield.IP.cor.mat)){
  
  level.yield.IP.cor.mat[[a]]$rho[level.yield.IP.cor.mat[[a]]$p.value > 0.05 ] <- 0
  
  level.yield.IP.cor.mat[[b]]$rho[level.yield.IP.cor.mat[[b]]$p.value > 0.05 ] <- 0
  
  g1 <- graph.edgelist(as.matrix(level.yield.IP.cor.mat[[a]][1:2]), directed = FALSE)
  E(g1)$weight <- as.matrix(level.yield.IP.cor.mat[[a]][,3])
  
  g2 <- graph.edgelist(as.matrix(level.yield.IP.cor.mat[[b]][1:2]), directed = FALSE)
  E(g2)$weight <- as.matrix(level.yield.IP.cor.mat[[b]][,3])
  
  level.yield.IP.adj.mat1 <- as.matrix(as_adjacency_matrix(g1, attr ="weight"))
  
  level.yield.IP.adj.mat2 <- as.matrix(as_adjacency_matrix(g2, attr ="weight"))
  
  
  pair.IP.list <- as.data.frame(as.matrix(level.yield.IP.cor.mat[[1]][1:2]))
  
  pair.IP.list$pair <- paste(pair.IP.list$var1, pair.IP.list$var2, sep ="_")
  
  res <- diff.corr(level.yield.IP.adj.mat1, level.yield.IP.adj.mat2)
  
  res$pair.dif <- paste(res$var1, res$var2, sep = "_")
  
  res$group1 <- a
  res$group2 <- b
  
  new.dif <- res
  
  dif <- rbind(dif, new.dif)
  
  }
 
}
xtable(dif)

#============================================================================


# entropy properties

level.yield.IP.netNEL <- lapply(level.yield.IP.net, as_graphnel)
temp <- as_graphnel(level.yield.IP.net[[1]])
# 
entropy.low <- as.data.frame(infoTheoreticGCM(level.yield.IP.netNEL[[1]],coeff = "lin", infofunct = "sphere")$pis)
entropy.low$node <- row.names(entropy.low)
row.names(entropy.low) <- NULL
names(entropy.low)[1] <- "entropy"
entropy.low$group <- "low.yield"

entropy.medium <- as.data.frame(infoTheoreticGCM(level.yield.IP.netNEL[[2]], coeff = "lin", infofunct = "sphere")$pis)
entropy.medium$node <- row.names(entropy.medium)
row.names(entropy.medium) <- NULL
names(entropy.medium)[1] <- "entropy"
entropy.medium$group <- "medium.yield"

entropy.high <- as.data.frame(infoTheoreticGCM(level.yield.IP.netNEL[[3]], coeff = "lin", infofunct = "sphere")$pis)
entropy.high$node <- row.names(entropy.high)
row.names(entropy.high) <- NULL
names(entropy.high)[1] <- "entropy"
entropy.high$group <- "high.yield"

entropy.all <- rbind(entropy.low, entropy.medium, entropy.high)

entropy.all$node <- NULL
ggplot(data = entropy.all, aes(x = group, y = entropy, fill = group)) + geom_boxplot()
# ================================
# other network statistic
source("~/Documents/Github/network.project/chapter3/R/functions/function_node_net_stat.R")  # node_stat() and net_stat()
source("~/Documents/Github/network.project/chapter3/R/functions/function_random_graph.R") 

# level.yield.IP.net
names(level.yield.IP.net) <- c("low.yield", "medium.yield", "high.yield")

node.stat.list <- sapply(level.yield.IP.net, node_stat, simplify = FALSE, 
                         USE.NAMES = TRUE)

# combine the node statistic from each network of country by season
node.df <- do.call(rbind, node.stat.list)

# add coolumn to indicate the source of node stat
node.df$group <- gsub("\\..*", "", row.names(node.df))

# delete row name
row.names(node.df) <- NULL

# reorder the colummn by moving column named country_season into the first
node.df <- node.df %>% dplyr::select(group, everything())

new.reform <- node.df %>% melt(id.vars = c("var", "group"), 
                               variable.name = "Node_Measure",
                               value.name = "Score") 

new.reform$group <- factor(new.reform$group, levels = c("low", "medium", "high"))
node_box <- ggplot(new.reform, aes(x = group, y = value)) + geom_boxplot() + facet_wrap( ~ variable, scales = 'free', ncol = 4)
ggsave(node_box, filename = "yield_ds_node_box.pdf")

## ======= network wise statistics of our empirical network

# apply net_stat function to all the object list
net.stat.list <- sapply(level.yield.IP.net, net_stat, simplify = FALSE, 
                        USE.NAMES = TRUE)
# combine the network stat from all list object
net.df <- do.call(rbind, net.stat.list)

# add column to indicate the source of data 
net.df$group <- row.names(net.df)

# delete row name 
row.names(net.df) <- NULL

# reorder the colummn by moving column named country_season into the first
net.df <- net.df %>% dplyr::select(group, everything())

## ======= random network for comparing our empirical network

# create round_net object to store the data
rand_net <- matrix(nrow = 0, ncol = 3)

for (i in 1:length(level.yield.IP.net)) {
  # simulate the random network using same parameter of the empirical network
  temp <- random_graph(level.yield.IP.net[[i]])
  
  # summarize the network properties of random network
  new.row <- temp %>% summarise(mclus_coef = mean(cluster_coef), 
                                mavr_path = mean(average_path))
  
  # name add column to indicate the source of network
  new.row$group <- names(level.yield.IP.net[i])
  
  # combine network stat of random network from all sources 
  rand_net <- rbind(rand_net, new.row)
}

# merge network stat of empirical network and random network
network.stat <- merge(net.df, rand_net)

# re arrange the column
network.stat <- network.stat[c("group", "Node", "Edges",  "CEN_BET", 
                               "CEN_CLO",  "CEN_DEG", "DENSITY", "DIAM",
                               "AVG_P", "mavr_path", "TRANSITIVITY", "mclus_coef")]
xtable(network.stat)
