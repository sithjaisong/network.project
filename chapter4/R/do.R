# continue from the chapter 3
library(igraph)
library(dplyr)
library(DiffCorr)
library(graph)
library(QuACN)

load(file = "~/Google Drive/surveySKEP1/chapter3netdata.RData")


source("~/Documents/Github/network.project/chapter4/R/functions/function_compcorr.R")
source("~/Documents/Github/network.project/chapter4/R/functions/function_diff.corr.R")

source("~/Documents/Github/network.project/chapter4/R/functions/function_plot.network.R")
source("~/Documents/Github/network.project/chapter4/R/functions/function_plot.difnetwork.R")

pair.IP.list <- as.data.frame(as.matrix(country.season.cor.mat[[9]][1:2]))
difnet <- list()
#==================================================================================
area <- c("TMN", "WJV", "LAG", "SUP", "MKD")
for(i in 1:5){
  
  country.season.cor.mat[[2*(i-1) + 1]]$rho[country.season.cor.mat[[ 2*(i-1)+1]]$p.value > 0.05 ] <- 0
  g1 <- graph.edgelist(as.matrix(country.season.cor.mat[[2*(i-1) + 1]][1:2]), directed = FALSE)
  E(g1)$weight <- as.matrix(country.season.cor.mat[[2*(i-1) + 1]][,3])
  
  country.season.cor.mat[[2*(i-1) + 2]]$rho[country.season.cor.mat[[2*(i-1) + 2]]$p.value > 0.05 ] <- 0
  g2 <- graph.edgelist(as.matrix(country.season.cor.mat[[2*(i-1) + 2]][1:2]), directed = FALSE)
  E(g2)$weight <- as.matrix(country.season.cor.mat[[2*(i-1) + 2]][,3])
  
  
  country.season.adj.mat1 <- as.matrix(as_adjacency_matrix(g1, attr ="weight"))
  
  country.season.adj.mat2 <- as.matrix(as_adjacency_matrix(g2, attr ="weight"))
  
  res <- diff.corr(country.season.adj.mat1 , country.season.adj.mat2)
  
  diff_comb <- left_join(pair.IP.list, res[c("var1", "var2", "r1", "r2")])
  
  diff_comb$index <- ifelse(abs(diff_comb$r1) - abs(diff_comb$r2) > 0, 1, -1)
  
  gdif <- graph.edgelist(as.matrix(diff_comb[1:2]), directed = FALSE)
  
  diff_comb[is.na(diff_comb)] <- 0
  E(gdif)$weight <- as.matrix(diff_comb[,5])
  difnet[[i]] <- plot_difnetwork(gdif)
  # ============================================================================
  
  l <-layout.fruchterman.reingold(g1) 
  layout(t(1:3))
  plot(plot_network(g1), layout = l, main = "Dry season")
  plot(plot_difnetwork(gdif), layout = l, main = paste("Differential network of survey data in", area[i], sep = " "))
  plot(plot_network(g2), layout = l, main = "Wet season")
    }

