
# Summary 
# The workflow of this research composed of  four steps
# 1. Load survey data of SKEP1  (raw data), and compact the data.
# 2. Generate the correlation matrix for building a network from data that were grouped by country and season
# 3. Analyze the topological statistic of the network, and interpret 
# 4. Detect the communities within the network
# 

library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(extrafont)
library(ggExtra)
library(gtable)
library(igraph)
library(xtable)
library(reshape2)

##### 1.Load survey data of SKEP1  (raw data), and compact the data. ######
# The data are in the shared folder named "surveySKEP1"
# You can download or folk from Github:[Crop_Survey_Database](https://github.com/sithjaisong/Crop_Survey_Database.git) from my fork.

source("~/Documents/Github/network.project/chapter3/R/data-processing/do.R")
#
head(injury.profiles)

injury.profiles <- injury.profiles %>% transform(country = as.factor(country), 
                                                 year = as.factor(year), season = as.factor(season))

# I created the compared the variable by country by season
# reshape
injury.profiles$year <- NULL  # remove column year
injury.profiles$index <- NULL

# set data as injury profiles
injury.profiles$SNL <- NULL
injury.profiles$AW <- NULL
#source("~/Documents/Github/network.project/chapter3/R/network-analysis/figure/F1.dataset.R")

#all.dataset.boxplot <- boxplot_survey_data(injury.profiles)

#all.dataset.boxplot


#### 2. Generate the correlationsion matrix  #####

source("~/Documents/Github/network.project/chapter3/R/data-processing/co_occurrence_matrix.R")

# subset the data by country and season
ind.ds <- injury.profiles %>% filter(country == "IND" & season == "DS")
ind.ws <- injury.profiles %>% filter(country == "IND" & season == "WS")
idn.ds <- injury.profiles %>% filter(country == "IDN" & season == "DS")
idn.ws <- injury.profiles %>% filter(country == "IDN" & season == "WS")
phl.ds <- injury.profiles %>% filter(country == "PHL" & season == "DS")
phl.ws <- injury.profiles %>% filter(country == "PHL" & season == "WS")
tha.ds <- injury.profiles %>% filter(country == "THA" & season == "DS")
tha.ws <- injury.profiles %>% filter(country == "THA" & season == "WS")
vnm.ds <- injury.profiles %>% filter(country == "VNM" & season == "DS")
vnm.ws <- injury.profiles %>% filter(country == "VNM" & season == "WS")

# create the list 
country.season.dataset <- list(ind.ds, ind.ws, idn.ds, idn.ws, 
                               phl.ds, phl.ws, tha.ds, tha.ws, vnm.ds, vnm.ws)

# create the list object to store the data
country.season.cor.mat <- list()

for (i in 1:length(country.season.dataset)) {
  
  # select out the country and season varible
  temp <- 
    country.season.dataset[[i]][!names(season_country_dataset[[i]]) %in% c("country", "season")]
  
   # generate the corrlation matrix 
  
  country.season.cor.mat[[i]] <- cooc_table(temp)
}
 
##### 3. Generate the network object  #####
source("~/Documents/Github/network.project/chapter3/R/functions/function_plot_network.R") 

country.season.net <- list()

for (i in 1:length(country.season.cor.mat)) {
  
  # keep the correlation coefficient at p.value < 0.05
  cut.table <- country.season.cor.mat[[i]] %>% filter(p.value < 0.05)

# construct the netwotk object 
country.season.net[[i]] <- plot_network(cut.table)

}
# plot(country.season)

##### 4. network wise wise properties #####
source("~/Documents/Github/network.project/chapter3/R/functions/function_node_net_stat.R")  # node_stat() and net_stat()
source("~/Documents/Github/network.project/chapter3/R/functions/function_random_graph.R") 

# name the list object
names(country.season.net) <- c("ind_ds", "ind_ws", "idn_ds", 
                               "idn_ws", "phl_ds", "phl_ws", "tha_ds", "tha_ws", "vnm_ds", 
                               "vnm_ws")

# compute the node statistic properties
node.stat.list <- sapply(country.season.net, node_stat, simplify = FALSE, 
                         USE.NAMES = TRUE)

# combine the node statistic from each network of country by season
node.df <- do.call(rbind, node.stat.list)

# add coolumn to indicate the source of node stat
node.df$country_season <- gsub("\\..*", "", row.names(node.df))

# delete row name
row.names(node.df) <- NULL

# reorder the colummn by moving column named country_season into the first
node.df <- node.df %>% select(country_season, everything())

#xtable(node.df)

## ======= network wise statistics of our empirical network

# apply net_stat function to all the object list
net.stat.list <- sapply(country.season.net, net_stat, simplify = FALSE, 
                        USE.NAMES = TRUE)
# combine the network stat from all list object
net.df <- do.call(rbind, net.stat.list)

# add column to indicate the source of data 
net.df$country_season <- gsub("\\..*", "", row.names(net.df))

# delete row name 
row.names(net.df) <- NULL

# reorder the colummn by moving column named country_season into the first
net.df <- net.df %>% select(country_season, everything())

## ======= random network for comparing our empirical network

# create round_net object to store the data
rand_net <- matrix(nrow = 0, ncol = 3)

for (i in 1:length(country.season.net)) {
  # simulate the random network using same parameter of the empirical network
  temp <- random_graph(country_season_net[[i]])
  
  # summarize the network properties of random network
  new.row <- temp %>% summarise(mclus_coef = mean(cluster_coef), 
                                mavr_path = mean(average_path))
  
  # name add column to indicate the source of network
  new.row$country_season <- names(country_season_net[i])
  
  # combine network stat of random network from all sources 
  rand_net <- rbind(rand_net, new.row)
}

# merge network stat of empirical network and random network
network.stat <- merge(net.df, rand_net)

# re arrange the column
network.stat <- network.stat[c("country_season", "Node", "Edges",  "CEN_BET", 
                               "CEN_CLO",  "CEN_DEG", "DENSITY", "DIAM",
                               "AVG_P", "mavr_path", "TRANSITIVITY", "mclus_coef")]
# print the network statisitc
# xtable(network.stat)

##### 4. detect community ####
 
# call function cluster.network 
source("~/Documents/Github/network.project/chapter3/R/functions/function_cluster.network.R") 

for(i in 1:length(country.season.net)){
  # setect the community with the function cluster.network
  graph <- cluster.network(country.season.net[[i]]) 

  # plot network graph
plot(graph, 
     edge.width = 2, 
     mark.groups = NULL, 
     vertex.frame.color = NA, 
     main = paste("Communities in", names(country_season_net[i]), "network", sep =" " )
)

}


