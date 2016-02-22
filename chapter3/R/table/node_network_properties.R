## ======= network wise statistics

# and we will find the network statistics


names(country_season_net) <- c("ind_ds", "ind_ws", "idn_ds", 
                               "idn_ws", "phl_ds", "phl_ws", "tha_ds", "tha_ws", "vnm_ds", 
                               "vnm_ws")

node.stat.list <- sapply(country_season_net, node_stat, simplify = FALSE, 
                         USE.NAMES = TRUE)

node.df <- do.call(rbind, node.stat.list)

node.df$country_season <- gsub("\\..*", "", row.names(node.df))

row.names(node.df) <- NULL

node.df$country_season <- as.factor(node.df$country_season)

## ======= network wise statistics

net.stat.list <- sapply(country_season_net, net_stat, simplify = FALSE, 
                        USE.NAMES = TRUE)

net.df <- do.call(rbind, net.stat.list)

net.df$country_season <- gsub("\\..*", "", row.names(net.df))

row.names(net.df) <- NULL


net.df <- net.df %>% select(country_season, everything())
xtable(net.df)


## ======= random network

rand_net <- matrix(nrow = 0, ncol = 3)

for (i in 1:length(country_season_net)) {
  
  temp <- random_graph(country_season_net[[i]])
  
  new.row <- temp %>% summarise(mclus_coef = mean(cluster_coef), 
                                mavr_path = mean(average_path))
  
  new.row$country_season <- names(country_season_net[i])
  
  rand_net <- rbind(rand_net, new.row)
}

network_data <- merge(net.df, rand_net)
#network_data <- network_data[-14]

network_data <- network_data[c("country_season", "Node", "Edges",  "CEN_BET", "CEN_CLO", "CEN_EIG", "DG_ASSORT", "CEN_DEG", "DENSITY", "AVG_P", "mavr_path", "TRANSITIVITY", "mclus_coef")]

xtable(network_data)
