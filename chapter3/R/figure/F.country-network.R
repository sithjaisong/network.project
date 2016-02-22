# start with data 

head(data)
country_net <- list()

for(i in 1:length(unique(data$country))){
  
  country_data <- data %>% filter(country == unique(data$country)[i]) %>% select(-c(country, year, season))
  
  #=====
  country_data <- country_data[, apply(country_data, 2, var, na.rm = TRUE) != 0]  # exclude the column with variation = 0
  
  # constrcut the cooccurance table
  table <- cooc_table(country_data)
  
  # select only the pairs have p.adjusted < 0.05
  cut.table <- table %>% filter(p.adjusted < 0.05)
  
  netgraph <- plot_network(cut.table)
  
  #  print(netgraph)
  
  country_net[[i]] <- netgraph 
}

layout(matrix(c(1, 2), nrow = 1, ncol = 2, byrow=TRUE))
pdf("./manuscript1/pic/country-net.pdf", onefile = T)

for(i in 1:2){
  plot(country_net[[5]], layout = layout_with_fr)
}

dev.off()
do.call(f, country_net)
