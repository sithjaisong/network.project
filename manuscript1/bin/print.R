# print country network

load(file = "~/Google Drive/surveySKEP1/networkdata.RData")

#pdf(file = "~/manuscript1/pic/net1.pdf")
pdf("network.pdf", height = 1200, width = 600) #call the png writer

par(mfrow = c(3, 2), mar = c(1, 1, 1, 2))

for(i in 1:length(country.name)){
  
  country_data <- data %>% 
    filter(country == country.name[i]) %>%
    select(-c(country, year, season))
  
  #=====
  country_data <- country_data[, apply(country_data, 2, var, na.rm = TRUE) != 0]  # exclude the column with variation = 0
  
  # constrcut the cooccurance table
  table <- cooc_table(country_data)
  
  # select only the pairs have p.adjusted < 0.05
  cut.table <- table %>% filter(p.adjusted < 0.05)
  
  #  netgraph <- 
  plot_network(cut.table)
  
  print(netgraph)
  
  # country_net[[i]] <- netgraph 
}

dev.copy(jpeg,filename ="plot.jpg", width = 1200, height = 1200)
dev.off()

dev.print(pdf, 'network.pdf')
#run the plot
dev.off()
