
# function for analysing network statistics of both node and network level
node_stat <- function(graph){
  abs.graph <- graph
  E(abs.graph)$weight <- abs(E(abs.graph)$weight)
  
node.value <- data.frame(
  
    id = V(graph)$name, # node name
    
    deg = degree(graph), # degree distribution
    
    eig = evcent(graph)$vector, # eginvector centality
    
    bet = betweenness(abs.graph, directed =FALSE), #betweenness centality
    
    clo = closeness(abs.graph), #closeness centality
    
    tra = transitivity(graph, type = c("weighted")) # cluster coefficients
  )
  
  node.value$res <- as.vector(lm(eig ~ bet, data = node.value)$residuals)
  
  names(node.value) <- c("var", "degree", "eigen", "betweenness", "closeness","clust.coef", "residue")
  
  row.names(node.value) <- NULL
  
  return(node.value)
}


# avr.nr.neighb.deg = knn(graph)$knn #1.average neartest neighbor degree
####
####
####
####


net_stat <- function(graph){
  
  abs.graph <- graph
  
  E(abs.graph)$weight <- abs(E(abs.graph)$weight)
  
  network.value <- data.frame(
    
    avr.path = average.path.length(graph), #2.average path
    
    cent.bet = centr_betw(abs.graph, directed = FALSE)$centralization, #3.betweenness centralization
    
    cent.clos = centr_clo(graph)$centralization, #4. closeness centralization
    
    cen.eig = centr_eigen(graph, directed = FALSE)$centralization, #5. eigenvector centrailization
    
    cent.deg = centr_degree(graph)$centralization, #6. degree centralization
    
    deg.assort = assortativity_degree(graph, directed = FALSE), #7. degree assortavivity
    
    density = edge_density(graph, loops = FALSE), #8. density
    
    tra = transitivity(graph, type = "global") # transitivity
    
  )
  
  names(network.value) <- c("AVG_P", "CEN_BET", "CEN_CLO","CEN_EIG","DG_ASSORT", "CEN_DEG", "DENSITY", "TRANSITIVITY")
  
  row.names(network.value) <- NULL
  
  return(network.value)
}

