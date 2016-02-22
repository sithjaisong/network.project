
# function for analysing network statistics of both node and network level
node_stat <- function(graph){
  abs.graph <- graph
  E(abs.graph)$weight <- abs(E(abs.graph)$weight)
  
node.value <- data.frame(
  
    id = V(graph)$name, # node name
    
    deg = degree(graph), # degree distribution
    
    bet = betweenness(abs.graph, normalized = TRUE, directed =FALSE), #betweenness centality
    
    clo = closeness(abs.graph), #closeness centality
    
    tra = transitivity(graph, type = c("weighted")) # cluster coefficients
  )
  
  names(node.value) <- c("var", "degree", "betweenness", "closeness","clust.coef")
  
  row.names(node.value) <- NULL
  
  return(node.value)
}

################################

net_stat <- function(graph){
  
  abs.graph <- graph
  
  E(abs.graph)$weight <- abs(E(abs.graph)$weight)
  
  network.value <- data.frame(
    
    node = vcount(graph), #1.number of nodes
    
    edge = ecount(graph), #2.number of edges
    
    avr.path = average.path.length(graph),
    
    diam = diameter(abs.graph, unconnected = TRUE),
    
    cent.bet = centr_betw(abs.graph, directed = FALSE)$centralization, #3.betweenness centralization
    
    cent.clos = centr_clo(graph)$centralization, #4. closeness centralization
    
    cent.deg = centr_degree(graph)$centralization, #5. degree diacentralization
    
    density = edge_density(graph, loops = FALSE), #6. density
    
    tra = transitivity(graph, type = "global") #7. transitivity
    
  )
  
  names(network.value) <- c("Node", "Edges", "AVG_P", "DIAM", "CEN_BET", "CEN_CLO", "CEN_DEG", "DENSITY", "TRANSITIVITY")
  
  row.names(network.value) <- NULL
  
  return(network.value)
}
#eos
