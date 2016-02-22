
cluster.network <- function(graph){
  
  node <- as.character(V(graph)$name)
  
  Community <- cluster_walktrap(graph, weights = abs(E(graph)$weight), steps = 2)
  
  prettyColors <- brewer.pal(n = 8, name = 'Set2')
  
  V(graph)$color <- prettyColors[membership(Community)]
  
  V(graph)$size <- 15
  
  return(graph)
}