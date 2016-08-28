
cluster.network <- function(graph){
  
  node <- as.character(V(graph)$name)
  
 Community <- cluster_optimal(graph, weights = abs(E(graph)$weight))
 #Community <- page_rank(graph)
  
  prettyColors <- brewer.pal(n = 8, name = 'Set2')
  
  V(graph)$color <- prettyColors[membership(Community)]
  
  V(graph)$size <- 15
  
  return(graph)
}


cluster_optimal(country.season.net[[6]], weights = abs(E(country.season.net[[6]])$weight))

