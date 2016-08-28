
cluster.heatmap.node.properties <- function(graph){
  
  cen <- centrality_auto(graph)$node.centrality
  cen$node <- row.names(cen)
  cen$CC <- transitivity(graph, type = "local", isolates = "zero")
  
  # plot heat map with dedrogram
 
   pheatmap(t(cen[,-4]), 
           cluster_rows = FALSE, 
           clustering_distance_rows = "daisy", 
           clustering_method = "average", 
           annotation_names_col = TRUE,
           legend = FALSE)
  
}
