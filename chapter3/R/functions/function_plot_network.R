plot_network <- function(table){
  
  #== adjust the vertices properties
  
  net <- graph.edgelist(as.matrix(table[ ,c("var1","var2")]), directed = FALSE)
  
  V(net)$color <- adjustcolor("khaki2", alpha.f = .8)
  
  V(net)$frame.color <- adjustcolor("khaki2", alpha.f = .8)
  
  V(net)$shape <- "circle"
  
  V(net)$size <- 25
  
  V(net)$label.color <- "black"
  
  V(net)$label.font <- 2
  
  V(net)$label.family <- "Helvetica"
  
  V(net)$label.cex <- 1.0
  
  # == adjust the edge proterties
  
  #E(net)$label <- round(table$rho, 2)
  
  E(net)$weight <- as.matrix(table[, "rho"])
  
  E(net)$width <- abs(E(net)$weight) * 10
  
  col <- c("orangered", "steelblue2")
  
  colc <- cut(table$rho, breaks = c(-1, 0, 1), include.lowest = TRUE)
  
  E(net)$color <- col[colc]
  
#  minC <- rep(-Inf, vcount(net))

#  maxC <- rep(Inf, vcount(net))

#  minC[1] <- maxC[1] <- 0
  co <- layout_with_fr(net)
  #, minx = minC, maxx = maxC, miny =minC, maxy = maxC)

  net$layout <- co

  # == plot network model == #
 # netgraph <- plot.igraph(net, layout = co, rescale = FALSE, xlim = range(co[,1]), ylim = range(co[,2]), margin = -0.25)
  
  return(net)
}



