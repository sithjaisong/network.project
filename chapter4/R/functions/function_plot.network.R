plot_network <- function(net){
  
  V(net)$color <- adjustcolor("khaki2", alpha.f = .8)
  
  V(net)$frame.color <- adjustcolor("khaki2", alpha.f = .8)
  
  V(net)$shape <- "circle"
  
  V(net)$size <- 25
  
  V(net)$label.color <- "black"
  
  V(net)$label.font <- 2
  
  V(net)$label.family <- "Helvetica"
  
  V(net)$label.cex <- 1.0
  
  E(net)$width <- abs(E(net)$weight)*5
  
  col <- c("orangered", "steelblue2")
  
  colc <- cut(E(net)$weight, breaks = c(-1, 0, 1), include.lowest = TRUE)
  
  E(net)$color <- col[colc]
  
  return(net)
}
