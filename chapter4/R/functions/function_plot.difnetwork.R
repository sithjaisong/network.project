plot_difnetwork <- function(net){
  
  V(net)$color <- adjustcolor("khaki2", alpha.f = .8)
  
  V(net)$frame.color <- adjustcolor("khaki2", alpha.f = .8)
  
  V(net)$shape <- "circle"
  
  V(net)$size <- 25
  
  V(net)$label.color <- "black"
  
  V(net)$label.font <- 2
  
  V(net)$label.family <- "Helvetica"
  
  V(net)$label.cex <- 1.0
  
  E(net)$width <- 5
  E(net)[weight == 0]$width <- 0
  E(net)[weight > 0 ]$color <-"orangered4"
  E(net)[weight < 0]$color <-"royalblue4"
  
  return(net)
}
