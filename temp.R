

mat <- as_adjacency_matrix(country.season.net[[6]] %>% delete.vertices("RT") %>% delete.vertices("RS")  %>% delete.vertices("SHR"), attr="weight" )
mat <- as.matrix(mat)

net <- graph_from_adjacency_matrix(mat, mode = "undirected", weighted = TRUE)
     
     
     V(net)$color <- adjustcolor("khaki2", alpha.f = .8)
     
     V(net)$frame.color <- adjustcolor("khaki2", alpha.f = .8)
     
     V(net)$shape <- "circle"
     
     V(net)$size <- 25
     
     V(net)$label.color <- "black"
     
     V(net)$label.font <- 2
     
     V(net)$label.family <- "Helvetica"
     
     V(net)$label.cex <- 1.0
     
     
     col <- c("orangered", "steelblue2")
     
     colc <- cut(E(net)$weight, breaks = c(-1, 0, 1), include.lowest = TRUE)
     
     E(net)$color <- col[colc]
     
     #  minC <- rep(-Inf, vcount(net))
     
     #  maxC <- rep(Inf, vcount(net))
     
     #  minC[1] <- maxC[1] <- 0
     co <- layout_with_fr(net)
     #, minx = minC, maxx = maxC, miny =minC, maxy = maxC)
     
     net$layout <- co

# graph number 6
