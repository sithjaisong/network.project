df <- data.frame(c("A", "B", "C", "C", "C", "D", "B"), 
             c("B", "C", "C", "D", "E", "E", "F")
           )

g <- graph_from_data_frame(df, directed = FALSE)
E(g)$weight <- 1

g <- simplify(g)
ug <- as.directed(g)
plot(ug)

degree(ug)
betweenness(ug)
transitivity(ug, type = "local")


E(ug)$arrow.mode <- 0
color <- c("gray60", "white")
Community <- cluster_optimal(ug)
V(ug)$color <- color[membership(Community)]
plot(ug)
