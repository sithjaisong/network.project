library(qgraph)
plot.node.centrality <- function(graph){
  
cen <- centrality_auto(graph)$node.centrality
cen$node <- row.names(cen)
cen$CC <- igraph::transitivity(graph, type = "local", isolates = "zero")
row.names(cen) <- NULL

p1 <- cen %>% ggplot(aes(x= Degree, y = node)) +
  geom_point(size = 3, color ="red") +
  theme_bw() +
  theme(panel.grid.major.x =  element_blank(),
        panel.grid.minor.x =  element_blank(),
        panel.grid.major.y = element_line(color = "grey", linetype = 3)) +
  xlab("Node degree") + 
  ylab("Node")    


p2 <- cen %>% ggplot(aes(x= CC, y = node)) + 
  geom_point(size = 3, color ="blue") +
  theme_bw() +
  theme(panel.grid.major.x =  element_blank(),
        panel.grid.minor.x =  element_blank(),
        panel.grid.major.y = element_line(color = "grey", linetype = 3),
        axis.title.y = element_blank()) +
  xlab("Clustering Coef") 

p3 <- cen %>% ggplot(aes(x= Betweenness, y = node)) + 
  geom_point(size = 3, color = "black") +
  theme_bw() +
  theme(panel.grid.major.x =  element_blank(),
        panel.grid.minor.x =  element_blank(),
        panel.grid.major.y = element_line(color = "grey", linetype = 3),
        axis.title.y = element_blank()) +
  xlab("Betweenness")

plot_grid(p1, p2, p3, labels=c("A", "B", "C"), ncol = 3, nrow = 1 )

}