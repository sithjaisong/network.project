plot.node.centrality <- function(graph){
  
cen <- centrality_auto(graph)$node.centrality
cen$node <- row.names(cen)
row.names(cen) <- NULL

p1 <- cen %>% ggplot(aes(x= Degree, y = reorder(node, Degree))) +
  geom_point(size = 3, color ="red") +
  theme_bw() +
  theme(panel.grid.major.x =  element_blank(),
        panel.grid.minor.x =  element_blank(),
        panel.grid.major.y = element_line(color = "grey", linetype = 3)) +
  xlab("Node degree") + 
  ylab("Variables")  


p2 <- cen %>% ggplot(aes(x= Closeness, y = reorder(node, Closeness))) + 
  geom_point(size = 3, color ="blue") +
  theme_bw() +
  theme(panel.grid.major.x =  element_blank(),
        panel.grid.minor.x =  element_blank(),
        panel.grid.major.y = element_line(color = "grey", linetype = 3)) +
  xlab("Closeness") + 
  ylab("Variables")  

p3 <- cen %>% ggplot(aes(x= Betweenness, y = reorder(node, Betweenness))) + 
  geom_point(size = 3, color ="yellow") +
  theme_bw() +
  theme(panel.grid.major.x =  element_blank(),
        panel.grid.minor.x =  element_blank(),
        panel.grid.major.y = element_line(color = "grey", linetype = 3)) +
  xlab("Betweenness") + 
  ylab("Variables")  


plot_grid(p1, p2, p3, labels=c("A", "B", "C"), ncol = 3, nrow = 1 )

}