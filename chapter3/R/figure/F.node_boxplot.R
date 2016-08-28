new.reform <- node.df %>% melt(id.vars = c("var", "group"), 
                               variable.name = "Node_Measure",
                               value.name = "Score") %>%
  separate(country_season, c("country", "season"), "_")

#node_prop_boxplot <- 
  
  
  
ggplot(new.reform, aes(x = season, y = value)) + geom_boxplot() + facet_wrap(country ~ variable, scales = 'free', ncol = 4)

#ggsave(node_prop_boxplot, filename = "./chapter3/results/plots/node_prop_boxplot.pdf")
