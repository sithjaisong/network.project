#############################################################
#
# R script for data analysis as presented in Figures:
# - Fig : Boxplot of each of injury variables 
#
# files needed: 
# - data from do.R
# 
#############################################################

n.data <- data %>% transform(
  country = as.factor(country),
  year = as.factor(year),
  season = as.factor(season)
)

# I created the compared the variable by country by season
# reshape
n.data$year <- NULL # remove column year

re.n.data <- melt(n.data)

varname <- unique(as.character(as.factor(re.n.data$variable)))

p <- list()

for (i in 1:length(varname)) {
  
 p[[i]] <- ggplot(re.n.data %>% filter(variable == varname[i]), aes(x = country, y = value, fill = season, color = season )) +
    geom_boxplot() + theme(legend.position = "none") +
    scale_fill_manual(values = c("WS" = "steelblue1", "DS" = "indianred1")) + 
    scale_color_manual(values = c("WS" = "steelblue4", "DS" = "indianred4")) +
    ggtitle(paste(varname[i]))
  
}

ps <- do.call("grid.arrange", p) 

ggsave(file = "./manuscript1/pic/datasets.pdf", ps, dpi = 300)

