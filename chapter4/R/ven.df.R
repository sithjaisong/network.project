# Venn



library(colorfulVennPlot)
library(RColorBrewer)

y <- c(67,78,0,84,0,4,2)
names(y) <- c("001","010","011","100","101","110","111")
labels <- c("Low yield","High yield","Medium yield")
plot.new()
plotVenn3d(y, labels, Colors = brewer.pal(7, "Set3"))

