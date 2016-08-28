library(VennDiagram)
library(RColorBrewer)
pair.yield.all <- level.yield.IP.cor.mat[[1]] %>% dplyr::select(var1, var2) %>% mutate(pair = paste(var1, var2, sep = "_"))

pair.yield.low <- level.yield.IP.cor.mat[[1]] %>% filter(p.value < 0.05) %>% dplyr::select(var1, var2) %>% mutate(pair = paste(var1, var2, sep = "_"))

pair.yield.medium <- level.yield.IP.cor.mat[[2]] %>% filter(p.value < 0.05) %>% dplyr::select(var1, var2) %>% mutate(pair = paste(var1, var2, sep = "_"))

pair.yield.high <- level.yield.IP.cor.mat[[3]] %>% filter(p.value < 0.05) %>% dplyr::select(var1, var2) %>% mutate(pair = paste(var1, var2, sep = "_"))


pair.low <- ifelse(pair.yield.all[3]$pair %in% pair.yield.low[3]$pair == TRUE, 1, 0)
pair.medium <- ifelse(pair.yield.all[3]$pair %in% pair.yield.medium[3]$pair == TRUE, 1, 0)
pair.high <- ifelse(pair.yield.all[3]$pair %in% pair.yield.high[3]$pair == TRUE, 1, 0)

intersect <- cbind(pair.low, pair.medium, pair.high)

grid.newpage()
venn.plot <- draw.triple.venn(
area1 = nrow(subset(intersect, pair.low ==1)),
area2 = nrow(subset(intersect, pair.medium ==1)),
area3 = nrow(subset(intersect, pair.high ==1)),
n12 = nrow(subset(intersect, pair.low == 1 & pair.medium == 1)),
n23 = nrow(subset(intersect, pair.medium == 1 & pair.high == 1)),
n13 =nrow(subset(intersect, pair.low == 1 & pair.high == 1)),
n123 = nrow(subset(intersect, pair.low == 1  & pair.medium == 1 & pair.high == 1)),
  category = c("Low", "Medium", "High"),
  fill = brewer.pal(3, "Accent"), 
  lty = "blank",
  cex = 2,
  cat.cex = 2,
  cat.col = c("black", "black", "black")
)
grid.draw(venn.plot)
grid.newpage()