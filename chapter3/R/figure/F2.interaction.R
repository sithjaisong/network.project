

library(ggExtra)

rho.value <- full.interation$rho
rho.value <- as.data.frame(rho.value)
rho.value$correlation <- ifelse(rho.value[1] < 0, "negative", "positive")
rho.value$rho.value <- abs(rho.value$rho.value)
names(rho.value) <- c("rho", "correlation")
rho.value$rho <- as.numeric(rho.value$rho)
rho.value$correlation <- as.factor(rho.value$correlation)


# use rho.value
out <- wilcox.test(rho ~ correlation, data = rho.value)
out$statistic
# Wilcoxon rank sum test with continuity correction
# 
# data:  rho by correlation
# W = 9066, p-value = 0.02051
# alternative hypothesis: true location shift is not equal to 0
# positive more than negative


P1 <- ggplot(rho.value, aes(x = rho, fill = correlation)) + 
  geom_histogram(position ="identity", alpha = 0.5, binwidth = 0.05, bins = 20) + 
  scale_x_continuous(expand = c(0, 0)) + 
  #  scale_y_continuous(expand = c(0, 0)) + 
  #  expand_limits(y = c(min(mtcars$hp) - 0.1 * diff(range(mtcars$hp)), 
  #                      max(mtcars$hp) + 0.1 * diff(range(mtcars$hp)))) + 
  expand_limits(x = c(0,1), y = c(0, 50)) 
+ theme(plot.margin = unit(c(0.2, 0.2, 0.5, 0.5), "lines"))



# Horizontal marginal boxplot - to appear at the top of the chart
P2 <- ggplot(rho.value, aes(x = correlation, y = rho,  fill = correlation)) + 
  geom_boxplot(outlier.colour = NA, alpha = 0.5) + 
  geom_jitter(position = position_jitter(width = 0.05)) + p
  expand_limits(y = c(0, 1)) + coord_flip() + 
  theme(axis.text = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks = element_blank()
        , plot.margin = unit(c(1, 0.2, -1.5, 0.5), "lines")
  )

P2
# Assembly part
Gt1 <- ggplot_gtable(ggplot_build(P1)) # turn ggplot onject to the gtable object
Gt2 <- ggplot_gtable(ggplot_build(P2))

maxWidth <- unit.pmax(Gt1$widths[2:3], Gt2$widths[2:3])

Gt1$widths[2:3] <- as.list(maxWidth)
Gt2$widths[2:3] <- as.list(maxWidth)

# create gtable 2 rows with 1 column

Gt <- gtable(widths = unit(c(1), "null"), height = unit(c(1, 7), "null"))

#gtable_show_layout(G1)

Gt <- gtable_add_grob(Gt, Gt1, 2, 1) # place Gt1 (histogram) at the bottom 
Gt <- gtable_add_grob(Gt, Gt2, 1, 1) # place Gt2 (boxplot) at the top

grid.newpage() # clear space
grid.draw(Gt) # plot

ggsave(Gt, file = "combinedplot.pdf")
