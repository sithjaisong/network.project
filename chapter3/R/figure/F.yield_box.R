

yield.data <- FORM1 %>% select(Country, Season, yld.area1, yld.area2, yld.area3) %>% gather(area, yield, yld.area1:yld.area3) %>% mutate(area = gsub("yld.area", "", area))

yield.data$group <- paste(yield.data$Country, yield.data$Season, sep = "_")

ggplot(yield.data, aes(x = Season, y = yield, fill = Season)) + geom_boxplot() + stat_summary(fun.y = mean, color = "darkred", geom = "point", shape = 18, size =3) + facet_grid(~ Country)

 



