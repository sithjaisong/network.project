
load(file = "~/Google Drive/surveySKEP1/networkdata.RData")

# load library

library(ggplot2)
library(pgirmess)
library(multcompView) 

# this is for testing the degree and betweenness of network associated with geographic location

country.name <- unique(node.df$country)

for(i in 1:length(country.name)){
  out <- shapiro.test(subset(node.df, country %in% country.name[i])$degree)
  print(out) 
}

# Shapiro-Wilk normality test
# 
# data:  IDN
# W = 0.92132, p-value = 0.1051
# 
# 
# Shapiro-Wilk normality test
# 
# data:  IND
# W = 0.9353, p-value = 0.4396
# 
# 
# Shapiro-Wilk normality test
# 
# data:  PHL
# W = 0.79921, p-value = 0.003592
# 
# 
# Shapiro-Wilk normality test
# 
# data:  THA
# W = 0.87894, p-value = 0.02058
# 
# 
# Shapiro-Wilk normality test
# 
# data: VNM
# W = 0.92972, p-value = 0.1079

kruskal.test(degree ~ country, data = node.df)
# result "H = 22.291, 4 d.f., P < 0.001)

kmc <- kruskalmc(degree ~ country, data = node.df, probs = 0.05) # multiple-comparison test
print(kmc) # gives row names as comparisons and TRUE or FALSE
#wilcox.test(degree ~ country, data = count.deg, alternative = "two.sided", subset = country %in% c("IND", "VNM"))

# Multiple comparison test after Kruskal-Wallis 
# p.value: 0.05 
# Comparisons
# obs.dif critical.dif difference
# IDN-IND  5.058333     26.48150      FALSE
# IDN-PHL 20.375000     24.77118      FALSE
# IDN-THA 19.814474     23.23345      FALSE
# IDN-VNM  5.459783     22.17321      FALSE
# IND-PHL 15.316667     28.08788      FALSE
# IND-THA 24.872807     26.74156      FALSE
# IND-VNM 10.518116     25.82574      FALSE
# PHL-THA 40.189474     25.04899       TRUE
# PHL-VNM 25.834783     24.06886       TRUE
# THA-VNM 14.354691     22.48315      FALSE


test <- kmc$dif.com$difference # select logical vector
names(test) <- row.names(kmc$dif.com) # add comparison names
# create a list with "homogenous groups" coded by letter
let <- multcompLetters(test, compare = "<", threshold = 0.05,
                       Letters = c(letters, LETTERS, "."),
                       reversed = FALSE)

# IDN  IND  PHL  THA  VNM 
# "ab" "ab"  "a"  "b"  "b"

gg <- ggplot(node.df, aes(x = reorder(country, degree), y = degree)) + geom_boxplot(aes(fill = country)) + ggtitle("Degree") + ylim(0, 20)

df1 <- data.frame(a = c(1, 1, 2, 3, 4, 4, 4, 4, 5, 5), b = c(17, 17.5, 17.5, 17.5, 17.5, 17, 17, 17.5, 17.5, 17))
#df2 <- data.frame(a = c(1, 1, 2, 2, 2, 2, 3, 3), b = c(9.5, 10, 10, 9.5, 9.5, 10, 10, 9.5))
#df3 <- data.frame(a = c(2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5), b = c(14.5, 15, 15, 14.5, 14.5, 15, 15, 14.5, 14.5, 15, 15, 14.5))

gg_test <- gg + geom_line(data = df1, aes(x = a, y = b)) + annotate("text", x = 3, y = 18, label = "*", size = 8) 
#+
#     geom_line(data = df2, aes(x = a, y = b)) + annotate("text", x = 2, y = 11, label = "n.s", size = 8) +
#     geom_line(data = df3, aes(x = a, y = b)) + annotate("text", x = 3.5, y = 16, label = "n.s.", size = 8)

ggsave(gg_test, file = "./manuscript1/pic/degree_test1.pdf", width = 20, height = 15, units = "cm", dpi = 720)



#attach(node.df)
#out <- posthoc.kruskal.conover.test(x=degree, g=country, p.adjust.method="bonferroni")

############# Result ##############
# data:  degree and country 
# 
#        IDN    IND    PHL     THA   
#   IND 1.0000 -      -       -     
#   PHL 0.0970 0.8293 -       -     
#   THA 0.0741 0.0362 1.6e-05 -     
#   VNM 1.0000 1.0000 0.0087  0.4302
#####################################
#print(out$statistic)
#detach()


#===== compute the boxplot of betweenness

#okay the boxplot show that only PHL is significantly different to other country
#==============================================


 for(i in 1:length(country.name)){
   out <- shapiro.test(subset(node.df, country %in% country.name[i])$betweenness)
   print(out) 
 }
# 
# Shapiro-Wilk normality test
# 
# data:  IDN
# W = 0.79763, p-value = 0.0007966
# 
# 
# Shapiro-Wilk normality test
# 
# data:  IND
# W = 0.7976, p-value = 0.008801
# 
# 
# Shapiro-Wilk normality test
# 
# data:  PHL
# W = 0.58419, p-value = 1.83e-05
# 
# 
# Shapiro-Wilk normality test
# 
# data:  THA
# W = 0.69794, p-value = 5.254e-05
# 
# 
# Shapiro-Wilk normality test
# 
# data:  VNM
# W = 0.8435, p-value = 0.002079

kruskal.test(betweenness ~ country, data = node.df)
kmc <- kruskalmc(betweenness ~ country, data = node.df, probs = 0.05) # multiple-comparison test
print(kmc) # gives row names as comparisons and TRUE or FALSE
# result "H = 10.552, 4 d.f., P < 0.05)

# Multiple comparison test after Kruskal-Wallis 
# p.value: 0.05 
# Comparisons
# obs.dif critical.dif difference
# IDN-IND  2.8583333     26.48150      FALSE
# IDN-PHL 21.5000000     24.77118      FALSE
# IDN-THA  3.1894737     23.23345      FALSE
# IDN-VNM  4.9913043     22.17321      FALSE
# IND-PHL 18.6416667     28.08788      FALSE
# IND-THA  0.3311404     26.74156      FALSE
# IND-VNM  7.8496377     25.82574      FALSE
# PHL-THA 18.3105263     25.04899      FALSE
# PHL-VNM 26.4913043     24.06886       TRUE
# THA-VNM  8.1807780     22.48315      FALSE



test <- kmc$dif.com$difference # select logical vector
names(test) <- row.names(kmc$dif.com) # add comparison names
# create a list with "homogenous groups" coded by letter
let <- multcompLetters(test, compare = "<", threshold = 0.05,
                       Letters = c(letters, LETTERS, "."),
                       reversed = FALSE)

# IDN  IND  PHL  THA  VNM 
# "ab" "ab"  "a" "ab"  "b" 

pp <- ggplot(node.df, aes(x = reorder(country, betweenness), y = betweenness)) + 
  geom_boxplot(aes(fill = country)) + 
  ggtitle("Betweenness")

df1 <- data.frame(a = c(1, 1, 3, 5, 5), b = c(59.5, 60, 60, 60, 59.5))

pp_test <- pp + geom_line(data = df1, aes(x = a, y = b)) + annotate("text", x = 3, y = 61, label = "*", size = 8) 

ggsave(pp_test, file = "./manuscript1/pic/betweenness_test1.pdf", width = 20, height = 15, units = "cm", dpi = 720)
plot_grid(gg_test, pp_test, labels = c("A", "B"), align = "h")

#=== NOTE

# for the multiple comparison test we can use the package pgirmess



