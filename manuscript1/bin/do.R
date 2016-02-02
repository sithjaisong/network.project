#'Title: do.R
#'Descript:
#'
#'
#'
#'
#'
# load libraries 

libs <- c("plyr", 'dplyr', 'tidyr', 'reshape', 'reshape2',  # data manipulation
          'ggplot2', "gridExtra", "extrafont", "ggExtra", "gtable",  # visualizaation
          'igraph' # network analysis
          )
lapply(libs, library, character.only = TRUE)


#load functions
source("~/Documents/Github/network.project/manuscript1/functions/function_cooc_table.R") # cooc_table()
source("~/Documents/Github/network.project/manuscript1/functions/function_plot_network.R") # plot_network()
source("~/Documents/Github/network.project/manuscript1/functions/function_node_net_stat.R") # node_stat() and net_stat()
source("~/Documents/Github/network.project/manuscript1/functions/function_random_graph.R") # simulate random graph model from oc-occurreance network

# load data
load(file = "~/Google Drive/surveySKEP1/FORM2.RData")
  
 data <- FORM2 %>%
  separate(filename, c("SKEP", "SURVEY", "Country", "Year", "Season", "Fieldno"), "-") %>%
  transform(Fieldno = gsub(pattern = "\\.xls$", replacement = "", .$Fieldno)) %>% select(-c(SKEP, SURVEY, Fieldno))

head(data)
dim(data) # there are 27 variables, but there should be 21 variables (WA, WB, DH, GM, WM, LF, DEF, BPH, WBP, GLH, BLB, LB, BS, NBS, SHB, SHR, NB, FSM, DP)

# reamane the variables of injuries
#' WA = weed above
#' WB = weed below
#' SNL = snal
#' DH = deadheart
#' RT = rat
#' GM = gall midge
#' RB = rice bug
#' WH = white head
#' DP = dirty panicle
#' FSM = false smut
#' NB = neck blast
#' SHB = sheath blight
#' SHR = sheath rot
#' SR = stem rot
#' BPH = brown plant hopper
#' WPH = white backed plant hopper
#' AW = army worm
#' LF = leaffolder
#' WM = whorl maggot
#' BLB = bacterial leaf blight
#' BLS = bacterial leaf streak
#' BS = brown spot
#' LB = leaf blast
#' NBS = narrow brown spot
#' RS = red stripe

names(data) <- c("country", "year", "season", "WA", "WB", "SNL", "DH", "RT",  "GM", "RB", "WH", "DP",  "FSM",  "NB",  "SHB", "SHR",
                  "SR", "BPH", "WPH", "AW",   "LF",  "WM",  "BLB",  "BLS",   "BS",   "LB",  "NBS",  "RS")


data <- data[complete.cases(data), ] 

#==== Country Network =======#
country.name <- c("IDN", "IND", "PHL", "THA", "VNM")

#pdf(file = "~/manuscript1/pic/net1.pdf")
#par(mfrow = c(3, 2), mar = c(1, 1, 1, 1))
country_net <- list()

for(i in 1:length(country.name)){
  
  country_data <- data %>% filter(country == country.name[i]) %>% select(-c(country, year, season))
  
  #=====
  country_data <- country_data[, apply(country_data, 2, var, na.rm = TRUE) != 0]  # exclude the column with variation = 0
  
  # constrcut the cooccurance table
  table <- cooc_table(country_data)
  
  # select only the pairs have p.adjusted < 0.05
  cut.table <- table %>% filter(p.adjusted < 0.05)
  
  netgraph <- plot_network(cut.table)
  
#  print(netgraph)
  
 country_net[[i]] <- netgraph 
}
#pdf(filename = "network.png", onefile = TRUE) #call the png writer
#run the plot
#dev.off()

##=== These are the pest and disease injury cooccurance network 
#
names(country_net) <- country.name

names(country_net) # list object

##======= network wise statistics

# and we will find the network statistics

node.stat.list <- sapply(country_net, node_stat, simplify = FALSE, USE.NAMES = TRUE)

node.df <- do.call(rbind, node.stat.list)

node.df$country <- gsub("\\..*", "", row.names(node.df))

row.names(node.df) <- NULL

node.df$country <- as.factor(node.df$country)

##======= network wise statistics

net.stat.list <- sapply(country_net, net_stat, simplify = FALSE, USE.NAMES = TRUE)

net.df <- do.call(rbind, net.stat.list)

net.df$country <- gsub("\\..*", "", row.names(net.df))

row.names(net.df) <- NULL

# degree distribution has two groups
#1 IDN, IND and PHL
#2 THA and VNM
#==============================================

### Full co-occurance network

full.data <- data # to store the original data set
full.data$country <- NULL
full.data$year <- NULL
full.data$season <- NULL

full.net  <- full.data %>% cooc_table() %>%
  filter(p.adjusted < 0.05) 

%>%
  plot_network()

full.node.data <- node_stat(full.net)

full.net.data <- net_stat(full.net)

full.interaction <- full.data %>% cooc_table()

# save the output
save(data, table, country_net, node.df, net.df, full.interaction, file = "~/Google Drive/surveySKEP1/networkdata.RData")
