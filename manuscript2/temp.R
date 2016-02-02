THA <- c.data %>% filter(country == "THA")

c.data <- c.data %>% mutate(ph_n = bphx + wbpx + glhx,
                            stnk_n = rbx + stbx)

# next remove the variables named bphx, wbpx, glhx, rbx, stbx

c.data <- c.data %>% select(-c(bphx, wbpx, glhx, rbx, stbx))

#### subset the country to check the quality of the data

#####################
##### Indonesia #####
#####################


Indo <- c.data %>% filter(country == "IDN")

Indo <- Indo[ , apply(Indo[, -c(1,2)], 2, var, na.rm = TRUE) != 0] # exclude the column with variation = 0

Indo <- Indo[complete.cases(Indo), ] # exclude row which cantain NA

describe(Indo)

m.Indo <- melt(Indo[, !names(Indo) %in% c("fno", "country")])

varnames <- colnames(Indo[, !names(Indo) %in% c("fno", "country")])

p <- list()

for(i in 1:length(varnames)) {
  
  gdata <- m.Indo %>% filter(variable == varnames[i])
  p[[i]] <- ggplot(gdata, aes(x = value)) + 
    geom_histogram(stat = "bin") + ggtitle(paste("Histogram of", varnames[i], sep = " "))
  
}

plot_grid(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], p[[6]], p[[7]], p[[8]], p[[9]], p[[10]],
          p[[11]], p[[12]], p[[13]], p[[14]], p[[15]], p[[16]], p[[17]], p[[18]], p[[19]], p[[20]],
          p[[21]], p[[22]], p[[23]], p[[24]], p[[25]], p[[26]], p[[27]], p[[28]], p[[29]], p[[30]],
          p[[31]], p[[32]], ncol = 3, align = "v")


#####################
##### India     #####
#####################

India <- c.data %>% filter(country == "IND")

#####################
##### Indonesia #####
#####################

Phil <- c.data %>% filter(country == "PHL")
Tha <- c.data %>% filter(country == "THA")
Vnm <- c.data %>% filter(country == "VNM")