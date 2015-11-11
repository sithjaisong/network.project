####==== 2.clean ====####
#
#
##### Load library #####
library(dplyr)
library(psych)
library(reshape2)
library(ggplot2)
library(cowplot)

c.data <- data

c.data[c.data == "-"] <- NA # replace '-' with NA

c.data[c.data == ""] <- NA # replace 'missing c.data' with NA

names(c.data) <- tolower(names(c.data)) # for more consistancy

# we will subset the data by countr





##### Remove columns related to the production situation

c.data <- c.data[, -c(1,3, 5:45)]

##### remove un-important variable
#====================================
###### insect pest injuries
 c.data$lma <- NULL # remove leaf miner
 c.data$rha <- NULL # remove rice hispa
 c.data$thrx <- NULL # remove thrips
 c.data$pmx <- NULL # remove panicle mite
# c.data$defa <- NULL # remove defoliators
 c.data$awx <- NULL # remove army worms
# c.data$rbx <- NULL # remove rice bug
# c.data$rbbx <- NULL # remove rice black bug
# c.data$stbx <- NULL # remove stink bug
 c.data$hbx <- NULL # remove bugburn
 c.data$bbx <- NULL # remove bugburn

 ####### diseases
# c.data$rsa <- NULL # remove red stripe
# c.data$lsa <- NULL # remove leaf scald
 c.data$gsdx <- NULL # remove 
 c.data$rbpx <- NULL 
#=====================================

##### corract the type of varibles

 injury <- apply(c.data[-c(1,2)], 2, as.numeric)
 c.data <- cbind(c.data[c(1,2)], injury)
 c.data <- as.tbl(c.data)
 

 c.data <- c.data %>% transform(country = as.factor(country), waa = as.numeric(waa), 
                                wba = as.numeric(wba), dhx = as.numeric(dhx), whx = as.numeric(whx), 
                                ssx = as.numeric(ssx), wma = as.numeric(wma), lfa = as.numeric(lfa), 
                                defa = as.numeric(defa), bphx = as.numeric(bphx), wbpx = as.numeric(wbpx), 
                                rbx = as.numeric(rbx), rbbx = as.numeric(rbbx), glhx = as.numeric(glhx), 
                                stbx = as.numeric(stbx), blba = as.numeric(blba), lba = as.numeric(lba), 
                                bsa = as.numeric(bsa), blsa = as.numeric(blsa), nbsa = as.numeric(nbsa), 
                                rsa = as.numeric(rsa), lsa = as.numeric(lsa), shbx = as.numeric(shbx), 
                                shrx = as.numeric(shrx), srx = as.numeric(srx), fsmx = as.numeric(fsmx), 
                                nbx = as.numeric(nbx), dpx = as.numeric(dpx), rtdx = as.numeric(rtdx), 
                                rsdx = as.numeric(rsdx), rtx = as.numeric(rtx)
                                )
 
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
