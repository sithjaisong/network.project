

ind.ds.cor <- country.season.cor.mat[[1]]
idn.ds.cor <- country.season.cor.mat[[3]]
phl.ds.cor <- country.season.cor.mat[[5]]
tha.ds.cor <- country.season.cor.mat[[7]]
vnm.ds.cor <- country.season.cor.mat[[9]]
ind.ds.cor$group <- "ind.ds"
idn.ds.cor$group <- "idn.ds"
phl.ds.cor$group <- "phl.ds"
tha.ds.cor$group <- "tha.ds"
vnm.ds.cor$group <- "vnm.ds"

dry.country.cor <- rbind(ind.ds.cor, idn.ds.cor, phl.ds.cor, tha.ds.cor, vnm.ds.cor)

fit <- aov(rho ~ group, data = dry.country.cor)

test <- TukeyHSD(fit)

test_letters <- multcompLetters(test$group[,4])

test_letters

# ===== for contruct the Venn diagram
pair.all <- ind.ds.cor %>% dplyr::select(var1, var2) %>% mutate(pair = paste(var1, var2, sep = "_"))

ind.ds.cor.pair  <- ind.ds.cor %>% filter(p.value < 0.05) %>% dplyr::select(var1, var2) %>% mutate(pair = paste(var1, var2, sep = "_"))

idn.ds.cor.pair <- idn.ds.cor %>% filter(p.value < 0.05) %>% dplyr::select(var1, var2) %>% mutate(pair = paste(var1, var2, sep = "_"))

phl.ds.cor.pair <- phl.ds.cor %>% filter(p.value < 0.05) %>% dplyr::select(var1, var2) %>% mutate(pair = paste(var1, var2, sep = "_"))

tha.ds.cor.pair <- tha.ds.cor %>% filter(p.value < 0.05) %>% dplyr::select(var1, var2) %>% mutate(pair = paste(var1, var2, sep = "_"))

vnm.ds.cor.pair <- vnm.ds.cor %>% filter(p.value < 0.05) %>% dplyr::select(var1, var2) %>% mutate(pair = paste(var1, var2, sep = "_"))


pair.low <- ifelse(pair.yield.all[3]$pair %in% pair.yield.low[3]$pair == TRUE, 1, 0)
pair.medium <- ifelse(pair.yield.all[3]$pair %in% pair.yield.medium[3]$pair == TRUE, 1, 0)
pair.high <- ifelse(pair.yield.all[3]$pair %in% pair.yield.high[3]$pair == TRUE, 1, 0)



