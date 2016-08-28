```{r}
##### construct the correlation matrix ######

all.pearson <- cor(all, method = "pearson", use = "pairwise") # pearson correlation

all.spearman <- cor(all, method = "spearman", use = "pairwise") # spearman correlation

all.kendall <- cor(all, method = "kendall", use = "pairwise")# kendall correlation

all.biweight <- bicor(all, use = "pairwise") # Biweight Midcorrelation from WGCNA package


#===========================================
##### Transform the correlation matrix #####
#============================================

# change from matrix to data frame, and extract the value of each correlation approach

### Peason correlation
df.pearson <- as.data.frame(all.pearson)
df.pearson.corval <- df.pearson[1]
colnames(df.pearson.corval) <- "Pearson"

### Spearman correlation

df.spear <- as.data.frame(all.spearman)
df.spear.corval <- df.spear[1]
colnames(df.spear.corval) <- "Spearman"

### Kendall correlation

df.kendall <- as.data.frame(all.kendall)
df.kendall.corval <- df.kendall[1]
colnames(df.kendall.corval) <- "Kendall"

### Biweight Midcorrelation
df.biweight <- as.data.frame(all.biweight)
df.biweight.cor.val <- df.biweight[1]
colnames(df.biweight.cor.val) <- "Biweight"

#====================================================
##### Combine correlation value of each method ######
#===================================================
# will add more correlation
bind.cor <- cbind(df.pearson.corval,
                  df.spear.corval,
                  df.kendall.corval,
                  df.biweight.cor.val)

##### Cluster Analysis and correlation matrix #####
cor.of.cor <- cor(bind.cor)
pheatmap(cor.of.cor, cellwidth = 50, cellheight = 50, fontsize = 16)