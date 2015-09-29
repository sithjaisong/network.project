library(bioDist)
library(vegan)
library(mvtnorm)
library(reshape)
library(ggplot2)
library(igraph)

results_sub <- subset(results, as.numeric(as.character(rho)) > 0.25)

head(results_sub)
g1 <- graph.edgelist(as.matrix(subset(results_sub, trt== "PHL_1")[,2:3]), directed=FALSE)
g2 <- graph.edgelist(as.matrix(subset(results_sub, trt== "IND_2")[,2:3]), directed=FALSE)

V(g1)$color<-"red"
V(g2)$color<-"blue"

E(g1)$color<-"black"
E(g2)$color<-"black"

V(g1)$label <- T
V(g2)$label <-NA

V(g1)$size <- 30
V(g2)$size < -30

par(mfrow=c(1,2))
plot(g1, layout = layout.fruchterman.reingold)

, vertex.shapre = "none", vertex.label = V(g1))
plot(g2,layout=layout.circle)


#making the spearman's distance matrix
IP.dist <- spearman.dist(data.matrix(cluster.IP.data[,-1]))

##### the test below is the PERMANOVA outlined in the text #####
adonis(IP.dist ~ cluster.IP.data$groups, permutations=9999)

#making figures for figure 1#
#head(community)

# ggplot(cluster.IP.data) + 
#   theme_bw() + 
#   facet_wrap(~groups, scales="free") +
#   geom_smooth(aes(x = X4, y = X6, colour = Ecosystem, size=.5), se = FALSE) +
#   theme(aspect.ratio=1)+ 
#   geom_point(aes(x=X4,y=X6,colour=Ecosystem), 
#                                    size=3,alpha=0.5)+ 
#   theme(axis.text.x = element_text(size = 15),
#         axis.text.y = element_text(size = 15),
#         axis.title.x = element_text(size=15),
#         axis.title.y = element_text(size=15))+
#   theme(legend.title = element_text(size=15),
#         legend.text = element_text(size=15))+
#   scale_colour_manual(values = c("red","blue"))+
#   labs(x = "Microbe 4", y = "Microbe 6")


mds <- monoMDS(IP.dist)
#head(cocur)
plot(mds)
# dim(cocur)
# ggplot.NMDS(mds, cluster.IP.data$groups, c("red","blue"))
# 
# 
# ###simulations for Supplementary Material#
# covariance<-seq(1,0,-.01)
# results<-matrix(nrow=0,ncol=3)
# for(i in 1:length(covariance)){
#   xx<-covariance[i]
#   for(j in 1:100){
#     sig<-matrix(cbind(1,xx,xx,
#                       xx,1,xx,
#                       xx,xx,1),ncol=3)	
#     
#     community<-cbind(
#       rbind(abs(rmvnorm(100, mean=rep(10,3),sigma=sig)), 			   													cbind(abs(rnorm(100,10,10)),abs(rnorm(100,10,10)),abs(rnorm(100,10,10)))),
#       rbind(cbind(abs(rnorm(100,10,10)),abs(rnorm(100,10,10)),abs(rnorm(100,10,10))),abs(rmvnorm(100, 				mean=rep(10,3),sigma=sig)))
#     )
#     
#     Ecosystem<-c(rep("A",100),rep("B",100))
#     Samples<-as.factor(seq(1,200,1))
#     community<-data.frame(Samples,Ecosystem, community)
#     
#     community.melt<-melt(community, id=c("Samples","Ecosystem"))
#     
#     X1<-cast(subset(community.melt, Ecosystem=="A"), Ecosystem+variable~Samples, value="value")
#     X2<-cast(subset(community.melt, Ecosystem=="B"), Ecosystem+variable~Samples, value="value")
#     colnames(X2)<-colnames(X1)
#     
#     cocur<-data.frame(rbind(X1,X2))
#     
#     cocur.dist<-spearman.dist(data.matrix(cocur[,-c(1:2)]))
#     
#     
#     object<-adonis(cocur.dist~cocur$Ecosystem, permutations=9999)
#     Rsq<-object$aov.tab$R2[1]
#     p.value<-as.matrix(object$aov.tab[6])[1]
#     new.row<-c(covariance[i],Rsq,p.value)
#     results<-rbind(results,new.row)
#   }
#   print(i/length(covariance))
# }
# 
# ###looking at results
# 
# data<-read.csv(file.choose(), header=TRUE)
# head(data)
# data<-data[,-1]
# 
# names(data)<-c("correlation","Rsq","p.value")
# head(data)
# library(ggplot2)
# data<-data.frame(data)
#
# ggplot(data, aes(correlation, p.value))+geom_point(alpha=0.5)+theme_bw()+geom_hline(yintercept=0.05, colour="red",size=2)+scale_y_log10(breaks=c(0.0001,0.001,0.01,0.05,.1,1))+theme(aspect.ratio=1)
# 
# ggplot(data, aes(correlation, Rsq))+geom_point(alpha=0.5)+theme_bw()+theme(aspect.ratio=1)
# 
# ggplot(data, aes(p.value, Rsq))+geom_point(alpha=0.5)+theme_bw()+theme(aspect.ratio=1)+geom_vline(xintercept=0.05, colour="red",size=2)+scale_x_log10(breaks=c(0.0001,0.001,0.01,0.05,.1,1))