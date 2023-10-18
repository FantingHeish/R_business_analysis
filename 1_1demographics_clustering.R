seg.df<-read.csv("1_demographics.csv",stringsAsFactors=TRUE)
summary(seg.df)
head(seg.df, n=8)
head(seg.df) 
summary(seg.df, digits = 2)
seg.df$gender<-ifelse(seg.df$Gender=="2",0,1) 
seg.dist <- dist(seg.df) 
as.matrix(seg.dist)[1:5, 1:5]
seg.hc <- hclust(seg.dist, method="complete") 
plot(seg.hc) 
plot(cut(as.dendrogram(seg.hc), h = 8)$lower[[3]]) 
seg.df[c(96, 220),] 
seg.df[c(141, 256),] 
seg.df[c(96, 147),] 
plot(seg.hc) 
rect.hclust(seg.hc, k=4, border = "red") 
seg.hc.segment <- cutree(seg.hc, k=4) 
table(seg.hc.segment) 
library(cluster) 
clusplot(seg.df, seg.hc.segment,
         color = TRUE, #color the groups
         shade = TRUE, #shade the ellipses for group membership
         labels = 4, #label only the groups, not the individual points
         lines = 0, #omit distance lines between groups
         main = "Hierarchical cluster plot", # figure title
)
aggregate(seg.df, list(seg.hc.segment), mean)  


set.seed(96743)
seg.k <- kmeans(seg.df, centers = 4)
aggregate(seg.df, list(seg.k$cluster), mean)
clusplot(seg.df, seg.k$cluster,
         color = TRUE, #color the groups
         shade = TRUE, #shade the ellipses for group membership
         labels = 4, #label only the groups, not the individual points
         lines = 0, #omit distance lines between groups
         main = "K-means cluster plot", # figure title
)

library(mclust)
seg.mc <- Mclust(seg.df, G=3)
summary(seg.mc)

seg.mc4 <- Mclust(seg.df, G =4) #specifying the number of clusters
summary(seg.mc4)
BIC(seg.mc, seg.mc4)
aggregate(seg.df, list(seg.mc$classification), mean)
aggregate(seg.df, list(seg.mc4$classification), mean)

library(cluster)
clusplot(seg.df, seg.mc$classification, color = TRUE, shade = TRUE,
         labels = 4, lines = 0, main = "Model-based cluster plot")
