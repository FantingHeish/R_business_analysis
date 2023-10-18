brand.ratings <- read.csv("2_chocolate_rating.csv", stringsAsFactors = TRUE)
brand.sc <- brand.ratings 
cor(brand.sc[,6:14]) 
library(corrplot) 
corrplot(cor(brand.sc[,6:14]))
corrplot(cor(brand.sc[,6:14]), order = "hclust") 
brand.mean <- aggregate(. ~ brand, data=brand.sc, mean) 
brand.mean
rownames(brand.mean) <- brand.mean[, 1] 
brand.mean 

brand.mean <- brand.mean[ -c(1:5)]

brand.mean <- brand.mean[ -c(10:12)]
 
library(gplots) 
heatmap.2(as.matrix(brand.mean),main = "Brand attributes", trace = "none", key = FALSE, dend ="none") 

brand.pc<- princomp(brand.mean, cor = TRUE)
summary(brand.pc) 
plot(brand.pc,type="l") # scree plot 
loadings(brand.pc) 
brand.pc$scores 
biplot(brand.pc, main = "Brand positioning")
