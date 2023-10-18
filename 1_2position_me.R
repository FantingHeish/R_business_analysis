brand.ratings <- read.csv("2_chocolate_rating.csv", stringsAsFactors = TRUE)
brand.sc <- brand.ratings
brand.screscale <- brand.ratings
#------------
brand.screscale[,6:14] <- scale (brand.ratings[,6:14])
#we select all rows and first 9 columns as 10th column is a factor variable.
summary(brand.screscale)

cor(brand.screscale[,6:14]) 

library(corrplot) 
corrplot(cor(brand.screscale[,6:14]), order = "hclust") 

brand.meanrescale <- aggregate(. ~ brand, data=brand.screscale, mean) 
brand.meanrescale
rownames(brand.meanrescale) <- brand.meanrescale[, 1] 
brand.meanrescale 

brand.meanrescale <- brand.meanrescale[ -c(1:5)]

brand.meanrescale <- brand.meanrescale[ -c(10:12)]


library(gplots) 
heatmap.2(as.matrix(brand.meanrescale),main = "Brand attributes", trace = "none", key = FALSE, dend ="none") 

brand.pcrescale<- princomp(brand.meanrescale, cor = TRUE)
summary(brand.pcrescale) 

# wrong (donot know why)
# v <- round(brand.pcrescale^2/sum(brand.pcrescale$sdev^2) * 100)
# v

plot(brand.pcrescale,type="l") # scree plot 
#------------
library(factoextra)
brand.ei <- get_eigenvalue(brand.pcrescale)
brand.ei
#------------
loadings(brand.pcrescale) 
brand.pcrescale$scores 
biplot(brand.pcrescale, main = "Brand positioning", cex=0.7)


# distance between
# East Van Roasters, Franceschi
# Pura Delizia, Soma
distance_1 <- colMeans(brand.meanrescale[c("East Van Roasters","Franceschi","Pura Delizia","Soma"),]) - brand.meanrescale[,]
summary(distance_1)

# distance between
# Holy Cacao, Naive
# Chocolate Makers, Burnt Fork Bend
distance_2 <- colMeans(brand.meanrescale[c("Holy Cacao","Naive","Chocolate Makers","Burnt Fork Bend"),]) - brand.meanrescale[,]
summary(distance_2)

# distance between
# Laia aka Chat-Noir, Kyya
# Domori, Dormouse
distance_3 <- colMeans(brand.meanrescale[c("Laia aka Chat-Noir","Kyya","Domori","Dormouse"),]) - brand.meanrescale[,]
summary(distance_3)



#------------------

# 3 Factor analysis using factanal
library(nFactors)
nScree(brand.meanrescale)
eigen(cor(brand.meanrescale))

brand.fa <- factanal(brand.meanrescale, factors = 2, rotation = "varimax", scores = "regression")

brand.fl<- brand.fa$loadings[, 1:2]
plot(brand.fl,type="n") # set up plot
text(brand.fl,labels=names(brand.meanrescale),cex=.7)


brand.fs <- brand.fa$scores
plot(brand.fl,type="n") # set up plot
text(brand.fl,labels=rownames(brand.meanrescale),cex=.7)
