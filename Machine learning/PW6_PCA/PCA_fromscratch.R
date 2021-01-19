library(factoextra)
library(gridExtra)
library(ggplot2)
iris = read.csv("iris.data", header = TRUE)
summary(iris)
x = iris[,-5]
y = factor(iris[,5])

summary(x)

x_scaled = scale(x)

#let's determine covariance and correlation matrix
covarMatrix = cov(x_scaled)

eigen = eigen(covarMatrix)
eigen

corMatrix = cor(x_scaled)
covarMatrix

corMatrix 
#puisque les données sont standardisé la matrice de covariance des données 
#standardisé est la meme que la matrice de la correlation

#let's determine the explained variance 

#compare with screeplot
PCA_iris=princomp(iris[,-5], cor=T) 
fviz_screeplot(PCA_iris, addlabels = TRUE, ylim = c(0, 50))

#Projection Matrix
projectionMatrix =covarMatrix%*%eigen$vectors
projectionMatrix

#Scores Matrix dimension 3 and 4 are useless
scoreMatrix = x_scaled%*%projectionMatrix
#scoreMatrix

plot(scoreMatrix[,1],scoreMatrix[,2], xlab = "PC1", ylab="PC2",col= y)

