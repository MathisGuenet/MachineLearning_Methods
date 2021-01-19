library(factoextra)
library(FactoMineR)
library(gridExtra)
library(ggplot2)
iris = read.csv("iris.data", header = TRUE)
summary(iris)

#means and quartiles of the 3 different flower classes for the 4 different features'
par(mfrow=c(2,2))
boxplot(sepal_length ~ class, data = iris, xlab = "Iris Class", ylab = "sepal_length")
boxplot(sepal_width ~ class, data = iris, xlab = "Iris Class", ylab = "sepal_width")
boxplot(petal_length ~ class, data = iris, xlab = "Iris Class", ylab = "petal_length")
boxplot(petal_width ~ class, data = iris, xlab = "Iris Class", ylab = "petal_width")


# histogram of sepal_length
ggplot(iris, aes(x=sepal_length, fill=class)) +
  geom_histogram(binwidth=.2, alpha=.5)
# histogram of sepal_width
ggplot(iris, aes(x=sepal_width, fill=class)) +
  geom_histogram(binwidth=.2, alpha=.5)
# histogram of petal_length
ggplot(iris, aes(x=petal_length, fill=class)) +
  geom_histogram(binwidth=.2, alpha=.5)
# histogram of petal_width
ggplot(iris, aes(x=petal_width, fill=class)) +
  geom_histogram(binwidth=.2, alpha=.5)

PCA_iris=princomp(iris[,-5], cor=T) 

str(PCA_iris)

summary(PCA_iris)

par(mfrow=c(1,1))
plot(PCA_iris) 

biplot(PCA_iris) 


get_eig(PCA_iris)

#percentage of explained variances
fviz_screeplot(PCA_iris, addlabels = TRUE, ylim = c(0, 50))
var = get_pca_var(PCA_iris)

#head of coordinate of variable in function of principal component
head(var$coord)

#head of contribution of the variable for the specific principal component
head(var$contrib)

# Graph of variables using their contributions
fviz_pca_var(PCA_iris, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)

ind = get_pca_ind(PCA_iris)
# Coordinates of individuals
head(ind$coord)

# Graph of individuals using contribution
fviz_pca_ind(PCA_iris,
             label = "none", # hide individual labels
             habillage = iris$class, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE # Concentration ellipses
)

# Biplot of individuals and variables
fviz_pca_biplot(PCA_iris, repel = FALSE)

