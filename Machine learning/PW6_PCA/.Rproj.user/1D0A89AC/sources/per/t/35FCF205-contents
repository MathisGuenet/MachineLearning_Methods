library(factoextra)
library(gridExtra)
library(ggplot2)

mtcars

PCA_mtcars = princomp(mtcars, cor=T)
fviz_screeplot(PCA_mtcars, addlabels = TRUE, ylim = c(0, 80))

#we see that dimensions 1 and 2 explain 85% of the variance, so we'll use this 2 dimensions

fviz_pca_var(PCA_mtcars, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)

fviz_pca_ind(PCA_mtcars)

fviz_pca_biplot(PCA_mtcars, repel = TRUE) 

#cyl, disp, wt, hp & mpg contribue beaucoup sur la dimension 1 et
#ils sont très corrélés entre eux (mpg est corrélé negativement)

#A gauche du graphique au environ de l'axe  des abscises (dimension 1) on peut observer 
#un groupe qui forme un certain type de voiture (voiture avec beaucoup de puissance plus luxuese avec des grandes marques de voiture)

#Alors que les voitures qui sont à l'opposé (a droite du graphique au environ de l'axe des abscises (dimension 1) sont des voitures plus low-cost

#l'axe des ordonnées (dimension 2) nous donne pas trop d'information sur les types de voiture