
# Loading the dataset.. I have putted it into a folder called "datasets"
dataset <- read.csv('http://www.mghassany.com/MLcourse/datasets/Social_Network_Ads.csv')

# Describing and Exploring the dataset

str(dataset) # to show the structure of the dataset. 
summary(dataset) # will show some statistics of every column. 
# Remark what it shows when the column is a numerical or categorical variable.
# Remark that it has no sense for the variable User.ID

boxplot(Age ~ Purchased, data=dataset, col = "blue", main="Boxplot Age ~ Purchased")
# You know what is a boxplot right? I will let you interpret it.
boxplot(EstimatedSalary ~ Purchased, data=dataset,col = "red",
        main="Boxplot EstimatedSalary ~ Purchased")
# Another boxplot

aov(EstimatedSalary ~Purchased, data=dataset)
# Anova test, but we need to show the summary of 
# it in order to see the p-value and to interpret.

summary(aov(EstimatedSalary ~Purchased, data=dataset))
# What do you conclude ?
# Now another anova test for the variable Age
summary(aov(Age ~Purchased, data=dataset))

# There is a categorical variable in the dataset, which is Gender.
# Of course we cannot show a boxplot of Gender and Purchased.
# But we can show a table, or a mosaic plot, both tell the same thing.
table(dataset$Gender,dataset$Purchased)
# Remark for the function table(), that
# in lines we have the first argument, and in columns we have the second argument.
# Don't forget this when you use table() to show a confusion matrix!
mosaicplot(~ Purchased + Gender, data=dataset,
           main = "MosaicPlot of two categorical variables: Puchased & Gender",
           color = 2:3, las = 1)

# since these 2 variables are categorical, we can apply
# a Chi-square test. The null hypothesis is the independance between
# these variables. You will notice that p-value = 0.4562 which is higher than 0.05 (5%)
# so we cannot reject the null hypothesis. 
# conclusion: there is no dependance between Gender and Purchased (who
# said that women buy more than men? hah!)

chisq.test(dataset$Purchased, dataset$Gender)

# Let's say we want to remove the first two columns as we are not going to use them.
# But, we can in fact use a categorical variable as a predictor in logistic regression.
# It will treat it the same way as in regression. Check Appendix C.
# Try it by yourself if you would like to.
dataset = dataset[3:5]
str(dataset) # show the new structure of dataset


# splitting the dataset into training and testing sets
library(caTools)
set.seed(703817) # CHANGE THE VALUE OF SEED. PUT YOUR STUDENT'S NUMBER INSTEAD OF 123.
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# scaling
# So here, we have two continuous predictors, Age and EstimatedSalary.
# There is a very big difference in their scales (units).
# That's why we scale them. But it is not always necessary.

training_set[-3] <- scale(training_set[-3]) #only first two columns
test_set[-3] <- scale(test_set[-3])

# Note that, we replace the columns of Age and EstimatedSalary in the training and
# test sets but their scaled versions. I noticed in a lot of reports that you scaled
# but you did not do the replacing.
# Note too that if you do it column by column you will have a problem because 
# it will replace the column by a matrix, you need to retransform it to a vector then.
# Last note, to call the columns Age and EstimatedSalary we can it like I did or 
# training_set[c(1,2)] or training_set[,c(1,2)] or training_set[,c("Age","EstimatedSalary")]


# logistic regression

classifier.logreg <- glm(Purchased ~ Age + EstimatedSalary , family = binomial, data=training_set)
classifier.logreg
summary(classifier.logreg)

# prediction
pred.glm = predict(classifier.logreg, newdata = test_set[,-3], type="response")
# Do not forget to put type response. 
# By the way, you know what you get when you do not put it, right?

# Now let's assign observations to classes with respect to the probabilities
pred.glm_0_1 = ifelse(pred.glm >= 0.5, 1,0)
# I created a new vector, because we need the probabilities later for the ROC curve.

# show some values of the vectors
head(pred.glm)
head(pred.glm_0_1)

# confusion matrix
cm = table(test_set[,3], pred.glm_0_1)
cm
# First line to store it into cm, second line to show the matrix! 

# You remember my note about table() function and the order of the arguments?
cm = table(pred.glm_0_1, test_set[,3])
cm

# You can show the confusion matrix in a mosaic plot by the way
mosaicplot(cm,col=sample(1:8,2)) # colors are random between 8 colors.

# ROC
require(ROCR)
score <- prediction(pred.glm,test_set[,3]) # we use the predicted probabilities not the 0 or 1
performance(score,"auc") # y.values
plot(performance(score,"tpr","fpr"),col="green")
abline(0,1,lty=8)



#exo2

slope <- -coef(classifier.logreg)[2]/(coef(classifier.logreg)[3])
intercept <- -coef(classifier.logreg)[1]/(coef(classifier.logreg)[3])

plot(test_set$Age,test_set$EstimatedSalary, main="EstimatedSalary vs Age", xlab="Age", ylab="EstimatedSalary", bg = ifelse(pred.glm_0_1 == 1, "blue4", "red3"))
abline(intercept,slope)

#exo3 blue point are the persons who were predicted to purchased 

plot(test_set$Age, test_set$EstimatedSalary, xlab="Age",ylab="EstimatedSalary")
title("Decision Boundary Logistic Regression")
points(test_set[1:2], pch = 21, bg = ifelse(pred.glm_0_1 == 1, "blue4", "red3"))
abline(intercept,slope, lwd=2)

#exo 4 blue point are the person who really purchased

plot(test_set$Age, test_set$EstimatedSalary, xlab="Age",ylab="EstimatedSalary")
title("Decision Boundary of real data")
points(test_set[1:2], pch = 21, bg = ifelse(test_set[3] == 1, "blue4", "red3"))
abline(intercept,slope, lwd=2)

#confusion matrix of linera Regression
cm = table(test_set[,3], pred.glm_0_1)
cm

#exo 5 LDA : Linear Discriminant Analysis

library(MASS)
classifier.lda <- lda(Purchased~Age+EstimatedSalary, data=training_set)
classifier.lda

#exo 7
prediction_lda=predict(classifier.lda, newdata=test_set[,-3],type='response')
prediction_lda

#exo 8 confusion matrix for prediction with LDA we can compare with the matrix confusion of linear regression
cm = table(prediction_lda$class, test_set[,3])
cm
#the accuracy is almost the same

#exo 9

# create a grid corresponding to the scales of Age and EstimatedSalary
# and fill this grid with lot of points
X1 = seq(min(training_set[, 1]) - 1, max(training_set[, 1]) + 1, by = 0.01)
X2 = seq(min(training_set[, 2]) - 1, max(training_set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
# Adapt the variable names
colnames(grid_set) = c('Age', 'EstimatedSalary')

# plot 'Estimated Salary' ~ 'Age'
plot(test_set[, 1:2],
     main = 'Decision Boundary LDA',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))

# color the plotted points with their real label (class)
points(test_set[1:2], pch = 21, bg = ifelse(test_set[, 3] == 1, 'green4', 'red3'))

# Make predictions on the points of the grid, this will take some time
pred_grid = predict(classifier.lda, newdata = grid_set)$class

# Separate the predictions by a contour
contour(X1, X2, matrix(as.numeric(pred_grid), length(X1), length(X2)), add = TRUE)


#exo 11

# qda() is a function of library(MASS)
classifier.qda <- qda(Purchased~Age+EstimatedSalary, data = training_set)
classifier.qda


#exo 13
prediction_qda = predict(classifier.qda, newdata = test_set[-3], type="response")
cm = table(prediction_qda$class, test_set[,3])
cm 
X1 = seq(min(training_set[, 1]) - 1, max(training_set[, 1]) + 1, by = 0.01)
X2 = seq(min(training_set[, 2]) - 1, max(training_set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
# Adapt the variable names
colnames(grid_set) = c('Age', 'EstimatedSalary')

# plot 'Estimated Salary' ~ 'Age'
plot(test_set[, 1:2],
     main = 'Decision Boundary LDA',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))

# color the plotted points with their real label (class)
points(test_set[1:2], pch = 21, bg = ifelse(test_set[, 3] == 1, 'green4', 'red3'))

# Make predictions on the points of the grid, this will take some time
pred_grid = predict(classifier.qda, newdata = grid_set)$class

# Separate the predictions by a contour
contour(X1, X2, matrix(as.numeric(pred_grid), length(X1), length(X2)), add = TRUE)



#exo 14 #j'ai refait toutes les prediction pour que ca soit plus propre
pred.glm = predict(classifier.logreg, newdata = test_set[,-3], type="response")
pred.lda = predict(classifier.lda, newdata = test_set[-3], type="response")
pred.qda = predict(classifier.qda, newdata = test_set[-3], type="response")


score.glm <- prediction(pred.glm,test_set[,3])
score.lda <- prediction(pred.lda$posterior[,2],test_set[,3])
score.qda <- prediction(pred.qda$posterior[,2],test_set[,3])

performance(score.glm,"auc")@y.values
performance(score.lda,"auc")@y.values
performance(score.qda,"auc")@y.values

plot(performance(score.glm,"tpr", "fpr"), col="green",label = "glm")
plot(performance(score.lda,"tpr", "fpr"), col="red",add=T,label = "lda")
plot(performance(score.qda,"tpr", "fpr"), col="blue",add=T,label = "qda")



#exo 10

set = training_set
classifier2 <- lda(Purchased ~ ., data = set)
mu_0 = classifier2$means[1,]
m_1 = classifier2$means[2,]
class0 = set[which(set$Purchased==0),]
class1 = set[which(set$Purchased==1),]
sigma = (1/(nrow(set)-1))*(nrow(class0)*cov(class0[-3])+nrow(class1)*cov(class1[-3]))

