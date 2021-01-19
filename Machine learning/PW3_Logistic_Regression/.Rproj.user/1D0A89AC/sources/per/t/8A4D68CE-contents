#1
library(MASS)
dim(Boston)

#2
train = 1:400
test = - train
variables = which(names(Boston) ==c("age", "medv"))
training_data = Boston[train,]
test_data = Boston[test,]

#3
cor(training_data$age, training_data$medv)

#4
plot(training_data$age, training_data$medv, main = "medv in function of age",
     xlab = "age", ylab = "medv")
model = lm(formula = medv ~ age, data = training_data)
abline(model)

#5
model2 = lm(formula = medv  ~ age + log(lstat), data = training_data)

#6
summary(model2)

#7
#the predictor of age is not significant pr = 8.85e-12 > p-value= 2.2e-16

#8
#The model isnt a whole significant because the age isnt very significant

#9
modelAll = lm(formula = medv ~ ., data = training_data)
modelAll
summary(modelAll)

#10
modelAll2 = lm(formula = medv ~ log(lstat) + . - lstat, data = training_data)
modelAll2
summary(modelAll2)

#11
# R^2 = 0.7339 and with log(lstat) R^2= 0.785

#12
round(cor(training_data), digit = 2)

#13
library(corrplot)
corrplot.mixed(cor(training_data))

#14
cor(training_data$tax,training_data$rad)
#we can see also on the plot that the corrolation is 0.868
#les deux variables sont très corrolé

#15
modelAll3 = lm(formula = medv ~ log(lstat) + . - lstat - tax, data = training_data)
summary(modelAll3)
#R^2 was 0.785 and without tax R^2 = 0.777
#ici tax et rad are very corollated but we can observe that without tax R^2 is the almost same
#F-statistic was 108.4, now F-statistic = 112.8
#if two variable are very corollated, we can keep only one of this two variable

#Of course R^2 should go a little lower because we deleted one of the variables. 
#But check for the model significance (F-statistic) gets higher, 
#which means the p-values gets lower and thus the model is more significant without rad.

#16
#MSE mean squared error 
#MSE = 1/N*sum((actual - prediction)^2)
MSE = mean(modelAll3$residuals^2)
MSE

#17
#str(training_data$chas)
sum(training_data$chas==1)
#we have 35 suburbs in this data set bound the Charles river

#18
boxplot(training_data$medv~training_data$chas)
#there is a very little difference with the median value 

#19
#group i is bound the river so μi is the mean of medv in group i
aggregate(medv ~ chas, training_data, mean)
#ui = 28.44 and uj = 23.94

#20
anova = aov(medv ~ chas, data=training_data)
summary(anova)
#it's significant

#21
model5 = lm(formula = medv ~ chas , data = training_data)
summary(model5)
#F-statistic = 19.68 and r^2 = 0.09 
#chas coefficient is 4.5, so it adds a valuable information

#22
model6 = lm(formula = medv ~ . - chas, data = training_data)
summary(model6)
#F-statistic = 87.7 and r^2= 0.73

model7 = lm(formula = medv ~ ., data = training_data)
summary(model7)
#F-statistic = 81.8 and r^2= 0.73
#Without chas F-statistic is higher, so the model is more significant without chase

#23
model8 = lm(medv ~ (log(lstat) : age), data = training_data)
summary(model8)


#24
model9 = lm(medv ~ (log(lstat) * age), data = training_data)
summary(model9)
