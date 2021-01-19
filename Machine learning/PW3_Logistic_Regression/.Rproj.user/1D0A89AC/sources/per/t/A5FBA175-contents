load("EU.RData")
mod = lm(formula = Seats2011 ~ Population2010, data = EU)
mod

mod$coefficients

myModel = lm(formula = CamCom2011 ~ Population2010, data = EU )
myModel$coefficients
sumMyModel = summary(myModel)
sumMyModel

anova(myModel)

library(MASS)
dim(Boston)
train = 1:400
test = -train
variables = which(names(Boston) ==c("lstat", "medv"))
training_data = Boston[train, variables]
testing_data = Boston[test, variables]
plot(training_data$lstat, training_data$medv, main = "medv in function of lstat",
     xlab = "lstat", ylab = "medv")

plot(log(training_data$lstat), training_data$medv)
model = lm(formula =  medv ~ log(lstat), data = training_data)
model
summary(model)

  