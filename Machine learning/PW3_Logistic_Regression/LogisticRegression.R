#question 1

social_network = read.csv(file ='Social_Network_Ads.csv')
summary(social_network)
dim(social_network)
#str(social_network)
datanb = 1:400
data = social_network[datanb,]

#question 2

library(corrplot)
plot(social_network$Age,social_network$Purchased)
cor(social_network$Age,social_network$Purchased)
plot(social_network$EstimatedSalary,social_network$Purchased)
cor(social_network$EstimatedSalary,social_network$Purchased)

#question 3
#scale(social_network[,c("Age","EstimatedSalary","Purchased")])

library(caTools)
set.seed(123)
split = sample.split(social_network$Purchased, SplitRatio = 0.75)
training_set = subset(social_network, split == TRUE)
test_set = subset(social_network, split == FALSE)

#question 5 

model = glm(Purchased ~ Age ,data = training_set,family = "binomial") 
summary(model)

#question 6
#catégorie 0 et 1 qui est une loi de bernouilli qui est une loi spÃ©cial de la loi binomial

#question 7
#\log( \frac{p(X)}{1-p(X)} ) = \beta_0 + \beta_1 X$

#question 8
#Age is significatly different from 0 (we can see it in the summary) at least at alpha = 0
#So Age is significant to the model

#question 9
#AIC=2*k - 2*ln(L), we ca find it in the summary of the model
AIC = 256.11

#question 10
plot(training_set$Age,training_set$Purchased)
#probability = predict(model, test_set, response = true)
#curve(probability,add=TRUE)

library(ggplot2)
ggplot(training_set, aes(x=Age, y=Purchased)) +
  geom_point() +
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)


#question 11

model2 = glm(Purchased  ~ Age + EstimatedSalary, family = "binomial", data = training_set)

#question 12
summary(model2)
#both predictors (age and estimated salary) are significant to the model

#question 13 
#The AIC of the first model (without estimated salary) was 256.11 
#but by adding estimated Salary in the model, the AIC is smaller 205.78
#The model with lower value of AIC is better

#question 14
probabilities = predict(model2, test_set, type = "response" )

#question 15
probabilities2 = ifelse(probabilities<0.5,0,1)

#question 16
confusion.matrix = table(probabilities2,test_set$Purchased)
confusion.matrix
#So we have 57 true positive, 26 true negative, 7 false negative and 10 false positive

#question 17
library(caret)
library(lattice)
#accuracy = (true positive + true negative) / total
accuracy = (57+26)/(57+10+7+26)
accuracy = confusion.matrix[0,0] + confusion.matrix[0,1] / sum(confusion.matrix)
sensitivity = sensitivity(confusion.matrix)
sensitivity
specificity = specificity(confusion.matrix)
specificity
