#TD fait avec Arthur Clavel et Mathis Guenet

library(MASS)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(gbm)


dataset = read.csv('spam.csv')
dataset$spam = as.numeric(dataset$spam)
#we convert true and false to 0 and 1
set.seed(18)

#to calculate the error
calc_acc = function (predicted,actual){mean(predicted == actual)}

Spam_idx = sample(1:nrow(dataset), nrow(dataset) / 2) 
# We split the datasets
spam_train = dataset[Spam_idx,]
spam_test  = dataset[-Spam_idx,]

#summary(dataset)


#--Single Tree--#
spam_tree = rpart(spam ~ . , data = spam_train)
spam_tree_predict = predict(spam_tree, newdata = spam_test)
spam_tree_predict =ifelse(spam_tree_predict>0.5,TRUE,FALSE)
#spam_tree_predict

#--Logistic Regression--#
spam_lm = glm(formula = spam ~ . ,family = "binomial", data = spam_train)
spam_lm_predict=predict(spam_lm,spam_test, type="response") #return
spam_lm_predict =ifelse(spam_lm_predict>0.5,TRUE,FALSE)
#spam_lm_predict


#--Bagging Tree--#
spam_bagging = randomForest(spam ~ ., data = spam_train, ntrees = 500, family = "binomial",  mtry = 57)
spam_bagging_predict = predict(spam_bagging, newdata = spam_test)
spam_bagging_predict =ifelse(spam_lm_predict>0.5,TRUE,FALSE)
#spam_bagging_predict 


#--RandomForest Tree--#
spam_randomForest = randomForest(spam ~ ., data = spam_train, mtry = 7, importance = TRUE, ntrees = 500, family = "binomial")
spam_randomForest_predict = predict(spam_randomForest, newdata = spam_test)
spam_randomForest_predict =ifelse(spam_lm_predict>0.5,TRUE,FALSE)
#spam_randomForest_predict 

#--Boosting Tree--#
spam_boost = gbm(spam ~ ., data = spam_train, distribution = "bernoulli", n.trees = 500, interaction.depth = 4, shrinkage = 0.01)
#for classification we need bernouilli
spam_boost_predict = predict(spam_boost, newdata = spam_test, type ="response")
spam_boost_predict=ifelse(spam_boost_predict>0.5,TRUE,FALSE)
#spam_boost_predict


#--Interpreting Result--#
#Get the result of accuracy in table
model = c('linear','single tree','bagging','random forest','boosting')
accuracy = c(calc_acc(spam_lm_predict,spam_test$spam),calc_acc(spam_tree_predict,spam_test$spam),calc_acc(spam_bagging_predict, spam_test$spam),calc_acc(spam_randomForest_predict, spam_test$spam),calc_acc(spam_boost_predict, spam_test$spam))
rmseAccuracy.data = data.frame(model,accuracy)  
library(knitr)  
knitr::kable(rmseAccuracy.data, "simple")  


barplot(accuracy,col=c(2,3,4,5,6),legend.text = c("linear","single tree","bagging",'random forest','boosting'),
        args.legend=list(x="bottomleft"),xlab = "Type of prediction", ylab = "Accuracy of model", 
        main = "Accuracy of differant model",) 