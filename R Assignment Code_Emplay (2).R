
#*******************Statistical Model for Case Study - Emplay*******************#

getwd()

setwd("C:/Users/Vikas/Downloads/R Assignment Emplay/Case-study1")

mydata <- read.csv("traindata_R.csv")

dim(mydata)

names(mydata)

#********Part - 1: Data Manipulation and Summary Statistics***********#

#**********Count Of Women for every Category of Education**********#

if (class(mydata$Wife_education) != "factor")
       mydata$Wife_education <- as.factor(mydata$Wife_education)

count <- c()

for(i in c(1:nlevels(mydata$Wife_education)))
  count[i] <- length(mydata$Wife_education[mydata$Wife_education == levels(mydata$Wife_education)[i]])

count

sum(count)

#*****Sum must be equal to number of rows of train data*****#

#**************Average Age for every Category of Education**************#

Average <- c()

for(i in c(1:nlevels(mydata$Wife_education)))
  Average[i] <- mean(mydata$Wife_age[mydata$Wife_education == levels(mydata$Wife_education)[i]])

Average

#***********Average Number Of Children for every Category of Education***********#

Average_child <- c()

for(i in c(1:nlevels(mydata$Wife_education)))
  Average_child[i] <- mean(mydata$Number_of_children_ever_born[mydata$Wife_education == levels(mydata$Wife_education)[i]])

Average_child <- floor(Average_child) #use either floor or ceiling

#************Percentage of Women working for every Category of Education************#

if (class(mydata$Wife_working) != "factor")
       mydata$Wife_working <- as.factor(mydata$Wife_working)

perc_wom_work <- c()

wom_work <- c()

for(i in c(1:nlevels(mydata$Wife_education))) {

wom_work <- mydata$Wife_working[mydata$Wife_education == levels(mydata$Wife_education)[i]]

perc_wom_work[i] <- length(wom_work[wom_work == 0])/length(wom_work)  * 100

}

perc_wom_work #****Percentages of Working Women in each Category of Education****#

#***********Percentage of women who have a high standard of living[Index = 4]************#

if (class(mydata$Standard_of_living_index) != "factor")
       mydata$Standard_of_living_index <- as.factor(mydata$Standard_of_living_index)

perc_high_index <- c()

Index <- c()

for(i in c(1:nlevels(mydata$Wife_education))) { 

Index <- mydata$Standard_of_living_index[mydata$Wife_education == levels(mydata$Wife_education)[i]]

perc_high_index[i] <- length(Index[Index == 4])/length(Index)  *  100

}

perc_high_index #***Percentage of Women having standard of living index as 4***#

#************Consolidation of Statistics into a Data Frame************#

DF <- rbind(count,Average,Average_child,perc_wom_work,perc_high_index)

if(class(DF) != "data.frame")
   DF <- as.data.frame(DF)

names(DF) <- c("Category 1","Category 2","Category 3","Category 4")

DF #**********Data frame having all the 20 Variables**********#

target_train <- subset(mydata, select = c("Party_voted_for"))

input_var_train <- mydata[, !names(mydata) %in% c("Party_voted_for")]

input_num_train <- mydata[, names(mydata) %in% c("Wife_age",
                                                 "Number_of_children_ever_born")]

input_categ_train <- input_var_train[, !names(input_var_train) %in% names(input_num_train)]

categ_target_train <- data.frame(input_categ_train, target_train)

#****************Categorical to Numerical Variables Transformation*********************#

install.packages("Information")

library(Information)

woe_func <- function(var,target_) {

fact_vars <- var

target <- target_

for(i in c(1:ncol(fact_vars))) {
   if(class(fact_vars[[i]]) != "factor")
         fact_vars[[i]] <- as.factor(fact_vars[[i]])
}

samp <- cbind(fact_vars,target)

IV_Categ <- Information::create_infotables(data = samp, y = names(target) )

num_levels <- c()

for(i in c(1:ncol(fact_vars)))
{
   num_levels[i] <- nlevels(fact_vars[[i]])
}

woe_mat <- matrix(nrow = nrow(fact_vars), ncol = ncol(fact_vars))

for(i in c(1:ncol(fact_vars)))
{
      for(j in c(1:nrow(fact_vars)))
      {
           for(k in c(1:num_levels[i]))
           {
                 dat <- as.data.frame(fact_vars[i][j, ])
                 if(dat == IV_Categ$Tables[[i]][[1]][k])
                        woe_mat[j,i] <- IV_Categ$Tables[[i]][[4]][k]
          }
     }
}

woe_frame <- as.data.frame(woe_mat)

names(woe_frame) <- names(fact_vars)

return(woe_frame)
}

woe_set <- woe_func(input_categ_train,target_train)

proce_data <- cbind(input_num_train, woe_set)

str(proce_data)

final_train <- cbind(proce_data, target_train)

dim(final_train)

str(final_train)

#*********Part - 2:*********#

#*********Fitting the Logistic Model for the Processed Training Data*********#

logistic.glm <- glm(formula = Party_voted_for~., family = binomial(link = "logit"),
                                           data = final_train)

summary(logistic.glm)

#**********Fitting a Random Forest Model to the Training Data**********#

install.packages("randomForest")

library(randomForest)

fact_target <- as.factor(target_train$Party_voted_for)

new_train <- cbind(proce_data, fact_target)

dim(new_train)

str(new_train)

rf <- randomForest(fact_target~., data = new_train)

importance(rf)

rf

#*********Fitting a Decision Tree to the Training Data*********#

install.packages("rpart")

library(rpart)

tree.fit <- rpart(formula = fact_target~., data = new_train, method = "class")

printcp(tree.fit)

plotcp(tree.fit)

summary(tree.fit)

test_data <- read.csv("testdata_R.csv")

names(test_data)

#***Variables having greater predictive Power are selected based on the model results for***#
                                #*******training Data***********#

test_categ <- subset(test_data, select = c("Wife_education","Standard_of_living_index",
                                           "Media_exposure","Husband_education",
                                            "Wife_religion","Wife_working","Husband_occupation"))

target_test <- subset(test_data, select = c("Party_voted_for"))

woe_test_set <- woe_func(test_categ,target_test)

test_num <- subset(test_data, select = c("Wife_age","Number_of_children_ever_born"))

proce_test_data <- cbind(test_num,woe_test_set)

#****************Predicting the probabilities on the training set*****************#

install.packages("e1071")

library(e1071)

library(caret)

P = predict(logistic.glm,newdata = proce_data,type = "response") #Logistic Regression# 

p.result <- round(P)

P_rf <- predict(rf, newdata = proce_data,type = "response")      #RandomForest#

P_tree <- predict(tree.fit, newdata = proce_data,type = "class") #Decision Tree#

#********Confusion Matrix for Training data********#

LR_train_conf <- confusionMatrix(p.result, target_train$Party_voted_for)   #Logistic Regression#

RF_train_conf <- confusionMatrix(P_rf,target_train$Party_voted_for)        #RandomForest#

Tree_train_conf <- confusionMatrix(P_tree,target_train$Party_voted_for)      #DecisionTree#

LR_train_conf

RF_train_conf

Tree_train_conf

#****************Predicting the probabilities on the test set*****************#

P.test <- predict(logistic.glm,newdata = proce_test_data,type = "response") #LogisticRegression#

P_test_result <- round(P.test)

P_rf_test_roc <- predict(rf,newdata = proce_test_data,type = "prob")        #RandomForest#

P_rf_test <- predict(rf,newdata = proce_test_data,type = "class")        #RandomForest#

P_test_tree <- predict(tree.fit,newdata = proce_test_data,type = "class")   #DecisionTree

P_test_tree_roc <- predict(tree.fit,newdata = proce_test_data,type = "prob")   #DecisionTree

#*********Confusion Matrices for test data*********#

LR_test_conf <- confusionMatrix(P_test_result, target_test$Party_voted_for) #LogisticRegression#

RF_test_conf <- confusionMatrix(P_rf_test, target_test$Party_voted_for)     #RandomForest#

Tree_test_conf <- confusionMatrix(P_test_tree, target_test$Party_voted_for)   #DecisionTree

LR_test_conf

RF_test_conf

Tree_test_conf

#************Part - 3: Model Evaluation******************#

#***************ROC Curves for all the Models*******************#

library(ROCR)

pred_LR_test <- prediction(P.test,target_test$Party_voted_for)

perf_LR_test <- performance(pred_LR_test,"tpr","fpr")

plot(perf_LR_test)

pred_RF_test <- prediction(P_rf_test_roc[,2],target_test$Party_voted_for)

perf_RF_test <- performance(pred_RF_test,"tpr","fpr")

plot(perf_RF_test)

pred_tree_test <- prediction(P_test_tree_roc[,2],target_test$Party_voted_for)

perf_tree_test <- performance(pred_tree_test,"tpr","fpr")

plot(perf_tree_test)

plot(perf_LR_test,colorize = TRUE)          #ROC Curve for Logistic Regression

plot(perf_RF_test,add = TRUE,colorize = TRUE)  #ROC Curve for Random Forest

plot(perf_tree_test,add = TRUE,colorize = TRUE) #ROC Curve for Decision Tree
  
AUC_LR <- performance(pred_LR_test,"auc")

AUC_RF <- performance(pred_RF_test,"auc")

AUC_tree <- performance(pred_tree_test,"auc")

#*******AREA UNDER CURVE VALUE FOR DECISON TREE IS 0.6959924*******#           #test set#
#*******AREA UNDER CURVE VALUE FOR RANDOM FOREST IS 0.75231*******#            #test set#
#*******AREA UNDER CURVE VALUE FOR LOGISTIC REGRESSION IS 0.7276145*******#    #test set#

#***Area under curve is more for Random Forest***#       #Test set#

#***********predictive accuracy for Logistic Regression is 70.06%***********#  #test set#
#***********predictive accuracy for Random Forest is 69.65%************#       #test set#
#***********predictive accuracy for Decision tree model is 69.86%*************#  #test set#

#****As Predictive Accuracy is approximately same for all the models, Consider AUC****#

#***Random Forest is slightly better performing model than LR and DecisionTrees***#









