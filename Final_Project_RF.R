#set working directory
#setwd("C:\\Myprogram\\Final_Project\\")
load("attrition_df_fin.Rdata")
load("attrition_df_fin_2.Rdata")
load("attrition_df_fin_3.Rdata")

#Install required libraries
library(randomForest) #rf model
library(caret) # feature selection
library(e1071) # model tuning
library(ROCR) # model evaluation

indexes_RF <- sample(1:nrow(attrition_df_fin),size = 0.7*nrow(attrition_df_fin))
indexes_RF
#Split 70%,of the random data to the train samples
train.data_RF <- attrition_df_fin[indexes_RF,]
#split remaining 30% of random data to test samples
test.data_RF <- attrition_df_fin [-indexes_RF,]
test.data_RF


## separate feature and class variables
test.feature.vars_RF <- test.data_RF[,-2]
test.class.var_RF <- test.data_RF[,2]
test.class.var_RF


## build initial model with training data
#formula.init <- "credit.rating ~ ."  NOT REQUIRED
#formula.init <- as.formula(formula.init)  NOT REQUIRED

#to chose the optimal mtry values
mtry_ops <- tuneRF(train.data_RF,train.data_RF$Attrition,
                   stepFactor = 1.2,improve = 0.01, trace = T,plot = T)

#to choose mtry = 4 (4 feature for every cycle of iteration)
rf.model <- randomForest(Attrition ~ ., data = train.data_RF, importance=T, 
                         mtry=4, nodesize=5, ntrees=500)

## view model details
print(rf.model)
## This gives us information on OOBE which is around 23%


#importance parameter gives which is the significant varible
importance(rf.model)

randomForest::varImpPlot(rf.model, sort=T,n.var=20, type=1)

## predict and evaluate results
rf.predictions <- predict(rf.model, test.feature.vars_RF, type="class")
caret::confusionMatrix(data=rf.predictions, reference=test.class.var_RF, 
                       positive="1")


#######Model 2##########
indexes_RF_2 <- sample(1:nrow(attrition_df_fin_2),size = 0.7*nrow(attrition_df_fin_2))
indexes_RF_2
#Split 70%,of the random data to the train samples
train.data_RF_2 <- attrition_df_fin_2[indexes_RF_2,]
#split remaining 30% of random data to test samples
test.data_RF_2 <- attrition_df_fin_2 [-indexes_RF_2,]
test.data_RF_2


## separate feature and class variables
test.feature.vars_RF_2 <- test.data_RF_2[,-1]
test.class.var_RF_2 <- test.data_RF_2[,1]
test.class.var_RF_2


## build initial model with training data
#formula.init <- "credit.rating ~ ."  NOT REQUIRED
#formula.init <- as.formula(formula.init)  NOT REQUIRED

#to chose the optimal mtry values
mtry_ops_2 <- tuneRF(train.data_RF_2,train.data_RF_2$Attrition,
                   stepFactor = 1.2,improve = 0.01, trace = T,plot = T)

#to choose mtry = 4 (4 feature for every cycle of iteration)
rf.model_2 <- randomForest(Attrition ~ ., data = train.data_RF_2, importance=T, 
                         mtry=4, nodesize=5, ntrees=500)

## view model details
print(rf.model_2)
## This gives us information on OOBE which is around 14%


#importance parameter gives which is the significant varible
importance(rf.model_2)
#plot the important parameters in order
randomForest::varImpPlot(rf.model_2, sort=T,n.var=22, type=1)

## predict and evaluate results
rf.predictions_2 <- predict(rf.model_2, test.feature.vars_RF_2, type="class")
caret::confusionMatrix(data=rf.predictions_2, reference=test.class.var_RF_2, 
                       positive="1")

## hyperparameter optimizations - for model 3 (taking only imp parameter from Varimplot)
# run grid search 
nodesize.vals_3 <- c(2, 3, 4, 5)
#below code will run longeg: node size 2 will have 200 then 500 then 1000 then 2000 trees
#ntree.vals <- c(200, 500, 1000, 2000)
ntree.vals_3 <- c(100,200,300,400,500)
formula.new_3 <- "Attrition ~ OverTime + MonthlyIncome 
                + TotalWorkingYears
                  +StockOptionLevel + YearsAtCompany" 

formula.new_3 <- as.formula(formula.new_3)
#tuning.results_3 <- tune.randomForest(formula.new_3, 
tuning.results_3 <- tune.randomForest(Attrition ~ ., 
                                    data = train.data_RF_2,
                                    mtry=3, 
                                    nodesize=nodesize.vals_3, 
                                    ntree=ntree.vals_3)

print(tuning.results_3)

# get best model and predict and evaluate performance
rf.model.best_3 <- tuning.results_3$best.model
rf.predictions.best_3 <- 
  predict(rf.model.best_3, test.feature.vars_RF_2, type="class")
confusionMatrix(data=rf.predictions.best_3, reference=test.class.var_RF_2, 
                positive="1")

#good accuracy of 86%
