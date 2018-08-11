#set working directory
setwd("C:\\Myprogram\\Final_Project\\")

#import and read the data
attrition_data <- read.csv("Attrition.csv")
summary(attrition_data)

#we dont have any null values in the data set

names(attrition_data)

#Below features consist of a Factor data (but importes as integer)
#Education 
#EnvironmentSatisfaction
#JobInvolvement
#JobSatisfaction
#PerformanceRating
#RelationshipSatisfaction
#StockOptionLevel
#WorkLifeBalance


#data type transformation - factoring
#creating the function --> this function will convert to the factors.
convert.to.factors <- function(df,categvars){
  for (variable in categvars){
    df[[variable]] <- as.factor(df[[variable]])
  }
  return(df)
}
#moving the categorical variables to a variable so that we can pass it into the 
#function
categorical.vars <- c('Education','EnvironmentSatisfaction',               
                      'JobInvolvement',
                      'JobSatisfaction','PerformanceRating',                 
                      'RelationshipSatisfaction','StockOptionLevel',         
                      'WorkLifeBalance')


#calling the fucntion --> by passing the variables to the function
attrition_df <- convert.to.factors (attrition_data,categorical.vars)


#Below are the features which has factor with one level or no variation in the data or
#will not contribute(like EmployeeNumber)
#So we will exclude the below data
#EmployeeCount
#Over18
#StandardHours
attrition_df_ex <- attrition_df[c(-9,-10,-22,-27)]

#Normalising - Scaling the features:(Note:Ensure it is numeric first before scaling)
#we can normalise the below features to bring them to a reduced scales
#Age
#DailyRate
#HourlyRate
#MonthlyIncome
#MonthlyRate
#TotalWorkingYears

#Function for normalising -Scaling
scale.feature <- function(df,variableints){
  for (variableinp in variableints){
    df[[variableinp]] <- scale(df[[variableinp]], center = T, scale = T)
    # the centre is for mean centering.. we will take the values from the mean
    #this will subtract the values from the mean
  }
  
  return (df)
  
}
#normalise variable -> we will use the above fuction to normalise the variable
numeric.vars <- c("Age","DailyRate","HourlyRate",
                  "MonthlyIncome","MonthlyRate","TotalWorkingYears")
attrition_df_fin <- scale.feature(attrition_df_ex,numeric.vars)

attrition_df_fin$Attrition <- ifelse(attrition_df_fin$Attrition == "Yes" , 1, 0)
attrition_df_fin$Attrition <- as.factor(attrition_df_fin$Attrition)

#filter the integer coloumns
filterintegercols <-  Filter(is.integer,attrition_df_fin)
filternumericcols <-  Filter(is.numeric,attrition_df_fin)

names(filterintegercols)
names(filternumericcols)
correlation_int <- cor(filterintegercols)
library(corrplot)
#visualizing the correlation
corrplot(correlation_int, method = "number") # Display the correlation coefficient

#creating the samples
set.seed(123)

#Creating Random samples for Test and Train data
#set. seed will reset the pointer..so this will fetch the same random samples 
#to all
indexes <- sample(1:nrow(attrition_df_fin),size = 0.7*nrow(attrition_df_fin))
indexes
#Split 70%,of the random data to the train samples
train.data <- attrition_df_fin[indexes,]
#split remaining 30% of random data to test samples
test.data <- attrition_df_fin [-indexes,]
test.data
names(attrition_df_fin)
#seperate the feature and class variables
test.feature.vars <- test.data[,-2] #take all except depensent variable
test.calss.var <- test.data[,2] #just taking only the dependent variable
head(test.calss.var)

table(train.data$Attrition)
prop.table(table(train.data$Attrition))
prop.table(table(test.data$Attrition))


#build a logostic regressoin model
logis.model <- glm(Attrition ~ . , data = train.data, family = "binomial")

#to view model details
summary(logis.model)
#Business interpretation : The Percent salary hike does not provide the any significance
#in the attrtion of employees - Seems the employess are happy with salary hike

#nagelkerke r-squared (We are getting R2 value as 0.4759202 for below)
library("fmsb")
NagelkerkeR2(logis.model)

#mcfadden's pseudo r-squared
library ("pscl")
pR2(logis.model)



#perform and evaluate prediction
predictlogis <- predict(logis.model, newdata = test.data, type = "response")
# the predict will return the probablity of happening 
predictlogis

#make the probalities to 1 if it is greated than 1
test.data$predidictedvalues <- ifelse(predictlogis > 0.5,1,0)
test.data$predidictedvalues
test.data$Attrition

#checking accuracy - here the mean fuction return the the percentage of 
#not matching

misclasserror <- mean(test.data$predidictedvalues != test.data$Attrition)
#printing the percentage of not matching b/w credit rating and predicted credit rating
print(misclasserror)

#printing the 1-percentage not matched --> this give percentage matched
print(paste('This is the accuracy: ', 1-misclasserror))


#Area under Curve - run syntax by syntax and see the plot
library("ROCR")
pred <- ROCR::prediction(predictlogis,test.calss.var)
pref <- ROCR::performance(prediction.obj = pred, measure = "tpr",
                          x.measure = "fpr")



plot(pref)

#Hosmer lemeshow goodness of fit test for logistic regression
#Ho: Model fits the data well
#Ha: Model does not fit the data well
library("ResourceSelection")
#converting the attrition data to numeric again to test how the data fits
#test.data$Attrition_num <- as.integer(test.data$Attrition)
test.data$Attrition_num <- ifelse(test.data$Attrition == "1" , 1, 0)
test.data$Attrition_num
test.data$predidictedvalues
hoslem.test(test.data$Attrition_num, test.data$predidictedvalues)

#confusion matrix
library(SDMTools)
confumatrx <- SDMTools::confusion.matrix(test.calss.var,test.data$predidictedvalues)
confumatrx

prop.table(confumatrx)


######################Model 2##############################

attrition_df_fin_2 <- attrition_df_fin
#We will remove the less significant variables for model2
#age
#JobLevel
#MonthlyRate
#DailyRate
#Education
#EducationField
#HourlyRate
#JobRole
names(attrition_df_fin_2)
#create df excluding less significant variables frm previous model

attrition_df_fin_2 <- attrition_df_fin_2[,c(-1,-13,-4,-18,-7,-8,-11,-14)]



#creating the samples
set.seed(123)

#Creating Random samples for Test and Train data
#set. seed will reset the pointer..so this will fetch the same random samples 
#to all

indexes_2 <- sample(1:nrow(attrition_df_fin_2),size = 0.7*nrow(attrition_df_fin_2))
indexes_2
#Split 70%,of the random data to the train samples
train.data_2 <- attrition_df_fin_2[indexes_2,]
#split remaining 30% of random data to test samples
test.data_2 <- attrition_df_fin_2 [-indexes_2,]
test.data_2
names(attrition_df_fin_2)
#seperate the feature and class variables
test.feature.vars_2 <- test.data_2[,-1] #take all except depensent variable
test.calss.var_2 <- test.data_2[,1] #just taking only the dependent variable
head(test.calss.var_2)

table(train.data_2$Attrition)
prop.table(table(train.data_2$Attrition))
prop.table(table(test.data_2$Attrition))


#build a logostic regressoin model
logis.model_2 <- glm(Attrition ~ . , data = train.data_2, family = "binomial")

#to view model details
summary(logis.model_2)
#Business interpretation : The Percent salary hike does not provide the any significance
#in the attrtion of employees - Seems the employess are happy with salary hike

#nagelkerke r-squared (We are getting R2 value as 0.4759202 for below)
library("fmsb")
NagelkerkeR2(logis.model_2)

#mcfadden's pseudo r-squared
library ("pscl")
pR2(logis.model_2)



#perform and evaluate prediction
predictlogis_2 <- predict(logis.model_2, newdata = test.data_2, type = "response")
# the predict will return the probablity of happening 
predictlogis_2

#make the probalities to 1 if it is greated than 1
test.data_2$predidictedvalues_2 <- ifelse(predictlogis_2 > 0.5,1,0)
test.data_2$predidictedvalues_2
#test.data$Attrition

#checking accuracy - here the mean fuction return the the percentage of 
#not matching

misclasserror_2 <- mean(test.data_2$predidictedvalues_2 != test.data_2$Attrition)
#printing the percentage of not matching b/w credit rating and predicted credit rating
print(misclasserror_2)

#printing the 1-percentage not matched --> this give percentage matched
print(paste('This is the accuracy: ', 1-misclasserror_2))


#Area under Curve - run syntax by syntax and see the plot
library("ROCR")
pre2_2 <- ROCR::prediction(predictlogis_2,test.calss.var_2)
pref_2 <- ROCR::performance(prediction.obj = pred, measure = "tpr",
                          x.measure = "fpr")



plot(pref_2)

#Hosmer lemeshow goodness of fit test for logistic regression
#Ho: Model fits the data well
#Ha: Model does not fit the data well
library("ResourceSelection")
#converting the attrition data to numeric again to test how the data fits
#test.data$Attrition_num <- as.integer(test.data$Attrition)
#test.data$Attrition_num <- ifelse(test.data$Attrition == "1" , 1, 0)
#test.data$Attrition_num
#test.data$predidictedvalues
#hoslem.test(test.data$Attrition_num, test.data$predidictedvalues)

#confusion matrix
library(SDMTools)
confumatrx_2 <- SDMTools::confusion.matrix(test.calss.var_2,test.data_2$predidictedvalues_2)
confumatrx_2

prop.table(confumatrx_2)
prop.table(confumatrx)

#Model 3 ######################################################### 
#Taking below important variables into considerations
#DistanceFromHome
#EnvironmentSatisfaction
#JobInvolvement
#JobSatisfaction
#OverTime
#RelationshipSatisfaction
#TrainingTimesLastYear
#YearsSinceLastPromotion
#YearsWithCurrManager
summary(logis.model)
summary(logis.model_2)

attrition_df_fin_3 <- attrition_df_fin
names(attrition_df_fin_3)
attrition_df_fin_3 <- attrition_df_fin_3[,c(2,6,9,12,20,23,26,30,31)]



#creating the samples
set.seed(123)

#Creating Random samples for Test and Train data
#set. seed will reset the pointer..so this will fetch the same random samples 
#to all

indexes_3 <- sample(1:nrow(attrition_df_fin_3),size = 0.7*nrow(attrition_df_fin_3))
indexes_3
#Split 70%,of the random data to the train samples
train.data_3 <- attrition_df_fin_3[indexes_3,]
#split remaining 30% of random data to test samples
test.data_3 <- attrition_df_fin_3 [-indexes_3,]
test.data_3
names(attrition_df_fin_3)
#seperate the feature and class variables
test.feature.vars_3 <- test.data_3[,-1] #take all except depensent variable
test.calss.var_3 <- test.data_3[,1] #just taking only the dependent variable
head(test.calss.var_3)

table(train.data_3$Attrition)
prop.table(table(train.data_3$Attrition))
prop.table(table(test.data_3$Attrition))


#build a logostic regressoin model
logis.model_3 <- glm(Attrition ~ . , data = train.data_3, family = "binomial")

#to view model details
summary(logis.model_3)
#Business interpretation : The Percent salary hike does not provide the any significance
#in the attrtion of employees - Seems the employess are happy with salary hike

#nagelkerke r-squared (We are getting R2 value as 0.223 for below)
library("fmsb")
NagelkerkeR2(logis.model_3)

#mcfadden's pseudo r-squared
library ("pscl")
pR2(logis.model_3)



#perform and evaluate prediction
predictlogis_3 <- predict(logis.model_3, newdata = test.data_3, type = "response")
# the predict will return the probablity of happening 
predictlogis_3

#make the probalities to 1 if it is greated than 1
test.data_3$predidictedvalues_3 <- ifelse(predictlogis_3 > 0.5,1,0)
test.data_3$predidictedvalues_3
#test.data$Attrition

#checking accuracy - here the mean fuction return the the percentage of 
#not matching

misclasserror_3 <- mean(test.data_3$predidictedvalues_3 != test.data_3$Attrition)
#printing the percentage of not matching b/w credit rating and predicted credit rating
print(misclasserror_3)

#printing the 1-percentage not matched --> this give percentage matched
print(paste('This is the accuracy: ', 1-misclasserror_2))


#Area under Curve - run syntax by syntax and see the plot
library("ROCR")
pre2_3 <- ROCR::prediction(predictlogis_3,test.calss.var_3)
pref_3 <- ROCR::performance(prediction.obj = pred, measure = "tpr",
                            x.measure = "fpr")



plot(pref_3)

#Hosmer lemeshow goodness of fit test for logistic regression
#Ho: Model fits the data well
#Ha: Model does not fit the data well
library("ResourceSelection")
#converting the attrition data to numeric again to test how the data fits
#test.data$Attrition_num <- as.integer(test.data$Attrition)
#test.data$Attrition_num <- ifelse(test.data$Attrition == "1" , 1, 0)
#test.data$Attrition_num
#test.data$predidictedvalues
#hoslem.test(test.data$Attrition_num, test.data$predidictedvalues)

#confusion matrix
library(SDMTools)
confumatrx_3 <- SDMTools::confusion.matrix(test.calss.var_3,test.data_3$predidictedvalues_3)
confumatrx_3

print(misclasserror)

print(misclasserror_2) #this model is good 87 % accracy and 0.43 R squared

print(misclasserror_3)


#Save the cleaned data to the .Rdata file. So we can load this DF for next techniques
save(attrition_df_fin,file="attrition_df_fin.Rdata")
save(attrition_df_fin_2,file="attrition_df_fin_2.Rdata")
save(attrition_df_fin_3,file="attrition_df_fin_3.Rdata")
