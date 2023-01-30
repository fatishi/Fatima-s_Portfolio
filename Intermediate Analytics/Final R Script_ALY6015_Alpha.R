# Group Project for ALY 6015-21495
# TELCO CUSTOMER CHURN

# Team Alpha:
#Aytaj Khankishiyeva
#Fatima Nurmakhamadova
#David Belyaev
#Vaibhav Arora
#Xiaolu Shen

#February 17, 2022

################################################################################
# Import the needed packages
################################################################################
library(readr)
library(plyr)
# To use advanced confusion matrix
#create the data partition for training and confusion matrix (wrapper function)
library (caret)
# Help with visualizing logistic regression
library(ggplot2)
library(gridExtra) # for visualizations, create the grid or layout
# install.packages("pROC")
library(pROC) # create the receiver of operating characteristics and the area under the curve.
# Help with showing coefficients and create the plot
library(coefplot)
# install.packages('e1071')
library(e1071)
#Helps for doing the kind of basic data analysis as descriptive statistics
library(psych)
# Help with data manipulation operations 
library(dplyr)
library(GGally)
library(glmnet)
library(tidyverse)
library(MASS)


################################################################################
# Import the data 
################################################################################
# Downloading the data set using the file.choose()
# please select the file <Telco-Customer-Churn.csv>
OGdata <- read.csv(file.choose(), header = T,  stringsAsFactors = TRUE) 
# OGdata # view the data set


################################################################################
# Data cleaning and preparations
################################################################################
# 1. missing values:
# Checking for missing values
any(is.na(OGdata))
#Checking the type of the variables and lengths
summary(OGdata) # 11 Na values exist in the column 'TotalCharges'

# Substituting missing values with the column mean (for all the columns)
for (cols in colnames(OGdata)) {
  if (cols %in% names(OGdata[,sapply(OGdata, is.numeric)])) {
    OGdata<-OGdata%>%
      mutate(!!cols := replace(!!rlang::sym(cols), is.na(!!rlang::sym(cols)), 
                               mean(!!rlang::sym(cols), na.rm=TRUE)))
  }
}

# Checking for missing values to make sure no more missing values are present
any(is.na(OGdata))

# target response: Churn 
table(OGdata$Churn) # 5174 No, 1869 Yes

# Delete customerID column as it only contains unique identifier which is no 
# use for the analysis and modeling
OGdata <- OGdata[,-1]

# Convert the columns: SeniorCitizen into factors
OGdata$SeniorCitizen <- as.factor(OGdata$SeniorCitizen)

# From the summary function, here are three levels in the column multiplelines, 
# internetService, OnlineSecurity, OnlineBackup, DeviceProtection, TechSupport, 
# StreamingTV, StreamingMovies, Contract. However, according to the glosarry, 
# there are only two levels for these column. Thus, we take this as mistakly
# recorded data, and convery them into No
OGdata[OGdata == "No internet service" | OGdata == "No phone service"] <- "No"

################################################################################
# EDA
################################################################################

#Performing EDA using descriptive statistics
summary(OGdata)

# Statistics summary for discrete & continuous variables for tenure, 
# MonthlyCharges, and TotalCharges
describe(OGdata$tenure)
describe(OGdata$MonthlyCharges)
describe(OGdata$TotalCharges)

# Visualize the variable's distribution against churn indicator:
# Continuous Variables:  
# tenure - box plot based on churn
pTenure <- ggplot(OGdata, aes(Churn, tenure, color = Churn)) 
pTenure + geom_boxplot() +
  labs(x = "Churn Indicator", y = "Tenure", title = "Tenure Distribution")

# Monthly Charges - box plot based on churn
pMonthlyCharges <- ggplot(OGdata, aes(Churn, MonthlyCharges, color = Churn)) 
pMonthlyCharges + geom_boxplot() +
  labs(x = "Churn Indicator", y = "Monthly Charges ($)", title = "Monthly Charges Distribution")

# Total Charges - box plot and bar based on churn
# box plot
pTotalCharges <- ggplot(OGdata, aes(Churn, TotalCharges, color = Churn)) 
pTotalCharges + geom_boxplot() +
  labs(x = "Churn Indicator", y = "Total Charges ($)", title = "Total Charges Distribution")

##########################################

# Categorical variable:
# gender - barplot based on churn
pGender <- ggplot(OGdata, aes(gender, group = Churn, color = Churn)) 
pGender + geom_bar(size = 1.5) +
  labs(x = "Gender", y = "Frequncy", title = "Gender Distribution") 

# SeniorCitizen - barplot based on churn
pSeniorCitizen <- ggplot(OGdata, aes(SeniorCitizen, group = Churn, color = Churn)) 
pSeniorCitizen + geom_bar(size = 1.5) +
  labs(x = "SeniorCitizen", y = "Frequncy", title = "SeniorCitizen Distribution") 

# Partner - barplot based on churn
pPartner <- ggplot(OGdata, aes(Partner, group = Churn, color = Churn)) 
pPartner + geom_bar(size = 1.5) +
  labs(x = "Partner ", y = "Frequncy", title = "Partner  Distribution") 

# Dependents - barplot based on churn
pDependents <- ggplot(OGdata, aes(Dependents, group = Churn, color = Churn)) 
pDependents + geom_bar(size = 1.5) +
  labs(x = "Dependents", y = "Frequncy", title = "Dependents Distribution") 

# Phone Service - barplot based on churn
pPhoneService <- ggplot(OGdata, aes(PhoneService, group = Churn, color = Churn)) 
pPhoneService + geom_bar(size = 1.5) +
  labs(x = "PhoneService", y = "Frequncy", title = "Phone Service Distribution") 

# Contract - barplot based on churn
pContract <- ggplot(OGdata, aes(Contract, group = Churn, color = Churn)) 
pContract + geom_bar(size = 1.5) +
  labs(x = "Contract", y = "Frequncy", title = "Contract Distribution") 

# PaymentMethod - barplot based on churn
pPaymentMethod <- ggplot(OGdata, aes(PaymentMethod, group = Churn, color = Churn)) 
pPaymentMethod + geom_bar(size = 1.5) +
  labs(x = "Payment Method", y = "Frequncy", title = "Payment Method Distribution") 

# PayperlessBilling - barplot based on churn
pPayperlessbill <- ggplot(OGdata, aes(PaperlessBilling, group = Churn, color = Churn)) 
pPayperlessbill + geom_bar(size = 1.5) +
  labs(x = "Paperless Billing", y = "Frequncy", title = "Contract Distribution") 

# DeviceProtection - barplot based on churn
pDevicePr <- ggplot(OGdata, aes(DeviceProtection, group = Churn, color = Churn)) 
pDevicePr + geom_bar(size = 1.5) +
  labs(x = "Device Protection", y = "Frequncy", title = "Payment Method Distribution") 

# Put all the barplots side by side
require(gridExtra)
plot1 <- pGender + geom_bar(size = 1.5) +
  labs(x = "Gender", y = "Frequncy", title = "Gender Distribution")
plot2 <- pSeniorCitizen + geom_bar(size = 1.5) +
  labs(x = "Senior Citizen", y = "Frequncy", title = "Senior Citizen Distribution") 
plot3 <- pPartner + geom_bar(size = 1.5) +
  labs(x = "Partner ", y = "Frequncy", title = "Partner Distribution") 
plot4 <- pDependents + geom_bar(size = 1.5) +
  labs(x = "Dependents", y = "Frequncy", title = "Dependents Distribution") 
plot5 <- pPhoneService + geom_bar(size = 1.5) +
  labs(x = "Phone Service", y = "Frequncy", title = "Phone Service Distribution")
plot6 <- pContract + geom_bar(size = 1.5) +
  labs(x = "Contract", y = "Frequncy", title = "Contract Distribution") 
plot7 <- pPaymentMethod + geom_bar(size = 1.5) +
  labs(x = "Payment Method", y = "Frequncy", title = "Payment Method Distribution")
plot8 <- pPayperlessbill + geom_bar(size = 1.5) +
  labs(x = "Paperless Billing", y = "Frequncy", title = "Paperless Billing Distribution") 
plot9 <- pDevicePr + geom_bar(size = 1.5) +
  labs(x = "Device Protection", y = "Frequncy", title = "Device Protection Distribution") 
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9, ncol=3)

# ggpair plots: 
# Pair plot of the variables identified in the initial analysis including 
# TotalCharges,Partner,Dependents, tenure, MonthlyCharges and Churn
# Create a subset of the dataset
OGdataplot <- subset(OGdata,
                     select = c(TotalCharges, Partner, Dependents, tenure, MonthlyCharges, Churn))
OGdataplot

OGdataplot %>% 
  ggpairs(., mapping = ggplot2::aes(colour=Churn), 
          lower = list(continuous = wrap("smooth", alpha = 0.3, size=0.1)))

################################################################################
# Variable 'Tenure' modifications
################################################################################

# splitting tenure into factors and mutating the column
OGdata <- mutate(OGdata, ten_fact = tenure)

#mutating tenure column to 0-1 years of tenure
OGdata$ten_fact[OGdata$ten_fact >=0 & OGdata$ten_fact <= 12] <- '0-1year'
#mutating tenure column to 1-2 years of tenure
OGdata$ten_fact[OGdata$ten_fact> 12 & OGdata$ten_fact <= 24] <- '1-2years'
#mutating tenure column to 2-3 years of tenure
OGdata$ten_fact[OGdata$ten_fact > 24 & OGdata$ten_fact <= 36] <- '2-3years'
#mutating tenure column to 3-4 years of tenure
OGdata$ten_fact[OGdata$ten_fact > 36 & OGdata$ten_fact <= 48] <- '3-4years'
#mutating tenure column to 4-5 years of tenure
OGdata$ten_fact[OGdata$ten_fact > 48 & OGdata$ten_fact <= 60] <- '4-5years'
#mutating tenure column to 5-6 years of tenure
OGdata$ten_fact[OGdata$ten_fact > 60 & OGdata$ten_fact <= 72] <- '5-6years'

#converting tenure into factors
OGdata$ten_fact <- as.factor(OGdata$ten_fact)

# remove the column --- old tenure column (the fifth column)
OGdata <- OGdata[,-5]

#############################################################################
# logistic regression 
#############################################################################

# - Split the data into a train and test set

# check the data type in the data set
# str(OGdata)  # the SeniorCitizen is discrete instead of factors 
# which it should be, change it:
# Making SeniorCitizen - variable as a factor
OGdata$SeniorCitizen <- as.factor(OGdata$SeniorCitizen)

# set the random seeds
set.seed(1234)

# Get 80% of random row numbers
dt <- sample(nrow(OGdata), nrow(OGdata) * 0.8)

# Get training data which include 80% of rows 
train <- OGdata[dt,]
# Get rest 20% test data 
test <- OGdata[-dt,]

# Number of rows in original data set
dim(OGdata)

# Verify number of rows in training data set
dim(train)

# Verify number of rows in testing data set
dim(test)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
################################################################################
# Generalized Linear Models (GLM)
################################################################################

#- Run logistic regression using binomial family and logit link function
# 1- Fit a logistic regression model with all variables
fit.full <- glm(Churn ~ ., data = train, family=binomial(link="logit"))

#Show fitted model
summary(fit.full)


# 2- Fit a logistic regression model with only significant variables
fit.reduced <- glm(Churn ~ SeniorCitizen + MultipleLines+Contract+
                     PaperlessBilling+PaymentMethod+ten_fact, 
                   data = train, family=binomial(link="logit"))

#Show fitted model
summary(fit.reduced)

# 3- Perform stepwise selection with full model
OGdata_Step <- stepAIC(fit.full, direction = 'both')

#fit a model with best predictors defined in stepwise selection
Stepwise_model <- OGdata_Step

#Show fitted model
Stepwise_model
summary(Stepwise_model)

#Run the anova() function to compare three models
anova(fit.reduced, fit.full, Stepwise_model, test="Chisq")


#Show the regression coefficients of all models putting 
#the results on an odds scale (odds)
exp(coef(fit.reduced))
exp(coef(Stepwise_model))
exp(coef(fit.full))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Train set predictions

######### 1 - Reduced model 
# Predict Private "Yes" or "No" based on other variables
probabilities.train_reduced <- predict(fit.reduced, newdata=train, type="response")

#Converting probabilities into Yes/No values using 0.5 threshold
# Values with probability >= 50% will be set as Yes(Churn == Yes). 
# Others will be set as No
predicted.classes.min_reduced <- as.factor(ifelse(probabilities.train_reduced 
                                                  >= 0.5, "Yes", "No"))

#Show the model accuracy with advanced 
#confusion matrix of the train set
confusionMatrix(predicted.classes.min_reduced, 
                train$Churn, positive = 'Yes')


######### 2 - Full model 
# Predict Private "Yes" or "No" based on other variables
probabilities.train_full <- predict(fit.full, newdata=train, type="response")

#Converting probabilities into Yes/No values using 0.5 threshold
# Values with probability >= 50% will be set as Yes(Churn == Yes). 
#Others will be set as No
predicted.classes.min_full <- as.factor(ifelse(probabilities.train_full >= 0.5, "Yes", "No"))

#Show the model accuracy with advanced 
#confusion matrix of the train set
confusionMatrix(predicted.classes.min_full, 
                train$Churn, positive = 'Yes')


######### 3 - Stepwise model
# Predict Private "Yes" or "No" based on other variables
probabilities.train_step <- predict(Stepwise_model, newdata=train, type="response")

#Converting probabilities into Yes/No values using 0.5 threshold
# Values with probability >= 50% will be set as Yes(Churn == Yes). Others will be set as No
predicted.classes.min_step <- as.factor(ifelse(probabilities.train_step >= 0.5, "Yes", "No"))

#Show the model accuracy with advanced 
#confusion matrix of the train set
confusionMatrix(predicted.classes.min_step, 
                train$Churn, positive = 'Yes')


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Test set predictions

######### 1 - Reduced model 

# Predict Private "Yes" or "No" based on other variables
probabilities.test_reduced <- predict(fit.reduced, newdata=test, type="response")

#Converting probabilities into Yes/No values using 0.5 threshold
# Values with probability >= 50% will be set as Yes(Churn == Yes). 
#Others will be set as No
predicted.classes.min_test_reduced <- as.factor(ifelse(probabilities.test_reduced
                                                       >= 0.5, "Yes", "No"))

#Show the model accuracy with advanced 
#confusion matrix of the test set
confusionMatrix(predicted.classes.min_test_reduced, 
                test$Churn, positive = 'Yes')


######### 2 - Full model 


# Predict Private "Yes" or "No" based on other variables
probabilities.test_full <- predict(fit.full, newdata=test, type="response")

#Converting probabilities into Yes/No values using 0.5 threshold
# Values with probability >= 50% will be set as Yes(Churn == Yes). 
#Others will be set as No
predicted.classes.min_test_full <- as.factor(ifelse(probabilities.test_full >= 0.5, "Yes", "No"))

#Show the model accuracy with advanced 
#confusion matrix of the test set
confusionMatrix(predicted.classes.min_test_full, 
                test$Churn, positive = 'Yes')


######### 3 - Stepwise model

# Predict Private "Yes" or "No" based on other variables
probabilities.test_step <- predict(Stepwise_model, newdata=test, type="response")

#Converting probabilities into Yes/No values using 0.5 threshold
# Values with probability >= 50% will be set as Yes(Churn == Yes). Others will be set as No
predicted.classes.min_test_step <- as.factor(ifelse(probabilities.test_step >= 0.5, "Yes", "No"))

#Show the model accuracy with advanced 
#confusion matrix of the test set
confusionMatrix(predicted.classes.min_test_step, 
                test$Churn, positive = 'Yes')


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Showing and explaining metrics for Accuracy, Precision, Recall, 
#and Specificity of the model in the train set

######### 1 - Reduced model 

#Assign confusion matrix into variable calling metrics
perf_metric_reduced<-confusionMatrix(predicted.classes.min_test_reduced, 
                                     test$Churn, positive = 'Yes')
#Show performance metric
perf_metric_reduced$byClass


######### 2 - Full model 

#Assign confusion matrix into variable calling metrics
perf_metric_full<-confusionMatrix(predicted.classes.min_test_full, 
                                  test$Churn, positive = 'Yes')
#Show performance metric
perf_metric_full$byClass


######### 3 - Stepwise model

#Assign confusion matrix into variable calling metrics
perf_metric_step <-confusionMatrix(predicted.classes.min_test_step, 
                                   test$Churn, positive = 'Yes')
#Show performance metric
perf_metric_step$byClass


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot and interpret the ROC curve from the data standpoint

######### 1 - Reduced model 

# Generate data for the ROC curve
myROC_reduced <- roc(test$Churn, probabilities.test_reduced)
# Show ROC/AUC data
myROC_reduced
#Plot ROC curve
plot(myROC_reduced, col="blue", ylab="Sensitivity - TP Rate", 
     xlab="Specificity - FP Rate")


######### 2 - Full model 

# Generate data for the ROC curve
myROC_full <- roc(test$Churn, probabilities.test_full)
# Show ROC/AUC data
myROC_full
#Plot ROC curve
plot(myROC_full, col="blue", ylab="Sensitivity - TP Rate", 
     xlab="Specificity - FP Rate")


######### 3 - Stepwise model

# Generate data for the ROC curve
myROC_step <- roc(test$Churn, probabilities.test_step)
# Show ROC/AUC data
myROC_step
#Plot ROC curve
plot(myROC_step, col="blue", ylab="Sensitivity - TP Rate", 
     xlab="Specificity - FP Rate")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Calculate the area under the ROC curve (AUC)

######### 1 - Reduced model 
auc_reduced <- auc(myROC_reduced)
auc_reduced

######### 2 - Full model 
auc_full <- auc(myROC_full)
auc_full

######### 3 - Stepwise model
auc_step <- auc(myROC_step)
auc_step

####################################################  



################################################################################
# LASSO  regression
################################################################################
# convert the response type into factors
OGdata$Churn <- as.factor(OGdata$Churn)

# set the random seeds
set.seed(1234)

# Get 80% of random row numbers
dt <- sample(nrow(OGdata), nrow(OGdata) * 0.8)

# Get training data which include 80% of rows 
train <- OGdata[dt,]
# Get rest 20% test data 
test <- OGdata[-dt,]

# create input matrices

# use model.matrix to separately build matrices for training and test
trainX <- model.matrix(Churn ~., train)

# somehow the matrix has an additional column <intercept>, remove it
trainX <- trainX[,-1]

# similar as the previous step, create input matrix for input matrix of test set.
testX <- model.matrix(Churn ~., test)
testX <- testX[,-1]


# pass the response <Grad.Rate> values into the train and test matrices:
trainY <- train$Churn
testY <- test$Churn


# LASSO regularization (L1) 
set.seed(12345)

# Use cv.glmnet function to cross validate to find the best lambda values
# The cross validation method I chose is K-fold and here, K is 10.
CV.L1 <- cv.glmnet(trainX, trainY, alpha = 1, nfolds = 10, family="binomial", type.measure='auc')

# As using K-fold cross validation method, the outputs values below may be 
# different from the values in the report.

# lambda.min
CV.L1$lambda.min
# log of lambda.min
log(CV.L1$lambda.min)
# one-standard-error lambda
CV.L1$lambda.1se
# log of one-standard-error lambda
log(CV.L1$lambda.1se)

# plot the result of CV.L2
plot(CV.L1)


# Fit the models with previous lambdas
# Lambda.min
modL1.min <- glmnet(trainX, trainY, alpha = 1, lambda = CV.L1$lambda.min, family="binomial")
# View the coefficients
coef(modL1.min)

# Lambda.1se
modL1.1se <- glmnet(trainX, trainY, alpha = 1, lambda = CV.L1$lambda.1se, family="binomial")
# View the coefficients
coef(modL1.1se) 



# fit model against the training set (minimum lambda)
L1.MinPredictTrain <- predict(modL1.min, s = CV.L1$lambda.min, newx = trainX, type = "response")
# fit model against the test set (minimum lambda)
L1.MinPredictTest <- predict(modL1.min, s = CV.L1$lambda.min, newx = testX, type = "response")

# fit model against the training set (1se lambda)
L1.1sePredictTrain <- predict(modL1.1se, s = CV.L1$lambda.1se, newx = trainX, type = "response")
# fit model against the test set (1se lambda)
L1.1sePredictTest <- predict(modL1.1se, s = CV.L1$lambda.1se, newx = testX, type = "response")


# performance for train set (minimum lambda)
# use ifelse function to classify the response values
predicted.train.min <- as.factor(ifelse(L1.MinPredictTrain >=0.5, "Yes", "No"))
# create the confusion matrix
confusionMatrix(predicted.train.min, trainY, positive = 'Yes') 


# performance for train set (1se lambda)
# use ifelse function to classify the response values
predicted.train.1se <- as.factor(ifelse(L1.1sePredictTrain >=0.5, "Yes", "No"))
# create the confusion matrix
confusionMatrix(predicted.train.1se, trainY, positive = 'Yes') 


# performance for test set (minimum lambda)
predicted.test.min <- as.factor(ifelse(L1.MinPredictTest >=0.5, "Yes", "No"))
# create the confusion matrix
confusionMatrix(predicted.test.min, testY, positive = 'Yes') 

# performance for test set (1se lambda)
predicted.test.1se <- as.factor(ifelse(L1.1sePredictTest >=0.5, "Yes", "No"))
# create the confusion matrix
confusionMatrix(predicted.test.1se, testY, positive = 'Yes') 


# ROC curve and AUC value for minlambda fitting on train
ROCMinTrain <- roc(train$Churn, L1.MinPredictTrain)
# AUC 
ROCMinTrain
#Plot ROC curve
plot(ROCMinTrain, col="blue", ylab="Sensitivity - TP Rate", xlab="Specificity - FP Rate")

# ROC curve and AUC value for minlambda fitting on test
ROCMinTest <- roc(test$Churn, L1.MinPredictTest)
# AUC 
ROCMinTest
#Plot ROC curve
plot(ROCMinTest, col="blue", ylab="Sensitivity - TP Rate", xlab="Specificity - FP Rate")

# ROC curve and AUC value for one standard lambda fitting on train
ROC1seTrain <- roc(train$Churn,L1.1sePredictTrain)
# AUC 
ROC1seTrain
#Plot ROC curve
plot(ROC1seTrain, col="blue", ylab="Sensitivity - TP Rate", xlab="Specificity - FP Rate")

# ROC curve and AUC value for one standard lambda fitting on train
ROC1seTest <- roc(test$Churn,L1.1sePredictTest)
# AUC 
ROC1seTest
#Plot ROC curve
plot(ROC1seTest, col="blue", ylab="Sensitivity - TP Rate", xlab="Specificity - FP Rate")

# put all the AUC values into one data frame
df_AUC <- data.frame(c('Minimum Lambda on Train','Minimum Lambda on Test', '1se Lambda on Train', '1se lambda on Test'), 
                     c(ROCMinTrain$auc, ROCMinTest$auc, ROC1seTrain$auc, ROC1seTest$auc))
names(df_AUC) <- c('model','AUC' )

