library(plyr)
library(caret)
library(stringr)
library(glmnet)
library(kernlab)
library(varImp)


##########################################################################################
# NB. Please Enter the Directory Path to the Folder Containing This File After 'setwd' Below
##########################################################################################

setwd("C:/Users/jcodo/Documents/Lurger_ODoherty_Capstone")

##########################################################################################



set.seed(101)

cases <- read.csv(file='Cases_Anonymised2.csv', header = TRUE)
names(cases)[names(cases) == 'ï..CaseID'] <- 'CaseID'
cases <- head(cases, -8)
summary(cases)
##only including approved cases
# cases <- cases[cases$CaseApprovalStatusTypeID == 1,]

retained_rows <- c(2813, 2864, 2759, 2793, 2867, 2582, 2859, 2893, 2739, 2847, 2762, 2837, 2829, 
                   2842, 2810, 2845, 2862, 2878, 2805, 2841, 2868, 2709, 2647, 2860, 2714, 
                   2737, 2892, 2846, 2719, 3044, 2720, 2850)
retained_rows <- sort(retained_rows)
cases <- cases %>% filter(CaseTypeID %in% retained_rows)

cases <- cases[-c(11, 9, 2)]
head(cases)
summary(cases)
##formatting start and end dates
cases$CreatedDate <- as.Date(cases$CreatedDate, format = '%d/%m/%Y')
cases$Actual_Date <- as.Date(cases$Actual_Date, format = '%d/%m/%Y')
cases[is.na(cases$Actual_Date),]$Actual_Date <- cases[is.na(cases$Actual_Date),]$CreatedDate
cases$CaseApprovalDate <- as.Date(cases$CaseApprovalDate, format = '%d/%m/%Y')

##only including cases where approval date is after created date
cases <- cases[cases$CaseApprovalDate >= cases$CreatedDate, ]
cases <- na.omit(cases)
a <- cases$Actual_Date
b <- cases$CreatedDate
c <- cases$CaseApprovalDate - cases$Actual_Date
c
cases$time_taken <- c


#time taken = difference betwwen creation and approval
# cases$time_taken <- cases$CaseApprovalDate - cases$CreatedDate
cases$time_taken <- as.integer(cases$time_taken)

#removing original date columns
cases <- cases[-c(9)]
cases <- cases[!is.na(cases$time_taken),]

###merging in clusters
cases <- (merge(kmeans_clusters, cases, by = 'CaseTypeID'))
cases <- cases %>% filter(CaseID %in% clock_test$CaseID)
cases <- (merge(clock_prediction, cases, by = 'CaseID'))

head(cases)


###removing cases that are predicted to clock stop
cases <- cases[cases$clock_predict == 0,]
cases$clock_stopped <- ifelse(cases$CaseID %in% clock_stop$Clock_Stopped_Cases, 1, 0)
# cases <- cases[cases$clock_stopped == 0,]
head(cases)
d <- cases[c(1, 2, 3, 4, 8, 9, 11, 12, 13, 14)]
head(d)
class(d$CaseTriggerReasonID)
d[c(3, 4, 7)] <- scale(d[c(3, 4, 7)])
summary(d)


###training and test split
smp_size <- floor(0.70 * nrow(d))
train_ind <- sample(seq_len(nrow(d)), size = smp_size)
train <- d[train_ind, ]
test <- d[-train_ind, ]

head(train)

###linear model
lin_model <- glm(time_taken ~  CaseTypeID + km_Scaled + Actual_Date + CreatedDate + ConcurrencyRowVersion + CaseTriggerReasonID + clock_stopped, data = train)
predictions <- predict(lin_model,test)
test$preds <- predictions
RMSE_Base <- RMSE(test$preds, test$time_taken)
RMSE_Base

d[c(3, 4, 7)] <- scale(d[c(3, 4, 7)])
summary(d)



smp_size <- floor(0.70 * nrow(d))
train_ind <- sample(seq_len(nrow(d)), size = smp_size)
train <- d[train_ind, ]
test <- d[-train_ind, ]
head(test)


###linear model
lin_model <- glm(time_taken ~  CaseTypeID + km_Scaled + Actual_Date + CreatedDate + ConcurrencyRowVersion + CaseTriggerReasonID + clock_stopped, data = train)
predictions <- predict(lin_model,test)
test$preds <- predictions



summary(lin_model)

#########################################################
# SETTING TRAIN CONTROL
#########################################################

# setting trainControl for continued use
myControl = trainControl(
  method = "cv", 
  number = 50,
  verboseIter = TRUE)

head(cases)
#ridge regression
model_ridge <- train(time_taken ~  CaseTypeID + km_Scaled + CaseTriggerReasonID + CreatedDate + ConcurrencyRowVersion + Actual_Date, train,
                     tuneGrid = expand.grid(alpha = 0, lambda = seq(0.0001, 1, length = 20)),
                     method = "glmnet",
                     trControl = myControl,
                     metric = 'RMSE',
                     preProcess = c("center", "scale"))

head(train)

ridge_predictions <- predict(model_ridge,test)
test$ridge_preds <- ridge_predictions
RMSE_Ridge <- RMSE(test$ridge_preds, test$time_taken)
RMSE_Ridge
head(test)


#lasso regression
model_lasso <- train(time_taken ~  CaseTypeID + km_Scaled + CaseTriggerReasonID + CreatedDate + ConcurrencyRowVersion + Actual_Date, train,
                     tuneGrid = expand.grid(alpha = 1, lambda = seq(0.0001, 1, length = 20)),
                     method = "glmnet",
                     trControl = myControl,
                     metric = 'RMSE',
                     preProcess = c("center", "scale"))



lasso_predictions <- predict(model_lasso,test)
test$lasso_preds <- lasso_predictions
RMSE_lasso <- RMSE(test$lasso_preds, test$time_taken)
RMSE_lasso



#Support Vector Machine

noGrid <- expand.grid(sigma = 0.2, C = 0)
SVM.noreg <- train(time_taken ~  CaseTypeID + km_Scaled + CaseTriggerReasonID + CreatedDate + ConcurrencyRowVersion + Actual_Date, 
                   train, epsilon = 0, tunGrid = noGrid, 
                   method = "svmRadial",
                   preProcess = c("center", "scale"))

SVM.noreg$results
ggplot(SVM.noreg)
SVM.noreg$bestTune
SVM.noreg$finalModel
SVM.noreg.pred <- predict(SVM.noreg, test)
RMSE_SVM_Base <- RMSE(SVM.noreg.pred, test$time_taken) ###### Measuring eprformanc with RMSE
RMSE_SVM_Base






modelLookup("svmRadial")
RadGrid <- expand.grid(sigma = seq(2^(-2), 2, 0.2), C = seq(2^-2, 2, 0.8))

SVMbase <- train(time_taken ~  CaseTypeID + km_Scaled + CaseTriggerReasonID + CreatedDate + ConcurrencyRowVersion + Actual_Date,
                 train, tuneGrid = RadGrid, epsilon = 2^c(-10, -5, 0, 1, 2, 3, 5),
                 method = "svmRadial",
                 trControl = myControl,
                 metric = 'RMSE',
                 preProcess = c("center", "scale"))
SVMbase
plot(SVMbase$finalModel)
SVMbase$bestTune
SVMbase$finalModel
SVMbase.pred <- predict(SVMbase, test)
(RMSE_SVM_Base_Tuned <- RMSE(SVMbase.pred, test$time_taken))
RMSE_SVM_Base_Tuned



###L2-Regularised SVM

SVMRR <- train(time_taken ~  CaseTypeID + km_Scaled + CaseTriggerReasonID + CreatedDate + ConcurrencyRowVersion + Actual_Date,
               train,  
               method = "svmLinear3", ##L2 Regularized Support Vector Machine (dual) with Linear Kernel
               epsilon =  2^c(-10, -5, 0, 1, 2, 3, 5), #### same parameter search as RBF no regularization
               C = seq(2^-2, 2, 0.8),
               trControl = myControl,
               preProcess = c("center", "scale"))
RMSE_SVM_Ridge
SVMRR$bestTune
SVMRR$finalModel
plot(SVMRR)
SVMRR$results
SVMRR.pred <- predict(SVMRR, test)
RMSE_SVM_Ridge <- RMSE(SVMRR.pred, test$time_taken)
RMSE_SVM_Ridge
head(test)
summary(cases$time_taken)


test$SVMPreds <- SVMbase.pred





write.csv(test, 'test_results.csv')


merging <- test[c(1, 14)]
merging$SVMPreds <- round(merging$SVMPreds)
head(merging)
