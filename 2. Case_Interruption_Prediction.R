library(plyr)
library(caret)
library(stringr)
library(glmnet)
library(e1071)
library(tree)
library(randomForest)


##########################################################################################
# NB. Please Enter the Directory Path to the Folder Containing This File After 'setwd' Below
##########################################################################################

setwd("C:/Users/jcodo/Documents/Lurger_ODoherty_Capstone")

##########################################################################################



set.seed(101)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Data Prep~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

###loading in data + initial cleanup
cases <- read.csv(file='Cases_Anonymised2.csv', header = TRUE)
clock_stop <- read.csv(file='Clock_Stopped.csv', header = TRUE)
names(cases)[names(cases) == 'ï..CaseID'] <- 'CaseID'
names(clock_stop)[names(clock_stop) == 'ï..Clock_Stopped_Cases'] <- 'Clock_Stopped_Cases'
cases <- head(cases, -8)
summary(cases)

##only including approved cases
#cases <- cases[cases$CaseApprovalStatusTypeID == 1,]
cases <- cases %>% filter(CaseApprovalStatusTypeID %in% c(1,-999))
cases$CaseApprovalStatusTypeID <- droplevels(cases$CaseApprovalStatusTypeID)

retained_rows <- c(2813, 2864, 2759, 2793, 2867, 2582, 2859, 2893, 2739, 2847, 2762, 2837, 2829, 
                   2842, 2810, 2845, 2862, 2878, 2805, 2841, 2868, 2709, 2647, 2860, 2714, 
                   2737, 2892, 2846, 2719, 3044, 2720, 2850)
retained_rows <- sort(retained_rows)
cases <- cases %>% filter(CaseTypeID %in% retained_rows)
cases <- cases %>% filter(CaseStatusTypeID %in% c(3,4))

cases$CaseStatusTypeID <- droplevels(cases$CaseStatusTypeID)

cases <- cases[-c(11, 9, 7, 2)]

cases$CreatedDate <- as.Date(cases$CreatedDate, format = '%d/%m/%Y')
cases$Actual_Date <- as.Date(cases$Actual_Date, format = '%d/%m/%Y')
cases[is.na(cases$Actual_Date),]$Actual_Date <- cases[is.na(cases$Actual_Date),]$CreatedDate
summary(cases$CreatedDate)
summary(cases$Actual_Date)

head(cases[cases$CaseTypeID == 2813,])

cases$CaseStatusTypeID <- as.numeric(as.character(cases$CaseStatusTypeID))
summary(cases)
cases$CaseStatusTypeID <- ifelse(cases$CaseStatusTypeID == 3, 0, 1)
table(cases$CaseStatusTypeID)

##defining which cases had a clock stoppage (1 = clock stopped)
cases$clock_stopped <- ifelse(cases$CaseID %in% clock_stop$Clock_Stopped_Cases, 1, 0)
cases[,c(1,11)]
table(cases$clock_stopped)
cases$clock_stopped <- as.factor(cases$clock_stopped)

###adding in the casetype clusters
cases <- (merge(kmeans_clusters, cases, by = 'CaseTypeID'))
cases$km_Scaled <- as.factor(cases$km_Scaled)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Training & Test Sets ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

##training and test sets
rows <- sample(nrow(cases))
cases <- cases[rows, ]



trainingRows <- createDataPartition(cases$CaseTypeID, p = .5, list= FALSE)
clock_train <- cases[trainingRows,]
clock_test <- cases[-trainingRows,]

# data_split <- createDataPartition(cases$CaseTypeID, p = .3, list= FALSE)
# clock_stop_cases <- cases[data_split,]
# cases_predictive <- cases[-data_split,]
# 
# clock_trainingRows <- createDataPartition(clock_stop_cases$CaseTypeID, p = .7, list= FALSE)
# clock_train <- clock_stop_cases[trainingRows,]
# clock_test <- clock_stop_cases[-trainingRows,]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Modeling~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#



#################################
######     LINEAR MODEL    ######
#################################

linmodel <- glm(clock_stopped ~  CaseTypeID + CreatedDate + km_Scaled,family = 'binomial', data = clock_train)

#using linear model to predict on clock_test data
linear_predict <- predict(linmodel, clock_test, type = 'response')
#creating confusion matrix using same method as with decision tree
p_class <- ifelse(linear_predict > .5, 1, 0)
linear_conmat <- confusionMatrix(table(p_class, clock_test$clock_stopped))
linear_conmat
summary(cases)

#################################
######    DECISION TREE    ######
#################################

# #using the decision tree model to create predictions on clock_test data
# #provides probability that a value is 1 i.e. large income
decisiontree <- tree(clock_stopped ~ CaseTypeID + km_Scaled, data = clock_train)
tree_predict <- predict(decisiontree, clock_test, type = 'vector')
tree_predict

#setting parameters for our confusion matrix.
#If tree_predict is higher than .45, we will predict 'large' income (represented by a 1). Otherwise, 'small'
tree_pclass <- ifelse(tree_predict > .45 , 1,0)
tree_pclass <- tree_pclass[,2]
tree_conmat <- confusionMatrix(table(tree_pclass, clock_test$clock_stopped))
tree_conmat

#################################
######    RANDOM FOREST    ######
#################################
summary(cases)
#creating random forest model on clock_training data
ranFor <- randomForest(clock_train[,c(1,2, 7, 11)], clock_train[,12], data = clock_train, ntree = 250, mtry = 2,na.action=na.roughfix)
head(clock_train)

#using random forest model to predict on clock_test data
rf_predict <- predict(ranFor, clock_test, type = 'prob')
summary(ranFor)
ranFor$importance
#creating confusion matrix using same method as before
rf_pclass <- ifelse(rf_predict > .45, 1,0)
rf_pclass <- rf_pclass[,2]
rf_conmat <- confusionMatrix(table(rf_pclass, clock_test[,12]))
rf_conmat
length(clock_test$clock_stopped[clock_test$clock_stopped == 0])

head(clock_test)
clock_test$clock_predict <- rf_pclass
clock_prediction <- clock_test[c(3,13)]

###################################################################################################
head(cases)


ranFor$importance



# upSampledTrain <- upSample(x = train[,1:10], y = train$clock_stopped, yname = "clock_stopped")
# 
# #Down Sampling data
# downSampledTrain <- downSample(x = train[,1:10], y = train$clock_stopped, yname = "clock_stopped")
# 
# ranForUp <- randomForest(upSampledTrain[,c(1,2, 7)], upSampledTrain[,11], data = upSampledTrain, ntree = 250, mtry = 2,na.action=na.roughfix)
# 
# #using random forest model to predict on test data
# rf_predictUp <- predict(ranForUp, test, type = 'prob')
# new_rf_predict_up <- rf_predictUp[,2]
# summary(ranForUp)
# ranForUp$importance
# #creating confusion matrix using same method as before
# rf_pclassUp <- ifelse(new_rf_predict_up > .5, 1,0)
# rf_conmatUp <- confusionMatrix(table(rf_pclassUp, test[,11]))
# rf_conmatUp

