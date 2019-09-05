library(plyr)
library(dplyr)
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


###determining cases for allocation
cases_to_allocate <- read.csv(file='TD_Cases_Required_Skills_ALL.csv', header = TRUE)
cases_to_allocate <- cases_to_allocate %>% filter(CaseID %in% test$CaseID)

cases_to_allocate <- cases_to_allocate[cases_to_allocate$Required_Skill_1 != 'NULL',]
cases_to_allocate <- cases_to_allocate[cases_to_allocate$Required_Skill_2 != 'NULL',]
cases_to_allocate <- cases_to_allocate[cases_to_allocate$Required_Skill_3 != 'NULL',]
cases_to_allocate <- cases_to_allocate[cases_to_allocate$DueDate != 'NULL',]

cases_to_allocate <- cases_to_allocate %>% group_by(CaseID) %>% slice(1:5)

write.csv(cases_to_allocate, 'allocated_cases.csv')
alloc <- cases_to_allocate[c(1, 2, 5, 6, 22, 23, 24, 27)]

alloc <- (merge(alloc, merging, by = 'CaseID'))


###adding in predicted times from Case Handling Duration model for each case
names(alloc)[names(alloc) == 'SVMPreds'] <- 'Predicted_Time'
head(alloc)


###adding in start dates for each case
case_start <- as.data.frame(unique(alloc$CaseID))
names(case_start)[names(case_start) == 'unique(alloc$CaseID)'] <- 'CaseID'
case_start$start <- as.Date('01/10/2019', format = '%d/%m/%Y')

for (i in 1:nrow(case_start))
{
  case_start[i,]$start <- case_start[i,]$start + as.integer(i / 10)
}


alloc <- (merge(alloc, case_start, by = 'CaseID'))
summary(alloc$Due)
alloc$Due <- alloc$start + 300
# alloc[alloc$Due > (alloc$start[1] + 380)]$Due <- alloc$start[1] + 380

summary(alloc$Due)
alloc <- alloc[-c(2, 4, 8)]

###adding in a capacity for each case
case_capacity <- as.data.frame(unique(alloc$CaseID))
names(case_capacity)[names(case_capacity) == 'unique(alloc$CaseID)'] <- 'CaseID'
head(case_capacity)
case_capacity$capacity <- 0
for (i in 1:nrow(case_capacity))
{
  case_capacity[i,]$capacity <- sample(1:61, 1)
}
alloc <- (merge(alloc, case_capacity, by = 'CaseID'))
head(alloc)
head(case_capacity)
write.csv(case_capacity, 'case_capacity.csv')


###adding in due dates
write.csv(alloc, 'cases_processed.csv')

due_dates <- alloc[c(1, 9, 11)]
due_dates <- due_dates %>% group_by(CaseID) %>% slice(1:1)
due_dates <- as.data.frame(due_dates)
write.csv(due_dates, 'due_dates.csv')
head(due_dates)
case_starter <- read.csv(file='case_starter.csv', header = TRUE)
names(case_starter)[names(case_starter) == 'ï..CaseID'] <- 'CaseID'

###adding in companies
list_of_cases <- alloc[c(1, 2)]
list_of_cases <- list_of_cases %>% group_by(CaseID) %>% slice(1:1)
list_of_cases <- as.data.frame(list_of_cases)
case_starter[nrow(list_of_cases),] <- 0
head(case_starter)
case_starter$CaseID <- list_of_cases$CaseID
case_starter$CompanyID <- list_of_cases$CompanyID
head(case_starter)
case_starter <- mutate_all(case_starter, ~replace(., is.na(.), 0))
write.csv(case_starter, 'CASES.csv')

