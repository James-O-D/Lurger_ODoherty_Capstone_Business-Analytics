import numpy as np
import pandas as pd
import csv
from datetime import datetime as dt
print('''
  ____                                         _    _ _                 _               ____   ___  _  ___  
 |  _ \ ___  ___  ___  _   _ _ __ ___ ___     / \  | | | ___   ___ __ _| |_ ___  _ __  |___ \ / _ \/ |/ _ \ 
 | |_) / _ \/ __|/ _ \| | | | '__/ __/ _ \   / _ \ | | |/ _ \ / __/ _` | __/ _ \| '__|   __) | | | | | (_) |
 |  _ <  __/\__ \ (_) | |_| | | | (_|  __/  / ___ \| | | (_) | (_| (_| | || (_) | |     / __/| |_| | |\__, |
 |_| \_\___||___/\___/ \__,_|_|  \___\___| /_/   \_\_|_|\___/ \___\__,_|\__\___/|_|    |_____|\___/|_|  /_/ 
                                                                                                            
''')
print('Thank you for using the automatic resource allocator. \nPlease wait a moment while we assign your resources to cases.')

##########################################################
#IMPORTING CSV FILES
##########################################################

########################################################################################################################
# NB. Please Enter the Directory Path to the Folder Containing This File After each 'pd.read_csv' Prior to CSV File Name
########################################################################################################################

df_resources = pd.read_csv(r"\Users\jcodo\Documents\Lurger_ODoherty_Capstone\TD_Resources.csv")
df_cases = pd.read_csv(r"\Users\jcodo\Documents\Lurger_ODoherty_Capstone\CASES.csv")
df_cases_required_skills = pd.read_csv(r"\Users\jcodo\Documents\Lurger_ODoherty_Capstone\TD_Cases_Required_Skills.csv", index_col = 'CaseID')
df_resources_skills = pd.read_csv(r"\Users\jcodo\Documents\Lurger_ODoherty_Capstone\TD_Resources_Skills1.csv", index_col = 'HPRAPersonnelID')
skill_capacities = pd.read_csv(r"\Users\jcodo\Documents\Lurger_ODoherty_Capstone\Skill_Capacities.csv", index_col = 'Skill')
df_all = pd.read_csv(r"\Users\jcodo\Documents\Lurger_ODoherty_Capstone\cases_processed.csv")
df_all = df_all.iloc[:,1:]
unique_cases = df_all.CaseID.unique()
res_skills = df_resources_skills.iloc[:, 4:]
case_capacities = pd.read_csv(r"\Users\jcodo\Documents\Lurger_ODoherty_Capstone\case_capacity.csv", index_col = 'CaseID')
case_capacities = case_capacities.iloc[:,1:]
df_conflict = pd.read_csv(r"\Users\jcodo\Documents\Lurger_ODoherty_Capstone\Conflict_of_Interest1.csv", index_col = 'HPRAPersonnelID')
conflict = df_conflict.iloc[:,2:]
df_cases = df_cases.iloc[:,1:]
timelines = pd.read_csv(r"\Users\jcodo\Documents\Lurger_ODoherty_Capstone\Personnel_Timeline.csv", index_col = 'HPRAPersonnelID')
due_dates = pd.read_csv(r"\Users\jcodo\Documents\Lurger_ODoherty_Capstone\due_dates.csv", index_col = 'CaseID')
due_dates = due_dates.iloc[:,1:]


##########################################################
#INITIALISING VARIABLES
##########################################################

allocated_resources_for_case = []
allocated_skills_for_case = []
resource_to_check = 0
flagged_cases = {}
case_to_compare = 0
task_in_case = 0
free_days = 0
predicted_days_for_case = 0
day_counter_for_resources = 0


##########################################################
#FORMATTING DATES
##########################################################
due_dates_date = []
for i in due_dates.iloc[:, 1]:
    due_dates_date.append(dt.strptime(i, '%Y-%m-%d'))


for i in range(len(due_dates_date)):
    due_dates.iloc[i, 1] = due_dates_date[i]

timeline_dates = []
for i in timelines.columns:
    timeline_dates.append(dt.strptime(i, '%d/%m/%Y'))
timelines.columns = timeline_dates

##########################################################
#COLLATING TASKS PER CASE
##########################################################
tasks = np.zeros([len(unique_cases), 15])

for i in range(len(unique_cases)):
    task_in_case = 0
    for j in range(len(df_all)):
        if df_all.iloc[j, 0] == unique_cases[i]:
            tasks[i, task_in_case] = int(df_all.iloc[j, 2])
            task_in_case += 1
            tasks[i, task_in_case] = int(df_all.iloc[j, 3])
            task_in_case += 1
            tasks[i, task_in_case] = int(df_all.iloc[j, 4])
            task_in_case += 1

##########################################################
#DEFINING FUNCTIONS
##########################################################

def skill_match(skill, case):
    if skill != 0:
        for resource in range(len(df_resources)):
            current_capacity = df_resources.iloc[resource, 4]
            resource_to_check = df_resources.iloc[resource, 0]
            potential_added_capacity = case_capacities.iloc[case].values
            if (capacity_check(current_capacity, potential_added_capacity)) \
                    and(timeline_open(resource_to_check, case)) \
                    and (df_resources.iloc[resource, 3] < 6) \
                    and (skill in res_skills.loc[resource_to_check].values) \
                    and (resource not in allocated_resources_for_case) \
                    and no_hard_conflict(resource_to_check, case):
                soft_conflict(resource_to_check, case)
                allocated_resources_for_case.append(resource)
                allocated_skills_for_case.append(case_capacities.iloc[case])
                return True
        return False
    return True

def capacity_check(current_capacity, potential_added_capacity):
    if current_capacity + potential_added_capacity < 100:
        return True
    return False

def no_hard_conflict(resource_to_check, case):
    if conflict.ix[resource_to_check, 0] == df_cases.iloc[case, 1]:
        return False
    elif conflict.ix[resource_to_check, 1] == df_cases.iloc[case, 1]:
        return False
    return True

def soft_conflict(resource_to_check, case):
    if ((conflict.ix[resource_to_check, 2] == df_cases.iloc[case, 1]) or (conflict.ix[resource_to_check, 3] == df_cases.iloc[case, 1])):
            df_cases.iloc[case, 18] = 1

def timeline_open(resource_to_check, case):
    free_days = sum(timelines.loc[resource_to_check, '2019-10-01': str(due_dates.iloc[case, 1])])
    predicted_days_for_case = due_dates.iloc[case, 0]
    day_counter_for_resources = predicted_days_for_case
    if free_days > predicted_days_for_case:
        return True
    else:
        return False


##########################################################
#MAIN FUNCTION
##########################################################

for case in range(len(unique_cases)):
    allocated_resources_for_case = []
    allocated_skills_for_case = []
    for skill in tasks[case]:
        if not skill_match(skill, case):
            break
    for i in range(len(allocated_resources_for_case)):
        if not len(allocated_skills_for_case) == len(tasks[case]):
            break
        else:
            df_cases.iloc[case, 3 + i] = df_resources.iloc[allocated_resources_for_case[i], 0]
            df_cases.iloc[case, 2] = 1
            b = df_resources.iloc[allocated_resources_for_case[i], 3]
            df_resources.iloc[allocated_resources_for_case[i], 5 + b] = df_cases.iloc[case, 0]
            df_resources.iloc[allocated_resources_for_case[i], 3] += 1
            df_resources.iloc[allocated_resources_for_case[i], 4] += allocated_skills_for_case[i].values
            day_counter_for_resources = due_dates.iloc[case, 0]

            for j in range(len(timelines.loc[df_resources.iloc[allocated_resources_for_case[i], 0], '01/10/2019':str(due_dates.iloc[case, 1])])):
                if (timelines.loc[df_resources.ix[allocated_resources_for_case[i], 0], str(timelines.columns[j])]) != 0 and day_counter_for_resources > 0:
                    timelines.loc[df_resources.ix[allocated_resources_for_case[i], 0], str(timelines.columns[j])] = 0
                    day_counter_for_resources -= 1

timelines = timelines.reset_index(drop=True)
df_unsolved_cases = df_cases[df_cases['CaseAssigned'] == 0]
print(df_unsolved_cases.head())
print(df_cases.head())
print(df_resources.head())

##########################################################
#EXPORTING TABLES TO CSV
##########################################################
export_csv = df_resources.to_csv(r"\Users\jcodo\PycharmProjects\Capstone_Project\RESOURCES_ALLOCATED.csv",index=None, header=True)  # type: object
export_csv1 = df_cases.to_csv(r"\Users\jcodo\PycharmProjects\Capstone_Project\CASES_ALLOCATED.csv",index=None, header=True)  # type: object
export_csv2 = timelines.to_csv(r"\Users\jcodo\PycharmProjects\Capstone_Project\UPDATED_PERSONNEL_TIMELINE.csv",index=None, header=True)  # type: object
