# Predict-Employee-Attrition-using-Logistic-Regression-in-R
Predicting why so many people are leaving the company anually based on the provided employee data

The probability of attrition in XYZ company is around 15% every year . Company gets effected due to such a large proportion of Attrition every year. The issues caused due to attrition are as follows

The former employees’ projects get delayed, which makes it difficult to meet timelines, resulting in a reputation loss among consumers and partners
A sizeable department has to be maintained, for the purposes of recruiting new talent
More often than not, the new employees have to be trained for the job and/or given time to acclimatise themselves to the company.

	Let us find out the reason behind such havoc and help XYZ to retain its employees.

### Loading Libraries and theme for ggplots
```R
#loading libraries
load.libraries <- c('tidyr','dplyr','MASS','car','ggplot2','reshape2','e1071','cowplot','caTools','caret','ROCR','lubridate')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependences = TRUE)
sapply(load.libraries, require, character = TRUE)

#loading theme for ggplot
theme_HRcase <- function () { 
  theme_bw(base_size=10, base_family="Avenir") %+replace% 
    theme(
      panel.background  = element_rect(fill="gray80", colour=NA),
      plot.background = element_rect(fill="gray96", colour=NA), 
      legend.background = element_rect(fill="white", colour=NA),
      legend.key = element_rect(fill="transparent", colour=NA),
      legend.position="bottom"
    )
}
```

### Loading Data and Cleaning

```R
#loading dataset general_data.csv
general_data1<- read.csv("general_data.csv",stringsAsFactors = F)

summary(general_data1)

str(general_data1)

sum(is.na(general_data1)) #Total 28 na values found


#Performing Univariate Analysis on general_data

sapply(general_data1, function(x) sum(length(which(is.na(x)))))
#19 NA's in NumCompaniesWorked 
# 9 NA's in TotalWorkingYears

#Since there are only 28 complete rows in which NA's are found out of 4410 rows we can remove these rows
general_data<-general_data1[!is.na(general_data1$NumCompaniesWorked)&!is.na(general_data1$TotalWorkingYears),]

#Replacing Education levels with names as mentioned in Data dictionary
general_data$Education[general_data$Education == 1] <- "Below College"
general_data$Education[general_data$Education == 2] <- "College"
general_data$Education[general_data$Education == 3] <- "Bachelor"
general_data$Education[general_data$Education == 4] <- "Master"
general_data$Education[general_data$Education == 5] <- "Doctor"

#Converting Education to Factor
general_data$Education<-as.factor(general_data$Education)

summary(general_data$Education)
#Bachelor   Below College       College        Doctor        Master 
#1701             508           842           143          1188 

#Converting Job Level to factor as itis a categorical variable with categories 1,2,3,4,5
general_data$JobLevel<-as.factor(general_data$JobLevel)

summary(general_data$JobLevel)
#1    2    3    4    5 
#1619 1590  651  318  204 

#Checking the count of other categorical variables
table(general_data$Department)
table(general_data$EducationField)
table(general_data$EmployeeCount) #This column will be removed later since it has a single category
table(general_data$Gender)
table(general_data$JobRole)
table(general_data$MaritalStatus)
table(general_data$Over18) #This column will be removed later since it has a single category
table(general_data$StandardHours) #This column will be removed later since it has a single category
table(general_data$StockOptionLevel)



#Importing employee_survey data in R dataframe
employee_survey_data<-read.csv("employee_survey_data.csv",stringsAsFactors = F)

sum(is.na(employee_survey_data))   #There are 83 NA values in employee_survey_data
#In this data we can replace NA's with median value since it is survey data and not any fact as such

#Performing Univariate Analysis on employee_survey_data 

sapply(employee_survey_data, function(x) sum(length(which(is.na(x)))))
#25 NA's in EnvironmentSatisfaction
#20 NA's in JobSatisfaction
#38 NA's in WorkLifeBalance

#Replacing Environment Satisfaction with names as mentioned in Data dictionary
employee_survey_data$EnvironmentSatisfaction[employee_survey_data$EnvironmentSatisfaction == 1] <- "Low"
employee_survey_data$EnvironmentSatisfaction[employee_survey_data$EnvironmentSatisfaction == 2] <- "Medium"
employee_survey_data$EnvironmentSatisfaction[employee_survey_data$EnvironmentSatisfaction == 3] <- "High"
employee_survey_data$EnvironmentSatisfaction[employee_survey_data$EnvironmentSatisfaction == 4] <- "VeryHigh"
employee_survey_data$EnvironmentSatisfaction[is.na(employee_survey_data$EnvironmentSatisfaction)] <- "Medium"

#converting employee_survey_data$EnvironmentSatisfaction to factor
employee_survey_data$EnvironmentSatisfaction<-as.factor(employee_survey_data$EnvironmentSatisfaction)

summary(employee_survey_data$EnvironmentSatisfaction)
#High      Low   Medium VeryHigh 
#1350      845      881     1334

#Replacing Job Satisfaction with names as mentioned in Data dictionary
employee_survey_data$JobSatisfaction[employee_survey_data$JobSatisfaction == 1] <- "Low"
employee_survey_data$JobSatisfaction[employee_survey_data$JobSatisfaction == 2] <- "Medium"
employee_survey_data$JobSatisfaction[employee_survey_data$JobSatisfaction == 3] <- "High"
employee_survey_data$JobSatisfaction[employee_survey_data$JobSatisfaction == 4] <- "VeryHigh"
employee_survey_data$JobSatisfaction[is.na(employee_survey_data$JobSatisfaction)] <- "Medium"

#Changing employee_survey_data$JobSatisfaction to factor
employee_survey_data$JobSatisfaction<-as.factor(employee_survey_data$JobSatisfaction)

summary(employee_survey_data$JobSatisfaction)
#High      Low   Medium VeryHigh 
#1323      860      860     1367 

#Replacing Work Life Balance with names as mentioned in Data dictionary
employee_survey_data$WorkLifeBalance[employee_survey_data$WorkLifeBalance == 1] <- "Bad"
employee_survey_data$WorkLifeBalance[employee_survey_data$WorkLifeBalance == 2] <- "Good"
employee_survey_data$WorkLifeBalance[employee_survey_data$WorkLifeBalance == 3] <- "Better"
employee_survey_data$WorkLifeBalance[employee_survey_data$WorkLifeBalance == 4] <- "Best"
employee_survey_data$WorkLifeBalance[is.na(employee_survey_data$WorkLifeBalance)] <- "Good"

#Changing employee_survey_data$WorkLifeBalance to factor
employee_survey_data$WorkLifeBalance<-as.factor(employee_survey_data$WorkLifeBalance)

summary(employee_survey_data$WorkLifeBalance)
#Bad   Best Better   Good 
#239    454   2660   1057

setdiff(employee_survey_data$EmployeeID,general_data1$EmployeeID)
#Employees in general_data and employee_survey_data are same. So we merge them

#Merging employee_survey_data with general_data into merge_emp_data_1
merge_emp_data1<-merge(general_data,employee_survey_data)

sum(is.na(merge_emp_data1))
#no NA values are found now in merged dataset


#reading manager_survey_data in R dataframe
manager_survey_data<-read.csv("manager_survey_data.csv",stringsAsFactors = F)

summary(manager_survey_data)

sum(is.na(manager_survey_data))   #no NA values found

#Performing univariate analysis on manager_survey_data

#Replacing Job Involvement with names as mentioned in Data dictionary
manager_survey_data$JobInvolvement[manager_survey_data$JobInvolvement == 1] <- "Low"
manager_survey_data$JobInvolvement[manager_survey_data$JobInvolvement == 2] <- "Medium"
manager_survey_data$JobInvolvement[manager_survey_data$JobInvolvement == 3] <- "High"
manager_survey_data$JobInvolvement[manager_survey_data$JobInvolvement == 4] <- "VeryHigh"

#changing manager_survey_data$JobInvolvement to factor
manager_survey_data$JobInvolvement<-as.factor(manager_survey_data$JobInvolvement)

summary(manager_survey_data$JobInvolvement)
#High      Low   Medium VeryHigh 
#2604      249     1125      432

#Replacing Performance Rating with names as mentioned in Data dictionary
manager_survey_data$PerformanceRating[manager_survey_data$PerformanceRating == 1] <- "Low"
manager_survey_data$PerformanceRating[manager_survey_data$PerformanceRating == 2] <- "Good"
manager_survey_data$PerformanceRating[manager_survey_data$PerformanceRating == 3] <- "Excellent"
manager_survey_data$PerformanceRating[manager_survey_data$PerformanceRating == 4] <- "Outstanding"

#Changing manager_survey_data$PerformanceRating to factor
manager_survey_data$PerformanceRating<-as.factor(manager_survey_data$PerformanceRating)

summary(manager_survey_data$PerformanceRating)
#3    4 
#3732  678

setdiff(manager_survey_data$EmployeeID,general_data1$EmployeeID)
#Employees in general_data and manager_survey_data are same. So we merge them in merge_emp_data2

#merge manager_survey_data with merge_emp_data1 to form merge_emp_data2
merge_emp_data2<-merge(merge_emp_data1,manager_survey_data)

sum(is.na(merge_emp_data2)) #no NA values generated after merging dataframes

#reading in_time of employees
in_time<-read.csv("in_time.csv",stringsAsFactors = F)
summary(in_time)

#Checking if in_time and employee_survey_data contains information for same employees
setdiff(in_time$X,general_data1$EmployeeID)
#Since difference in their employee ids is 0 for all rows each rows of 
#employee_survey_data and in_time have same employee data


out_time<-read.csv("out_time.csv",stringsAsFactors = F)
summary(out_time)

#Checking if out_time and employee_survey_data contains information for same employees
setdiff(out_time$X,general_data1$EmployeeID)
#Since difference in their employee ids is 0 for all rows each rows of 
#employee_survey_data and out_time have same employee data


#remove employeeID to convert total in_time dataframe to number using sapply
in_time_n<-data.frame(in_time[,-1])

#remove employeeID to convert total in_time dataframe to number using sapply
out_time_n<-data.frame(out_time[,-1])


#function to convert time to number
myfunc<-function(x){
  x<-as.numeric(as.POSIXct(strptime(x, "%Y-%m-%d %H:%M:%S")))
  x<-x/3600
  return (x) 
}


#convert in_time to number
hours_in_time<-data.frame(sapply(in_time_n,FUN=myfunc))
#hours_in_time$EmployeeID=in_time$X

#convert out_time to number
hours_out_time<-data.frame(sapply(out_time_n,FUN=myfunc))
#hours_out_time$EmployeeID=out_time$X

#Taking difference between in times and out times
hours_time<-hours_out_time-hours_in_time


#Create empty dataframe with rows for all employees and 2 columns for EmployeeID and average per day time of each employee

avgtimeperday<-data.frame(matrix(NA, nrow = nrow(hours_in_time), ncol = 2))
avgtimeperday$X2<-rowMeans(hours_time[,-262],na.rm=T)
avgtimeperday$X1<-in_time$X
colnames(avgtimeperday)<-c("EmployeeID","timeperday")

summary(avgtimeperday$timeperday)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#5.951   6.673   7.407   7.701   8.369  11.031 

#merge merge_emp_data2 with averagetimeperday based on EmployeeID into merge_emp_data3
merge_emp_data3<-merge(merge_emp_data2,avgtimeperday)


##Calculating Leaves of employees
a<- apply(in_time, 1, function(x) sum(is.na(x)))
b<- apply(out_time, 1, function(x) sum(is.na(x)))

mean(a)-mean(b)       #0
median(a)-median(b)   #0
#Since difference between means and medians of in_time and out_time is zero we can conclude that 
#there is no occurence where there is NA for in_time and a value for out_time or vice versa

#So now we can use any one in_time or out_time for calculating leaves of employees
#creating empty dataframe with 3 columns:EmployeeID, Number of leaves and flag for defaulter
defaulters<-data.frame(matrix(NA, nrow = nrow(in_time), ncol = 2))

#assigning employeeID for first column
defaulters$X1<-in_time$X

#assigning number of leaves for secong column
defaulters$X2<-a

#assigning 1 for defaulter(value > median(leaves of total employees)) and 0 for others
#defaulters$X3<- ifelse(defaulters$X2>median(defaulters$X2),1,0)

#removing column with total leaves. This column is removed since if we include it in our model 
#it will be difficult to convey the results to the business, so as to how much value sould be a decider for employee leaving the organization
#defaulters<-defaulters[,-2]

colnames(defaulters)<-c("EmployeeID","leaves")

#merge previous merged dataframe with defaulters dataset
merge_emp_data4<-merge(merge_emp_data3,defaulters)

#converting dependent values attrition to numbers Yes=1 and no=0
merge_emp_data4$Attrition<- ifelse(merge_emp_data4$Attrition=="Yes",1,0)

(sum(merge_emp_data4$Attrition)/nrow(merge_emp_data4))*100
#around 16.08% of employees leave the company every year

#removing column Over18,EmployeeCount and StandardHours since it contains only 1 unique value
unique(merge_emp_data4$Over18)
unique(merge_emp_data4$EmployeeCount)
unique(merge_emp_data4$StandardHours)

merge_emp_data4<-within(merge_emp_data4,rm(Over18,EmployeeCount,StandardHours))
```

### Bivariate Analysis

```R
#-----------------------------------Attrition Based on Age------------------------------#


ggplot(as.data.frame(merge_emp_data4), aes(x = Age,fill=factor(Attrition))) +
  geom_histogram(breaks=seq(18, 60, by = 1),col="black" )+
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Age",y = "Attrition",
       title = "Fig 1: Proportion of Attrition Based on Age" ) +theme_HRcase()+
  guides(fill=guide_legend(title="Attrition"))

#Freshers are more likely attrite
```


