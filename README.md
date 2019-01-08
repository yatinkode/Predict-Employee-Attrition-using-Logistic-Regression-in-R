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

#### Attrition Based on Age
```R
ggplot(as.data.frame(merge_emp_data4), aes(x = Age,fill=factor(Attrition))) +
  geom_histogram(breaks=seq(18, 60, by = 1),col="black" )+
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Age",y = "Attrition",
       title = "Fig 1: Proportion of Attrition Based on Age" ) +theme_HRcase()+
  guides(fill=guide_legend(title="Attrition"))

#Freshers are more likely attrite
```
![data](https://github.com/yatinkode/Predict-Employee-Attrition-using-Logistic-Regression-in-R/blob/main/images/imageattrite.png)

#### Attrition Based on Business Travel
```R
t<-prop.table(table(merge_emp_data4$Attrition,merge_emp_data4$BusinessTravel),2)

ggplot(as.data.frame(t), aes(x = Var2,y=Freq,fill=Var1)) +
  geom_bar(position="stack",stat="identity",col="black") +
  theme(plot.title = element_text(hjust = 0.8)) +
  labs(x = "Business Travel",y = "Percent of Attrition",
       title = "Fig 1: Proportion of Attrition Based on Business Travel" ) +theme_HRcase()+
  guides(fill=guide_legend(title="Attrition"))+
  scale_y_continuous(labels = scales::percent)

#Those who travel frequently are more likely to Attrite
```
![data](https://github.com/yatinkode/Predict-Employee-Attrition-using-Logistic-Regression-in-R/blob/main/images/businesstravelattr.png)

#### Attrition Based on Department
```R
t<-prop.table(table(merge_emp_data4$Attrition,merge_emp_data4$Department),2)

ggplot(as.data.frame(t), aes(x = Var2,y=Freq,fill=Var1)) +
  geom_bar(position="stack",stat="identity",col="black") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Department",y = "Percent of Attrition",
       title = "Fig 1: Proportion of Attrition Based on Department" ) +theme_HRcase()+
  guides(fill=guide_legend(title="Attrition"))+
  scale_y_continuous(labels = scales::percent)

#Those who are from HR Department are more likely to Attrite
```
![data](https://github.com/yatinkode/Predict-Employee-Attrition-using-Logistic-Regression-in-R/blob/main/images/deptattr.png)

#### Attrition Based on Distance from Home
```R
t<-prop.table(table(merge_emp_data4$Attrition,merge_emp_data4$DistanceFromHome),2)

ggplot(as.data.frame(t), aes(x = Var2,y=Freq,fill=Var1)) +
  geom_bar(position="stack",stat="identity",col="black") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Education",y = "Percent of Attrition",
       title = "Fig 1: Proportion of Attrition Based on Distance from Home" ) +theme_HRcase()+
  guides(fill=guide_legend(title="Attrition"))+
  scale_y_continuous(labels = scales::percent)

#No specific Finding observed regarding distance from home
```
![data](https://github.com/yatinkode/Predict-Employee-Attrition-using-Logistic-Regression-in-R/blob/main/images/distancehomeattr.png)

#### Attrition Based on Education
```R
t<-prop.table(table(merge_emp_data4$Attrition,merge_emp_data4$Education),2)

ggplot(as.data.frame(t), aes(x = Var2,y=Freq,fill=Var1)) +
  geom_bar(position="stack",stat="identity",col="black") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Education",y = "Percent of Attrition",
       title = "Fig 1: Proportion of Attrition Based on Education" ) +theme_HRcase()+
  guides(fill=guide_legend(title="Attrition"))+
  scale_y_continuous(labels = scales::percent)

#Those who are having College Educaation are more likely to Attrite
```
![data](https://github.com/yatinkode/Predict-Employee-Attrition-using-Logistic-Regression-in-R/blob/main/images/eduattr.png)

#### Attrition Based on Education Field
```R
t<-prop.table(table(merge_emp_data4$Attrition,merge_emp_data4$EducationField),2)

ggplot(as.data.frame(t), aes(x = Var2,y=Freq,fill=Var1)) +
  geom_bar(position="stack",stat="identity",col="black") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Education Field",y = "Percent of Attrition",
       title = "Fig 1: Proportion of Attrition Based on Education Field" ) +theme_HRcase()+
  guides(fill=guide_legend(title="Attrition"))+
  scale_y_continuous(labels = scales::percent)

#Those who are from HR Education Field are most likely to Attrite
```
![data](https://github.com/yatinkode/Predict-Employee-Attrition-using-Logistic-Regression-in-R/blob/main/images/edufieldattr.png)

### Scaling continuous variables
```R
merge_emp_data4$Age<-scale(merge_emp_data4$Age)
merge_emp_data4$MonthlyIncome<-scale(merge_emp_data4$MonthlyIncome)
merge_emp_data4$DistanceFromHome<-scale(merge_emp_data4$DistanceFromHome)
merge_emp_data4$NumCompaniesWorked<-scale(merge_emp_data4$NumCompaniesWorked)
merge_emp_data4$PercentSalaryHike<-scale(merge_emp_data4$PercentSalaryHike)
merge_emp_data4$StockOptionLevel<-scale(merge_emp_data4$StockOptionLevel)
merge_emp_data4$TotalWorkingYears<-scale(merge_emp_data4$TotalWorkingYears)
merge_emp_data4$TrainingTimesLastYear<-scale(merge_emp_data4$TrainingTimesLastYear)
merge_emp_data4$YearsAtCompany<-scale(merge_emp_data4$YearsAtCompany)
merge_emp_data4$YearsSinceLastPromotion<-scale(merge_emp_data4$YearsSinceLastPromotion)
merge_emp_data4$YearsWithCurrManager<-scale(merge_emp_data4$YearsWithCurrManager)
merge_emp_data4$timeperday<-scale(merge_emp_data4$timeperday)
merge_emp_data4$leaves<-scale(merge_emp_data4$leaves)
```

### Finding out correlation matrix
```R
#Making a correlation matrix of all continuous variables including attrition
emp_cormat <- merge_emp_data4[, c(3,2,6,13:21,27:28)]

str(emp_cormat)
emp_cormat <- round(cor(emp_cormat),2)
head(emp_cormat)

#Function to convert lower triangle to NA
get_upper_tri <- function(emp_cormat){
  emp_cormat[lower.tri(emp_cormat)]<- NA
  return(emp_cormat)
}

upper_tri <- get_upper_tri(emp_cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)


ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "#F55F22", high = "#259B55", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1))+ coord_fixed()

ggheatmap + geom_text(aes(Var2, Var1, label = value), color = "black", size = 3) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 8, barheight = 1,title.position = "top", title.hjust = 0.5))+
  ggtitle("Correlation plot of continuous variables in HR  dataset")

#Positive correlation of YearsAtCompany and YearsWithCurrManager is 0.77, It is highly probable that employee remains in a single project with same manager for most of the time but not neccesary enough
```
![data](https://github.com/yatinkode/Predict-Employee-Attrition-using-Logistic-Regression-in-R/blob/main/images/corrplot.png)

### Creating dummy variables
```R
# creating a dataframe of categorical features
merge_emp_data4_cat<- merge_emp_data4[,c(4,5,7,8,9,10,11,12,22:26)]
#View(merge_emp_data4_cat)

# converting categorical attributes to factor
merge_emp_data4_fact<- data.frame(sapply(merge_emp_data4_cat, function(x) factor(x)))
str(merge_emp_data4_fact)


#converting categorical variables to dummy variables
dummies<- data.frame(sapply(merge_emp_data4_fact, function(x) data.frame(model.matrix(~x-1,data =merge_emp_data4_fact))[,-1]))

#In Gender column 1=Male and 0=Female so we assign an appropriate name to it for understanding
colnames(dummies)[which(names(dummies) == "Gender")] <- "Gender.Male"

employee_final<- cbind(merge_emp_data4[,-c(4,5,7,8,9,10,11,12,22:26)],dummies) 
```

### Train and Test Data
```R
# splitting the data between train and test
set.seed(100)

indices = sample.split(employee_final$Attrition, SplitRatio = 0.7)

train = employee_final[indices,]

test = employee_final[!(indices),]
```
### Modelling
```R
#Initial model
model_1 = glm( Attrition ~ .-EmployeeID, data = train, family = "binomial")
summary(model_1)

# Stepwise selection
model_2<- stepAIC(model_1, direction="both")
summary(model_2)
vif(model_2)
#Removing EducationField.xLife.Sciences with VIF 17.44505	Residual Deviance 2003.3	and AIC 2071.3 in next model

model_3<-glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
               StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + 
               YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + 
               timeperday + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
               Department.xResearch...Development + Department.xSales + 
               Education.xDoctor + EducationField.xMarketing + 
               EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
               JobLevel.x5 + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
               MaritalStatus.xMarried + MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
               EnvironmentSatisfaction.xVeryHigh + JobSatisfaction.xLow + 
               JobSatisfaction.xVeryHigh + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
               WorkLifeBalance.xGood + JobInvolvement.xLow + JobInvolvement.xVeryHigh, 
             family = "binomial", data = train)

summary(model_3)
vif(model_3)
#Removing EducationField.xMedical with VIF 1.207399	Residual Deviance 2003.4 and AIC 2069.4 in next model

model_4<-glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
               StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + 
               YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + 
               timeperday + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
               Department.xResearch...Development + Department.xSales + 
               Education.xDoctor + EducationField.xMarketing + 
               EducationField.xOther + EducationField.xTechnical.Degree + 
               JobLevel.x5 + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
               MaritalStatus.xMarried + MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
               EnvironmentSatisfaction.xVeryHigh + JobSatisfaction.xLow + 
               JobSatisfaction.xVeryHigh + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
               WorkLifeBalance.xGood + JobInvolvement.xLow + JobInvolvement.xVeryHigh, 
             family = "binomial", data = train)

summary(model_4)
vif(model_4)
#Removing EducationField.xOther with VIF 1.059212 Residual Deviance	2005.9 and AIC 2069.9 in next model

model_5<-glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
               StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + 
               YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + 
               timeperday + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
               Department.xResearch...Development + Department.xSales + 
               Education.xDoctor + EducationField.xMarketing + 
               EducationField.xTechnical.Degree + JobLevel.x5 + JobRole.xManufacturing.Director + 
               JobRole.xResearch.Director + MaritalStatus.xMarried + MaritalStatus.xSingle + 
               EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVeryHigh + JobSatisfaction.xLow + 
               JobSatisfaction.xVeryHigh + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
               WorkLifeBalance.xGood + JobInvolvement.xLow + JobInvolvement.xVeryHigh, 
             family = "binomial", data = train)

summary(model_5)
vif(model_5)
#removing MaritalStatus.xMarried with VIF 2.284682	Residual Deviance	2008.2 and AIC	2070.2 in next model


model_6<-glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
               StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + 
               YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + 
               timeperday + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
               Department.xResearch...Development + Department.xSales + 
               Education.xDoctor + EducationField.xMarketing + 
               EducationField.xTechnical.Degree + JobLevel.x5 + JobRole.xManufacturing.Director +  
               JobRole.xResearch.Director + MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
               EnvironmentSatisfaction.xVeryHigh + JobSatisfaction.xLow + 
               JobSatisfaction.xVeryHigh + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
               WorkLifeBalance.xGood + JobInvolvement.xLow + JobInvolvement.xVeryHigh, 
             family = "binomial", data = train)

summary(model_6)
vif(model_6)
#EducationField.xMarketing with VIF 1.466868	Residual Deviance	2011.2 and AIC	2071.2 in next model


model_7<-glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
               StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + 
               YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + 
               timeperday + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
               Department.xResearch...Development + Department.xSales + 
               Education.xDoctor + EducationField.xTechnical.Degree + 
               JobLevel.x5 + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
               MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
               EnvironmentSatisfaction.xVeryHigh + JobSatisfaction.xLow + 
               JobSatisfaction.xVeryHigh + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
               WorkLifeBalance.xGood + JobInvolvement.xLow + JobInvolvement.xVeryHigh, 
             family = "binomial", data = train)

summary(model_7)
vif(model_7)
#Remove MonthlyIncome	with VIF 1.050014 Residual Deviance	2014.4 and AIC	2072.4 in next model

model_8<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
               StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + 
               YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + 
               timeperday + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
               Department.xResearch...Development + Department.xSales + 
               Education.xDoctor +
               EducationField.xTechnical.Degree + 
               JobLevel.x5 + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
               MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
               EnvironmentSatisfaction.xVeryHigh + JobSatisfaction.xLow + 
               JobSatisfaction.xVeryHigh + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
               WorkLifeBalance.xGood + JobInvolvement.xLow + JobInvolvement.xVeryHigh, 
             family = "binomial", data = train)

summary(model_8)
vif(model_8)
#remove YearsAtCompany 	.	4.821408	2018.1	2074.1

model_9<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
               StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + 
               timeperday + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
               Department.xResearch...Development + Department.xSales + 
               Education.xDoctor +
               EducationField.xTechnical.Degree + 
               JobLevel.x5 + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
               MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
               EnvironmentSatisfaction.xVeryHigh + JobSatisfaction.xLow + 
               JobSatisfaction.xVeryHigh + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
               WorkLifeBalance.xGood + JobInvolvement.xLow + JobInvolvement.xVeryHigh, 
             family = "binomial", data = train)

summary(model_9)
vif(model_9)
#Remove JobInvolvement.xLow	.	1.038474	2021.6	2075.6


model_10<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                timeperday + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                Education.xDoctor +
                EducationField.xTechnical.Degree + 
                JobLevel.x5 + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                EnvironmentSatisfaction.xVeryHigh + JobSatisfaction.xLow + 
                JobSatisfaction.xVeryHigh + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                WorkLifeBalance.xGood + JobInvolvement.xVeryHigh, 
              family = "binomial", data = train)

summary(model_10)
vif(model_10)
#JobInvolvement.xVeryHigh	.	1.03098	2025	2077


model_11<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                timeperday + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                Education.xDoctor +
                EducationField.xTechnical.Degree + 
                JobLevel.x5 + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                EnvironmentSatisfaction.xVeryHigh + JobSatisfaction.xLow + 
                JobSatisfaction.xVeryHigh + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                WorkLifeBalance.xGood, 
              family = "binomial", data = train)

summary(model_11)
vif(model_11)
#Remove EducationField.xTechnical.Degree	.	1.02516	2029	2079

model_12<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                timeperday + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                Education.xDoctor +
                JobLevel.x5 + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                EnvironmentSatisfaction.xVeryHigh + JobSatisfaction.xLow + 
                JobSatisfaction.xVeryHigh + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                WorkLifeBalance.xGood, 
              family = "binomial", data = train)

summary(model_12)
vif(model_12)
#Remove StockOptionLevel	*	1.044923	2033.2	2081.2

model_13<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                timeperday + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                Education.xDoctor +
                JobLevel.x5 + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                EnvironmentSatisfaction.xVeryHigh + JobSatisfaction.xLow + 
                JobSatisfaction.xVeryHigh + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                WorkLifeBalance.xGood, 
              family = "binomial", data = train)

summary(model_13)
vif(model_13)
#Remove JobLevel.x5	*	1.034776	2037.7	2083.7

model_14<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                timeperday + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                Education.xDoctor +
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                EnvironmentSatisfaction.xVeryHigh + JobSatisfaction.xLow + 
                JobSatisfaction.xVeryHigh + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                WorkLifeBalance.xGood, 
              family = "binomial", data = train)

summary(model_14)
vif(model_14)
#Remove Education.xDoctor	1.030859	2042.8	2086.8


model_15<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                timeperday + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                EnvironmentSatisfaction.xVeryHigh + JobSatisfaction.xLow + 
                JobSatisfaction.xVeryHigh + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                WorkLifeBalance.xGood, 
              family = "binomial", data = train)

summary(model_15)
vif(model_15)
#EnvironmentSatisfaction.xVeryHigh	1.167215	2049.5	2091.5

model_16<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                timeperday + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                JobSatisfaction.xLow + 
                JobSatisfaction.xVeryHigh + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                WorkLifeBalance.xGood, 
              family = "binomial", data = train)

summary(model_16)
vif(model_16)
#BusinessTravel.xTravel_Rarely	3.76784	2058.3	2098.3

model_17<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                timeperday + BusinessTravel.xTravel_Frequently +
                Department.xResearch...Development + Department.xSales + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                JobSatisfaction.xLow + 
                JobSatisfaction.xVeryHigh + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                WorkLifeBalance.xGood, 
              family = "binomial", data = train)

summary(model_17)
vif(model_17)
#JobRole.xResearch.Director	1.033557		2065.1	2103.1

model_18<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                timeperday + BusinessTravel.xTravel_Frequently +
                Department.xResearch...Development + Department.xSales + 
                JobRole.xManufacturing.Director + 
                MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                JobSatisfaction.xLow + 
                JobSatisfaction.xVeryHigh + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                WorkLifeBalance.xGood, 
              family = "binomial", data = train)

summary(model_18)
vif(model_18)
#Remove WorkLifeBalance.xBest	2.167122	2074.6	2110.6

model_19<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                timeperday + BusinessTravel.xTravel_Frequently +
                Department.xResearch...Development + Department.xSales + 
                JobRole.xManufacturing.Director + 
                MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                JobSatisfaction.xLow + 
                JobSatisfaction.xVeryHigh + WorkLifeBalance.xBetter + 
                WorkLifeBalance.xGood, 
              family = "binomial", data = train)

summary(model_19)
vif(model_19)
#TrainingTimesLastYear	1.018423	2084.1	2118.1

model_20<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                timeperday + BusinessTravel.xTravel_Frequently +
                Department.xResearch...Development + Department.xSales + 
                JobRole.xManufacturing.Director + 
                MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                JobSatisfaction.xLow + 
                JobSatisfaction.xVeryHigh + WorkLifeBalance.xBetter + 
                WorkLifeBalance.xGood, 
              family = "binomial", data = train)

summary(model_20)
vif(model_20)
#WorkLifeBalance.xGood	1.767529	2092.1	2124.1


model_21<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                timeperday + BusinessTravel.xTravel_Frequently +
                Department.xResearch...Development + Department.xSales + 
                JobRole.xManufacturing.Director + 
                MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                JobSatisfaction.xLow + 
                JobSatisfaction.xVeryHigh + WorkLifeBalance.xBetter, 
              family = "binomial", data = train)

summary(model_21)
vif(model_21)
#Remove WorkLifeBalance.xBetter	1.770849	2102.2	2132.2

model_22<-glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                timeperday + BusinessTravel.xTravel_Frequently +
                Department.xResearch...Development + Department.xSales + 
                JobRole.xManufacturing.Director + 
                MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                JobSatisfaction.xLow + 
                JobSatisfaction.xVeryHigh, 
              family = "binomial", data = train)

summary(model_22)
vif(model_22)

#Remove Age which is only left as **

model_23<-glm(formula = Attrition ~ NumCompaniesWorked + 
                TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                timeperday + BusinessTravel.xTravel_Frequently +
                Department.xResearch...Development + Department.xSales + 
                JobRole.xManufacturing.Director + 
                MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                JobSatisfaction.xLow + 
                JobSatisfaction.xVeryHigh, 
              family = "binomial", data = train)

summary(model_23)
vif(model_23)


final_model<- model_23

final_model

#Deviance Residuals: 
#    Min       1Q   Median       3Q      Max  
#-2.1137  -0.5679  -0.3438  -0.1749   3.8555  
#
#Coefficients:
#                                   Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                        -1.99289    0.25381  -7.852 4.09e-15 ***
#NumCompaniesWorked                  0.26859    0.05738   4.681 2.86e-06 ***
#TotalWorkingYears                  -0.75956    0.09191  -8.264  < 2e-16 ***
#YearsSinceLastPromotion             0.56548    0.07752   7.295 2.99e-13 ***
#YearsWithCurrManager               -0.52327    0.08715  -6.005 1.92e-09 ***
#timeperday                          0.64622    0.05409  11.948  < 2e-16 ***
#BusinessTravel.xTravel_Frequently   0.90467    0.13008   6.955 3.53e-12 ***
#Department.xResearch...Development -0.86262    0.24242  -3.558 0.000373 ***
#Department.xSales                  -0.92779    0.25344  -3.661 0.000251 ***
#JobRole.xManufacturing.Director    -0.73590    0.22072  -3.334 0.000856 ***
#MaritalStatus.xSingle               1.04629    0.11361   9.210  < 2e-16 ***
#EnvironmentSatisfaction.xLow        1.10762    0.12909   8.580  < 2e-16 ***
#JobSatisfaction.xLow                0.64663    0.13613   4.750 2.03e-06 ***
#JobSatisfaction.xVeryHigh          -0.56217    0.13851  -4.059 4.94e-05 ***
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#    Null deviance: 2704.5  on 3066  degrees of freedom
#Residual deviance: 2112.6  on 3053  degrees of freedom
#AIC: 2140.6
#
#Number of Fisher Scoring iterations: 6
```

### Model Evaluation
```R
#predicted probabilities of Attribution  for test data

test_pred = predict(final_model, type = "response", newdata = test[,-3])  #Not considering Attrition while predicting values

summary(test_pred)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.0003405 0.0377544 0.0993289 0.1574060 0.2119120 0.9467265 

#Predicted Probablities are from 0.03% to 94.6%

#binding probablities onto test data
test$prob <- test_pred
View(test)


# Let's use the probability cutoff of 50%.
test_pred_attr <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attr <- factor(ifelse(test$Attrition==1,"Yes","No"))

#Confusion Matrix for reference
table(test_actual_attr,test_pred_attr)

test_conf <- confusionMatrix(test_pred_attr, test_actual_attr, positive = "Yes")

test_conf

#          Reference
#Prediction   No  Yes
#       No  1075  165
#       Yes   28   47
#                                          
#               Accuracy : 0.8532          
#                 95% CI : (0.8329, 0.8719)
#    No Information Rate : 0.8388          
#    P-Value [Acc > NIR] : 0.08141         
#                                          
#                  Kappa : 0.2657          
# Mcnemar's Test P-Value : < 2e-16         
#                                          
#            Sensitivity : 0.22170         
#            Specificity : 0.97461         
#         Pos Pred Value : 0.62667         
#         Neg Pred Value : 0.86694         
#             Prevalence : 0.16122         
#         Detection Rate : 0.03574         
#   Detection Prevalence : 0.05703         
#      Balanced Accuracy : 0.59816         
#                                          
#       'Positive' Class : Yes  
```
#### Choose the cutoff value
```R
# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_attr <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attr, test_actual_attr, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Summary of test probability

summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,0.40,col=c(2,"darkgreen",4,"darkred"),lwd=c(1,1,1,1),c("Sensitivity","Specificity","Accuracy"))
```
![data](https://github.com/yatinkode/Predict-Employee-Attrition-using-Logistic-Regression-in-R/blob/main/images/senspeplot.png)

```R
cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]


# Let's choose a cutoff value of 0.16 for final model

test_pred <- factor(ifelse(test$prob >= cutoff, "Yes", "No"))
test_actual <- factor(ifelse(test$Attrition == 1,"Yes","No"))
table(test_pred,test_actual)

test_conf <- confusionMatrix(test_pred, test_actual, positive = "Yes")
(acc <- test_conf$overall[1])
(sens <- test_conf$byClass[1])
(spec <- test_conf$byClass[2])

acc
#Accuracy 
#0.7247148

sens
#Sensitivity 
#0.7264151 

spec
#Specificity 
#0.7289211 

View(test)
```
#### KS -statistic - Test Data
```R
# KS statistic is an indicator of how well the model discriminates between 
# the two classes.
# It is equal to 0% for the random model, and 100% for the perfect model

test_pred_attrition <- ifelse(test_pred == "Yes",1,0)
test_actual_attrition <- ifelse(test_actual == "Yes",1,0)

pred_object_test <- prediction(test_pred_attrition, test_actual_attrition)
performance_measures_test <- performance(pred_object_test, "tpr", "fpr")

# Lets plot the Area under curve
auc <- performance(pred_object_test,"auc")
unlist(auc@y.values)
# AUC 0.7276681

plot(performance_measures_test,col = "red")
abline(0,1, lty = 8, col = "grey")
```
![data](https://github.com/yatinkode/Predict-Employee-Attrition-using-Logistic-Regression-in-R/blob/main/images/ks.png)

```R
ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])
(max(ks_table_test))

## Summary 
# KS Static - 45.5%  (KS Static > 40 % indicates a good model)
```

#### Lift & Gain Chart 
```R
# Gain chart is a popular method to visually inspect model performance 
# in binary prediction. It presents the percentage of captured 
# positive responses as a function of selected percentage of a sample

# Lift basically just tells you the factor by which your model is 
# outperforming a random model, i.e. a model-less situation

lift <- function(labels , predicted_prob,groups=10) {
  if (is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if (is.factor(predicted_prob)) predicted_prob <- 
      as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp = sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain = Cumresp/sum(totalresp)*100,
           Cumlift = Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Attr_decile = lift(test_actual_attrition, test_pred_attrition, groups = 10)

ggplot(Attr_decile,aes(x = bucket, y = Gain)) + 
  geom_line() + 
  geom_point(col = 'red2', size = 5) +
  geom_text(aes(label = round(Gain,2)),  
            nudge_x = -0.40, nudge_y = -0.40)+ labs(x = "bucket",y = "Gain",title = "Fig 1: Gain Chart" )+ theme_HRcase()
#We reach greater than 75% gain at 4th decile, which is sign of good model
```
![data](https://github.com/yatinkode/Predict-Employee-Attrition-using-Logistic-Regression-in-R/blob/main/images/gain.png)

```R
ggplot(Attr_decile,aes(x = bucket, y = Cumlift)) + 
  geom_line() + 
  geom_point(col = 'red2', size = 5) +
  geom_text(aes(label = round(Cumlift,2)), 
            nudge_x = 0.10, nudge_y = 0.10)+labs(x = "bucket",y = "Lift",title = "Fig 1: Lift Chart" )+ theme_HRcase()

#By observing lift chart we see that highest lift of 2.11 found at 3rd decile.
#It means our model is 2.11 times better than a random model
```
![data](https://github.com/yatinkode/Predict-Employee-Attrition-using-Logistic-Regression-in-R/blob/main/images/lift.png)

```R
#Summarizing Model Evaluation
#Accuracy       0.7247148    
#Sensitivity    0.7264151
#Specificity    0.7289211
#KS Statistic   45.5%
#AOC            Area Under Curve (AOC)
#Gain           75.4% gain at 4th Decile
#Lift           2.11 at 3rd Decile
```

