##################################Data Preparation#############################
#We have 5 datasets, combining which we need to create 1 dataset on which we can 
#perform analysis
#Looking at all the data, first we will work with intime and outtime files
#The derived column from this say "actual working hours" which will be be difference 
#between in time and out time will help us to see if that variable is affecting our
#dependent variable or not

#lets import in time and out time files
in_time <- read.csv("in_time.csv", stringsAsFactors = F)
out_time <- read.csv("out_time.csv", stringsAsFactors = F)

#seems that 1st column is employee ids so updating column name
colnames(in_time)[1] <- "EmployeeID"
colnames(out_time)[1] <- "EmployeeID"

#data is wide lets convert it into long data rather than wide
#install.packages("tidyr")
library(tidyr)
in_time_long <- gather(in_time,Date,Time,colnames(in_time)[2:262])
out_time_long <- gather(out_time,Date,Time,colnames(in_time)[2:262])

#removing first character from Date Column and converting it into date format
in_time_long$Date <- substring(in_time_long$Date,2)
out_time_long$Date <- substring(out_time_long$Date,2)

in_time_long$Date <- as.Date(in_time_long$Date, format = "%Y.%m.%d")
out_time_long$Date <- as.Date(out_time_long$Date, format = "%Y.%m.%d")

#now removing NA from this table as it shows that either its holiday or employee was on leave
#so it doesnt make sense including such rows

in_time_long <- in_time_long[complete.cases(in_time_long),]
out_time_long <- out_time_long[complete.cases(out_time_long),]

str(in_time_long)
str(out_time_long)

#We thought of including average working hours for each employee in main datase
#so rounding off the in and out time to the nearest hours so that mean can be calculated easily
in_time_long$Coming_Time <- format(round(strptime(in_time_long$Time,"%Y-%m-%d %H:%M:%S",tz=""), units="hours"), format="%H:%M:%S")
out_time_long$Leaving_Time <- format(round(strptime(out_time_long$Time,"%Y-%m-%d %H:%M:%S",tz=""), units="hours"), format="%H:%M:%S")

#extracting hours seperately
in_time_long$In_Hours <- format(as.POSIXct(strptime(in_time_long$Coming_Time,"%H:%M:%S",tz="")) ,format = "%H")
out_time_long$Out_Hours <- format(as.POSIXct(strptime(out_time_long$Leaving_Time,"%H:%M:%S",tz="")) ,format = "%H")

#merging in and out timing dataset
employee_working_hrs <- merge(in_time_long,out_time_long, by = c("EmployeeID","Date"))

#giving necessaryy column names
colnames(employee_working_hrs)[3] <- "In_Date_and_Time"
colnames(employee_working_hrs)[6] <- "Out_Date_and_Time"

#calculating actual working hours so converting hours column into numeric
employee_working_hrs$In_Hours <- as.numeric(employee_working_hrs$In_Hours)
employee_working_hrs$Out_Hours <- as.numeric(employee_working_hrs$Out_Hours)

employee_working_hrs$actual_working_hours <- employee_working_hrs$Out_Hours - employee_working_hrs$In_Hours

#Now lets create a table cosnisting distinct employee id with there avg working hours
#install.packages("dplyr")
library(dplyr)

emp_with_wrkhrs <- employee_working_hrs %>%
  select(EmployeeID,actual_working_hours) %>%
  group_by(EmployeeID) %>%
  summarise(round(mean(actual_working_hours)))

colnames(emp_with_wrkhrs)[2] <- "Actual_working_hrs"


#Now lets merge all the dataset we have withe the main dataset
#merging general_data, employee_survey_data, Manager_survey_data & emp_with_wrkhrs

General_data <- read.csv("general_data.csv", stringsAsFactors = F)
employee_survey_data <- read.csv("employee_survey_data.csv", stringsAsFactors = F)
manager_survey_data <- read.csv("manager_survey_data.csv", stringsAsFactors = F)


survey_data <- merge(employee_survey_data,manager_survey_data, by= "EmployeeID")
General_data <- merge(General_data,survey_data, by = "EmployeeID")
General_data <- merge(General_data,emp_with_wrkhrs, by = "EmployeeID")


#Now lete treat NA Values in this data
sapply(General_data, function(x) sum(is.na(x)))

#so we can see that there are NA values in following columns
#NumCompaniesWorked = 19, EnvironmentSatisfaction = 25, JobSatisfaction=20
#WorkLifeBalance = 38, TotalWorkingYears = 9

View(subset(General_data, is.na(NumCompaniesWorked)))
View(subset(General_data, is.na(TotalWorkingYears)))

#for NumCompaniesWorked & TotalWorkingYears total 28 out of 4410 which is 0.006349206
#so its 0.6%, capping this value to its mean median or mode better to delete this records

General_data <- General_data[!is.na(General_data$NumCompaniesWorked),]
General_data <- General_data[!is.na(General_data$TotalWorkingYears),]

#install.packages("ggplot2")
library(ggplot2)

str(General_data)

#EnvironmentSatisfaction, JobSatisfaction, WorkLifeBalance are factor variable
#lets convert those into factors 

General_data$EnvironmentSatisfaction <- as.factor(General_data$EnvironmentSatisfaction)
General_data$JobSatisfaction <- as.factor(General_data$JobSatisfaction)
General_data$WorkLifeBalance <- as.factor(General_data$WorkLifeBalance)

ggplot(General_data,aes(General_data$EnvironmentSatisfaction))+geom_bar() #Most of the have given 3 ratings
ggplot(General_data,aes(General_data$JobSatisfaction))+geom_bar() #Most of the have given 4 ratings
ggplot(General_data,aes(General_data$WorkLifeBalance))+geom_bar() #Most of the have given 3 ratings

#so You can see from the above graph that NA values are too less
#so better to cap those records with Mode value
install.packages("Hmisc")
library(Hmisc)


General_data$EnvironmentSatisfaction <- impute(General_data$EnvironmentSatisfaction, 3)
General_data$JobSatisfaction <- impute(General_data$JobSatisfaction, 4)
General_data$WorkLifeBalance <- impute(General_data$WorkLifeBalance, 3)

#we have completed with NA values
#Now lets convert all column to its respective data type
#Also at the same time we will do univariate analysis 
str(General_data)
General_data$Attrition <- as.factor(General_data$Attrition)

#BusinessTravel
General_data$BusinessTravel <- as.factor(General_data$BusinessTravel)

#Department
General_data$Department <- as.factor(General_data$Department)

#DistanceFromHome
General_data$DistanceFromHome2 <- ifelse(General_data$DistanceFromHome<=10,General_data$DistanceFromHome2 <- "1-10",
                                         ifelse(General_data$DistanceFromHome>10&General_data$DistanceFromHome<=20,General_data$DistanceFromHome2 <- "11-20",General_data$DistanceFromHome2 <- "More than 20"))

General_data$DistanceFromHome <- General_data$DistanceFromHome2
General_data <- General_data[,c(1:30)]

General_data$DistanceFromHome <- as.factor(General_data$DistanceFromHome)

str(General_data)

#Education
General_data$Education <- as.factor(General_data$Education)
#EducationField
General_data$EducationField <- as.factor(General_data$EducationField)
#Gender
General_data$Gender <- as.factor(General_data$Gender)
#JobLevel
General_data$JobLevel <- as.factor(General_data$JobLevel)
#JobRole
General_data$JobRole <- as.factor(General_data$JobRole)
#MaritalStatus
General_data$MaritalStatus <- as.factor(General_data$MaritalStatus)

#NumCompaniesWorked
#Lets convert this in 4 range. 
#0 = Fresher, 1-3, 4-7, Above 7 and then would convert this into factor

General_data$NumCompaniesWorked2 <- ifelse(General_data$NumCompaniesWorked==0,General_data$NumCompaniesWorked2 <- "Fresher",
                                           ifelse(General_data$NumCompaniesWorked>=1&General_data$NumCompaniesWorked<=3,General_data$NumCompaniesWorked2 <- "1-3",
                                                  ifelse(General_data$NumCompaniesWorked>3&General_data$NumCompaniesWorked<=7,General_data$NumCompaniesWorked2 <- "4-7",
                                                         General_data$NumCompaniesWorked2 <- "More than 7")))


General_data$NumCompaniesWorked <- General_data$NumCompaniesWorked2
General_data <- General_data[,c(1:30)]

General_data$NumCompaniesWorked <- as.factor(General_data$NumCompaniesWorked)

#PercentSalaryHike
str(General_data)
quantile(General_data$PercentSalaryHike,seq(0,1,0.01))
#Converting this variable into ranges as well as follows
#11-14,15-19,20-25

General_data$PercentSalaryHike2 <-ifelse(General_data$PercentSalaryHike>=11&General_data$PercentSalaryHike<=14,General_data$PercentSalaryHike2 <- "11-14",
                                         ifelse(General_data$PercentSalaryHike>14&General_data$PercentSalaryHike<=19,General_data$PercentSalaryHike2 <- "14-19",General_data$PercentSalaryHike2 <- "20-25"))

General_data$PercentSalaryHike <- General_data$PercentSalaryHike2
General_data <- General_data[,c(1:30)]

General_data$PercentSalaryHike <- as.factor(General_data$PercentSalaryHike)

#StockOptionLevel
General_data$StockOptionLevel <- as.factor(General_data$StockOptionLevel)

#TotalWorkingYears
quantile(General_data$TotalWorkingYears,seq(0,1,0.01))
#so there are wide variety of experience people from nil experience till 40
#lets group the in 4 categories as follows
#0-10,11-20,21-30,31-40

General_data$TotalWorkingYears2 <- ifelse(General_data$TotalWorkingYears>=0&General_data$TotalWorkingYears<=10,General_data$TotalWorkingYears2 <- "0-10",
                                          ifelse(General_data$TotalWorkingYears>10&General_data$TotalWorkingYears<=20,General_data$TotalWorkingYears2 <- "11-20",
                                                 ifelse(General_data$TotalWorkingYears>20&General_data$TotalWorkingYears<=30,General_data$TotalWorkingYears2 <- "21-30",
                                                        General_data$TotalWorkingYears2 <- "31-40")))

General_data$TotalWorkingYears <- General_data$TotalWorkingYears2
General_data <- General_data[,c(1:30)]

General_data$TotalWorkingYears <- as.factor(General_data$TotalWorkingYears)

#TrainingTimesLastYear
General_data$TrainingTimesLastYear <- as.factor(General_data$TrainingTimesLastYear)

#YearsAtCompany
quantile(General_data$YearsAtCompany,seq(0,1,0.01))
#Here also you can create 4 ranges
#0-10,11-20,21-30,31-40

General_data$YearsAtCompany2 <- ifelse(General_data$YearsAtCompany>=0&General_data$YearsAtCompany<=10,General_data$YearsAtCompany2 <- "0-10",
                                       ifelse(General_data$YearsAtCompany>10&General_data$YearsAtCompany<=20,General_data$YearsAtCompany2 <- "11-20",
                                              ifelse(General_data$YearsAtCompany>20&General_data$YearsAtCompany<=30,General_data$YearsAtCompany2 <- "21-30",
                                                     General_data$YearsAtCompany2 <- "31-40")))

General_data$YearsAtCompany <- General_data$YearsAtCompany2
General_data <- General_data[,c(1:30)]

General_data$YearsAtCompany <- as.factor(General_data$YearsAtCompany)

#YearsSinceLastPromotion
str(General_data)
quantile(General_data$YearsSinceLastPromotion,seq(0,1,0.01))

#Let convert this variable into factor by creating ranges
General_data$YearsSinceLastPromotion2 <- ifelse(General_data$YearsSinceLastPromotion>=0&General_data$YearsSinceLastPromotion<=5,General_data$YearsSinceLastPromotion2 <- "0-5 Years",
                                                ifelse(General_data$YearsSinceLastPromotion>5&General_data$YearsSinceLastPromotion<=10,General_data$YearsSinceLastPromotion2 <- "6-10 Years",General_data$YearsSinceLastPromotion2 <- "11-15 Years"))

General_data$YearsSinceLastPromotion <- General_data$YearsSinceLastPromotion2
General_data <- General_data[,c(1:30)]

General_data$YearsSinceLastPromotion <- as.factor(General_data$YearsSinceLastPromotion)                                                     

#YearsWithCurrManager
quantile(General_data$YearsWithCurrManager,seq(0,1,0.01))

General_data$YearsWithCurrManager2 <- ifelse(General_data$YearsWithCurrManager>=0&General_data$YearsWithCurrManager<=5,General_data$YearsWithCurrManager2 <- "0-5 Years",
                                             ifelse(General_data$YearsWithCurrManager>5&General_data$YearsWithCurrManager<=10,General_data$YearsWithCurrManager2 <- "6-10 Years",General_data$YearsWithCurrManager2 <- "11-17 Years"))

General_data$YearsWithCurrManager <- General_data$YearsWithCurrManager2
General_data <- General_data[,c(1:30)]

General_data$YearsWithCurrManager <- as.factor(General_data$YearsWithCurrManager)                                                     

#JobInvolvement
General_data$JobInvolvement <- as.factor(General_data$JobInvolvement)

#PerformanceRating
General_data$PerformanceRating <- as.factor(General_data$PerformanceRating)

#Actual_working_hrs
quantile(General_data$Actual_working_hrs,seq(0,1,0.01))
General_data$Actual_working_hrs <- as.factor(General_data$Actual_working_hrs)

#Age
quantile(General_data$Age,seq(0,1,0.01))
#lets convert them into following range
#18-30, 31-40, 41-50, 51-60
General_data$Age2 <- ifelse(General_data$Age>=18&General_data$Age<=30,General_data$Age2 <- "18-30",
                            ifelse(General_data$Age>30&General_data$Age<=40,General_data$Age2 <- "30-40",
                                   ifelse(General_data$Age>40&General_data$Age<=50,General_data$Age2 <- "41-50",
                                          General_data$Age2 <- "Above 50")))

General_data$Age <- General_data$Age2
General_data <- General_data[,c(1:30)]

General_data$Age <- as.factor(General_data$Age)

#Beside all this variables "Over18", 'EmployeeCount", "StandardHours" Have only single value in it, so you can directly delete it.
#Will Delete EmployeeID as well and will make new dataset
str(General_data)
General_data_Final <- General_data[,-c(1,9,16,18)]

#Now lets check outliers in MonthlyIncome
quantile(General_data_Final$MonthlyIncome,seq(0,1,0.01))
#There are no outliers

#Now lets normalize Monthly Income
General_data_Final$MonthlyIncome<- scale(General_data_Final$MonthlyIncome)

##################################EDA##################################
library(cowplot)
bar_theme<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
plot_grid(ggplot(General_data,aes(General_data$BusinessTravel,fill=Attrition))+
            geom_bar(aes(y = ((..count..)/sum(..count..))*100), position = "Fill")+
            labs(title = "BusinessTravel Univariate Analysis", y = "Percent", x = "BusinessTravel")+bar_theme, 
          ggplot(General_data,aes(General_data$Department,fill=Attrition))+
            geom_bar(aes(y = ((..count..)/sum(..count..))*100), position = "Fill")+
            labs(title = "Department Univariate Analysis", y = "Percent", x = "Department")+bar_theme,
          ggplot(General_data,aes(General_data$DistanceFromHome,fill=Attrition))+
            geom_bar(aes(y = ((..count..)/sum(..count..))*100), position = "Fill")+
            labs(title = "Travelling Distance", y = "Percent", x = "DistanceFromHome")+bar_theme,
          ggplot(General_data,aes(General_data$Education,fill=Attrition))+
            geom_bar(aes(y = ((..count..)/sum(..count..))*100), position = "Fill")+
            labs(title = "Education", y = "Percent", x = "Education")+bar_theme,
          align = "h")
plot_grid(ggplot(General_data,aes(General_data$EducationField,fill=Attrition))+
            geom_bar(aes(y = ((..count..)/sum(..count..))*100), position = "Fill")+
            labs(title = "Education Field", y = "Percent", x = "EducationField")+bar_theme, 
          ggplot(General_data,aes(General_data$Gender,fill=Attrition))+
            geom_bar(aes(y = ((..count..)/sum(..count..))*100), position = "Fill")+
            labs(title = "Gender", y = "Percent", x = "Gender")+bar_theme,
          ggplot(General_data,aes(General_data$JobLevel,fill=Attrition))+
            geom_bar(aes(y = ((..count..)/sum(..count..))*100), position = "Fill")+
            labs(title = "Job Level", y = "Percent", x = "JobLevel")+bar_theme,
          ggplot(General_data,aes(General_data$JobRole,fill=Attrition))+
            geom_bar(aes(y = ((..count..)/sum(..count..))*100), position = "Fill")+
            labs(title = "Job Role", y = "Percent", x = "JobRole")+bar_theme,
          ggplot(General_data,aes(General_data$MaritalStatus,fill=Attrition))+
            geom_bar(aes(y = ((..count..)/sum(..count..))*100), position = "Fill")+
            labs(title = "Marital Status", y = "Percent", x = "MaritalStatus")+bar_theme,
          align = "h")
plot_grid(ggplot(General_data,aes(General_data$NumCompaniesWorked,fill=Attrition))+
            geom_bar(aes(y = ((..count..)/sum(..count..))*100), position = "Fill")+
            labs(title = "Num of Companies Worked", y = "Percent", x = "NumCompaniesWorked")+bar_theme, 
          ggplot(General_data,aes(General_data$PercentSalaryHike,fill=Attrition))+
            geom_bar(aes(y = ((..count..)/sum(..count..))*100), position = "Fill")+
            labs(title = "Percent Salary Hike", y = "Percent", x = "PercentSalaryHike")+bar_theme,
          ggplot(General_data,aes(General_data$StockOptionLevel,fill=Attrition))+
            geom_bar(aes(y = ((..count..)/sum(..count..))*100), position = "Fill")+
            labs(title = "Stock Option Level", y = "Percent", x = "StockOptionLevel")+bar_theme,
          ggplot(General_data,aes(General_data$TotalWorkingYears,fill=Attrition))+
            geom_bar(aes(y = ((..count..)/sum(..count..))*100), position = "Fill")+
            labs(title = "Total Working Years", y = "Percent", x = "TotalWorkingYears")+bar_theme,
          ggplot(General_data,aes(General_data$TrainingTimesLastYear,fill=Attrition))+
            geom_bar(aes(y = ((..count..)/sum(..count..))*100), position = "Fill")+
            labs(title = "Training Times Last Year", y = "Percent", x = "TrainingTimesLastYear")+bar_theme,
          ggplot(General_data,aes(General_data$YearsAtCompany,fill=Attrition))+
            geom_bar(aes(y = ((..count..)/sum(..count..))*100), position = "Fill")+
            labs(title = "Years At Company", y = "Percent", x = "YearsAtCompany")+bar_theme,
          align = "h")
plot_grid(ggplot(General_data,aes(General_data$YearsSinceLastPromotion,fill=Attrition))+
            geom_bar(aes(y = ((..count..)/sum(..count..))*100), position = "Fill")+
            labs(title = "Year sSince Last Promotion", y = "Percent", x = "YearsSinceLastPromotion")+bar_theme, 
          ggplot(General_data,aes(General_data$YearsWithCurrManager,fill=Attrition))+
            geom_bar(aes(y = ((..count..)/sum(..count..))*100), position = "Fill")+
            labs(title = "Year sWith Curr Manager", y = "Percent", x = "YearsWithCurrManager")+bar_theme,
          ggplot(General_data,aes(General_data$JobInvolvement,fill=Attrition))+
            geom_bar(aes(y = ((..count..)/sum(..count..))*100), position = "Fill")+
            labs(title = "Job Involvement", y = "Percent", x = "JobInvolvement")+bar_theme,
          ggplot(General_data,aes(General_data$PerformanceRating,fill=Attrition))+
            geom_bar(aes(y = ((..count..)/sum(..count..))*100), position = "Fill")+
            labs(title = "Performance Rating", y = "Percent", x = "PerformanceRating")+bar_theme,
          ggplot(General_data,aes(General_data$Actual_working_hrs,fill=Attrition))+
            geom_bar(aes(y = ((..count..)/sum(..count..))*100), position = "Fill")+
            labs(title = "Actual working hrs", y = "Percent", x = "Actual_working_hrs")+bar_theme,
          ggplot(General_data,aes(General_data$Age,fill=Attrition))+
            geom_bar(aes(y = ((..count..)/sum(..count..))*100), position = "Fill")+
            labs(title = "Age", y = "Percent", x = "Age")+bar_theme,
          align = "h")
plot_grid(ggplot(General_data,aes(General_data$EnvironmentSatisfaction,fill=Attrition))+
            geom_bar(aes(y = ((..count..)/sum(..count..))*100), position = "Fill")+
            labs(title = "Environment Satisfaction", y = "Percent", x = "EnvironmentSatisfaction")+bar_theme, 
          ggplot(General_data,aes(General_data$JobSatisfaction,fill=Attrition))+
            geom_bar(aes(y = ((..count..)/sum(..count..))*100), position = "Fill")+
            labs(title = "Job Satisfaction", y = "Percent", x = "JobSatisfaction")+bar_theme,
          ggplot(General_data,aes(General_data$WorkLifeBalance,fill=Attrition))+
            geom_bar(aes(y = ((..count..)/sum(..count..))*100), position = "Fill")+
            labs(title = "Work Life Balance", y = "Percent", x = "WorkLifeBalance")+bar_theme,
          align = "h")



#it seems that people who travle frequently, churn the most.
#HR Have high percentage of employee leaving which is an intresting insight.
#People who stayed near like around 1 to 20 km of radius have churned the most, which comes as surprise.
#Company doesnt seem to be favourite for people who have completed their collage.
#It seems that who have done MBA in HR are unstable in the company
#There is no such difference of the attrition genderwise
#In job level as well, people till job level 4 churns with very negligible difference between churning rate at each level
#It surprising to see that people at Research director level churns a lot compare to others.
#People who are single seems to be at top who churns.
#People who is having 4 to 7 and or more than 7 years of experience churn the most
#People who have got highest hike left the firm
#people with less than 10 years of experience have highest rate of churning
#stock option level doesnt affecting the churn that much, there is very less variance in each level
#people who have been given 2 or 3 times training or no training have churned
#Also its intresting to see that who wer working for more than 30 years have left the firm, may be because they might have retired
#Derived column Actual working hours gives us important insight that people with long working hours churns the most
#People who are in their 20's or late 20's leave the company most
#People with either low or very high involvment in job churns the most
#Surprisingly highest rating doesnt convince employee to stay at the company
#Variance between year since last promotion is very less.
#People who are working with manager for less than  years leaves the most

#So from EDA we have seen that Department, Business Travel, Education Field, Job Role,
#Marital Status, Total working years, Years at company, Age, Actual working hours, Years with current mamnager are the factors which are affecting most

##############################Further Standarization############################

#Now Lets create a dummy variable for factor variables
str(General_data_Final)

#converting depended variable in 0 & 1
#0 = "No" & 1 = "Yes"
summary(General_data_Final$Attrition)
levels(General_data_Final$Attrition)<-c(0,1)
General_data_Final$Attrition<- as.numeric(levels(General_data_Final$Attrition))[General_data_Final$Attrition]
str(General_data_Final)

#Excluding dependent variable and Monthly Income
General_data_Final_Char <- General_data_Final[,-c(2,12)]

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(General_data_Final_Char, 
                            function(x) data.frame(model.matrix(~x-1,data =General_data_Final_Char))[,-1]))
summary(General_data_Final_Char$Gender)
summary(General_data_Final_Char$PerformanceRating)

#For Gender Female, Male are 0 & 1 Respectively 
#For PerformanceRating 3 and 4 ratings are 0 & 1 

#Final Dataset
General_Data_For_Model <- cbind(General_data_Final[,c(2,12)],dummies)

#########################Logistic Regression Model###########################
# splitting the data between train and test
library(MASS)
library(caTools)

set.seed(100)

indices = sample.split(General_Data_For_Model$Attrition, SplitRatio = 0.7)

train = General_Data_For_Model[indices,]

test = General_Data_For_Model[!(indices),]

#Initial Model 
model_1 = glm(Attrition~., data = train, family = "binomial")
summary(model_1) #AIC: 2091.4

#Step wise selection
model_2 = stepAIC(model_1,direction = "both")
summary(model_2) #AIC: 2063.9

#Removing variable through VIF and p value check
library(car)
vif(model_2)

#Excluding Total working years 31 to 40
model_3 <- glm(formula = Attrition ~ Age.x30.40 + Age.x41.50 + Age.xAbove.50 + 
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + DistanceFromHome.xMore.than.20 + 
                 Education.x5 + EducationField.xLife.Sciences + EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 Gender + JobLevel.x5 + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 NumCompaniesWorked.x4.7 + NumCompaniesWorked.xFresher + NumCompaniesWorked.xMore.than.7 + 
                 StockOptionLevel.x1 + StockOptionLevel.x3 + TotalWorkingYears.x11.20 + 
                 TotalWorkingYears.x21.30  + TrainingTimesLastYear.x2 + 
                 TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
                 YearsAtCompany.x31.40 + YearsSinceLastPromotion.x11.15.Years + 
                 YearsSinceLastPromotion.x6.10.Years + YearsWithCurrManager.x11.17.Years + 
                 YearsWithCurrManager.x6.10.Years + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 JobInvolvement.x2 + JobInvolvement.x3 + Actual_working_hrs.x8 + 
                 Actual_working_hrs.x9 + Actual_working_hrs.x10 + Actual_working_hrs.x11 + 
                 YearsAtCompany.x21.30, family = "binomial", data = train)

summary(model_3) #AIC : 2082.6
vif(model_3)
#So now looking at the vif and p value we can remove Business travel Rarely

model_4 <- glm(formula = Attrition ~ Age.x30.40 + Age.x41.50 + Age.xAbove.50 + 
                 BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + DistanceFromHome.xMore.than.20 + 
                 Education.x5 + EducationField.xLife.Sciences + EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 Gender + JobLevel.x5 + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 NumCompaniesWorked.x4.7 + NumCompaniesWorked.xFresher + NumCompaniesWorked.xMore.than.7 + 
                 StockOptionLevel.x1 + StockOptionLevel.x3 + TotalWorkingYears.x11.20 + 
                 TotalWorkingYears.x21.30 + TrainingTimesLastYear.x2 + TrainingTimesLastYear.x4 + 
                 TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + YearsAtCompany.x31.40 + 
                 YearsSinceLastPromotion.x11.15.Years + YearsSinceLastPromotion.x6.10.Years + 
                 YearsWithCurrManager.x11.17.Years + YearsWithCurrManager.x6.10.Years + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x2 + JobInvolvement.x3 + 
                 Actual_working_hrs.x8 + Actual_working_hrs.x9 + Actual_working_hrs.x10 + 
                 Actual_working_hrs.x11 + YearsAtCompany.x21.30, family = "binomial", 
               data = train)

summary(model_4) #AIC : 2087.3
vif(model_4)

#let remove Education field life sciences
model_5 = glm(formula = Attrition ~ Age.x30.40 + Age.x41.50 + Age.xAbove.50 + 
                BusinessTravel.xTravel_Frequently + Department.xResearch...Development + 
                DistanceFromHome.xMore.than.20 + Education.x5 + 
                EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                EducationField.xTechnical.Degree + Gender + JobLevel.x5 + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                MaritalStatus.xMarried + MaritalStatus.xSingle + NumCompaniesWorked.x4.7 + 
                NumCompaniesWorked.xFresher + NumCompaniesWorked.xMore.than.7 + 
                StockOptionLevel.x1 + StockOptionLevel.x3 + TotalWorkingYears.x11.20 + 
                TotalWorkingYears.x21.30 + TrainingTimesLastYear.x2 + TrainingTimesLastYear.x4 + 
                TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + YearsAtCompany.x31.40 + 
                YearsSinceLastPromotion.x11.15.Years + YearsSinceLastPromotion.x6.10.Years + 
                YearsWithCurrManager.x11.17.Years + YearsWithCurrManager.x6.10.Years + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x2 + JobInvolvement.x3 + 
                Actual_working_hrs.x8 + Actual_working_hrs.x9 + Actual_working_hrs.x10 + 
                Actual_working_hrs.x11 + YearsAtCompany.x21.30, family = "binomial", 
              data = train)

summary(model_5) #AIC : 2101.6
vif(model_5)

#lets remove work life balace level 2 
model_6 = glm(formula = Attrition ~ Age.x30.40 + Age.x41.50 + Age.xAbove.50 + 
                BusinessTravel.xTravel_Frequently + Department.xResearch...Development + 
                DistanceFromHome.xMore.than.20 + Education.x5 + EducationField.xMarketing + 
                EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                Gender + JobLevel.x5 + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                NumCompaniesWorked.x4.7 + NumCompaniesWorked.xFresher + NumCompaniesWorked.xMore.than.7 + 
                StockOptionLevel.x1 + StockOptionLevel.x3 + TotalWorkingYears.x11.20 + 
                TotalWorkingYears.x21.30 + TrainingTimesLastYear.x2 + TrainingTimesLastYear.x4 + 
                TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + YearsAtCompany.x31.40 + 
                YearsSinceLastPromotion.x11.15.Years + YearsSinceLastPromotion.x6.10.Years + 
                YearsWithCurrManager.x11.17.Years + YearsWithCurrManager.x6.10.Years + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x2 + JobInvolvement.x3 + 
                Actual_working_hrs.x8 + Actual_working_hrs.x9 + Actual_working_hrs.x10 + 
                Actual_working_hrs.x11 + YearsAtCompany.x21.30, family = "binomial", 
              data = train)

summary(model_6) #AIC: 2121.5
vif(model_6)

#lets remove maritial status = married as its vif is above 2 and p value is 0.010559
model_7 = glm(formula = Attrition ~ Age.x30.40 + Age.x41.50 + Age.xAbove.50 + 
                BusinessTravel.xTravel_Frequently + Department.xResearch...Development + 
                DistanceFromHome.xMore.than.20 + Education.x5 + EducationField.xMarketing + 
                EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                Gender + JobLevel.x5 + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + MaritalStatus.xSingle + 
                NumCompaniesWorked.x4.7 + NumCompaniesWorked.xFresher + NumCompaniesWorked.xMore.than.7 + 
                StockOptionLevel.x1 + StockOptionLevel.x3 + TotalWorkingYears.x11.20 + 
                TotalWorkingYears.x21.30 + TrainingTimesLastYear.x2 + TrainingTimesLastYear.x4 + 
                TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + YearsAtCompany.x31.40 + 
                YearsSinceLastPromotion.x11.15.Years + YearsSinceLastPromotion.x6.10.Years + 
                YearsWithCurrManager.x11.17.Years + YearsWithCurrManager.x6.10.Years + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x2 + JobInvolvement.x3 + 
                Actual_working_hrs.x8 + Actual_working_hrs.x9 + Actual_working_hrs.x10 + 
                Actual_working_hrs.x11 + YearsAtCompany.x21.30, family = "binomial", 
              data = train)

summary(model_7) #AIC: 2126.3
vif(model_7)

#now looking at the vif you see that every vif is below 2
#so now focusing on just p value
#lest remove work life balance with level 4
model_8 = glm(formula = Attrition ~ Age.x30.40 + Age.x41.50 + Age.xAbove.50 + 
                BusinessTravel.xTravel_Frequently + Department.xResearch...Development + 
                DistanceFromHome.xMore.than.20 + Education.x5 + EducationField.xMarketing + 
                EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                Gender + JobLevel.x5 + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + MaritalStatus.xSingle + 
                NumCompaniesWorked.x4.7 + NumCompaniesWorked.xFresher + NumCompaniesWorked.xMore.than.7 + 
                StockOptionLevel.x1 + StockOptionLevel.x3 + TotalWorkingYears.x11.20 + 
                TotalWorkingYears.x21.30 + TrainingTimesLastYear.x2 + TrainingTimesLastYear.x4 + 
                TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + YearsAtCompany.x31.40 + 
                YearsSinceLastPromotion.x11.15.Years + YearsSinceLastPromotion.x6.10.Years + 
                YearsWithCurrManager.x11.17.Years + YearsWithCurrManager.x6.10.Years + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x3  + JobInvolvement.x2 + JobInvolvement.x3 + 
                Actual_working_hrs.x8 + Actual_working_hrs.x9 + Actual_working_hrs.x10 + 
                Actual_working_hrs.x11 + YearsAtCompany.x21.30, family = "binomial", 
              data = train)

summary(model_8) #AIC: 2124.6

#lets remove years at company
model_9 = glm(formula = Attrition ~ Age.x30.40 + Age.x41.50 + Age.xAbove.50 + 
                BusinessTravel.xTravel_Frequently + Department.xResearch...Development + 
                DistanceFromHome.xMore.than.20 + Education.x5 + EducationField.xMarketing + EducationField.xMedical +
                EducationField.xOther + EducationField.xTechnical.Degree + 
                Gender + JobLevel.x5 + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + MaritalStatus.xSingle + 
                NumCompaniesWorked.x4.7 + NumCompaniesWorked.xFresher + NumCompaniesWorked.xMore.than.7 + 
                StockOptionLevel.x1 + StockOptionLevel.x3 + TotalWorkingYears.x11.20 + 
                TotalWorkingYears.x21.30 + TrainingTimesLastYear.x2 + TrainingTimesLastYear.x4 + 
                TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + YearsAtCompany.x31.40 + 
                YearsSinceLastPromotion.x11.15.Years + YearsSinceLastPromotion.x6.10.Years + 
                YearsWithCurrManager.x11.17.Years + YearsWithCurrManager.x6.10.Years + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x3  + JobInvolvement.x2 + JobInvolvement.x3 + 
                Actual_working_hrs.x8 + Actual_working_hrs.x9 + Actual_working_hrs.x10 + 
                Actual_working_hrs.x11, family = "binomial", 
              data = train)

summary(model_9)#AIC : 2122.9

#lets remove education field medical
model_10 = glm(formula = Attrition ~ Age.x30.40 + Age.x41.50 + Age.xAbove.50 + 
                 BusinessTravel.xTravel_Frequently + Department.xResearch...Development + 
                 DistanceFromHome.xMore.than.20 + Education.x5 + EducationField.xMarketing +
                 EducationField.xOther + EducationField.xTechnical.Degree + 
                 Gender + JobLevel.x5 + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + MaritalStatus.xSingle + 
                 NumCompaniesWorked.x4.7 + NumCompaniesWorked.xFresher + NumCompaniesWorked.xMore.than.7 + 
                 StockOptionLevel.x1 + StockOptionLevel.x3 + TotalWorkingYears.x11.20 + 
                 TotalWorkingYears.x21.30 + TrainingTimesLastYear.x2 + TrainingTimesLastYear.x4 + 
                 TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + YearsAtCompany.x31.40 + 
                 YearsSinceLastPromotion.x11.15.Years + YearsSinceLastPromotion.x6.10.Years + 
                 YearsWithCurrManager.x11.17.Years + YearsWithCurrManager.x6.10.Years + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x3  + JobInvolvement.x2 + JobInvolvement.x3 + 
                 Actual_working_hrs.x8 + Actual_working_hrs.x9 + Actual_working_hrs.x10 + 
                 Actual_working_hrs.x11, family = "binomial", 
               data = train)

summary(model_10) #AIC :2121.7

#let remove distance from home more than 20 
model_11 = glm(formula = Attrition ~ Age.x30.40 + Age.x41.50 + Age.xAbove.50 + 
                 BusinessTravel.xTravel_Frequently + Department.xResearch...Development + 
                 Education.x5 + EducationField.xMarketing +
                 EducationField.xOther + EducationField.xTechnical.Degree + 
                 Gender + JobLevel.x5 + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + MaritalStatus.xSingle + 
                 NumCompaniesWorked.x4.7 + NumCompaniesWorked.xFresher + NumCompaniesWorked.xMore.than.7 + 
                 StockOptionLevel.x1 + StockOptionLevel.x3 + TotalWorkingYears.x11.20 + 
                 TotalWorkingYears.x21.30 + TrainingTimesLastYear.x2 + TrainingTimesLastYear.x4 + 
                 TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + YearsAtCompany.x31.40 + 
                 YearsSinceLastPromotion.x11.15.Years + YearsSinceLastPromotion.x6.10.Years + 
                 YearsWithCurrManager.x11.17.Years + YearsWithCurrManager.x6.10.Years + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x3  + JobInvolvement.x2 + JobInvolvement.x3 + 
                 Actual_working_hrs.x8 + Actual_working_hrs.x9 + Actual_working_hrs.x10 + 
                 Actual_working_hrs.x11, family = "binomial", 
               data = train)

summary(model_11) #2120.6

#removing TrainingTimesLastYear level 2
model_12 = glm(formula = Attrition ~ Age.x30.40 + Age.x41.50 + Age.xAbove.50 + 
                 BusinessTravel.xTravel_Frequently + Department.xResearch...Development + 
                 Education.x5 + EducationField.xMarketing +
                 EducationField.xOther + EducationField.xTechnical.Degree + 
                 Gender + JobLevel.x5 + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + MaritalStatus.xSingle + 
                 NumCompaniesWorked.x4.7 + NumCompaniesWorked.xFresher + NumCompaniesWorked.xMore.than.7 + 
                 StockOptionLevel.x1 + StockOptionLevel.x3 + TotalWorkingYears.x11.20 + 
                 TotalWorkingYears.x21.30 + TrainingTimesLastYear.x4 + 
                 TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + YearsAtCompany.x31.40 + 
                 YearsSinceLastPromotion.x11.15.Years + YearsSinceLastPromotion.x6.10.Years + 
                 YearsWithCurrManager.x11.17.Years + YearsWithCurrManager.x6.10.Years + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x3  + JobInvolvement.x2 + JobInvolvement.x3 + 
                 Actual_working_hrs.x8 + Actual_working_hrs.x9 + Actual_working_hrs.x10 + 
                 Actual_working_hrs.x11, family = "binomial", 
               data = train)

summary(model_12) #AIC : 2119.6

#lets remove StockOption Level 3 
model_13 = glm(formula = Attrition ~ Age.x30.40 + Age.x41.50 + Age.xAbove.50 + 
                 BusinessTravel.xTravel_Frequently + Department.xResearch...Development + 
                 Education.x5 + EducationField.xMarketing +
                 EducationField.xOther + EducationField.xTechnical.Degree + 
                 Gender + JobLevel.x5 + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + MaritalStatus.xSingle + 
                 NumCompaniesWorked.x4.7 + NumCompaniesWorked.xFresher + NumCompaniesWorked.xMore.than.7 + 
                 StockOptionLevel.x1 + TotalWorkingYears.x11.20 + 
                 TotalWorkingYears.x21.30 + TrainingTimesLastYear.x4 + 
                 TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + YearsAtCompany.x31.40 + 
                 YearsSinceLastPromotion.x11.15.Years + YearsSinceLastPromotion.x6.10.Years + 
                 YearsWithCurrManager.x11.17.Years + YearsWithCurrManager.x6.10.Years + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x3  + JobInvolvement.x2 + JobInvolvement.x3 + 
                 Actual_working_hrs.x8 + Actual_working_hrs.x9 + Actual_working_hrs.x10 + 
                 Actual_working_hrs.x11, family = "binomial", 
               data = train)

summary(model_13) #AIC : 2119.2

#lets remove TotalWorkingYears.x11.20
model_14 = glm(formula = Attrition ~ Age.x30.40 + Age.x41.50 + Age.xAbove.50 + 
                 BusinessTravel.xTravel_Frequently + Department.xResearch...Development + 
                 Education.x5 + EducationField.xMarketing +
                 EducationField.xOther + EducationField.xTechnical.Degree + 
                 Gender + JobLevel.x5 + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + MaritalStatus.xSingle + 
                 NumCompaniesWorked.x4.7 + NumCompaniesWorked.xFresher + NumCompaniesWorked.xMore.than.7 + 
                 StockOptionLevel.x1 + TotalWorkingYears.x21.30 + TrainingTimesLastYear.x4 +
                 TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + YearsAtCompany.x31.40 + 
                 YearsSinceLastPromotion.x11.15.Years + YearsSinceLastPromotion.x6.10.Years + 
                 YearsWithCurrManager.x11.17.Years + YearsWithCurrManager.x6.10.Years + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x3  + JobInvolvement.x2 + JobInvolvement.x3 + 
                 Actual_working_hrs.x8 + Actual_working_hrs.x9 + Actual_working_hrs.x10 + 
                 Actual_working_hrs.x11, family = "binomial", 
               data = train)
summary(model_14) #AIC : 2119.1

#lets remove gender
model_15 = glm(formula = Attrition ~ Age.x30.40 + Age.x41.50 + Age.xAbove.50 + 
                 BusinessTravel.xTravel_Frequently + Department.xResearch...Development + 
                 Education.x5 + EducationField.xMarketing +
                 EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobLevel.x5 + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + MaritalStatus.xSingle + 
                 NumCompaniesWorked.x4.7 + NumCompaniesWorked.xFresher + NumCompaniesWorked.xMore.than.7 + 
                 StockOptionLevel.x1 + TotalWorkingYears.x21.30 + TrainingTimesLastYear.x4 +
                 TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + YearsAtCompany.x31.40 + 
                 YearsSinceLastPromotion.x11.15.Years + YearsSinceLastPromotion.x6.10.Years + 
                 YearsWithCurrManager.x11.17.Years + YearsWithCurrManager.x6.10.Years + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x3  + JobInvolvement.x2 + JobInvolvement.x3 + 
                 Actual_working_hrs.x8 + Actual_working_hrs.x9 + Actual_working_hrs.x10 + 
                 Actual_working_hrs.x11, family = "binomial", 
               data = train)

summary(model_15)#AIC : 2119.5

#removing TrainingTimesLastYear level 5
model_16 = glm(formula = Attrition ~ Age.x30.40 + Age.x41.50 + Age.xAbove.50 + 
                 BusinessTravel.xTravel_Frequently + Department.xResearch...Development + 
                 Education.x5 + EducationField.xMarketing +
                 EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobLevel.x5 + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + MaritalStatus.xSingle + 
                 NumCompaniesWorked.x4.7 + NumCompaniesWorked.xFresher + NumCompaniesWorked.xMore.than.7 + 
                 StockOptionLevel.x1 + TotalWorkingYears.x21.30 + TrainingTimesLastYear.x4 +
                 TrainingTimesLastYear.x6 + YearsAtCompany.x31.40 + 
                 YearsSinceLastPromotion.x11.15.Years + YearsSinceLastPromotion.x6.10.Years + 
                 YearsWithCurrManager.x11.17.Years + YearsWithCurrManager.x6.10.Years + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x3  + JobInvolvement.x2 + JobInvolvement.x3 + 
                 Actual_working_hrs.x8 + Actual_working_hrs.x9 + Actual_working_hrs.x10 + 
                 Actual_working_hrs.x11, family = "binomial", 
               data = train)

summary(model_16)#AIC : 2120

#removing TrainingTimesLastYear.x4
model_17 = glm(formula = Attrition ~ Age.x30.40 + Age.x41.50 + Age.xAbove.50 + 
                 BusinessTravel.xTravel_Frequently + Department.xResearch...Development + 
                 Education.x5 + EducationField.xMarketing +
                 EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobLevel.x5 + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + MaritalStatus.xSingle + 
                 NumCompaniesWorked.x4.7 + NumCompaniesWorked.xFresher + NumCompaniesWorked.xMore.than.7 + 
                 StockOptionLevel.x1 + TotalWorkingYears.x21.30 +
                 TrainingTimesLastYear.x6 + YearsAtCompany.x31.40 + 
                 YearsSinceLastPromotion.x11.15.Years + YearsSinceLastPromotion.x6.10.Years + 
                 YearsWithCurrManager.x11.17.Years + YearsWithCurrManager.x6.10.Years + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x3  + JobInvolvement.x2 + JobInvolvement.x3 + 
                 Actual_working_hrs.x8 + Actual_working_hrs.x9 + Actual_working_hrs.x10 + 
                 Actual_working_hrs.x11, family = "binomial", 
               data = train)

summary(model_17)#2121.3

#removing education field other
model_18 = glm(formula = Attrition ~ Age.x30.40 + Age.x41.50 + Age.xAbove.50 + 
                 BusinessTravel.xTravel_Frequently + Department.xResearch...Development + 
                 Education.x5 + EducationField.xMarketing +
                 EducationField.xTechnical.Degree + 
                 JobLevel.x5 + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + MaritalStatus.xSingle + 
                 NumCompaniesWorked.x4.7 + NumCompaniesWorked.xFresher + NumCompaniesWorked.xMore.than.7 + 
                 StockOptionLevel.x1 + TotalWorkingYears.x21.30 +
                 TrainingTimesLastYear.x6 + YearsAtCompany.x31.40 + 
                 YearsSinceLastPromotion.x11.15.Years + YearsSinceLastPromotion.x6.10.Years + 
                 YearsWithCurrManager.x11.17.Years + YearsWithCurrManager.x6.10.Years + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x3  + JobInvolvement.x2 + JobInvolvement.x3 + 
                 Actual_working_hrs.x8 + Actual_working_hrs.x9 + Actual_working_hrs.x10 + 
                 Actual_working_hrs.x11, family = "binomial", 
               data = train)

summary(model_18)#AIC: 2123.3

#Removing NumCompaniesWorked.xFresher
model_19 = glm(formula = Attrition ~ Age.x30.40 + Age.x41.50 + Age.xAbove.50 + 
                 BusinessTravel.xTravel_Frequently + Department.xResearch...Development + 
                 Education.x5 + EducationField.xMarketing +
                 EducationField.xTechnical.Degree + 
                 JobLevel.x5 + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + MaritalStatus.xSingle + 
                 NumCompaniesWorked.x4.7 + NumCompaniesWorked.xMore.than.7 + 
                 StockOptionLevel.x1 + TotalWorkingYears.x21.30 +
                 TrainingTimesLastYear.x6 + YearsAtCompany.x31.40 + 
                 YearsSinceLastPromotion.x11.15.Years + YearsSinceLastPromotion.x6.10.Years + 
                 YearsWithCurrManager.x11.17.Years + YearsWithCurrManager.x6.10.Years + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x3  + JobInvolvement.x2 + JobInvolvement.x3 + 
                 Actual_working_hrs.x8 + Actual_working_hrs.x9 + Actual_working_hrs.x10 + 
                 Actual_working_hrs.x11, family = "binomial", 
               data = train)

summary(model_19)#AIC : 2124.4

#removing YearsSinceLastPromotion.x11.15.Years
model_20 = glm(formula = Attrition ~ Age.x30.40 + Age.x41.50 + Age.xAbove.50 + 
                 BusinessTravel.xTravel_Frequently + Department.xResearch...Development + 
                 Education.x5 + EducationField.xMarketing +
                 EducationField.xTechnical.Degree + 
                 JobLevel.x5 + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + MaritalStatus.xSingle + 
                 NumCompaniesWorked.x4.7 + NumCompaniesWorked.xMore.than.7 + 
                 StockOptionLevel.x1 + TotalWorkingYears.x21.30 +
                 TrainingTimesLastYear.x6 + YearsAtCompany.x31.40 + 
                 YearsSinceLastPromotion.x6.10.Years + 
                 YearsWithCurrManager.x11.17.Years + YearsWithCurrManager.x6.10.Years + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x3  + JobInvolvement.x2 + JobInvolvement.x3 + 
                 Actual_working_hrs.x8 + Actual_working_hrs.x9 + Actual_working_hrs.x10 + 
                 Actual_working_hrs.x11, family = "binomial", 
               data = train)

summary(model_20) #AIC : 2126.2

#lets remove TotalWorkingYears.x21.30
model_21 = glm(formula = Attrition ~ Age.x30.40 + Age.x41.50 + Age.xAbove.50 + 
                 BusinessTravel.xTravel_Frequently + Department.xResearch...Development + 
                 Education.x5 + EducationField.xMarketing +
                 EducationField.xTechnical.Degree + 
                 JobLevel.x5 + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + MaritalStatus.xSingle + 
                 NumCompaniesWorked.x4.7 + NumCompaniesWorked.xMore.than.7 + 
                 StockOptionLevel.x1 +TrainingTimesLastYear.x6 + YearsAtCompany.x31.40 +
                 YearsSinceLastPromotion.x6.10.Years + 
                 YearsWithCurrManager.x11.17.Years + YearsWithCurrManager.x6.10.Years + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x3  + JobInvolvement.x2 + JobInvolvement.x3 + 
                 Actual_working_hrs.x8 + Actual_working_hrs.x9 + Actual_working_hrs.x10 + 
                 Actual_working_hrs.x11, family = "binomial", 
               data = train)
summary(model_21) #AIC : 2128.8

#so if you see at model_19 each variable p-value is below 0.05
#and if we go by the stars and started removing variables which have 1 star but value is less than 0.05
#then you see that out AIC as well increases significantly like by 2.0 or so
#so finalizing on model_19 as our final model, we will go further

#final model
final_model <- model_19#AIC : 2124.4

#############################Model Evaluation################################

#predicting the probabilities of attrition for test data
test_pred <- predict(final_model, type = "response", newdata = test[,-1])

#lets see the summary
summary(test_pred)

test$prob <- test_pred

#lets use the probability cutoff of 50%
test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))

library(caret)
#confusion matrix
#table(test_actual_attrition,test_pred_attrition)
test_conf <- confusionMatrix(test_pred_attrition,test_actual_attrition,positive = "Yes")
test_conf

#so in confusion matrix for threshold 0.50 our accuracy is 83% which is good
#but the sensitivity is only 18%, that is our model is predicting only 18% employees correctly who will leave the firm(with threshold 0.50)
#and specificity is 95%
#but according to our business case sensitivity should be on higher saide, so maing necessary changes to make adjustment

#Now lets select the threshold value
perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#creating cutoff value from 0.000701 to 0.920678
summary(test_pred)

s = seq(.01,.92,length=100)


OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#plot for sensitivity & specificity trend
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]

#let's choose cutoff value of 0.14787879 from final model

test_cutoff_attrition <- factor(ifelse(test_pred >=0.14787879, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")
conf_final

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc # 0.713308

sens # 0.7264151 

spec # 0.7107888

View(test)

#so from this we get to know that if we make yes for all those record where probability is greater than 0.14787879
#we achieve better result in terms of accuracy, sensitivity and specificity.

# Factors that affecting more people to leave the firm are as follows
#Business travel : Travel frequently
#Education field : HR 
#Department : HR
#Maritial Status : Single
#Job Role : Manufacturing director and Research Director
#Job Level : 5
#No of companies worked : 4 to 7 or more than 7
#training time last year : level 6
#Years at company : 31 to 40
#years with current manager
#Actual Working hours : more than 8 hours 
#Age 
#Environment satisfaction
#Job satisfaction


#moderatlt affecting variables
#total working years : 21 to 30 or fresher
#year since last promotion : 11 to 15 years
#Job involvement
#work life balance
#stock option level 1
#Education