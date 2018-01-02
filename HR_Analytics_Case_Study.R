library(dplyr)
library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(GGally)
library(ROCR)
#reading all the CSV files and converting to data frame
employee_survey_data<-read.csv("employee_survey_data.csv",stringsAsFactors = F)
manager_survey_data<-read.csv("manager_survey_data.csv",stringsAsFactors = F)
general_data<-read.csv("general_data.csv",stringsAsFactors = F)
in_time <- read.csv("in_time.csv",stringsAsFactors = F)
out_time <- read.csv("out_time.csv",stringsAsFactors = F)

str(employee_survey_data)    
str(manager_survey_data)    
str(general_data)    
str(in_time)    
str(out_time)    
#all have 4410 variables

#checking for unique Ids now
length(unique(tolower(employee_survey_data$EmployeeID)))  
length(unique(tolower(manager_survey_data$EmployeeID))) 
length(unique(tolower(general_data$EmployeeID)))
length(unique(tolower(in_time$X))) 
length(unique(tolower(out_time$X))) 


#calculating the average working hours per employyee using out_time and in_time
#converting to time format
in_time <- sapply(in_time, function(x) as.POSIXlt(x, origin="1970-01-01","%y-%m-%d %H:%M:%S"))
in_time<-as.data.frame(in_time)

out_time <- sapply(out_time, function(x) as.POSIXlt(x, origin="1970-01-01","%y-%m-%d %H:%M:%S"))
out_time<-as.data.frame(out_time)

#removing first column for both data frames as they are not useful
in_time$X<-NULL
out_time$X<-NULL

#calculating the difference
diff<-out_time-in_time

#remove columns with all NA values
diff <- diff[,colSums(is.na(diff))<nrow(diff)]

#converting all values to numeric
diff<-sapply(diff,function(x) as.numeric(x))
diff<-as.data.frame(diff)
#checking diff data frame
class(diff)

#new vector for storing employee id to join with avg work hours
EmployeeID<-seq(from = 1, to = 4410, by = 1)

#aggregating mean of each row #roll up
diff$AvgWorkHrs<-apply(diff,1,mean,na.rm=TRUE)

#creating Average work hours per employeee data frame
AvgWorkHrs<-cbind(EmployeeID,diff$AvgWorkHrs)
AvgWorkHrs<-as.data.frame(AvgWorkHrs)

#using setdiff to check wether all data is for same employees only
setdiff(employee_survey_data$EmployeeID,manager_survey_data$EmployeeID) 
setdiff(manager_survey_data$EmployeeID,general_data$EmployeeID) 
setdiff(general_data$EmployeeID,AvgWorkHrs$EmployeeID) 
# since setdiff is 0 we know all data is for same employees only

#merging all the data frames to create master data frame 
attrition_data<-merge(employee_survey_data,manager_survey_data,by="EmployeeID",all=F)
attrition_data<-merge(attrition_data,general_data,by="EmployeeID",all=F)
attrition_data<-merge(attrition_data,AvgWorkHrs,by="EmployeeID",all = F)
colnames(attrition_data)[30] <- "AvgWorkhrs"
View(attrition_data)

#we have master frame attrition_data

#data Cleaning
str(attrition_data)

# De-Duplication is not needed 

#removing 3 variables from data frame which have the same value for all rows
attrition_data$EmployeeCount<-NULL
attrition_data$Over18<-NULL
attrition_data$StandardHours<-NULL
# Bringing a few variables to the correct format as given in data dictionary, 
# we are converting the numeric values to categorical variables
attrition_data$Education[which(attrition_data$Education==1)]<-'Below College'
attrition_data$Education[which(attrition_data$Education==2)]<-'College'
attrition_data$Education[which(attrition_data$Education==3)]<-'Bachelor'
attrition_data$Education[which(attrition_data$Education==4)]<-'Master'
attrition_data$Education[which(attrition_data$Education==5)]<-'Doctor'
attrition_data$EnvironmentSatisfaction[which(attrition_data$EnvironmentSatisfaction==1)]<-'Low'
attrition_data$EnvironmentSatisfaction[which(attrition_data$EnvironmentSatisfaction==2)]<-'Medium'
attrition_data$EnvironmentSatisfaction[which(attrition_data$EnvironmentSatisfaction==3)]<-'High'
attrition_data$EnvironmentSatisfaction[which(attrition_data$EnvironmentSatisfaction==4)]<-'Very High'
attrition_data$JobInvolvement[which(attrition_data$JobInvolvement==1)]<-'Low'
attrition_data$JobInvolvement[which(attrition_data$JobInvolvement==2)]<-'Medium'
attrition_data$JobInvolvement[which(attrition_data$JobInvolvement==3)]<-'High'
attrition_data$JobInvolvement[which(attrition_data$JobInvolvement==4)]<-'Very High'
attrition_data$JobSatisfaction[which(attrition_data$JobSatisfaction==1)]<-'Low'
attrition_data$JobSatisfaction[which(attrition_data$JobSatisfaction==2)]<-'Medium'
attrition_data$JobSatisfaction[which(attrition_data$JobSatisfaction==3)]<-'High'
attrition_data$JobSatisfaction[which(attrition_data$JobSatisfaction==4)]<-'Very High'
attrition_data$WorkLifeBalance[which(attrition_data$WorkLifeBalance==1)]<-'Bad'
attrition_data$WorkLifeBalance[which(attrition_data$WorkLifeBalance==2)]<-'Good'
attrition_data$WorkLifeBalance[which(attrition_data$WorkLifeBalance==3)]<-'Better'
attrition_data$WorkLifeBalance[which(attrition_data$WorkLifeBalance==4)]<-'Best'
attrition_data$PerformanceRating[which(attrition_data$PerformanceRating==1)]<-'Low'
attrition_data$PerformanceRating[which(attrition_data$PerformanceRating==2)]<-'Good'
attrition_data$PerformanceRating[which(attrition_data$PerformanceRating==3)]<-'Excellent'
attrition_data$PerformanceRating[which(attrition_data$PerformanceRating==4)]<-'Outstanding'

#checking the data set again

str(attrition_data)
#EDA , here we check the impact of categorical and continuous variables on attrition
#checking categorical variables using bar charts
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")

plot_grid(ggplot(attrition_data, aes(x=factor(EnvironmentSatisfaction),fill=Attrition))+ geom_bar(), 
          ggplot(attrition_data, aes(x=factor(JobSatisfaction),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(attrition_data, aes(x=factor(WorkLifeBalance),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(attrition_data, aes(x=factor(JobInvolvement),fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")   
plot_grid(ggplot(attrition_data, aes(x=factor(PerformanceRating),fill=Attrition))+ geom_bar(), 
          ggplot(attrition_data, aes(x=factor(BusinessTravel),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(attrition_data, aes(x=factor(Department),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(attrition_data, aes(x=factor(Education),fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")  
plot_grid(ggplot(attrition_data, aes(x=factor(EducationField),fill=Attrition))+ geom_bar(), 
          ggplot(attrition_data, aes(x=factor(Gender),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(attrition_data, aes(x=factor(JobRole),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(attrition_data, aes(x=factor(MaritalStatus),fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")  

# Histogram and Boxplots for numeric variables 
box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

plot_grid(ggplot(attrition_data, aes(Age))+ geom_histogram(binwidth = 10),
          ggplot(attrition_data, aes(x="",y=Age))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(attrition_data, aes(AvgWorkhrs))+ geom_histogram(binwidth = 10),
          ggplot(attrition_data, aes(x="",y=AvgWorkhrs))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(attrition_data, aes(DistanceFromHome))+ geom_histogram(binwidth = 10),
          ggplot(attrition_data, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(attrition_data, aes(MonthlyIncome))+ geom_histogram(binwidth = 10),
          ggplot(attrition_data, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(attrition_data, aes(PercentSalaryHike))+ geom_histogram(binwidth = 10),
          ggplot(attrition_data, aes(x="",y=PercentSalaryHike))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(attrition_data, aes(YearsAtCompany))+ geom_histogram(binwidth = 10),
          ggplot(attrition_data, aes(x="",y=YearsAtCompany))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(attrition_data, aes(JobLevel))+ geom_histogram(binwidth = 10),
          ggplot(attrition_data, aes(x="",y=JobLevel))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(attrition_data, aes(NumCompaniesWorked))+ geom_histogram(binwidth = 10),
          ggplot(attrition_data, aes(x="",y=NumCompaniesWorked))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(attrition_data, aes(StockOptionLevel))+ geom_histogram(binwidth = 10),
          ggplot(attrition_data, aes(x="",y=StockOptionLevel))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(attrition_data, aes(TotalWorkingYears))+ geom_histogram(binwidth = 10),
          ggplot(attrition_data, aes(x="",y=TotalWorkingYears))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(attrition_data, aes(TrainingTimesLastYear))+ geom_histogram(binwidth = 10),
          ggplot(attrition_data, aes(x="",y=TrainingTimesLastYear))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(attrition_data, aes(YearsSinceLastPromotion))+ geom_histogram(binwidth = 10),
          ggplot(attrition_data, aes(x="",y=YearsSinceLastPromotion))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(attrition_data, aes(YearsWithCurrManager))+ geom_histogram(binwidth = 10),
          ggplot(attrition_data, aes(x="",y=YearsWithCurrManager))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

# Data Preparation

str(attrition_data)

# Outlier treatment and imputing missing value
#removing outliers for few variables from the data set as seen from the boxplots
box <- boxplot.stats(attrition_data$YearsAtCompany)
out <- box$out
ad1 <- attrition_data[ !attrition_data$YearsAtCompany %in% out, ]
attrition <- ad1
box <- boxplot.stats(attrition$AvgWorkhrs)
out <- box$out
ad1 <- attrition[ !attrition$AvgWorkhrs %in% out, ]
attrition <- ad1
box <- boxplot.stats(attrition$NumCompaniesWorked)
out <- box$out
ad1 <- attrition[ !attrition$NumCompaniesWorked %in% out, ]
attrition <- ad1
box <- boxplot.stats(attrition$StockOptionLevel)
out <- box$out
ad1 <- attrition[ !attrition$StockOptionLevel %in% out, ]
attrition <- ad1
box <- boxplot.stats(attrition$TotalWorkingYears)
out <- box$out
ad1 <- attrition[ !attrition$TotalWorkingYears %in% out, ]
attrition <- ad1
box <- boxplot.stats(attrition$TrainingTimesLastYear)
out <- box$out
ad1 <- attrition[ !attrition$TrainingTimesLastYear %in% out, ]
attrition <- ad1
box <- boxplot.stats(attrition$YearsSinceLastPromotion)
out <- box$out
ad1 <- attrition[ !attrition$YearsSinceLastPromotion %in% out, ]
attrition <- ad1
box <- boxplot.stats(attrition$YearsWithCurrManager)
out <- box$out
ad1 <- attrition[ !attrition$YearsWithCurrManager %in% out, ]
attrition <- ad1


# Missing value
sapply(attrition, function(x) sum(is.na(x)))
#checking NA vvalues for all the columns

# removing NAs as they dont have aisgnificant presence in the data set, 63/2484=2% NAs 
attrition <- attrition[!is.na(attrition$EnvironmentSatisfaction),]
attrition <- attrition[!is.na(attrition$JobSatisfaction),]
attrition <- attrition[!is.na(attrition$WorkLifeBalance),]
attrition <- attrition[!is.na(attrition$NumCompaniesWorked),]
attrition <- attrition[!is.na(attrition$TotalWorkingYears),]
#check for NAs again
sapply(attrition, function(x) sum(is.na(x))) 
#removed NA from the data set, now we have no NAs and outliers

#checking data set again
str(attrition)


# Feature standardisation
#scaling continuos variables
# Normalising continuous features 
attrition$Age<- scale(attrition$Age) 
attrition$DistanceFromHome<- scale(attrition$DistanceFromHome) 
attrition$JobLevel<- scale(attrition$JobLevel) 
attrition$MonthlyIncome<- scale(attrition$MonthlyIncome) 
attrition$NumCompaniesWorked<- scale(attrition$NumCompaniesWorked) 
attrition$PercentSalaryHike<- scale(attrition$PercentSalaryHike) 
attrition$StockOptionLevel<- scale(attrition$StockOptionLevel) 
attrition$TotalWorkingYears<- scale(attrition$TotalWorkingYears) 
attrition$TrainingTimesLastYear<- scale(attrition$TrainingTimesLastYear) 
attrition$YearsAtCompany<- scale(attrition$YearsAtCompany) 
attrition$YearsSinceLastPromotion<- scale(attrition$YearsSinceLastPromotion) 
attrition$YearsWithCurrManager<- scale(attrition$YearsWithCurrManager) 
attrition$AvgWorkhrs<- scale(attrition$AvgWorkhrs) 

# converting target variable Attrition from No/Yes character to factorwith levels 0/1 
attrition$Attrition<- ifelse(attrition$Attrition=="Yes",1,0)

# Checking attrition rate of prospect employee

attrition_percent <- sum(attrition$Attrition)/nrow(attrition)
attrition_percent # 17.48% attrition rate. 

View(attrition)
# creating a dataframe of categorical features
attrition_chr<- attrition[,c(2,3,4,5,6,9,10,12,13,14,16,17)]

# converting categorical attributes to factor
attrition_fact<- data.frame(sapply(attrition_chr, function(x) factor(x)))
str(attrition_fact)

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(attrition_fact, 
                            function(x) data.frame(model.matrix(~x-1,data =attrition_fact))))



# Final dataset
attrition_final<- cbind(attrition[,c(7,8,11,15,18,19,20,21,22,23,24,25,26,27)],dummies) 
View(attrition_final)


# splitting the data between train and test
set.seed(100)

indices = sample.split(attrition_final$Attrition, SplitRatio = 0.7)

train = attrition_final[indices,]

test = attrition_final[!(indices),]


# Logistic Regression: 

#Initial model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1)

# Stepwise selection
library("MASS")
model_2<- stepAIC(model_1, direction="both")

summary(model_2)

# Removing multicollinearity through VIF check
library(car)
vif(model_2)
#all have low vif , we have to check by P value

#Excluding JobLevel
model_3<- glm(formula = Attrition ~ Age + DistanceFromHome + 
                NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                YearsWithCurrManager + AvgWorkhrs + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xMedium + 
                JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + 
                WorkLifeBalance.xBad + BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + 
                Department.xHuman.Resources + EducationField.xLife.Sciences + 
                EducationField.xMedical + Gender.xFemale + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xDivorced + MaritalStatus.xMarried, family = "binomial", 
              data = train) 

summary(model_3) 

vif(model_3) 

#removing Gender.XFemale

model_4<- glm(formula = Attrition ~ Age + DistanceFromHome + 
                NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                YearsWithCurrManager + AvgWorkhrs + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xMedium + 
                JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + 
                WorkLifeBalance.xBad + BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + 
                Department.xHuman.Resources + EducationField.xLife.Sciences + 
                EducationField.xMedical + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xDivorced + MaritalStatus.xMarried, family = "binomial", 
              data = train) 

summary(model_4)

vif(model_4) # cannot exclude any more variable based on vif 
#as most of them have low vif; those with higher vif are very significant and not correlated

#Excluding DistanceFromHome
model_5<- glm(formula = Attrition ~ Age + 
                NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                YearsWithCurrManager + AvgWorkhrs + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xMedium + 
                JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + 
                WorkLifeBalance.xBad + BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + 
                Department.xHuman.Resources + EducationField.xLife.Sciences + 
                EducationField.xMedical + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xDivorced + MaritalStatus.xMarried, family = "binomial", 
              data = train)

summary(model_5) 

#Excluding PercentSalarYHike
model_6<-  glm(formula = Attrition ~ Age + 
                 NumCompaniesWorked + TotalWorkingYears + 
                 YearsWithCurrManager + AvgWorkhrs + EnvironmentSatisfaction.xHigh + 
                 EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xMedium + 
                 JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + 
                 WorkLifeBalance.xBad + BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + 
                 Department.xHuman.Resources + EducationField.xLife.Sciences + 
                 EducationField.xMedical + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xDivorced + MaritalStatus.xMarried, family = "binomial", 
               data = train)
summary(model_6)
vif(model_6)

#Excluding jobrole research director
model_7<- glm(formula = Attrition ~ Age + 
                NumCompaniesWorked + TotalWorkingYears + 
                YearsWithCurrManager + AvgWorkhrs + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xMedium + 
                JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + 
                WorkLifeBalance.xBad + BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + 
                Department.xHuman.Resources + EducationField.xLife.Sciences + 
                EducationField.xMedical + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xDivorced + MaritalStatus.xMarried, family = "binomial", 
              data = train)   

summary(model_7) 

#Excluding EducationFieldXmedicine
model_8<- glm(formula = Attrition ~ Age + 
                          NumCompaniesWorked + TotalWorkingYears + 
                          YearsWithCurrManager + AvgWorkhrs + EnvironmentSatisfaction.xHigh + 
                          EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xMedium + 
                          JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + 
                          WorkLifeBalance.xBad + BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + 
                          Department.xHuman.Resources + EducationField.xLife.Sciences + 
                          JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                          MaritalStatus.xDivorced + MaritalStatus.xMarried, family = "binomial", 
                        data = train)   

summary(model_8) 
vif(model_8)


#Excluding EducationFieldXLifeSciences
model_9<-glm(formula = Attrition ~ Age + 
                          NumCompaniesWorked + TotalWorkingYears + 
                          YearsWithCurrManager + AvgWorkhrs + EnvironmentSatisfaction.xHigh + 
                          EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xMedium + 
                          JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + 
                          WorkLifeBalance.xBad + BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + 
                          Department.xHuman.Resources + 
                          JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                          MaritalStatus.xDivorced + MaritalStatus.xMarried, family = "binomial", 
                        data = train)   

summary(model_9) 
vif(model_9)

#Excluding departmentXHumanEResources
model_10<-glm(formula = Attrition ~ Age + 
               NumCompaniesWorked + TotalWorkingYears + 
               YearsWithCurrManager + AvgWorkhrs + EnvironmentSatisfaction.xHigh + 
               EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xMedium + 
               JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + 
               WorkLifeBalance.xBad + BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + 
               JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
               MaritalStatus.xDivorced + MaritalStatus.xMarried, family = "binomial", 
             data = train)   

summary(model_10) 
vif(model_10)

#excluding worklifebalanceXBad
model_11<-glm(formula = Attrition ~ Age + 
                NumCompaniesWorked + TotalWorkingYears + 
                YearsWithCurrManager + AvgWorkhrs + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xMedium + 
                JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + 
                 BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xDivorced + MaritalStatus.xMarried, family = "binomial", 
              data = train) 
  
  
  summary(model_11)
  
  #Excluding BusinessTRavelxNontravel
  
  
  model_12<-glm(formula = Attrition ~ Age + 
                  NumCompaniesWorked + TotalWorkingYears + 
                  YearsWithCurrManager + AvgWorkhrs + EnvironmentSatisfaction.xHigh + 
                  EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xMedium + 
                  JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + 
                 BusinessTravel.xTravel_Frequently + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                  MaritalStatus.xDivorced + MaritalStatus.xMarried, family = "binomial", 
                data = train) 
  
  summary(model_12)
  
  #exlcuding EnvironmentSatisfactionxHigh
  model_13<-glm(formula = Attrition ~ Age + 
                  NumCompaniesWorked + TotalWorkingYears + 
                  YearsWithCurrManager + AvgWorkhrs +  
                  EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xMedium + 
                  JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + 
                  BusinessTravel.xTravel_Frequently + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                  MaritalStatus.xDivorced + MaritalStatus.xMarried, family = "binomial", 
                data = train) 
  
  summary(model_13)
  
  
  #exlcuding EnvironmentSatisfactionxMedium
  model_14<-glm(formula = Attrition ~ Age + 
                  NumCompaniesWorked + TotalWorkingYears + 
                  YearsWithCurrManager + AvgWorkhrs +  
                  EnvironmentSatisfaction.xLow + 
                  JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + 
                  BusinessTravel.xTravel_Frequently + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                  MaritalStatus.xDivorced + MaritalStatus.xMarried, family = "binomial", 
                data = train) 
  
  summary(model_14)
  vif(model_14)
  
  
  #exlcuding JobRoleXsalesExecutive
  model_15<-glm(formula = Attrition ~ Age + 
                  NumCompaniesWorked + TotalWorkingYears + 
                  YearsWithCurrManager + AvgWorkhrs +  
                  EnvironmentSatisfaction.xLow + 
                  JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + 
                  BusinessTravel.xTravel_Frequently + 
                  JobRole.xResearch.Scientist + 
                  MaritalStatus.xDivorced + MaritalStatus.xMarried, family = "binomial", 
                data = train) 
  
  summary(model_15)
  vif(model_15)
  
  #exlcuding JobRoleXresearchscientist
  model_16<-glm(formula = Attrition ~ Age + 
                  NumCompaniesWorked + TotalWorkingYears + 
                  YearsWithCurrManager + AvgWorkhrs +  
                  EnvironmentSatisfaction.xLow + 
                  JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + 
                  BusinessTravel.xTravel_Frequently + 
                  MaritalStatus.xDivorced + MaritalStatus.xMarried, family = "binomial", 
                data = train) 
  
  summary(model_16)
  vif(model_16)
  
  #exlcuding Age
  model_17<-glm(formula = Attrition ~ 
                  NumCompaniesWorked + TotalWorkingYears + 
                  YearsWithCurrManager + AvgWorkhrs +  
                  EnvironmentSatisfaction.xLow + 
                  JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + 
                  BusinessTravel.xTravel_Frequently + 
                  MaritalStatus.xDivorced + MaritalStatus.xMarried, family = "binomial", 
                data = train) 
  
  summary(model_17)
  vif(model_17)

# 11 significant variables in the model

final_model<- model_17


# Model Evaluation

#predicted probabilities of Attrition for test data

test_pred = predict(final_model, type = "response", 
                    newdata = test[,-2])
# -2 for excluding the target variable, Attrition

# Checking summary

summary(test_pred)

test$prob <- test_pred
View(test)

# Let's use the probability cutoff of 50%.

test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))


table(test_pred_attrition,test_actual_attrition)
confusionMatrix(test_pred_attrition,test_actual_attrition, positive = "Yes")

#######################################################################
test_pred_attrition <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))



test_conf <- confusionMatrix(test_pred_attrition,test_actual_attrition, positive = "Yes")
test_conf
#######################################################################

#########################################################################################
# Finding optimal probability cuttoff 

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
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]


# Let's choose a cutoff value of 0.1695 for final model

test_cutoff_attrition<- factor(ifelse(test_pred >=0.1695, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

View(test)

### KS -statistic - Test Data ######

test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)



#on testing  data
pred_object_test<- prediction(test_cutoff_attrition, test_actual_attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)



# Lift & Gain chart 

require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Attrition_decile = lift(test_actual_attrition, test_pred, groups = 10)

plot_grid(ggplot(Attrition_decile,aes(x=Attrition_decile$bucket,y=Attrition_decile$Gain, color=""))+geom_line()+geom_point(),
          ggplot(Attrition_decile,aes(x=Attrition_decile$bucket,y=Attrition_decile$Cumlift))+geom_line()+geom_point(), 
          align = "h",ncol = 1)
