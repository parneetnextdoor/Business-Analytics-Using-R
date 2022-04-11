################################################################################################################################################################################

#PART 1: INTERPRETING INFORMATION THROUGH DATA EXPLORATION
#1 Is the Data Analyzable and Usable?:
str(chimera_2022)                # Illustrates the structure of the Data Frame (Column, names, rows) 
colnames(chimera_2022)           # Illustrates the names of columns of the Data Frame
print("Although there is not missing data, and the data is fairly structured and clean, when using the str() function, it illustrates that all variables are 'numeric', even though variables such as 'exit', 'gender', 'local', etc should be integers")
 
################################################################################################################################################################################

#2 Analysis of Employee Profiles:
#i.
table(chimera_2022$gender)       # Females = 6732, Males = 11400
chimera_males <- chimera_2022[chimera_2022$gender == 1,]
chimera_females <- chimera_2022[chimera_2022$gender == 0,]
chimera_gendertotal <- as.numeric(length(chimera_males$gender) + length(chimera_females$gender))
Male_Ratio <- length(chimera_males$gender) / chimera_gendertotal
print(Male_Ratio)*100
Female_Ratio <- length(chimera_females$gender) / chimera_gendertotal
print(Female_Ratio)*100
#The portion of males is 62.87227% and the portion of females is 37.12773%
6732/11400                       # Female:Male Gender Ratio = 0.5905263
11400/6732                       # Male:Female Gender Ratio = 1.693405

#ii.
range(chimera_2022$age)          # Min = 19, Max = 56 (Chimera's age range of employees is 19 to 56)

#iii.  
sequence <- seq(0,100, by=10)
partial_distribution <- cut(chimera_2022$age, sequence)
Answer <- transform(table(partial_distribution), Percentages = prop.table(Freq)*100,2)
print(Answer)
hist(chimera_2022$age)

#iv.
table(chimera_2022$local)        # Foreign Nationals = 7244, Local = 10888
chimera_foreignnationals <- chimera_2022[chimera_2022$local == 0,]
chimera_nonforeignnationals <- chimera_2022[chimera_2022$local == 1,]
chimera_localtotal <- as.numeric(length(chimera_foreignnationals$local) + length(chimera_nonforeignnationals$local))
foreignnationalspercentage <- length(chimera_foreignnationals$local) / chimera_localtotal
print(foreignnationalspercentage)*100
#39.95147% of employees were Foreign Nationals
7244/(7244+10888)                

#v.
hist(chimera_2022$city_size)     # Employees are located in 5 cities
table(chimera_2022$city_size)    # Employees are located in 5 cities

#vi.
table(chimera_2022$part_time)    # Full-time = 14372, Part-time = 3760
chimera_parttime <- chimera_2022[chimera_2022$part_time == 1,]
chimera_fulltime <- chimera_2022[chimera_2022$part_time == 0,]
chimera_parttimefulltime <- as.numeric(length(chimera_parttime$part_time) + length(chimera_fulltime$part_time))
parttime_percentage <- length(chimera_parttime$part_time) / chimera_parttimefulltime
print(parttime_percentage)*100
#20.73682% of employees were Part Time
3760/(14372+3760)

#vii.
hist(chimera_2022$education)     # Most employees graduated from highest-tier education
table(chimera_2022$education)    # Highest-tier = 13058, Medium-tier = 3255, Lowest-tier = 1819
(1819+3255)/(1819+3255+13058)    # 27.98368% of employees went to non- top-tier schools
 
################################################################################################################################################################################

#3 Analysis of Employee Climate Survey:
#a.
summary(chimera_2022$job_satisfaction)     # Illustrates a basic summary stats table, showing Min, Q1, Med, Mean, Q3, Max
sd(chimera_2022$job_satisfaction)          # Standard Deviation = 0.2
hist(chimera_2022$job_satisfaction)        # Employee Job Satisfaction is Normally Distributed

#b.
summary(chimera_2022$kpi_performance)      # Illustrates a basic summary stats table, showing Min, Q1, Med, Mean, Q3, Max
sd(chimera_2022$kpi_performance)           # Standard Deviation = 0.1
hist(chimera_2022$kpi_performance)         # KPI Performance is Normally Distributed

#c.
summary(chimera_2022$boss_survey)          # Illustrates a basic summary stats table, showing Min, Q1, Med, Mean, Q3, Max
sd(chimera_2022$boss_survey)               # Standard Deviation = 0.2
hist(chimera_2022$boss_survey)             # Boss Survey is Normally Distributed

#d. 
print("One of the measures I have chosen is 'half_day_leaves' because it can illustrate is the employee culture is strict or more laid back")
summary(chimera_2022$half_day_leaves)      # The mean of employees asking to leave for half a day is 4.599 times in the year
hist(chimera_2022$half_day_leaves)         

print("One of the measures I have chosen is 'job_satisfaction' because it can illustrate how happy the employees are with their jobs")
summary(chimera_2022$job_satisfaction)      # The mean of job satisfaction is 0.60 on the company-specific scale
hist(chimera_2022$job_satisfaction)         

print("One of the measures I have chosen is 'training' because it how much effort the company puts into training their employees")
summary(chimera_2022$training)      # The mean of employees employee training given is 2.601 weeks per year
hist(chimera_2022$training)         

################################################################################################################################################################################

#4 Describe the Summary Descriptive Statistics
#a.
#install.packages("stargazer")
#library(stargazer)

DT1 <- chimera_2022 
stargazer(DT1, type = "text")

#b. 

DT_Exits <- subset(chimera_2022, exit==1)
cat("Summary Statistics for Employees Who Exited Chimera Corporation")
stargazer(DT_Exits, type = "text")

#c.
DT_Remains <- subset(chimera_2022, exit==0)
cat("Summary Statistics for Employees Who Remained at Chimera Corporation")
stargazer(DT_Remains, type = "text")

##########*Line 118 to 257 is to show this work can be done manually, but takes much longer###################

chimera_2022$age[chimera_2022$exit=="1"]                       # Subset: Descriptive statistics for the variable "age" used in Q2 and Q3 for employees that left the firm by the end of the year
min(chimera_2022$age[chimera_2022$exit=="1"])
max(chimera_2022$age[chimera_2022$exit=="1"])
median(chimera_2022$age[chimera_2022$exit=="1"])
mean(chimera_2022$age[chimera_2022$exit=="1"])
sd(chimera_2022$age[chimera_2022$exit=="1"])

chimera_2022$gender[chimera_2022$exit=="1"]                    # Subset: Descriptive statistics for the variable "gender" used in Q2 and Q3 for employees that left the firm by the end of the year
min(chimera_2022$gender[chimera_2022$exit=="1"])
max(chimera_2022$gender[chimera_2022$exit=="1"])
median(chimera_2022$gender[chimera_2022$exit=="1"])
mean(chimera_2022$gender[chimera_2022$exit=="1"])
sd(chimera_2022$gender[chimera_2022$exit=="1"])

chimera_2022$local[chimera_2022$exit=="1"]                     # Subset: Descriptive statistics for the variable "local" used in Q2 and Q3 for employees that left the firm by the end of the year
min(chimera_2022$local[chimera_2022$exit=="1"])
max(chimera_2022$local[chimera_2022$exit=="1"])
median(chimera_2022$local[chimera_2022$exit=="1"])
mean(chimera_2022$local[chimera_2022$exit=="1"])
sd(chimera_2022$local[chimera_2022$exit=="1"])

chimera_2022$city_size[chimera_2022$exit=="1"]                 # Subset: Descriptive statistics for the variable "city_size" used in Q2 and Q3 for employees that left the firm by the end of the year
min(chimera_2022$city_size[chimera_2022$exit=="1"])
max(chimera_2022$city_size[chimera_2022$exit=="1"])
median(chimera_2022$city_size[chimera_2022$exit=="1"])
mean(chimera_2022$city_size[chimera_2022$exit=="1"])
sd(chimera_2022$city_size[chimera_2022$exit=="1"])

chimera_2022$part_time[chimera_2022$exit=="1"]                 # Subset: Descriptive statistics for the variable "part_time" used in Q2 and Q3 for employees that left the firm by the end of the year
min(chimera_2022$part_time[chimera_2022$exit=="1"])
max(chimera_2022$part_time[chimera_2022$exit=="1"])
median(chimera_2022$part_time[chimera_2022$exit=="1"])
mean(chimera_2022$part_time[chimera_2022$exit=="1"])
sd(chimera_2022$part_time[chimera_2022$exit=="1"])

chimera_2022$job_satisfaction[chimera_2022$exit=="1"]          # Subset: Descriptive statistics for the variable "job_satisfaction" used in Q2 and Q3 for employees that left the firm by the end of the year
min(chimera_2022$job_satisfaction[chimera_2022$exit=="1"])
max(chimera_2022$job_satisfaction[chimera_2022$exit=="1"])
median(chimera_2022$job_satisfaction[chimera_2022$exit=="1"])
mean(chimera_2022$job_satisfaction[chimera_2022$exit=="1"])
sd(chimera_2022$job_satisfaction[chimera_2022$exit=="1"])

chimera_2022$kpi_performance[chimera_2022$exit=="1"]           # Subset: Descriptive statistics for the variable "kpi_performance" used in Q2 and Q3 for employees that left the firm by the end of the year
min(chimera_2022$kpi_performance[chimera_2022$exit=="1"])
max(chimera_2022$kpi_performance[chimera_2022$exit=="1"])
median(chimera_2022$kpi_performance[chimera_2022$exit=="1"])
mean(chimera_2022$kpi_performance[chimera_2022$exit=="1"])
sd(chimera_2022$kpi_performance[chimera_2022$exit=="1"])

chimera_2022$boss_survey[chimera_2022$exit=="1"]               # Subset: Descriptive statistics for the variable "boss_survey" used in Q2 and Q3 for employees that left the firm by the end of the year
min(chimera_2022$boss_survey[chimera_2022$exit=="1"])
max(chimera_2022$boss_survey[chimera_2022$exit=="1"])
median(chimera_2022$boss_survey[chimera_2022$exit=="1"])
mean(chimera_2022$boss_survey[chimera_2022$exit=="1"])
sd(chimera_2022$boss_survey[chimera_2022$exit=="1"])

chimera_2022$half_day_leaves[chimera_2022$exit=="1"]           # Subset: Descriptive statistics for the variable "half_day_leaves" used in Q2 and Q3 for employees that left the firm by the end of the year
min(chimera_2022$half_day_leaves[chimera_2022$exit=="1"])
max(chimera_2022$half_day_leaves[chimera_2022$exit=="1"])
median(chimera_2022$half_day_leaves[chimera_2022$exit=="1"])
mean(chimera_2022$half_day_leaves[chimera_2022$exit=="1"])
sd(chimera_2022$half_day_leaves[chimera_2022$exit=="1"])

chimera_2022$training[chimera_2022$exit=="1"]                  # Subset: Descriptive statistics for the variable "training" used in Q2 and Q3 for employees that left the firm by the end of the year
min(chimera_2022$training[chimera_2022$exit=="1"])
max(chimera_2022$training[chimera_2022$exit=="1"])
median(chimera_2022$training[chimera_2022$exit=="1"])
mean(chimera_2022$training[chimera_2022$exit=="1"])
sd(chimera_2022$training[chimera_2022$exit=="1"])

#c. 
chimera_2022$age[chimera_2022$exit=="0"]                       # Subset: Descriptive statistics for the variable "age" used in Q2 and Q3 for employees that remained with the firm by the end of the year
min(chimera_2022$age[chimera_2022$exit=="0"])
max(chimera_2022$age[chimera_2022$exit=="0"])
median(chimera_2022$age[chimera_2022$exit=="0"])
mean(chimera_2022$age[chimera_2022$exit=="0"])
sd(chimera_2022$age[chimera_2022$exit=="0"])

chimera_2022$gender[chimera_2022$exit=="0"]                    # Subset: Descriptive statistics for the variable "gender" used in Q2 and Q3 for employees that remained with the firm by the end of the year
min(chimera_2022$gender[chimera_2022$exit=="0"])
max(chimera_2022$gender[chimera_2022$exit=="0"])
median(chimera_2022$gender[chimera_2022$exit=="0"])
mean(chimera_2022$gender[chimera_2022$exit=="0"])
sd(chimera_2022$gender[chimera_2022$exit=="0"])

chimera_2022$local[chimera_2022$exit=="0"]                     # Subset: Descriptive statistics for the variable "local" used in Q2 and Q3 for employees that remained with the firm by the end of the year
min(chimera_2022$local[chimera_2022$exit=="0"])
max(chimera_2022$local[chimera_2022$exit=="0"])
median(chimera_2022$local[chimera_2022$exit=="0"])
mean(chimera_2022$local[chimera_2022$exit=="0"])
sd(chimera_2022$local[chimera_2022$exit=="0"])

chimera_2022$city_size[chimera_2022$exit=="0"]                 # Subset: Descriptive statistics for the variable "city_size" used in Q2 and Q3 for employees that remained with the firm by the end of the year
min(chimera_2022$city_size[chimera_2022$exit=="0"])
max(chimera_2022$city_size[chimera_2022$exit=="0"])
median(chimera_2022$city_size[chimera_2022$exit=="0"])
mean(chimera_2022$city_size[chimera_2022$exit=="0"])
sd(chimera_2022$city_size[chimera_2022$exit=="0"])

chimera_2022$part_time[chimera_2022$exit=="0"]                 # Subset: Descriptive statistics for the variable "apart_time" used in Q2 and Q3 for employees that remained with the firm by the end of the year
min(chimera_2022$part_time[chimera_2022$exit=="0"])
max(chimera_2022$part_time[chimera_2022$exit=="0"])
median(chimera_2022$part_time[chimera_2022$exit=="0"])
mean(chimera_2022$part_time[chimera_2022$exit=="0"])
sd(chimera_2022$part_time[chimera_2022$exit=="0"])

chimera_2022$job_satisfaction[chimera_2022$exit=="0"]          # Subset: Descriptive statistics for the variable "job_satisfaction" used in Q2 and Q3 for employees that remained with the firm by the end of the year
min(chimera_2022$job_satisfaction[chimera_2022$exit=="0"])
max(chimera_2022$job_satisfaction[chimera_2022$exit=="0"])
median(chimera_2022$job_satisfaction[chimera_2022$exit=="0"])
mean(chimera_2022$job_satisfaction[chimera_2022$exit=="0"])
sd(chimera_2022$job_satisfaction[chimera_2022$exit=="0"])

chimera_2022$kpi_performance[chimera_2022$exit=="0"]           # Subset: Descriptive statistics for the variable "kpi_performance" used in Q2 and Q3 for employees that remained with the firm by the end of the year
min(chimera_2022$kpi_performance[chimera_2022$exit=="0"])
max(chimera_2022$kpi_performance[chimera_2022$exit=="0"])
median(chimera_2022$kpi_performance[chimera_2022$exit=="0"])
mean(chimera_2022$kpi_performance[chimera_2022$exit=="0"])
sd(chimera_2022$kpi_performance[chimera_2022$exit=="0"])

chimera_2022$boss_survey[chimera_2022$exit=="0"]               # Subset: Descriptive statistics for the variable "boss_survey" used in Q2 and Q3 for employees that remained with the firm by the end of the year
min(chimera_2022$boss_survey[chimera_2022$exit=="0"])
max(chimera_2022$boss_survey[chimera_2022$exit=="0"])
median(chimera_2022$boss_survey[chimera_2022$exit=="0"])
mean(chimera_2022$boss_survey[chimera_2022$exit=="0"])
sd(chimera_2022$boss_survey[chimera_2022$exit=="0"])

chimera_2022$half_day_leaves[chimera_2022$exit=="0"]           # Subset: Descriptive statistics for the variable "half_day_leaves" used in Q2 and Q3 for employees that remained with the firm by the end of the year
min(chimera_2022$half_day_leaves[chimera_2022$exit=="0"])
max(chimera_2022$half_day_leaves[chimera_2022$exit=="0"])
median(chimera_2022$half_day_leaves[chimera_2022$exit=="0"])
mean(chimera_2022$half_day_leaves[chimera_2022$exit=="0"])
sd(chimera_2022$half_day_leaves[chimera_2022$exit=="0"])

chimera_2022$training[chimera_2022$exit=="0"]                  # Subset: Descriptive statistics for the variable "training" used in Q2 and Q3 for employees that remained with the firm by the end of the year
min(chimera_2022$training[chimera_2022$exit=="0"])
max(chimera_2022$training[chimera_2022$exit=="0"])
median(chimera_2022$training[chimera_2022$exit=="0"])
mean(chimera_2022$training[chimera_2022$exit=="0"])
sd(chimera_2022$training[chimera_2022$exit=="0"])

##########*Line 118 to 257 is to show this work can be done manually, but takes much longer###################

#5.) Comparing descriptive statistics from Question 4
print("Upon doing the analysis for Q54, some patterns between the descriptive statistics are illustrated in figure 1. The variables that I found that could be contributing to Chimera Corporation’s ~14% attrition rate is “job_satisfaction”, “kpi_performance”, and “boss_survey”. For the variable “boss_survey”, the company-specific scale of unit for superior evaluations is significantly lower for the subset of employees that left the firm during the year compared to the subset of employees that remained. When comparing the descriptive statistics for “boss_survey”, it is evident that the Minimum, Maximum, Median, Mean, and the Standard Deviation is significantly lower for the employees who left compared to the ones who stayed. This suggests that employees that left the firm have given significantly lower superior evaluations compared to employees that stayed. Employees that have high job satisfaction are more likely to be more productive than employees that have lower lob satisfaction. The employees that quit may have been unhappy with the way they were treated by their superiors, which led to a decrease in “job_satisfaction” and “kpi_performance”. The Minimum, Maximum, Median, and Mean for “job_satisfaction” was significantly lower for employees that left the firm compared to employees that remained with the firm.")

################################################################################################################################################################################

#6.) What percentage of employees left the firm in the last year?
#a.
chimera_2022$core[chimera_2022$exit=="1"]                      # A subset created for all core and non-core employees that left the firm
table(chimera_2022$core[chimera_2022$exit=="1"])               # A table that illustrates the number of core employees that left with non-core employees
1997/(1997+460)*100                                            # 81.27798% of employees that left the firm worked in core business

#b.
chimera_2022$high_potential[chimera_2022$exit=="1"]            # A subset created for all high potential and non-high potential employees that left the firm
table(chimera_2022$high_potential[chimera_2022$exit=="1"])     # A table that illustrates the number of high potential that left with non-high potential employees
279/(2178+279)*100                                             # 11.35531% of employees that left the firm were identified as high potential employees

################################################################################################################################################################################

#PART 2: PRESENTING PRELIMINARY DATA ANALYSIS
#7.) 
print("Given the preliminary data exploration, I have found that the significant indicator of Chimera Corporation’s high attrition rate is due to the undesirable working environment that is created by the bosses and superiors of Chimera Corporation. This directly influences employee productivity, which decreases the KPI performance and job satisfaction that is reported by employees.")
print("The next step is to understand the interests of all stakeholders. We must come up with a solution that satisfies everyone’s needs. This means communicating with the stakeholders across Chimera Corporation and listen to each other with the intention to understand that Chimera Corporation’s high attrition rate is a problem. We want to do this to ignore our true interests, as sometimes consultants and superiors often become attached to one particular situation.")
print("After a stakeholder analysis, we can list possible solutions. Chimera Corporation can consult with HR to implement a mandatory training program for all superiors, which can re-establish managerial training objectives. A training program for all managers will standardize the managerial process and will help eliminate gaps between current and desired performance and knowledge for business value.")

################################################################################################################################################################################

#PART 3: PRESENTING REGRESSIONS
#9.)
#a.
Model.A <- lm(formula = exit ~ salary + rank + gender, data = chimera_2022)
summary(Model.A)

#b.
Model.B <- lm(formula = exit ~ boss_survey + job_satisfaction, data = chimera_2022)
summary(Model.B)

#a. 

Model.C <- lm(formula = exit ~ salary + boss_survey + job_satisfaction + rank + gender, data = chimera_2022)
summary(Model.C)

#9.1
AIC(Model.A)
AIC(Model.B)
AIC(Model.C)

stargazer(Model.A,  Model.B, Model.C, type = "text")

print("The best performing model is Model C, as it has the highest Adjusted R2 value of 0.163, compared to 0.011 and 0.160 for Model A and Model B. A larger Adjusted R2 value indicates a higher amount of variability. In this case, the Adjusted R2 value for Model 3 of 0.163 means that 16.3% of the variance can be predicted in the model, which indicates a higher accuracy in the data that is being used. Furthermore, Model C also has the lowest AIC of 9356.975, compared to 12381.28 and 9421.942 for Model A and Model B. The Akaikes Information Criterion [AIC (Akaike, 1974)] measures of the goodness of fit of an estimated statistical model and can also be used for model selection. A Lower AIC value indicates a better-fit model, as it illustrates Model C is less complex and generally a better fit for the data compared to Model A and Model B.")

