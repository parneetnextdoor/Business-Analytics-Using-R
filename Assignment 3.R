#Q1 

chimera_exit <- subset(chimera_2022, exit == 1) #subset of all employees that exited
chimera_stay <- subset(chimera_2022, exit == 0) #subset of all employees that stayed

odds_ratio <- nrow(chimera_exit) / nrow(chimera_stay) 
odds_ratio #0.1567464

#Q2

Industry_OddsRatio <- 9/91
Industry_OddsRatio #0.0989011

#Q3 

Difference_OddsRatio <- odds_ratio - Industry_OddsRatio
Difference_OddsRatio #0.05784531

#4

install.packages("stargazer")
require(stargazer)
stargazer(chimera_exit, type = "text")
stargazer(chimera_stay, type = "text")

#5

newdata1 <- with(chimera_exit, data.table(boss_survey = mean(boss_survey), job_satisfaction = mean(job_satisfaction), rank=mean(rank), gender=mean(gender)
                                          
Model.1.Logit:  Exit ~ chimera_exit(Boss_Survey, Job_Satisfaction, Rank, Gender)
Model.2.Logit: Exit ~  chimera_exit(Boss_Survey, Job_Satisfaction, Rank, Gender, newdata1)







