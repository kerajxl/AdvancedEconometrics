###########################################################################
#		Advanced Econometrics Project                                         #
#   Spring semester                                                       #
#   Robert Kowalczyk & Jarosław Leski                                     #
#   University of Warsaw, Faculty of Economic Sciences                    #
#                                                                         #
#                Attrition & Performance of Employees                     #
#                Binary Choice Models (Logit & Probit)                    #
#                                                                         #
###########################################################################

Sys.setenv(LANG = "en")
options(scipen = 5)

# Installing necessary libraries
if (!require(htmltools)) install.packages('htmltools')
if (!require(LogisticDx)) install.packages('LogisticDx')
if (!require(logistf)) install.packages('logistf')
if (!require(devtools)) install.packages('devtools')
if (!require(mfx)) install.packages('mfx')
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(regclass)) install.packages('regclass')
if (!require(corrplot)) install.packages('corrplot')

# Loading necessary libraries
library("regclass")
library("sandwich")
library("lmtest")
library("MASS")
library("mfx")
library("htmltools")
library("LogisticDx")
library("aod")
library("logistf") #Firth's bias reduction method
library("stargazer")
library("BaylorEdPsych")
library("corrplot")
library(gridExtra)


# Loading external functions
source("marginaleffects.R")
source("linktest.R")

# Loading the data about Employee Attrition & Performance
attrition <- read_delim("HR_Employee_Attrition.csv")

attrition$Over7KM <- ifelse(attrition$DistanceFromHome > median(attrition$DistanceFromHome), 1, 0)

# attrition$Single <- ifelse(attrition$MaritalStatus == 'Single', 1, 0)
attrition$Age2 <- attrition$Age^2
attrition$Married <- ifelse(attrition$MaritalStatus == 'Married', 1, 0)
attrition$Divorced <- ifelse(attrition$MaritalStatus == 'Divorced', 1, 0)
attrition$Female <- ifelse(attrition$Gender == 'Female', 1, 0)
attrition$Attrition <- ifelse(attrition$Attrition == 'Yes', 1, 0)
attrition$OverTime <- ifelse(attrition$OverTime == 'Yes', 1, 0)
# attrition$Travel_Rarely <- ifelse(attrition$BusinessTravel == 'Travel_Rarely', 1, 0)
# attrition$Travel_Frequently <- ifelse(attrition$BusinessTravel == 'Travel_Frequently', 1, 0)
attrition$lnMonthlyIncome <- log(attrition$MonthlyIncome)
attrition$AgeOverTime <- attrition$Age * attrition$OverTime

attrition %>% 
  head()

# attrition$BusinessTravel <- factor(attrition$BusinessTravel,
#                            # levels from lowest to highest
#                            levels = c("Non-Travel",
#                                       "Travel_Rarely",
#                                       "Travel_Frequently"),
#                            ordered = TRUE) # ordinal

attrition %>% 
  is.na() %>% 
  colSums() %>% 
  sort()


# Data analysis


attrition %>% 
  group_by(Attrition) %>% 
  summarise(Count = n()) %>%
  ggplot(aes(x = Attrition, y = Count)) +
  geom_bar(stat = "identity", fill = "deepskyblue", color = "grey40", alpha = 0.5) +
  theme_bw() +
  coord_flip() +
  geom_text(
    aes(
      x = Attrition,
      y = 0.01,
      label = paste0(Count, ifelse(Attrition == 1, ' (Yes)', ' (No)'))
    ),
    hjust = -0.8,
    vjust = -1,
    size = 3,
    colour = "black",
    fontface = "bold",
    angle = 360
  ) +
  labs(title = "Employee Attrition (Number of cases)", x = "Employee Attrition", y =
         "Number of cases") +
  theme(plot.title = element_text(hjust = 0.5))

options(repr.plot.width=8, repr.plot.height=6) 

attrition %>%
  select(Female, Age) %>%
  filter(!is.na(Age)) %>%
  group_by(Female) %>% 
  summarise(mean = mean(Age))

attrition$Female <- ifelse(attrition$Female == 1, 'Female', 'Male')

dat_text <- data.frame(label = c("Mean = 37.3", "Mean = 36.7"),
                       Female   = c('Female', 'Male'))


attrition %>%
  select(Female, Age) %>%
  filter(!is.na(Age)) %>%
  group_by(Female) %>%
  ggplot(aes(x = Age)) +
  geom_density(aes(fill = factor(Female)), alpha = 0.5, show.legend = FALSE) +
  facet_wrap( ~ Female) +
  theme_minimal() +
  geom_vline(
    aes(xintercept = mean(Age)),
    color = "red",
    linetype = "dashed",
    size = 1
  ) +
  labs(title = "Age Distribution by Gender") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(
    data    = dat_text,
    mapping = aes(x = 45, y = 0.03, label = label),
    hjust   = -0.1,
    vjust   = -1
  ) +
  scale_fill_manual(values = c("#F781F3", "#819FF7")) 

attrition$Attrition <- ifelse(attrition$Attrition == 1, 'Yes', 'No')

attrition %>% 
  ggplot(aes(x = MonthlyIncome, fill = Attrition, color = Attrition)) +
  geom_histogram(position = 'identity', alpha = 0.5) + 
  labs(title = "Monthly Income Distribiution by Attrition", y = 'Number of cases') + 
  theme_minimal() 

attrition %>% 
  ggplot(aes(x = lnMonthlyIncome, fill = Attrition, color = Attrition)) +
  geom_histogram(position = 'identity', alpha = 0.5) + 
  labs(title = "Natural Logarithm of Monthly Income Distribiution by Attrition" , y = 'Number of cases') + 
  theme_minimal()



attrition %>% 
  select(Attrition, YearsSinceLastPromotion) %>% 
  group_by(Attrition, YearsSinceLastPromotion) %>% 
  summarise(number = n()) %>% 
  ggplot(aes(x = YearsSinceLastPromotion, y = number, fill = Attrition, color = Attrition)) +
  geom_bar(stat = "identity", position = "fill", alpha = 0.5) +
  scale_y_continuous(labels = scales::percent_format()) + 
  labs(title = "Stacked Years since last promotion Distribution by Attrition" , y = 'Percentage') + 
  theme_minimal() -> plot1

attrition %>% 
  select(Attrition, YearsSinceLastPromotion) %>% 
  group_by(Attrition, YearsSinceLastPromotion) %>% 
  ggplot(aes(x = YearsSinceLastPromotion, fill = Attrition, color = Attrition)) +
  geom_bar(alpha = 0.5) + 
  labs(title = "Years since last promotion Distribution by Attrition" , y = 'Number of cases') + 
  theme_minimal() -> plot2

grid.arrange(plot2, plot1)

attrition$JobSatis <- 0
attrition$JobSatis[attrition$JobSatisfaction == 1] <- 'Low' 
attrition$JobSatis[attrition$JobSatisfaction == 2] <- 'Medium' 
attrition$JobSatis[attrition$JobSatisfaction == 3] <- 'High' 
attrition$JobSatis[attrition$JobSatisfaction == 4] <- 'Very High' 

attrition$JobSatis <- factor(attrition$JobSatis, levels = c('Low', 'Medium', 'High', 'Very High'))

attrition %>% 
  select(Attrition, JobSatis) %>% 
  group_by(Attrition, JobSatis) %>% 
  ggplot(aes(x = JobSatis, fill = Attrition, color = Attrition)) +
  geom_bar(alpha = 0.5) + 
  labs(title = "Job satisfaction by Attrition" , y = 'Number of cases') + 
  theme_minimal() -> plot3

attrition %>% 
  select(Attrition, JobSatis) %>% 
  group_by(Attrition, JobSatis) %>% 
  summarise(number = n()) %>% 
  ggplot(aes(x = JobSatis, y = number, fill = Attrition, color = Attrition)) +
  geom_bar(stat = "identity", position = "fill", alpha = 0.5) +
  scale_y_continuous(labels = scales::percent_format()) + 
  labs(title = "Stacked Job satisfaction by Attrition" , y = 'Percentage') + 
  theme_minimal() -> plot4

grid.arrange(plot3, plot4)

attrition$Attrition <- ifelse(attrition$Attrition == 'Yes', 1, 0)
attrition$Female <- ifelse(attrition$Female == 'Female', 1, 0)


attrition <- attrition %>% 
  select(-EmployeeCount, -Over18, -StandardHours, -EmployeeNumber, -DailyRate, -Department, -BusinessTravel,
         -DistanceFromHome, -EducationField, -HourlyRate, -JobLevel, -JobRole, -EnvironmentSatisfaction, 
         -MaritalStatus, -MonthlyRate, -PercentSalaryHike, -PerformanceRating, -RelationshipSatisfaction,
         -StockOptionLevel, -YearsInCurrentRole, -YearsWithCurrManager, -Gender, -MonthlyIncome,
         -JobInvolvement, -NumCompaniesWorked, -TrainingTimesLastYear, -TotalWorkingYears, -JobSatis) # Same value for each row or some unnecesarry columns

correl <- cor(attrition, method = "kendall")
corrplot(correl)

sapply(attrition, 
       function(x) 
         unique(x))

summary(attrition)


# Estimation of OLS model
OLSmodel <- lm(Attrition ~ ., data = attrition)
summary(OLSmodel)

# White's estimator of the variance-covariane matrix
robust_vcov = vcovHC(OLSmodel, data = attrition, type = "HC")
coeftest(OLSmodel, vcov.=robust_vcov)

# Estimation of probit model
probitModel1 <- glm(Attrition ~ ., data=attrition, 
                    family=binomial(link="probit"))
summary(probitModel1)
# YearsAtCompany is not significant pvalue = 0.752172
attrition1 <- attrition %>% select(-Education)

probitModel2 <- glm(Attrition ~ ., data=attrition1, 
                    family=binomial(link="probit"))
summary(probitModel2)
lmtest::lrtest(probitModel1, probitModel2) # pvalue 0.7425 not rejecting H0: beta_YearsAtCompany = 0

# Education is not significant pvalue = 0.460503
attrition2 <- attrition1
probitModel3 <- glm(Attrition ~ ., data=attrition2, 
                    family=binomial(link="probit"))
summary(probitModel3)

lmtest::lrtest(probitModel2, probitModel3) # pvalue 0.4616 not rejecting H0: beta_Education = 0

###########
attrition3 <- attrition2 %>% select(-YearsAtCompany)
probitModel4 <- glm(Attrition ~ ., data=attrition3, 
                    family=binomial(link="probit"))
summary(probitModel4)

############
lmtest::lrtest(probitModel3, probitModel4)  # pvalue 0.06984 not rejecting H0: beta_TotalWorkingYears = 0

attrition4 <- attrition3 %>% select(Attrition, Age,  Age2, JobSatisfaction, WorkLifeBalance, Over7KM, YearsSinceLastPromotion, Married, lnMonthlyIncome)
probitModel5 <- glm(Attrition ~ ., data=attrition4, 
                    family=binomial(link="probit"))
summary(probitModel5)
linktest_result = linktest(probitModel5)
lrtest(probitModel4, probitModel5) # pvalue 0.04832 rejecting H0: beta_TrainingTimesLastYear = 0

# Marginal effects
meff = probitmfx(formula=Attrition ~ ., data=attrition4, atmean=TRUE)
meff

user.def.obs = c(1,30,2,2,3,0,3,3,1,900,1,0,0,7.7,0) #convention: (intercept, x1, x2, ...)
marginaleffects(probitModel5, user.def.obs)

# R-squared statistics
PseudoR2(probitModel5)

# Linktest

summary(linktest_result)

probitModel5 <- glm(Attrition ~ Age +  JobSatisfaction  + WorkLifeBalance
                    + YearsSinceLastPromotion + Over7KM + Age2 + Married +lnMonthlyIncome , data=attrition4, 
                    family=binomial(link="probit"))
summary(probitModel5)
gofresults = gof(probitModel5)
gofresults$gof


logitModel <- glm(Attrition ~ Age +  JobSatisfaction  + WorkLifeBalance
                    + YearsSinceLastPromotion + Over7KM + Age2 + Married +lnMonthlyIncome + Over7KM + 
                    NumCompaniesWorked + JobInvolvement, data=attrition, 
                    family=binomial(link="logit"))
summary(logitModel)
linktest_result = linktest(logitModel)
gofresults = gof(logitModel)
gofresults$gof

BaylorEdPsych::PseudoR2(probitModel5)
BaylorEdPsych::PseudoR2(logitModel)

hist(attrition$MonthlyIncome)

