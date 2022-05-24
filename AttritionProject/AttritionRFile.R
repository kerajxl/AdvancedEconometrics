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

# Installing necessary libraries ----
if (!require(htmltools)) install.packages('htmltools')
if (!require(LogisticDx)) install.packages('LogisticDx')
if (!require(logistf)) install.packages('logistf')
if (!require(devtools)) install.packages('devtools')
if (!require(mfx)) install.packages('mfx')
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(regclass)) install.packages('regclass')
if (!require(corrplot)) install.packages('corrplot')

# Loading necessary libraries ----
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


# Loading external functions ----
source("marginaleffects.R")
source("linktest.R")

# Loading the data about Employee Attrition & Performance ----
attrition <- read_delim("HR_Employee_Attrition.csv")

# Necessary data transformations ----
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
attrition$AgexMarried <- attrition$Age * attrition$Married

attrition %>% 
  head()

# attrition$BusinessTravel <- factor(attrition$BusinessTravel,
#                            # levels from lowest to highest
#                            levels = c("Non-Travel",
#                                       "Travel_Rarely",
#                                       "Travel_Frequently"),
#                            ordered = TRUE) # ordinal

# Check if in the dataset exist some missing values ----

attrition %>% 
  is.na() %>% 
  colSums() %>% 
  sort()


# Data analysis ----

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
  select(Attrition, Age) %>%
  filter(!is.na(Age)) %>%
  group_by(Attrition) %>% 
  summarise(mean = mean(Age))

attrition$Female <- ifelse(attrition$Female == 1, 'Female', 'Male')
attrition$Attrition <- ifelse(attrition$Attrition == 1, 'Yes', 'No')

dat_text <- data.frame(label = c("Mean = 37.6", "Mean = 33.6"),
                       Attrition  = c('No', 'Yes'))


attrition %>%
  select(Attrition, Age) %>%
  filter(!is.na(Age)) %>%
  group_by(Attrition) %>%
  ggplot(aes(x = Age)) +
  geom_density(aes(fill = factor(Attrition)), alpha = 0.5, show.legend = FALSE) +
  facet_wrap( ~ Attrition) +
  theme_minimal() +
  geom_vline(
    aes(xintercept = ifelse(Attrition == 'Yes', 33.6, 37.6)),
    color = "red",
    linetype = "dashed",
    size = 1
  ) +
  labs(title = "Age Distribution by Attrition") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(
    data    = dat_text,
    mapping = aes(x = 45, y = 0.03, label = label),
    hjust   = -0.1,
    vjust   = -1
  ) +
  scale_fill_manual(values = c("#F781F3", "#819FF7")) 



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
  labs(title = "Job Satisfaction by Attrition" , y = 'Number of cases') + 
  theme_minimal() -> plot3

attrition %>% 
  select(Attrition, JobSatis) %>% 
  group_by(Attrition, JobSatis) %>% 
  summarise(number = n()) %>% 
  ggplot(aes(x = JobSatis, y = number, fill = Attrition, color = Attrition)) +
  geom_bar(stat = "identity", position = "fill", alpha = 0.5) +
  scale_y_continuous(labels = scales::percent_format()) + 
  labs(title = "Stacked Job Satisfaction by Attrition" , y = 'Percentage') + 
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

# Correlation matrix ----

correl <- cor(attrition, method = "kendall")
corrplot(correl)

sapply(attrition, 
       function(x) 
         unique(x))

summary(attrition)


# Estimation of OLS model ----
OLSmodel <- lm(Attrition ~ Age + Age2 + AgeOverTime + AgexMarried + Education + JobInvolvement +
                 JobSatisfaction + lnMonthlyIncome + NumCompaniesWorked +
                 TotalWorkingYears + WorkLifeBalance + YearsAtCompany + YearsSinceLastPromotion +
                 Over7KM + Married + Female, data = attrition)
summary(OLSmodel)

# White's estimator of the variance-covariane matrix ----
robust_vcov = vcovHC(OLSmodel, data = attrition, type = "HC")
OLSWhite <- coeftest(OLSmodel, vcov.=robust_vcov)

# Estimation of probit model ----
probitModel1 <- glm(Attrition ~ Age + Age2 + AgeOverTime + AgexMarried + Education + JobInvolvement +
                      JobSatisfaction + lnMonthlyIncome + NumCompaniesWorked  +
                      TotalWorkingYears + WorkLifeBalance + YearsAtCompany + YearsSinceLastPromotion +
                      Over7KM + Married + Female, data=attrition, 
                    family=binomial(link="probit"))
summary(probitModel1)

####

logitModel1 <- glm(Attrition ~ Age + Age2 + AgeOverTime + AgexMarried + Education + JobInvolvement +
                      JobSatisfaction + lnMonthlyIncome + NumCompaniesWorked  +
                      TotalWorkingYears + WorkLifeBalance + YearsAtCompany + YearsSinceLastPromotion +
                      Over7KM + Married + Female, data=attrition, 
                    family=binomial(link="logit"))
summary(logitModel1)

stargazer(OLSWhite, probitModel1, logitModel1, type = 'text',
          align = TRUE, style = "default", df = FALSE,
          star.cutoffs = c(0.05, 0.01, 0.001))

####
AIC(probitModel1); AIC(logitModel1) 
BIC(probitModel1); BIC(logitModel1) #logit rulez

# General-to-Specific ----

# - Education
logitModel2 <- glm(Attrition ~ Age + Age2 + AgeOverTime + AgexMarried  + JobInvolvement +
                     JobSatisfaction + lnMonthlyIncome + NumCompaniesWorked  +
                     TotalWorkingYears + WorkLifeBalance + YearsAtCompany + YearsSinceLastPromotion +
                     Over7KM + Married + Female, data=attrition, 
                   family=binomial(link="logit"))

summary(logitModel2)
lmtest::lrtest(logitModel1, logitModel2) #0.43 p-value can not reject the null
# - YearsAtCompany
logitModel3 <- glm(Attrition ~ Age + Age2 + AgeOverTime + AgexMarried + JobInvolvement +
                     JobSatisfaction + lnMonthlyIncome + NumCompaniesWorked  +
                     TotalWorkingYears + WorkLifeBalance  + YearsSinceLastPromotion +
                     Over7KM + Married + Female, data=attrition, 
                   family=binomial(link="logit"))

summary(logitModel3)
lmtest::lrtest(logitModel1, logitModel3) #0.47 p-value can not reject the null 

# - AgexMarried
logitModel4 <- glm(Attrition ~ Age + Age2 + AgeOverTime +  JobInvolvement +
                     JobSatisfaction + lnMonthlyIncome + NumCompaniesWorked  +
                     TotalWorkingYears + WorkLifeBalance  + YearsSinceLastPromotion +
                     Over7KM + Married + Female, data=attrition, 
                   family=binomial(link="logit"))

summary(logitModel4)
lmtest::lrtest(logitModel1, logitModel4) #0.4094 p-value can not reject the null 

# - Female
logitModel5 <- glm(Attrition ~ Age + Age2 + AgeOverTime +  JobInvolvement + 
                     JobSatisfaction + lnMonthlyIncome + NumCompaniesWorked +
                     TotalWorkingYears + WorkLifeBalance  + YearsSinceLastPromotion +
                     Over7KM + Married , data=attrition, 
                   family=binomial(link="logit"))

summary(logitModel5)
lmtest::lrtest(logitModel1, logitModel5) #0.21 p-value can not reject the null 



# Diagnostic tests ----

linktest_result = linktest(logitModel5)
gofresults = gof(logitModel5)
gofresults$gof

### - interacton
logitModel6 <- glm(Attrition ~ Age + Age2 +   JobInvolvement + 
                     JobSatisfaction + lnMonthlyIncome + NumCompaniesWorked +
                     TotalWorkingYears + WorkLifeBalance  + YearsSinceLastPromotion +
                     Over7KM + Married , data=attrition, 
                   family=binomial(link="logit"))

summary(logitModel6)
lmtest::lrtest(logitModel1, logitModel6) #0.21 p-value can not reject the null 

# Diagnostic tests ----

linktest_result = linktest(logitModel6)
gofresults = gof(logitModel6)
gofresults$gof

BaylorEdPsych::PseudoR2(logitModel6)

# Marginal effects ----
options(scipen = 99)
meff = probitmfx(formula=Attrition ~ Age + Age2 +   JobInvolvement + 
                   JobSatisfaction + lnMonthlyIncome + NumCompaniesWorked +
                   TotalWorkingYears + WorkLifeBalance  + YearsSinceLastPromotion +
                   Over7KM + Married , data=attrition, atmean=TRUE)
meff
mean(attrition$Age)

(meff$mfxest[1]+2*meff$mfxest[2]*mean(attrition$Age))*100


stargazer(logitModel1,logitModel2,logitModel3,logitModel4,logitModel5,logitModel6, type = 'text',
          align = TRUE, style = "default", df = FALSE,
          star.cutoffs = c(0.05, 0.01, 0.001))
