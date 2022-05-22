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

if (!require(htmltools)) install.packages('htmltools')
if (!require(LogisticDx)) install.packages('LogisticDx')
if (!require(logistf)) install.packages('logistf')
if (!require(devtools)) install.packages('devtools')
if (!require(mfx)) install.packages('mfx')
devtools::source_url("https://github.com/njtierney/naniar/blob/master/R/mcar-test.R?raw=TRUE") # Similar to BaylorEdPsych

library("sandwich")
library("lmtest")
library("MASS")
library("mfx")
library("htmltools")
library("LogisticDx")
library("aod")
library("logistf") #Firth's bias reduction method
library("stargazer")

source("marginaleffects.R")
source("linktest.R")

# iv. Test the hypothesis H0: beta1=beta2=0 using the Wald statistics
H <- rbind(c(0,1,0,0), c(0,0,1,0))

# h %*% coef(dem.probit)
wald.test.results = wald.test(b = coef(dem.probit), 
                              Sigma = vcov(dem.probit), L = H)
wald.test.results
