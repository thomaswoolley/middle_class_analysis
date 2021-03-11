rm(list=ls())
setwd("~/Desktop/Thesis")
library(RCurl)
library(margins)
library(openxlsx)
library(haven)
library(dplyr)
library(lmtest)
library(zoo)
library(sandwich)
library(robust)
library(mfx)
library(foreign)
options(scipen=999)
data <- read_sav("Aug12 Middle Class_cleaned.sav")

###Remove nonrespondents
data <- subset(data, q5!=9)
data <- subset(data, income!=10)

df <- data.frame(data$q5, data$income, data$kidsa, data$ownrent, data$sex, data$qe2,
                   data$q20a, data$q20b, data$q20e, data$q20f, data$minors, data$race3m1, data$kidsa,
                   data$qe1, data$q6, data$health, data$educ2, data$age, data$marital, data$recadults)

df$q5 <- df$data.q5
df$data.q5 <- NULL

df$income <- df$data.income
df$data.income <- NULL

df$kidsa <- df$data.kidsa
df$data.kidsa <- NULL

df$ownrent <- df$data.ownrent
df$data.ownrent <- NULL

df$sex <- df$data.ownrent
df$data.ownrent <- NULL

df$qe2 <- df$data.qe2
df$data.qe2 <- NULL

df$q20a <- df$data.q20a
df$data.q20a <- NULL

df$q20b <- df$data.q20b
df$data.q20b <- NULL

df$q20e <- df$data.q20e
df$data.q20e <- NULL

df$q20f <- df$data.q20f
df$data.q20f <- NULL

df$minors <- df$data.minors
df$data.minors <- NULL

df$race3m1 <- df$data.race3m1
df$data.race3m1 <- NULL

df$qe1 <- df$data.qe1
df$data.qe1 <- NULL

df$q6 <- df$data.q6
df$data.q6 <- NULL

df$health <- df$data.health
df$data.health <- NULL

df$educ2 <- df$data.educ2
df$data.educ2 <- NULL

df$age <- df$data.age
df$data.age <- NULL

df$marital <- df$data.marital
df$data.marital <- NULL

df$sex <- df$data.sex
df$data.sex <- NULL

df$adults <- df$data.recadults
df$data.recadults <- NULL

df$kids <- df$data.kidsa
df$data.kidsa <- NULL

#Now Create Dependent Middle Class Indicator Variables

df$upper <- ifelse(df$q5==1, 1, 0)

df$upper_mid <- ifelse(df$q5==2, 1,0)

df$middle <- ifelse(df$q5==3, 1,0)

df$lower_mid <- ifelse(df$q5==4, 1,0)

df$lower <- ifelse(df$q5==5, 1,0)

#Dummy Income Variables
df$tenk <- ifelse(df$income==2, 1, 0)
df$twentyk <- ifelse(df$income==3, 1, 0)
df$thirtyk <- ifelse(df$income==4, 1, 0)
df$fourtyk <- ifelse(df$income==5, 1, 0)
df$under75 <- ifelse(df$income==6, 1, 0)
df$under100 <- ifelse(df$income==7, 1, 0)
df$under150 <- ifelse(df$income==8, 1, 0)
df$over150 <- ifelse(df$income==9, 1, 0)

#Dummy Homeowner Variables


#Create Independent Explanatory Variables
#homeowner: If the person owns a home
df$homeowner <- ifelse(df$ownrent==1, 1, 0)

#retired: If the person is retired
df$retired <- ifelse(df$qe1==1, 1, 0)

#married: if the person is married
df$married <- ifelse(df$marital==1, 1, 0)

#Independent Variables for Financial Instability
#medtrub: if the person had trouble paying for medical care for yourelf/family
df$medtrub <- ifelse(df$q20a==1, 1, 0)

#renttrub: if the person had trouble paying for rent/mortgage
df$renttrub <- ifelse(df$q20b==1, 1, 0)

#billtrub: if the person had trouble paying their bills
df$billtrub <- ifelse(df$q20e==1, 1, 0)

#spendingcuts: if the person had to cut houshold spending
df$spendingcuts <- ifelse(df$q20f==1, 1, 0)
  
#Create Race Dummy Variables
df$white <- ifelse(df$race3m1==1, 1, 0)
df$black <- ifelse(df$race3m1==2, 1, 0)
df$asian <- ifelse(df$race3m1==3, 1, 0)
df$hispanic <- ifelse(df$race3m1==7, 1, 0)

#Create Sex Variable
df$male <- ifelse(df$sex==1, 1, 0)

#Health Dummy Variables
df$healthiest <- ifelse(df$health==1 ,1 ,0)
df$healthy <- ifelse(df$health==2, 1, 0)
df$mid_health <- ifelse(df$health==3, 1, 0)

#Childhood Social Class Dummy
df$born_upper <- ifelse(df$q6==1, 1, 0)
df$born_upper_mid <- ifelse(df$q6==2, 1, 0)
df$born_middle <- ifelse(df$q6==3, 1, 0)
df$born_lower_mid <- ifelse(df$q6==4, 1, 0)

#Education Level
df$hs_grad <- ifelse(df$educ2==3, 1, 0)
df$some_college <- ifelse(df$educ2==4 | df$educ2==5, 1, 0)
df$college_grad <- ifelse(df$educ2==6 | df$educ2==7, 1, 0)
df$postgrad <- ifelse(df$educ2==8, 1, 0)

#Household Size
df$one_adult <- ifelse(df$adults==1, 1, 0)
df$two_adult <- ifelse(df$adults==2, 1, 0)

#Probit Regressions

low_mid_all <- glm(lower_mid ~ tenk + twentyk + thirtyk + fourtyk + under75 + under100 + under150 + over150 + male 
               + black + asian + hispanic + medtrub + renttrub + billtrub + spendingcuts + homeowner + retired
               + kids + healthiest + healthy + mid_health + hs_grad + some_college + college_grad + postgrad 
               + born_upper + born_upper_mid + born_middle + born_lower_mid, 
               family = binomial(link = "probit"), data = df)

middle_all <- glm(middle ~ tenk + twentyk + thirtyk + fourtyk + under75 + under100 + under150 + over150 + male 
               + black + asian + hispanic + medtrub + renttrub + billtrub + spendingcuts + homeowner + retired
               + kids + healthiest + healthy + mid_health + hs_grad + some_college + college_grad + postgrad 
               + born_upper + born_upper_mid + born_middle + born_lower_mid, 
               family = binomial(link = "probit"), data = df)

upper_mid_all <- glm(upper_mid ~ tenk + twentyk + thirtyk + fourtyk + under75 + under100 + under150 + over150 + male 
               + black + asian + hispanic + medtrub + renttrub + billtrub + spendingcuts + homeowner + retired
               + kids + healthiest + healthy + mid_health + hs_grad + some_college + college_grad + postgrad 
               + born_upper + born_upper_mid + born_middle + born_lower_mid, 
               family = binomial(link = "probit"), data = df)

low_mid_1adult <- glm(lower_mid ~ tenk + twentyk + thirtyk + fourtyk + under75 + under100 + under150 + over150 + male 
                  + black + asian + hispanic + medtrub + renttrub + billtrub + spendingcuts + homeowner + retired
                  + kids + healthiest + healthy + mid_health + hs_grad + some_college + college_grad + postgrad 
                  + born_upper + born_upper_mid + born_middle + born_lower_mid, 
                  family = binomial(link = "probit"), data = df)

middle_1adult <- glm(middle ~ tenk + twentyk + thirtyk + fourtyk + under75 + under100 + under150 + over150 + male 
                  + black + asian + hispanic + medtrub + renttrub + billtrub + spendingcuts + homeowner + retired
                  + kids + healthiest + healthy + mid_health + hs_grad + some_college + college_grad + postgrad 
                  + born_upper + born_upper_mid + born_middle + born_lower_mid, 
                  family = binomial(link = "probit"), data = df)

upper_mid_1adult <- glm(upper_mid ~ tenk + twentyk + thirtyk + fourtyk + under75 + under100 + under150 + over150 + male 
                  + black + asian + hispanic + medtrub + renttrub + billtrub + spendingcuts + homeowner + retired
                  + kids + healthiest + healthy + mid_health + hs_grad + some_college + college_grad + postgrad 
                  + born_upper + born_upper_mid + born_middle + born_lower_mid, 
                  family = binomial(link = "probit"), data = df)

lower_mid_2adult <- glm(lower_mid ~ tenk + twentyk + thirtyk + fourtyk + under75 + under100 + under150 + over150 + male 
                  + black + asian + hispanic + medtrub + renttrub + billtrub + spendingcuts + homeowner + retired
                  + kids + healthiest + healthy + mid_health + hs_grad + some_college + college_grad + postgrad 
                  + born_upper + born_upper_mid + born_middle + born_lower_mid, 
                  family = binomial(link = "probit"), data = df)

mddle_2adult <- glm(middle ~ tenk + twentyk + thirtyk + fourtyk + under75 + under100 + under150 + over150 + male 
                  + black + asian + hispanic + medtrub + renttrub + billtrub + spendingcuts + homeowner + retired
                  + kids + healthiest + healthy + mid_health + hs_grad + some_college + college_grad + postgrad 
                  + born_upper + born_upper_mid + born_middle + born_lower_mid, 
                  family = binomial(link = "probit"), data = df)

upper_mid_2adult <- glm(upper_mid ~ tenk + twentyk + thirtyk + fourtyk + under75 + under100 + under150 + over150 + male 
                  + black + asian + hispanic + medtrub + renttrub + billtrub + spendingcuts + homeowner + retired
                  + kids + healthiest + healthy + mid_health + hs_grad + some_college + college_grad + postgrad 
                  + born_upper + born_upper_mid + born_middle + born_lower_mid, 
                  family = binomial(link = "probit"), data = df)

coeftest(probit1, vcov = vcovHC(probit1, type = "HC1"))

bptest(probit1)
test1 <- probitmfx(lower_mid ~ tenk + twentyk + thirtyk + fourtyk + under75 + under100 + under150 + over150 + male 
                   + black + asian + hispanic + medtrub + renttrub + billtrub + spendingcuts + homeowner + retired
                   + kids + healthiest + healthy + mid_health + hs_grad + some_college + college_grad + postgrad 
                   + born_upper + born_upper_mid + born_middle + born_lower_mid, df, atmean=FALSE, robust=TRUE)
summaryR(probit1)
summary(margins(probit1), robust=TRUE)
summary(margins(probit1))
summary(test1)
summary(margins(probit1))
summary(probit2)
summary(margins(probit2))