###########################################################################
#		                      Advanced Econometrics                           #                                                 
#           Project: Binary choice models. Credit risk analysis           #                            
#                           Artur Skowroński                              #
#                             Warsaw 2022                                 #
###########################################################################

# Settings
options(scipen = 99)
Sys.setenv(LANG = "en")
set.seed(411423)

# Packages
# Data Analysis
library('dplyr')
library('tibble')
library('tidyr')
library('ggplot2')
library('psych')
library('tseries')
library('corrplot')

# Modelling - logistic regression
library("sandwich")
library("lmtest")
library("MASS")
library("mfx")

# PseudoR2 calculation
library("BaylorEdPsych") # https://cran.r-project.org/src/contrib/Archive/BaylorEdPsych/
library("sandwich")
library("lmtest")
library("MASS")
library("mfx")
library("htmltools")
library("LogisticDx")
library("aod")
library("logistf")

# Models comparison
library("stargazer")

# 1. Introduction ----------

# Import data
credit_org <- read.csv('cs-training.csv')
credit <- data.frame(credit_org)

# Exploratory Data Analysis
credit <- subset(credit, select = -c(X))
glimpse(credit)
psych::describe(credit)

# Our default rate (SeriousDlqin2yrs) in two years is equal to 7%
# Age from 0-109 seems unrealistic?
# At least one person had a trouble with monthly debt payment 98 times
# Somebody has monthly income to 0 - it seems unrealistic, or maybe this person has lost a job
# Most probably the same person had also problem with paying debt in 90 days (98 times for one person)
# Somebody has 54 loans (maybe he is enterpreneur)
# As I thought, the same problem with payment occured in 60-98 days period (98 times for one person)
# Somebody has 20 dependents in family excluding themselves (quite a lot)

# In my dataset there are a lot of missing values, especially in MonthlyIncome variable
# I will fill na values by its median

# 2. Analysis of target variable----------

credit %>% 
  filter(SeriousDlqin2yrs==1) %>% 
  psych::describe(.)

mean(credit$SeriousDlqin2yrs) # 0.06684 proportion of defaults

# Average age of people who had problem with paying rent is equal to 45.93, one person was 101 years old
# These people had small balance on credit cards
# It is worth to higlight that the median of not paying in 30-59 days period is equal to 0 (quite surprising)
# There were cases were DebtRatio was equal to 0 (so it looks like, it might be potential outlier)
# Again person was in default state, while she/he didn't have any open credit lines, mean around 7 is quite high
# One person 98 times was in default before (definitely a lot of times)
# Number of dependends seems to be rational

# To sum up, there might some kind of mistaken records and outliers. I will definitely do sth with it.

# Countplot for target variable
ggplot(credit, aes(x = factor(SeriousDlqin2yrs))) +   
  geom_bar() +
  stat_count(geom = "text", colour = "white", size = 4,
             aes(label = ..count..),position=position_stack(vjust=0.5)) +
  xlab("SeriousDlqin2yrs") +
  ylab("")
  
# Age vs defaults
credit %>% 
  ggplot(aes(x=age, fill = SeriousDlqin2yrs)) +
  geom_histogram() +
  facet_wrap(~SeriousDlqin2yrs, scales = 'free_y',
              labeller = as_labeller(c("0" = "Non-defaults",
                                      "1" = "Defaults"))) +
  theme_bw() +
  guides(fill = "none") +
  xlab("Age") +
  ylab("")
 

# 3. Analysis of independent variables ----------  

# Histograms of independent variables
credit %>% 
  gather(Info, value, c(seq(2,10))) %>% 
  ggplot(aes(x=value, fill = Info)) +
  geom_histogram() +
  facet_wrap(~Info, scales = 'free', labeller = label_wrap_gen(width = 2, multi_line = TRUE))

# The same but logged
credit %>% 
  gather(Info, value, c(seq(2,10))) %>% 
  ggplot(aes(x=log1p(value), fill = Info)) +
  geom_histogram() +
  facet_wrap(~Info, scales = 'free')

# Luckily, the distribution of people who have credit is mostly dependent from people below 60
# Not only it is satisfactory, but also rational

# It will be worth to implement logarithmic scale into MonthlyIncome, NumberOfOpenCreditLinesAndLoans
# In my case, because of the fact that a lot of variables have values = 0, it is not worth to power it


# 4. Fill missing data ----------

credit$MonthlyIncome <- replace(credit$MonthlyIncome, is.na(credit$MonthlyIncome), median(credit$MonthlyIncome, na.rm=TRUE))

# Searching for mode of NumberOfDependents
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

find_mode(credit$NumberOfDependents)

credit$NumberOfDependents <- replace(credit$NumberOfDependents, is.na(credit$NumberOfDependents), 0)

corrplot(cor(credit, method = c("pearson")), type = "lower")

# Defaults are correlated with each other. That's why I will create two new variables:
# 1. Default flag defining IF EVER client was in default (Interaction)
# 2 I will keep variable which has the highest number of default occurences

credit$default_flag <- ifelse(credit$NumberOfTime30.59DaysPastDueNotWorse > 0 |
                              credit$NumberOfTime60.89DaysPastDueNotWorse > 0 |
                              credit$NumberOfTimes90DaysLate > 0, 1, 0)

table(credit$default_flag)
mean(credit$default_flag)

credit$max_default_occurences <- pmax(credit$NumberOfTime30.59DaysPastDueNotWorse,
                                      credit$NumberOfTime60.89DaysPastDueNotWorse,
                                      credit$NumberOfTimes90DaysLate)

# Delete default columns
delete_default_columns <- c("NumberOfTime30.59DaysPastDueNotWorse",
                           "NumberOfTime60.89DaysPastDueNotWorse",
                           "NumberOfTimes90DaysLate")
  
credit <- credit[ , !(names(credit) %in% delete_default_columns)]

corrplot(cor(credit, method = c("pearson")), type = "lower")

# 5. Outliers ----------

# Let's check some variables and decide if there are potential outliers

# Age
table(credit$age)

# One person is 0 years old - quite interesting, let's check it
credit[credit$age == 0, ] # MonthlyIncome = 6000
# something is not clear about, I will remove it
credit = credit[!credit$age == 0, ]

# max_default_occurences

# I am interested, how many times people were in default, because it was quite suspicious that some were even 98 times

table(credit$max_default_occurences) # 264 people - 98 times, no number of default occurences between 11-98

# In my opinion these observations wouldn't change a lot my predictions, however let's check their rates in terms of final default

credit %>%
  filter(SeriousDlqin2yrs == 1) %>%
  summarise(mean_default_occur = mean(max_default_occurences),
  quantile_q3 = quantile(max_default_occurences, 0.75))

# I don't want to get rid of these records, that's why I decided to change number of occurences > 90 into 17. I don't want to treat them as extreme outliers

new_defaults_till_17 <- credit %>%
           dplyr::select(max_default_occurences)  %>%
           lapply(FUN = function(x) ifelse(x > 50, 17, x))

credit$max_default_occurences <- new_defaults_till_17$max_default_occurences


# RevolvingUtilizationOfUnsecuredLines
sort(table(credit$RevolvingUtilizationOfUnsecuredLines))
table(credit$RevolvingUtilizationOfUnsecuredLines > 1)
table(credit$RevolvingUtilizationOfUnsecuredLines > 5) # 254, quite a lot, let's replace these values with 5
credit$RevolvingUtilizationOfUnsecuredLines <- ifelse(credit$RevolvingUtilizationOfUnsecuredLines > 5, 5, credit$RevolvingUtilizationOfUnsecuredLines)

# Debt Ratio & MonthlyIncome

# I don't quite get it how somebody can earn for example 1
sort(table(credit$MonthlyIncome))
# For the purpose of analysis, let's suppose that people who earns less than 100 are actually unemployed, so they will get 1's

credit$MonthlyIncome <- ifelse(credit$MonthlyIncome < 100 | credit$MonthlyIncome==0, 1, credit$MonthlyIncome)

# At first let's check if there are people who owns something, while earning = 1
sum(credit$MonthlyIncome == 1 & credit$DebtRatio > 0)
# There such observations - it is not appriopriate
# I have to get rid of it

credit <- credit[!(credit$MonthlyIncome == 0 & credit$DebtRatio > 0), ]

# Now, I will look for people who owns more than they actually earn

sum(credit$DebtRatio > 1)
quantile(credit$DebtRatio, 0.95)

# 6. Variables transformation ----------

# We should implement logarithm into these variables to be normal
credit$NumberOfOpenCreditLinesAndLoans_log<- log1p(credit$NumberOfOpenCreditLinesAndLoans)
credit$MonthlyIncome_log<- log1p(credit$MonthlyIncome)

# Logged
credit %>% 
  gather(Info, value, c(seq(11,12))) %>% 
  ggplot(aes(x=value, fill = Info)) +
  geom_histogram() +
  facet_wrap(~Info, scales = 'free')

# Create additional flag, to mark clients who owns more than they earn

credit$owns_more <- ifelse(credit$DebtRatio > 1, 1, 0)
# table(credit$owns_more)

credit$age2 <- (credit$age)**2

# Create additional flag, to mark unemployed clients - but in the model I don't want to use it

unemployed <- ifelse(credit$MonthlyIncome == 1, 1, 0)

# Interaction between unemployed and owns_more <- these are very dangerous clients
credit$dangerous_clients <- ifelse(credit$owns_more == 1 & unemployed == 1, 1, 0)
table(credit$dangerous_clients) # 2112 dangerous clients

# Interaction between age and NumberOfOpenCreditLinesAndLoans
credit$open_loan_on_pension <- ifelse(credit$NumberOfOpenCreditLinesAndLoans > 0 & credit$age > 65, 1, 0)

table(credit$open_loan_on_pension)


##############################
# END OF SHORT DATA ANALYSIS #
##############################

# 7. Modelling part ----------

train <- as.data.frame(credit)
train <- train[, c(1:4, 7:14,16)]

# NOTE: IN THE BELOW CODE I WILL CHECK HOW MY DATA BEHAVES IN ALL THE MODELS
# FOR THE FINAL DECISION LOOK AT 8th POINT "FINAL MODEL CHOICE"

# 7.1. First logit estimation
mylogit <- glm(SeriousDlqin2yrs~., data=train, 
                family=binomial(link="logit"))
summary(mylogit)
PseudoR2(mylogit)

# Joint insignificance of all variables test - logit
null_logit = glm(SeriousDlqin2yrs~1, data=train, family=binomial(link="logit"))
lrtest(mylogit, null_logit)

# Specification of logit
source("linktest.R")
linktest_result_logit = linktest(mylogit)
summary(linktest_result_logit)

# Simple backward selection
logit_gts <- step(mylogit, direction="backward", test = 'F', trace=TRUE)
summary(logit_gts)


linktest_result_logit = linktest(logit_gts)
summary(linktest_result_logit)


# 7.2. First probit estimation
myprobit <- glm(SeriousDlqin2yrs~., data=train, 
               family=binomial(link="probit"))
summary(myprobit)

# Joint insignificance of all variables test - probit
null_probit = glm(SeriousDlqin2yrs~1, data=train, family=binomial(link="probit"))
lrtest(myprobit, null_probit)

# Specification of probit
linktest_result_probit = linktest(myprobit)
summary(linktest_result_probit)


# 7.3. OLS with White's Robust Matrix

# linear probability model - just to show, that it is not optimal
lpm = lm(SeriousDlqin2yrs~., data=train)
summary(lpm)

# specification test
resettest(lpm, power=2:3, type="fitted")

# The p-value for our F-stat is 0.00. Therefore, at 5% significance level, I have to reject the Ramsey RESET test null hypothesis of correct specification. 
# This indicates that the functional form of my OLS model is not correct

# Linktest
source("linktest.R")
linktest_result = linktest(mylogit)
summary(linktest_result)

# heteroscedasticity
lpm.residuals = lpm$residuals
bptest(lpm.residuals~., data=train[, c(2:15)])
View(lpm$fitted.values)

# White's estimator of the variance-covariane matrix
robust_vcov <- vcovHC(lpm, data = train, type = "HC")
LPM_robust_matrix <- coeftest(lpm, vcov.=robust_vcov)

# Comparison of OLS vs OLS with White's robust matrix

robust.lpm = coeftest(lpm, vcov.=robust_vcov)


# 8. FINAL MODEL CHOICE  ----------

# Calculation of AIC and BIC for logit and probit
AIC(mylogit, myprobit)
BIC(mylogit, myprobit)
stargazer(lpm, robust.lpm, type="text")

# In both cases probit is better - I will use it

# Comparison of LPM, probit and logit

stargazer(lpm, myprobit, mylogit, type="text")

# General to specific
summary(myprobit)

# age2
gts_probit_no_age_2 <- glm(SeriousDlqin2yrs~., data=train[, names(train) != "age2"], 
                                    family=binomial(link="probit"))

summary(gts_probit_no_age_2)
lmtest::lrtest(myprobit, gts_probit_no_age_2) # pvalue = 0.7539- can't reject null hypothesis - omit age

# DebtRatio
`%ni%` <- Negate(`%in%`)
gts_probit_no_debtratio <- glm(SeriousDlqin2yrs~., data=train[, names(train) %ni% c("age2", "DebtRatio")], 
                         family=binomial(link="probit"))

summary(gts_probit_no_debtratio)
lmtest::lrtest(myprobit, gts_probit_no_debtratio) # 0.09 - can't reject null hypothesis - remove MonthlyIncome


# Diagnostic tests

# linktest
linktest_final_probit <- linktest(gts_probit_no_monthlyincome) # the form of model is not appriopriate
summary(linktest_final_probit)

# Other tests - Hosmer-Lemeshow and Osius-Rojek
data_for_gof <- train[, names(train) %ni% c("age2", "DebtRatio")]
columns_for_gof <- paste(colnames(data_for_gof)[c(2:11)], collapse = "+")
gts_probit_gof <- glm(SeriousDlqin2yrs ~ RevolvingUtilizationOfUnsecuredLines+age+NumberRealEstateLoansOrLines+
                        NumberOfDependents+default_flag+max_default_occurences+NumberOfOpenCreditLinesAndLoans_log+
                        MonthlyIncome_log+open_loan_on_pension+dangerous_clients, 
                      data=data_for_gof,
                      family=binomial(link="probit"))
gofresults = gof(gts_probit_gof, g = 11)

gofresults$gof
summary(gts_probit_gof)
# test  stat        val df          pVal
# HL chiSq  391.41727  9  9.281274e-79
# OsRo     Z  -10.39896 NA  2.506586e-25

# Remove dangerous_clients
data_for_gof <- data_for_gof[, names(data_for_gof) != "dangerous_clients"]
gts_probit_gof <- glm(SeriousDlqin2yrs ~ RevolvingUtilizationOfUnsecuredLines+age+NumberRealEstateLoansOrLines+
                        NumberOfDependents+default_flag+max_default_occurences+NumberOfOpenCreditLinesAndLoans_log+
                        MonthlyIncome_log+open_loan_on_pension, 
                      data=data_for_gof,
                      family=binomial(link="probit"))
gofresults = gof(gts_probit_gof)
gofresults$gof

# Remove open_loan_on_pension
data_for_gof <- train[, names(train) %ni% c("age2", "DebtRatio", "open_loan_on_pension")]
gts_probit_gof <- glm(SeriousDlqin2yrs ~ RevolvingUtilizationOfUnsecuredLines+age+NumberRealEstateLoansOrLines+
                        NumberOfDependents+default_flag+max_default_occurences+NumberOfOpenCreditLinesAndLoans_log+
                        MonthlyIncome_log+dangerous_clients, 
                      data=data_for_gof,
                      family=binomial(link="probit"))
gofresults = gof(gts_probit_gof, g= 11)
gofresults$gof

# Remove default_flag
data_for_gof <- train[, names(train) %ni% c("age2", "DebtRatio", "default_flag")]
gts_probit_gof <- glm(SeriousDlqin2yrs ~ RevolvingUtilizationOfUnsecuredLines+age+NumberRealEstateLoansOrLines+
                        NumberOfDependents+max_default_occurences+NumberOfOpenCreditLinesAndLoans_log+
                        MonthlyIncome_log+open_loan_on_pension+dangerous_clients, 
                      data=data_for_gof,
                      family=binomial(link="probit"))
gofresults = gof(gts_probit_gof, g= 11)
gofresults$gof

# Remove dangerous_clients and open_loan_on_pension
data_for_gof <- train[, names(train) %ni% c("age2", "DebtRatio", "dangerous_clients", "open_loan_on_pension")]
gts_probit_gof <- glm(SeriousDlqin2yrs ~ RevolvingUtilizationOfUnsecuredLines+age+NumberRealEstateLoansOrLines+
                        NumberOfDependents+default_flag+max_default_occurences+NumberOfOpenCreditLinesAndLoans_log+
                        MonthlyIncome_log, 
                      data=data_for_gof,
                      family=binomial(link="probit"))
gofresults = gof(gts_probit_gof, g= 11)
gofresults$gof

# Remove open_loan_on_pension and default_flag
data_for_gof <- train[, names(train) %ni% c("age2", "DebtRatio", "default_flag", "open_loan_on_pension")]
gts_probit_gof <- glm(SeriousDlqin2yrs ~ RevolvingUtilizationOfUnsecuredLines+age+NumberRealEstateLoansOrLines+
                        NumberOfDependents+max_default_occurences+NumberOfOpenCreditLinesAndLoans_log+
                        MonthlyIncome_log+dangerous_clients, 
                      data=data_for_gof,
                      family=binomial(link="probit"))
gofresults = gof(gts_probit_gof, g= 11)
gofresults$gof

# Remove NumberOfOpenCreditLinesAndLoans_log - the highest pvalue

data_for_gof <- train[, names(train) %ni% c("age2", "DebtRatio", "NumberOfOpenCreditLinesAndLoans_log")]
gts_probit_gof <- glm(SeriousDlqin2yrs ~ RevolvingUtilizationOfUnsecuredLines+age+NumberRealEstateLoansOrLines+
                        NumberOfDependents+default_flag+max_default_occurences+
                        MonthlyIncome_log+dangerous_clients+open_loan_on_pension, 
                      data=data_for_gof,
                      family=binomial(link="probit"))
gofresults = gof(gts_probit_gof, g= 11)
gofresults$gof

# Remove all interactions
data_for_gof <- train[, names(train) %ni% c("age2", "DebtRatio", "default_flag", "dangerous_clients", "open_loan_on_pension")]
gts_probit_gof <- glm(SeriousDlqin2yrs ~ RevolvingUtilizationOfUnsecuredLines+age+NumberRealEstateLoansOrLines+
                        NumberOfDependents+max_default_occurences+NumberOfOpenCreditLinesAndLoans_log+
                        MonthlyIncome_log, 
                      data=data_for_gof,
                      family=binomial(link="probit"))
gofresults = gof(gts_probit_gof, g= 11)
gofresults$gof

# MY MODEL DO NOT HAVE A PROPER STRUCTURE - LET's CONTINUE

data_for_marginal_eff <- train[, names(train) %ni% c("age2", "DebtRatio")]
marginal_eff = probitmfx(formula=SeriousDlqin2yrs ~ RevolvingUtilizationOfUnsecuredLines+age+NumberRealEstateLoansOrLines+
                   NumberOfDependents+default_flag+max_default_occurences+NumberOfOpenCreditLinesAndLoans_log+
                   MonthlyIncome_log+open_loan_on_pension+dangerous_clients , data=data_for_marginal_eff, atmean=TRUE)
marginal_eff

# Hypotheses validation
# default_flag
probit_with_default_flag <- glm(SeriousDlqin2yrs ~ default_flag , data=data_for_marginal_eff, 
                   family=binomial(link="probit"))

lrtest(gts_probit_no_debtratio, probit_with_default_flag) # pvalue = 0.00 reject H0
# MonthlyIncome
probit_with_mth_income_log <- glm(SeriousDlqin2yrs ~ MonthlyIncome_log , data=data_for_marginal_eff, 
                                family=binomial(link="probit"))

lrtest(gts_probit_no_debtratio, probit_with_mth_income_log) # pvalue - 0.00 reject H0

# Age
probit_with_age <- glm(SeriousDlqin2yrs ~ age , data=data_for_marginal_eff, 
                                  family=binomial(link="probit"))

lrtest(gts_probit_no_debtratio, probit_with_age) # pvalue - 0.00 reject H0

# NumberRealEstateLoansOrLines

probit_with_nrelol <- glm(SeriousDlqin2yrs ~ NumberRealEstateLoansOrLines , data=data_for_marginal_eff, 
                       family=binomial(link="probit"))

lrtest(gts_probit_no_debtratio, probit_with_nrelol) # pvalue - 0.00 reject H0

# Pseudo R^2

BaylorEdPsych::PseudoR2(gts_probit_gof)

# Summary table - stargazer

stargazer(LPM_robust_matrix, mylogit, myprobit, gts_probit_no_age_2, gts_probit_no_debtratio, 
          type = 'text',
          style = 'default',
          out = 'stargazer_comparison.html')

