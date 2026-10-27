pkgs <- c("wooldridge", "knitr", "stargazer")
# install.packages(pkgs)   # first time only
library(wooldridge)
library(knitr)
library(stargazer)
data("sleep75")

# Step 2.1
# summarise sleep, totwrk, educ
summary(sleep75$sleep)
summary(sleep75$totwrk)
summary(sleep75$educ)
summary(sleep75$age)

# step 2.2-regress sleep on totwrk and interpret results
reg1 <- lm(sleep ~ totwrk, data = sleep75)

coef(reg1) # intercept = 3589.37695 mins, b1 = -0.1507458
# For this sample, an increase in total minutes worked per week by 1 is associated with a
# predicted decrease of 0.1507458 minutes of sleep at night by week, on average

summary(reg1)

reg2 <- lm(sleep ~ totwrk + educ + age, data = sleep75)
reg2
coef(reg2)
# comparing 2 adults that are the same age and have the same years in education, but have differing 
# total minutes worked per week by 1 minute, the predicted minutes of sleep per night per week is 0.1484 less for the one working 1 more minute

summary(reg2)

# showing what each regression side by side looks like
stargazer(reg1, reg2, type = "text")
# coefficient of totwrk lower in reg2, same se, 
# adjusted R^2 for reg2 is higher meaning we have been penalised for adding extra
# regressors i.e., they dont explain as much of th variation in y
# but R^2 went up as well (thats to be expected0

# q3
#reg3 <- lm(loanamt~appinc, data = sleep75)
data = ("loanapp")
reg3 <- lm(loanamt ~ appinc, data = loanapp)
reg3
summary(reg3)

# q5- for a mult regression where b1 is the coefficient of totalminutes worked and there are also
# b2 - education and b3 - age, what procedure reproduces b1 exactly given tha all regressions
# include an intercept an use the same sample
# ANSWER: regress totwrk on educ and age and save the residuals, then regress dependent variable
#  i.e. sleep in this case on said residuals and slope will be = b1

# q6- A student regresses y on x by OLS with an intercept, using n = 805 obs.
# She reports only R² = 0.191. Standard errors are the usual OLS (homoskedasticity-only)
# standard errors. What is the absolute value of the t statistic for  H0:β1=0

# Only one regessor and an intercept, so q = 1, one restriction t test =
# F test = t^2
# this is the same as writing F test in its R^2 form!
# t^2 = F = R^2 / (1-R^2)/(n-2)
# which in turn gives: 189.5834363
# but we are looking for t stat not t^2 so:
sqrt(189.5834363) # gives 13.768... so approx 13.77

  
