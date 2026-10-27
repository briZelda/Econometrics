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


