rm(list = ls())

pkgs <- c("wooldridge", "knitr", "stargazer")
#install.packages(pkgs)   # first time only
library(wooldridge)
library(knitr)
library(stargazer)
data("loanapp")

# step 2. summarise loanapp, appinc, price, and obrat

summary(loanapp$loanamt)
summary(loanapp$appinc)
summary(loanapp$price)
summary(loanapp$obrat)

# step 2.2-regress loanamt on appinc and interpret the results
reg1 <- lm(loanamt~appinc, data = loanapp)
summary(reg1)
coef(reg1)
# b1 of appinc = 0.4008553
# In this sample, every thousand of dollars of extra applicant income is associated with a predicted increase in the loan amount by 400.86 dollars

# step 2.3 regress loanamt on appinc price and obrat and interpret results
reg2 <- lm(loanamt ~ appinc + price + obrat, data = loanapp)
summary(reg2)
coef(reg2)
# Comparing 2 mortgage loan applications on a property costing the same amount of money in thousands of dollars and applications with the same obligation %,
# in this sample, an increase in applicant income by a thousand dollars is associated with an increase in the loan amount granted by 89.17 dollars

stargazer(reg1, reg2, type = "text")
# adjusted r^2 increased a lot for regression including price and obrat which means that weve been penalised for their addition
# r^2 increased which is expected meaning more of the variation in price is explained by our regressors
# f stat is much higher for the second regression > than crit value therefore we have that there is a conncetion between the rest of the regressors and the price

reg3 <- lm(loanamt ~ appinc + white + black + hispan, data = loanapp)
coef(reg3)

