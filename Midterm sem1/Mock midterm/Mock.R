# mock midterm
rm(list = ls())

pkgs <- c("knitr", "stargazer")
#install.packages(pkgs)   # first time only
library(knitr)
library(stargazer)
ofpdata <- read.csv("OFP-1.csv")

ofpdata
summary(ofpdata$ofp)
summary(ofpdata$numchron)
summary(ofpdata$school)
summary(ofpdata$faminc)
summary(ofpdata$age)
summary(ofpdata$region)
summary(ofpdata$sex)

# regressing number of physician office visits on number of chronic conditions
reg1 = lm(ofp ~ numchron, data = ofpdata)
coef(reg1)
# intercept = 3.75, b1 = 1.31
# In this sample, an increase in the number of chronic illnesses a person has by
# 1 is associated with a predicted increase in the number of physician office
# visits by 1.31 on avg

# In this sample, if a person has 0 chronic condiitions, they are predicted to
# make 3.75 physician office visits on average.
summary(reg1)
# adjusted R^2 and R^2 are fairly similar
# the num of chronic conditions explains 6.9% roughly of thye variation in the
# number of physician office visits

reg2 = lm(ofp ~ numchron + school + faminc + age + I(age^2), data = ofpdata)
reg2
# Comparing people of same age, family income and number of years in school, in
# this sample, an increase in the number of chronic conditions by 1 is associated 
# with an increase in the number of physician office visits by 1.33 on avg.


# showing what each regression side by side looks like
stargazer(reg1, reg2, type = "text")
# coefficient of numchron slightly higher in reg2, same se, 
# adjusted R^2 for reg2 is higher meaning we have been penalised for adding extra
# regressors i.e., they dont explain as much of th variation in y
# but R^2 went up as well (thats to be expected0
# f statistic for reg2 is a lot lower than reg 1's f stat but both show that
#there is a connection between ofp and numchron for instance (i.e., b1 doesnt equal 0)

