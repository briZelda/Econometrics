# P1-exploring data and simple linear regression in R

#setup
install.packages("wooldridge")

library(wooldridge)
data("wage1")

#PART A
# 1. How many observations and vars are in the dataset? Look at the 1st few rows.
head(wage1)
summary(wage1)

# there are about 24 variables?
---------------------------------
# 2. Report the mean, median, standard deviation, minimum and maximum of wage, educ
# and exper.

#---WAGE
mean_wage <- mean(wage1$wage)
mean_wage # 5.896103 dollars

median_wages <- median(wage1$wage)
median_wages # 4.65 dollars

sd_wages <- sd(wage1$wage)
sd_wages # 3.6931 dollars

min_wages <- min(wage1$wage)
min_wages # 0.53 dollars

max_wages <- max(wage1$wage)
max_wages # 24.98 dollars

#---EDUC

print(mean_educ <- mean(wage1$educ)) # 12.5627yrs

print(median_educ <- median(wage1$educ)) #12 yrs

print(sd_educ <- sd(wage1$educ)) # 2.7690 yrs

print(min_educ <- min(wage1$educ)) # 0 yrs

print(max_educ <- max(wage1$educ)) # 18 yrs


#---EXPER
print(mean_exper <- mean(wage1$exper)) #17.017 yrs

print(median_exper <- median(wage1$exper)) #13.5yrs

print(sd_exper <- sd(wage1$exper)) # 13.5722 yrs

print(min_exper <- min(wage1$exper)) # 1

print(max_exper <- max(wage1$exper)) #51

# -------------------------------
# 3. Compute mean(wage1$female). What does this number tell you? Why does taking
# the mean of a 0/1 variable give you this?
mean_female <- mean(wage1$female)
# taking the mean of this binary variable gives us info on what percentage of US
# employees within the dataset are female

# ------------------------------------
# 4. Compare the mean and the median of wage. What does the difference suggest about the
# distribution of wages?

mean_wage
median_wages      

# the mean wage is higher than the median wage suggesting that there is a 
# right skew in the data, therefore there are some extreme values in the higher mean
# wages as compared to the lower ones, depicting an overestimation
# of mean wages = disproportionate representation of extremely high wages pushing up 
# mean.

# -------------------------------------------
# PART B-visualising

# 5. Plot a histogram of wage. Describe its shape. Is it consistent with your answer to
# Question 4?

hist(wage1$wage, col = "pink", xlab = "Avg hourly earnings in dollars", freq = FALSE)

# the graphs supports my answer that there is a right skew in the data, and the mean wages 
# are much higher than the median
# --------------------------------------------

# 6. Now plot a histogram of lwage. How does its shape compare with the histogram of
# wage?

hist(wage1$lwage, col = "darkmagenta", xlab = "log of wages", freq = FALSE)

# the shape of the histogram depicted by lwage has central tendency and follows 
# a bell shape, characteristic of a normal distribution, and this is because taking
# the log of a variable allows for extreme values to be filtered out/accounted for
# therefore this is a more accurate depiction of wage distribution when accounting for
# outliers/extrapolations=> mean and median wages are closer in number
# -----------------------------------------------------

# 7. Draw a boxplot of wage separately for men and women. What differences do you see
# in the median and the spread?

# firstly creating objects for wages of men and women
wage1$wage
View(wage1)

df <- data.frame(wage1$wage, wage1$female)
print(df)
View(df)
# dont see missing values but will treat for it anyway
summary(df)

#separating female and male wages
women_wages <- wage1$wage[wage1$female == 1]
men_wages <- wage1$wage[wage1$female == 0]

boxplot_W <- boxplot(women_wages, 
                     col = "lightgreen", 
                     ylab = "Womens avg wages in dollars per hour")

boxplot_M <- boxplot(men_wages, 
                     col = "lightblue",
                     ylab = "Mens avg wages in dollars per hour")
# visualising both at same time bc i cant tell otherwise
#boxplot(women_wages, men_wages, names = c("women", "men"), ylab = "wage", xlab = "gender")

median(women_wages, na.rm = TRUE) #3.75
median(men_wages, na.rm = TRUE) #6

IQR(women_wages, na.rm = TRUE) #2.51
IQR(men_wages, na.rm = TRUE) #4.6225

# mens wages have a higher median than womens, as well as a higher iqr
# mens wage ceiling is much higher than womens, and women's wage floor is much lower than mens
# takeaways
# womens min wages earned = lower than mens, and women are constrained to a smaller range of wages
# outliers = womens highest outlier is still not as high as max male outlier
# men = more spread of wage

# ---------------------------PART C
# 8. Make a scatter plot with educ on the horizontal axis and wage on the vertical axis.
# Describe the relationship: direction, rough strength, any unusual points. Compute the
# correlation between the two variables.

# scatter plot making
plot(wage1$educ, wage1$wage, 
     xlab = "Education in years",
     ylab = "Hourly wage in dollars/h",
     col = rgb(0, 0, 0, 0.4),
     pch = 16)
cor(wage1$educ, wage1$wage)    #0.4059033

# 9. Run a simple linear regression of wage on educ
reg1 <- lm(wage ~ educ, data = wage1) #fitted regression line (OLS)
coef(reg1)

# b)add the line to your scatterplot using abline(). does it summarise the data well?
abline(reg1, col = "magenta", lwd = 2) #lwd = line thickness
# ehh not great summarisation, lots of extremeties not being captured but might be a good thing

# c) for every extra year in education/of experience, we expect wage to increase 0.54 times on average
# 
# d) What wage does the line predict for someone with 12 years of education? And
# with 16

pred <- predict(reg1, newdata = data.frame(educ = c(12, 16)))

#10
plot(wage1$exper, wage1$wage, xlab = "Experience in years", ylab = "Wage in dollars per hour",
     pch = 16,
     col = rgb(0, 0, 0, 0.12))
cor(wage1$exper, wage1$wage)
# weaker correlation than education and wage 
reg2 <- lm(wage~exper, data = wage1)

abline(reg2, col = "lightblue", lwd = 2)
# completely misses upper values

# extension:
