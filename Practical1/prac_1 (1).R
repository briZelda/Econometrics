#' ---
#' title: "Practical 1: Solutions"
#' subtitle: "ECON20110/30370 Econometrics"
#' author: "Riddhi Kalsi"
#' output: pdf_document
#' ---
#+ setup, include = FALSE
knitr::opts_chunk$set(fig.width = 6, fig.height = 4, fig.align = "center",
                      message = FALSE, warning = FALSE)

## Setup
# clears environment
rm(list = ls())

# EDIT THIS LINE: put the path to the folder where you saved this script.
# All results, including the PDF, are stored in this folder.
# Note that the path uses forward slashes "/" not backward slashes "\"
setwd("C:/Users/brise/Desktop/Uni/econ(26-27)/econometrics/practicals/practical1")

# Install any package that is missing, then load all three.
# wooldridge: the data. knitr: tables (kable). stargazer: regression table.
pkgs <- c("wooldridge", "knitr", "stargazer")
new  <- pkgs[!pkgs %in% rownames(installed.packages())]  # packages not yet installed
if (length(new)) install.packages(new, repos = "https://cloud.r-project.org")

library(wooldridge)
library(knitr)
library(stargazer)

data("wage1")

# fmt() is a small helper for printing numbers in the text.
# It rounds x to d decimal places (2 by default) and keeps trailing zeros,
# so fmt(5.9) gives "5.90" and fmt(0.47909, 3) gives "0.479".
fmt <- function(x, d = 2) formatC(x, format = "f", digits = d)

#' ## Part A: Summarising
#'
#' ### Q1. Dimensions

dim(wage1)   # number of rows (observations) and columns (variables)
head(wage1[, c("wage", "lwage", "educ", "exper", "female")])  # first 6 rows, selected columns

#' The dataset has `r nrow(wage1)` observations and `r ncol(wage1)` variables.
#'
#' ### Q2. Summary statistics

# sapply() applies the function to each of the three variables and collects
# the results in a matrix; t() transposes it so each variable is a row.
sumstats <- t(sapply(wage1[c("wage", "educ", "exper")], function(x)
  c(Mean = mean(x), Median = median(x), SD = sd(x), Min = min(x), Max = max(x))))
kable(sumstats, digits = 2)  # prints the matrix as a formatted table

#' ### Q3. Mean of `female`

mean(wage1$female)

#' The mean of a 0/1 variable is the share of ones. So `r fmt(100 * mean(wage1$female), 1)`%
#' of the sample is female.
#'
#' ### Q4. Mean vs median of wage

c(Mean = mean(wage1$wage), Median = median(wage1$wage))

#' The mean is above the median. Wages are right-skewed: a small number of high
#' earners pull the mean up.
#'
#' ## Part B: Visualising
#'
#' ### Q5. Histogram of wage

# breaks = 30 asks for roughly 30 bars; try other values to see the effect.
hist(wage1$wage, breaks = 30, main = "", col = "grey80", border = "white",
     xlab = "Hourly wage (dollars)")
abline(v = mean(wage1$wage),   lty = 2, lwd = 2)  # vertical line at the mean (dashed)
abline(v = median(wage1$wage), lty = 1, lwd = 2)  # vertical line at the median (solid)
legend("topright", legend = c("Mean", "Median"), lty = c(2, 1), lwd = 2, bty = "n")

#' Most wages sit at the low end, with a long right tail. This matches Q4.
#'
#' ### Q6. Histogram of log wage

hist(wage1$lwage, breaks = 30, main = "", col = "grey80", border = "white",
     xlab = "Log hourly wage")

#' Taking logs compresses the right tail. Log wages are much closer to symmetric.
#'
#' ### Q7. Wage by sex

# wage ~ female means "wage split by female". factor() turns the 0/1 codes
# into labelled groups so the axis reads Male/Female.
boxplot(wage ~ factor(female, labels = c("Male", "Female")), data = wage1,
        xlab = "", ylab = "Hourly wage (dollars)", col = "grey85")

# SIDE NOTE. Question: what are the hollow circles above each box?
# Answer: they are outliers. The box runs from the 25th to the 75th percentile,
# so its height is the IQR. The whiskers stop at the most extreme observation
# within 1.5 x IQR of the box. Any observation beyond that is drawn
# individually as a circle. Here they are the high earners in the right tail.

# tapply() computes a statistic separately for each group (female = 0 and 1).
med <- tapply(wage1$wage, wage1$female, median)
iqr <- tapply(wage1$wage, wage1$female, IQR)
rbind(Median = med, IQR = iqr)  # stack the two results into one small table

#' The median wage is `r fmt(med["0"])` dollars for men and `r fmt(med["1"])`
#' for women. The interquartile range is `r fmt(iqr["0"])` for men and
#' `r fmt(iqr["1"])` for women.
#'
#' ## Part C: Exploring relationships
#'
#' ### Q8. Wage and education

# pch = 16 draws solid dots. rgb(0, 0, 0, 0.35) is black at 35% opacity,
# so overlapping points show up darker.
plot(wage1$educ, wage1$wage, pch = 16, col = rgb(0, 0, 0, 0.35),
     xlab = "Years of education", ylab = "Hourly wage (dollars)")
cor(wage1$wage, wage1$educ)

#' The relationship is positive: more education goes with higher wages on
#' average. The correlation is `r fmt(cor(wage1$wage, wage1$educ), 3)`, so there
#' is a lot of scatter around the trend.
#'
#' ### Q9. Regression of wage on education

reg1 <- lm(wage ~ educ, data = wage1)  # OLS of wage on educ, with an intercept
coef(reg1)                             # intercept and slope

plot(wage1$educ, wage1$wage, pch = 16, col = rgb(0, 0, 0, 0.35),
     xlab = "Years of education", ylab = "Hourly wage (dollars)")
abline(reg1, col = "firebrick", lwd = 2)  # adds the fitted OLS line

# predict() plugs new values of educ into the fitted line.
pred <- predict(reg1, newdata = data.frame(educ = c(12, 16)))
pred

#' Fitted line: $\widehat{\text{wage}} = `r fmt(coef(reg1)[1])` + `r fmt(coef(reg1)[2])`\,\text{educ}$.
#'
#' Each additional year of education is associated with a
#' `r fmt(coef(reg1)[2])` dollar higher predicted hourly wage. The predicted
#' wage is `r fmt(pred[1])` dollars at 12 years and `r fmt(pred[2])` dollars
#' at 16 years.
#'
#' ### Q10. Regression of wage on experience

reg2 <- lm(wage ~ exper, data = wage1)
coef(reg2)
cor(wage1$wage, wage1$exper)

# Points are very faint (12% opacity) so the lowess curve stays visible.
plot(wage1$exper, wage1$wage, pch = 16, col = rgb(0, 0, 0, 0.12),
     xlab = "Years of potential experience", ylab = "Hourly wage (dollars)")
abline(reg2, col = "firebrick", lwd = 2)
lines(lowess(wage1$exper, wage1$wage), lty = 2, lwd = 3)  # smooth local-average curve
legend("topright", legend = c("OLS line", "Lowess curve"),
       col = c("firebrick", "black"), lty = c(1, 2), lwd = c(2, 3), bty = "n")

#' The slope is small (`r fmt(coef(reg2)[2], 3)`) and the correlation is weak
#' (`r fmt(cor(wage1$wage, wage1$exper), 3)`).
#'
#' **What is lowess?** A straight line forces one shape on the data. Lowess
#' (locally weighted smoothing) does not. At each value of experience it fits
#' a small regression using only nearby observations, giving more weight to
#' the closest ones. Joining these local fits gives a smooth curve that
#' follows whatever shape the data have: linear, quadratic, logarithmic or
#' something else. It is a way to look at the shape before choosing a model.
#'
#' Here the lowess curve rises with experience and then levels off or falls.
#' That looks more like a quadratic than a straight line, so the OLS line
#' misses this curvature.
#'
#' ### Regression table
#'
#' Coefficients, N and $R^2$ only. Inference comes in Week 3.

#+ regtable, results = "asis"
# results = "asis" (set in the line above) lets the LaTeX table print as a
# proper table in the PDF. report = "vc" shows variable names and coefficients
# only; omit.stat drops the statistics we have not covered yet.
stargazer(reg1, reg2, type = "latex", header = FALSE,
          title = "Simple linear regressions of hourly wage",
          dep.var.labels = "Hourly wage (dollars)",
          covariate.labels = c("Years of education", "Years of experience", "Intercept"),
          report = "vc", digits = 3, omit.stat = c("adj.rsq", "ser", "f"))

#' ## Extension
#'
#' ### Q11. OLS by hand

b1 <- cov(wage1$educ, wage1$wage) / var(wage1$educ)  # slope = cov(x, y) / var(x)
b0 <- mean(wage1$wage) - b1 * mean(wage1$educ)       # intercept = ybar - b1 * xbar
c(b0 = b0, b1 = b1)
# all.equal() checks equality up to tiny rounding differences in the computer.
all.equal(unname(coef(reg1)), c(b0, b1))

#' The hand-computed values match `coef(reg1)`.
#'
#' ### Q12. Residuals

res1 <- resid(reg1)  # residual = actual wage minus fitted wage
sum(res1)            # prints something like 1e-14: zero up to rounding error

plot(wage1$educ, res1, pch = 16, col = rgb(0, 0, 0, 0.35),
     xlab = "Years of education", ylab = "Residual")
abline(h = 0, lty = 2, lwd = 2)

#' The residuals sum to zero, up to rounding error. Their spread grows with
#' education.