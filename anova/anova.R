# ANOVA and Multiple Regression - Chapter 10 Discovering Statisitcs, Andy Field 
#
setwd("~/OneDrive/MIDS/W203/Data/anova")

# use the downloaded dummy.dat file
dummy <- read.delim("dummy.dat", header=TRUE)
head(dummy)
summary(dummy)
ls.str()

# looking at the dummy data, there is a vector for libido, and vectors for dummy1 and dummy2 that 
# appear to be coded 0,1 for the dummy variable. There's also a vector for dose that ranges from 1-3
# but is supposed to represent "placebo", "low dose", "high dose", so we need to recode as a factor
dummy$dose<-factor(dummy$dose, levels = c(1:3), labels = c("Placebo", "Low Dose", "High Dose"))
ls.str()
summary(dummy)

# The libido vector is the single measurement here, but each value is one of the three categories.
# Therefor values of dummy1,dummy2 (0,0) result in the placebo values, values (0,1), (1,0) result
# in one of the low/high dose categories.

# The number of dummy variables needed is # groups - 1.

# now we can run the linear regression
dummy.1 <- lm(libido ~ dummy1 + dummy2, data = dummy)
summary(dummy.1)

# We see from the output that F(2, 12) is significant at p < .05
# Therefor using the group means is a better model than using the overall mean 
# We also see that beta(1) is significant at p < .05 but beta(2) is not (p=.282)

# --------------------- The Logic of ANOVA ---------------------- #
#
# The simplest model we can fit to a set of data is the grand mean (the mean of the outcome variable)
# This basic model represents the "no effect" of the predictor variable on the outcome variable
#
# We can fit a different model to the data that represents our hypotheses. If this model fits the data
# well then it must be better than using the grand mean.
#
# The intercept and one or more co-efficents describe the chosen model
#
# The bigger the coefficents the greater the deviation between the regression line and the grand mean
#
# In correlational research, the co-efficients represent the slope of the line, but in experimental
# research they reperesent the differences between the group mean
#
# If the differences between the group means are large enough then the resulting model will be a better
# fit of the data than the grand mean and we can infer that our group means are significantly different.
#
# The F-ration is the ratio of the explained variance (due to the model) to the unexplained variance
#
# ---------------------------------------------------------------#
#
# Calculate the total variation of the data (sum of squares)
# 

gsd <- sd(dummy$libido)
sst <- (gsd^2)*(length(dummy$libido)-1)

# Total amount of variation in the data is sst = 43.7333
# How much of this variation can the regression model explain?
# This is given by the model variance, which is the difference between each group mean and the grand mean, squared
placebo <- dummy[dummy$dummy1 == 0 & dummy$dummy2 == 0, ]
high <- dummy[dummy$dummy1 == 1 & dummy$dummy2 == 0, ]
low <- dummy[dummy$dummy1 ==0 & dummy$dummy2 == 1, ]

m.grand <- mean(dummy$libido)
m.low <- mean(low$libido) 
m.high <- mean(high$libido)
m.placebo <- mean(placebo$libido)

ssm <- length(placebo$libido)*(m.placebo - m.grand)^2 + length(low$libido)*(m.low - m.grand)^2 + length(high$libido)*(m.high - m.grand)^2

# The variance explained by the model is ssm = 20.1333, so out of 43.733 variance of the data, the model explains 20.1333 

# We can calculate the residuals as the variance not explained by the model, which is the sum of group variances and degrees of freedom

s.low <- sd(low$libido)
s.high <- sd(high$libido)
s.placebo <- sd(placebo$libido)

ssr <- (length(placebo$libido) - 1) * s.placebo^2 + (length(low$libido) - 1) * s.low^2 + (length(high$libido) - 1) * s.high^2

# The variance not explained by the model is ssr = 23.6

# The sum of mean squares
# the sum of mean squares for the model is ssm/degrees of freedom for the model

msm <- ssm/(3 - 1)

# the sum of mean squares for the residuals is ssr/degrees of freedom for residuals

msr <- ssr/(14 - 2)

# The F-ratio is msm/msr 

fr <- msm/msr 

# which gives 5.1184, and compare to the F-ratio computed as part of the linear regression of 5.119

# ---------------------- Planned Contrasts ------------------------ #
#
# Using the contrast.dat file downloaded from the supplemental website
contrast <- read.delim("contrast.dat", header = TRUE)
summary(contrast)
head(contrast)

contrast$dose<-factor(contrast$dose, levels = c(1:3), labels = c("Placebo", "Low Dose", "High Dose"))
ls.str(contrast)
# this data is coded with the weighting variables as dummy1 and dummy2

contrast.planned <- lm(libido ~ dummy1 + dummy2, data = contrast)
summary(contrast.planned)

