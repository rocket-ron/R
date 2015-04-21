############################################################################
#
# W203 Week 14 - Logistic Regression
#
# Exercises from Andy Field's Discovering Statistics With R, Chapter 8
#
############################################################################
#
# Eel fun, using the eel.dat data set from http://www.sagepub.com/dsur/study/default.htm
#
setwd("~/Documents/R/logistic regression")
#
# Get the eel data set
eelData <- read.delim("eel.dat", header = TRUE)
#
str(eelData)
head(eelData)
#
# reset the factor levels for the dichotomous variables "Intervention" and 
# "Cured" to the opposite of what R defaulted them to be 
eelData$Cured <- relevel(eelData$Cured, "Not Cured")
eelData$Intervention <- relevel(eelData$Intervention, "No Treatment")
str(eelData)
# 
# OK, that looks better. Let's get on with some logistic regression
# Use the glm() method, and use the Intervention variable as a predictor of 
# Cured outcome. In addition, specify the family of distributions to use as 
# the binomial distribution with the default link logit link function.
#
eelModel.1 <- glm(Cured ~ Intervention, data = eelData, family = binomial(link = "logit"))
#
# Now let's add to the model Duration and create a second model 
eelModel.2 <- glm(Cured ~ Intervention + Duration, data = eelData, family = binomial(link = "logit"))
#
# Let's see what we've got 
summary(eelModel.1)
summary(eelModel.2)
#
# Based on the model summaries, Intervention is statistically significanly different
# from zero, p < .003, but Duration is not.
# The chi square statistic on the first model is the difference between the two
# residual deviances (null deviande and residual deviance) = 154.08 - 144.16
print(154.08-144.16)   # 9.93
#
# Let's compute the chi square form the model itself
#
eelModel.1.chisq <- eelModel.1$null.deviance - eelModel.1$deviance
#
# We can calculate the probablity of the chi square statistic with the following
chisq.prob <- 1 - pchisq(eelModel.1$null.deviance - eelModel.1$deviance, eelModel.1$df.null - eelModel.1$df.residual)
#
# p < .002 based on the above calculation, so we can reject the null hypothesis
# that the model is no better than chance at predicting the outcome.
# Therefore: including Intervention in the model produced a significant improvement
# in the fit of the model, chi-sq(1) = 9.93, p = .002.
#
# If we examine the coefficients for model 1, we see that Intervention is a 
# significant predictor of being cured, b = 1.23, z = 3.07, p = .002
#
# Calculating the various R values for this model (model 1)
# 
# First, the standard R
# sqrt((z^2 - 2*degFreedom)/null deviance)
print(sqrt((3.074^2-(2*1))/154.08))   # this is pulling data from the summary display
#
# but let's do it directly from the model. First we extract the z-statistic to make
# it a little easier to deal with then we compute R
eelModel.1.z <- coef(summary(eelModel.1))[-1, 3]
eelModel.1.R <- sqrt((eelModel.1.z^2 - 2*(eelModel.1$df.null - eelModel.1$df.residual))/eelModel.1$null.deviance)
# 0.22
#
# R^2 L (Hosmer & Lemeshow) is chi-square / -2LL(baseline)
eelModel.1.R2L <- eelModel.1.chisq / eelModel.1$null.deviance   # .0644
#
# Cox and Snell R^2
eelModel.1.RCS <- 1 - exp((eelModel.1$deviance - eelModel.1$null.deviance) / length(eelModel.1$fitted.values))
# 0.0841
#
# Nagelkerke's R^2
eelModel.1.RCN <- eelModel.1.RCS / (1 - (exp(-(eelModel.1$null.deviance / length(eelModel.1$fitted.values)))))
# 0.113
#
# The book goes into a huge calculation using probabilities to calculate the odds ratio
# when all you need to do is take the exponential of the predictor coefficient.
eelModel.1.oddsRatio <- exp(eelModel.1$coefficients[2])   # 3.42
#
# Confidence Intervals
exp(confint(eelModel.1))
#
# The values of the confidence interval ends are greater than 1, which means as the predictor
# increases, so do the odds of being cured. Values less than one mean that as the predictor 
# increases the odds of being cured decrease. Since we observed that the direction was an 
# increase is odds with an increase of predictor variable, this corresponds and supports that
# observation. Furthermore, since the confidence interval doesn't go below 1 then it doesn't 
# include the chance that in the population the direction of the relationship is opposite to 
# what we observed.
#
#
# Model #2 Analysis - Adding Duration
#
# The b value is very small and not significant, p = 0.964. The deviance between the models
# is the same and the AIC is slightly higher in model 2, so model 1 is a better model than
# model 2. 
# 
# Comparing the models - manual calculations vs. anova
# Manual --
# 
eelModel.2.chisq <- eelModel.1$deviance - eelModel.2$deviance  # deviance between the 2 models
chisq.prob <- 1 - pchisq(eelModel.2.chisq, eelModel.1$df.residual - eelModel.2$df.residual)
# confirms p = 0.9644, chi squared is 0.002
#
# Use anova 
anova(eelModel.1, eelModel.2)  # gives same output of deviance (chi sq) = 0.002, df = 1
#
# Residuals
#
eelData$predicted.probabilities <- fitted(eelModel.1)
eelData$standardized.residuals <- rstandard(eelModel.1)
eelData$studentized.residuals <- rstudent(eelModel.1)
eelData$dfbeta <- dfbeta(eelModel.1)
eelData$dffit <- dffits(eelModel.1)
eelData$leverage <- hatvalues(eelModel.1)
#
# 
head(eelData[, c("Cured", "Intervention", "Duration", "predicted.probabilities")])
#
# from the output we see that the Cured, No Treatment -> 43% probability
# and that Cured, Intervention -> 72% probability, which helps to validate the model 
#
# Let's see if we see points for which the model is poor fit, or which have undue leverage
eelData[, c("leverage", "studentized.residuals", "dfbeta")]
#
# Leverage -> expected value = (k + 1)/N = 2/113 = 0.018 
# All the leverage values in the output are very close to this value, so that's good
#
# Residuals (standard or studentized) should have ony 5% outside +/- 1.96, 1% outside +/-2.58
# All the values in the output look very good
#
# dfBeta for constant and for Intervantion should be less than 1 -> all look OK.
#
# Soccer Penalty Data 
penaltyData <- read.delim("penalty.dat", header = TRUE)
head(penaltyData)
str(penaltyData)
#
# data looks ok, let's start a hierarchical logistic regression model setup, 
# starting with Scored ~ Previous
penaltyModel.1 = glm(Scored ~ Previous, data = penaltyData, family = binomial(link = "logit"))
summary(penaltyModel.1)
#
# highly significant coefficient of 0.0964, p = 0
penaltyModel.1.chisq <- penaltyModel.1$null.deviance - penaltyModel.1$deviance
penaltyModel.1.chisqProbability <- pchisq(penaltyModel.1.chisq, penaltyModel.1$df.null - penaltyModel.1$df.residual)
#
# We end up with a chi squared of 42.466, p = 1 -> we can't reject the null hypothesis
# that the model is better than chance
#
penaltyModel.1.z <- coef(summary(penaltyModel.1))[-1, 3]
penaltyModel.1.R <- sqrt((penaltyModel.1.z^2 - 2*(penaltyModel.1$df.null - penaltyModel.1$df.residual))/penaltyModel.1$null.deviance)
#
# R = 0.427
penaltyModel.1.R2L <- penaltyModel.1.chisq / penaltyModel.1$null.deviance
#
# R^2 L = 0.41
penaltyModel.1.RCS <- 1 - exp((penaltyModel.1$deviance - penaltyModel.1$null.deviance) / length(penaltyModel.1$fitted.values))
# R^2 CS = 0.432
#
penaltyModel.1.RCN <- penaltyModel.1.RCS / (1 - (exp(-(penaltyModel.1$null.deviance / length(penaltyModel.1$fitted.values)))))
# R^2 N = 0.577
# All the R and R^2 values are very high
#
# odds ratio
penaltyModel.1.oddsRatio <- exp(penaltyModel.1$coefficients[2]) # 1.10
# confidence intervals
exp(confint(penaltyModel.1))
#
# Move on to add PSWQ to the model 
penaltyModel.2 <- glm(Scored ~ Previous + PSWQ, data = penaltyData, family = binomial(link = "logit"))
summary(penaltyModel.2)
#
# b(Previous) = 0.0648, p = .0034
# b(PSWQ) = -0.23009, p = .00395
penaltyModel.2.chisq <- penaltyModel.1$deviance - penaltyModel.2$deviance
penaltyModel.2.chisqProbability <- pchisq(penaltyModel.2.chisq, penaltyModel.1$df.residual - penaltyModel.2$df.residual)
# chi squared = 12.51, p = .999
penaltyModel.2.z <- coef(summary(penaltyModel.2))[-1, 3]
penaltyModel.2.R <- sqrt((penaltyModel.2.z^2 - 2*(penaltyModel.1$df.residual - penaltyModel.2$df.residual))/penaltyModel.1$deviance)
# R values have come down to ~0.33 and 0.32, respectively
penaltyModel.2.R2L <- penaltyModel.2.chisq / penaltyModel.1$deviance
penaltyModel.2.RCS <- 1 - exp((penaltyModel.2$deviance - penaltyModel.1$deviance) / length(penaltyModel.2$fitted.values))
penaltyModel.2.RCN <- penaltyModel.2.RCS / (1 - (exp(-(penaltyModel.1$deviance / length(penaltyModel.2$fitted.values)))))
# R^2 values also show improvement
#
# add Anxious to the penalty model 
penaltyModel.3 <- glm(Scored ~ Previous + PSWQ + Anxious, data = penaltyData, family = binomial())
summary(penaltyModel.3)
#
# testing assumptions
library(car)
vif(penaltyModel.3)
1/vif(penaltyModel.3)
# Previous and Anxious have VIF 35.5 , way over 10.
#
# Check Pearson's correlation between predictor variables 
library(polycor)
penaltyVars <- penaltyData[, c("Previous", "PSWQ", "Anxious")]
cor(penaltyVars)
# extremely high correlation between Anxious and Previous, and a high correlation between Anxious and PSWQ
cor.test(penaltyData$Previous, penaltyData$Anxious)  # highly significant, cor = -0.9929
cor.test(penaltyData$Previous, penaltyData$PSWQ)  # highly significant, cor = -0.644
cor.test(penaltyData$Anxious, penaltyData$PSWQ)   # highly significant, cor = 0.65
#
# The better a player's previous score, the lower the anxiety
# Anxiety and PSWQ are correlation and have an inverse relationship with Previous
# This model is unreliable.
#
# Testing for linearity
# We need to test for linearity of the continuous predictors with the logit function.
# To do this we need to compute the indicator variable as ln(predictor) * predictor for each predictor
# Then we add all the variables and the interaction variables into a new model 
# We examine thie significance of the interaction variables. If any are significant
# then we have violated linearity for that variable/predictor
penaltyData$logPSWQInt <- log(penaltyData$PSWQ)*penaltyData$PSWQ
penaltyData$logAnxiousInt <- log(penaltyData$Anxious)*penaltyData$Anxious
penaltyData$logPreviousInt <- log(penaltyData$Previous)*penaltyData$Previous
penaltyModel.test <- glm(Scored ~ Previous + PSWQ + Anxious + logPreviousInt + logPSWQInt + logAnxiousInt, data = penaltyData, family = binomial())
summary(penaltyModel.test)
# None of the Int variables show significance, so linearity test passes
