# Simple Linear Regression in R
#
# Use the albumSales.dat file for these examples

setwd("~/OneDrive/MIDS/W203/Data/lr")
album1 <- read.delim("Album Sales 1.dat")
head(album1)
summary(album1)

# run a regression analysis using a linear model, excluding any NA entries
newModel <- lm(sales ~ adverts, data=album1, na.action=na.exclude)

summary(newModel)

# The summary() command gives the output from the linear regression
# It tells us that the value of R^2 is 0.3346, so we can get the Pearson's
# correlation co-efficient by taking the square root of R^2:
newModel.pearsonscc = sqrt(0.3346)  # which comes to 0.5784462

# Another way we can determine the pearsons correlation co-efficient is
# to run a correlation test 
cor(album1$adverts, album1$sales, use = "complete.obs", method="pearson")

# Which results in the same value of 0.5784

# The R^2 value tells us that the adverts accounts for 33.5% of the variation
# in album sales. Therefore there must be other variables that account for 
# the remaining 66.5% of variation in album sales.

# Examining the F-statistic from the summary output we have:
F-statistic: 99.59 on 1 and 198 DF,  p-value: < 2.2e-16

# This tells us that we can reject the null hypothesis that the model is not 
# significantly improved with a P < .001

# The regression coefficients are given as 
# intercept -> 134.1, gradient -> 0.09612
# so we can say that with no additional change in adverts we will have 134100
# albums sold, and that for every additional 1000 spent in adverts we will 
# sell an additional 96 albums (not the best investment in the world)
# Both the intercept and gradient values have a t-test value < .001 so we 
# can reject the null hypothesis that these values are not zero and they are
# a genuine effect in the population.

# We can examine the individual residuals from the model on the actual data
# and look for any residuals that seem particularly large
resid(newModel)

# examine the standardized residuals. 95% of z-scores shoule lie between +/- 1.96
# and 99% between +/-2.58. Anything over 3 sd should be suspect, or values outside
# +/- 3.29
rstandard(newModel)

# Examine for residuals that have undue influence on the model, first using the t-test
rstudent(newModel)

# We can calculate the Pearson residual as
PearsonResidual <- (resid(newModel)-mean(resid(newModel)))/sd(resid(newModel))





# Multiple linear regression - we extend the model to multiple predictors
# first get the album2 data
album2<-read.delim("Album Sales 2.dat", header = TRUE)
head(album2)
summary(album2)

# run the multiple regression model 
albumSales.2<-lm(sales ~ adverts, data = album2)
albumSales.3<-lm(sales ~ adverts + airplay + attract, data = album2)
summary(albumSales.2)
summary(albumSales.3)

# We can see from the R^2 that if the first model (albumSales.2) has multiple R^2 
# of 0.3346 and the second model adds two more predictors to get an R^2 of .6647
# that we have accounted for an addtional 33% of album sales by the inclusion of the
# other two predictors.




#---We can obtain standardized parameter estimates with the lm.beta() function---
library(car)
library(QuantPsyc)
library(boot)

# the lm.beta() gives us the standardized parameters, such that if parameter 1
# varies by 1 sd then the outcome variable changes by beta sd's
lm.beta(albumSales.3)
sd(album2$sales)
sd(album2$adverts)

# sd of sales = 80.698, sd of adverts = 485.655 (these are in 1000's), so
# 80,698 and 485,655, respectively
# the lm.beta shows 0.511 on adverts, so an increase of 1 sd in adverts results in
# an increase of 0.511 sd's of sales -> increase of 485,655 in adverts results in
# 0.511 * 80,698 in sales = 41,240

# the larger the beta values the more important the predictor

#---Confidence intervals are obtained with the confint() function----
confint(albumSales.3)

# compare the two models with anova
anova(albumSales.2, albumSales.3)

# The anova gives us an F=96.447 and p < .001, so the updated model is an 
# improvement over the first model

# Model Diagnostics
#----Obtain casewise diagnostics and add them to the original data file.---

album2$residuals<-resid(albumSales.3)
album2$standardized.residuals <- rstandard(albumSales.3)
album2$studentized.residuals <- rstudent(albumSales.3)
album2$cooks.distance<-cooks.distance(albumSales.3)
album2$dfbeta <- dfbeta(albumSales.3)
album2$dffit <- dffits(albumSales.3)
album2$leverage <- hatvalues(albumSales.3)
album2$covariance.ratios <- covratio(albumSales.3)

#----List of standardized residuals greater than 2--------------
album2$standardized.residuals>2| album2$standardized.residuals < -2

#---Create a variable called large.residual, which is TRUE (or 1) if the residual is greater than 2, or less than -2.----------
album2$large.residual <- album2$standardized.residuals > 2 | album2$standardized.residuals < -2

#---Count the number of large residuals-------------
sum(album2$large.residual)


#---Display the value of sales, airplay, attract, adverts, and the standardized residual, for those cases which have a residual greater than 2 or less than -2.-------------
album2[album2$large.residual,c("sales", "airplay", "attract", "adverts", "standardized.residuals")]

#-----Cook's distance, leverage and covariance ratio for cases with large residuals.---------
album2[album2$large.residual , c("cooks.distance", "leverage", "covariance.ratios")]


#----The Durbin-Watson test is obtained with either dwt() or durbinWatsonTest()---
durbinWatsonTest(albumSales.3)
dwt(albumSales.3)

#----Obtaining the VIF---
vif(albumSales.3)

#----The tolerance is 1/VIF---
1/vif(albumSales.3)

#----The mean VIF---
mean(vif(albumSales.3))


#---Histogram of studentized residuals---

hist(album2$studentized.residuals)
hist(rstudent(albumSales.3))

#--Plot of residuals against fitted (predicted) values, with a flat line at the mean--
plot(albumSales.3$fitted.values,rstandard(albumSales.3))
abline(0, 0)

#same as above
plot(albumSales.3)

