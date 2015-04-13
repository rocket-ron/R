#############################################################
#
# Multiple Regression in R
#
# From Andy Field's book Discovering Statistics Using R
#
#############################################################
#
setwd("~/OneDrive/MIDS/W203/Data/multiple regression")

# Load up the album2.dat file as discussed in 7.8.2.2
album2 <- read.delim("Album Sales 2.dat", header = TRUE)

summary(album2)
#
# From the Album Sales 2 data set there are 4 vectors in the
# data frame: adverts, sales, airplay and attract
#
# The goal is to create a model that takes into account these
# predictors on the sales as the outcome variable.
# The first thing we can do is load the car library to use 
# the handy scatterplot() in that library. 
library(car)
scatterplot(album2$sales, album2$adverts)

# The data show a pretty wide variance for these variables
# but we see that there is a line that may indicate a linear 
# relationship exists.
#
# Out of curiousity, let's see what the correlation is between
# these two vectors.
cor.test(album2$sales, album2$adverts)

# The correlation t-test shows that we can reject the null 
# hypothesis that there is no correlation between adverts and 
# album sales and support the hypothesis that there is a 
# correlation with p = 2.2e-16 (highly significant). The 
# correlation is 0.5785 with a 95% confidence interval of 
# 0.4781 - 0.66394. 
#
# Performing a linear regression and obtaining the first model
albumSales.2 <- lm(sales ~ adverts, data = album2)
summary(albumSales.2)

# The model shows an increase of 1 unit of adverts results in
# 0.09612 in album sales, highly significant, with a multiple 
# R^2 = 0.3346, and an adjusted R^2 of 0.3313
#
# Let's take a look at the plot output
plot(albumSales.2)
#
# There appears to be a bit of heteroscadicity in the Residuals
# vs. Fitted graph; not a huge amount, but it's there.
# The QQ Plot actually looks somewhat reasonable.
# Scale-Location plot shows a similar indication of
# heteroscadicity
# Don't see anything with respect to leverage that is of concern
#
#
# Now, we think that airplay is also a significant predictor of 
# album sales, so let's create another model that now inludes
# airplay
albumSales.3 <- lm(sales ~ adverts + airplay, data = album2)
summary(albumSales.3)

# As we expected, airplay has a highly significant non-zero
# coefficient. For each unit increase in airplay, album sales
# increase 3.59 units The multiple R^2 has increased to 0.6293,
# so our model accounts for almost 63% of the variation in album sales.
#
plot(albumSales.3)
#
# Little bit better on the homoscedacity than the first model 
# QQ Plot still looks reasonable
# Other diagnostics look good
#
# Now let's add in the final variable, attractiveness of the band 
albumSales.4 <- lm(sales ~ adverts + airplay + attract, data = album2)
summary(albumSales.4)
#
# Now we have all 3 beta coefficients and all are statistically
# significantly different from zero.
# The R^2 has increased to 0.665
plot(albumSales.4)
#
# Diagnostics from the plot() all look OK
#
# We can examine the standardized beta estimates by loading the 
# QuantPsych library and using the lm.beta() 
library(QuantPsyc)
library(boot)

lm.beta(albumSales.4)
#
# The betas are given in terms of standard deviations. So now
# for adverts with a standardized beta of 0.511 we can say that
# as advert budget increases by 1 SD then album sales increase 
# by 0.511 SD.
# Also, every 1 SD increase in airplay results in 0.512 increase
# in album sales.
# Finally, every 1 SD increase in band attractiveness results
# in a 0.192 increase in album sales.
#
# Compute the confidence intervas for the model 
confint(albumSales.4)

# We see that our coefficients fall in range of each of these 
# confidence intervals and that the confidence intervals don't
# cross 0.
#
# Compare models using anova
anova(albumSales.2, albumSales.3)

# Compared to a model based on the mean, the first model improved
# with F(2, 197) = 156.57, p < .001
anova(albumSales.3, albumSales.4)

# Compared to the previous model, the final model is an improvement
# F(2, 196) = 20.681, p < .001

# Compute some useful diagnostic statistics and place them in the
# dataframe
album2$standardized.residuals <- rstandard(albumSales.4)
album2$studentized.residuals <- rstudent(albumSales.4)
album2$cooks.distance <- cooks.distance(albumSales.4)
album2$dfbeta <- dfbeta(albumSales.4)
album2$dffit <- dffits(albumSales.4)
album2$leverage <- hatvalues(albumSales.4)
album2$covariance.ratios <- covratio(albumSales.4)
#
album2$large.residual <- (album2$standardized.residuals > 2) | (album2$standardized.residuals < -2)
# how many residuals do we have outside 2 sd?
sum(album2$large.residual)
#
# There are 12 data points with large residuals outside the 96.5% range.
# The next command will display those rows and the specified columns
album2[album2$large.residual, c("sales","airplay","attract","adverts","standardized.residuals")]
#
# There's nothing too huge except for data point 169, which is ~ 3.1
# Let's take a look at some of the other diagnostic variables for these
# 12 data points. 
album2[album2$large.residual, c("cooks.distance", "leverage", "covariance.ratios")]
#
# We know that the CVR upper bound is 1 + 3(k + 1)/n = 1 + 3(3 + 1)/200
# and the CVR lower bound is 1 - 3(k + 1)n = 1 - 3(3 + 1)/200
1 + 12/200
1 - 12/200
# 1.06 and 0.94, respectively.
# Looking at the covariance ratios, the only point outside these bounds
# is 169, again. However, the Cooks distance value for this point is
# of little convern, so this point is not excercising undue influence 
#
# Durbin-Watson test of independence
dwt(albumSales.4)
#
# This shows that we can't reject the null hypothesis of no autocorrelation
#
# Multicollinearity
vif(albumSales.4)
1/vif(albumSales.4)		# Tolerance is 1/VIF
mean(vif(albumSales.4))
#
# If the largest VIF > 10 then there is cause for concern
# If the average VIF is substantially > 1 then the regression may be biased
# Tolerance < 0.1 indicates a serious issue
# Tolerance < 0.2 indicates a potential issue