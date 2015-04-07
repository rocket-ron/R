# We're going to use a similar script to the simple_regression script and 
# the Countries3.Rdata file to take a look at Internet Users as a function
# of GDP. However, this time we'll transform the GDP data by taking the
# log(GDP) and performing the regression with that to help deal with the
# heteroskedacity issues we saw in the non-transformed data.
#
#
# Set the working directory
setwd("~/Documents/R/lr")
#
# Load the car library for the scatterplot function
library(car)
#
# Load the countries dataset
load("Countries3.Rdata")
#
# Take a look at the Countries data
summary(Countries)
#
# Use the country name as the row name as a convenience
rownames(Countries) <- Countries$country
#
# Look at the number of internet users per country by gdp
scatterplot(Countries$gdp, Countries$internet_users_2011)
#
# This looks like there will be some problems with linear regression 
# assumptions, and in the previous version of the script we saw
# heteroskedacity issues. 
# So let's try transforming the data for gdp by taking the log base 10 (gdp)
#
Countries$loggdp <- log(Countries$gdp)
#
scatterplot(Countries$loggdp, Countries$internet_users_2011)
#
# Now we see a little better scatterplot
# Let's go ahead and construct our linear regression model
model = lm(internet_users_2011 ~ loggdp, data = Countries)
summary(model)
#
# the coefficients are highly significant as we saw before but
# let's take a look at the plots of the model to see how they look
plot(model)
#
# plot 1: Residuals vs. Fitted
# We still see some variation of the band of residuals but it's much better
# than before the transformation. There's a better overall uniformity to the
# band 
#
# plot 2: QQ Plot 
# Again we see that the ends of the plot move away from the diagonal, but it 
# is much better than before the trasnformation. The lower left of the plot 
# is still away from the diagonal, however. But normality is probably not as much 
# of an issue here because of the Central Limit Theorem and sample size.
#
# plot 3: Scale Location 
# This alternative homoskedacity plot shows much better then before the
# transformation as well.
#
# plot 4: Residuals vs. Leverage
# There's nothing to worry about on this plot. 
#
library(lmtest)
bptest(model)
# Again the Beusch-Pagan test doesn't really show anything for heterskedacity
#
# So if we look at the summary of the model again, we should be able to 
# convert the gdp coefficients back to something that makes sense instead
# of logarithm.
summary(model)
#
# The intercept is -66.518 but the beta co-efficient is 13.89. 
# That would make an equation of Y = 13.89*loggdp - 66.518
# So to convert the beta co-efficent we take e^beta 
exp(13.89)
#
# e^13.89 = 1077334
# for every 1077334 change in gdp we have 1 unit change in internet users.