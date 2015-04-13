### A demonstration of simple OLS regression in R

# 1. Setting up a regression

# Use car for scatterplots
library(car)

# Load the Countries dataset
load("Countries3.Rdata")
summary(Countries)

# Let's look at the number of internet users as predicted by
# a country's GDP.  As we'll see, this model isn't very well
# chosen, but it will give us a chance to see some of the problems
# that often come up with linear regression.

# First, check the scatterplot to get a sense of the underlying
# relationship.
scatterplot(Countries$gdp, Countries$internet_users_2011)

# That doesn't look too linear, so we already know that our
# regression is somewhat problematic.

# Set the rownames so that they appear in the diagnostic plots
rownames(Countries) = Countries$Country

# Create the linear model, and look at a summary
model = lm(internet_users_2011 ~ gdp, data = Countries)
summary(model)

# 2. Regression diagnostics

# use the plot command for common regression diagnostics
plot(model)
# This command pulls up a series of plots that can be used to
# check the standard OLS assumptions
#
# plot 1: residuals versus fitted values.  check for 
# heteroskedasticity.  This means that the spread of points
# on the graph changes from the left side to the right side.  
# We can also look for non-linear relationships.  That would
# mean that the band of points moves up and down as you look
# from the left to the right of the plot.  This particular
# regression seems to suffer from both problems.
#
# plot 2: qqplot of standardized residuals vs. normal curve.  
# Check to see if the errors are normally distributed.  That
# means that the points should follow the diagonal line closely.
# In this regression, we don't seem to have normal residuals, 
# but it's a large sample size, so we can use the central limit
# theorem and we don't have to worry about this.
#
# plot 3: scale-location.  This is a variation on the 
# residuals vs fitted values plot.  Since we take the absolute
# value of the resuduals, all points appear above the x-axis,
# and more variance appears as higher points.  Heteroskedasticity
# can then be detected if the mass of points moves up and down
# as you look from the left to the right of the graph.
# Curiously, this regression doesn't seem to have a problem on
# this plot.  It seems the non-linearity is actually obscuring
# the problem with heteroskedasticity.
#
# plot 4: residuals vs leverage. Look for outliers that 
# may be biasing the model.
# In this regression, no points have Cook's distance > 1, so
# we don't need to worry.

# Here's an explicit way to check for points with large residuals 
outlierTest(model)

# Could use the Durbin-Watson test to check for autocorrelation
# It's really not necessary here since the order of Countries
# is alphabetical, and there's no reason to suspect that alphabetical
# proximity influences residuals.
dwt(model)

# We suspect heteroskedasticity here, and we could check
# with a Breusch-Pagan test.  Since we are testing a null
# hypothesis of homoskedasticity, this test will tend to
# be significant for large sample sizes, so interpret the
# results with caution.
library(lmtest)
bptest(model)
# In this case, the test is negative.  Breusch-Pagan is based
# on squaring the residuals, so again, it could be that the
# non-linear relationship is obscuring the heteroskedasticity.

# Just in case, let's check the heteroskedasticity-robust errors
# We are keeping out regression line the same, just adjusting
# our estimates of significance.
library(sandwich)
coeftest(model, vcov = vcovHC)

# As an exercise, you could try the log of gdp instead
# to see what really nice regression output looks like