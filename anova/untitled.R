#-------------------------- ANOVA Test Procedure -----------------------#
# 
# This script uses the Viagra data as described in Discovering Statistics Using R, pg. 434, by Andy Field
#
#-----------------------------------------------------------------------#
#
# Load libraries
library(ggplot2)
library(granova)
library(car)
library(Rcmdr)
library(pastecs)
library(multcomp)
library(compute.es)

# First we enter the data manually

libido <- c(3,2,1,1,4,5,2,4,2,3,7,4,5,3,6)
dose <- gl(3, 5, labels = c("Placebo", "Low Dose", "High Dose"))
viagraData <- data.frame(dose, libido)

ls.str(viagraData)

# Let's examine the data and test assumptions
shapiro.test(viagraData$libido)

# Shapiro-Wilk's test indicates non-normal sampling distribution for libido at p = .581


leveneTest(viagraData$libido, viagraData$dose, center=median)
#
# Levene's Test is non-significant at F(2, 12) = 0.1176 and p = 0.89
# Levene's test indicates that the variances are not significantly different
# Homogeneity of variance is ok

# Graph - this gets very fancy by placing 2 points for the mean to give the appearance of a dot with a border
# 
# From the graph we see that the error bars overlap from one mean to the next and that there seems to 
# be a linear trend as the data changes
line <- ggplot(viagraData, aes(dose, libido))
line + stat_summary(fun.y = mean, geom = "line", size = 1, aes(group=1), colour = "#FF6633") + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, size = 0.75, colour = "#990000") + stat_summary(fun.y = mean, geom = "point", size = 4, colour = "#990000") + stat_summary(fun.y = mean, geom = "point", size = 3, colour = "#FF6633") + labs(x = "Dose of Viagra", y = "Mean Libido")

# Descriptive statistics
by(viagraData$libido, viagraData$dose, stat.desc)

# We can use the lm() or aov() function to create the model.
viagraModel <- aov(libido ~ dose, data = viagraData)

summary(viagraModel)

# Our viagra model shows that the probability of the F value occuring if there were 
# no effect in the population at p = .0247, or significant at p < .05

# Let's take a look at the plots the aov constructed
plot(viagraModel)

# Welch's F Test
oneway.test(libido ~ dose, data = viagraData)

# Welch's F is more conservative at F(2, 7.943) = 4.23, p = 0.54 which indicates non-significance

# Using robust methods (Wilcox)