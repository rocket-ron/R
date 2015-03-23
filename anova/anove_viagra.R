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
# Homogeneity of variance is ok - so we use regular ANOVA
# otherwise we would use Welch's F, or use one of the robust methods based on trimmed mean or bootstraps

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

summary.lm(viagraModel)

# Our viagra model shows that the probability of the F value occuring if there were 
# no effect in the population at p = .0247, or significant at p < .05

# Let's take a look at the plots the aov constructed
plot(viagraModel)

# Welch's F Test
oneway.test(libido ~ dose, data = viagraData)

# Welch's F is more conservative at F(2, 7.943) = 4.23, p = 0.54 which indicates non-significance

# Using robust methods (Wilcox)
# Load the latest Wilcox R tests 
source("~/OneDrive/MIDS/W203/Data/Rallfun-V27.txt")

# The Wilcox tests want the data in a matrix format, so first we convert the data frame 
viagraMatrix <- unstack(viagraData, libido ~ dose)
viagraMatrix

# Now we use the first test, the trimmed mean of 10%
t1way(viagraMatrix, tr = .1)

# The outcome of the trimmed mean function yields F(2, 7.943) = 4.32, p = 0.0537
# Moving on to the median-based test
med1way(viagraMatrix)

# The median one-way test yields F = 4.783 and p = 0.07
# Finally, we can use the bootstrap one way test with trimmed mean @ 0.05 and 2000 bootstrap samples
t1waybt(viagraMatrix, tr = 0.05, nboot = 2000)

# This yields F = 4.32, p = 0.061
# All the one-way robust tests indicate that there is no relationship between the dose of
# viagra and the libido

# Planned Contrasts of the Viagra Data 
summary(viagraModel)

# Here we see from the output that the low dose effect is non-significant at p = 0.28
# but that the effect of high dose is significant at p = .008

# let's set our own contrasts 
contrast1 <- c(-2, 1, 1)
contrast2 <- c(0, -1, 1)
contrasts(viagraData$dose) <- cbind(contrast1, contrast2)
viagraData$dose

# Contrast 1 compares the placebo group against the two experiment groups
# Contrast 2 compares the low-dose group to the high-dose group

viagraPlanned <- aov(libido ~ dose, data = viagraData)

summary.lm(viagraPlanned)

# Our hypothesis was that the experimental groups would increase libido above the control group
# Therefore we should use a 1-tailed comparison
# contrasting the expiremental group to the control group is significant at p < (.0293 / 2) = .0147
.0293/2

# and also we predicted that a higher dose would increase the effect over the low dose 
# contrast2 is significant at p < 0.0652 / 2 = .01325
.0625/2

# The planned contrasts show that taking viagra significantly increased libido compared to a
# control group, t(12) = 2.47, p < .05, and taking a high dose significantly increased libido
# compared to a low dose, t(12) = 2.029, p < .05 (one-tailed)

# Trend analysis
contrasts(viagraData$dose) <- contr.poly(3)
viagraTrend <- aov(libido ~ dose, data = viagraData)
summary.lm(viagraTrend)

# based on the trend analysis, a linear trend at t = 3.157, p = 0.008
# quadralinear is not significant at t=0.521 and p < 0.612

# Posthoc tests
# Bonferroni
pairwise.t.test(viagraData$libido, viagraData$dose, paired = FALSE, p.adjust.method = "bonferroni")

# Benjamini-Hochberg
pairwise.t.test(viagraData$libido, viagraData$dose, paired = FALSE, p.adjust.method = "BH")

# Tukey  (requires multcomp library)
postHocs <- glht(viagraModel, linfct = mcp(dose = "Tukey"))
summary(postHocs)
confint(postHocs)

# Dunnett
postHocs <- glht(viagraModel, linfct = mcp(dose = "Dunnett"), base = 1)
summary(postHocs)
confint(postHocs)

# Rand Wilcox's robust tests
lincon(viagraMatrix)
mcppb20(viagraMatrix)

# Effect size, omega squared

omega<-function(SSm, SSr, dfm, MSr)
{
	SSt = SSm + SSr
	omega = (SSm-(dfm*MSr))/(SSt+MSr)
	print(paste("Omega-Squared: ", omega))
	}
omega(20.13, 23.60, 2, 1.967)

# Effect size between low-dose and placebo group
mes(2.2, 3.2, 1.3038405, 1.3038405, 5, 5)
# d = -0.77, r = 0.46 (.74 sd)

# between high-dose and placebo group
mes(2.2, 5, 1.3038405, 1.5811388, 5, 5)
# d = -1.93, r = -0.69 ( ~ 2 sd's)

# between low and high dose groups
mes(3.2, 5, 1.3038405, 1.5811388, 5, 5)
# d = -1.24, r = -0.53 (1.24 sd's)

# Effect sizes for orthogonal contrasts 
rcontrast <- function(t, df)
{r <- sqrt(t^2/(t^2 + df))
         print(paste("r = ", r))
         }

 rcontrast(2.474, 12)
 rcontrast(2.029, 12)