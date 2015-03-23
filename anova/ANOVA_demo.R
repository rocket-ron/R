# A demonstration of ANOVA in R

# Load Youtube video data
load("Videos_clean.Rdata")
summary(Videos)


# check the rate variable for normality
hist(Videos$rate)

# That's not great, but remember that ANOVA is a robust-test
# and the data is on a 1-5 scale, which isn't normally
# a place we'd worry

# Let's look at the means, by each category and overall
by(Videos$rate, Videos$category, mean, na.rm=T)
mean(Videos$rate, na.rm=T)

# We can get nicer output with the tapply function
tapply(Videos$rate, Videos$category, mean, na.rm=T)

# Perform the analysis of variance and check the significance
aovm = aov(rate ~ category, Videos)
summary(aovm)

# Post-hoc, we can compare our groups pairwise
tt = pairwise.t.test(Videos$rate, Videos$category, p.adjust.method = "bonferroni")
tt

# That's hard to read, so let's create a prettier table
# Write a function to add significance stars to a p-value
sig_stars = function(p)
{
	stars = symnum(p, na = F, cutpoints = c(0, .001, .01, .05, .1, 1), symbols=c("***","**", "*", ".", " "))
	return( paste(round(p, 3), stars) )
}

# apply our new function to every element in our matrix
t_table = apply(tt$p.value, c(1,2), sig_stars) 
t_table
# get rid of the quotes
t_table = noquote( t_table )
t_table

# tidy up the column names and erase the NAs
colnames(t_table) = abbreviate(colnames(t_table))

# Notice that upper.tri indexes the upper right
# triangle of the matrix
upper.tri(t_table)

# set those positions to the empty string
t_table[upper.tri(t_table)] = ""

# the finished table
t_table

