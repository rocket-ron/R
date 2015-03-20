### A demonstration of correlation and chi-square in R

### Preparation

# car gives us nice scatterplots
library(car)

# We'll use our Country-by-Country dataset
load("Countries2.Rdata")
summary(Countries)


# We'll also use Google's dataset of takedown requests -
# that is, orders that come from governments of 
# different countries to remove certain content 
# from Youtube, search results, and other online products.
# Each row of this dataset corresponds to a specific
# country and a specific online product (you can think
# of the unit of analysis as country x product), and there
# are several variables of interest:
#
# Country - the country making specific takedown requests
# Product - the online product the content is hosted on 
#           (Youtube, Blogger, etc)
# Reason - a reason why the content is being targeted 
#           (copyright violation, government criticism, etc..)
# Court.Orders - the number of requests from the Country's 
#             court system
# Executive..Police..etc. - the number of requests from the
#             executive and other branches of government
# Items.Requested.To.Be.Removed - the number of separate items
#             of content.  However, this variable seems to
#             have a lot of missing values

# Read in the data
Requests = read.csv("Removal_Requests.csv")
head(Requests)

# Note that there are multiple rows per country in 
# the Requests dataframe.

# Create a new variable for total number of requests from
# all branches of government
Requests$total.takedowns = Requests$Court.Orders + Requests$Executive..Police..etc.

# To merge our datasets, we first need to sum together all the
# rows for each country in the Requests dataset, so that 
# each country only appears in one row.
# (we'll lose some variables when we do this, such as the product 
# the request referred to)
R2 = aggregate(Requests[,c("Court.Orders", "Executive..Police..etc.", "total.takedowns")], list(Country = Requests$Country), sum)

# Notice that there's one row per country now.
head(R2)

# Perform the merge
Countries = merge(Countries, R2, by="Country", all=T)

head(Countries)



### Correlation: Linear relationships between metric variables

# Let's examine the relationship between corruption 
# and takedown requests.

# Use a scatterplot to see how linear the relationship looks
scatterplot(Countries$cpi, Countries$total.takedowns)

#check the correlation
cor.test(Countries$cpi, Countries$total.takedowns)

# the cor function allows us to construct a correlation matrix
cor(Countries[,c("gdp", "cpi", "total.takedowns")], use = "pairwise.complete.obs")

# the output is actually a matrix object, so we can 
# do things like square each value to get R-squared
cor(Countries[,c("gdp", "cpi", "total.takedowns")], use = "pairwise.complete.obs")**2



### Chi-square: Testing for relationships between categorical variables
# Here are three different approaches, depending on structure of dataset

## 1. Two categorical variables

# Look at the frequency table between region and whether a country is corrupt
table(Countries$region, Countries$high_cpi)

# We store the results of our chi-square test so we can extract more
# values from the output
cs = chisq.test(Countries$region, Countries$high_cpi)

# Examine the test result
cs

# Look at the std. residuals to see which regions contribute most to the result
cs$stdres

# Check the expected counts to see if any are less than 5 and
# if we should therefore try Fisher's exact test
cs$expected

# Use Fisher's exact test in this case:
fisher.test(Countries$region, Countries$high_cpi)

# For an effect size, we could compute Cramer's V manually
# We may wish to put the code in a function so we can use
# it again whenever we want.
cramers_v = function(cs)
{
	cv = sqrt(cs$statistic / (sum(cs$observed) * (min(dim(cs$observed))-1)))
	print.noquote("Cramer's V:")
	return(as.numeric(cv))
}

# run our new function on our chi-square test
cramers_v(cs)

# As a rule of thumb,
# Cramer's V under .2 is weak
# between .2 and .4 is strong
# and above .4 is very strong

## 2. Count data, one variable in columns

# Consider each request to be the unit of analysis, and consider two variables:
# Whether it came from a corrupt or trustworthy country; and whether it came
# through a court order or executive/police action.  We want to know if these
# variables are independent or related.

# We can use aggregate to collapse the rows to just the high_cpi variable
Corrupt_Source = aggregate(Countries[,c("Court.Orders", "Executive..Police..etc.")], list(high_cpi = Countries$high_cpi), sum, na.rm=T)

# Note that we've created a table of counts:
Corrupt_Source

# Not required, but we can add row names to make the chi-square output prettier
rownames(Corrupt_Source)=Corrupt_Source$high_cpi

# We want to plug our count table into the chi-square test
# but we first have to remove the first column,
# because it's a factor.
# Otherwise, R will throw an error.
# Notice that we can use a negative index to omit columns
# That is, we can choose columns 2 and 3 with c(2,3)
# or we can get the same thing by skipping column 1 with c(-1)
Corrupt_Source[,c(-1)]

# Plug this into the Chi-square test
cs = chisq.test(Corrupt_Source[,c(-1)])
cs

# Look at the standardized residuals to see which direction the effect is in
cs$stdres

# Check the expected counts to see if any are less than 5 and
# if we should therefore try Fisher's exact test
cs$expected

# Since we have a 2x2 matrix, we can measure the effect
# size elegantly as an odds ratio.
# First, get the odds an order came from a Court for
# corrupt countries
corrupt_odds = Corrupt_Source["Corrupt","Court.Orders"] / Corrupt_Source["Corrupt","Executive..Police..etc."]

# Do the same for the trustworth countries.
trustworthy_odds = Corrupt_Source["Trustworthy","Court.Orders"] / Corrupt_Source["Trustworthy","Executive..Police..etc."]

# The odds ratio is just one divided by the other
corrupt_odds / trustworthy_odds

## 3. Count data, both variables in rows

# Let's see if corrupt countries are likely to target different products
# than trustworthy ones.  For this, we can't aggregate our data by Country
# so go back to the original request data, and merge in the high_cpi variable
# also, remove countries that are missing corruption data
Requests2 = merge(Countries[,c("Country", "high_cpi")], Requests, by="Country")
Requests2 = Requests2[ ! is.na(Requests2$high_cpi),]
head(Requests2)

# We want separate columns for takedown requests from corrupt countries
# and from trustworthy countries.  Here, we create both columns, and copy
# each value for total.takedowns to the appropriate one.
Corrupt_Product = Requests2[,c("Product","high_cpi")]
Corrupt_Product$Corrupt = ifelse(Requests2$high_cpi == "Corrupt", Requests2$total.takedowns, 0)
Corrupt_Product$Trustworthy = ifelse(Requests2$high_cpi == "Trustworthy", Requests2$total.takedowns, 0)

# Observe that each row only has a positive value in one of the two new columns
head(Corrupt_Product)

# Next we sum Corrupt and Trustworthy columns for each product.
Corrupt_Product =  aggregate(Corrupt_Product[,c("Corrupt","Trustworthy")], list( Product = Corrupt_Product$Product), sum)

# We are left with a contingency table
Corrupt_Product

# We could have also created the table in one step, using the cast command
library(reshape)
Corrupt_Product = cast(Requests2, Product ~ high_cpi , fun = sum, value = c("total.takedowns"))
Corrupt_Product

# Run a chi-square test as before
cs = chisq.test(Corrupt_Product[,c(-1)])
cs

# Check standardized residuals
cs$stdres

# And expected values
cs$expected

# The fisher test is probably too computationally intensive to run
fisher.test(Corrupt_Product[,c(-1)])

# could also use monte-carlo simulation to check significance
chisq.test(Corrupt_Product[,c(-1)], simulate.p.value = T)

# let's use the function we wrote earlier to check the effect size
cramers_v(cs)