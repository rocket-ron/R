# This is a list of commands and processes to use with R when exploring data set

# Set the working directory, usually where the data files are located
setwd("~/OneDrive/MIDS/W203/Data/08")

# Load a data file. There are several types, from delimited data, CSV files and RData file
dataframe <- read.delim("handlebars.dat", header=TRUE)

# CSV file
dataframe <- read.csv("Removal_Requests.csv", header=TRUE)

# RData file restores a data session
load("Countries2.RData")

##########################################################################################

# Summarize a dataframe
summary(dataframe)

# list the items in the environment
ls()
# list the items and describe each - good to see a dataframe and the type of each vector or column
ls.str()

# dump the top 5 rows of a data frame 
head(dataframe)
head(Countries)

##########################################################################################

# Examine Data visually

# Load the ggplot2 library
library(ggplot2)


# Load the Facebook data into a dataframe
facebookData = read.delim("~/OneDrive/MIDS/W203/Data/04/FacebookNarcissism.dat", header = TRUE)

# scatterplot with different elements
graph <- ggplot(facebookData, aes(NPQC_R_Total, Rating))
graph + geom_point()
graph + geom_point(shape = 7, size = 6)
graph + geom_point(aes(color = Rating_Type))
graph + geom_point(color = "RED")
graph + geom_point(aes(color = Rating_Type), position = "jitter")
graph + geom_point(aes(shape = Rating_Type), position = "jitter")

# add a smoothing line
# color can also be a factor in the data frame.
# the method parameter can be left off for the default, which is not linear
graph + geom_point() + geom_smooth(method = "lm", alpha=0.1, colour="red", fill="blue") + labs(x = "Narcisisim", y = "Rating")

# The car library has a nice default scatter plot
library(car)
scatterplot(Countries$gdp, Countries$fertility_rate)
head(Countries)

# histograms
