# Set the working directory
setwd("/Users/rcordell/Documents/R/W203")

# Load the ggplot library
library(ggplot2)

# Load the Facebook data into a dataframe
facebookData = read.delim("FacebookNarcissism.dat", header = TRUE)

# experiment with different graph elements
graph <- ggplot(facebookData, aes(NPQC_R_Total, Rating))
# simple scatterplot
graph + geom_point()
# scsatterplot with inverted triangle shapes instead of circles
graph + geom_point(shape = 17)
# scatterplot with squares with x's in them
graph + geom_point(shape = 7)
# scatterplot with big squares with x's in them
graph + geom_point(shape = 7, size = 6)
# scatterplot with large right-side up triangles
graph + geom_point(shape = 2, size = 6)
# scatterplot with large + signs
graph + geom_point(shape = 3, size = 6)
# scatterplot with large X's
graph + geom_point(shape = 4, size = 6)
# scatterplot with large diamonds
graph + geom_point(shape = 5, size = 6)
# scatterplot with default shape and size, but vary color by variable "rating_type"
# Rating_Type is a 4-level factor. A legend for the colors is shown on the right
graph + geom_point(aes(color = Rating_Type))
# scatterplot with red dots
graph + geom_point(color = "RED")
# scatterplot that uses jitter to spread the points out so they're not on top of one another
# and vary color by Rating_Type
graph + geom_point(aes(color = Rating_Type), position = "jitter")
# vary shape by "Rating_Type". A legend for shapes is shown on the right
graph + geom_point(aes(shape = Rating_Type), position = "jitter")
#
#
# Let's play with a different data set, Exam Anxiety
examData <- read.delim("Exam Anxiety.dat", header = TRUE)
summary(examData)
# scatter plot of Exam data
scatter <- ggplot(examData, aes(Anxiety, Exam))
scatter + geom_point()
# custom labels for the scatter plot
scatter + geom_point() + labs(x = "Exam Anxiety", y = "Exam Performance %")
# add a curve that follows the mean of the data
scatter + geom_point() + geom_smooth() + labs(x = "Exam Anxiety", y = "Exam Performance %")
# add a line that follows a linear model of the data
scatter + geom_point() + geom_smooth(method = "lm") + labs(x = "Exam Anxiety", y = "Exam Performance %")
# change the color of the line to RED
scatter + geom_point() + geom_smooth(method = "lm", colour = "RED") + labs(x = "Exam Anxiety", y = "Exam Performance %")
# remove the envelope from the line
scatter + geom_point() + geom_smooth(method = "lm", colour = "RED", se = F) + labs(x = "Exam Anxiety", y = "Exam Performance %")
# change the color of the envelope and line to blue and make it transparent
scatter + geom_point() + geom_smooth(method = "lm", alpha = 0.1, fill = "Blue") + labs(x = "Exam Anxiety", y = "Exam Performance %")
# color the scatterplot points by gender - but this isn't working as expected
scatter <- ggplot(examData, aes(Anxiety, Exam), color = Gender)
scatter + geom_point() + geom_smooth(method = "lm") + labs(x = "Exam Anxiety", y = "Exam Performance %")
# scatter + geom_point() + geom_smooth(method = "lm") + labs(x = "Exam Anxiety", y = "Exam Performance %")
# scatter <- ggplot(examData, aes(Anxiety, Exam), color = Gender)

scatter + geom_point()
scatter + geom_point() + geom_smooth(method = "lm")
scatter + geom_point() + geom_smooth(method = "lm", aes(fill = Gender), alpha = 0.1)
# create a linear line for each Gender and color the envelope accordingly
scatter + geom_point() + geom_smooth(method = "lm", aes(color = Gender, fill = Gender), alpha = 0.1)
scatter + geom_point() + geom_smooth(method = "lm", aes(color = Gender, fill = Gender), alpha = 0.1) + labs(x = "Exam Anxiety", y = "Exam Performance %")
scatter <- ggplot(examData, aes(Anxiety, Exam, color = Gender))
scatter + geom_point() + geom_smooth(method = "lm", aes(color = Gender, fill = Gender), alpha = 0.1) + labs(x = "Exam Anxiety", y = "Exam Performance %")
scatter + geom_point() + geom_smooth(method = "lm", aes(color = Gender, fill = Gender), alpha = 0.1) + labs(x = "Exam Anxiety", y = "Exam Performance %")

# Now let's graph some of the Festival Hygiene data
festivalData <- read.delim("DownloadFestival.dat", header = TRUE)

graph + geom_point()
graph + geom_point() + geom_smooth(method = "lm")
graph + geom_point() + geom_smooth(method = "lm", se = F)
graph + geom_point() + geom_smooth(method = "lm", aes(color = Rating_Type))
graph + geom_point() + geom_smooth(method = "lm", aes(color = Rating_Type), se = F)
graph + geom_point(aes(color = Rating_Type)) + geom_smooth(method = "lm", aes(color = Rating_Type), se = F)
graph + geom_point(aes(color = Rating_Type), position = "jitter") + geom_smooth(method = "lm", aes(color = Rating_Type), se = F)
graph + geom_point(aes(color = Rating_Type), position = "jitter") + geom_smooth(method = "lm", aes(color = Rating_Type), se = F) + labs(x = "Total Score", y = "Narcissism Rating")
graph + geom_point(aes(color = Rating_Type)) + geom_smooth(method = "lm", aes(color = Rating_Type), se = F) + labs(x = "Total Score", y = "Narcissism Rating")
graph + geom_point(aes(color = Rating_Type), position = "jitter") + geom_smooth(method = "lm", aes(color = Rating_Type), se = F) + labs(x = "Total Score", y = "Narcissism Rating")
graph + geom_smooth(aes(color = Rating_Type)) + labs(x = "Total Score", y = "Narcissism Rating")

graph + geom_point(aes(color = Rating_Type), position = "jitter") + geom_smooth(method = "lm", aes(color = Rating_Type), se = F) + labs(x = "Total Score", y = "Narcissism Rating")
graph + geom_point(aes(color = Rating_Type), position = "jitter") + geom_smooth(method = "lm", aes(color = Rating_Type), se = F) + labs(x = "Narcissism (NPQC)", y = "Facebook Picture Rating")

# Create a histogram of festival data
festivalHistogram <- ggplot(festivalData, aes(day1))
festivalHistogram + geom_histogram()

# Adjust bin width
festivalHistogram + geom_histogram(binwidth = 0.4)
festivalHistogram + geom_histogram(binwidth = 0.4) + labs(x = "Hygeine (Day 1)", y = "Frequency")

festivalHistogram <- ggplot(festivalData, aes(day1), theme(legend.position = "none"))
festivalHistogram + geom_histogram(binwidth = 0.4) + labs(x = "Hygeine (Day 1)", y = "Frequency")
festivalHistogram <- ggplot(festivalData, aes(day1)) + theme(legend.position = "none")
festivalHistogram + geom_histogram(binwidth = 0.4) + labs(x = "Hygeine (Day 1)", y = "Frequency")
festivalHistogram <- ggplot(festivalData, aes(gender, day1)) + theme(legend.position = "none")
festivalBoxplot <- ggplot(festivalData, aes(gender, day1)) + theme(legend.position = "none")
festivalBoxplot + geom_boxplot() + labs(x = "Gender", y = "Hygiene (Day 1 of Festival)")

# Order the data to look for outliers
festivalData <- festivalData[order(festivalData$day1),]
festivalData

# User RCommander to edit the data to correct the error
library(Rcmdr)

festivalBoxplot <- ggplot(festivalData, aes(gender, day1)) + theme(legend.position = "none")
festivalBoxplot + geom_boxplot() + labs(x = "Gender", y = "Hygiene (Day 1 of Festival)")
festivalHistogram <- ggplot(festivalData, aes(day1)) + theme(legend.position = "none")
festivalHistogram + geom_histogram()

festivalHistogram + geom_histogram(binwidth = 0.4)
festivalBoxplot + geom_boxplot() + labs(x = "Gender", y = "Hygiene (Day 1 of Festival)")
festivalBoxplot <- ggplot(festivalData, aes(gender, day2)) + theme(legend.position = "none")
festivalBoxplot + geom_boxplot() + labs(x = "Gender", y = "Hygiene (Day 2 of Festival)")

festivalBoxplot <- ggplot(festivalData, aes(gender, day3)) + theme(legend.position = "none")
festivalBoxplot + geom_boxplot() + labs(x = "Gender", y = "Hygiene (Day 3 of Festival)")

density <- ggplot(festivalData, aes(day1))
density + geom_density()

# Create the function outlierSummary as given by the book
function(variable, digits = 2){
	
	zvariable<-(variable-mean(variable, na.rm = TRUE))/sd(variable, na.rm = TRUE)
		
	outlier95<-abs(zvariable) >= 1.96
	outlier99<-abs(zvariable) >= 2.58
	outlier999<-abs(zvariable) >= 3.29
	
	ncases<-length(na.omit(zvariable))
	
	percent95<-round(100*length(subset(outlier95, outlier95 == TRUE))/ncases, digits)
	percent99<-round(100*length(subset(outlier99, outlier99 == TRUE))/ncases, digits)
	percent999<-round(100*length(subset(outlier999, outlier999 == TRUE))/ncases, digits)
	
	cat("Absolute z-score greater than 1.96 = ", percent95, "%", "\n")
	cat("Absolute z-score greater than 2.58 = ",  percent99, "%", "\n")
	cat("Absolute z-score greater than 3.29 = ",  percent999, "%", "\n")
}

outlierSummary(festivalData$day2)

# Now let's use the ChickFlick data
chickFlick <- read.delim("ChickFlick.dat", header = TRUE)

bar <- ggplot(chickFlick, aes(film, arousal))
bar + stat_summary(fun.y = mean, geom = "bar", fill = "White", color = "Black")
bar + stat_summary(fun.y = mean, geom = "bar", fill = "White", color = "Black") + stat_summary(fun.data = mean_cl_normal, geom = "pointrange")
bar + stat_summary(fun.y = mean, geom = "bar", fill = "White", color = "Black") + stat_summary(fun.data = mean_cl_boot, geom = "pointrange")
bar + stat_summary(fun.y = mean, geom = "bar", fill = "White", color = "Black") + stat_summary(fun.data = mean_cl_boot, geom = "pointrange")
bar <- ggplot(chickFlick, aes(film, arousal, fill = gender))
bar + stat_summary(fun.y = mean, geom = "bar", position = "dodge") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width = 0.90), width = 0.2)
bar + stat_summary(fun.y = mean, geom = "bar", position = "dodge") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width = 0.90), width = 0.2) + labs(x = "Film", y = "Mean Arousal", fill = "Gender")
bar <- ggplot(chickFlick, aes(film, arousal, fill = film))
bar + stat_summary(fun.y = mean, geom = "bar")
bar + stat_summary(fun.y = mean, geom = "bar") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2)

# Let's see how facets work
bar + stat_summary(fun.y = mean, geom = "bar") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) + facet_wrap( ~ gender)
bar + stat_summary(fun.y = mean, geom = "bar") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) + facet_wrap( ~ gender) + labs(x = "Film", y = "Mean Arousal") + theme(legend.position = "none")

# Change the color of the bars
bar + stat_summary(fun.y = mean, geom = "bar") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) + facet_wrap( ~ gender) + labs(x = "Film", y = "Mean Arousal") + theme(legend.position = "none") + scale_fill_manual(values = c("Female" = "Blue", "Male" = "Green"))

# Remove the legend
bar + stat_summary(fun.y = mean, geom = "bar") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) + facet_wrap( ~ gender) + labs(x = "Film", y = "Mean Arousal") + theme(legend.position = "none")

bar <- ggplot(chickFlick, aes(film, arousal, fill = gender))
bar <- ggplot(chickFlick, aes(film, arousal, fill = gender), scale_fill_manual("Gender", values = c("Female" = "Blue", "Male" = "Green")))
bar + stat_summary(fun.y = mean, geom = "bar")
bar + stat_summary(fun.y = mean, geom = "bar", position = "dodge")
bar + stat_summary(fun.y = mean, geom = "bar", position = "dodge") + scale_fill_manual("Gender", values = c("Female" = "Blue", "Male" = "Green"))
bar + stat_summary(fun.y = mean, geom = "bar", position = "dodge") + scale_fill_manual("Gender", values = c("Female" = "#3366FF", "Male" = "#336633"))

bar + stat_summary(fun.y = mean, geom = "bar", position = "dodge") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) + scale_fill_manual("Gender", values = c("Female" = "#3366FF", "Male" = "#336633"))
bar + stat_summary(fun.y = mean, geom = "bar", position = "dodge") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width = 0.90)) + scale_fill_manual("Gender", values = c("Female" = "#3366FF", "Male" = "#336633"))

# From Section 4 class - working with z-values and normal distrubutions
# Population mean = 10,000, Standard error = 1000/sqrt(10) where 10 = sample size and 1000 is population standard deviation, and 10,300 is the sample mean we're testing
# This will give P(sample mean < 10,300)
pnorm(10300,10000,316.22)
[1] 0.8286151

# This will give P(sample mean > 10,300)
1 - pnorm(10300,10000,316.22)

# Checking the Standard Error value
1000/sqrt(10)

# Now check to see what is probability of sample mean between 10,500 and 9,500
# First find P(sample mean < 10500)
pnorm(10500, 10000, 316.22)

# Hmmmm - it's going to be the same for 9,500 - so if we take the probability of the two tails, so to speak
2 * (1 - pnorm(10500, 10000, 316.22))

# I wonder if it is mor accurate to put in the SE computation
2 * (1 - pnorm(10500, 10000, 1000/sqrt(10)))

# Actually we want 1 - P
1 - 2 * (1 - pnorm(10500, 10000, 1000/sqrt(10)))
# Here's a better way to do it using the z-values (need to look at the pnorm function in more detail)
pnorm(-1.58) - pnorm(1.58)

# And another way
1 - 2 * pnorm(1.58, lower.tail = FALSE)
